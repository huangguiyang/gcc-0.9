/* Reload pseudo regs into hard regs for insns that require hard regs.
   Copyright (C) 1987 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY.  No author or distributor
accepts responsibility to anyone for the consequences of using it
or for whether it serves any particular purpose or works at all,
unless he says so in writing.  Refer to the GNU CC General Public
License for full details.

Everyone is granted permission to copy, modify and redistribute
GNU CC, but only under the conditions described in the
GNU CC General Public License.   A copy of this license is
supposed to have been given to you along with GNU CC so you
can know your rights and responsibilities.  It should be in a
file named COPYING.  Among other things, the copyright notice
and this notice must be preserved on all copies.  */


#include "config.h"
#include "rtl.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "reload.h"
#include "insn-config.h"
#include "basic-block.h"
#include <stdio.h>

/* This file contains the reload pass of the compiler, which is
   run after register allocation has been done.  It checks that
   each insn is valid (operands required to be in registers really
   are in registers of the proper class) and fixes up invalid ones
   by copying values temporarily into registers for the insns
   that need them.

   The results of register allocation are described by the vector
   reg_renumber; the insns still contain pseudo regs, but reg_renumber
   can be used to find which hard reg, if any, a pseudo reg is in.

   The technique we always use is to free up a few hard regs that are
   called ``reload regs'', and for each place where a pseudo reg
   must be in a hard reg, copy it temporarily into one of the reload regs.

   All the pseudos that were formerly allocated to the hard regs that
   are now in use as reload regs must be ``spilled''.  This means
   that they go to other hard regs, or to stack slots if no other
   available hard regs can be found.  Spilling can invalidate more
   insns, requiring additional need for reloads, so we must keep checking
   until the process stabilizes.

   For machines with different classes of registers, we must keep track
   of the register class needed for each reload, and make sure that
   we allocate enough reload registers of each class.

   The file reload.c contains the code that checks one insn for
   validity and reports the reloads that it needs.  This file
   is in charge of scanning the entire rtl code, accumulating the
   reload needs, spilling, assigning reload registers to use for
   fixing up each insn, and generating the new insns to copy values
   into the reload registers.  */

/* During reload_as_needed, element N contains a REG rtx for the hard reg
   into which pseudo reg N has been reloaded (perhaps for a previous insn). */
static rtx *reg_last_reload_reg;

/* Element N is the constant value to which pseudo reg N is equivalent,
   or zero if pseudo reg N is not equivalent to a constant.
   find_reloads looks at this in order to replace pseudo reg N
   with the constant it stands for.  */
rtx *reg_equiv_constant;

/* During reload_as_needed, element N contains the last pseudo regno
   reloaded into the Nth reload register.  This vector is in parallel
   with spill_regs.  */
static int reg_reloaded_contents[FIRST_PSEUDO_REGISTER];

/* In parallel with spill_regs, contains REG rtx's for those regs.
   Holds the last rtx used for any given reg, or 0 if it has never
   been used for spilling yet.  This rtx is reused, provided it has
   the proper mode.  */
static rtx spill_reg_rtx[FIRST_PSEUDO_REGISTER];

/* In parallel with spill_regs, contains nonzero for a spill reg
   that was stored after the last time it was used.
   The precise value is the insn generated to do the store.  */
static rtx spill_reg_store[FIRST_PSEUDO_REGISTER];

/* This table is the inverse mapping of spill_regs:
   indexed by hard reg number,
   it contains the position of that reg in spill_regs,
   or -1 for something that is not in spill_regs.  */
static short spill_reg_order[FIRST_PSEUDO_REGISTER];

/* Describes order of use of registers for reloading
   of spilled pseudo-registers.  `spills' is the number of
   elements that are actually valid; new ones are added at the end.  */
static char spill_regs[FIRST_PSEUDO_REGISTER];

/* Describes order of preference for putting regs into spill_regs.
   Contains the numbers of all the hard regs, in order most preferred first.
   This order is different for each function.
   It is set up by order_regs_for_reload.  */
static char potential_reload_regs[FIRST_PSEUDO_REGISTER];

/* Nonzero if spilling (REG n) does not require reloading it into
   a register in order to do (MEM (REG n)).  */

static char spill_indirect_ok;

/* Tables describing and classifying the hardware registers.  */

static HARD_REG_SET reg_class_contents[] = REG_CLASS_CONTENTS;

static enum reg_class reg_class_superclasses[N_REG_CLASSES][N_REG_CLASSES]
  = REG_CLASS_SUPERCLASSES;

/* Indexed by hard register number, contains 1 for registers
   that are fixed use (stack pointer, pc, frame pointer, etc.).
   These are the registers that cannot be used to allocate
   a pseudo reg whose life does not cross calls.  */

static char fixed_regs[] = FIXED_REGISTERS;

/* Indexed by hard register number, contains 1 for registers
   that are fixed use or are clobbered by function calls.
   These are the registers that cannot be used to allocate
   a pseudo reg whose life crosses calls.  */

static char call_clobbered_regs[] = CALL_USED_REGISTERS;

static void reload_as_needed ();
static void choose_reload_targets ();
static void forget_old_reloads ();
static void order_regs_for_reload ();

/* Main entry point for the reload pass, and only entry point
   in this file.

   FIRST is the first insn of the function being compiled.

   GLOBAL nonzero means we were called from global_alloc
   and should attempt to reallocate any pseudoregs that we
   displace from hard regs we will use for reloads.
   If GLOBAL is zero, we do not have enough information to do that,
   so any pseudo reg that is spilled must go to the stack.

   DUMPFILE is the global-reg debugging dump file stream, or 0.
   If it is nonzero, messages are written to it to describe
   which registers are seized as reload regs, which pseudo regs
   are spilled from them, and where the pseudo regs are reallocated to.  */

void
reload (first, global, dumpfile)
     rtx first;
     int global;
     FILE *dumpfile;
{
  register int n_spills;
  register int class;
  register int i;

  int something_changed;
  int something_needs_reloads;

  /* Compute which hard registers are now in use
     as homes for pseudo registers.
     This is done here rather than (eg) in global_alloc
     because this point is reached even if not optimizing.  */

  for (i = FIRST_PSEUDO_REGISTER; i < max_regno; i++)
    mark_home_live (i);

#ifndef REGISTER_CONSTRAINTS
  /* If all the pseudo regs have hard regs,
     except for those that are never referenced,
     we know that no reloads are needed.  */
  /* But that is not true if there are register constraints, since
     in that case some pseudos might be in the wrong kind of hard reg.  */

  for (i = FIRST_PSEUDO_REGISTER; i < max_regno; i++)
    if (reg_renumber[i] == -1 && reg_n_refs[i] != 0)
      break;

  if (i == max_regno)
    return;
#endif

  reg_equiv_constant = (rtx *) alloca (max_regno * sizeof (rtx));
  bzero (reg_equiv_constant, max_regno * sizeof (rtx));

  /* Compute the order of preference for hard registers to spill.
     Store them by decreasing preference in potential_reload_regs.  */

  order_regs_for_reload ();

  /* So far, no hard regs have been spilled.  */
  n_spills = 0;
  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    spill_reg_order[i] = -1;

  /* This loop scans the entire function each go-round
     and repeats until one repetition spills no additional hard regs.  */

  /* This flag is set when a psuedo reg is spilled,
     to require another pass.  Note that getting an additional reload
     reg does not necessarily imply any pseudo reg was spilled;
     sometimes we find a reload reg that no pseudo reg was allocated in.  */
  something_changed = 1;
  /* This flag is set if there are any insns that require reloading.  */
  something_needs_reloads = 0;
  while (something_changed)
    {
      register rtx insn;
      /* For each class, number of reload regs needed in that class.
	 This is the maximum over all insns of the needs in that class
	 of the individual insn.  */
      int max_needs[N_REG_CLASSES];

      something_changed = 0;
      bzero (max_needs, sizeof max_needs);

      /* Compute the most additional registers needed by any instruction.
	 Collect information separately for each class of regs.  */

      for (insn = first; insn; insn = NEXT_INSN (insn))
	{
	  if (GET_CODE (insn) == INSN || GET_CODE (insn) == JUMP_INSN
	      || GET_CODE (insn) == CALL_INSN)
	    {
	      register int r;
	      int insn_needs[N_REG_CLASSES];
	      
	      /* While we are scanning all the insns, note those that
		 initialize a pseudo reg from constant, where the
		 pseudo reg does not have a hard reg and its value
		 never changes.  Delete these insns and record the
		 constant values to be substituted for the pseudoregs.  */

	      if (REG_NOTES (insn) != 0
		  && (enum reg_note) GET_MODE (REG_NOTES (insn)) == REG_CONST)
		{
		  i = REGNO (SET_DEST (PATTERN (insn)));
		  if (reg_renumber[i] < 0)
		    {
		      reg_equiv_constant[i] = SET_SRC (PATTERN (insn));
		      /* This pseudo register needs no stack slot.  */
		      reg_n_refs[i] = 0;
		      /* Delete the insn that loads the pseudo register.  */
		      PUT_CODE (insn, NOTE);
		      NOTE_LINE_NUMBER (insn) = NOTE_INSN_DELETED;
		      NOTE_SOURCE_FILE (insn) = 0;
		      continue;
		    }
		}

	      for (i = 0; i < N_REG_CLASSES; i++)
		insn_needs[i] = 0;

	      /* Analyze the instruction.  */

	      find_reloads (insn, 0, spill_indirect_ok, global, spill_reg_order);

	      if (n_reloads == 0)
		continue;

	      something_needs_reloads = 1;

	      /* Count each reload once in every class
		 containing the reload's own class.  */

	      for (i = 0; i < n_reloads; i++)
		{
		  register enum reg_class *p;
		  /* Don't count the dummy reloads, for which one of the
		     regs mentioned in the insn can be used for reloading.
		     Don't count optional reloads.  */
		  if (reload_reg_rtx[i] != 0
		      || reload_optional[i] != 0)
		    continue;
		  insn_needs[(int) reload_reg_class[i]]++;
		  p = reg_class_superclasses[(int) reload_reg_class[i]];
		  while (*p != LIM_REG_CLASSES)
		    insn_needs[(int) *p++]++;
		}

	      /* Remember for later shortcuts which insns had any reloads.  */

	      PUT_MODE (insn, n_reloads ? QImode : VOIDmode);

	      /* For each class, collect maximum need of any insn */

	      for (i = 0; i < N_REG_CLASSES; i++)
		if (max_needs[i] < insn_needs[i])
		  max_needs[i] = insn_needs[i];
	    }
	  /* Note that there is a continue statement above.  */
	}

      /* Now deduct from the needs for the registers already
	 available (already spilled).  */

      for (i = 0; i < n_spills; i++)
	{
	  register enum reg_class *p;
	  class = (int) REGNO_REG_CLASS (spill_regs[i]);

	  max_needs[class]--;
	  p = reg_class_superclasses[class];
	  while (*p != LIM_REG_CLASSES)
	    max_needs[(int) *p++]--;
	}

      /* If all needs are met, we win.  */

      for (i = 0; i < N_REG_CLASSES; i++)
	if (max_needs[i] > 0)
	  break;
      if (i == N_REG_CLASSES)
	break;

      /* Not all needs are met; must spill more hard regs.
	 Now find more reload regs to satisfy the remaining need.
	 Count them in `spills', and add entries to
	 `spill_regs' and `spill_reg_order'.  */

      for (class = 0; class < N_REG_CLASSES; class++)
	while (max_needs[class] > 0)
	  {
	    register enum reg_class *p;

	    /* Consider the potential reload regs that aren't
	       yet in use as reload regs, in order of preference.
	       Find the most preferred one that's in this class.  */

	    for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
	      if (potential_reload_regs[i] >= 0
		  && TEST_HARD_REG_BIT (reg_class_contents[class],
					potential_reload_regs[i]))
		break;
	    if (i == FIRST_PSEUDO_REGISTER)
	      abort ();	/* No reload reg possible? */

	    /* Make potential_reload_regs[i] an additional reload reg.  */

	    spill_regs[n_spills] = potential_reload_regs[i];
	    spill_reg_order[potential_reload_regs[i]] = n_spills;
	    potential_reload_regs[i] = -1;
	    if (dumpfile)
	      fprintf (dumpfile, "Spilling reg %d.\n", spill_regs[n_spills]);

	    /* Clear off the needs we just satisfied.  */

	    max_needs[class]--;
	    p = reg_class_superclasses[class];
	    while (*p != LIM_REG_CLASSES)
	      max_needs[(int) *p++]--;

	    /* Spill every pseudo reg that was allocated to this reg
	       or to something that overlaps this reg.  */

	    for (i = FIRST_PSEUDO_REGISTER; i < max_regno; i++)
	      if (reg_renumber[i] >= 0
		  && reg_renumber[i] <= spill_regs[n_spills]
		  && (reg_renumber[i] 
		      + HARD_REGNO_NREGS (reg_renumber[i],
					  PSEUDO_REGNO_MODE (i))
		      > spill_regs[n_spills]))
		{
		  /* Mark it as no longer having a hard register home.  */
		  reg_renumber[i] = -1;
		  /* We will need to scan everything again.  */
		  something_changed = 1;
		  if (global)
		    {
		      retry_global_alloc (i, spill_regs[n_spills],
					  spill_reg_order);
		      /* Update regs_ever_live for new home (if any).  */
		      mark_home_live (i);
		    }
		  if (dumpfile)
		    {
		      if (reg_renumber[i] == -1)
			fprintf (dumpfile, " Register %d now on stack.\n", i);
		      else
			fprintf (dumpfile, " Register %d now in %d.\n",
				 i, reg_renumber[i]);
		    }
		  if (dumpfile)
		    fprintf (dumpfile, "\n");
		}
	    regs_ever_live[spill_regs[n_spills]] = 1;
	    n_spills++;
	}
    }

  /* Use the reload registers where necessary
     by generating move instructions to move the must-be-register
     values into or out of the reload registers.  */

  if (something_needs_reloads)
    reload_as_needed (first, n_spills, global);
}

/* Mark the slots in regs_ever_live for the hard regs
   used by pseudo-reg number REGNO.  */

mark_home_live (regno)
     int regno;
{
  register int i, lim;
  i = reg_renumber[regno];
  if (i < 0)
    return;
  lim = i + HARD_REGNO_NREGS (i, PSEUDO_REGNO_MODE (regno));
  while (i < lim)
    regs_ever_live[i++] = 1;
}

struct hard_reg_n_uses { int regno; int uses; };

static int
hard_reg_used_less_p (p1, p2)
     struct hard_reg_n_uses *p1, *p2;
{
  return p1->uses - p2->uses;
}

/* Choose the order to consider regs for use as reload registers
   based on how much trouble would be caused by spilling one.
   Store them in order of decreasing preference in potential_reload_regs.  */

static void
order_regs_for_reload ()
{
  register int i;
  register int o = 0;

  struct hard_reg_n_uses hard_reg_n_uses[FIRST_PSEUDO_REGISTER];

  /* Count number of uses of each hard reg by pseudo regs allocated to it
     and then order them by decreasing use.  */

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    {
      hard_reg_n_uses[i].uses = 0;
      hard_reg_n_uses[i].regno = i;
    }

  for (i = FIRST_PSEUDO_REGISTER; i < max_regno; i++)
    if (reg_renumber[i] >= 0)
      hard_reg_n_uses[reg_renumber[i]].uses += reg_n_refs[i];

  qsort (hard_reg_n_uses, FIRST_PSEUDO_REGISTER,
	 sizeof hard_reg_n_uses[0], hard_reg_used_less_p);

  /* Prefer registers not so far used, for use in temporary loading.
     Among them, prefer registers not preserved by calls.  */

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    if (regs_ever_live[i] == 0 && call_clobbered_regs[i]
	&& ! fixed_regs[i])
      potential_reload_regs[o++] = i;

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    if (regs_ever_live[i] == 0 && ! call_clobbered_regs[i])
      potential_reload_regs[o++] = i;

  /* Now add the regs that are already used,
     preferring those used less often.  */

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    if (regs_ever_live[hard_reg_n_uses[i].regno] != 0)
      potential_reload_regs[o++] = hard_reg_n_uses[i].regno;

#if 0
  /* For regs that are used, don't prefer those not preserved by calls
     because those are likely to contain high priority things
     that are live for short periods of time.  */

  for (i = FIRST_PSEUDO_REGISTER - 1; i >= 0; i--)
    if (regs_ever_live[i] != 0 && ! call_clobbered_regs[i])
      potential_reload_regs[o++] = i;
#endif
}

/* Reload pseudo-registers into hard regs around each insn as needed.
   Additional register load insns are output before the insn that needs it
   and perhaps store insns after insns that modify the reloaded pseudo reg.

   reg_last_reload_reg and reg_reloaded_contents keep track of
   which pseudo-registers are already available in reload registers.
   We update these for the reloads that we perform,
   as the insns are scanned.  */

static void
reload_as_needed (first, n_spills, live_known)
     rtx first;
     int n_spills;
     int live_known;
{
  register rtx insn;
  register int i;

  /* Often (MEM (REG n)) is still valid even if (REG n) is put on the stack.
     Set spill_indirect_ok if so.  */
  register rtx tem
    = gen_rtx (MEM, SImode,
	       gen_rtx (PLUS, Pmode,
			gen_rtx (REG, Pmode, FRAME_POINTER_REGNUM),
			gen_rtx (CONST_INT, VOIDmode, 4)));

  spill_indirect_ok = 1;
  GO_IF_LEGITIMATE_ADDRESS (QImode, tem, wins);
  spill_indirect_ok = 0;
wins:

  bzero (spill_reg_rtx, sizeof spill_reg_rtx);
  reg_last_reload_reg = (rtx *) alloca (max_regno * sizeof (rtx));
  bzero (reg_last_reload_reg, max_regno * sizeof (rtx));
  for (i = 0; i < n_spills; i++)
    reg_reloaded_contents[i] = -1;

  for (insn = first; insn;)
    {
      register rtx next = NEXT_INSN (insn);
      if (GET_CODE (insn) == INSN || GET_CODE (insn) == JUMP_INSN
	  || GET_CODE (insn) == CALL_INSN)
	{
	  if (GET_MODE (insn) == VOIDmode)
	    n_reloads = 0;
	  /* First find the pseudo regs that must be reloaded for this insn.
	     This info is returned in the tables reload_... (see reload.h).
	     Also modify the body of INSN by substituting RELOAD
	     rtx's for those pseudo regs.  */
	  else
	    find_reloads (insn, 1, spill_indirect_ok, live_known, spill_reg_order);

	  if (n_reloads > 0)
	    {
	      /* Now compute which reload regs to reload them into.  Perhaps
		 reusing reload regs from previous insns, or else output
		 load insns to reload them.  Maybe output store insns too.
		 Record the choices of reload reg in reload_reg_rtx.  */
	      choose_reload_targets (insn, n_spills);
	      /* Substitute the chosen reload regs from reload_reg_rtx
		 into the insn's body (or perhaps into the bodies of other
		 load and store insn that we just made for reloading
		 and that we moved the structure into).  */
	      subst_reloads ();
	    }
	  /* Any previously reloaded spilled pseudo reg, stored in this insn,
	     is no longer validly lying around to save a future reload.
	     Note that this does not detect pseudos that were reloaded
	     for this insn in order to be stored in
	     (obeying register constraints).  That is correct; such reload
	     registers ARE still valid.  */
	  forget_old_reloads (PATTERN (insn));
	}
      /* Don't assume a spilled reg is still good after a call insn;
	 it could be (and probably is) a call_clobbered_reg.  */
      if (GET_CODE (insn) == CODE_LABEL || GET_CODE (insn) == CALL_INSN)
	{
	  for (i = 0; i < n_spills; i++)
	    reg_reloaded_contents[i] = -1;
	}
      insn = next;
    }
}

/* If we see a pseudo-reg being stored into,
   don't try to reuse an old reload reg
   which previously contained a copy of it.  */

static void
forget_old_reloads (x)
     rtx x;
{
  if (GET_CODE (x) == SET && GET_CODE (SET_DEST (x)) == REG)
    {
      register int regno = REGNO (SET_DEST (x));
      reg_last_reload_reg[regno] = 0;
    }
  else if (GET_CODE (x) == PARALLEL)
    {
      register int i;
      for (i = 0; i < XVECLEN (x, 0); i++)
	{
	  register rtx y = XVECEXP (x, 0, i);
	  if (GET_CODE (y) == SET && GET_CODE (SET_DEST (y)) == REG)
	    {
	      register int regno = REGNO (SET_DEST (y));
	      reg_last_reload_reg[regno] = 0;
	    }
	}
    }
}

static int
reload_reg_class_lower_p (p1, p2)
     short *p1, *p2;
{
  register int r1 = *p1, r2 = *p2;
  /* Consider required reloads before optional ones.  */
  register int t = reload_optional[r1] - reload_optional[r2];
  if (t) return t;
  return (int) reload_reg_class[r1] - (int) reload_reg_class[r2];
}

/* Assign hard reg targets for the pseudo-registers we must reload
   into hard regs for this insn.
   Also output the instructions to copy them in and out of the hard regs.

   For machines with register classes, we are responsible for
   finding a reload reg in the proper class.  */

static void
choose_reload_targets (insn, n_spills)
     rtx insn;
     int n_spills;
{
  register int j;
  char reload_reg_in_use[FIRST_PSEUDO_REGISTER];
  short reload_order[FIRST_PSEUDO_REGISTER];
  char reload_inherited[FIRST_PSEUDO_REGISTER];

/* For each reload, the index in spill_regs of the spill register used,
   or -1 if we did not need one of the spill registers for this reload.  */
  int reload_spill_index[FIRST_PSEUDO_REGISTER];

  bzero (reload_inherited, FIRST_PSEUDO_REGISTER);
  bzero (reload_reg_in_use, FIRST_PSEUDO_REGISTER);

  /* In order to be certain of getting the registers we need,
     we must sort the reloads into order of increasing register class.
     Then our grabbing of reload registers will parallel the process
     that provided the reload registers.  */

  /* This used to look for an existing reloaded home for all
     of the reloads, and only then perform any new reloads.
     But that could lose if the reloads were done out of reg-class order
     because a later reload with a looser constraint might have an old
     home in a register needed by an earlier reload with a tighter constraint.
     It would be possible with even hairier code to detect such cases
     and handle them, but it doesn't seem worth while yet.  */

  for (j = 0; j < n_reloads; j++)
    {
      reload_order[j] = j;
      reload_spill_index[j] = -1;
    }

  if (n_reloads > 1)
    qsort (reload_order, n_reloads, sizeof (short), reload_reg_class_lower_p);

  for (j = 0; j < n_reloads; j++)
    {
      register int r = reload_order[j];
      register int i;
      register rtx new;

      /* No need to find a reload-register if find_reloads chose one.  */

      if (reload_reg_rtx[r] != 0)
	continue;

      /* First see if this pseudo is already available as reloaded
	 for a previous insn.  */

      {
	register int regno;
	if (reload_in[r] != 0
	    && GET_CODE (reload_in[r]) == REG
	    && (regno = REGNO (reload_in[r]),
		reg_last_reload_reg[regno]))
	  {
	    i = spill_reg_order[REGNO (reg_last_reload_reg[regno])];

	    if (reg_reloaded_contents[i] == regno
		&& TEST_HARD_REG_BIT (reg_class_contents[(int) reload_reg_class[r]],
				      spill_regs[i]))
	      {
		/* Mark the reload register as in use for this insn.  */
		reload_reg_rtx[r] = reg_last_reload_reg[regno];
		reload_reg_in_use[i] = 1;
		reload_inherited[r] = 1;
		reload_spill_index[r] = i;
	      }
	  }
      }

      /* If this is not a pseudo, here's a different way to see
	 if it is already lying around.  */
      if (reload_in[r] != 0 && CONSTANT_ADDRESS_P (reload_in[r]))
	{
	  register rtx equiv
	    = find_equiv_reg (reload_in[r], insn, reload_reg_class[r], -1, 0);
	  /* If we found an equivalent reg, say no code need be generated
	     to load it, and use it as our reload reg.  */
	  if (equiv != 0)
	    {
	      reload_reg_rtx[r] = equiv;
	      reload_inherited[r] = 1;
	      /* If it is a spill reg,
		 mark the spill reg as in use for this insn.  */
	      if ((i = spill_reg_order[REGNO (equiv)]) != 0)
		{
		  reload_reg_in_use[i] = 1;
		}
	    }
	}

      /* If it isn't lying around, and isn't optional,
	 find a place to reload it into.  */
      if (reload_reg_rtx[r] != 0 || reload_optional[r] != 0)
	continue;

      /* Value not lying around; find a register to reload it into.
	 Here I is not a regno, it is an index into spill_regs.  */
      for (i = 0; i < n_spills; i++)
	{
	  if (reload_reg_in_use[i] == 0
	      && TEST_HARD_REG_BIT (reg_class_contents[(int) reload_reg_class[r]],
				    spill_regs[i]))

	    break;
	}

      if (i == n_spills)
	abort ();

      reload_reg_in_use[i] = 1;
      new = spill_reg_rtx[i];
      if (new == 0 || GET_MODE (new) != reload_mode[r])
	spill_reg_rtx[i] = new = gen_rtx (REG, reload_mode[r], spill_regs[i]);

      reload_reg_rtx[r] = new;
      reload_spill_index[r] = i;
      reg_reloaded_contents[i] = -1;
    }

  /* Now for all the spill regs newly used in this instruction,
     record what psuedo-regs they contain copies of,
     to save code for reloads of subsequent instructions.
     If reload_reg_rtx[r] is 0, this is an optional reload
     that we opted to ignore.  */
  for (j = 0; j < n_reloads; j++)
    {
      register int r = reload_order[j];
      register int i = reload_spill_index[r];

      if (i >= 0 && reload_reg_rtx[r] != 0)
	{
	  if (reload_out[r] != 0 && GET_CODE (reload_out[r]) == REG)
	    {
	      register int nregno = REGNO (reload_out[r]);
	      reg_last_reload_reg[nregno] = reload_reg_rtx[r];
	      reg_reloaded_contents[i] = nregno;
	    }
	  if (reload_out[r] == 0 && GET_CODE (reload_in[r]) == REG)
	    {
	      register int nregno = REGNO (reload_in[r]);
	      reg_last_reload_reg[nregno] = reload_reg_rtx[r];
	      reg_reloaded_contents[i] = nregno;
	    }
	}
    }

  /* Now output the instructions to copy the data into and out of the
     reload registers.  Do these in the order that the reloads were reported,
     since reloads of base and index registers precede reloads of operands
     and the operands may need the base and index registers reloaded.  */

  for (j = 0; j < n_reloads; j++)
    {
      register rtx old;

      old = reload_in[j];
      if (old != 0 && ! reload_inherited[j]
	  && reload_reg_rtx[j] != old
	  && reload_reg_rtx[j] != 0)
	{
	  register rtx reloadreg = reload_reg_rtx[j];
	  /* Encapsulate RELOADREG so its machine mode matches what
	     is being copied into it.
	     Except, if OLD is multiple regs and we are only using one,
	     reload only that one.  */
	  if (GET_CODE (old) == SUBREG
	      && !(GET_MODE_SIZE (GET_MODE (SUBREG_REG (old))) > UNITS_PER_WORD
		   && (GET_MODE_SIZE (GET_MODE (SUBREG_REG (old)))
		       > GET_MODE_SIZE (GET_MODE (old)))))
	    old = SUBREG_REG (old);
	  if (GET_MODE (old) != VOIDmode
	      && GET_MODE (reloadreg) != GET_MODE (old))
	    reloadreg = gen_rtx (SUBREG, GET_MODE (old), reloadreg, 0);
	  emit_insn_before (gen_move_insn (reloadreg, old), insn);
	  /* If this reload wants reload_in[j] incremented by a constant,
	     output code to get this done before the insn reloaded for.  */
	  if (reload_inc[j] != 0)
	    {
	      /* If reload_in[j] is a register, assume we can
		 output an insn to increment it directly.  */
	      if (GET_CODE (old) == REG &&
		  (REGNO (old) < FIRST_PSEUDO_REGISTER
		   || reg_renumber[REGNO (old)] >= 0))
		emit_insn_before (gen_add2_insn (old,
						 gen_rtx (CONST_INT, VOIDmode,
							  reload_inc[j])),
				  insn);
	      else
		/* Else we must not assume we can increment reload_in[j]
		   (even though on many target machines we can);
		   increment the copy in the reload register,
		   save that back, then decrement the reload register
		   so it has its original contents.  */
		{
		  emit_insn_before (gen_add2_insn (reloadreg,
						   gen_rtx (CONST_INT, VOIDmode,
							    reload_inc[j])),
				    insn);
		  emit_insn_before (gen_move_insn (old, reloadreg), insn);
		  emit_insn_before (gen_sub2_insn (reloadreg,
						   gen_rtx (CONST_INT, VOIDmode,
							    reload_inc[j])),
				    insn);
		}
	    }
	}
      /* If we are reloading a register that was recently
	 reloaded for writing, see if we can prove there was
	 actually no need to store the old value in it.  */
      if (reload_inherited[j] && reload_spill_index[j] >= 0
	  && spill_reg_store[reload_spill_index[j]] != 0
	  && dead_or_set_p (insn, reload_in[j]))
	{
	  register rtx i1;
	  /* If the spilled-reg we are reloading is no longer referenced
	     anywhere between the store into it and here,
	     we can delete that store.  */
	  for (i1 = NEXT_INSN (spill_reg_store[reload_spill_index[j]]);
	       i1 != insn; i1 = NEXT_INSN (i1))
	    if ((GET_CODE (i1) == INSN || GET_CODE (i1) == JUMP_INSN
		 || GET_CODE (i1) == CALL_INSN)
		&& reg_mentioned_p (reload_in[j], PATTERN (i1)))
	      break;
	  if (i1 == insn)
	    {
	      delete_insn (spill_reg_store[reload_spill_index[j]]);
	      /* See if the spilled-reg has been completely replaced
		 with reload regs.  If so, set its reg_n_refs to 0
		 so no stack slot will be made for it.  */
	      if (reg_n_deaths[REGNO (reload_in[j])] == 1
		  && reg_basic_block[REGNO (reload_in[j])] >= 0)
		{
		  /* We know that it was used only between here
		     and the beginning of the current basic block.
		     Search that range; see if any ref remains.  */
		  for (i1 = PREV_INSN (insn); i1; i1 = PREV_INSN (i1))
		    {
		      if (GET_CODE (i1) == CODE_LABEL
			  || GET_CODE (i1) == JUMP_INSN)
			break;
		      if ((GET_CODE (i1) == INSN || GET_CODE (i1) == CALL_INSN)
			  && reg_mentioned_p (reload_in[j], PATTERN (i1)))
			goto still_used;
		    }
		  reg_n_refs[REGNO (reload_in[j])] = 0;
		still_used: ;
		}
	    }
	}

      old = reload_out[j];
      if (old != 0
	  && reload_reg_rtx[j] != old
	  && reload_reg_rtx[j] != 0)
	{
	  register rtx reloadreg = reload_reg_rtx[j];
	  if (GET_CODE (old) == SUBREG
	      && !(GET_MODE_SIZE (GET_MODE (SUBREG_REG (old))) > UNITS_PER_WORD
		   && (GET_MODE_SIZE (GET_MODE (SUBREG_REG (old)))
		       > GET_MODE_SIZE (GET_MODE (old)))))
	    old = SUBREG_REG (old);
	  if (GET_MODE (reloadreg) != GET_MODE (old))
	    reloadreg = gen_rtx (SUBREG, GET_MODE (old), reloadreg, 0);
	  spill_reg_store[reload_spill_index[j]]
	    = emit_insn_after (gen_move_insn (old, reloadreg), insn);
	}
      else
	spill_reg_store[reload_spill_index[j]] = 0;
    }
}
