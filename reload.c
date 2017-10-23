/* Search an insn for pseudo regs that must be in hard regs and are not.
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


/* This file contains subroutines used only from the file reload1.c.
   It knows how to scan one insn for operands and values
   that need to be copied into registers to make valid code.
   It also finds other operands and values which are valid
   but for which equivalent values in registers exist and
   ought to be used instead.

   Before processing the first insn of the function, call `init_reload'.

   To scan an insn, call `find_reloads'.  This does two things:
   1. sets up tables describing which values must be reloaded
   for this insn, and what kind of hard regs they must be reloaded into;
   2. optionally record the locations where those values appear in
   the data, so they can be replaced properly later.
   This is done only if the second arg to `find_reloads' is nonzero.

   The third arg to `find_reloads' specifies the value of `indirect_ok'.

   Then you must choose the hard regs to reload those pseudo regs into,
   and generate appropriate load insns before this insn and perhaps
   also store insns after this insn.  Set up the array `reload_reg_rtx'
   to contain the REG rtx's for the registers you used.  In some
   cases `find_reloads' will return a nonzero value in `reload_reg_rtx'
   for certain reloads.  Then that tells you which register to use,
   so you do not need to allocate one.  But you still do need to add extra
   instructions to copy the value into and out of that register.

   Finally you must call `subst_reloads' to substitute the reload reg rtx's
   into the locations already recorded.

NOTE SIDE EFFECTS:

   find_reloads can alter the operands of the instruction it is called on.

   1. Any uses of VOLATILE are removed, since it no longer matters.

   2. Pseudo-registers that are equivalent to constants are replaced
   with those constants if they are not in hard registers.

   3. Two operands of any sort may be interchanged, if they are in a
   commutative instruction.
   This happens only if find_reloads thinks the instruction will compile
   better that way.

   In all of these cases, calling find_reloads a second time immediately
   after would produce no further change.
*/

/* Tell config.h to use the strict definitions of REG_OK_FOR_INDEX_P, etc.
   The strict definitions reject pseudo regs.  */

#define REG_OK_STRICT

#include "config.h"
#include "rtl.h"
#include "insn-config.h"
#include "recog.h"
#include "reload.h"
#include "regs.h"
#include "hard-reg-set.h"

extern char insn_operand_address_p[][MAX_RECOG_OPERANDS];

/* The variables set up by `find_reloads' are:

   n_reloads		  number of distinct reloads needed; max reload # + 1
       tables indexed by reload number
   reload_in		  rtx for value to reload from
   reload_out		  rtx for where to store reload-reg afterward if nec
			   (often the same as reload_in)
   reload_reg_class	  enum reg_class, saying what regs to reload into
   reload_mode		  enum machine_mode; mode this operand should have
			   when reloaded.
   reload_optional	  char, nonzero for an optional reload.
			   Optional reloads are ignored unless the
			   value is already sitting in a register.
   reload_inc		  int, amount to increment reload_in by
			   before this insn.
   reload_reg_rtx	  rtx.  This is the register to reload into.
			   If it is zero when `find_reloads' returns,
			   you must find a suitable register in the class
			   specified by reload_reg_class, and store here
			   an rtx for that register with mode from reload_mode.
*/

int n_reloads;

rtx reload_in[FIRST_PSEUDO_REGISTER];
rtx reload_out[FIRST_PSEUDO_REGISTER];
enum reg_class reload_reg_class[FIRST_PSEUDO_REGISTER];
enum machine_mode reload_mode[FIRST_PSEUDO_REGISTER];
rtx reload_reg_rtx[FIRST_PSEUDO_REGISTER];
char reload_optional[FIRST_PSEUDO_REGISTER];
int reload_inc[FIRST_PSEUDO_REGISTER];

/* Replacing reloads.

   If `replace_reloads' is nonzero, then as each reload is recorded
   an entry is made for it in the table `replacements'.
   Then later `subst_reloads' can look through that table and
   perform all the replacements needed.  */

/* Nonzero means record the places to replace.  */
static int replace_reloads;

/* Each replacement is recorded with a structure like this.  */
struct replacement
{
  rtx *where;			/* Location to store in */
  int what;			/* which reload this is for */
  enum machine_mode mode;	/* mode it must have */
};

static struct replacement replacements[MAX_RECOG_OPERANDS * ((MAX_REGS_PER_ADDRESS * 2) + 1)];

/* Number of replacements currently recorded.  */
static int n_replacements;

/* The instruction we are doing reloads for;
   so we can test whether a register dies in it.  */
static rtx this_insn;

/* Nonzero means (MEM (REG n)) is valid even if (REG n) is spilled.  */
static int indirect_ok;

/* If hard_regs_live_known is nonzero,
   we can tell which hard regs are currently live,
   at least enough to succeed in choosing dummy reloads.  */
static int hard_regs_live_known;

/* Indexed by hard reg number,
   element is nonegative if hard reg has been spilled.
   This vector is passed to `find_reloads' as an argument
   and is not changed here.  */
static short *static_reload_reg_p;

static enum reg_class reg_class_subunion[N_REG_CLASSES][N_REG_CLASSES]
  = REG_CLASS_SUBUNION;

static HARD_REG_SET reg_class_contents[] = REG_CLASS_CONTENTS;

static char call_clobbered_regs[] = CALL_USED_REGISTERS;

static rtx find_reloads_toplev ();
static void find_reloads_address ();
static void find_reloads_address_1 ();
static int reg_dead_here_p ();
static int hard_reg_dead_here_p ();
static int hard_reg_set_here_p ();
static int refers_to_regno_p ();
static rtx forget_volatility ();
static rtx subst_reg_equivs ();
rtx find_equiv_reg ();
static int find_inc_amount ();

/* Record one reload that needs to be performed.
   IN is an rtx saying where the data are to be found before this instruction.
   OUT says where they must be stored after the instruction.
   (IN is zero for data not read, and OUT is zero for data not written.)
   INLOC and OUTLOC point to the places in the instructions where
   IN and OUT were found.
   CLASS is a register class required for the reloaded data.
   INMODE is the machine mode that the instruction requires
   for the reg that replaces IN and OUTMODE is likewise for OUT.

   If IN is zero, then OUT's location and mode should be passed as
   INLOC and INMODE.

   OPTIONAL nonzero means this reload does not need to be performed:
   it can be discarded if that is more convenient.  */
   
static int
push_reload (in, out, inloc, outloc, class, inmode, outmode, optional)
     register rtx in, out;
     rtx *inloc, *outloc;
     enum reg_class class;
     enum machine_mode inmode, outmode;
     int optional;
{
  register int i;
  enum machine_mode mode = inmode;
  if (outloc != 0 && GET_MODE_SIZE (outmode) > GET_MODE_SIZE (mode))
    mode = outmode;

  /* If IN is a pseudo register everywhere-equivalent to a constant, and 
     it is not in a hard register, reload straight from the constant,
     since we want to get rid of such pseudo registers.  */
  if (in != 0 && GET_CODE (in) == REG)
    {
      register int regno = REGNO (in);

      if (regno >= FIRST_PSEUDO_REGISTER && reg_renumber[regno] < 0
	  && reg_equiv_constant[regno] != 0)
	in = reg_equiv_constant[regno];
    }

  /* Likewise for OUT.  Of course, OUT will never be equivalent to
     an actual constant, but it might be equivalent to a memory location
     (in the case of a parameter).  */
  if (out != 0 && GET_CODE (out) == REG)
    {
      register int regno = REGNO (out);

      if (regno >= FIRST_PSEUDO_REGISTER && reg_renumber[regno] < 0
	  && reg_equiv_constant[regno] != 0)
	out = reg_equiv_constant[regno];
    }

  if (class == NO_REGS)
    abort ();

  /* Narrow down the class of register wanted if that is
     desirable on this machine for efficiency.  */
  if (in != 0)
    class = PREFERRED_RELOAD_CLASS(in, class);

  /* We can use an existing reload if the class is right
     and at least one of IN and OUT is a match
     and the other is at worst neutral.
     (A zero compared against anything is neutral.)  */
  for (i = 0; i < n_reloads; i++)
    if (reload_reg_class[i] == class
	&& ((in != 0 && reload_in[i] == in
	     && (out == 0 || reload_out[i] == 0 || reload_out[i] == out))
	    ||
	    (out != 0 && reload_out[i] == out
	     && (in == 0 || reload_in[i] == 0 || reload_in[i] == in))))
      break;

  if (i == n_reloads)
    {
      /* We found no existing reload suitable for re-use.
	 So add an additional reload.  */

      reload_in[i] = in;
      reload_out[i] = out;
      reload_reg_class[i] = class;
      reload_mode[i] = mode;
      reload_reg_rtx[i] = 0;
      reload_optional[i] = optional;
      reload_inc[i] = 0;

      n_reloads++;
    }
  else
    {
      /* We are reusing an existing reload,
	 but we may have additional information for it.
	 For example, we may now have both IN and OUT
	 while the old one may have just one of them.  */

      if (GET_MODE_SIZE (mode) > GET_MODE_SIZE (reload_mode[i]))
	reload_mode[i] = mode;
      if (in != 0)
	reload_in[i] = in;
      if (out != 0)
	reload_out[i] = out;
      reload_optional[i] &= optional;
    }

  /* If this is an IN/OUT reload in an insn that sets the CC,
     it must be for an autoincrement.  It doesn't work to store
     the incremented value after the insn because that would clobber the CC.
     So we must do the increment of the value reloaded from,
     increment it, store it back, then decrement again.  */
  if (out != 0 && GET_CODE (PATTERN (this_insn)) == SET
      && SET_DEST (PATTERN (this_insn)) == cc0_rtx)
    {
      out = 0;
      reload_out[i] = 0;
      reload_inc[i] = find_inc_amount (PATTERN (this_insn), in);
      /* If we did not find a nonzero amount-to-increment-by,
	 that contradicts the belief that IN is being incremented
	 in an address in this insn.  */
      if (reload_inc[i] == 0)
	abort ();
    }

  /* If we will replace IN and OUT with the reload-reg,
     record where they are located so that substitution need
     not do a tree walk.  */

  if (replace_reloads)
    {
      if (inloc != 0)
	{
	  register struct replacement *r = &replacements[n_replacements++];
	  r->what = i;
	  r->where = inloc;
	  r->mode = inmode;
	}
      if (outloc != 0 && outloc != inloc)
	{
	  register struct replacement *r = &replacements[n_replacements++];
	  r->what = i;
	  r->where = outloc;
	  r->mode = outmode;
	}
    }

  /* If this reload is just being introduced and it has both
     an incoming quantity and an outgoing quantity that are
     supposed to be made to match, see if either one of the two
     can serve as the place to reload into.

     If one of them is acceptable, set reload_reg_rtx[i]
     to that one.  */

  if (in != 0 && out != 0 && in != out && reload_reg_rtx[i] == 0)
    {
      /* See if OUT will do.  */
      while (GET_CODE (out) == SUBREG)
	out = SUBREG_REG (out);
      if (GET_CODE (out) == REG)
	{
	  register int regno = REGNO (out);

	  /* When we consider whether the insn uses OUT,
	     ignore references within IN.  They don't prevent us
	     from copying IN into OUT, because those refs would
	     move into the insn that reloads IN.

	     However, we only ignore IN in its role as this operand.
	     If the insn uses IN elsewhere and it contains OUT,
	     that counts.  We can't be sure it's the "same" operand
	     so it might not go through this reload.  */
	  *inloc = const0_rtx;

	  if (reg_renumber[regno] >= 0)
	    regno = reg_renumber[regno];
	  if (regno < FIRST_PSEUDO_REGISTER
	      && ! refers_to_regno_p (regno, PATTERN (this_insn), outloc)
	      && TEST_HARD_REG_BIT (reg_class_contents[(int) reload_reg_class[i]], regno))
	    {
	      reload_reg_rtx[i] = out;
	      /* If the outgoing register already contains the same value
		 as the incoming one, we can dispense with loading it.
		 The easiest way to tell the caller that is to give a phony
		 value for the incoming operand (same as outgoing one).  */
	      if ((GET_CODE (in) == REG || CONSTANT_ADDRESS_P (in))
		  && 0 != find_equiv_reg (in, this_insn, 0, REGNO (out),
					  static_reload_reg_p))
		reload_in[i] = out;
	    }

	  *inloc = in;
	}

      while (GET_CODE (in) == SUBREG)
	in = SUBREG_REG (in);

      /* Consider using IN if OUT was not acceptable
	 or if OUT dies in this insn (like the quotient in a divmod insn).
	 We can't use IN unless it is free after this insn,
	 which means we must know accurately which hard regs are live.  */
      if (hard_regs_live_known
	  && GET_CODE (in) == REG
	  && (reload_reg_rtx[i] == 0
	      || reg_dead_here_p (REGNO (reload_reg_rtx[i]), this_insn)))
	{
	  register int regno = REGNO (in);
	  if (reg_dead_here_p (regno, this_insn))
	    {
	      if (reg_renumber[regno] >= 0)
		regno = reg_renumber[regno];
	      if (regno < FIRST_PSEUDO_REGISTER
		  && ! hard_reg_set_here_p (regno, PATTERN (this_insn))
		  && TEST_HARD_REG_BIT (reg_class_contents[(int) reload_reg_class[i]], regno))
		{
		  /* If we were going to use OUT as the reload reg
		     and changed our mind, it means OUT is a dummy that
		     dies here.  So don't bother copying value to it.  */
		  if (reload_reg_rtx[i] == out)
		    reload_out[i] = 0;
		  reload_reg_rtx[i] = in;
		}
	    }
	}
    }

  return i;
}

/* Record an additional place we must replace a value
   for which we have already recorded a reload.
   RELOADNUM is the value returned by push_reload
   when the reload was recorded.
   This is used in insn patterns that use match_dup.  */

static void
push_replacement (loc, reloadnum, mode)
     rtx *loc;
     int reloadnum;
     enum machine_mode mode;
{
  if (replace_reloads)
    {
      register struct replacement *r = &replacements[n_replacements++];
      r->what = reloadnum;
      r->where = loc;
      r->mode = mode;
    }
}

/* This page contains subroutines used mainly for determining
   whether the IN or an OUT of a reload can serve as the
   reload register.  */

/* Return 1 if hard reg number REGNO is on INSN's dead list,
   either explicitly or in the guise of a pseudo-reg allocated to REGNO.  */

static int
hard_reg_dead_here_p (regno, insn)
     register int regno;
     rtx insn;
{
  register rtx link;

  for (link = REG_NOTES (insn); link; link = XEXP (link, 1))
    if ((enum reg_note) GET_MODE (link) == REG_DEAD)
      {
	register int r = REGNO (XEXP (link, 0));
	if (reg_renumber[r] >= 0)
	  r = reg_renumber[r];
	if (r == regno)
	  return 1;
      }

  return 0;
}

/* Return 1 if hard reg number REGNO is stored in by expression X,
   either explicitly or in the guise of a pseudo-reg allocated to REGNO.
   X should be the body of an instruction.  */

static int
hard_reg_set_here_p (regno, x)
     register int regno;
     rtx x;
{
  if (GET_CODE (x) == SET)
    {
      register rtx op0 = SET_DEST (x);
      if (GET_CODE (op0) == REG)
	{
	  register int r = REGNO (op0);
	  if (reg_renumber[r] >= 0)
	    r = reg_renumber[r];
	  if (r == regno)
	    return 1;
	}
    }
  else if (GET_CODE (x) == PARALLEL)
    {
      register int i = XVECLEN (x, 0) - 1;
      for (; i >= 0; i--)
	if (hard_reg_set_here_p (regno, XVECEXP (x, 0, i)))
	  return 1;
    }

  return 0;
}

/* Return nonzero if register number REGNO is explicitly on the
   dead-list of INSN.  */

static int
reg_dead_here_p (regno, insn)
     register int regno;
     rtx insn;
{
  register rtx link;

  for (link = REG_NOTES (insn); link; link = XEXP (link, 1))
    if ((enum reg_note) GET_MODE (link) == REG_DEAD
	&& REGNO (XEXP (link, 0)) == regno)
      return 1;

  return 0;
}

/* Return nonzero if hard register REGNO appears 
   either explicitly or implicitly in X
   other than being stored into.

   References contained within the substructure at LOC do not count.  */

static int
refers_to_regno_p (regno, x, loc)
     int regno;
     rtx x;
     rtx *loc;
{
  register int i;
  register RTX_CODE code;
  register char *fmt;

 repeat:
  code = GET_CODE (x);
  if (code == REG)
    {
      i = REGNO (x);
      if (reg_renumber[i] >= 0)
	i = reg_renumber[i];
      return i == regno;
    }

  if (code == SET)
    {
      if (GET_CODE (SET_DEST (x)) != REG
	  && refers_to_regno_p (regno, SET_DEST (x), loc))
	return 1;
      if (loc == &SET_SRC (x))
	return 0;
      x = SET_SRC (x);
      goto repeat;
    }

  /* X does not match, so try its subexpressions.  */

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e' && loc != &XEXP (x, i))
	{
	  if (i == 0)
	    {
	      x = XEXP (x, 0);
	      goto repeat;
	    }
	  else
	    if (refers_to_regno_p (regno, XEXP (x, i), loc))
	      return 1;
	}
      else if (fmt[i] == 'E')
	{
	  register int j;
	  for (j = XVECLEN (x, i) - 1; j >=0; j--)
	    if (loc != &XVECEXP (x, i, j)
		&& refers_to_regno_p (regno, XVECEXP (x, i, j), loc))
	      return 1;
	}
    }
  return 0;
}

/* Like rtx_equal_p except that it considers two REGs as equal
   if they renumber to the same value.  */

int
rtx_renumbered_equal_p (x, y)
     rtx x, y;
{
  register int i;
  register RTX_CODE code = GET_CODE (x);
  register char *fmt;
      
  if (x == y)
    return 1;
  if ((code == REG || (code == SUBREG && GET_CODE (SUBREG_REG (x)) == REG))
      && (GET_CODE (y) == REG || (GET_CODE (y) == SUBREG
				  && GET_CODE (SUBREG_REG (y)) == REG)))
    {
      register int j;
      if (code == SUBREG)
	{
	  i = REGNO (SUBREG_REG (x));
	  if (reg_renumber[i] >= 0)
	    i = reg_renumber[i];
	  i += SUBREG_WORD (x);
	}
      else
	{
	  i = REGNO (x);
	  if (reg_renumber[i] >= 0)
	    i = reg_renumber[i];
	}
      if (GET_CODE (y) == SUBREG)
	{
	  j = REGNO (SUBREG_REG (y));
	  if (reg_renumber[j] >= 0)
	    j = reg_renumber[j];
	  j += SUBREG_WORD (y);
	}
      else
	{
	  j = REGNO (y);
	  if (reg_renumber[j] >= 0)
	    j = reg_renumber[j];
	}
      return i == j;
    }
  if (code != GET_CODE (y))
    return 0;
  if (code == LABEL_REF)
    return XEXP (x, 0) == XEXP (y, 0);
  if (code == SYMBOL_REF)
    return XSTR (x, 0) == XSTR (y, 0);

  /* (MULT:SI x y) and (MULT:HI x y) are NOT equivalent.  */

  if (GET_MODE (x) != GET_MODE (y))
    return 0;

  /* Compare the elements.  If any pair of corresponding elements
     fail to match, return 0 for the whole things.  */

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      switch (fmt[i])
	{
	case 'i':
	  if (XINT (x, i) != XINT (y, i))
	    return 0;
	  break;

	case 'e':
	  if (rtx_renumbered_equal_p (XEXP (x, i), XEXP (y, i)) == 0)
	    return 0;
	  break;

	case '0':
	  break;

	  /* It is believed that rtx's at this level will never
	     contain anything but integers and other rtx's,
	     except for within LABEL_REFs and SYMBOL_REFs.  */
	default:
	  abort ();
	}
    }
  return 1;
}

/* Main entry point of this file: search the body of INSN
   for values that need reloading and record them with push_reload.
   REPLACE nonzero means record also where the values occur
   so that subst_reloads can be used.
   IND_OK says that a memory reference is a valid memory address.

   LIVE_KNOWN says we have valid information about which hard
   regs are live at each point in the program; this is true when
   we are called from global_alloc but false when stupid register
   allocation has been done.

   RELOAD_REG_P if nonzero is a vector indexed by hard reg number
   which is nonzero if the reg has been commandeered for reloading into.
   It is copied into STATIC_RELOAD_REG_P and referenced from there
   by various subroutines.  */

void
find_reloads (insn, replace, ind_ok, live_known, reload_reg_p)
     rtx insn;
     int replace, ind_ok;
     int live_known;
     short *reload_reg_p;
{
#ifdef REGISTER_CONSTRAINTS

  enum reload_modified { RELOAD_NOTHING, RELOAD_READ, RELOAD_READ_WRITE, RELOAD_WRITE };

  register int insn_code_number;
  register int i;
  int noperands;
  char *constraints[MAX_RECOG_OPERANDS];
  int this_alternative[MAX_RECOG_OPERANDS];
  char this_alternative_win[MAX_RECOG_OPERANDS];
  int this_alternative_matches[MAX_RECOG_OPERANDS];
  int swapped;
  int goal_alternative[MAX_RECOG_OPERANDS];
  int this_alternative_number;
  int goal_alternative_number;
  int operand_reloadnum[MAX_RECOG_OPERANDS];
  int goal_alternative_matches[MAX_RECOG_OPERANDS];
  int goal_alternative_matched[MAX_RECOG_OPERANDS];
  char goal_alternative_win[MAX_RECOG_OPERANDS];
  int goal_alternative_swapped;
  enum reload_modified modified[MAX_RECOG_OPERANDS];
  int best;
  int commutative;
  char operands_match[MAX_RECOG_OPERANDS][MAX_RECOG_OPERANDS];
  rtx body = PATTERN (insn);

  this_insn = insn;
  n_reloads = 0;
  n_replacements = 0;
  replace_reloads = replace;
  indirect_ok = ind_ok;
  hard_regs_live_known = live_known;
  static_reload_reg_p = reload_reg_p;

  if (GET_CODE (body) == USE || GET_CODE (body) == CLOBBER
      || GET_CODE (body) == ASM_INPUT
      || GET_CODE (body) == ADDR_VEC || GET_CODE (body) == ADDR_DIFF_VEC)
    return;

  forget_volatility (PATTERN (insn));

  insn_code_number = recog_memoized (insn);
  noperands = insn_n_operands[insn_code_number];
  insn_extract (insn);

  if (noperands == 0)
    return;

  commutative = 0;

  bcopy (insn_operand_constraint[insn_code_number],
	 constraints, sizeof constraints);

  /* If we will need to know, later, whether some pair of operands
     are the same, we must compare them now and save the result.
     Reloading the base and index registers will clobber them
     and afterward they will fail to match.  */

  for (i = 0; i < noperands; i++)
    {
      register char *p = constraints[i];
      register int c;
      while (c = *p++)
	if (c == '%')
	  commutative = 1;
	else if (c >= '0' && c <= '9')
	  {
	    c -= '0';
	    operands_match[c][i]
	      = rtx_renumbered_equal_p (recog_operand[c], recog_operand[i]);
	    if (commutative)
	      {
		if (c == 1 || c == 2)
		  operands_match[3 - c][i]
		    = rtx_renumbered_equal_p (recog_operand[3 - c], recog_operand[i]);
		if (i == 1 || i == 2)
		  operands_match[c][3 - i]
		    = rtx_renumbered_equal_p (recog_operand[c], recog_operand[3 - i]);
		/* Note that c is supposed to be less than i.
		   No need to consider subtracting both c and i from 3
		   because in that case they are 1 and 2
		   and the element we want is operands_match[1][2].  */
	      }
	  }
    }

  /* Examine each operand that is a memory reference or memory address
     and reload parts of the addresses into index registers.
     While we are at it, initialize the array `modified'.
     Also here any references to pseudo regs that didn't get hard regs
     but are equivalent to constants get replaced in the insn itself
     with those constants.  Nobody will ever see them again.  */

  for (i = 0; i < noperands; i++)
    {
      register RTX_CODE code = GET_CODE (recog_operand[i]);
      modified[i] = RELOAD_READ;
      if (constraints[i][0] == 'p')
	find_reloads_address (VOIDmode,
			      recog_operand[i], recog_operand_loc[i]);
      if (code == MEM)
	find_reloads_address (GET_MODE (recog_operand[i]),
			      XEXP (recog_operand[i], 0),
			      &XEXP (recog_operand[i], 0));
      if (code == SUBREG)
	find_reloads_toplev (recog_operand[i]);
      if (code == REG)
	{
	  register int regno = REGNO (recog_operand[i]);
	  if (regno >= FIRST_PSEUDO_REGISTER && reg_renumber[regno] < 0
	      && reg_equiv_constant[regno] != 0)
	    recog_operand[i] = *recog_operand_loc[i]
	      = reg_equiv_constant[regno];
	}
    }

  /* Now see what we need for pseudo-regs that didn't get hard regs
     or got the wrong kind of hard reg.  For this, we must consider
     all the operands together against the register constraints.  */

  best = MAX_RECOG_OPERANDS + 100;

  swapped = 0;
 try_swapped:
  this_alternative_number = 0;
  /* The constraints are made of several alternatives.
     Each operand's constraint looks like foo,bar,... with commas
     separating the alternatives.  The first alternatives for all
     operands go together, the second alternatives go together, etc.

     First loop over alternatives.  */

  while (*constraints[0])
    {
      /* Loop over operands for one constraint alternative.  */
      /* LOSERS counts those that don't fit this alternative
	 and would require loading.  */
      int losers = 0;
      /* BAD is set to 1 if it some operand can't fit this alternative
	 even after reloading.  */
      int bad = 0;
      /* REJECT is a count of how undesirable this alternative says it is
	 if any reloading is required.  If the alternative matches exactly
	 then REJECT is ignored, but otherwise it gets this much
	 counted against it in addition to the reloading needed.  */
      int reject = 0;

      for (i = 0; i < noperands; i++)
	{
	  register char *p = constraints[i];
	  register int win = 0;
	  int badop = 1;
	  int c;
	  register rtx operand = recog_operand[i];
	  int offset = 0;
	  int force_reload = 0;

	  /* If the operand is a SUBREG, extract
	     the REG or MEM within.  */

	  while (GET_CODE (operand) == SUBREG)
	    {
	      offset += SUBREG_WORD (operand);
	      operand = SUBREG_REG (operand);
	      if (GET_CODE (operand) == MEM
/*** This is overcautious, as for BYTES_BIG_ENDIAN it is still possible
     to avoid setting force_reload if the mode of the subreg
     is SImode or bigger.  */
#ifndef BYTES_BIG_ENDIAN
		  && offset != 0
#endif
		  && !offsetable_address_p (operand))
		force_reload = 1;
	    }

	  this_alternative[i] = (int) NO_REGS;
	  this_alternative_win[i] = 0;
	  this_alternative_matches[i] = -1;

	  /* Scan this alternative's specs for this operand;
	     set WIN if the operand fits any letter in this alternative.
	     Otherwise, clear BADOP if this operand could
	     fit some letter after reloads. */

	  while (*p && (c = *p++) != ',')
	    switch (c)
	      {
	      case '=':
		modified[i] = RELOAD_WRITE;
		break;

	      case '+':
		modified[i] = RELOAD_READ_WRITE;
		break;

	      case '*':
		break;

	      case '%':
		commutative = 1;
		break;

	      case '?':
		reject++;
		break;

	      case '!':
		reject = 100;
		break;

	      case '#':
		/* Ignore rest of this alternative as far as
		   reloading is concerned.  */
		while (*p && *p != ',') p++;
		break;

	      case '0':
	      case '1':
	      case '2':
	      case '3':
	      case '4':
		c -= '0';
		this_alternative_matches[i] = c;
		/* We are supposed to match a previous operand.
		   If we do, we win if that one did.
		   If we do not, then both of us lose.  */
		if ((swapped && (c != 1 || i != 2))
		    /* If we are matching as if operands 1 and 2 were swapped,
		       also pretend that operands_match had been computed
		       with them swapped.
		       But if i is 2 and c is 1, don't exchange them,
		       because operands_match is valid only on one
		       side of its diagonal.  */
		    ? (operands_match
		        [(c == 1 || c == 2) ? 3 - c : c]
		        [(i == 1 || i == 2) ? 3 - i : i])
		    : operands_match[c][i])
		  win = this_alternative_win[c];
		else
		  {
		    if (this_alternative_win[c])
		      losers++;
		    this_alternative_win[c] = 0;
		    if (this_alternative[c] == (int) NO_REGS)
		      bad = 1;
		  }
		/* This can be fixed with reloads if the operand
		   we are supposed to match can be fixed with reloads.  */
		badop = 0;
		break;

	      case 'p':
		/* All necessary reloads for an address_operand
		   were handled in find_reloads_address.  */
		this_alternative[i] = (int) ALL_REGS;
		win = 1;
		break;

	      case 'm':
		if (GET_CODE (operand) == MEM
		    || (GET_CODE (operand) == REG
			&& REGNO (operand) >= FIRST_PSEUDO_REGISTER
			&& reg_renumber[REGNO (operand)] < 0))
		  win = 1;
		if (GET_CODE (operand) == CONST_DOUBLE)
		  bad = 0;
		break;

	      case '<':
		if (GET_CODE (operand) == MEM
		    && (GET_CODE (XEXP (operand, 0)) == PRE_DEC
			|| GET_CODE (XEXP (operand, 0)) == POST_DEC))
		  win = 1;
		break;

	      case '>':
		if (GET_CODE (operand) == MEM
		    && (GET_CODE (XEXP (operand, 0)) == PRE_INC
			|| GET_CODE (XEXP (operand, 0)) == POST_INC))
		  win = 1;
		break;

		/* Memory operand whose address is offsettable.  */
	      case 'o':
		if ((GET_CODE (operand) == MEM
		     && offsetable_address_p (operand))
		    || (GET_CODE (operand) == REG
			&& REGNO (operand) >= FIRST_PSEUDO_REGISTER
			&& reg_renumber[REGNO (operand)] < 0))
		  win = 1;
		if (GET_CODE (operand) == CONST_DOUBLE)
		  bad = 0;
		break;

	      case 'F':
		if (GET_CODE (operand) == CONST_DOUBLE)
		  win = 1;
		break;

	      case 'G':
	      case 'H':
		if (GET_CODE (operand) == CONST_DOUBLE
		    && CONST_DOUBLE_OK_FOR_LETTER_P (operand, c))
		  win = 1;
		break;

	      case 's':
		if (GET_CODE (operand) == CONST_INT)
		  break;
	      case 'i':
		if (CONSTANT_ADDRESS_P (operand))
		  win = 1;
		break;

	      case 'I':
	      case 'J':
	      case 'K':
	      case 'L':
	      case 'M':
		if (GET_CODE (operand) == CONST_INT
		    && CONST_OK_FOR_LETTER_P (INTVAL (operand), c))
		  win = 1;
		break;

	      case 'g':
		if (GENERAL_REGS == ALL_REGS
		    || GET_CODE (operand) != REG
		    || (REGNO (operand) >= FIRST_PSEUDO_REGISTER
			&& reg_renumber[REGNO (operand)] < 0))
		  win = 1;
		/* Drop through into 'r' case */

	      case 'r':
		this_alternative[i]
		  = (int) reg_class_subunion[this_alternative[i]][(int) GENERAL_REGS];
		goto reg;

	      default:
		this_alternative[i]
		  = (int) reg_class_subunion[this_alternative[i]][(int) REG_CLASS_FROM_LETTER (c)];

	      reg:
		badop = 0;
		if (GET_CODE (operand) == REG
		    && reg_renumbered_fits_class_p (operand, this_alternative[i], offset))
		  win = 1;
		break;
	      }
	  constraints[i] = p;

	  /* Record which operands fit this alternative.  */
	  if (win && ! force_reload)
	    this_alternative_win[i] = 1;
	  else
	    {
	      losers++;
	      if (badop)
		bad = 1;
	    }
	}

      /* If one set of possibilities, in the register constraints,
	 accept all the operands, then this insn needs no operand reloads.  */
      if (losers == 0)
	{
	  /* If we are matching swapped operands for a commutative insn,
	     then we must really swap the operands to use this alternative.
	     They are already swapped in recog_operand.
	     Swap them in the instruction.  */
	  if (swapped)
	    {
	      *recog_operand_loc[1] = recog_operand[1];
	      *recog_operand_loc[2] = recog_operand[2];
	    }
	  for (i = 0; i < noperands; i++)
	    goal_alternative_win[i] = 1;
	  bcopy (this_alternative, goal_alternative,
		 sizeof this_alternative);
	  goal_alternative_number = this_alternative_number;
	  goto finish;
	}

      /* REJECT, set by the ! and ? constraint characters
	 discourages the use of this alternative for a reload goal.  */
      if (reject > 0)
	losers += reject;

      /* If this alternative is close enough that reloads could fix it,
	 record it as the chosen goal for reloading.  */
      if (! bad && best > losers)
	{
	  bcopy (this_alternative, goal_alternative,
		 sizeof this_alternative);
	  bcopy (this_alternative_win, goal_alternative_win,
		 sizeof this_alternative);
	  bcopy (this_alternative_matches, goal_alternative_matches,
		 sizeof this_alternative);
	  goal_alternative_swapped = swapped;
	  best = losers;
	  goal_alternative_number = this_alternative_number;
	}
      this_alternative_number++;
    }

  /* If insn is commutative (it's safe to exchange operands 1 and 2)
     then we need to try each alternative twice,
     the second time matching the operands 1 and 2
     as if we had exchanged them.
     To do this, really exchange them in recog_operand.

     If we have just tried the alternatives the second time,
     return recog_operand to normal and drop through.  */

  if (commutative)
    {
      swapped = !swapped;
      if (swapped)
	{
	  recog_operand[1] = *recog_operand_loc[2];
	  recog_operand[2] = *recog_operand_loc[1];

	  bcopy (insn_operand_constraint[insn_code_number],
		 constraints, sizeof constraints);
	  goto try_swapped;
	}
      else
	{
	  recog_operand[1] = *recog_operand_loc[1];
	  recog_operand[2] = *recog_operand_loc[2];
	}
    }

  /* The operands don't meet the constraints.
     goal_alternative describes the alternative
     that we could reach by reloading the fewest operands.
     Reload so as to fit it.  */

  if (best == MAX_RECOG_OPERANDS + 100)
    abort ();			/* No alternative works with reloads??  */

  /* If the best alternative is with operands 1 and 2 swapped,
     really swap them in the instruction before reporting the reloads.  */

  if (goal_alternative_swapped)
    {
      /* We do not alter recog_operand_loc because recog_operand_loc[I]
	 points to the place where the instruction holds operand I.  */
      /* Interchange the operands in the instruction.  */
      *recog_operand_loc[1] = recog_operand[2];
      *recog_operand_loc[2] = recog_operand[1];
      /* Make recog_operand match the instruction again.  */
      recog_operand[1] = *recog_operand_loc[1];
      recog_operand[2] = *recog_operand_loc[2];
    }

  /* Right now, for any pair of operands I and J that are required to match,
     with I < J,
     goal_alternative_matches[J] is I.
     Set up goal_alternative_matched as the inverse function:
     goal_alternative_matched[I] = J.  */

  for (i = 0; i < noperands; i++)
    goal_alternative_matched[i] = -1;

  for (i = 0; i < noperands; i++)
    if (! goal_alternative_win[i]
	&& goal_alternative_matches[i] >= 0)
      goal_alternative_matched[goal_alternative_matches[i]] = i;

  /* Jump to `finish' from above if all operands are valid already.
     In that case,
     goal_alternative_win is all 1 and operand_reloadnum is all -1.  */
 finish:

  /* Now record reloads for all the operands that need them.  */
  for (i = 0; i < noperands; i++)
    if (! goal_alternative_win[i])
      {
	/* Operands that match previous ones have already been handled.  */
	if (goal_alternative_matches[i] >= 0)
	  ;
	else if (GET_CODE (recog_operand[i]) == CONST_DOUBLE
		 && alternative_allows_memconst (insn_operand_constraint[insn_code_number][i], goal_alternative_number))
	  *recog_operand_loc[i] = recog_operand[i] = XEXP (recog_operand[i], 2);
	else if (goal_alternative_matched[i] == -1)
	  operand_reloadnum[i] =
	    push_reload (modified[i] != RELOAD_WRITE ? recog_operand[i] : 0,
			 modified[i] != RELOAD_READ ? recog_operand[i] : 0,
			 recog_operand_loc[i], 0,
			 (enum reg_class) goal_alternative[i],
			 insn_operand_mode[insn_code_number][i], 0, 0);
	/* In a matching pair of operands, one must be input only
	   and the other must be output only.
	   Pass the input operand as IN and the other as OUT.  */
	else if (modified[i] == RELOAD_READ
		 && modified[goal_alternative_matched[i]] == RELOAD_WRITE)
	  operand_reloadnum[goal_alternative_matched[i]]
	    = operand_reloadnum[i]
	    = push_reload (recog_operand[i],
			   recog_operand[goal_alternative_matched[i]],
			   recog_operand_loc[i],
			   recog_operand_loc[goal_alternative_matched[i]],
			   (enum reg_class) goal_alternative[i],
			   insn_operand_mode[insn_code_number][i],
			   insn_operand_mode[insn_code_number][goal_alternative_matched[i]],
			   0);
	else if (modified[i] == RELOAD_WRITE
		 && modified[goal_alternative_matched[i]] == RELOAD_READ)
	  operand_reloadnum[goal_alternative_matched[i]]
	    = operand_reloadnum[i]
	    = push_reload (recog_operand[goal_alternative_matched[i]],
			   recog_operand[i],
			   recog_operand_loc[goal_alternative_matched[i]],
			   recog_operand_loc[i],
			   (enum reg_class) goal_alternative[i],
			   insn_operand_mode[insn_code_number][goal_alternative_matched[i]],
			   insn_operand_mode[insn_code_number][i],
			   0);
	else abort ();
      }
    else
      {
	rtx operand = recog_operand[i];
	/* For each operand that's a pseudo-register 
	   that didn't get a hard register, make an optional reload.
	   This may get done even if the insn needs no reloads otherwise.  */
	while (GET_CODE (operand) == SUBREG)
	  operand = XEXP (operand, 0);
	if (GET_CODE (operand) == REG
	    && REGNO (operand) >= FIRST_PSEUDO_REGISTER
	    && reg_renumber[REGNO (operand)] < 0
	    && (enum reg_class) goal_alternative[i] != NO_REGS)
	  operand_reloadnum[i]
	    = push_reload (modified[i] != RELOAD_WRITE ? recog_operand[i] : 0,
			   modified[i] != RELOAD_READ ? recog_operand[i] : 0,
			   recog_operand_loc[i], 0,
			   (enum reg_class) goal_alternative[i],
			   insn_operand_mode[insn_code_number][i],
			   0, 1);
	else
	  operand_reloadnum[i] = -1;
      }

  /* If this insn pattern contains any MATCH_DUP's, make sure that
     they will be substituted if the operands they match are substituted.
     Also do now any substitutions we already did on the operands.  */
  for (i = insn_n_dups[insn_code_number] - 1; i >= 0; i--)
    {
      *recog_dup_loc[i] = *recog_operand_loc[recog_dup_num[i]];
      if (operand_reloadnum[recog_dup_num[i]] >= 0)
	push_replacement (recog_dup_loc[i], operand_reloadnum[recog_dup_num[i]],
			  insn_operand_mode[insn_code_number][i]);
    }

  /* For each reload of a reg into some other class of reg,
     search for an existing equivalent reg (same value now) in the right class.
     We can use it as long as we don't need to change its contents.  */
  for (i = 0; i < n_reloads; i++)
    if (reload_reg_rtx[i] == 0
	&& reload_in[i] != 0
	&& GET_CODE (reload_in[i]) == REG
	&& reload_out[i] == 0)
      {
	reload_reg_rtx[i]
	  = find_equiv_reg (reload_in[i], insn, reload_reg_class[i], -1,
			    static_reload_reg_p);
	/* Prevent generation of insn to load the value
	   because the one we found already has the value.  */
	if (reload_reg_rtx[i])
	  reload_in[i] = reload_reg_rtx[i];
      }

#else /* no REGISTER_CONSTRAINTS */
  int noperands;
  int insn_code_number;
  register int i;
  rtx body = PATTERN (insn);

  if (GET_CODE (body) == USE || GET_CODE (body) == CLOBBER
      || GET_CODE (body) == ASM_INPUT
      || GET_CODE (body) == ADDR_VEC || GET_CODE (body) == ADDR_DIFF_VEC)
    return;

  n_reloads = 0;
  n_replacements = 0;
  replace_reloads = replace;
  indirect_ok = ind_ok;
  this_insn = insn;

  forget_volatility (body);

  insn_code_number = recog_memoized (insn);
  noperands = insn_n_operands[insn_code_number];
  insn_extract (insn);

  if (noperands == 0)
    return;

  for (i = 0; i < noperands; i++)
    {
      register RTX_CODE code = GET_CODE (recog_operand[i]);

      if (insn_operand_address_p[insn_code_number][i])
	find_reloads_address (VOIDmode,
			      recog_operand[i], recog_operand_loc[i]);
      if (code == MEM)
	find_reloads_address (GET_MODE (recog_operand[i]),
			      XEXP (recog_operand[i], 0),
			      &XEXP (recog_operand[i], 0));
      if (code == SUBREG)
	find_reloads_toplev (recog_operand[i]);
      if (code == REG)
	{
	  register int regno = REGNO (recog_operand[i]);
	  if (regno >= FIRST_PSEUDO_REGISTER && reg_renumber[regno] < 0
	      && reg_equiv_constant[regno] != 0)
	    recog_operand[i] = *recog_operand_loc[i]
	      = reg_equiv_constant[regno];
	}
    }
#endif /* no REGISTER_CONSTRAINTS */
}

/* Return 1 if alternative number ALTNUM in constraint-string CONSTRAINT
   accepts a memory operand with constant address.  */

static int
alternative_allows_memconst (constraint, altnum)
     char *constraint;
     int altnum;
{
  register int c;
  /* Skip alternatives before the one requested.  */
  while (altnum > 0)
    {
      while (*constraint++ != ',');
      altnum--;
    }
  /* Scan the requested alternative for 'm' or 'o'.
     If one of them is present, this alternative accepts memory constants.  */
  while (c == *constraint++ && c != ',' && c != '#')
    if (c == 'm' || c == 'o')
      return 1;
  return 0;
}

/* Scan X for memory references and scan the addresses for reloading.
   Also checks for references to "constant" regs that we want to eliminate
   and replaces them with the values they stand for.
   We may alter X descructively if it contains a reference to such.
   If X is just a constant reg, we return the equivalent value
   instead of X.  */

static rtx
find_reloads_toplev (x)
     rtx x;
{
  register RTX_CODE code = GET_CODE (x);

  register char *fmt = GET_RTX_FORMAT (code);
  register int i;

  if (code == REG)
    {
      register int regno = REGNO (x);
      if (regno >= FIRST_PSEUDO_REGISTER && reg_renumber[regno] < 0
	  && reg_equiv_constant[regno] != 0)
	return reg_equiv_constant[regno];
    }
  else if (code == MEM)
    find_reloads_address (GET_MODE (x), XEXP (x, 0), &XEXP (x, 0));
  else
    for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
      {
	if (fmt[i] == 'e')
	  XEXP (x, i) = find_reloads_toplev (XEXP (x, i));
      }
  return x;
}

/* Note that we take shortcuts assuming that no multi-reg machine mode
   occurs as part of an address.  */

static void
find_reloads_address (mode, ad, loc)
     enum machine_mode mode;
     rtx ad;
     rtx *loc;
{
  register int regno;

  if (GET_CODE (ad) == REG)
    {
      regno = REGNO (ad);

      if (regno >= FIRST_PSEUDO_REGISTER && reg_renumber[regno] < 0
	  && reg_equiv_constant[regno] != 0)
	{
	  GO_IF_LEGITIMATE_ADDRESS (mode, reg_equiv_constant[regno], win1);
	}
      if (! (reg_renumber[regno] < 0 ? indirect_ok
	     : REGNO_OK_FOR_BASE_P (regno)))
	push_reload (ad, 0, loc, 0, BASE_REG_CLASS, GET_MODE (ad), 0, 0);
      return;

    win1:
      *loc = ad = reg_equiv_constant[regno];
      return;
    }

  GO_IF_LEGITIMATE_ADDRESS (mode, ad, win);
  find_reloads_address_1 (ad, 0, loc);
  return;

  /* The address appears valid, so reloads are not needed.
     But the address may contain an eliminable register we need to eliminate.
     This can happen because a machine with indirect addressing
     may consider a pseudo register by itself a valid address even when
     it has failed to get a hard reg.
     So do a tree-walk to find and eliminate all such regs.  */
 win:
  *loc = subst_reg_equivs (ad);
}

/* Find all pseudo regs appearing in AD
   that are eliminable in favor of equivalent values
   and do not have hard regs; replace them by their equivalents.  */

static rtx
subst_reg_equivs (ad)
     rtx ad;
{
  register int regno;

  register RTX_CODE code = GET_CODE (ad);
  register int i;
  register char *fmt;

  switch (code)
    {
    case CONST_INT:
    case CONST:
    case CONST_DOUBLE:
    case SYMBOL_REF:
    case LABEL_REF:
    case PC:
    case CC0:
      return ad;

    case REG:
      {
	register int regno = REGNO (ad);

	if (regno >= FIRST_PSEUDO_REGISTER && reg_renumber[regno] < 0
	    && reg_equiv_constant[regno] != 0)
	  return reg_equiv_constant[regno];
      }
      return ad;
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    if (fmt[i] == 'e')
      XEXP (ad, i) = subst_reg_equivs (XEXP (ad, i));
  return ad;
}

/* Record the pseudo registers we must reload into hard registers
   in a subexpression of a memory address, X.
   CONTEXT = 1 means we are considering regs as index regs,
   = 0 means we are considering them as base regs.

   We return X, whose operands may have been altered,
   or perhaps a RELOAD rtx if X itself was a REG that must be reloaded.  */

/* Note that we take shortcuts assuming that no multi-reg machine mode
   occurs as part of an address.
   Also, this is not fully machine-customizable; it works for machines
   such as vaxes and 68000's and 32000's, but other possible machines
   could have addressing modes that this does not handle right.  */

static void
find_reloads_address_1 (x, context, loc)
     rtx x;
     int context;
     rtx *loc;
{
  register RTX_CODE code = GET_CODE (x);

  if (code == PLUS)
    {
      register rtx op0 = XEXP (x, 0);
      register rtx op1 = XEXP (x, 1);
      register RTX_CODE code0 = GET_CODE (op0);
      register RTX_CODE code1 = GET_CODE (op1);
      register int  regno;
      if (code0 == MULT || code0 == SIGN_EXTEND || code1 == MEM)
	{
	  find_reloads_address_1 (op0, 1, &XEXP (x, 0));
	  find_reloads_address_1 (op1, 0, &XEXP (x, 1));
	}
      else if (code1 == MULT || code1 == SIGN_EXTEND || code0 == MEM)
	{
	  find_reloads_address_1 (op0, 0, &XEXP (x, 0));
	  find_reloads_address_1 (op1, 1, &XEXP (x, 1));
	}
      else if (code0 == CONST_INT || code0 == CONST
	       || code0 == SYMBOL_REF || code0 == LABEL_REF)
	{
	  find_reloads_address_1 (op1, 0, &XEXP (x, 1));
	}
      else if (code1 == CONST_INT || code1 == CONST
	       || code1 == SYMBOL_REF || code1 == LABEL_REF)
	{
	  find_reloads_address_1 (op0, 0, &XEXP (x, 0));
	}
      else if (code0 == REG && code1 == REG)
	{
	  if (REG_OK_FOR_INDEX_P (op0)
	      && REG_OK_FOR_BASE_P (op1))
	    return;
	  else if (REG_OK_FOR_INDEX_P (op1)
	      && REG_OK_FOR_BASE_P (op0))
	    return;
	  else if (REG_OK_FOR_BASE_P (op1))
	    find_reloads_address_1 (op0, 1, &XEXP (x, 0));
	  else if (REG_OK_FOR_BASE_P (op0))
	    find_reloads_address_1 (op1, 1, &XEXP (x, 1));
	  else if (REG_OK_FOR_INDEX_P (op1))
	    find_reloads_address_1 (op0, 0, &XEXP (x, 0));
	  else if (REG_OK_FOR_INDEX_P (op0))
	    find_reloads_address_1 (op1, 0, &XEXP (x, 1));
	  else
	    {
	      find_reloads_address_1 (op0, 1, &XEXP (x, 0));
	      find_reloads_address_1 (op1, 0, &XEXP (x, 1));
	    }
	}
      else if (code0 == REG)
	{
	  find_reloads_address_1 (op0, 1, &XEXP (x, 0));
	  find_reloads_address_1 (op1, 0, &XEXP (x, 1));
	}
      else if (code1 == REG)
	{
	  find_reloads_address_1 (op1, 1, &XEXP (x, 1));
	  find_reloads_address_1 (op0, 0, &XEXP (x, 0));
	}
    }
  else if (code == POST_INC || code == POST_DEC
	   || code == PRE_INC || code == PRE_DEC)
    {
      if (GET_CODE (XEXP (x, 0)) == REG)
	{
	  register int regno = REGNO (XEXP (x, 0));

	  /* ??? Is this a bug?
	     I don't see any reason why an argument register
	     could not appear in an auto-increment
	     and yet fail to get a hard reg.  Then this would crash.  */
	  if (regno >= FIRST_PSEUDO_REGISTER && reg_renumber[regno] < 0
	      && reg_equiv_constant[regno] != 0)
	    abort ();

	  if (reg_renumber[regno] >= 0)
	    regno = reg_renumber[regno];
	  if ((regno >= FIRST_PSEUDO_REGISTER
	       || !(context ? REGNO_OK_FOR_INDEX_P (regno)
		    : REGNO_OK_FOR_BASE_P (regno))))
	    {
	      register rtx link;
	      int reloadnum
		= push_reload (XEXP (x, 0), XEXP (x, 0),
			       &XEXP (x, 0), 0,
			       context ? INDEX_REG_CLASS : BASE_REG_CLASS,
			       GET_MODE (XEXP (x, 0)), 0, 0);

	      for (link = REG_NOTES (this_insn);
		   link; link = XEXP (link, 1))
		if ((enum reg_note) GET_MODE (link) == REG_INC
		    && REGNO (XEXP (link, 0)) == REGNO (XEXP (x, 0)))
		  push_replacement (&XEXP (link, 0), reloadnum, VOIDmode);
	    }
	  return;
	}
    }
  else if (code == REG)
    {
      register int regno = REGNO (x);

      if (regno >= FIRST_PSEUDO_REGISTER && reg_renumber[regno] < 0
	  && reg_equiv_constant[regno] != 0)
	{
	  push_reload (reg_equiv_constant[regno], 0, loc, 0,
		       context ? INDEX_REG_CLASS : BASE_REG_CLASS,
		       GET_MODE (x), 0, 0);
	  return;
	}

      if (reg_renumber[regno] >= 0)
	regno = reg_renumber[regno];
      if ((regno >= FIRST_PSEUDO_REGISTER
	   || !(context ? REGNO_OK_FOR_INDEX_P (regno)
		: REGNO_OK_FOR_BASE_P (regno))))
	{
	  push_reload (x, 0, loc, 0,
		       context ? INDEX_REG_CLASS : BASE_REG_CLASS,
		       GET_MODE (x), 0, 0);
	  return;
	}
    }
  else
    {
      register char *fmt = GET_RTX_FORMAT (code);
      register int i;
      for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
	{
	  if (fmt[i] == 'e')
	    find_reloads_address_1 (XEXP (x, i), context, &XEXP (x, i));
	}
    }
}

/* Substitute into X the registers into which we have reloaded
   the things that need reloading.  The array `replacements'
   says contains the locations of all pointers that must be changed
   and says what to replace them with.

   Return the rtx that X translates into; usually X, but modified.  */

void
subst_reloads ()
{
  register int i;

  for (i = 0; i < n_replacements; i++)
    {
      register struct replacement *r = &replacements[i];
      register rtx reloadreg = reload_reg_rtx[r->what];
      if (reloadreg)
	{
	  /* Encapsulate RELOADREG so its machine mode matches what
	     used to be there.  */
	  if (GET_MODE (reloadreg) != r->mode && r->mode != VOIDmode)
	    reloadreg = gen_rtx (SUBREG, r->mode, reloadreg, 0);
	  *r->where = reloadreg;
	}
      /* If reload got no reg and isn't optional, something's wrong.  */
      else if (! reload_optional[r->what])
	abort ();
    }
}

/* Alter X by eliminating all VOLATILE and UNCHANGING expressions.
   Each of them is replaced by its operand.
   Thus, (PLUS (VOLATILE (MEM (REG 5))) (CONST_INT 4))
   becomes (PLUS (MEM (REG 5)) (CONST_INT 4)).

   If X is itself a VOLATILE expression,
   we return the expression that should replace it
   but we do not modify X.  */

static rtx
forget_volatility (x)
     register rtx x;
{
  enum rtx_code code = GET_CODE (x);
  register char *fmt;
  register int i;
  register rtx value = 0;
  register rtx tem;

  switch (code)
    {
    case LABEL_REF:
    case SYMBOL_REF:
    case CONST_INT:
    case CONST_DOUBLE:
    case CONST:
    case REG:
    case CC0:
    case PC:
      return x;

    case VOLATILE:
    case UNCHANGING:
      return XEXP (x, 0);
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	XEXP (x, i) = forget_volatility (XEXP (x, i));
      if (fmt[i] == 'E')
	{
	  register int j;
	  for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	    XVECEXP (x, i, j) = forget_volatility (XVECEXP (x, i, j));
	}
    }

  return x;
}


/* Check the insns before INSN to see if there is a suitable register
   containing the same value as GOAL.
   If OTHER is -1, look for a register in class CLASS.
   Otherwise, just see if register number OTHER shares GOAL's value.

   Return an rtx for the register found, or zero if none is found.

   If RELOAD_REG_P is nonzero, it is a vector indexed by hard reg number
   and we reject any hard reg whose element in the vector is 1.

   This function is used by final as well as in the reload pass.  */

rtx
find_equiv_reg (goal, insn, class, other, reload_reg_p)
     register rtx goal;
     rtx insn;
     enum reg_class class;
     register int other;
     short *reload_reg_p;
{
  register rtx p = insn;
  rtx valtry, value, where;
  register rtx pat;
  register int regno;
  int valueno;

  if (GET_CODE (goal) == REG)
    regno = REGNO (goal);
  else
    regno = -1;

  /* Scan insns back from INSN, looking for one that copies
     a value into or out of GOAL.
     Stop and give up if we reach a label.  */

  while (1)
    {
      p = PREV_INSN (p);
      if (p == 0 || GET_CODE (p) == CODE_LABEL)
	return 0;
      if (GET_CODE (p) == INSN)
	{
	  pat = PATTERN (p);
	  /* First check for something that sets some reg equal to GOAL.  */
	  if (GET_CODE (pat) == SET
	      && ((regno >= 0
		   && GET_CODE (SET_SRC (pat)) == REG
		   && REGNO (SET_SRC (pat)) == regno
		   && GET_CODE (valtry = SET_DEST (pat)) == REG)
		  ||
		  (regno >= 0
		   && GET_CODE (SET_DEST (pat)) == REG
		   && REGNO (SET_DEST (pat)) == regno
		   && GET_CODE (valtry = SET_SRC (pat)) == REG)
		  ||
		  (SET_SRC (pat) == goal
		   && GET_CODE (valtry = SET_DEST (pat)) == REG))
	      && (other >= 0
		  ? REGNO (valtry) == other
		  : (valueno = REGNO (valtry),
		     reg_renumber[valueno] >= 0 ? valueno = reg_renumber[valueno] : 0,
		     valueno < FIRST_PSEUDO_REGISTER &&
		     TEST_HARD_REG_BIT (reg_class_contents[(int) class],
					valueno))))
	    {
	      value = valtry;
	      where = p;
	      break;
	    }
	}
    }

  /* We found a previous insn copying GOAL into a suitable other reg VALUE
     (or copying VALUE into GOAL, if GOAL is also a register).
     Now verify that VALUE is really valid.  */

  if (regno >= FIRST_PSEUDO_REGISTER)
    regno = reg_renumber[regno];

  /* VALUENO gets the register number of VALUE;
     for a pseudo reg, it gets the hard reg number that the pseudo has,
     and we give up if the pseudo has no hard reg.  */

  valueno = REGNO (value);
  if (valueno >= FIRST_PSEUDO_REGISTER)
    valueno = reg_renumber[valueno];

  if (valueno < 0)
    return 0;

  /* Reject VALUE if it is one of the regs reserved for reloads.
     Reload1 knows how to reuse them anyway, and it would get
     confused if we allocated one without its knowledge.  */

  if (reload_reg_p != 0 && reload_reg_p[valueno] >= 0)
    return 0;

  /* Now verify that the values of GOAL and VALUE remain unaltered
     until INSN is reached.  */

  p = insn;
  while (1)
    {
      p = PREV_INSN (p);
      if (p == where)
	return value;

      /* Don't trust the conversion past a function call
	 if either of the two is in a call-clobbered register.  */
      if (GET_CODE (p) == CALL_INSN
	  && ((regno >= 0 && call_clobbered_regs[regno])
	      || (valueno >= 0 && call_clobbered_regs[valueno])))
	return 0;
	
      if (GET_CODE (p) == INSN || GET_CODE (p) == JUMP_INSN
	  || GET_CODE (p) == CALL_INSN)
	{
	  /* If this insn P stores in either REG or VALUE, return 0.  */

	  pat = PATTERN (p);
	  if (GET_CODE (pat) == SET || GET_CODE (pat) == CLOBBER)
	    {
	      register rtx dest = SET_DEST (pat);
	      while (GET_CODE (dest) == SUBREG
		     || GET_CODE (dest) == VOLATILE
		     || GET_CODE (dest) == ZERO_EXTRACT
		     || GET_CODE (dest) == SIGN_EXTRACT
		     || GET_CODE (dest) == STRICT_LOW_PART)
		dest = XEXP (dest, 0);
	      if (GET_CODE (dest) == REG)
		{
		  register int xregno = REGNO (dest);
		  if (reg_renumber[xregno] >= 0)
		    xregno = reg_renumber[xregno];
		  if (xregno == regno || xregno == valueno)
		    return 0;
		}
	    }
	  else if (GET_CODE (pat) == PARALLEL)
	    {
	      register int i;
	      for (i = XVECLEN (pat, 0) - 1; i >= 0; i--)
		{
		  register rtx v1 = XVECEXP (pat, 0, i);
		  if (GET_CODE (v1) == SET || GET_CODE (v1) == CLOBBER)
		    {
		      register rtx dest = SET_DEST (v1);
		      while (GET_CODE (dest) == SUBREG
			     || GET_CODE (dest) == VOLATILE
			     || GET_CODE (dest) == ZERO_EXTRACT
			     || GET_CODE (dest) == SIGN_EXTRACT
			     || GET_CODE (dest) == STRICT_LOW_PART)
			dest = XEXP (dest, 0);
		      if (GET_CODE (dest) == REG)
			{
			  register int xregno = REGNO (dest);
			  if (reg_renumber[xregno] >= 0)
			    xregno = reg_renumber[xregno];
			  if (xregno == regno || xregno == valueno)
			    return 0;
			}
		    }
		}
	    }
	  /* If this insn auto-increments or auto-decrements
	     either regno or valueno, return 0 now.  */
	  {
	    register rtx link;

	    for (link = REG_NOTES (p); link; link = XEXP (link, 1))
	      if ((enum reg_note) GET_MODE (link) == REG_INC)
		{
		  register int incno = REGNO (XEXP (link, 0));
		  if (reg_renumber[incno] >= 0)
		    incno = reg_renumber[incno];
		  if (incno == regno || incno == valueno)
		    return 0;
		}
	  }
	}
    }
}

/* Find a place where INCED appears in an increment or decrement operator
   within X, and return the amount INCED is incremented by
   (negative if decremented).  */

static int
find_inc_amount (x, inced)
     rtx x, inced;
{
  register enum rtx_code code = GET_CODE (x);
  register char *fmt;
  register int i;

  if (code == MEM)
    {
      register rtx addr = XEXP (x, 0);
      if ((GET_CODE (addr) == PRE_DEC
	   || GET_CODE (addr) == POST_DEC)
	  && XEXP (addr, 0) == inced)
	return - GET_MODE_SIZE (GET_MODE (x));
      if ((GET_CODE (addr) == PRE_INC
	   || GET_CODE (addr) == POST_INC)
	  && XEXP (addr, 0) == inced)
	return GET_MODE_SIZE (GET_MODE (x));
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	{
	  register int tem = find_inc_amount (XEXP (x, i), inced);
	  if (tem != 0)
	    return tem;
	}
      if (fmt[i] == 'E')
	{
	  register int j;
	  for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	    {
	      register int tem = find_inc_amount (XVECEXP (x, i, j), inced);
	      if (tem != 0)
		return tem;
	    }
	}
    }

  return 0;
}
