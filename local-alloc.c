/* Allocate registers within a basic block, for GNU compiler.
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


/* Allocation of hard register numbers to pseudo registers is done in
   two passes.  In this pass we consider only regs that are born and
   die once within one basic block.  We do this one basic block at a
   time.  Then the next pass allocates the registers that remain.
   Two passes are used because this pass uses methods that work only
   on linear code, but that do a better job than the general methods
   used in global_alloc, and more quickly too.

   The assignments made are recorded in the vector reg_renumber
   whose space is allocated here.  The rtl code itself is not altered.

   We assign each instruction in the basic block a number
   which is its order from the beginning of the block.
   Then we can represent the lifetime of a pseudo register with
   a pair of numbers, and check for conflicts easily.
   We can record the availability of hard registers with a
   HARD_REG_SET for each instruction.  The HARD_REG_SET
   contains 0 or 1 for each hard reg.

   To avoid register shuffling, we tie registers together when one
   dies by being copied into another, or dies in an instruction that
   does arithmetic to produce another.  The tied registers are
   allocated as one.  Registers with different reg class preferences
   can never be tied unless the class preferred by one is a subclass
   of the one preferred by the other.

   Tying is represented with "quantity numbers".
   A non-tied register is given a new quantity number.
   Tied registers have the same quantity number.
   
   We have provision to exempt registers, even when they are contained
   within the block, that can be tied to others that are not contained in it.
   This is so that global_alloc could process them both and tie them then.
   But this is currently disabled since tying in global_alloc is not
   yet implemented.  */

#include <stdio.h>
#include "config.h"
#include "rtl.h"
#include "basic-block.h"
#include "regs.h"
#include "hard-reg-set.h"

/* What about hardware registers used and set within same insn?
   Will that ever happen for a non-fixed register?
   Our lifetime-tracking for hardware registers would lose.
   [This caution is an old comment that may be obsolete;
    I think there is no longer a problem, but I'm not sure.]  */

/* Some constants defined by config.h.  */

static char fixed_regs[] = FIXED_REGISTERS;
static char call_clobbered_regs[] = CALL_USED_REGISTERS;
static HARD_REG_SET reg_class_contents[] = REG_CLASS_CONTENTS;

/* HARD_REG_SETs containing the same information found in
   FIXED_REGISTERS and CALL_USED_REGISTERS.  */

static HARD_REG_SET fixed_reg_set, call_clobbered_reg_set;

/* Next quantity number available for allocation.  */

static int next_qty;

/* Element Q is the hard reg number chosen for quantity Q,
   or -1 if none was found.  */

static int *qty_phys_reg;

/* Insn number (counting from head of basic block)
   where quantity Q was born.  */

static int *qty_birth;

/* Insn number (counting from head of basic block)
   where quantity Q died.  Due to the way tying is done,
   and the fact that we consider in this pass only regs that die but once,
   a quantity can die only once.  Each quantity's life span
   is a set of consecutive insns.  */

static int *qty_death;

/* Number of words needed to hold the data in quantity Q.
   This depends on its machine mode.  It is used for these purposes:
   1. If it is 0, the qty is not really in use and is not allocated.
   2. It is used in computing the relative importances of qtys,
      which determines the order in which we look for regs for them.
   3. It is used in rules that prevent tying several registers of
      different sizes in a way that is geometrically impossible
      (see combine_regs).  */

static int *qty_size;

/* This holds the mode of the registers that are tied to qty Q,
   or VOIDmode if registers with differing modes are tied together.  */

static enum machine_mode *qty_mode;

/* Nonzero if any of the regs tied to qty Q lives across a CALL_INSN.  */

static char *qty_crosses_call;

/* Preferred reg class of qty Q.  */

static enum reg_class *qty_reg_class;

/* reg_qty[n] is the qty number of (REG n),
   or -1 if (REG n) is not local to the current basic block,
   or -2 if not known yet.  */

static int *reg_qty;

/* The offset (in words) of register N within its quantity.
   This can be nonzero if register N is SImode, and has been tied
   to a subreg of a DImode register.  */

static int *reg_offset;

/* Vector of substitutions of register numbers,
   used to map pseudo regs into hardware regs.
   This is set up as a result of register allocation.
   Element N is the hard reg assigned to pseudo reg N,
   or is -1 if no hard reg was assigned.
   If N is a hard reg number, element N is N.  */

short *reg_renumber;

/* Set of hard registers live at the current point in the scan
   of the instructions in a basic block.  */

static HARD_REG_SET regs_live;

/* Indexed by insn-number-within-basic-block,
   a set or hard registers live *after* that insn.  */

static HARD_REG_SET *regs_live_at;

/* Nonzero if a CALL_INSN has been scanned
   but we have not yet seen a reference to the value returned.  */

static int call_seen;

static void block_alloc ();
static int combine_regs ();
static void wipe_dead_reg ();
static void reg_is_born ();
static void reg_clobbered ();
static void mark_life ();
static void post_mark_life ();
static int qty_better_p ();
static int qty_better_p_1 ();

/* Allocate a new quantity (new within current basic block)
   for register number REGNO which is born in insn number INSN_NUMBER
   within the block.  MODE and SIZE are info on reg REGNO.  */

static void
alloc_qty (regno, mode, size, insn_number)
     int regno;
     enum machine_mode mode;
     int size, insn_number;
{
  register int qty = next_qty++;
  reg_qty[regno] = qty;
  reg_offset[regno] = 0;
  qty_size[qty] = size;
  qty_mode[qty] = mode;
  qty_birth[qty] = insn_number;
  qty_crosses_call[qty] = reg_crosses_call[regno];
  qty_reg_class[qty] = reg_preferred_class (regno);
}

/* Main entry point of this file.  */

void
local_alloc ()
{
  register int b, i;

  /* Initialize "constant" tables.  */

  CLEAR_HARD_REG_SET (fixed_reg_set);
  CLEAR_HARD_REG_SET (call_clobbered_reg_set);

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    {
      if (fixed_regs[i])
	SET_HARD_REG_BIT (fixed_reg_set, i);
      if (call_clobbered_regs[i])
	SET_HARD_REG_BIT (call_clobbered_reg_set, i);
    }

  /* Allocate vectors of temporary data.
     See the declarations of these variables, above,
     for what they mean.  */

  qty_phys_reg = (int *) alloca (max_regno * sizeof (int));
  qty_birth = (int *) alloca (max_regno * sizeof (int));
  qty_death = (int *) alloca (max_regno * sizeof (int));
  qty_size = (int *) alloca (max_regno * sizeof (int));
  qty_mode = (enum machine_mode *) alloca (max_regno * sizeof (enum machine_mode));
  qty_crosses_call = (char *) alloca (max_regno);
  qty_reg_class = (enum reg_class *) alloca (max_regno * sizeof (enum reg_class));

  reg_qty = (int *) alloca (max_regno * sizeof (int));
  reg_offset = (int *) alloca (max_regno * sizeof (int));

  reg_renumber = (short *) oballoc (max_regno * sizeof (short));
  for (i = 0; i < max_regno; i++)
    reg_renumber[i] = -1;

  /* Allocate each block's local registers, block by block.  */

  for (b = 0; b < n_basic_blocks; b++)
    {
      for (i = 0; i < max_regno; i++)
	reg_qty[i] = -2;

      bzero (reg_offset, max_regno * sizeof (int));

      bzero (qty_birth, max_regno * sizeof (int));
      bzero (qty_death, max_regno * sizeof (int));
      bzero (qty_size, max_regno * sizeof (int));
      bzero (qty_mode, max_regno * sizeof (enum machine_mode));
      bzero (qty_phys_reg, max_regno * sizeof (int));
      bzero (qty_crosses_call, max_regno);
      bzero (qty_reg_class, max_regno * sizeof (enum reg_class));

      next_qty = FIRST_PSEUDO_REGISTER;

      block_alloc (b);
    }
}

/* Allocate hard regs to the pseudo regs used only within block number B.
   Only hard regs that die but once can be handled.  */

static void
block_alloc (b)
     int b;
{
  register int i, q;
  register rtx insn;
  int insn_number = 0;
  int insn_count = 0;
  short *qty_order;

  call_seen = 0;

  /* Count the instructions in the basic block.  */

  insn = basic_block_end[b];
  while (1)
    {
      insn_count++;
      if (insn == basic_block_head[b])
	break;
      insn = PREV_INSN (insn);
    }

  regs_live_at = (HARD_REG_SET *) alloca (insn_count * sizeof (HARD_REG_SET));
  bzero (regs_live_at, insn_count * sizeof (HARD_REG_SET));

  /* Initialize table of hardware registers currently live.  */

#ifdef HARD_REG_SET
  regs_live = *basic_block_live_at_start[b];
#else
  COPY_HARD_REG_SET (regs_live, basic_block_live_at_start[b]);
#endif

  /* This loop scans the instructions of the basic block
     and assigns quantities to registers.
     It computes which registers to tie.  */

  insn = basic_block_head[b];
  while (1)
    {
      register rtx body = PATTERN (insn);
      insn_number++;

      if (GET_CODE (insn) == INSN || GET_CODE (insn) == JUMP_INSN
	  || GET_CODE (insn) == CALL_INSN)
	{
	  register rtx link;
	  register int win = 0;
	  register rtx r0, r1;
	  int combined_regno = -1;

	  /* Is this insn suitable for tying two registers?
	     If so, try doing that.
	     Suitable insns are (set reg0 reg1) and
	     (set reg0 (arithop reg1 ...)).
	     Subregs in place of regs are also ok.
	     An insn with parallel sets is ok if the first set is suitable.

	     If tying is done, WIN is set nonzero.  */

	  if (GET_CODE (body) == SET
	      && (r0 = SET_DEST (body),
		  GET_CODE (r0) == REG || GET_CODE (r0) == SUBREG)
	      && (r1 = SET_SRC (body),
		  GET_CODE (r1) == REG || GET_CODE (r1) == SUBREG))
	    win = combine_regs (r1, r0, b, insn_number, insn);
	  else if (GET_CODE (body) == SET
		   && (r0 = SET_DEST (body),
		       GET_CODE (r0) == REG || GET_CODE (r0) == SUBREG)
		   && GET_RTX_FORMAT (GET_CODE (SET_SRC (body)))[0] == 'e'
		   && (r1 = XEXP (SET_SRC (body), 0),
		       GET_CODE (r1) == REG || GET_CODE (r1) == SUBREG))
	    win = combine_regs (r1, r0, b, insn_number, insn);
	  else if (GET_CODE (body) == PARALLEL)
	    {
	      rtx set1 = XVECEXP (body, 0, 0);
	      if ((r0 = SET_DEST (set1),
		   GET_CODE (r0) == REG || GET_CODE (r0) == SUBREG)
		  && GET_RTX_FORMAT (GET_CODE (SET_SRC (set1)))[0] == 'e'
		  && (r1 = XEXP (SET_SRC (set1), 0),
		      GET_CODE (r1) == REG || GET_CODE (r1) == SUBREG))
		win = combine_regs (r1, r0, b, insn_number, insn);
	    }

	  /* If registers were just tied, set COMBINED_REGNO
	     to the number of the register being set here that was tied.
	     It should not be assigned a new quantity in the normal
	     way for registers that are set.  */

	  if (win)
	    {
	      while (GET_CODE (r1) == SUBREG)
		r1 = SUBREG_REG (r1);
	      combined_regno = REGNO (r1);
	    }

	  /* Mark the death of everything that dies in this instruction,
	     except for anything that was just combined.
	     They can be found on the REG_NOTES list of the instruction.  */
	  for (link = REG_NOTES (insn); link; link = XEXP (link, 1))
	    if (XEXP (link, 0)
		&& (enum reg_note) GET_MODE (link) == REG_DEAD
		&& combined_regno != REGNO (XEXP (link, 0)))
	      {
		if (combined_regno >= 0 &&
		    reg_qty[combined_regno] == reg_qty[REGNO (XEXP (link, 0))])
		  /* Here for the death of the quotient in a divmod insn:
		     something that was born and dead in this insn
		     but combined with something else that also dies here.
		     Mark the qty as dying one instruction later.  */
		  wipe_dead_reg (XEXP (link, 0), insn_number,
				 insn_number + 1, b);
		else
		  wipe_dead_reg (XEXP (link, 0), insn_number, insn_number, b);
	      }
	    else if ((enum reg_note) GET_MODE (link) == REG_CONST)
	      {
		/* Also, if this insn introduces a "constant" register,
		   that could just be replaced by the value it is given here,
		   tell global-alloc not to allocate it
		   unless it is used at least twice more.  */
		i = REGNO (XEXP (link, 0));
		if (reg_n_refs[i] <= 2)
		  {
		    reg_live_length[i] = -1;
		    /* If value is not constant, we have a parameter
		       or a static chain pointer.  Tell local-alloc
		       as well not to allocate it.  */
		    if (! CONSTANT_ADDRESS_P (SET_SRC (PATTERN (insn))))
		      reg_basic_block[i] = -2;
		  }
		else
		  /* In any case, lower its priority for global-alloc.  */
		  reg_live_length[i] *= 2;
	      }

	  /* Allocate qty numbers for all registers local to this block
	     that are born (set) in this instruction.
	     A pseudo that already has a qty is not changed.  */

	  if (GET_CODE (PATTERN (insn)) == SET
	      && (GET_CODE (SET_DEST (PATTERN (insn))) == REG
		  || GET_CODE (SET_DEST (PATTERN (insn))) == SUBREG))
	    reg_is_born (SET_DEST (PATTERN (insn)), insn_number, b);
	  else if (GET_CODE (PATTERN (insn)) == CLOBBER)
	    reg_clobbered (XEXP (PATTERN (insn), 0), insn_number);
	  else if (GET_CODE (PATTERN (insn)) == PARALLEL)
	    for (i = XVECLEN (PATTERN (insn), 0) - 1; i >= 0; i--)
	      {
		if (GET_CODE (XVECEXP (PATTERN (insn), 0, i)) == SET
		    && (GET_CODE (SET_DEST (XVECEXP (PATTERN (insn), 0, i))) == REG
			|| GET_CODE (SET_DEST (XVECEXP (PATTERN (insn), 0, i))) == SUBREG))
		  reg_is_born (SET_DEST (XVECEXP (PATTERN (insn), 0, i)),
			       insn_number, b);
		else if (GET_CODE (XVECEXP (PATTERN (insn), 0, i)) == CLOBBER)
		  reg_clobbered (XEXP (XVECEXP (PATTERN (insn), 0, i), 0),
				 insn_number);
	      }
	}
      if (GET_CODE (insn) == CALL_INSN)
	call_seen = 1;
      if (insn == basic_block_end[b])
	break;
      /* We don't need this for the block's first instruction
	 since no regs we care about are live before that instruction.
	 Also we do not allocate space in regs_live_at for that instruction. */
      IOR_HARD_REG_SET (regs_live_at[insn_number], regs_live);
      insn = NEXT_INSN (insn);
    }

  /* Now every register that is local to this basic block
     has been given a hardware register (its reg_qty is < FIRST_PSEUDO_REGISTER)
     or is tied to something not local to this block (reg_qty is -1)
     or belongs to a qty with a known birth.  (Verify this now.)

     If a qty's death has not been established, it indicates a dead store.
     That is ok if the insn is not entirely dead.
     So set the qty'd death to just after its birth.  */

  for (i = FIRST_PSEUDO_REGISTER; i < next_qty; i++)
    {
      if (qty_birth[i] == 0)
	abort ();
      if (qty_death[i] == 0)
	qty_death[i] = qty_birth[i] + 1;
    }

  /* Now order the qtys so we assign them registers
     in order of decreasing length of life.  */
  qty_order = (short *) alloca (next_qty * sizeof (short));
  for (i = FIRST_PSEUDO_REGISTER; i < next_qty; i++)
    qty_order[i] = i;

#define EXCHANGE(I1, I2)  \
  { i = qty_order[I1]; qty_order[I1] = qty_order[I2]; qty_order[I2] = i; }

  if (next_qty == 2 + FIRST_PSEUDO_REGISTER)
    {
      if (qty_better_p (FIRST_PSEUDO_REGISTER + 1, FIRST_PSEUDO_REGISTER))
	EXCHANGE (FIRST_PSEUDO_REGISTER, FIRST_PSEUDO_REGISTER + 1);
    }
  else if (next_qty == 3 + FIRST_PSEUDO_REGISTER)
    {
      if (qty_better_p (FIRST_PSEUDO_REGISTER + 1, FIRST_PSEUDO_REGISTER))
	EXCHANGE (FIRST_PSEUDO_REGISTER, FIRST_PSEUDO_REGISTER + 1);
      if (qty_better_p (FIRST_PSEUDO_REGISTER + 2, FIRST_PSEUDO_REGISTER + 1))
	EXCHANGE (FIRST_PSEUDO_REGISTER + 2, FIRST_PSEUDO_REGISTER + 1);
      if (qty_better_p (FIRST_PSEUDO_REGISTER + 1, FIRST_PSEUDO_REGISTER))
	EXCHANGE (FIRST_PSEUDO_REGISTER, FIRST_PSEUDO_REGISTER + 1);
    }
  else if (next_qty > 3 + FIRST_PSEUDO_REGISTER)
    qsort (qty_order + FIRST_PSEUDO_REGISTER,
	   next_qty - FIRST_PSEUDO_REGISTER, sizeof (short), qty_better_p_1);

  /* Now for each qty that is not a hardware register,
     look for a hardware register to put it in.
     First try the register class that is cheapest for this qty,
     if there is more than one class.  */

  for (i = FIRST_PSEUDO_REGISTER; i < next_qty; i++)
    {
      q = qty_order[i];
      if (qty_size[q] >= 0)
	{
	  if (N_REG_CLASSES > 1)
	    {
	      qty_phys_reg[q] = find_free_reg (qty_crosses_call[q],
					       qty_reg_class[q],
					       qty_mode[q], q,
					       qty_birth[q], qty_death[q]);
	      if (qty_phys_reg[q] >= 0)
		continue;
	    }

	  qty_phys_reg[q] = find_free_reg (qty_crosses_call[q], GENERAL_REGS,
					   qty_mode[q], q,
					   qty_birth[q], qty_death[q]);
	}
    }

  /* Now propagate the register assignments
     to the pseudo regs belonging to the qtys.  */

  for (i = FIRST_PSEUDO_REGISTER; i < max_regno; i++)
    if (reg_qty[i] >= 0 && qty_phys_reg[reg_qty[i]] >= 0)
      {
	reg_renumber[i] = qty_phys_reg[reg_qty[i]] + reg_offset[i];
      }
}

/* Compare two quantities' priority for getting real registers.
   We give longer-lived quantities higher priority
   so that the shorter-lived ones will tend to be in the same places
   which gives in general the maximum room for the regs to
   be allocated by global-alloc.  */

static int
qty_better_p (q1, q2)
     int q1, q2;
{
  return ((qty_death[q1] - qty_birth[q1]) * qty_size[q2]
	  > (qty_death[q2] - qty_birth[q2]) * qty_size[q1]);
}

static int
qty_better_p_1 (q1, q2)
     short *q1, *q2;
{
  return ((qty_death[*q1] - qty_birth[*q1]) * qty_size[*q2]
	  > (qty_death[*q2] - qty_birth[*q2]) * qty_size[*q1]);
}

/* Attempt to combine the two registers (rtx's) USEDREG and SETREG.
   Returns 1 if have done so, or 0 if cannot.

   Combining registers means marking them as having the same quantity
   and adjusting the offsets within the quantity if either of
   them is a SUBREG).

   There are elaborate checks for the validity of combining.  */

   
static int
combine_regs (usedreg, setreg, b, insn_number, insn)
     rtx usedreg, setreg;
     int b;
     int insn_number;
     rtx insn;
{
  register int ureg, sreg;
  register int offset = 0;
  int usize, ssize;
  register int sqty;

  while (GET_CODE (usedreg) == SUBREG)
    {
      offset += SUBREG_WORD (usedreg);
      usedreg = SUBREG_REG (usedreg);
    }
  if (GET_CODE (usedreg) != REG)
    return 0;
  ureg = REGNO (usedreg);
  usize = REG_SIZE (usedreg);

  /* REG 0 is assigned implicitly by function calls,
     but since that is implicit, reg_is_born will not
     have been called for it.  Do so now
     if this is the first use following a function call.  */
  if (ureg == FUNCTION_VALUE_REGNUM
      && call_seen)
    {
      reg_is_born (usedreg, insn_number, -1);
      call_seen = 0;
    }

  while (GET_CODE (setreg) == SUBREG)
    {
      offset -= SUBREG_WORD (setreg);
      setreg = SUBREG_REG (setreg);
    }
  if (GET_CODE (setreg) != REG)
    return 0;
  sreg = REGNO (setreg);
  ssize = REG_SIZE (setreg);

  /* Do not combine registers unless one fits within the other.  */
  if (offset > 0 && usize + offset > ssize)
    return 0;
  if (offset < 0 && usize + offset < ssize)
    return 0;
  /* Do not combine with a smaller already-assigned object
     if that smaller object is already combined with something bigger
     or if that smaller object is a hard reg.
     In the latter case, we would implicitly be using consecutive
     hard regs, and there is no code to keep track of that.
     (This is overcautious; we could check that ssize actually
     requires more hard regs at this spot.)  */
  if (ssize > usize && reg_qty[ureg] >= 0
      && (usize < qty_size[reg_qty[ureg]]
	  || reg_qty[ureg] < FIRST_PSEUDO_REGISTER))
    return 0;

  /* Don't do anything with the non-allocatable registers.
     Also, don't tie a call-clobberable register
     to something that must live across calls.
     Also, don't tie a hardware register to anything larger than it.  */
  if (ureg < FIRST_PSEUDO_REGISTER)
    {
      if (fixed_regs[ureg])
	return 0;
      if (reg_crosses_call[sreg] && call_clobbered_regs[ureg])
	return 0;
      if (usize < ssize)
	return 0;
    }

  /* Don't tie something that crosses calles
     to something tied to a call-clobbered hardware register.  */
  if (reg_qty[ureg] < FIRST_PSEUDO_REGISTER && reg_qty[ureg] >= 0
      && call_clobbered_regs[reg_qty[ureg]]
      && reg_crosses_call[sreg])
    return 0;
  if (reg_qty[sreg] < FIRST_PSEUDO_REGISTER && reg_qty[sreg] >= 0
      && call_clobbered_regs[reg_qty[sreg]]
      && reg_crosses_call[ureg])
    return 0;

  if (sreg < FIRST_PSEUDO_REGISTER)
    {
      if (fixed_regs[sreg])
	return 0;
      if (reg_crosses_call[ureg] && call_clobbered_regs[sreg])
	return 0;
      if (ssize < usize)
	return 0;
    }

  /* Tying something to itself is ok iff no offset involved.  */

  if (ureg == sreg)
    return offset == 0;

  /* Don't try to connect two different hardware registers.  */

  if (ureg < FIRST_PSEUDO_REGISTER && sreg < FIRST_PSEUDO_REGISTER)
    return 0;

  /* Don't connect two different machine modes if they have different
     implications as to which registers may be used.  */

  if (!MODES_TIEABLE_P (GET_MODE (usedreg), GET_MODE (setreg)))
    return 0;

  /* Do nothing if SREG is a pseudo that already has a quantity.
     Also do nothing if it's a hard register that already has one,
     since that means it has been used already in this basic block
     and something else still live may already be tied to it.  */

  if (reg_qty[sreg] != -2)
    return 0;

  /* Summarize the status of what we know about SREG in SQTY:
     >= 0 for a hard reg, -2 for a pseudo local to the basic block,
     -1 for a pseudo not local to the basic block.
     Note that reg_n_deaths[sreg]==0 for a dead store.  */

  sqty = -2;
  if (sreg < FIRST_PSEUDO_REGISTER)
    sqty = sreg;
  else if (reg_basic_block[sreg] != b || reg_n_deaths[sreg] > 1)
    sqty = -1;

  /* For now, since global_alloc has no idea of tying,
     there is no use noting those local pseudos that could
     profitably be delayed till global_alloc and get tied to global ones.
     So right now give up if either SREG or UREG is a pseudo
     not local to the block.  */

  if (reg_qty[ureg] == -1 || sqty == -1)
    return 0;

  /* If SREG is not local to the basic block, or if it is a hard reg,
     then tie UREG (and all others it is tied to) to SREG.
     Only if UREG is a pseudo-reg local to this basic block
     and not already tied to a hardware register,
     and SREG is 1) external to the block or 2) a hardware register.
     Also if SREG is a hardware register insist that it be in the class
     that UREG and its other tied regs want to be in.  */

  if (sqty != -2 && ureg >= FIRST_PSEUDO_REGISTER
      && reg_qty[ureg] >= FIRST_PSEUDO_REGISTER
#if 0
/* qty_best_class would require info not currently computed until
   after this scan is complete.  */
      &&
      (sqty == -1 ||
       TEST_HARD_REG_BIT (reg_class_contents[(int) qty_best_class (reg_qty[ureg])],
			  sreg))
#else
      &&
      (sqty == -1 ||
       TEST_HARD_REG_BIT (reg_class_contents[(int) reg_preferred_class (ureg)],
			  sreg))
#endif
      )
    {
      /* We get rid of the quantity that ureg belongs to
	 and make all regs of that quantity get sqty instead.  */
      register int i;
      register int v = reg_qty[ureg];
      if (sqty == -1) offset = 0;
      else
	{
	  reg_is_born (setreg, insn_number, b);
	  post_mark_life (sqty, qty_mode[sqty], 1, qty_birth[v], insn_number);
	}

      qty_birth[sqty] = qty_birth[v];
      qty_death[v] = qty_birth[v]; /* So qty V won't occupy any hard reg */
      qty_crosses_call[sqty] |= qty_crosses_call[v];
      if (qty_size[v] > qty_size[sqty])
	{
	  qty_size[sqty] = qty_size[v];
	  qty_mode[sqty] = qty_mode[v];
	}
      for (i = 0; i < max_regno; i++)
	if (reg_qty[i] == v)
	  {
	    reg_qty[i] = sqty;
	    reg_offset[i] -= offset;
	  }
    }
  /* Else if we don't already know about SREG, tie it to UREG
     if this is the last use of UREG.
     If UREG is a hardware register (or tied to one), don't tie
     if it is not in the class that SREG wants.
     If UREG is not a hardware register, don't tie
     if it and SREG want different classes.  */
  else if (sqty == -2 && regno_dead_p (ureg, insn)
	   && (reg_qty[ureg] >= FIRST_PSEUDO_REGISTER
	       ? reg_classes_fit (ureg, sreg)
	       : TEST_HARD_REG_BIT (reg_class_contents[(int) reg_preferred_class (sreg)],
					reg_qty[ureg])))
    {
      if (reg_qty[ureg] == -2)
	reg_is_born (usedreg, insn_number, b);
      sqty = reg_qty[sreg] = reg_qty[ureg];
      reg_offset[sreg] = reg_offset[ureg] + offset;
      if (sqty >= 0)
	{
	  qty_crosses_call[sqty] |= reg_crosses_call[sreg];
	  if (usize < ssize)
	    {
	      register int i;
	      for (i = 0; i < max_regno; i++)
		if (reg_qty[i] == sqty)
		  reg_offset[i] -= offset;
	      qty_size[sqty] = ssize;
	      qty_mode[sqty] = GET_MODE (setreg);
	    }
	}
    }
  else
    return 0;

  return 1;
}

/* Return nonzero if R2's preferred class is the same as or contains
   R1's preferred class.  R1 and R2 are pseudo-register numbers.  */

int
reg_classes_fit (r1, r2)
     int r1, r2;
{
  register enum reg_class c1 = reg_preferred_class (r1);
  register enum reg_class c2 = reg_preferred_class (r2);
  if (c1 == c2) return 1;

  if (c2 == ALL_REGS)
  win:
    return 1;
  GO_IF_HARD_REG_SUBSET (reg_class_contents[(int)c1],
			 reg_class_contents[(int)c2],
			 win);
  return 0;
}

/* Handle the beginning of the life of register REG.
   REG can actually be a SUBREG instead of a REG.
   Note that combine_regs assumes that BLOCKNUM is irrelevant
   for hard registers.  */

static void
reg_is_born (reg, insn_number, blocknum)
     rtx reg;
     int insn_number;
     int blocknum;
{
  register int regno;

  while (GET_CODE (reg) == SUBREG)
    reg = SUBREG_REG (reg);

  regno = REGNO (reg);
  if (regno < FIRST_PSEUDO_REGISTER)
    {
      reg_qty[regno] = regno;
      qty_phys_reg[regno] = regno;
      qty_mode[regno] = GET_MODE (reg);
      mark_life (regno, GET_MODE (reg), 1);
    }
  else if (reg_qty[regno] >= -1)
    ;
  else if (reg_basic_block[regno] == blocknum
	   && reg_n_deaths[regno] == 1)
    alloc_qty (regno, GET_MODE (reg), PSEUDO_REGNO_SIZE (regno), insn_number);
  else
    reg_qty[regno] = -1;
}

/* Handle the clobberage of register REG in insn INSN_NUMBER.
   Just mark the register as in use, only just after this instruction.  */

static void
reg_clobbered (reg, insn_number)
     rtx reg;
     register int insn_number;
{
  register int regno;

  if (reg == 0)
    return;

  if (GET_CODE (reg) != REG)
    return;

  regno = REGNO (reg);
  if (regno < FIRST_PSEUDO_REGISTER)
    {
      register int lim = regno + HARD_REGNO_NREGS (regno, GET_MODE (reg));
      register int i;
      for (i = regno; i < lim; i++)
	SET_HARD_REG_BIT (regs_live_at[insn_number], i);
    }
}

/* Record the death in insn DEATH_INSN_NUMBER for the register REG.  */

static void
wipe_dead_reg (reg, this_insn_number, death_insn_number, blocknum)
     register rtx reg;
     int this_insn_number;
     int death_insn_number;
     int blocknum;
{
  register int regno = REGNO (reg);

  /* If a pseudo reg is referred to but was never set,
     we will find here that its qty is -2.
     Since these regs do not conflict with anything,
     mark them as born and dead in the same place.  */
  if (reg_qty[regno] == -2
      && regno >= FIRST_PSEUDO_REGISTER
      && reg_basic_block[regno] == blocknum
      && reg_n_deaths[regno] == 1)
    alloc_qty (regno, GET_MODE (reg), REG_SIZE (reg), this_insn_number);
  if (reg_qty[regno] >= 0)
    {
      qty_death[reg_qty[regno]] = death_insn_number;
      if (reg_qty[regno] < FIRST_PSEUDO_REGISTER)
	{
	  mark_life (reg_qty[regno], GET_MODE (reg), 0);
	  if (this_insn_number != death_insn_number)
	    post_mark_life (reg_qty[regno], GET_MODE (reg), 1,
			    this_insn_number, death_insn_number);
	}
    }
}

/* Find a block of SIZE words of hard regs in reg_class CLASS
   that can hold something of machine-mode MODE
     (but actually we test only the first of the block for holding MODE)
   and still free between insn BORN_INSN and insn DEAD_INSN,
   and return the number of the first of them.
   Return -1 if such a block cannot be found.
   If CALL_PRESERVED is nonzero, insist on registers preserved
   over subroutine calls, and return -1 if cannot find such.  */

static int
find_free_reg (call_preserved, class, mode, qty, born_insn, dead_insn)
     int call_preserved;
     enum reg_class class;
     enum machine_mode mode;
     int qty;
     int born_insn, dead_insn;
{
  register int i, ins;
  register HARD_REG_SET used;

  COPY_HARD_REG_SET (used,
		     call_preserved ? call_clobbered_reg_set : fixed_reg_set);

  for (ins = born_insn; ins < dead_insn; ins++)
    IOR_HARD_REG_SET (used, regs_live_at[ins]);

  IOR_COMPL_HARD_REG_SET (used, reg_class_contents[(int) class]);

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    if (! TEST_HARD_REG_BIT (used, i)
	&& HARD_REGNO_MODE_OK (i, mode))
      {
	register int j;
	register int size1 = HARD_REGNO_NREGS (i, mode);
	for (j = 1; j < size1 && ! TEST_HARD_REG_BIT (used, i + j); j++);
	if (j == size1)
	  {
	    post_mark_life (i, mode, 1, born_insn, dead_insn);
	    return i;
	  }
	i += j;			/* Skip starting points we know will lose */
      }
  return -1;
}

static void
mark_life (regno, mode, life)
     register int regno;
     enum machine_mode mode;
     int life;
{
  register int j = HARD_REGNO_NREGS (regno, mode);
  if (life)
    while (--j >= 0)
      SET_HARD_REG_BIT (regs_live, regno + j);
  else
    while (--j >= 0)
      CLEAR_HARD_REG_BIT (regs_live, regno + j);
}

static void
post_mark_life (regno, mode, life, birth, death)
     register int regno, life, birth;
     enum machine_mode mode;
     int death;
{
  register int j = HARD_REGNO_NREGS (regno, mode);
  register HARD_REG_SET this_reg;

  CLEAR_HARD_REG_SET (this_reg);
  while (--j >= 0)
    SET_HARD_REG_BIT (this_reg, regno + j);

  if (life)
    while (birth < death)
      {
	IOR_HARD_REG_SET (regs_live_at[birth], this_reg);
	birth++;
      }
  else
    while (birth < death)
      {
	AND_COMPL_HARD_REG_SET (regs_live_at[birth], this_reg);
	birth++;
      }
}

void
dump_local_alloc (file)
     FILE *file;
{
  register int i;
  for (i = FIRST_PSEUDO_REGISTER; i < max_regno; i++)
    if (reg_renumber[i] != -1)
      fprintf (file, ";; Register %d in %d.\n", i, reg_renumber[i]);
}
