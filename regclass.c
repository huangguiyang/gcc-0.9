/* Compute register class preferences for pseudo-registers.
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


/* This file contains two passes of the compiler: reg_scan and reg_class.  */

#include "config.h"
#include "rtl.h"
#include "regs.h"
#include "insn-config.h"
#include "recog.h"

#define max(A,B) ((A) > (B) ? (A) : (B))
#define min(A,B) ((A) < (B) ? (A) : (B))

/* savings[R].savings[CL] is the amount saved by putting register R
   in class CL.  This data is used within `regclass' and freed
   when it is finished.  */

struct savings
{
  short savings[N_REG_CLASSES];
};

static struct savings *savings;

/* (enum reg_class) prefclass[R] is the preferred class for register R.
   This is available after `regclass' is run.  */

static char *prefclass;

static enum reg_class reg_class_subunion[N_REG_CLASSES][N_REG_CLASSES]
  = REG_CLASS_SUBUNION;

static enum reg_class reg_class_subclasses[N_REG_CLASSES][N_REG_CLASSES]
  = REG_CLASS_SUBCLASSES;

/* Indexed by pseudo register number, gives uid of first insn using the reg
   (as of the time reg_scan is called).  */

short *regno_first_uid;

/* Indexed by pseudo register number, gives uid of last insn using the reg
   (as of the time reg_scan is called).  */

short *regno_last_uid;

void reg_class_record ();
void record_address_regs ();
void reg_scan_mark_refs ();


/* Return the reg_class in which pseudo reg number REGNO is best allocated.
   This function is sometimes called before the info has been computed.
   When that happens, just return GENERAL_REGS, which is innocuous.  */

enum reg_class
reg_preferred_class (regno)
     int regno;
{
  if (prefclass == 0)
    return GENERAL_REGS;
  return (enum reg_class) prefclass[regno];
}

/* This is a pass of the compiler that scans all instructions
   and calculates the preferred class for each pseudo-register.
   This information can be accessed later by calling `reg_preferred_class'.
   This pass comes just before local register allocation.  */

regclass (f, nregs)
     rtx f;
     int nregs;
{
#ifdef REGISTER_CONSTRAINTS
  register rtx insn;
  register int i;

  init_recog ();

  /* Zero out our accumulation of the cost of each class for each reg.  */

  savings = (struct savings *) alloca (nregs * sizeof (struct savings));
  bzero (savings, nregs * sizeof (struct savings));

  /* Scan the instructions and record each time it would
     save code to put a certain register in a certain class.  */

  for (insn = f; insn; insn = NEXT_INSN (insn))
    if ((GET_CODE (insn) == INSN
	 && GET_CODE (PATTERN (insn)) != USE
	 && GET_CODE (PATTERN (insn)) != CLOBBER
	 && GET_CODE (PATTERN (insn)) != ASM_INPUT)
	|| (GET_CODE (insn) == JUMP_INSN
	    && GET_CODE (PATTERN (insn)) != ADDR_VEC
	    && GET_CODE (PATTERN (insn)) != ADDR_DIFF_VEC)
	|| GET_CODE (insn) == CALL_INSN)
      {
	int insn_code_number = recog_memoized (insn);

	insn_extract (insn);

	for (i = insn_n_operands[insn_code_number] - 1; i >= 0; i--)
	  reg_class_record (recog_operand[i],
			    &insn_operand_constraint[insn_code_number][i]);
      }

  /* Now for each register look at how desirable each class is
     and find which class is preferred.  Store that in `prefclass[REGNO]'.  */
    
  prefclass = (char *) oballoc (nregs);

  for (i = FIRST_PSEUDO_REGISTER; i < nregs; i++)
    {
      register int best_savings = 0;
      enum reg_class best = ALL_REGS;

      /* This is an enum reg_class, but we call it an int
	 to save lots of casts.  */
      register int class;
      register struct savings *p = &savings[i];

      for (class = (int) ALL_REGS - 1; class > 0; class--)
	{
	  if (p->savings[class] > best_savings)
	    {
	      best_savings = p->savings[class];
	      best = (enum reg_class) class;
	    }
	  else if (p->savings[class] == best_savings)
	    {
	      best = reg_class_subunion[(int)best][class];
	    }
	}

#if 0
      /* Note that best_savings is twice number of places something
	 is saved.  */
      if ((best_savings - p->savings[(int) GENERAL_REGS]) * 5 < reg_n_refs[i])
	prefclass[i] = (char) GENERAL_REGS;
      else
	prefclass[i] = (char) best;
#else
      prefclass[i] = (char) best;
#endif
    }
#endif /* REGISTER_CONSTRAINTS */
}

#ifdef REGISTER_CONSTRAINTS

/* Scan an operand OP to which the constraint *CONSTRAINT_LOC should apply
   and record the preferred register classes from the constraint for OP
   if OP is a register.  If OP is a memory reference, record suitable
   preferences for registers used in the address.

   We can deduce both the insn code number and which operand in the insn
   this is supposed to be from the position of CONSTRAINT_LOC
   in the table of constraints.  */

void
reg_class_record (op, constraint_loc)
     rtx op;
     char **constraint_loc;
{
  char *constraint = *constraint_loc;
  register char *p;
  register enum reg_class class = NO_REGS;
  char *next = 0;
  int insn_code = (constraint_loc - insn_operand_constraint[0]) / MAX_RECOG_OPERANDS;

  while (1)
    {
      if (GET_CODE (op) == VOLATILE
	  || GET_CODE (op) == UNCHANGING)
	op = XEXP (op, 0);
      else if (GET_CODE (op) == SUBREG)
	op = SUBREG_REG (op);
      else break;
    }

  /* Memory reference: scan the address.  */

  if (GET_CODE (op) == MEM)
    record_address_regs (XEXP (op, 0), 2, 0);

  if (GET_CODE (op) != REG)
    {
      /* If the constraint says the operand is supposed to BE an address,
	 scan it as one.  */

      if (constraint != 0 && constraint[0] == 'p')
	record_address_regs (op, 2, 0);
      return;
    }

  /* Operand is a register: examine the constraint for specified classes.  */

  for (p = constraint; *p || next; p++)
    {
      if (*p == 0)
	{
	  p = next;
	  next = 0;
	}
      switch (*p)
	{
	case '=':
	case '+':
	case '?':
	case '#':
	case '!':
	case '%':
	case 'F':
	case 'G':
	case 'H':
	case 'i':
	case 's':
	case 'm':
	case 'o':
	case 'p':
	case ',':
	  break;

	  /* * means ignore following letter
	     when choosing register preferences.  */
	case '*':
	  p++;
	  break;

	case 'g':
	case 'r':
	  if (GENERAL_REGS == ALL_REGS)
	    return;
	  class = GENERAL_REGS;
	  break;

	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	  /* If constraint says "match another operand",
	     use that operand's constraint to choose preferences.  */
	  next = insn_operand_constraint[insn_code][*p - '0'];
	  break;

	default:
	  class
	    = reg_class_subunion[(int) class][(int) REG_CLASS_FROM_LETTER (*p)];
	}
    }

  {
    register int i;
    register struct savings *pp;
    register enum reg_class class1;
    pp = &savings[REGNO (op)];

    /* Increment the savings for this reg
       for each class contained in the one the constraint asks for.  */

    if (class != NO_REGS && class != ALL_REGS)
      {
	pp->savings[(int) class] += 2;
	for (i = 0; ; i++)
	  {
	    class1 = reg_class_subclasses[(int)class][i];
	    if (class1 == LIM_REG_CLASSES)
	      break;
	    pp->savings[(int) class1] += 2;
	  }
      }
  }
}

/* Record the pseudo registers we must reload into hard registers
   in a subexpression of a memory address, X.
   BCOST is the cost if X is a register and it fails to be in BASE_REG_CLASS.
   ICOST is the cost if it fails to be in INDEX_REG_CLASS. */

void
record_address_regs (x, bcost, icost)
     rtx x;
     int bcost, icost;
{
  register RTX_CODE code = GET_CODE (x);

  switch (code)
    {
    case CONST_INT:
    case CONST:
    case CC0:
    case PC:
    case SYMBOL_REF:
    case LABEL_REF:
      return;

    case PLUS:
      /* When we have an address that is a sum,
	 we must determine whether registers are "base" or "index" regs.
	 If there is a sum of two registers, we must choose one to be
	 the "base".  Luckily, we can use the REGNO_POINTER_FLAG
	 to make a good choice most of the time.  */
      {
	register RTX_CODE code0 = GET_CODE (XEXP (x, 0));
	register RTX_CODE code1 = GET_CODE (XEXP (x, 1));
	int icost0 = 0;
	int icost1 = 0;
	int suppress1 = 0;
	int suppress0 = 0;

	if (code0 == MULT || code1 == MEM)
	  icost0 = 2;
	else if (code1 == MULT || code0 == MEM)
	  icost1 = 2;
	else if (code0 == CONST_INT)
	  suppress0 = 1;
	else if (code1 == CONST_INT)
	  suppress1 = 1;
	else if (code0 == REG && code1 == REG)
	  {
	    if (REGNO_POINTER_FLAG (REGNO (XEXP (x, 0))))
	      icost1 = 2;
	    else if (REGNO_POINTER_FLAG (REGNO (XEXP (x, 1))))
	      icost0 = 2;
	    else
	      icost0 = icost1 = 1;
	  }
	else if (code0 == REG)
	  {
	    if (code1 == PLUS
		&& ! REGNO_POINTER_FLAG (REGNO (XEXP (x, 0))))
	      icost0 = 2;
	    else
	      REGNO_POINTER_FLAG (REGNO (XEXP (x, 0))) = 1;
	  }
	else if (code1 == REG)
	  {
	    if (code0 == PLUS
		&& ! REGNO_POINTER_FLAG (REGNO (XEXP (x, 1))))
	      icost1 = 2;
	    else
	      REGNO_POINTER_FLAG (REGNO (XEXP (x, 1))) = 1;
	  }

	/* ICOST0 determines whether we are treating operand 0
	   as a base register or as an index register.
	   SUPPRESS0 nonzero means it isn't a register at all.
	   ICOST1 and SUPPRESS1 are likewise for operand 1.  */

	if (! suppress0)
	  record_address_regs (XEXP (x, 0), 2 - icost0, icost0);
	if (! suppress1)
	  record_address_regs (XEXP (x, 1), 2 - icost1, icost1);
      }
      break;

    case POST_INC:
    case PRE_INC:
    case POST_DEC:
    case PRE_DEC:
      /* Double the importance of a pseudo register that is incremented
	 or decremented, since it would take two extra insns
	 if it ends up in the wrong place.  */
      record_address_regs (XEXP (x, 0), 2 * bcost, 2 * icost);
      break;

    case REG:
      {
	register struct savings *pp;
	register enum reg_class class, class1;
	pp = &savings[REGNO (x)];

	/* We have an address (or part of one) that is just one register.  */

	/* Record BCOST worth of savings for classes contained
	   in BASE_REG_CLASS.  */

	class = BASE_REG_CLASS;
	if (class != NO_REGS && class != ALL_REGS)
	  {
	    register int i;
	    pp->savings[(int) class] += bcost;
	    for (i = 0; ; i++)
	      {
		class1 = reg_class_subclasses[(int)class][i];
		if (class1 == LIM_REG_CLASSES)
		  break;
		pp->savings[(int) class1] += bcost;
	      }
	  }

	/* Record ICOST worth of savings for classes contained
	   in INDEX_REG_CLASS.  */

	class = INDEX_REG_CLASS;
	if (class != NO_REGS && class != ALL_REGS)
	  {
	    register int i;
	    pp->savings[(int) class] += icost;
	    for (i = 0; ; i++)
	      {
		class1 = reg_class_subclasses[(int)class][i];
		if (class1 == LIM_REG_CLASSES)
		  break;
		pp->savings[(int) class1] += icost;
	      }
	  }
      }
      break;

    default:
      {
	register char *fmt = GET_RTX_FORMAT (code);
	register int i;
	for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
	  if (fmt[i] == 'e')
	    record_address_regs (XEXP (x, i), bcost, icost);
      }
    }
}
#endif /* REGISTER_CONSTRAINTS */

/* This is a pass of the compiler run before cse.
   It sets up the vectors regno_first_uid, regno_last_uid.  */

reg_scan (f, nregs)
     rtx f;
     int nregs;
{
  register int i;
  register rtx last, insn;

  /* This prevents dump_flow_info from losing if called
     before regclass is run.  */
  prefclass = 0;

  regno_first_uid = (short *) oballoc (nregs * sizeof (short));
  bzero (regno_first_uid, nregs * sizeof (short));

  regno_last_uid = (short *) oballoc (nregs * sizeof (short));
  bzero (regno_last_uid, nregs * sizeof (short));

  for (insn = f; insn; insn = NEXT_INSN (insn))
    if (GET_CODE (insn) == INSN
	|| GET_CODE (insn) == CALL_INSN
	|| GET_CODE (insn) == JUMP_INSN)
      reg_scan_mark_refs (PATTERN (insn), INSN_UID (insn));
}

void
reg_scan_mark_refs (x, uid)
     rtx x;
     int uid;
{
  register RTX_CODE code = GET_CODE (x);

  switch (code)
    {
    case CONST_INT:
    case CONST:
    case CONST_DOUBLE:
    case CC0:
    case PC:
    case SYMBOL_REF:
    case LABEL_REF:
      return;

    case REG:
      {
	register int regno = REGNO (x);

	regno_last_uid[regno] = uid;
	if (regno_first_uid[regno] == 0)
	  regno_first_uid[regno] = uid;
      }
      break;

    default:
      {
	register char *fmt = GET_RTX_FORMAT (code);
	register int i;
	for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
	  {
	    if (fmt[i] == 'e')
	      reg_scan_mark_refs (XEXP (x, i), uid);
	    else if (fmt[i] == 'E')
	      {
		register int j;
		for (j = XVECLEN (x, i) - 1; j >= 0; j--)
		  reg_scan_mark_refs (XVECEXP (x, i, j), uid);		  
	      }
	  }
      }
    }
}
