/* Convert RTL to assembler code and output it, for GNU compiler.
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


/* This is the final pass of the compiler.
   It looks at the rtl code for a function and outputs assembler code.

   Final is responsible for changing pseudo-register references
   into hard regs or stack slots.  This is done by altering the
   REG rtx's for the pseudo regs into either hard regs or MEM rtx's.
   SUBREG rtx's must also be altered.

   Some optimizations are also done at this level.
   Move instructions that were made unnecessary by good register allocation
   are detected and omitted from the output.
   Instructions to set the condition codes are omitted when it can be
   seen that the condition codes already had the desired values.
   In some cases it is sufficient if the inherited condition codes
   have related values, but this may require the following insn
   (the one that tests the condition codes) to be modified.

   The code for the function prologue and epilogue are generated
   directly as assembler code by the macros FUNCTION_PROLOGUE and
   FUNCTION_EPILOGUE.  Those instructions never exist as rtl.  */

#include <stdio.h>
#include <stab.h>
#include "config.h"
#include "rtl.h"
#include "regs.h"
#include "insn-config.h"
#include "recog.h"
#include "conditions.h"

#define min(A,B) ((A) < (B) ? (A) : (B))

void output_asm_insn ();
static void alter_reg ();
static void alter_subreg ();
static int alter_cond ();
static void output_asm_label ();
static void output_operand ();
static void output_address ();
static void output_addr_reg ();
void output_addr_const ();

static char *reg_name[] = REGISTER_NAMES;

/* File in which assembler code is being written.  */

static FILE *outfile;

/* All the symbol-blocks (levels of scoping) in the compilation
   are assigned sequence numbers in order of appearance of the
   beginnings of the symbol-blocks.  Both final and dbxout do this,
   and assume that they will both give the same number to each block.
   Final uses these sequence numbers to generate assembler label names
   LBBnnn and LBEnnn for the beginning and end of the symbol-block.
   Dbxout uses the sequence nunbers to generate references to the same labels
   from the dbx debugging information.  */

static next_block_index;

/* This variable contains machine-dependent flags (defined in tm-...h)
   set and examined by output routines
   that describe how to interpret the condition codes properly.  */

CC_STATUS cc_status;

/* Last source file name mentioned in a NOTE insn.  */

static char *lastfile;

/* Indexed by hardware reg number, is 1 if that register is ever
   used in the current function.

   In life_analysis, or in stupid_life_analysis, this is set
   up to record the hard regs used explicitly.  Reload adds
   in the hard regs used for holding pseudo regs.  Final uses
   it to generate the code in the function prologue and epilogue
   to save and restore registers as needed.  */

char regs_ever_live[FIRST_PSEUDO_REGISTER];

/* Element N is nonzero if pseudo-reg N is being allocated in memory.
   The value of the element is an rtx (MEM ...) to be used
   to replace references to pseudo-reg N.
   This is set up at the end of global allocation.

   These MEM rtx's all have VOIDmode because we do not know the correct mode.
   When they are substituted into the code, they will be given the
   correct mode, copied from the (REG...) being replaced.  */

rtx *reg_spill_replacement;

/* Initialize data in final at the beginning of a compilation.  */

void
init_final (filename)
     char *filename;
{
  next_block_index = 2;
  lastfile = filename;
}

/* Main entry point for final pass: output assembler code from rtl.
   FIRST is the first insn of the rtl for the function being compiled.
   FILE is the file to write assembler code to.
   FNNAME is the name of the function being compiled.
   WRITE_SYMBOLS is 1 for gdb symbols, 2 for dbx symbols.
   OPTIMIZE is nonzero if we should eliminate redundant
     test and compare insns.  */

void
final (first, file, fnname, write_symbols, optimize)
     rtx first;
     FILE *file;
     char *fnname;
     int write_symbols;
     int optimize;
{
  register rtx insn;
  register int i;

  /* Length so far allocated in PENDING_BLOCKS.  */
  int max_depth = 20;
  /* Stack of sequence numbers of symbol-blocks of which we have seen the
     beginning but not yet the end.  Sequence numbers are assigned at
     the beginning; this stack allows us to find the sequence number
     of a block that is ending.  */
  int *pending_blocks = (int *) alloca (max_depth * sizeof (int));
  /* Number of elements currently in use in PENDING_BLOCKS.  */
  int depth = 0;

  /* Allocate in the stack frame whatever did not make it into a hard reg.  */

  reg_spill_replacement = (rtx *) alloca (max_regno * sizeof (rtx));
  bzero (reg_spill_replacement, max_regno * sizeof (rtx));

  /* Parameter copies that didn't get into hardware registers
     should be referenced in the parameter list.
     For other registers, allocate a local stack slot.  */

  for (i = FIRST_PSEUDO_REGISTER; i < max_regno; i++)
    {
      if (reg_renumber[i] < 0 && reg_n_refs[i] > 0)
	reg_spill_replacement[i]
	  = assign_stack_local (VOIDmode, PSEUDO_REGNO_BYTES (i));
      alter_reg (regno_reg_rtx[i]);
    }

  init_recog ();
  outfile = file;

  /* Tell assembler to switch to text segment.  */

  fprintf (file, "%s\n", TEXT_SECTION_ASM_OP);

  /* Tell assembler to move to target machine's alignment for functions.  */

  ASM_OUTPUT_ALIGN (file, floor_log2 (FUNCTION_BOUNDARY / BITS_PER_UNIT));

  /* Output label for the function.  */

  fprintf (file, "_%s:\n", fnname);

  /* Record beginning of the symbol-block that's the entire function.  */
  /* Is this incorrect now?  */

  if (write_symbols == 1)
    {
      pending_blocks[depth++] = next_block_index;
      fprintf (file, "\t.gdbbeg %d\n", next_block_index++);
    }

  /* Initial line number is supposed to be output
     before the function's prologue and label
     so that the function's address will not appear to be
     in the last statement of the preceding function.  */
  if (NOTE_LINE_NUMBER (first) != NOTE_INSN_DELETED)
    output_source_line (file, first);

#ifdef FUNCTION_PROLOGUE
  /* First output the function prologue: code to set up the stack frame.  */
  FUNCTION_PROLOGUE (file, get_frame_size ());
#endif

  CC_STATUS_INIT;

  for (insn = NEXT_INSN (first); insn; insn = NEXT_INSN (insn))
    {
      switch (GET_CODE (insn))
	{
	case NOTE:
	  if (! write_symbols)
	    break;
	  if (NOTE_LINE_NUMBER (insn) == NOTE_INSN_FUNCTION_BEG)
	    abort ();		/* Obsolete; shouldn't appear */
	  if (NOTE_LINE_NUMBER (insn) == NOTE_INSN_LOOP_BEG
	      || NOTE_LINE_NUMBER (insn) == NOTE_INSN_LOOP_END)
	    break;
	  if (NOTE_LINE_NUMBER (insn) == NOTE_INSN_DELETED)
	    break;		/* An insn that was "deleted" */
	  if (NOTE_LINE_NUMBER (insn) == NOTE_INSN_BLOCK_BEG)
	    {
	      /* Beginning of a symbol-block.  Assign it a sequence number
		 and push the number onto the stack PENDING_BLOCKS.  */

	      if (depth == max_depth)
		{
		  /* PENDING_BLOCKS is full; make it longer.  */
		  register int *new
		    = (int *) alloca (2 * max_depth * sizeof (int));
		  bcopy (pending_blocks, new, max_depth * sizeof (int));
		  pending_blocks = new;
		  max_depth <<= 1;
		}
	      pending_blocks[depth++] = next_block_index;

	      /* Output debugging info about the symbol-block beginning.  */

	      if (write_symbols == 2)
		fprintf (file, "LBB%d:\n", next_block_index++);
	      else
		fprintf (file, "\t.gdbbeg %d\n", next_block_index++);
	    }
	  else if (NOTE_LINE_NUMBER (insn) == NOTE_INSN_BLOCK_END)
	    {
	      /* End of a symbol-block.  Pop its sequence number off
		 PENDING_BLOCKS and output debugging info based on that.  */

	      if (write_symbols == 2)
		{
		  if (depth > 0)
		    fprintf (file, "LBE%d:\n", pending_blocks[--depth]);
		}
	      else
		fprintf (file, "\t.gdbend %d\n", pending_blocks[--depth]);
	    }
	  else
	    /* This note is a line-number.  */
	    output_source_line (file, insn);
	  break;

	case BARRIER:
	  break;

	case CODE_LABEL:
	  fprintf (file, "L%d:\n", CODE_LABEL_NUMBER (insn));
	  CC_STATUS_INIT;
	  break;

	default:
	  {
	    register rtx body = PATTERN (insn);
	    int insn_code_number;
	    char *template;

	    /* An INSN, JUMP_INSN or CALL_INSN.
	       First check for special kinds.  */
	       
	    if (GET_CODE (body) == USE /* These are just declarations */
		|| GET_CODE (body) == CLOBBER)
	      break;
	    if (GET_CODE (body) == ASM_INPUT)
	      {
		output_asm_insn (XSTR (body, 0), 0);
		break;
	      }

	    /* Detect insns that are really jump-tables
	       and output them as such.  */

	    if (GET_CODE (body) == ADDR_VEC)
	      {
		enum machine_mode mode = GET_MODE (body);
		char *pseudo = (mode == SImode) ? ".int"
		  : ((mode == HImode) ? ".word" : (char *) abort ());
		register int vlen, idx;
		vlen = XVECLEN (body, 0);
		for (idx = 0; idx < vlen; idx++)
		  fprintf (file, "\t%s L%d\n", pseudo,
			   CODE_LABEL_NUMBER (XEXP (XVECEXP (body, 0, idx), 0)));
		break;
	      }
	    if (GET_CODE (body) == ADDR_DIFF_VEC)
	      {
		enum machine_mode mode = GET_MODE (body);
		char *pseudo = (mode == SImode) ? ".int"
		  : ((mode == HImode) ? ".word" : (char *) abort ());
		register int vlen, idx;
		vlen = XVECLEN (body, 1);
		for (idx = 0; idx < vlen; idx++)
		  fprintf (file, "\t%s L%d-L%d\n", pseudo,
			   CODE_LABEL_NUMBER (XEXP (XVECEXP (body, 1, idx), 0)),
			   CODE_LABEL_NUMBER (XEXP (XEXP (body, 0), 0)));
		break;
	      }

	    /* We have a real machine instruction as rtl.  */

	    body = PATTERN (insn);

	    /* Detect and ignore no-op move instructions
	       resulting from not allocating a parameter in a register.  */

	    if (GET_CODE (body) == SET
		&& (SET_DEST (body) == SET_SRC (body)
		    || (GET_CODE (SET_DEST (body)) == MEM
			&& GET_CODE (SET_SRC (body)) == MEM
			&& (XEXP (SET_DEST (body), 0)
			    == XEXP (SET_SRC (body), 0))))
		&& GET_CODE (SET_DEST (body)) != VOLATILE)
	      break;

	    /* Detect and ignore no-op move instructions
	       resulting from smart or fortuitous register allocation.  */

	    if (GET_CODE (body) == SET)
	      {
		if (GET_CODE (SET_DEST (body)) == SUBREG)
		  alter_subreg (SET_DEST (body));
		if (GET_CODE (SET_SRC (body)) == SUBREG)
		  alter_subreg (SET_SRC (body));
		if (GET_CODE (SET_DEST (body)) == REG
		    && GET_CODE (SET_SRC (body)) == REG)
		  {
		    rtx tem;
		    if (REGNO (SET_DEST (body))
			== REGNO (SET_SRC (body)))
		      break;
		    tem = find_equiv_reg (SET_DEST (body), insn, 0,
					  REGNO (SET_SRC (body)), 0);
		    if (tem != 0
			&& GET_MODE (tem) == GET_MODE (SET_DEST (body)))
		      break;
		  }
	      }

	    /* Check for redundant test and compare instructions 
	       (when the condition codes are already set up as desired).
	       This is done only when optimizing; if not optimizing,
	       it should be possible for the user to alter a variable
	       with the debugger in between statements
	       and the next statement should reexamine the variable
	       to compute the condition codes.  */

	    if (optimize
		&& GET_CODE (body) == SET
		&& GET_CODE (SET_DEST (body)) == CC0)
	      {
		if (GET_CODE (SET_SRC (body)) == SUBREG)
		  alter_subreg (SET_SRC (body));
		if ((cc_status.value1 != 0
		     && rtx_equal_p (SET_SRC (body), cc_status.value1))
		    || (cc_status.value2 != 0
			&& rtx_equal_p (SET_SRC (body), cc_status.value2)))
		  break;
	      }

	    /* If this is a conditional branch, maybe modify it
	       if the cc's are in a nonstandard state
	       so that it accomplishes the same thing that it would
	       do straightforwardly if the cc's were set up normally.  */

	    if (cc_status.flags != 0
		&& GET_CODE (insn) == JUMP_INSN
		&& GET_CODE (body) == SET
		&& SET_DEST (body) == pc_rtx
		&& GET_CODE (SET_SRC (body)) == IF_THEN_ELSE)
	      {
		/* This function may alter the contents of its argument
		   and clear some of the cc_status.flags bits.
		   It may also return 1 meaning condition now always true
		   or -1 meaning condition now always false
		   or 2 meaning condition nontrivial but altered.  */
		register int result = alter_cond (XEXP (SET_SRC (body), 0));
		/* If condition now has fixed value, replace the IF_THEN_ELSE
		   with its then-operand or its else-operand.  */
		if (result == 1)
		  SET_SRC (body) = XEXP (SET_SRC (body), 1);
		if (result == -1)
		  SET_SRC (body) = XEXP (SET_SRC (body), 2);
		/* The jump is now either unconditional or a no-op.
		   If it has become a no-op, don't try to output it.
		   (It would not be recognized.)  */
		if (SET_SRC (body) == pc_rtx)
		  continue;
		/* Rerecognize the instruction if it has changed.  */
		if (result != 0)
		  INSN_CODE (insn) = -1;
	      }

	    /* Make same adjustments to instructions that examine the
	       condition codes without jumping.  */

	    if (cc_status.flags != 0
		&& GET_CODE (body) == SET)
	      switch (GET_CODE (SET_SRC (body)))
		{
		case GTU:
		case GT:
		case LTU:
		case LT:
		case GEU:
		case GE:
		case LEU:
		case LE:
		case EQ:
		case NE:
		  {
		    register int result = alter_cond (SET_SRC (body));
		    if (result == 1)
		      SET_SRC (body) = gen_rtx (CONST_INT, VOIDmode, -1);
		    if (result == -1)
		      SET_SRC (body) = const0_rtx;
		    if (result != 0)
		      INSN_CODE (insn) = -1;
		  }
		}

	    /* Try to recognize the instruction.
	       If successful, verify that the operands satisfy the
	       constraints for the instruction.  Crash if they don't,
	       since `reload' should have changed them so that they do.  */

	    insn_code_number = recog_memoized (insn);
	    insn_extract (insn);
	    for (i = 0; i < insn_n_operands[insn_code_number]; i++)
	      if (GET_CODE (recog_operand[i]) == SUBREG)
		alter_subreg (recog_operand[i]);

#ifdef REGISTER_CONSTRAINTS
	    if (! constrain_operands (insn_code_number))
	      abort ();
#endif

	    /* Update `cc_status' for this instruction.
	       The instruction's output routine may change it further.
	       This should be a no-op for jump instructions
	       because their output routines may need to examine `cc_status',
	       below.  That's ok since jump insns don't normally alter
	       the condition codes.  */

	    NOTICE_UPDATE_CC (body);

	    /* If the proper template needs to be chosen by some C code,
	       run that code and get the real template.
	       In this case the template we were passed
	       consists of * and a decimal number.
	       The number is used to select which case is run,
	       in output_insn_hairy, a machine-generated function
	       that all the C code for these situations is written into.  */

	    template = insn_template[insn_code_number];
	    if (template == 0)
	      template = output_insn_hairy (insn_code_number,
					    recog_operand, insn);

	    /* Output assembler code from the template.  */

	    output_asm_insn (template, recog_operand);
	  }
	}
    }

#ifdef FUNCTION_EPILOGUE
  /* Finally, output the function epilogue:
     code to restore the stack frame and return to the caller.  */
  FUNCTION_EPILOGUE (file, get_frame_size ());
#endif

  /* If FUNCTION_EPILOGUE is not defined, then the function body
     itself contains return instructions wherever needed.  */
}

/* Output debugging info to the assembler file
   based on the NOTE insn INSN, assumed to be a line number.  */

output_source_line (file, insn)
     FILE *file;
     rtx insn;
{
  register char *filename = NOTE_SOURCE_FILE (insn);
  if (filename && (lastfile == 0 || strcmp (filename, lastfile)))
    fprintf (file, "\t.stabs \"%s\",%d,0,0,Ltext\n",
	     filename, N_SOL);
  lastfile = filename;

  fprintf (file, "\t.stabd %d,0,%d\n",
	   N_SLINE, NOTE_LINE_NUMBER (insn));
}

/* Replace all pseudo regs in *X with their allocated homes:
   either a hard reg found in reg_renumber
   or a memory location found in reg_spill_replacement.  */

static void
alter_subreg (x)
     register rtx x;
{
  register rtx y = SUBREG_REG (x);
  if (GET_CODE (y) == SUBREG)
    alter_subreg (y);

  if (GET_CODE (y) == REG)
    {
      /* If the containing reg really gets a hard reg, so do we.  */
      PUT_CODE (x, REG);
      REGNO (x) = REGNO (y) + SUBREG_WORD (x);
    }
  else if (GET_CODE (y) == MEM)
    {
      register int offset = SUBREG_WORD (x) * BITS_PER_WORD;
#ifdef BYTES_BIG_ENDIAN
      if (GET_MODE_SIZE (GET_MODE (x)) < UNITS_PER_WORD)
	offset -= (GET_MODE_SIZE (GET_MODE (x))
		   - min (UNITS_PER_WORD, GET_MODE_SIZE (GET_MODE (y))));
#endif
      PUT_CODE (x, MEM);
      XEXP (x, 0) = plus_constant (XEXP (y, 0), offset);
    }
}

static void
alter_reg (reg)
     rtx reg;
{
  register int regno = REGNO (reg);

  if (reg_spill_replacement[regno] != 0)
    {
      PUT_CODE (reg, MEM);
      XEXP (reg, 0) = XEXP (reg_spill_replacement[regno], 0);
    }
  else
    REGNO (reg) = reg_renumber[regno];

  return;
}

/* Given BODY, the body of a jump instruction, alter the jump condition
   as required by the bits that are set in cc_status.flags.
   Not all of the bits there can be handled at this level in all cases.
   The bits that are taken care of here are cleared.

   The value is normally 0.
    In this case, COND itself has usually been altered.
   1 means that the condition has become always true.
   -1 means that the condition has become always false.  */

static int
alter_cond (cond)
     register rtx cond;
{
  int value = 0;

  if (cc_status.flags & CC_REVERSED)
    {
      value = 2;
      switch (GET_CODE (cond))
	{
	case LE:
	  PUT_CODE (cond, GE);
	  break;
	case GE:
	  PUT_CODE (cond, LE);
	  break;
	case LT:
	  PUT_CODE (cond, GT);
	  break;
	case GT:
	  PUT_CODE (cond, LT);
	  break;
	case LEU:
	  PUT_CODE (cond, GEU);
	  break;
	case GEU:
	  PUT_CODE (cond, LEU);
	  break;
	case LTU:
	  PUT_CODE (cond, GTU);
	  break;
	case GTU:
	  PUT_CODE (cond, LTU);
	  break;
	}
    }

  if (cond != 0 && cc_status.flags & CC_NOT_POSITIVE)
    switch (GET_CODE (cond))
      {
      case LE:
      case LEU:
      case GEU:
	/* Jump becomes unconditional.  */
	return 1;

      case GT:
      case GTU:
      case LTU:
	/* Jump becomes no-op.  */
	return -1;

      case GE:
	PUT_CODE (cond, EQ);
	value = 2;
	break;

      case LT:
	PUT_CODE (cond, NE);
	value = 2;
	break;
      }

  if (cond != 0 && cc_status.flags & CC_NOT_NEGATIVE)
    switch (GET_CODE (cond))
      {
      case GE:
      case GEU:
	/* Jump becomes unconditional.  */
	return 1;

      case LT:
      case LTU:
	/* Jump becomes no-op.  */
	return -1;

      case LE:
      case LEU:
	PUT_CODE (cond, EQ);
	value = 2;
	break;

      case GT:
      case GTU:
	PUT_CODE (cond, NE);
	value = 2;
	break;
      }

  if (cond != 0 && cc_status.flags & CC_NO_OVERFLOW)
    switch (GET_CODE (cond))
      {
      case GEU:
	/* Jump becomes unconditional.  */
	return 1;

      case LEU:
	PUT_CODE (cond, EQ);
	value = 2;
	break;

      case GTU:
	PUT_CODE (cond, NE);
	value = 2;
	break;

      case LTU:
	/* Jump becomes no-op.  */
	return -1;
      }

  return value;
}

/* Output of assembler code from a template, and its subroutines.  */

/* Output text from TEMPLATE to the assembler output file,
   obeying %-directions to substitute operands taken from
   the vector OPERANDS.

   %N (for N a digit) means print operand N in usual manner.
   %lN means require operand N to be a CODE_LABEL or LABEL_REF
      and print the label name with no punctuation.
   %cN means require operand N to be a constant
      and print the constant expression with no punctuation.
   %aN means expect operand N to be a memory address
      (not a memory reference!) and print a reference
      to that address.
   %nN means expect operand N to be a constant
      and print a constant expression for minus the value
      of the operand, with no other punctuation.  */

void
output_asm_insn (template, operands)
     char *template;
     rtx *operands;
{
  register char *p;
  register int c;

  p = template;
  putc ('\t', outfile);
  while (c = *p++)
    {
      if (c != '%')
	putc (c, outfile);
      else
	{
	  if (*p == 'l')
	    {
	      c = atoi (++p);
	      output_asm_label (operands[c]);
	    }
	  else if (*p == 'c')
	    {
	      c = atoi (++p);
	      output_addr_const (outfile, operands[c]);
	    }
	  else if (*p == 'a')
	    {
	      c = atoi (++p);
	      output_address (operands[c]);
	    }
	  else if (*p == 'n')
	    {
	      c = atoi (++p);
	      if (GET_CODE (operands[c]) == CONST_INT)
		fprintf (outfile, "%d", - INTVAL (operands[c]));
	      else
		{
		  putc ('-', outfile);
		  output_addr_const (operands[c]);
		}
	    }
	  else
	    {
	      c = atoi (p);
	      output_operand (operands[c]);
	    }
	  while ((c = *p) >= '0' && c <= '9') p++;
	}
    }

  putc ('\n', outfile);
}

static void
output_asm_label (x)
     rtx x;
{
  if (GET_CODE (x) == LABEL_REF)
    fprintf (outfile, "L%d", CODE_LABEL_NUMBER (XEXP (x, 0)));
  else if (GET_CODE (x) == CODE_LABEL)
    fprintf (outfile, "L%d", CODE_LABEL_NUMBER (x));
  else
    abort ();
}

/* Print operand X using machine-dependent assembler syntax.
   The macro PRINT_OPERAND is defined just to control this function.  */

static void
output_operand (x)
     rtx x;
{
  if (GET_CODE (x) == SUBREG)
    alter_subreg (x);
  PRINT_OPERAND (outfile, x);
}

/* Print a memory reference operand for address X
   using machine-dependent assembler syntax.
   The macro PRINT_OPERAND_ADDRESS exists just to control this function.  */

static void
output_address (x)
     rtx x;
{
  if (GET_CODE (x) == SUBREG)
    alter_subreg (x);
  PRINT_OPERAND_ADDRESS (outfile, x);
}

/* Print an integer constant expression in assembler syntax.
   Addition and subtraction are the only arithmetic
   that may appear in these expressions.  */

void
output_addr_const (file, x)
     FILE *file;
     rtx x;
{
 restart:
  switch (GET_CODE (x))
    {
    case SYMBOL_REF:
      if (XSTR (x, 0)[0] == '*')
	fprintf (file, "%s", XSTR (x, 0) + 1);
      else
	fprintf (file, "_%s", XSTR (x, 0));
      break;

    case LABEL_REF:
      fprintf (file, "L%d", CODE_LABEL_NUMBER (XEXP (x, 0)));
      break;

    case CODE_LABEL:
      fprintf (file, "L%d", CODE_LABEL_NUMBER (x));
      break;

    case CONST_INT:
      fprintf (file, "%d", INTVAL (x));
      break;

    case CONST:
      x = XEXP (x, 0);
      goto restart;

    case PLUS:
      output_addr_const (file, XEXP (x, 0));
      fprintf (file, "+");
      output_addr_const (file, XEXP (x, 1));
      break;

    case MINUS:
      output_addr_const (file, XEXP (x, 0));
      fprintf (file, "-");
      output_addr_const (file, XEXP (x, 1));
      break;

    default:
      abort ();
    }
}
