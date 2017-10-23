/* Expands front end tree to back end RTL for GNU C-Compiler
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


/* This file handles the generation of rtl code from tree structure
   at the level of statements using subroutines in exp*.c and emit-rtl.c.
   It also creates the rtl expressions for parameters and auto variables
   and has full responsibility for allocating stack slots.
   A few routines in this file are called during later passes
   when stack frame management requires it.

   The main entry point is expand_function, which is at the end.  */

#include "config.h"

#include <stdio.h>

#include "rtl.h"
#include "tree.h"
#include "insn-flags.h"
#include "stupid.h"
#include "expr.h"

#define MAX(x,y) (((x) > (y)) ? (x) : (y))
#define MIN(x,y) (((x) < (y)) ? (x) : (y))

/* Label that will go on function epilogue.
   Jumping to this label serves as a "return" instruction
   on machines which require execution of the epilogue on all returns.  */

static rtx return_label;

/* The FUNCTION_DECL node for the function being compiled.  */

static tree this_function;

/* Offset to end of allocated area of stack frame.
   If stack grows down, this is the address of the last stack slot allocated.
   If stack grows up, this is the address for the next slot.  */
static int frame_offset;

/* Length in bytes of largest structure value returned by
   any function called so far in this function.  */
static int max_structure_value_size;

/* Label to jump back to for tail recursion, or 0 if we have
   not yet needed one for this function.  */
static rtx tail_recursion_label;

/* Place after which to insert the tail_recursion_label if we need one.  */
static rtx tail_recursion_reentry;

static int tail_recursion_args ();

/* Estimate the complexity of the compiled code for STMT.
   This is a rough estimate and is used for purposes
   of deciding which optimizations are worth applying.  */

static int
stmt_complexity (stmt)
     tree stmt;
{
  register tree s;
  register int c = 0;
  for (s = stmt; s; s = TREE_CHAIN (s))
    switch (TREE_CODE (s))
      {
      case LABEL_STMT:
	break;

      case COMPOUND_STMT:
	c += stmt_complexity (STMT_BODY (s));
	break;

      case LOOP_STMT:
	c += 1 + stmt_complexity (STMT_BODY (s));
	break;

      case EXIT_STMT:
	c += 2;
	break;

      case GOTO_STMT:
      case ASM_STMT:
	c += 1;
	break;

      case IF_STMT:
	c += 2 + stmt_complexity (STMT_THEN (s))
	  + stmt_complexity (STMT_ELSE (s));
	break;

      case EXPR_STMT:
      case RETURN_STMT:
	c += 3;
	break;

	/* The body of the case statement is actually not included
	   in the CASE_STMT, so it will be counted separately.  */
      case CASE_STMT:
	c += 3;
	break;

      case LET_STMT:
      case WITH_STMT:
	c += list_length (STMT_VARS (s)) + stmt_complexity (STMT_BODY (s));
	break;

      default: abort ();
      }
  return c;
}

static void expand_stmt ();
static void expand_stmts ();
static void expand_case_stmt ();

/* Set nonzero at beginning of function
   to prevent output of the NOTE_INSN_BLOCK_BEG for the outermost block.
   This is because `final' generates it specially,
   before the function prologue.  */

static int inhibit_block_beg;

/* Return the rtx-label that corresponds to a LABEL_DECL,
   creating it if necessary.  */

static rtx
label_rtx (label)
     tree label;
{
  if (DECL_RTL (label))
    return DECL_RTL (label);

  return DECL_RTL (label) = gen_label_rtx ();
}

/* Add an unconditional jump to LABEL as the next sequential instruction.  */

void
emit_jump (label)
     rtx label;
{
  do_pending_stack_adjust ();
  emit_jump_insn (gen_jump (label));
  emit_barrier ();
}

/* Return nonzero if T is a "simple enough" expression
   such that we prefer to duplicate it as a loop exit condition.
   We accept only comparisons whose operands are constants or variables.  */

static int
exit_simple_enough_p (t)
     tree t;
{
  register enum tree_code code = TREE_CODE (t);
  register tree op;
  if (!(code == EQ_EXPR || code == NE_EXPR
	|| code == LT_EXPR || code == LE_EXPR
	|| code == GT_EXPR || code == GE_EXPR))
    return 0;
  op = TREE_OPERAND (t, 0);
  if (TREE_CODE (op) != VAR_DECL
      && TREE_CODE (op) != INTEGER_CST
      && TREE_CODE (op) != REAL_CST)
    return 0;
  op = TREE_OPERAND (t, 1);
  if (TREE_CODE (op) != VAR_DECL
      && TREE_CODE (op) != INTEGER_CST
      && TREE_CODE (op) != REAL_CST)
    return 0;
  return 1;
}

/* Generate rtl code for a sequence of statements
   chained through the TREE_CHAIN.
   LOOP_EXIT says where an EXIT_STMT should jump to.  */

static void
expand_stmts (stmts, loop_exit)
     tree stmts;
     rtx loop_exit;
{
  register tree stmt;
  for (stmt = stmts; stmt; stmt = TREE_CHAIN (stmt))
    expand_stmt (stmt, loop_exit);
}

/* Generate rtl for one statement, STMT.
   LOOP_EXIT is an rtl CODE_LABEL to jump to to exit a loop.  */

/* Stack of LET_STMT blocks that we are currently within
   during the rtl-generation tree walk. */

struct block_stack
{
  tree block;			/* the LET_STMT tree node */
  rtx stack_level;		/* the saved-on-entry stack pointer */
  struct block_stack *next;	/* data for the containing LET_STMT or 0 */
};

struct block_stack *block_stack;

static void
expand_stmt (stmt, loop_exit)
     tree stmt;
     rtx loop_exit;
{
  struct block_stack thisblock;

  if (STMT_SOURCE_LINE (stmt) != 0)
    emit_note (STMT_SOURCE_FILE (stmt), STMT_SOURCE_LINE (stmt));

  switch (TREE_CODE (stmt))
    {
    case LABEL_STMT:
      do_pending_stack_adjust ();
      emit_label (label_rtx (STMT_BODY (stmt)));
      break;

    case GOTO_STMT:
      if (GET_CODE (label_rtx (STMT_BODY (stmt))) != CODE_LABEL)
	abort ();
      /* Look at the binding contours (LET_STMTs) we are jumping out of
	 and if any of them allocates a variable size auto variable
	 reset the stack to the appropriate level.  */
      {
	tree context = DECL_CONTEXT (STMT_BODY (stmt));
	struct block_stack *block;
	rtx stack_level = 0;

	/* Chase contexts up from the target label.  */
	while (context)
	  {
	    /* Chase contexts up from where we are.
	       We want the innermost block containing both
	       the goto and the label.  */
	    for (block = block_stack; block;
		 block = block->next)
	      {
		if (block->stack_level != 0)
		  stack_level = block->stack_level;
		if (block->block == context)
		  {
		    if (stack_level != 0)
		      emit_move_insn (gen_rtx (REG, Pmode,
					       STACK_POINTER_REGNUM),
				      stack_level);
		    goto context_done;
		  }
	      }
	    context = STMT_SUPERCONTEXT (context);
	  }
	context_done: ;
      }
      emit_jump (label_rtx (STMT_BODY (stmt)));
      break;

    case EXPR_STMT:
      expand_expr (STMT_BODY (stmt), 0, VOIDmode, 0);
      break;

    case COMPOUND_STMT:
      expand_stmts (STMT_BODY (stmt), loop_exit);
      break;

    case ASM_STMT:
      emit_insn (gen_rtx (ASM_INPUT, VOIDmode,
			  TREE_STRING_POINTER (STMT_BODY (stmt))));
      break;

    case IF_STMT:
      {
	register rtx afterlabel = gen_label_rtx ();

	/* Simpler handling if there is no else-part
	   or a null then-part.  */
	if (STMT_THEN (stmt) == 0)
	  {
	    do_jump (STMT_COND (stmt), NULL, afterlabel);
	    expand_stmts (STMT_ELSE (stmt), loop_exit);
	  }
	else if (STMT_ELSE (stmt) == 0)
	  {
	    do_jump (STMT_COND (stmt), afterlabel, NULL);
	    expand_stmts (STMT_THEN (stmt), loop_exit);
	  }
	else
	  {
	    register rtx elselabel = gen_label_rtx ();

	    do_jump (STMT_COND (stmt), elselabel, NULL);
	    expand_stmts (STMT_THEN (stmt), loop_exit);
	    emit_jump (afterlabel);
	    emit_label (elselabel);
	    expand_stmts (STMT_ELSE (stmt), loop_exit);
	  }
	do_pending_stack_adjust ();
	emit_label (afterlabel);
      }
      break;

    case EXIT_STMT:
      /* Exit if the condition is false.  */
      do_jump (STMT_BODY (stmt), loop_exit, NULL);
      break;

    case RETURN_STMT:
      if (STMT_BODY (stmt))
	{
	  register rtx val = 0;
	  register rtx op0;
	  /* For tail-recursive call to current function,
	     just jump back to the beginning.
	     It's unsafe if any auto variable in this function
	     has its address taken; for simplicity,
	     require stack frame to be empty.  */
	  if (! cse_not_expected
	      && frame_offset == 0
	      && TREE_CODE (STMT_BODY (stmt)) == MODIFY_EXPR
	      && TREE_CODE (TREE_OPERAND (STMT_BODY (stmt), 1)) == CALL_EXPR
	      && TREE_CODE (TREE_OPERAND (TREE_OPERAND (STMT_BODY (stmt), 1), 0)) == ADDR_EXPR
	      && TREE_OPERAND (TREE_OPERAND (TREE_OPERAND (STMT_BODY (stmt), 1), 0), 0) == this_function
	      /* Finish checking validity, and if valid emit code
		 to set the argument variables for the new call.  */
	      && tail_recursion_args (TREE_OPERAND (TREE_OPERAND (STMT_BODY (stmt), 1), 1),
				      DECL_ARGUMENTS (this_function)))
	    {
	      ;
	      if (tail_recursion_label == 0)
		{
		  tail_recursion_label = gen_label_rtx ();
		  emit_label_after (tail_recursion_label,
				    tail_recursion_reentry);
		}
	      emit_jump (tail_recursion_label);
	      emit_barrier ();
	      break;
	    }
#ifndef FUNCTION_EPILOGUE
	  /* If this is  return x == y;  then generate
	     if (x == y) return 1; else return 0;
	     if we can do it with explicit return insns.  */
	  if (TREE_CODE (STMT_BODY (stmt)) == MODIFY_EXPR)
	    switch (TREE_CODE (TREE_OPERAND (STMT_BODY (stmt), 1)))
	      {
	      case EQ_EXPR:
	      case NE_EXPR:
	      case GT_EXPR:
	      case GE_EXPR:
	      case LT_EXPR:
	      case LE_EXPR:
	      case TRUTH_ANDIF_EXPR:
	      case TRUTH_ORIF_EXPR:
	      case TRUTH_NOT_EXPR:
		op0 = gen_label_rtx ();
		val = DECL_RTL (DECL_RESULT (this_function));
		jumpifnot (TREE_OPERAND (STMT_BODY (stmt), 1), op0);
		emit_move_insn (val, const1_rtx);
		emit_insn (gen_rtx (USE, VOIDmode, val));
		emit_jump_insn (gen_return ());
		emit_barrier ();
		emit_label (op0);
		emit_move_insn (val, const0_rtx);
		emit_insn (gen_rtx (USE, VOIDmode, val));
		emit_jump_insn (gen_return ());
		emit_barrier ();
	      }
	  if (val != 0)
	    break;
#endif
	  val = expand_expr (STMT_BODY (stmt), 0, VOIDmode, 0);
	  if (GET_CODE (val) == REG)
	    emit_insn (gen_rtx (USE, VOIDmode, val));
	  emit_queue ();
	}
      /* Return insn or function epilogue ignore the stack pointer.  */
      clear_pending_stack_adjust ();
#ifdef FUNCTION_EPILOGUE
      emit_jump (return_label);
#else
      emit_jump_insn (gen_return ());
#endif
      emit_barrier ();
      break;

    case LET_STMT:
      {
	rtx oldstack = 0;
	register tree decl;

	/* Make an entry on BLOCK_STACK for the block we are entering.  */

	thisblock.block = stmt;
	thisblock.next = block_stack;
	thisblock.stack_level = 0;
	block_stack = &thisblock;

	/* Output a NOTE to mark the beginning of the scope,
	   except when inhibited (for a function's outermost block).  */

	if (inhibit_block_beg)
	  inhibit_block_beg = 0;
	else
	  emit_note (0, NOTE_INSN_BLOCK_BEG);

	if (reg_birth_insn)
	  {
	    /* If doing stupid register allocation,
	       mark all register variables of this block
	       as beginning life here.  */

	    register rtx last_insn = get_last_insn ();

	    for (decl = STMT_VARS (stmt); decl; decl = TREE_CHAIN (decl))
	      {
		if (TREE_CODE (decl) == VAR_DECL
		    && DECL_RTL (decl) != 0
		    && GET_CODE (DECL_RTL (decl)) == REG)
		  reg_birth_insn[REGNO (DECL_RTL (decl))] = last_insn;
	      }
	  }

	/* Allocate space for all variable-size variables,
	   and set OLDSTACK nonzero if there are any.  */
	for (decl = STMT_VARS (stmt); decl; decl = TREE_CHAIN (decl))
	  if (TREE_CODE (decl) == VAR_DECL
	      && !TREE_LITERAL (DECL_SIZE (decl)))
	    {
	      rtx address, size;

	      if (oldstack == 0)
		{
		  do_pending_stack_adjust ();
		  oldstack = copy_to_reg (gen_rtx (REG, Pmode,
						   STACK_POINTER_REGNUM));
		  thisblock.stack_level = oldstack;
		}
	      size = expand_expr (DECL_SIZE (decl), 0, VOIDmode, 0);
#ifdef STACK_GROWS_DOWNWARD
	      anti_adjust_stack (size);
#endif
	      address = copy_to_reg (gen_rtx (REG, Pmode,
					      STACK_POINTER_REGNUM));
#ifndef STACK_GROWS_DOWNWARD
	      anti_adjust_stack (size);
#endif
	      DECL_RTL (decl) = gen_rtx (MEM, DECL_MODE (decl), address);
	    }

	/* Compute and store the initial values
	   of all nonstatic variables bound here.  */
	for (decl = STMT_VARS (stmt); decl; decl = TREE_CHAIN (decl))
	  if (TREE_CODE (decl) == VAR_DECL && DECL_INITIAL (decl)
	      && ! TREE_STATIC (decl))
	    {
	      if (DECL_VOFFSET (decl)
		  || !TREE_LITERAL (DECL_SIZE (decl)))
		abort ();
	      emit_note (DECL_SOURCE_FILE (decl), DECL_SOURCE_LINE (decl));
	      expand_assignment (decl, DECL_INITIAL (decl));
	    }

	/* Generate code for the body of the block.  */

	expand_stmts (STMT_BODY (stmt), 0);

	/* Mark the end of the scope.  */

	emit_note (0, NOTE_INSN_BLOCK_END);

	if (reg_death_insn)
	  {
	    /* If doing stupid register allocation,
	       mark all register variables of this block
	       as having just died.  */
	    register rtx last_insn = get_last_insn ();

	    for (decl = STMT_VARS (stmt); decl; decl = TREE_CHAIN (decl))
	      {
		if (TREE_CODE (decl) == VAR_DECL
		    && DECL_RTL (decl) != 0
		    && GET_CODE (DECL_RTL (decl)) == REG)
		  reg_death_insn[REGNO (DECL_RTL (decl))] = last_insn;
	      }
	  }

	/* Restore stack level in effect before the block
	   (only if variable-size objects allocated).  */

	if (oldstack != 0)
	  emit_move_insn (gen_rtx (REG, Pmode,
				   STACK_POINTER_REGNUM),
			  oldstack);

	/* Restore block_stack level for containing block.  */

	block_stack = thisblock.next;
      }
      break;

    case LOOP_STMT:
      {
	register rtx lab1, lab2;
	register tree x1 = tree_last (STMT_BODY (stmt));

	/* There are several ways to arrange the compilation of a loop.
	   We choose one depending on where the exits are and what kinds
	   of conditions they test.  */

	lab1 = gen_label_rtx ();
	lab2 = gen_label_rtx ();

	/* If the body ends with a conditional exit or goto,
	   just compile it straight through.  The conditional at the end
	   will combine with the branch back.  */
	if (TREE_CODE (x1) == EXIT_STMT
	    || (TREE_CODE (x1) == IF_STMT
		&& (TREE_CODE (STMT_THEN (x1)) == GOTO_STMT
		    || (STMT_ELSE (x1)
			&& TREE_CODE (STMT_ELSE (x1)) == GOTO_STMT))))
	  {
	    do_pending_stack_adjust ();
	    emit_note (0, NOTE_INSN_LOOP_BEG);
	    emit_label (lab1);
	    expand_stmts (STMT_BODY (stmt), lab2);
	    emit_jump (lab1);
	  }
#if 0
	/* If the loop starts with a conditional exit that tests
	   a very simple condition, duplicate the test, jumping around
	   the loop if we don't want to execute it even once.
	   Then put the test at the end of the loop.  */
	else if (! cse_not_expected
		 && TREE_CODE (STMT_BODY (stmt)) == EXIT_STMT
		 && stmt_complexity (stmt) < 15
		 && exit_simple_enough_p (STMT_BODY (STMT_BODY (stmt))))
	  {
	    do_jump (STMT_BODY (STMT_BODY (stmt)), lab2, 0);
	    emit_note (0, NOTE_INSN_LOOP_BEG);
	    emit_label (lab1);
	    expand_stmts (TREE_CHAIN (STMT_BODY (stmt)), lab2);
	    do_jump (STMT_BODY (STMT_BODY (stmt)), 0, lab1);
	  }
#endif
	/* If the loop starts with a conditional exit that tests
	   a very simple condition, put that exit at the end of the loop
	   and enter by jumping to that test.  */
	else if (! cse_not_expected
		 && TREE_CODE (STMT_BODY (stmt)) == EXIT_STMT)
	  {
	    register rtx lab3 = gen_label_rtx ();
	    do_pending_stack_adjust ();
	    emit_note (0, NOTE_INSN_LOOP_BEG);
	    emit_jump (lab3);
	    emit_label (lab1);
	    expand_stmts (TREE_CHAIN (STMT_BODY (stmt)), lab2);
	    do_pending_stack_adjust ();
	    emit_label (lab3);
	    do_jump (STMT_BODY (STMT_BODY (stmt)), 0, lab1);
	  }
	/* Neither starts nor ends with a conditional exit.  Strange.
	   Do it the simplest possible way.  */
	else
	  {
	    do_pending_stack_adjust ();
	    emit_note (0, NOTE_INSN_LOOP_BEG);
	    emit_label (lab1);
	    expand_stmts (STMT_BODY (stmt), lab2);
	    emit_jump (lab1);
	  }

	emit_note (0, NOTE_INSN_LOOP_END);
	emit_label (lab2);
      }
      break;

    case CASE_STMT:
      expand_case_stmt (stmt);
      break;

    default:
      abort ();
    }

  /* Perform any postincrements or postdecrements.  */

  emit_queue ();
}

/* Emit code to alter this function's formal parms for a tail-recursive call.
   ACTUALS is a list of actual parameter expressions (chain of TREE_LISTs).
   FORMALS is the chain of decls of formals.
   Return 1 if this can be done;
   otherwise return 0 and do not emit any code.  */

static int
tail_recursion_args (actuals, formals)
     tree actuals, formals;
{
  register tree a = actuals, f = formals;
  register int i;
  register rtx *argvec;

  /* Check that number and types of actuals are compatible
     with the formals.  This is not always true in valid C code.
     Also check that no formal needs to be addressable
     and that all formals are scalars.  */

  /* Also count the args.  */

  for (a = actuals, f = formals, i = 0; a && f; a = TREE_CHAIN (a), f = TREE_CHAIN (f), i++)
    {
      if (TREE_TYPE (TREE_VALUE (a)) != TREE_TYPE (f))
	return 0;
      if (GET_CODE (DECL_RTL (f)) != REG || DECL_MODE (f) == BLKmode)
	return 0;
    }
  if (a != 0 || f != 0)
    return 0;

  /* Compute all the actuals.  */

  argvec = (rtx *) alloca (i * sizeof (rtx));

  for (a = actuals, i = 0; a; a = TREE_CHAIN (a), i++)
    argvec[i] = expand_expr (TREE_VALUE (a), 0, VOIDmode, 0);

  /* Find which actual values refer to current values of previous formals.
     Copy each of them now, before any formal is changed.  */

  for (a = actuals, i = 0; a; a = TREE_CHAIN (a), i++)
    {
      int copy = 0;
      register int j;
      for (f = formals, j = 0; j < i; f = TREE_CHAIN (f), j++)
	if (reg_mentioned_p (DECL_RTL (f), argvec[i]))
	  { copy = 1; break; }
      if (copy)
	argvec[i] = copy_to_reg (argvec[i]);
    }

  /* Store the values of the actuals into the formals.  */

  for (f = formals, i = 0; f; f = TREE_CHAIN (f), i++)
    {
      if (DECL_MODE (f) == GET_MODE (argvec[i]))
	emit_move_insn (DECL_RTL (f), argvec[i]);
      else
	convert_move (DECL_RTL (f), argvec[i]);
    }

  return 1;
}

/* Generate code for a CASE_STMT node,
   which stands for a dispatch table.  */

static void
expand_case_stmt (stmt)
     tree stmt;
{
   tree minval, maxval, range;
   rtx default_label = 0;
   register tree elt;
   register tree c;
   int count;
   tree index_exp;
   rtx index;
   rtx table_label = gen_label_rtx ();
   int ncases;
   rtx *labelvec;
   register int i;

   /* Get upper and lower bounds of case values.  */
   count = 0;
   for (c = STMT_CASE_LIST (stmt); c; c = TREE_CHAIN (c))
     if (elt = TREE_PURPOSE (c))
       {
	 /* Note that in Pascal it will be possible
	    to have a RANGE_EXPR here as long as both
	    ends of the range are constant.
	    It will be necessary to extend this function
	    to handle them.  */
	 if (TREE_CODE (elt) != INTEGER_CST)
	   abort ();

	 if (count++ == 0)
	   {
	     minval = maxval = elt;
	   }
	 else
	   {
	     if (INT_CST_LT (elt, minval))
	       minval = elt;
	     if (INT_CST_LT (maxval, elt))
	       maxval = elt;
	   }
       }
     else
       default_label = label_rtx (TREE_VALUE (c));

   if (default_label == 0)
     abort ();

   /* Compute span of values.  */
   range = combine (MINUS_EXPR, maxval, minval);

   /* If range of values is much bigger than number of values,
      make a sequence of conditional branches instead of a dispatch.  */
   if (TREE_INT_CST_HIGH (range) != 0
#ifdef HAVE_casesi
       || count < 4
#else
       /* If machine does not have a case insn that compares the
	  bounds, this means extra overhead for dispatch tables
	  which raises the threshold for using them.  */
       || count < 7
#endif
       || TREE_INT_CST_LOW (range) > 10 * count)
     {
       index_exp = get_unwidened (STMT_CASE_INDEX (stmt), 0);
       index = expand_expr (index_exp, 0, VOIDmode, 0);
       emit_queue ();

       index = protect_from_queue (index, 0);
       if (GET_CODE (index) == MEM)
	 index = copy_to_reg (index);
       do_pending_stack_adjust ();

       for (c = STMT_CASE_LIST (stmt); c; c = TREE_CHAIN (c))
	 if ((elt = TREE_PURPOSE (c))
	     && int_fits_type_p (elt, TREE_TYPE (index_exp)))
	   do_jump_if_equal (expand_expr (elt, 0, VOIDmode, 0), index,
			     label_rtx (TREE_VALUE (c)));

       emit_jump (default_label);
       return;
     }

   index_exp = STMT_CASE_INDEX (stmt);

#ifdef HAVE_casesi
   if (TYPE_MODE (TREE_TYPE (index_exp)) == DImode)
     {
       index_exp = build2 (MINUS_EXPR, index_exp, minval);
       TREE_TYPE (index_exp) = TREE_TYPE (STMT_CASE_INDEX (stmt));
       index_exp = convert (integer_type_node, index_exp);
       minval = integer_zero_node;
     }
   else if (TYPE_MODE (TREE_TYPE (index_exp)) != SImode)
     index_exp = convert (integer_type_node, index_exp);
   index = expand_expr (index_exp, 0, VOIDmode, 0);
   emit_queue ();
   index = protect_from_queue (index, 0);
   do_pending_stack_adjust ();

   emit_jump_insn (gen_casesi (index, expand_expr (minval, 0, VOIDmode, 0),
			       expand_expr (range, 0, VOIDmode, 0),
			       table_label));
#else
#ifdef HAVE_tablejump
   index_exp = build2 (MINUS_EXPR, index_exp, minval);
   TREE_TYPE (index_exp) = TREE_TYPE (STMT_CASE_INDEX (stmt));
   index_exp = convert (integer_type_node, index_exp);
   index = expand_expr (index_exp, 0, VOIDmode, 0);
   emit_queue ();
   index = protect_from_queue (index, 0);
   do_pending_stack_adjust ();

   do_tablejump (index,
		 gen_rtx (CONST_INT, VOIDmode, TREE_INT_CST_LOW (range)),
		 table_label, default_label);
#else
   lossage;
#endif /* not HAVE_tablejump */
#endif /* not HAVE_casesi */

   /* Get table of labels to jump to, in order of case index.  */

   ncases = TREE_INT_CST_LOW (range) + 1;
   labelvec = (rtx *) alloca (ncases * sizeof (rtx));
   bzero (labelvec, ncases * sizeof (rtx));

   for (c = STMT_CASE_LIST (stmt); c; c = TREE_CHAIN (c))
     if (elt = TREE_PURPOSE (c))
       {
	 register int i = TREE_INT_CST_LOW (elt) - TREE_INT_CST_LOW (minval);
	 labelvec[i] = gen_rtx (LABEL_REF, Pmode, label_rtx (TREE_VALUE (c)));
       }

   /* Fill in the gaps with the default.  */
   for (i = 0; i < ncases; i++)
     if (labelvec[i] == 0)
       labelvec[i] = gen_rtx (LABEL_REF, Pmode, default_label);

   /* Output the table */
   emit_label (table_label);

#ifdef CASE_VECTOR_PC_RELATIVE
   emit_jump_insn (gen_rtx (ADDR_DIFF_VEC, CASE_VECTOR_MODE,
			    gen_rtx (LABEL_REF, Pmode, table_label),
			    gen_rtvec_v (ncases, labelvec)));
#else
   emit_jump_insn (gen_rtx (ADDR_VEC, CASE_VECTOR_MODE,
			    gen_rtvec_v (ncases, labelvec)));
#endif
   emit_jump (default_label);
}

/* Find all the variables declared within a function
   and give them rtl definitions.  */

/* Return size needed for stack frame based on slots so far allocated.  */

int
get_frame_size ()
{
  return frame_offset;
}

/* Allocate a stack slot of SIZE bytes and return a MEM rtx for it
   with machine mode MODE.  */

rtx
assign_stack_local (mode, size)
     enum machine_mode mode;
     int size;
{
  register rtx value;

  /* This function may not be used during rtl generation
     because at that time space is being allocated for
     structure values returned by function calls,
     but we don't know how big the space is until the end
     of rtl generation.  */
  if (max_structure_value_size > 0)
    abort ();

  /* Make each stack slot a multiple of the main allocation unit.  */
  size = (((size + (BIGGEST_ALIGNMENT / BITS_PER_UNIT) - 1)
	   / (BIGGEST_ALIGNMENT / BITS_PER_UNIT))
	  * (BIGGEST_ALIGNMENT / BITS_PER_UNIT));

#ifdef FRAME_GROWS_DOWNWARD
  frame_offset -= size;
#endif
  value = gen_rtx (MEM, mode,
		   gen_rtx (PLUS, Pmode,
			    gen_rtx (REG, SImode, FRAME_POINTER_REGNUM),
			    gen_rtx (CONST_INT, VOIDmode, frame_offset)));
#ifndef FRAME_GROWS_DOWNWARD
  frame_offset += size;
#endif

  return value;
}

/* 1 + last pseudo register number used for one of the user's variables
   (as opposed to compiler-generated temporaries).  */

int first_temp_reg_num;

static void assign_vars_1 ();

/* Assign stack slots or pseudo-registers to all the variables
   local to the body of a function being compiled (STMT).  */

static void
assign_all_vars (stmt)
     tree stmt;
{
  frame_offset = STARTING_FRAME_OFFSET;
  assign_vars_1 (stmt);
  first_temp_reg_num = max_reg_num ();
}

/* Assign stack slots or pseudo-registers to all the identifiers
   local within STMT, by recursive tree walk, except for variables
   of varying size.  */

static void
assign_vars_1 (stmt)
     register tree stmt;
{
  register tree decl;

  while (stmt)
    {
      switch (TREE_CODE (stmt))
	{
	case COMPOUND_STMT:
	case LOOP_STMT:
	  assign_vars_1 (STMT_BODY (stmt));
	  break;

	case IF_STMT:
	  assign_vars_1 (STMT_THEN (stmt));
	  assign_vars_1 (STMT_ELSE (stmt));
	  break;

	case LET_STMT:
	  for (decl = STMT_VARS (stmt); decl; decl = TREE_CHAIN (decl))
	    {
	      if (TREE_TYPE (decl) == error_mark_node)
		DECL_RTL (decl) = gen_rtx (MEM, BLKmode, const0_rtx);
	      else if (TREE_CODE (decl) == FUNCTION_DECL)
		/* External function */
		DECL_RTL (decl)
		  = gen_rtx (MEM, FUNCTION_MODE,
			     gen_rtx (SYMBOL_REF, Pmode,
				      IDENTIFIER_POINTER (DECL_NAME (decl))));
	      else if (TREE_CODE (decl) != VAR_DECL)
		;
	      else if (TREE_STATIC (decl) || TREE_EXTERNAL (decl))
		;   /* These were done by assemble_variable.  */
	      else if (DECL_MODE (decl) != BLKmode
		       && ! TREE_VOLATILE (decl)
		       && ! TREE_ADDRESSABLE (decl)
		       && (TREE_REGDECL (decl) || ! obey_regdecls))
		{
		  /* Variable that can go in a register.  */
		  DECL_RTL (decl) = gen_reg_rtx (DECL_MODE (decl));
		  if (TREE_CODE (TREE_TYPE (decl)) == POINTER_TYPE)
		    mark_reg_pointer (DECL_RTL (decl));
		}
	      else if (TREE_LITERAL (DECL_SIZE (decl)))
		/* Variable of fixed size that goes on the stack.  */
		DECL_RTL (decl)
		  = assign_stack_local (DECL_MODE (decl),
					(TREE_INT_CST_LOW (DECL_SIZE (decl))
					 * DECL_SIZE_UNIT (decl)
					 + BITS_PER_UNIT - 1)
					/ BITS_PER_UNIT);
	      /* Rtl for a dynamic-size object is set up when
		 the storage for the object is pushed.  */

	    }
	  assign_vars_1 (STMT_BODY (stmt));
	}
      stmt = TREE_CHAIN (stmt);
    }
}

/* 1 + last pseudo register number used for loading a copy
   of a parameter of this function.  */

static int max_parm_reg;

/* Assign RTL expressions to the function's parameters.
   This may involve copying them into registers and using
   those registers as the RTL for them.  */

static void
assign_parms (fndecl)
     tree fndecl;
{
  register tree parm;
  register rtx parmloc;
  register int i;

  for (parm = DECL_ARGUMENTS (fndecl), i = 0; parm; parm = TREE_CHAIN (parm), i++)
    {
      if (DECL_VOFFSET (parm))
	abort ();
      if (TREE_TYPE (parm) == error_mark_node)
	parmloc = gen_rtx (MEM, BLKmode, const0_rtx);
      else
	parmloc
	  = gen_rtx (MEM, TYPE_MODE (DECL_ARG_TYPE (parm)),
		     gen_rtx (PLUS, SImode,
			      gen_rtx (REG, SImode, ARG_POINTER_REGNUM),
			      gen_rtx (CONST_INT, VOIDmode,
				       DECL_OFFSET (parm) / BITS_PER_UNIT)));

      /* PARMLOC now refers to the parameter in the arglist
	 in the form in which it is passed.
	 Now output code if necessary to convert it to
	 the type in which this function declares it,
	 and store a reference to that value in DECL_RTL.
	 This reference may be the same as PARMLOC
	 if no conversion is required.  */

      if (GET_MODE (parmloc) == BLKmode)
	DECL_RTL (parm) = parmloc;
      else if (! (TREE_ADDRESSABLE (parm)
		  || (obey_regdecls && ! TREE_REGDECL (parm))))
	{
	  /* Store the parm in a register during the function.  */
	  register rtx parmreg = gen_reg_rtx (TYPE_MODE (TREE_TYPE (parm)));

	  DECL_RTL (parm) = parmreg;

	  /* Copy the value into the register.  */
	  if (GET_MODE (parmreg) != GET_MODE (parmloc))
	    convert_move (parmreg, parmloc, 0);
	  else
	    emit_move_insn (parmreg, parmloc);

	  /* Mark the register as eliminable if we did no conversion.  */
	  if (GET_MODE (parmreg) == GET_MODE (parmloc))
	    REG_NOTES (get_last_insn ()) = gen_rtx (EXPR_LIST, REG_CONST,
						    parmreg, 0);

	  /* For pointer data type, suggest pointer register.  */
	  if (TREE_CODE (TREE_TYPE (parm)) == POINTER_TYPE)
	    mark_reg_pointer (parmreg);
	}
      else if (GET_MODE (parmloc) != TYPE_MODE (TREE_TYPE (parm)))
	{
	  /* Don't store in a register, but conversion is required.
	     Convert it via a register and store back in the parm list
	     in the new format.  The debugger will expect this anyway.  */

	  register rtx parmlcl
	    = gen_rtx (MEM, TYPE_MODE (TREE_TYPE (parm)),
		       copy_rtx (XEXP (parmloc, 0)));
	  register rtx parmreg = gen_reg_rtx (TYPE_MODE (TREE_TYPE (parm)));

	  convert_move (parmreg, parmloc, 0);
	  emit_move_insn (parmlcl, parmreg);
	  DECL_RTL (parm) = parmlcl;
	}
      else
	DECL_RTL (parm) = parmloc;
    }
  max_parm_reg = max_reg_num ();
}

/* Allocation of space for returned structure values.
   During the rtl generation pass, `get_structure_value_addr'
   is called from time to time to request the address of a block in our
   stack frame in which called functions will store the structures
   they are returning.  The same space is used for all of these blocks.  

   `get_structure_value_addr' records the maximum block size needed.

   At the end of generation `allocate_structure_value_space' is
   called to adjust `frame_offset' so that the needed space is allocated.  */

rtx
get_structure_value_addr (sizex)
     rtx sizex;
{
  register int size;
  if (GET_CODE (sizex) != CONST_INT)
    abort ();
  size = INTVAL (sizex);

  /* Round up to a multiple of the main allocation unit.  */
  size = (((size + (BIGGEST_ALIGNMENT / BITS_PER_UNIT) - 1)
	   / (BIGGEST_ALIGNMENT / BITS_PER_UNIT))
	  * (BIGGEST_ALIGNMENT / BITS_PER_UNIT));

  if (size > max_structure_value_size)
    {
      max_structure_value_size = size;
    }
#ifdef FRAME_GROWS_DOWNWARD
  return gen_rtx (PLUS, Pmode,
		  gen_rtx (REG, SImode, FRAME_POINTER_REGNUM),
		  gen_rtx (CONST_INT, VOIDmode, frame_offset - size));
#else  
  return gen_rtx (PLUS, Pmode,
		  gen_rtx (REG, SImode, FRAME_POINTER_REGNUM),
		  gen_rtx (CONST_INT, VOIDmode, frame_offset));
#endif
}

static void
allocate_structure_value_space ()
{
#ifdef FRAME_GROWS_DOWNWARD
  frame_offset -= max_structure_value_size;
#else
  frame_offset += max_structure_value_size;
#endif
  /* Allow `assign_stack_local' to be used once again.  */
  max_structure_value_size = 0;
}

/* Main entry point: generate the rtl code for a function SUBR
   represented as a tree.  Returns the first insn.

   NO_CSE is 1 if cse is not going to be done;
   this is passed because when cse is to be done it is sometimes
   desirable to generate excess temporaries at this stage to give
   cse an opportunity to go to work.  */

rtx
expand_function (subr, no_cse)
     tree subr;
     int no_cse;
{
  register int i;

  this_function = subr;
  cse_not_expected = no_cse;

  init_queue ();

#ifdef FUNCTION_EPILOGUE
  return_label = gen_label_rtx ();
#endif

  max_structure_value_size = 0;

  /* We are not currently within any block.  */
  block_stack = 0;
  tail_recursion_label = 0;

  clear_pending_stack_adjust ();
  clear_current_args_size ();

  /* Prevent ever trying to delete the first instruction of a function.
     Also tell final how to output a linenum before the function prologue.  */
  emit_note (DECL_SOURCE_FILE (subr), DECL_SOURCE_LINE (subr));
  /* Make sure first insn is a note even if we don't want linenums.
     This makes sure the first insn will never be deleted.
     Also, final expects a note to appear there.  */
  emit_note (0, NOTE_INSN_DELETED);

  /* Initialize rtx for parameters and local variables.
     In some cases this requires emitting insns.  */

  assign_parms (subr);
  /* After the parm initializations is where the tail-recursion label
     should go, if we end up needing one.  */
  tail_recursion_reentry = get_last_insn ();

  assign_all_vars (DECL_INITIAL (subr));

  /* Initialize rtx used to return the value.  */

  if (DECL_MODE (DECL_RESULT (subr)) == BLKmode)
    {
      /* Returning something that won't go in a register.  */
      register rtx value_address;

      /* Expect to be passed the address of a place to store the value,
	 in the same register that is normally used to return values.  */
      value_address = gen_reg_rtx (Pmode);
      emit_move_insn (value_address,
		      gen_rtx (REG, Pmode, STRUCT_VALUE_REGNUM));
      DECL_RTL (DECL_RESULT (subr))
	= gen_rtx (MEM, DECL_MODE (DECL_RESULT (subr)),
		   value_address);
    }
  else
    DECL_RTL (DECL_RESULT (subr))
      = gen_rtx (REG, DECL_MODE (DECL_RESULT (subr)),
		 FUNCTION_VALUE_REGNUM);

  /* If doing stupid allocation, mark parms as born here.  */

  if (obey_regdecls)
    {
      rtx insn = get_last_insn ();

      reg_birth_insn = (rtx *) oballoc (first_temp_reg_num * sizeof (rtx));
      reg_death_insn = (rtx *) oballoc (first_temp_reg_num * sizeof (rtx));
      bzero (reg_birth_insn, first_temp_reg_num * sizeof (rtx));
      bzero (reg_death_insn, first_temp_reg_num * sizeof (rtx));

      for (i = 0; i < max_parm_reg; i++)
	reg_birth_insn[i] = insn;
    }

  /* Don't generate a NOTE_INSN_BLOCK_BEG for the function's topmost block.
     final will do it specially, in order to make it come before
     the function prologue, and we don't want to have two of them.  */
  inhibit_block_beg = 1;

  /* Generate the actual code for the function.
     `assign_stack_local' may not be called again
     until after `allocate_structure_value_space'.  */

  expand_stmt (DECL_INITIAL (subr), 0);

  /* If doing stupid register allocation,
     mark any argument variables as dying here in the last insn generated
     (which is always an end-of-block comment, so it is never deleted).  */
  if (obey_regdecls)
    {
      rtx insn = get_last_insn ();
      for (i = 0; i < max_parm_reg; i++)
	reg_death_insn[i] = insn;
    }

  /* Return insn or function epilogue ignore the stack pointer.  */
  clear_pending_stack_adjust ();

/* If we require a true epilogue,
   put here the label that return statements jump to.
   If there will be no epilogue, write a return instruction.  */
#ifdef FUNCTION_EPILOGUE
  emit_label (return_label);
#else
  emit_jump_insn (gen_return ());
#endif

  allocate_structure_value_space ();

  return get_insns ();
}
