/* Convert tree expression to rtl instructions, for GNU compiler.
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
#include "tree.h"
#include "insn-flags.h"
#include "insn-codes.h"
#include "expr.h"

/* If this is nonzero, we do not bother generating VOLATILE
   around volatile memory references, and we are willing to
   output indirect addresses.  If cse is to follow, we reject
   indirect addresses so a useful potential cse is generated;
   if it is used only once, instruction combination will produce
   the same indirect address eventually.  */
int cse_not_expected;

/* Nonzero to generate code for all the subroutines within an
   expression before generating the upper levels of the expression.
   Nowadays this is never zero.  */
int do_preexpand_calls = 1;

/* Number of units that we should eventually pop off the stack.
   These are the arguments to function calls that have already returned.  */
int pending_stack_adjust;

/* Total size of arguments already pushed for function calls that
   have not happened yet.  Also counts 1 for each level of conditional
   expression that we are inside.  When this is nonzero,
   args passed to function calls must be popped right away
   to ensure contiguity of argument lists for future calls.  */
int current_args_size;

static rtx store_expr ();
static rtx expand_call ();
static void gen_call_1 ();
static rtx compare ();
static rtx compare1 ();
static rtx do_store_flag ();
static void preexpand_calls ();

/* MOVE_RATIO is the number of move instructions that is better than
   a block move.  */

#if defined (HAVE_movstrhi) || defined (HAVE_movstrsi)
#define MOVE_RATIO 2
#else
#define MOVE_RATIO 6
#endif

/* Table indexed by tree code giving 1 if the code is for a
   comparison operation, or anything that is most easily
   computed with a conditional branch.

   We include tree.def to give it the proper length.
   The contents thus created are irrelevant.
   The real contents are initialized in init_comparisons.  */

#define DEFTREECODE(SYM, NAME, TYPE, LENGTH) LENGTH,

static char comparison_code[] = {
#include "tree.def"
};
#undef DEFTREECODE

init_comparisons ()
{

  bzero (comparison_code, sizeof comparison_code);
  comparison_code[(int) EQ_EXPR] = 1;
  comparison_code[(int) NE_EXPR] = 1;
  comparison_code[(int) LT_EXPR] = 1;
  comparison_code[(int) GT_EXPR] = 1;
  comparison_code[(int) LE_EXPR] = 1;
  comparison_code[(int) GE_EXPR] = 1;
}

/* Manage the queue of increment instructions to be output
   for POSTINCREMENT_EXPR expressions, etc.  */

static rtx pending_chain;

/* Queue up to increment (or change) VAR later.  BODY says how:
   BODY should be the same thing you would pass to emit_insn
   to increment right away.  It will go to emit_insn later on.

   The value is a QUEUED expression to be used in place of VAR
   where you want to guarantee the pre-incrementation value of VAR.

   When constructing BODY, you should pass VAR through copy_rtx
   each time it is used.  If VAR is a MEM, this prevents BODY from
   sharing structure incorrectly with itself or with places that
   explicitly use VAR.  */

static rtx
enqueue_insn (var, body)
     rtx var, body;
{
  pending_chain = gen_rtx (QUEUED, GET_MODE (var),
			   var, 0, 0, body, pending_chain);
  return pending_chain;
}

/* Use protect_from_queue to convert a QUEUED expression
   into something that you can put immediately into an instruction.
   If the queued incrementation has not happened yet,
   protect_from_queue returns the variable itself.
   If the incrementation has happened, protect_from_queue returns a temp
   that contains a copy of the old value of the variable.

   Any time an rtx which might possibly be a QUEUED is to be put
   into an instruction, it must be passed through protect_from_queue first.
   QUEUED expressions are not meaningful in instructions.

   Do not pass a value through protect_from_queue and then hold
   on to it for a while before putting it in an instruction!
   If the queue is flushed in between, incorrect code will result.  */

rtx
protect_from_queue (x, modify)
     register rtx x;
     int modify;
{
  register RTX_CODE code = GET_CODE (x);
  if (code != QUEUED)
    {
      /* A special hack for read access to (MEM (QUEUED ...))
	 to facilitate use of autoincrement.
	 Make a copy of the contents of the memory location
	 rather than a copy of the address.  */
      if (code == MEM && GET_CODE (XEXP (x, 0)) == QUEUED && !modify)
	{
	  register rtx y = XEXP (x, 0);
	  XEXP (x, 0) = QUEUED_VAR (y);
	  if (QUEUED_INSN (y))
	    {
	      register rtx temp = gen_reg_rtx (GET_MODE (x));
	      emit_insn_before (gen_move_insn (temp, x),
				QUEUED_INSN (y));
	      return temp;
	    }
	  return x;
	}
      /* Otherwise, recursively protect the subexpressions of all
	 the kinds of rtx's that can contain a QUEUED.  */
      if (code == MEM)
	XEXP (x, 0) = protect_from_queue (XEXP (x, 0), 0);
      else if (code == PLUS || code == MULT)
	{
	  XEXP (x, 0) = protect_from_queue (XEXP (x, 0), 0);
	  XEXP (x, 1) = protect_from_queue (XEXP (x, 1), 0);
	}
      return x;
    }
  /* If the increment has not happened, use the variable itself.  */
  if (QUEUED_INSN (x) == 0)
    return QUEUED_VAR (x);
  /* If the increment has happened and a pre-increment copy exists,
     use that copy.  */
  if (QUEUED_COPY (x) != 0)
    return QUEUED_COPY (x);
  /* The increment has happened but we haven't set up a pre-increment copy.
     Set one up now, and use it.  */
  QUEUED_COPY (x) = gen_reg_rtx (GET_MODE (QUEUED_VAR (x)));
  emit_insn_before (gen_move_insn (QUEUED_COPY (x), QUEUED_VAR (x)),
		    QUEUED_INSN (x));
  return QUEUED_COPY (x);
}

/* perform all the pending incrementations.  */

void
emit_queue ()
{
  register rtx p;
  while (p = pending_chain)
    {
      QUEUED_INSN (p) = emit_insn (QUEUED_BODY (p));
      pending_chain = QUEUED_NEXT (p);
    }
}

void
init_queue ()
{
  if (pending_chain)
    abort ();
}

/* Copy data from FROM to TO, where the machine modes are not the same.
   Both modes may be integer, or both may be floating.
   UNSIGNEDP should be nonzero if FROM is an unsigned type.
   This causes zero-extension instead of sign-extension.  */

void
convert_move (to, from, unsignedp)
     register rtx to, from;
     int unsignedp;
{
  enum machine_mode to_mode = GET_MODE (to);
  enum machine_mode from_mode = GET_MODE (from);
  int to_real = to_mode == SFmode || to_mode == DFmode;
  int from_real = from_mode == SFmode || from_mode == DFmode;
  int extending = (int) to_mode > (int) from_mode;

  to = protect_from_queue (to, 1);
  from = protect_from_queue (from, 0);

  if (to_real != from_real)
    abort ();

  if (to_mode == from_mode || GET_CODE (from) == CONST_INT)
    {
      emit_move_insn (to, from);
      return;
    }

  if (to_real)
    {
#ifdef HAVE_extendsfdf2
      if (HAVE_extendsfdf2 && extending)
	{
	  emit_insn (gen_extendsfdf2 (to, from));
	  return;
	}
#endif
#ifdef HAVE_truncdfsf2
      if (HAVE_truncdfsf2 && ! extending)
	{
	  emit_insn (gen_truncdfsf2 (to, from));
	  return;
	}
#endif
      emit_library_call (gen_rtx (SYMBOL_REF, Pmode, (extending
						      ? "extendsfdf2"
						      : "truncdfsf2")),
			 1, from,  (extending ? SFmode : DFmode));
      copy_function_value (to);
      return;
    }

  if (to_mode == DImode)
    {
      emit_insn (gen_rtx (CLOBBER, VOIDmode, to));

      if (unsignedp)
	{
	  convert_move (gen_lowpart (SImode, to), from, unsignedp);
	  emit_clr_insn (gen_highpart (SImode, to));
	}
#ifdef HAVE_sltsi
      else if (HAVE_sltsi)
	{
	  convert_move (gen_lowpart (SImode, to), from, unsignedp);
	  emit_insn (gen_sltsi (gen_highpart (SImode, to)));
	}
#endif
      else
	{
	  register rtx label = gen_label_rtx ();

	  emit_clr_insn (gen_highpart (SImode, to));
	  convert_move (gen_lowpart (SImode, to), from, unsignedp);
	  emit_cmp_insn (gen_lowpart (SImode, to),
			 gen_rtx (CONST_INT, VOIDmode, 0),
			 0, 0);
	  emit_jump_insn (gen_bge (label));
	  expand_unop (SImode, one_cmpl_optab,
		       gen_highpart (SImode, to), gen_highpart (SImode, to),
		       1);
	  emit_label (label);
	}
      return;
    }

  if (from_mode == DImode)
    {
      convert_move (to, gen_lowpart (SImode, from), 0);
      return;
    }

  /* Now follow all the conversions between integers
     no more than a word long.  */

  if (to_mode == SImode && from_mode == HImode)
    {
      if (unsignedp)
	{
#ifdef HAVE_zero_extendhisi2
	  if (HAVE_zero_extendhisi2)
	    emit_insn (gen_zero_extendhisi2 (to, from));
	  else
#endif
	    abort ();
	}
      else
	{
#ifdef HAVE_extendhisi2
	  if (HAVE_extendhisi2)
	    emit_insn (gen_extendhisi2 (to, from));
	  else
#endif
	    abort ();
	}
      return;
    }

  if (to_mode == SImode && from_mode == QImode)
    {
      if (unsignedp)
	{
#ifdef HAVE_zero_extendqisi2
	  if (HAVE_zero_extendqisi2)
	    {
	      emit_insn (gen_zero_extendqisi2 (to, from));
	      return;
	    }
#endif
#if defined (HAVE_zero_extendqihi2) && defined (HAVE_extendhisi2)
	  if (HAVE_zero_extendqihi2 && HAVE_extendhisi2)
	    {
	      register rtx temp = gen_reg_rtx (HImode);
	      emit_insn (gen_zero_extendqihi2 (temp, from));
	      emit_insn (gen_extendhisi2 (to, temp));
	      return;
	    }
#endif
	}
      else
	{
#ifdef HAVE_extendqisi2
	  if (HAVE_extendqisi2)
	    {
	      emit_insn (gen_extendqisi2 (to, from));
	      return;
	    }
#endif
#if defined (HAVE_extendqihi2) && defined (HAVE_extendhisi2)
	  if (HAVE_extendqihi2 && HAVE_extendhisi2)
	    {
	      register rtx temp = gen_reg_rtx (HImode);
	      emit_insn (gen_extendqihi2 (temp, from));
	      emit_insn (gen_extendhisi2 (to, temp));
	      return;
	    }
#endif
	}
      abort ();
    }

  if (to_mode == HImode && from_mode == QImode)
    {
      if (unsignedp)
	{
#ifdef HAVE_zero_extendqihi2
	  if (HAVE_zero_extendqihi2)
	    {
	      emit_insn (gen_zero_extendqihi2 (to, from));
	      return;
	    }
#endif
	}
      else
	{
#ifdef HAVE_extendqihi2
	  if (HAVE_extendqihi2)
	    {
	      emit_insn (gen_extendqihi2 (to, from));
	      return;
	    }
#endif
	}
      abort ();
    }

  /* Now we are truncating an integer to a smaller one.
     If the result is a temporary, we might as well just copy it,
     since only the low-order part of the result needs to be valid
     and it is valid with no change.  */

  if (GET_CODE (to) == REG)
    {
      if (GET_CODE (from) == REG)
	{
	  emit_move_insn (to, gen_lowpart (GET_MODE (to), from));
	  return;
	}
#ifndef BYTES_BIG_ENDIAN
      else if (GET_CODE (from) == MEM)
	{
	  register rtx addr = XEXP (from, 0);
	  GO_IF_LEGITIMATE_ADDRESS (GET_MODE (to), addr, win);
	  if (0)
	    {
	    win:
	      emit_move_insn (to, gen_rtx (MEM, GET_MODE (to), addr));
	      return;
	    }
	}
#endif /* not BYTES_BIG_ENDIAN */
    }

  if (from_mode == SImode && to_mode == HImode)
    {
#ifdef HAVE_truncsihi2
      if (HAVE_truncsihi2)
	{
	  emit_insn (gen_truncsihi2 (to, from));
	  return;
	}
#endif
      abort ();
    }

  if (from_mode == SImode && to_mode == QImode)
    {
#ifdef HAVE_truncsiqi2
      if (HAVE_truncsiqi2)
	{
	  emit_insn (gen_truncsiqi2 (to, from));
	  return;
	}
#endif
      abort ();
    }

  if (from_mode == HImode && to_mode == QImode)
    {
#ifdef HAVE_trunchiqi2
      if (HAVE_trunchiqi2)
	{
	  emit_insn (gen_trunchiqi2 (to, from));
	  return;
	}
#endif
      abort ();
    }
}

/* Return an rtx for a value that would result
   from converting X to mode MODE.
   Both X and MODE may be floating, or both integer.
   UNSIGNEDP is nonzero if X is an unsigned value.
   This can be done by referring to a part of X in place
   or by copying to a new temporary with conversion.  */

rtx
convert_to_mode (mode, x, unsignedp)
     enum machine_mode mode;
     rtx x;
     int unsignedp;
{
  register rtx temp;
  if (mode == GET_MODE (x))
    return x;
  if (GET_MODE_SIZE (mode) <= GET_MODE_SIZE (GET_MODE (x)))
    return gen_lowpart (mode, x);
  temp = gen_reg_rtx (mode);
  convert_move (temp, x, unsignedp);
  return temp;
}

/* Generate several move instructions to copy LEN bytes
   from address FROM to address TO.  The caller must pass FROM and TO
    through protect_from_queue before calling.
   FROM_VOL and TO_VOL are nonzero if references to
    FROM and TO, respectively, should be marked VOLATILE.
   ALIGN (in bytes) is maximum alignment we can assume.  */

struct move_by_pieces
{
  rtx to;
  int autinc_to;
  int explicit_inc_to;
  int to_vol;
  rtx from;
  int autinc_from;
  int explicit_inc_from;
  int from_vol;
  int len;
  int offset;
  int reverse;
};

static void
move_by_pieces (to, from, len, align, to_vol, from_vol)
     rtx to, from;
     int len, align;
     int to_vol, from_vol;
{
  struct move_by_pieces data;

  data.offset = 0;
  data.to = to;
  data.from = from;
  data.to_vol = to_vol;
  data.from_vol = from_vol;
  data.autinc_to = (GET_CODE (to) == PRE_INC || GET_CODE (to) == PRE_DEC
		    || GET_CODE (to) == POST_INC || GET_CODE (to) == POST_DEC);
  data.autinc_from = (GET_CODE (from) == PRE_INC || GET_CODE (from) == PRE_DEC
		      || GET_CODE (from) == POST_INC
		      || GET_CODE (from) == POST_DEC);

  data.explicit_inc_from = 0;
  data.explicit_inc_to = 0;
  data.reverse = (GET_CODE (to) == PRE_DEC || GET_CODE (to) == POST_DEC);
  if (data.reverse) data.offset = len;
  data.len = len;

  /* If copying requires more than two move insns,
     copy addresses to registers (to make displacements shorter)
     and use post-increment if available.  */
  if (!(data.autinc_from && data.autinc_to)
      && move_by_pieces_ninsns (len, align) > 2)
    {
#ifdef HAVE_PRE_DECREMENT
      if (data.reverse && ! data.autinc_from)
	{
	  data.from = copy_to_reg (plus_constant (from, len));
	  data.autinc_from = 1;
	  data.explicit_inc_from = -1;
	}
#endif
#ifdef HAVE_POST_INCREMENT
      if (! data.autinc_from)
	{
	  data.from = copy_to_reg (from);
	  data.autinc_from = 1;
	  data.explicit_inc_from = 1;
	}
#endif
      if (!data.autinc_from && CONSTANT_ADDRESS_P (from))
	data.from = copy_to_reg (from);
#ifdef HAVE_PRE_DECREMENT
      if (data.reverse && ! data.autinc_to)
	{
	  data.to = copy_to_reg (plus_constant (to, len));
	  data.autinc_to = 1;
	  data.explicit_inc_to = -1;
	}
#endif
#ifdef HAVE_POST_INCREMENT
      if (! data.reverse && ! data.autinc_to)
	{
	  data.to = copy_to_reg (to);
	  data.autinc_to = 1;
	  data.explicit_inc_to = 1;
	}
#endif
      if (!data.autinc_to && CONSTANT_ADDRESS_P (to))
	data.to = copy_to_reg (to);
    }

#ifdef STRICT_ALIGNMENT
  if (align > MOVE_MAX)
    align = MOVE_MAX;
#else
  align = MOVE_MAX;
#endif

#ifdef HAVE_movti
  if (HAVE_movti && align >= GET_MODE_SIZE (TImode))
    move_by_pieces_1 (gen_movti, TImode, &data);
#endif
#ifdef HAVE_movdi
  if (HAVE_movdi && align >= GET_MODE_SIZE (DImode))
    move_by_pieces_1 (gen_movdi, DImode, &data);
#endif
  if (align >= GET_MODE_SIZE (SImode))
    move_by_pieces_1 (gen_movsi, SImode, &data);
  if (align >= GET_MODE_SIZE (HImode))
    move_by_pieces_1 (gen_movhi, HImode, &data);
  move_by_pieces_1 (gen_movqi, QImode, &data);
}

/* Return number of insns required to move L bytes by pieces.
   ALIGN (in bytes) is maximum alignment we can assume.  */

int
move_by_pieces_ninsns (l, align)
     unsigned int l;
     int align;
{
  register int n_insns = 0;

#ifdef STRICT_ALIGNMENT
  if (align > MOVE_MAX)
    align = MOVE_MAX;
#else
  align = MOVE_MAX;
#endif

#ifdef HAVE_movti
  if (HAVE_movti && align >= GET_MODE_SIZE (TImode))
    n_insns += l / GET_MODE_SIZE (TImode), l %= GET_MODE_SIZE (TImode);
#endif
#ifdef HAVE_movdi
  if (HAVE_movdi && align >= GET_MODE_SIZE (DImode))
    n_insns += l / GET_MODE_SIZE (DImode), l %= GET_MODE_SIZE (DImode);
#endif
  if (HAVE_movsi && align >= GET_MODE_SIZE (SImode))
    n_insns += l / GET_MODE_SIZE (SImode), l %= GET_MODE_SIZE (SImode);
  if (HAVE_movhi && align >= GET_MODE_SIZE (HImode))
    n_insns += l / GET_MODE_SIZE (HImode), l %= GET_MODE_SIZE (HImode);
  n_insns += l;

  return n_insns;
}

/* Subroutine of move_by_pieces.  Move as many bytes as appropriate
   with move instructions for mode MODE.  GENFUN is the gen_... function
   to make a move insn for that mode.  DATA has all the other info.  */

move_by_pieces_1 (genfun, mode, data)
     rtx (*genfun) ();
     enum machine_mode mode;
     struct move_by_pieces *data;
{
  register int size = GET_MODE_SIZE (mode);
  register rtx to1, from1;

#define add_offset(FLAG,X) (FLAG ? (X) : plus_constant (X, data->offset))

  while (data->len >= size)
    {
      to1 = gen_rtx (MEM, mode, add_offset (data->autinc_to, data->to));
      from1 = gen_rtx (MEM, mode, add_offset (data->autinc_from, data->from));

      if (data->to_vol) to1 = gen_rtx (VOLATILE, mode, to1);
      if (data->from_vol) from1 = gen_rtx (VOLATILE, mode, from1);

      if (data->reverse) data->offset -= size;
#ifdef HAVE_PRE_DECREMENT
      if (data->explicit_inc_to < 0)
	emit_insn (gen_sub2_insn (data->to,
				  gen_rtx (CONST_INT, VOIDmode, size)));
      if (data->explicit_inc_from < 0)
	emit_insn (gen_sub2_insn (data->from,
				  gen_rtx (CONST_INT, VOIDmode, size)));
#endif

      emit_insn (genfun (to1, from1));
#ifdef HAVE_POST_INCREMENT
      if (data->explicit_inc_to > 0)
	emit_insn (gen_add2_insn (data->to,
				  gen_rtx (CONST_INT, VOIDmode, size)));
      if (data->explicit_inc_from > 0)
	emit_insn (gen_add2_insn (data->from,
				  gen_rtx (CONST_INT, VOIDmode, size)));
#endif

      if (! data->reverse) data->offset += size;
      data->len -= size;
    }
}

/* Emit code to move a block Y to a block X.
   This may be done with string-move instructions,
   with multiple scalar move instructions, or with a library call.

   Both X and Y must be MEM rtx's (perhaps inside VOLATILE)
   with mode BLKmode.
   SIZE is an rtx that says how long they are.
   ALIGN is the maximum alignment we can assume they have,
   measured in bytes.  */

static void
emit_block_move (x, y, size, align)
     rtx x, y;
     rtx size;
     int align;
{

  register int max_step;
  rtx xinner, yinner;
  int xvolatile = 0, yvolatile = 0;

  if (GET_MODE (x) != BLKmode)
    abort ();

  if (GET_MODE (y) != BLKmode)
    abort ();

  x = protect_from_queue (x, 1);
  y = protect_from_queue (y, 0);

  xinner = x, yinner = y;

  if (GET_CODE (x) == VOLATILE)
    xvolatile = 1, xinner = XEXP (x, 0);
  if (GET_CODE (y) == VOLATILE)
    yvolatile = 1, yinner = XEXP (y, 0);

  if (GET_CODE (xinner) != MEM)
    abort ();
  if (GET_CODE (yinner) != MEM)
    abort ();
  if (size == 0)
    abort ();

  if (GET_CODE (size) == CONST_INT
      && (move_by_pieces_ninsns ((unsigned) INTVAL (size), align)
	  < MOVE_RATIO))
    move_by_pieces (XEXP (xinner, 0), XEXP (yinner, 0),
		    INTVAL (size), align,
		    xvolatile, yvolatile);
  else
    {
#ifdef HAVE_movstrsi
      if (HAVE_movstrsi)
	{
	  emit_insn (gen_movstrsi (x, y, size));
	  return;
	}
#endif
#ifdef HAVE_movstrhi
      if (HAVE_movstrhi
	  && GET_CODE (size) == CONST_INT
	  && ((unsigned) INTVAL (size)
	      < (1 << (GET_MODE_SIZE (HImode) * BITS_PER_UNIT - 1))))
	{
	  emit_insn (gen_movstrhi (x, y, size));
	  return;
	}
#endif
      emit_library_call (gen_rtx (SYMBOL_REF, Pmode, "bcopy"),
			 3, XEXP (yinner, 0), Pmode,
			 XEXP (xinner, 0), Pmode,
			 size, Pmode);
    }
}

/* Generate code to copy Y into X.
   Both Y and X must have the same mode, except that
   Y can be a constant with VOIDmode.
   This mode cannot be BLKmode; use emit_block_move for that.  */

emit_move_insn (x, y)
     rtx x, y;
{
  enum machine_mode mode = GET_MODE (x);
  x = protect_from_queue (x, 1);
  y = protect_from_queue (y, 0);

  if (mode == BLKmode)
    abort ();
  if (mov_optab[(int) mode].insn_code != CODE_FOR_nothing)
    emit_insn (GEN_FCN (mov_optab[(int) mode].insn_code) (x, y));
  else if (GET_MODE_SIZE (mode) >= GET_MODE_SIZE (SImode))
    {
      register int count = GET_MODE_SIZE (mode) / GET_MODE_SIZE (SImode);
      register int i;
      for (i = 0; i < count; i++)
	{
	  rtx x1, y1;
	  if (GET_CODE (x) == REG)
	    x1 = gen_rtx (SUBREG, SImode, x, i);
	  else
	    x1 = gen_rtx (MEM, SImode,
			  memory_address (SImode,
					  plus_constant (XEXP (x, 0),
							 i * GET_MODE_SIZE (SImode))));
	  if (GET_CODE (y) == REG)
	    y1 = gen_rtx (SUBREG, SImode, y, i);
	  else
	    y1 = gen_rtx (MEM, SImode,
			  memory_address (SImode,
					  plus_constant (XEXP (y, 0),
							 i * GET_MODE_SIZE (SImode))));
	  emit_insn (gen_movsi (protect_from_queue (x1, 1), protect_from_queue (y1, 0)));
	}
    }
  else
    abort ();
}

/* Pushing data onto the stack.  */

/* Push a block of length SIZE (perhaps variable)
   and return an rtx to address the beginning of the block.
   Note that it is not possible for the value returned to be a QUEUED.  */

static rtx
push_block (size)
     rtx size;
{
  register rtx temp;
  anti_adjust_stack (size);
	
#ifdef STACK_GROWS_DOWNWARD
  temp = gen_rtx (REG, Pmode, STACK_POINTER_REGNUM);
#else
  temp = gen_rtx (PLUS, Pmode,
		  gen_rtx (REG, Pmode, STACK_POINTER_REGNUM),
		  size);
  if (GET_CODE (size) != CONST_INT)
    temp = force_operand (temp, 0);
#endif
  return memory_address (QImode, temp);
}

static rtx
gen_push_operand ()
{
  return gen_rtx (
#ifdef STACK_GROWS_DOWNWARD
		  PRE_DEC,
#else
		  PRE_INC,
#endif
		  Pmode,
		  gen_rtx (REG, Pmode, STACK_POINTER_REGNUM));
}

/* Generate code to push X onto the stack, assuming it has mode MODE.
   MODE is redundant except when X is a CONST_INT (since they don't
   carry mode info).
   SIZE is an rtx for the size of data to be copied (in bytes),
   needed only if X is BLKmode.
   ALIGN (in bytes) is maximum alignment we can assume.  */

static void
emit_push_insn (x, mode, size, align)
     register rtx x;
     enum machine_mode mode;
     rtx size;
     int align;
{
  rtx xinner;

  xinner = x = protect_from_queue (x, 0);

  if (GET_CODE (x) == VOLATILE)
    xinner = XEXP (x, 0);

  if (mode == BLKmode)
    {
      register rtx temp;
	  
      if (size == 0)
	abort ();

      if (GET_CODE (size) == CONST_INT
	  && (move_by_pieces_ninsns ((unsigned) INTVAL (size), align)
	      < MOVE_RATIO))
	move_by_pieces (gen_push_operand (),
			XEXP (xinner, 0),
			INTVAL (size), align,
			0, GET_CODE (x) == VOLATILE);
      else
	{
	  temp = push_block (size);
#ifdef HAVE_movstrsi
	  if (HAVE_movstrsi)
	    {
	      emit_insn (gen_movstrsi (gen_rtx (MEM, BLKmode, temp), x, size));
	      return;
	    }
#endif
#ifdef HAVE_movstrhi
	  if (HAVE_movstrhi
	      && GET_CODE (size) == CONST_INT
	      && ((unsigned) INTVAL (size)
		  < (1 << (GET_MODE_SIZE (HImode) * BITS_PER_UNIT - 1))))
	    {
	      emit_insn (gen_movstrhi (gen_rtx (MEM, BLKmode, temp),
				       x, size));
	      return;
	    }
#endif
	  /* Correct TEMP so it holds what will be a description of
	     the address to copy to, valid after one arg is pushed.  */
#ifdef STACK_GROWS_DOWNWARD
	  temp = plus_constant (temp, GET_MODE_SIZE (Pmode));
#else
	  temp = plus_constant (temp, - GET_MODE_SIZE (Pmode));
#endif
	  emit_library_call (gen_rtx (SYMBOL_REF, Pmode, "bcopy"),
			     3, XEXP (xinner, 0), Pmode,
			     temp, Pmode,
			     size, Pmode);
	}
    }
  else if (mov_optab[(int) mode].insn_code != CODE_FOR_nothing)
    {
      register rtx push = gen_rtx (MEM, mode, gen_push_operand ());
      emit_insn (GEN_FCN (mov_optab[(int) mode].insn_code) (push, x));
    }
  else
    abort ();
}

/* Output a library call to function FUN (a SYMBOL_REF rtx)
   with NARGS different arguments, passed as alternating rtx values
   and machine_modes to convert them to.
   The rtx values should have been passed through protect_from_queue already.  */

/*VARARGS2*/
void
emit_library_call (fun, nargs, a1)
     rtx fun;
     int nargs;
     struct { rtx value; enum machine_mode mode; } a1;
{
  register int args_size = 0;
  register int argnum;
#ifndef STACK_GROWS_DOWNWARD
  for (argnum = 0; argnum < nargs; argnum++)
#else
  for (argnum = nargs - 1; argnum >= 0; argnum--)
#endif
    {
      register enum machine_mode mode = (&a1)[argnum].mode;
      register rtx val = (&a1)[argnum].value;
      /* Convert the arg value to the mode the library wants.  */
      /* ??? It is wrong to do it here; must do it earlier
	 where we know the signedness of the arg.  */
      if (GET_MODE (val) != mode && GET_MODE (val) != VOIDmode)
	{
	  val = gen_reg_rtx (mode);
	  convert_move (val, (&a1)[argnum].value, 0);
	}
      emit_push_insn (val, mode, 0, 0);
      args_size += GET_MODE_SIZE (mode);
      current_args_size += GET_MODE_SIZE (mode);
    }

  emit_queue ();
  gen_call_1 (fun, 0, args_size / GET_MODE_SIZE (SImode), args_size);
}

/* Expand an assignment that stores the value of FROM into TO.
   Return an rtx for the value of TO.  This may contain a QUEUED rtx.  */

rtx
expand_assignment (to, from)
     tree to, from;
{
  register rtx to_rtx = 0;

  /* Don't crash if the lhs of the assignment was erroneous.  */

  if (TREE_CODE (to) == ERROR_MARK)
    return expand_expr (from, 0, VOIDmode, 0);

  /* Assignment of a structure component needs special treatment
     if the structure component's rtx is not simply a MEM.  */

  if (TREE_CODE (to) == COMPONENT_REF)
    {
      register enum machine_mode mode1 = DECL_MODE (TREE_OPERAND (to, 1));
      int volstruct = 0;

      /* Get the structure as an rtx.  */

      to_rtx = expand_expr (TREE_OPERAND (to, 0), 0, VOIDmode, 0);

      /* If the structure is in a register or if the component
	 is a bit field, we cannot use addressing to access it.
	 Use bit-field techniques or SUBREG to store in it.  */

      if (mode1 == BImode || GET_CODE (to_rtx) == REG
	  || GET_CODE (to_rtx) == SUBREG)
	{
	  tree field = TREE_OPERAND (to, 1);
	  int bitsize = TREE_INT_CST_LOW (DECL_SIZE (field)) * DECL_SIZE_UNIT (field);
	  return store_bit_field (to_rtx, bitsize, DECL_OFFSET (field),
				  DECL_MODE (field),
				  expand_expr (from, 0, VOIDmode, 0));
	}

      /* Get the address of the structure the component is in.
	 Record if structure is volatile.  */

      if (GET_CODE (to_rtx) == VOLATILE)
	{
	  to_rtx = XEXP (to_rtx, 0);
	  volstruct = 1;
	}
      if (GET_CODE (to_rtx) != MEM)
	abort ();
      to_rtx = XEXP (to_rtx, 0);

      /* Now build a reference to just the desired component.  */

      to_rtx = gen_rtx (MEM, mode1,
			memory_address (mode1,
					plus_constant (to_rtx,
						       (DECL_OFFSET
							(TREE_OPERAND (to, 1))
							/ BITS_PER_UNIT))));
      to_rtx->in_struct = 1;

      /* Make component volatile if structure is.  */

      if (! cse_not_expected && volstruct)
	to_rtx = gen_rtx (VOLATILE, mode1, to_rtx);
    }

  /* Arrays in registers also need special treatment.  */

  if (TREE_CODE (to) == ARRAY_REF)
    {
      /* Check to see whether the array is in a register.  */
      tree array = TREE_OPERAND (TREE_OPERAND (to, 0), 0);
      register tree temexp;

      /* Look through any COMPONENT_REFS to the containing struct.
	 Start by taking the array out of the ADDR_EXPR that's operand 0.  */
      for (temexp = array;
	   TREE_CODE (temexp) == COMPONENT_REF;
	   temexp = TREE_OPERAND (temexp, 0));

      if (TREE_CODE (TREE_OPERAND (to, 1)) == INTEGER_CST
	  && TREE_CODE (temexp) == VAR_DECL
	  && DECL_RTL (temexp) != 0
	  && (GET_CODE (DECL_RTL (temexp)) == REG
	      || GET_CODE (DECL_RTL (temexp)) == SUBREG))
	{
	  /* The array or containing struct is a variable in a register
	     and the index is constant.  */
	  int bitsize = GET_MODE_BITSIZE (TYPE_MODE (TREE_TYPE (to)));

	  to_rtx = expand_expr (array, 0, VOIDmode, 0);
	  return store_bit_field (to_rtx, bitsize,
				  TREE_INT_CST_LOW (TREE_OPERAND (to, 1)) * bitsize,
				  TYPE_MODE (TREE_TYPE (to)),
				  expand_expr (from, 0, VOIDmode, 0));
	}

      /* The array is in memory.  Generate the tree for *(array+index)
	 and store into that insted.  */

      to = build_indirect_ref (build_binary_op (PLUS_EXPR,
						TREE_OPERAND (to, 0),
						TREE_OPERAND (to, 1)));
    }

  /* Ordinary treatment.  Expand TO to get a REG or MEM rtx.
     Don't re-expand if it was expanded already (in COMPONENT_REF case).  */

  if (to_rtx == 0)
    to_rtx = expand_expr (to, 0, VOIDmode, 0);

  /* Compute FROM and store the value in the rtx we got.  */

  store_expr (from, to_rtx);
  return to_rtx;
}

/* Generate code for computing expression EXP,
   and storing the value into TARGET.  Returns TARGET.
   TARGET may contain a QUEUED rtx.  */

static rtx
store_expr (exp, target)
     register tree exp;
     register rtx target;
{
  register rtx temp = expand_expr (exp, target, GET_MODE (target), 0);
  if (temp != target && TREE_CODE (exp) != ERROR_MARK)
    {
      target = protect_from_queue (target, 1);
      if (GET_MODE (temp) != GET_MODE (target)
	  && GET_MODE (temp) != VOIDmode)
	convert_move (target, temp, type_unsigned_p (TREE_TYPE (exp)));
      else if (GET_MODE (temp) == BLKmode)
	emit_block_move (target, temp, expr_size (exp),
			 TYPE_ALIGN (TREE_TYPE (exp)) / BITS_PER_UNIT);
      else
	emit_move_insn (target, temp);
    }
  return target;
}

/* Given an rtx VALUE that may contain additions and multiplications,
   return an equivalent value that just refers to a register or memory.
   This is done by generating instructions to perform the arithmetic
   and returning a pseudo-register containing the value.  */

rtx
force_operand (value, target)
     rtx value, target;
{
  register struct optab *binoptab = 0;
  register rtx op2 = XEXP (value, 1);
  /* Use subtarget as the target for operand 0 of a binary operation.  */
  register rtx subtarget = (target != 0 && GET_CODE (target) == REG ? target : 0);

  if (GET_CODE (value) == PLUS)
    binoptab = add_optab;
  else if (GET_CODE (value) == MINUS)
    binoptab = sub_optab;
  else if (GET_CODE (value) == MULT)
    {
      if (!CONSTANT_ADDRESS_P (op2)
	  && !(GET_CODE (op2) == REG && op2 != subtarget))
	subtarget = 0;
      return expand_mult (GET_MODE (value),
			  force_operand (XEXP (value, 0), subtarget),
			  force_operand (op2, 0),
			  target, 0);
    }

  if (binoptab)
    {
      if (!CONSTANT_ADDRESS_P (op2)
	  && !(GET_CODE (op2) == REG && op2 != subtarget))
	subtarget = 0;
      return expand_binop (GET_MODE (value), binoptab,
			   force_operand (XEXP (value, 0), subtarget),
			   force_operand (op2, 0),
			   target, 0, OPTAB_LIB_WIDEN);
      /* We give UNSIGNEP = 0 to expand_binop
	 because the only operations we are expanding here are signed ones.  */
    }
  return value;
}

/* expand_expr: generate code for computing expression EXP.
   An rtx for the computed value is returned.

   The value may be stored in TARGET if TARGET is nonzero.
   TARGET is just a suggestion; callers must assume that
   the rtx returned may not be the same as TARGET.

   If TMODE is not VOIDmode, it suggests generating the
   result in mode TMODE.  But this is done only when convenient.
   Otherwise, TMODE is ignored and the value generated in its natural mode.
   TMODE is just a suggestion; callers must assume that
   the rtx returned may not have mode TMODE.

   If SUM_OK is nonzero then when EXP is an addition
   we can return an rtx of the form (MULT (REG ...) (CONST_INT ...))
   or a nest of (PLUS ...) and (MINUS ...) where the terms are
   products as above, or REG or MEM, or constant.
   If SUM_OK is zero, in such cases we would output mul or add instructions
   and then return a pseudo reg containing the sum.  */

/* Subroutine of expand_expr:
   return the target to use when recursively expanding
   the first operand of an arithmetic operation.  */

static rtx
validate_subtarget (subtarget, otherop)
     rtx subtarget;
     tree otherop;
{
  if (TREE_LITERAL (otherop))
    return subtarget;
  if (TREE_CODE (otherop) == VAR_DECL
      && DECL_RTL (otherop) != subtarget)
    return subtarget;
  return 0;
}

rtx
expand_expr (exp, target, tmode, sum_ok)
     register tree exp;
     rtx target;
     enum machine_mode tmode;
     int sum_ok;
{
  register rtx op0, op1, temp;
  tree type = TREE_TYPE (exp);
  register enum machine_mode mode = TYPE_MODE (type);
  register enum tree_code code = TREE_CODE (exp);
  struct optab *this_optab;
  int negate_1;
  /* Use subtarget as the target for operand 0 of a binary operation.  */
  rtx subtarget = (target != 0 && GET_CODE (target) == REG ? target : 0);
  static tree dbg2;
  dbg2 = exp;

  /* If will do cse, generate all results into registers
     since 1) that allows cse to find more things
     and 2) otherwise cse could produce an insn the machine
     cannot support.  */

  if (! cse_not_expected && mode != BLKmode)
    target = subtarget;

  switch (code)
    {
    case FUNCTION_DECL:
    case VAR_DECL:
      temp = DECL_RTL (exp);
      if (! cse_not_expected && TREE_VOLATILE (exp))
	return gen_rtx (VOLATILE, DECL_MODE (exp), temp);
      else
	return temp;

    case PARM_DECL:
    case RESULT_DECL:
      if (DECL_RTL (exp) == 0)
	abort ();
      if (GET_CODE (DECL_RTL (exp)) == SYMBOL_REF)
	abort ();
      return DECL_RTL (exp);

    case INTEGER_CST:
      return gen_rtx (CONST_INT, VOIDmode, TREE_INT_CST_LOW (exp));

    case CONST_DECL:
      return expand_expr (DECL_INITIAL (exp), target, VOIDmode, 0);

    case REAL_CST:
      if (TREE_CST_RTL (exp))
	return TREE_CST_RTL (exp);
      /* If optimized, generate immediate float
	 which will be turned into memory float if necessary.  */
      if (!cse_not_expected)
	return immed_real_const (exp);
      output_constant_def (exp);
      return TREE_CST_RTL (exp);

    case COMPLEX_CST:
    case STRING_CST:
      if (TREE_CST_RTL (exp))
	return TREE_CST_RTL (exp);
      output_constant_def (exp);
      return TREE_CST_RTL (exp);

    case SAVE_EXPR:
      if (SAVE_EXPR_RTL (exp) == 0)
	{
	  SAVE_EXPR_RTL (exp) = gen_reg_rtx (mode);
	  store_expr (TREE_OPERAND (exp, 0), SAVE_EXPR_RTL (exp));
	}
      return SAVE_EXPR_RTL (exp);

    case INDIRECT_REF:
      {
	tree exp1 = TREE_OPERAND (exp, 0);
	tree exp2;

	/* A SAVE_EXPR as the address in an INDIRECT_EXPR is generated
	   for  *PTR += ANYTHING  where PTR is put inside the SAVE_EXPR.
	   This code has the same general effect as simply doing
	   expand_expr on the save expr, except that the expression PTR
	   is computed for use as a memory address.  This means different
	   code, suitable for indexing, may be generated.  */
	if (TREE_CODE (exp1) == SAVE_EXPR
	    && SAVE_EXPR_RTL (exp1) == 0
	    && TREE_CODE (exp2 = TREE_OPERAND (exp1, 0)) != ERROR_MARK
	    && TYPE_MODE (TREE_TYPE (exp1)) == Pmode
	    && TYPE_MODE (TREE_TYPE (exp2)) == Pmode)
	  {
	    temp = expand_expr (TREE_OPERAND (exp1, 0), 0, VOIDmode, 1);
	    op0 = memory_address (mode, temp);
	    op0 = copy_all_regs (op0);
	    SAVE_EXPR_RTL (exp1) = op0;
	  }
	else
	  {
	    op0 = expand_expr (TREE_OPERAND (exp, 0), 0, VOIDmode, 1);
	    op0 = memory_address (mode, op0);
	  }
      }
      temp = gen_rtx (MEM, mode, op0);
      if (! cse_not_expected && TREE_THIS_VOLATILE (exp))
	return gen_rtx (VOLATILE, mode, temp);
      else
	return temp;

    case COMPONENT_REF:
      {
	register enum machine_mode mode1 = DECL_MODE (TREE_OPERAND (exp, 1));
	int volstruct = 0;
	tree dbg1 = TREE_OPERAND (exp, 0);  /* For debugging */

	op0 = expand_expr (TREE_OPERAND (exp, 0), 0, VOIDmode, 0);
	if (mode1 == BImode || GET_CODE (op0) == REG
	    || GET_CODE (op0) == SUBREG)
	  {
	    tree field = TREE_OPERAND (exp, 1);
	    int bitsize = TREE_INT_CST_LOW (DECL_SIZE (field)) * DECL_SIZE_UNIT (field);
	    return extract_bit_field (op0, bitsize, DECL_OFFSET (field),
				      type_unsigned_p (TREE_TYPE (field)),
				      target, mode, tmode);
	  }
	if (tmode != VOIDmode)
	  mode = tmode;
	/* Get the address of the structure the component is in.  */
	if (GET_CODE (op0) == VOLATILE)
	  {
	    op0 = XEXP (op0, 0);
	    volstruct = 1;
	  }
	if (GET_CODE (op0) != MEM)
	  abort ();
	op0 = XEXP (op0, 0);
	op0 = gen_rtx (MEM, mode1,
		       memory_address (mode1,
				       plus_constant (op0,
						      (DECL_OFFSET
						       (TREE_OPERAND (exp, 1))
						       / BITS_PER_UNIT))));
	op0->in_struct = 1;
	if (! cse_not_expected && volstruct)
	  op0 = gen_rtx (VOLATILE, mode1, op0);
	if (mode == mode1 || mode == BLKmode)
	  return op0;
	if (target == 0)
	  target = gen_reg_rtx (mode);
	convert_move (target, op0, type_unsigned_p (TREE_TYPE (TREE_OPERAND (exp, 1))));
	return target;
      }

      /* ARRAY_REF is used in C for an actual array (not just a pointer)
	 indexed by a constant index.  It enables us to avoid taking the
	 address of the array, which may allow a short array (or a struct
	 or union containing one) to go in a register.  */
    case ARRAY_REF:
      {
	/* Check to see whether the array is in a register.  */
	tree array = TREE_OPERAND (TREE_OPERAND (exp, 0), 0);
	register tree temexp;

	/* Look through any COMPONENT_REFS to the containing struct.
	   Start by taking the array out of the ADDR_EXPR that's operand 0.  */
	for (temexp = array;
	     TREE_CODE (temexp) == COMPONENT_REF;
	     temexp = TREE_OPERAND (temexp, 0));

	if (TREE_CODE (TREE_OPERAND (exp, 1)) == INTEGER_CST
	    && TREE_CODE (temexp) == VAR_DECL
	    && DECL_RTL (temexp) != 0
	    && (GET_CODE (DECL_RTL (temexp)) == REG
		|| GET_CODE (DECL_RTL (temexp)) == SUBREG))
	  {
	    /* The array or containing struct is a variable in a register
	       and the index is constant.  */
	    int bitsize = GET_MODE_BITSIZE (TYPE_MODE (TREE_TYPE (exp)));

	    op0 = expand_expr (array, 0, VOIDmode, 0);
	    return extract_bit_field (op0, bitsize,
				      TREE_INT_CST_LOW (TREE_OPERAND (exp, 1)) * bitsize,
				      type_unsigned_p (TREE_TYPE (exp)),
				      target, mode, tmode);
	  }

	/* The array is in memory.  Generate the tree for *(array+index)
	   and expand that.  */

	temexp = build_indirect_ref (build_binary_op (PLUS_EXPR,
						      TREE_OPERAND (exp, 0),
						      TREE_OPERAND (exp, 1)));
	return expand_expr (temexp, 0, VOIDmode, 0);
      }

      /* Intended for a reference to a buffer of a file-object in Pascal.
	 But it's not certain that a special tree code will really be
	 necessary for these.  INDIRECT_REF might work for them.  */
    case BUFFER_REF:
      abort ();

    case CALL_EXPR:
      /* If this call was expanded already by preexpand_calls,
	 just return the result we got.  */
      if (CALL_EXPR_RTL (exp) != 0)
	return CALL_EXPR_RTL (exp);
      return expand_call (exp, target);

    case NOP_EXPR:
    case CONVERT_EXPR:
      if (TREE_CODE (type) == VOID_TYPE)
	{
	  expand_expr (TREE_OPERAND (exp, 0), target, VOIDmode, sum_ok);
	  return const0_rtx;
	}
      if (mode == TYPE_MODE (TREE_TYPE (TREE_OPERAND (exp, 0))))
	return expand_expr (TREE_OPERAND (exp, 0), target, VOIDmode, sum_ok);
      op0 = expand_expr (TREE_OPERAND (exp, 0), 0, mode, 0);
      if (GET_MODE (op0) == mode)
	return op0;
      if (target == 0)
	target = gen_reg_rtx (mode);
      convert_move (target, op0, type_unsigned_p (TREE_TYPE (TREE_OPERAND (exp, 0))));
      return target;

    case PLUS_EXPR:
      preexpand_calls (exp);
      if (TREE_CODE (TREE_OPERAND (exp, 0)) == INTEGER_CST)
	{
	  op1 = expand_expr (TREE_OPERAND (exp, 1), subtarget, VOIDmode, 1);
	  op1 = plus_constant (op1, TREE_INT_CST_LOW (TREE_OPERAND (exp, 0)));
	  if (sum_ok)
	    return op1;
	  return force_operand (op1, target);
	}
      negate_1 = 1;
    plus_minus:
      if (TREE_CODE (TREE_OPERAND (exp, 1)) == INTEGER_CST)
	{
	  op0 = expand_expr (TREE_OPERAND (exp, 0), subtarget, VOIDmode, 1);
	  op0 = plus_constant (op0,
			       negate_1 * TREE_INT_CST_LOW (TREE_OPERAND (exp, 1)));
	  if (sum_ok)
	    return op0;
	  return force_operand (op0, target);
	}
      this_optab = add_optab;
      if (!sum_ok) goto binop;
      subtarget = validate_subtarget (subtarget, TREE_OPERAND (exp, 1));
      op0 = expand_expr (TREE_OPERAND (exp, 0), subtarget, VOIDmode, 1);
      op1 = expand_expr (TREE_OPERAND (exp, 1), 0, VOIDmode, 1);
      /* Put a sum last, to simplify what follows.  */
#ifdef OLD_INDEXING
      if (GET_CODE (op1) == MULT)
	{
	  temp = op0;
	  op0 = op1;
	  op1 = temp;
	}
#endif
#ifndef OLD_INDEXING
      /* Make sure any term that's a sum with a constant comes last.  */
      if (GET_CODE (op0) == PLUS
	  && CONSTANT_ADDRESS_P (XEXP (op0, 1)))
	{
	  temp = op0;
	  op0 = op1;
	  op1 = temp;
	}
      /* If adding to a sum including a constant,
	 associate it to put the constant outside.  */
      if (GET_CODE (op1) == PLUS
	  && CONSTANT_ADDRESS_P (XEXP (op1, 1)))
	{
	  op0 = gen_rtx (PLUS, mode, XEXP (op1, 0), op0);
	  if (GET_CODE (XEXP (op1, 1)) == CONST_INT)
	    return plus_constant (op0, INTVAL (XEXP (op1, 1)));
	  else
	    return gen_rtx (PLUS, mode, op0, XEXP (op1, 1));
	}
#endif
      return gen_rtx (PLUS, mode, op0, op1);

    case MINUS_EXPR:
      preexpand_calls (exp);
      if (TREE_CODE (TREE_OPERAND (exp, 1)) == INTEGER_CST)
	{
	  negate_1 = -1;
	  goto plus_minus;
	}
      this_optab = sub_optab;
      goto binop;

    case MULT_EXPR:
      preexpand_calls (exp);
      /* If first operand is constant, swap them.
	 Thus the following special case checks need only
	 check the second operand.  */
      if (TREE_CODE (TREE_OPERAND (exp, 0)) == INTEGER_CST)
	{
	  register tree t1 = TREE_OPERAND (exp, 0);
	  TREE_OPERAND (exp, 0) = TREE_OPERAND (exp, 1);
	  TREE_OPERAND (exp, 1) = t1;
	}

      /* Attempt to return something suitable for generating an
	 indexed address, for machines that support that.  */

      if (sum_ok && TREE_CODE (TREE_OPERAND (exp, 1)) == INTEGER_CST) 
	{
	  op0 = expand_expr (TREE_OPERAND (exp, 0), subtarget, VOIDmode, 0);
	  if (GET_CODE (op0) != REG)
	    {
	      temp = gen_reg_rtx (GET_MODE (op0));
	      emit_move_insn (temp, op0);
	      op0 = temp;
	    }
	  return gen_rtx (MULT, mode, op0, 
			  gen_rtx (CONST_INT, VOIDmode,
				   TREE_INT_CST_LOW (TREE_OPERAND (exp, 1))));
	}
      subtarget = validate_subtarget (subtarget, TREE_OPERAND (exp, 1));
      /* Check for multiplying things that have been extended
	 from a narrower type.  If this machine supports multiplying
	 in that narrower type with a result in the desired type,
	 do it that way, and avoid the explicit type-conversion.  */
      if (TREE_CODE (TREE_OPERAND (exp, 0)) == NOP_EXPR
	  && TREE_CODE (TREE_TYPE (exp)) == INTEGER_TYPE
	  && (TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (TREE_OPERAND (exp, 0), 0)))
	      < TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (exp, 0))))
	  && ((TREE_CODE (TREE_OPERAND (exp, 1)) == INTEGER_CST
	       && int_fits_type_p (TREE_OPERAND (exp, 1),
				   TREE_TYPE (TREE_OPERAND (TREE_OPERAND (exp, 0), 0))))
	      ||
	      (TREE_CODE (TREE_OPERAND (exp, 1)) == NOP_EXPR
	       && (TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (TREE_OPERAND (exp, 1), 0)))
		   ==
		   TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (TREE_OPERAND (exp, 0), 0)))))))
	{
	  enum machine_mode innermode
	    = TYPE_MODE (TREE_TYPE (TREE_OPERAND (TREE_OPERAND (exp, 0), 0)));
	  this_optab = (type_unsigned_p (TREE_TYPE (exp))
			? umul_widen_optab : smul_widen_optab);
	  if ((int) innermode + 1 == (int) mode
	      && this_optab[(int) mode].insn_code != CODE_FOR_nothing)
	    {
	      op0 = expand_expr (TREE_OPERAND (TREE_OPERAND (exp, 0), 0),
				 0, VOIDmode, 0);
	      if (TREE_CODE (TREE_OPERAND (exp, 1)) == INTEGER_CST)
		op1 = expand_expr (TREE_OPERAND (exp, 1), 0, VOIDmode, 0);
	      else
		op1 = expand_expr (TREE_OPERAND (TREE_OPERAND (exp, 1), 0),
				   0, VOIDmode, 0);
	      goto binop2;
	    }
	}
      op0 = expand_expr (TREE_OPERAND (exp, 0), subtarget, VOIDmode, 0);
      op1 = expand_expr (TREE_OPERAND (exp, 1), 0, VOIDmode, 0);
      return expand_mult (mode, op0, op1, target, type_unsigned_p (type));

    case TRUNC_DIV_EXPR:
    case FLOOR_DIV_EXPR:
    case CEIL_DIV_EXPR:
    case ROUND_DIV_EXPR:
      preexpand_calls (exp);
      subtarget = validate_subtarget (subtarget, TREE_OPERAND (exp, 1));
      /* Possible optimization: compute the dividend with SUM_OK
	 then if the divisor is constant can optimize the case
	 where some terms of the dividend have coeffs divisible by it.  */
      op0 = expand_expr (TREE_OPERAND (exp, 0), subtarget, VOIDmode, 0);
      op1 = expand_expr (TREE_OPERAND (exp, 1), 0, VOIDmode, 0);
      return expand_divmod (0, code, mode, op0, op1, target,
			    type_unsigned_p (type));

    case RDIV_EXPR:
      preexpand_calls (exp);
      this_optab = flodiv_optab;
      goto binop;

    case TRUNC_MOD_EXPR:
    case FLOOR_MOD_EXPR:
    case CEIL_MOD_EXPR:
    case ROUND_MOD_EXPR:
      preexpand_calls (exp);
      subtarget = validate_subtarget (subtarget, TREE_OPERAND (exp, 1));
      op0 = expand_expr (TREE_OPERAND (exp, 0), subtarget, VOIDmode, 0);
      op1 = expand_expr (TREE_OPERAND (exp, 1), 0, VOIDmode, 0);
      return expand_divmod (1, code, mode, op0, op1, target,
			    type_unsigned_p (type));
#if 0
#ifdef HAVE_divmoddisi4
      if (GET_MODE (op0) != DImode)
	{
	  temp = gen_reg_rtx (DImode);
	  convert_move (temp, op0, 0);
	  op0 = temp;
	  if (GET_MODE (op1) != SImode && GET_CODE (op1) != CONST_INT)
	    {
	      temp = gen_reg_rtx (SImode);
	      convert_move (temp, op1, 0);
	      op1 = temp;
	    }
	  temp = gen_reg_rtx (SImode);
	  if (target == 0)
	    target = gen_reg_rtx (SImode);
	  emit_insn (gen_divmoddisi4 (temp, protect_from_queue (op0, 0),
				      protect_from_queue (op1, 0),
				      protect_from_queue (target, 1)));
	  return target;
	}
#endif
#endif

    case FIX_ROUND_EXPR:
    case FIX_FLOOR_EXPR:
    case FIX_CEIL_EXPR:
      abort ();			/* Not used for C.  */

    case FIX_TRUNC_EXPR:
      op0 = expand_expr (TREE_OPERAND (exp, 0), 0, VOIDmode, 0);
      if (target == 0)
	target = gen_reg_rtx (mode);
      if (mode == HImode || mode == QImode)
	{
	  register rtx temp = gen_reg_rtx (SImode);
	  expand_fix (temp, op0);
	  convert_move (target, temp, 0);
	}
      else
	expand_fix (target, op0);
      return target;

    case FLOAT_EXPR:
      op0 = expand_expr (TREE_OPERAND (exp, 0), 0, VOIDmode, 0);
      if (target == 0)
	target = gen_reg_rtx (mode);
      if (GET_MODE (op0) == HImode
	  || GET_MODE (op0) == QImode)
	{
	  register rtx temp = gen_reg_rtx (SImode);
	  convert_move (temp, op0, 0);
	  expand_float (target, temp);
	}
      else
	expand_float (target, op0);
      return target;

    case NEGATE_EXPR:
      op0 = expand_expr (TREE_OPERAND (exp, 0), target, VOIDmode, 0);
      temp = expand_unop (mode, neg_optab, op0, target, 0);
      if (temp == 0)
	abort ();
      return temp;

    case ABS_EXPR:
      /* First try to do it with a special abs instruction.
	 If that does not win, use conditional jump and negate.  */
      op0 = expand_expr (TREE_OPERAND (exp, 0), target, VOIDmode, 0);
      temp = expand_unop (mode, abs_optab, op0, target, 0);
      if (temp != 0)
	return temp;
      temp = gen_label_rtx ();
      if (target == 0 || GET_CODE (target) != REG)
	target = gen_reg_rtx (GET_MODE (op0));
      emit_move_insn (target, op0);
      emit_tst_insn (target);
      emit_jump_insn (gen_bge (temp));
      op0 = expand_unop (mode, neg_optab, target, target, 0);
      if (op0 != target)
	emit_move_insn (target, op0);
      emit_label (temp);
      return target;

    case MAX_EXPR:
    case MIN_EXPR:
      op1 = expand_expr (TREE_OPERAND (exp, 1), 0, VOIDmode, 0);
      if (target == 0 || GET_CODE (target) != REG || target == op1)
	target = gen_reg_rtx (GET_MODE (op0));
      op0 = expand_expr (TREE_OPERAND (exp, 0), target, VOIDmode, 0);
      if (target != op0)
	emit_move_insn (target, op0);
      op0 = gen_label_rtx ();
      if (code == MAX_EXPR)
	temp = (type_unsigned_p (TREE_TYPE (TREE_OPERAND (exp, 1)))
		? compare1 (target, op1, GEU, LEU, 1)
		: compare1 (target, op1, GE, LE, 0));
      else
	temp = (type_unsigned_p (TREE_TYPE (TREE_OPERAND (exp, 1)))
		? compare1 (target, op1, LEU, GEU, 1)
		: compare1 (target, op1, LE, GE, 0));
      emit_jump_insn (gen_rtx (SET, VOIDmode, pc_rtx,
			       gen_rtx (IF_THEN_ELSE, VOIDmode,
					temp,
					gen_rtx (LABEL_REF, VOIDmode, op0),
					pc_rtx)));
      emit_move_insn (target, op1);
      emit_label (temp);
      return target;

/* ??? Can optimize when the operand of this is a bitwise operation,
   by using a different bitwise operation.  */
    case BIT_NOT_EXPR:
      op0 = expand_expr (TREE_OPERAND (exp, 0), subtarget, VOIDmode, 0);
      temp = expand_unop (mode, one_cmpl_optab, op0, target, 1);
      if (temp == 0)
	abort ();
      return temp;

/* ??? Can optimize bitwise operations with one arg constant.
   Pastel optimizes (a bitwise1 n) bitwise2 (a bitwise3 b)
   and (a bitwise1 b) bitwise2 b (etc)
   but that is probably not worth while.  */

/* AND_EXPR is for bitwise anding.
   TRUTH_AND_EXPR is for anding two boolean values
   when we want in all cases to compute both of them.
   In general it is fastest to do TRUTH_AND_EXPR by
   computing both operands as actual zero-or-1 values
   and then bitwise anding.  In cases where there cannot
   be any side effects, better code would be made by
   treating TRUTH_AND_EXPR like TRUTH_ANDIF_EXPR;
   but the question is how to recognize those cases.  */

    case TRUTH_AND_EXPR:
    case BIT_AND_EXPR:
      preexpand_calls (exp);
      subtarget = validate_subtarget (subtarget, TREE_OPERAND (exp, 1));
      op0 = expand_expr (TREE_OPERAND (exp, 0), subtarget, VOIDmode, 0);
      op1 = expand_expr (TREE_OPERAND (exp, 1), 0, VOIDmode, 0);
      return expand_bit_and (mode, op0, op1, target);

/* See comment above about TRUTH_AND_EXPR; it applies here too.  */
    case TRUTH_OR_EXPR:
    case BIT_IOR_EXPR:
      preexpand_calls (exp);
      this_optab = ior_optab;
      goto binop;

    case BIT_XOR_EXPR:
      preexpand_calls (exp);
      this_optab = xor_optab;
      goto binop;

    case LSHIFT_EXPR:
    case RSHIFT_EXPR:
    case LROTATE_EXPR:
    case RROTATE_EXPR:
      preexpand_calls (exp);
      subtarget = validate_subtarget (subtarget, TREE_OPERAND (exp, 1));
      op0 = expand_expr (TREE_OPERAND (exp, 0), subtarget, VOIDmode, 0);
      return expand_shift (code, mode, op0, TREE_OPERAND (exp, 1), target,
			   type_unsigned_p (type));

/* ??? cv's were used to effect here to combine additive constants
   and to determine the answer when only additive constants differ.
   Also, the addition of one can be handled by changing the condition.  */
    case LT_EXPR:
    case LE_EXPR:
    case GT_EXPR:
    case GE_EXPR:
    case EQ_EXPR:
    case NE_EXPR:
      preexpand_calls (exp);
      temp = do_store_flag (exp, target);
      if (temp != 0)
	return temp;
      if (code == NE_EXPR && integer_zerop (TREE_OPERAND (exp, 1)))
	{
	  /* For foo != 0, load foo, and if it is nonzero load 1 instead. */
	  temp = expand_expr (TREE_OPERAND (exp, 0), subtarget, VOIDmode, 0);
	  if (temp != subtarget)
	    temp = copy_to_reg (temp);
	  op1 = gen_label_rtx ();
	  emit_cmp_insn (temp, const0_rtx, 0, type_unsigned_p (type));
	  emit_jump_insn (gen_beq (op1));
	  emit_move_insn (temp, const1_rtx);
	  emit_label (op1);
	  return temp;
	}
      /* If no set-flag instruction, must generate a conditional
	 store into a temporary variable.  Drop through
	 and handle this like && and ||.  */

    case TRUTH_ANDIF_EXPR:
    case TRUTH_ORIF_EXPR:
      temp = gen_reg_rtx (mode);
      emit_clr_insn (temp);
      op1 = gen_label_rtx ();
      jumpifnot (exp, op1);
      emit_0_to_1_insn (temp);
      emit_label (op1);
      return temp;

    case TRUTH_NOT_EXPR:
      op0 = expand_expr (TREE_OPERAND (exp, 0), target, VOIDmode, 0);
      /* The parser is careful to generate TRUTH_NOT_EXPR
	 only with operands that are always zero or one.  */
      temp = expand_binop (mode, xor_optab, op0,
			   gen_rtx (CONST_INT, mode, 1),
			   target, 1, OPTAB_LIB_WIDEN);
      if (temp == 0)
	abort ();
      return temp;

    case COMPOUND_EXPR:
      expand_expr (TREE_OPERAND (exp, 0), 0, VOIDmode, 0);
      emit_queue ();
      return expand_expr (TREE_OPERAND (exp, 1), target, VOIDmode, 0);

    case COND_EXPR:
      /* Note that COND_EXPRs whose type is a structure or union
	 are required to be constructed to contain assignments of
	 a temporary variable, so that we can evaluate them here
	 for side effect only.  If type is void, we must do likewise.  */
      op0 = gen_label_rtx ();
      op1 = gen_label_rtx ();

      if (mode == BLKmode || mode == VOIDmode)
	temp = 0;
      else if (target)
	temp = target;
      else
	temp = gen_reg_rtx (mode);

      jumpifnot (TREE_OPERAND (exp, 0), op0);
      current_args_size += 1;
      if (temp != 0)
	store_expr (TREE_OPERAND (exp, 1), temp);
      else
	expand_expr (TREE_OPERAND (exp, 1), 0, VOIDmode, 0);
      emit_queue ();
      emit_jump_insn (gen_jump (op1));
      emit_barrier ();
      emit_label (op0);
      if (temp != 0)
	store_expr (TREE_OPERAND (exp, 2), temp);
      else
	expand_expr (TREE_OPERAND (exp, 2), 0, VOIDmode, 0);
      emit_queue ();
      emit_label (op1);
      current_args_size -= 1;
      return temp;

    case MODIFY_EXPR:
      /* If lhs is complex, expand calls in rhs before computing it.
	 That's so we don't compute a pointer and save it over a call.
	 If lhs is simple, compute it first so we can give it as a
	 target if the rhs is just a call.  This avoids an extra temp and copy
	 and that prevents a partial-subsumption which makes bad code.
	 Actually we could treat component_ref's of vars like vars.  */
      if (TREE_CODE (TREE_OPERAND (exp, 0)) != VAR_DECL)
	preexpand_calls (exp);
      temp = expand_assignment (TREE_OPERAND (exp, 0),
				TREE_OPERAND (exp, 1));
      return temp;

    case PREINCREMENT_EXPR:
      op0 = expand_expr (TREE_OPERAND (exp, 0), 0, VOIDmode, 0);
      op1 = expand_expr (TREE_OPERAND (exp, 1), 0, VOIDmode, 0);
      expand_binop (mode, add_optab, copy_rtx (op0), op1, copy_rtx (op0),
		    0, OPTAB_LIB_WIDEN);
      return op0;

    case PREDECREMENT_EXPR:
      op0 = expand_expr (TREE_OPERAND (exp, 0), 0, VOIDmode, 0);
      op1 = expand_expr (TREE_OPERAND (exp, 1), 0, VOIDmode, 0);
      expand_binop (mode, sub_optab, copy_rtx (op0), op1, copy_rtx (op0),
		    0, OPTAB_LIB_WIDEN);
      return op0;

    case POSTINCREMENT_EXPR:
      op0 = expand_expr (TREE_OPERAND (exp, 0), 0, VOIDmode, 0);
      op1 = expand_expr (TREE_OPERAND (exp, 1), 0, VOIDmode, 0);
      op0 = stabilize (op0);
      return enqueue_insn (op0, gen_add2_insn (copy_rtx (op0), op1));

    case POSTDECREMENT_EXPR:
      op0 = expand_expr (TREE_OPERAND (exp, 0), 0, VOIDmode, 0);
      op1 = expand_expr (TREE_OPERAND (exp, 1), 0, VOIDmode, 0);
      op0 = stabilize (op0);
      return enqueue_insn (op0, gen_sub2_insn (copy_rtx (op0), op1));

    case ADDR_EXPR:
      op0 = expand_expr (TREE_OPERAND (exp, 0), 0, VOIDmode, 0);
      if (GET_CODE (op0) == VOLATILE)
	op0 = XEXP (op0, 0);
      if (GET_CODE (op0) != MEM)
	abort ();
      if (sum_ok)
	return XEXP (op0, 0);
      return force_operand (XEXP (op0, 0), target);

    case ENTRY_VALUE_EXPR:
      abort ();

    case ERROR_MARK:
      return gen_rtx (CONST_INT, (mode != VOIDmode) ? mode : SImode, 0);

    default:
      abort ();
    }

  /* Here to do an ordinary binary operator, generating an instruction
     from the optab already placed in `this_optab'.  */
 binop:
  /* Detect things like x = y | (a == b)
     and do them as (x = y), (a == b ? x |= 1 : 0), x.  */
  /* First, get the comparison or conditional into the second arg.  */
  if (comparison_code[(int) TREE_CODE (TREE_OPERAND (exp, 0))]
      || (TREE_CODE (TREE_OPERAND (exp, 0)) == COND_EXPR
	  && (integer_zerop (TREE_OPERAND (TREE_OPERAND (exp, 0), 1))
	      || integer_zerop (TREE_OPERAND (TREE_OPERAND (exp, 0), 2)))))
    {
      if (this_optab == ior_optab || this_optab == add_optab
	  || this_optab == xor_optab)
	{
	  tree exch = TREE_OPERAND (exp, 1);
	  TREE_OPERAND (exp, 1) = TREE_OPERAND (exp, 0);
	  TREE_OPERAND (exp, 0) = exch;
	}
    }
  if (comparison_code[(int) TREE_CODE (TREE_OPERAND (exp, 1))]
      || (TREE_CODE (TREE_OPERAND (exp, 1)) == COND_EXPR
	  && (integer_zerop (TREE_OPERAND (TREE_OPERAND (exp, 1), 1))
	      || integer_zerop (TREE_OPERAND (TREE_OPERAND (exp, 1), 2)))))
    {
      if (this_optab == ior_optab || this_optab == add_optab
	  || this_optab == xor_optab || this_optab == sub_optab
	  || this_optab == lshl_optab || this_optab == ashl_optab
	  || this_optab == lshr_optab || this_optab == ashr_optab
	  || this_optab == rotl_optab || this_optab == rotr_optab)
	{
	  tree thenexp, condexp;
	  rtx thenv = 0;

	  if (target == 0) target = gen_reg_rtx (mode);
	  store_expr (TREE_OPERAND (exp, 0), target);
	  op0 = gen_label_rtx ();

	  if (TREE_CODE (TREE_OPERAND (exp, 1)) != COND_EXPR)
	    {
	      do_jump (TREE_OPERAND (exp, 1), op0, 0);
	      thenv = const1_rtx;
	    }
	  else if (integer_zerop (TREE_OPERAND (TREE_OPERAND (exp, 1), 2)))
	    {
	      do_jump (TREE_OPERAND (TREE_OPERAND (exp, 1), 0), op0, 0);
	      thenexp = TREE_OPERAND (TREE_OPERAND (exp, 1), 1);
	    }
	  else
	    {
	      do_jump (TREE_OPERAND (TREE_OPERAND (exp, 1), 0), 0, op0);
	      thenexp = TREE_OPERAND (TREE_OPERAND (exp, 1), 2);
	    }

	  if (thenv == 0)
	    thenv = expand_expr (thenexp, 0, VOIDmode, 0);

	  if (this_optab == rotl_optab || this_optab == rotr_optab)
	    temp = expand_binop (mode, this_optab, target, thenv, target,
				 -1, OPTAB_LIB);
	  else if (this_optab == lshl_optab || this_optab == lshr_optab)
	    temp = expand_binop (mode, this_optab, target, thenv, target,
				 1, OPTAB_LIB_WIDEN);
	  else
	    temp = expand_binop (mode, this_optab, target, thenv, target,
				 0, OPTAB_LIB_WIDEN);
	  if (target != temp)
	    emit_move_insn (target, temp);

	  emit_label (op0);
	  return target;
	}
    }
  subtarget = validate_subtarget (subtarget, TREE_OPERAND (exp, 1));
  op0 = expand_expr (TREE_OPERAND (exp, 0), subtarget, VOIDmode, 0);
  op1 = expand_expr (TREE_OPERAND (exp, 1), 0, VOIDmode, 0);
 binop2:
  temp = expand_binop (mode, this_optab, op0, op1, target,
		       type_unsigned_p (TREE_TYPE (exp)), OPTAB_LIB_WIDEN);
 binop1:
  if (temp == 0)
    abort ();
  return temp;
}

/* Expand all function calls contained within EXP, innermost ones first.
   But don't look within expressions that have sequence points.
   For each CALL_EXPR, record the rtx for its value
   in the CALL_EXPR_RTL field..  */

static void
preexpand_calls (exp)
     tree exp;
{
  register int nops, i;

  if (! do_preexpand_calls)
    return;

  switch (TREE_CODE (exp))
    {
    case CALL_EXPR:
      if (CALL_EXPR_RTL (exp) == 0)
	CALL_EXPR_RTL (exp) = expand_call (exp, 0);
      return;

    case COMPOUND_EXPR:
    case COND_EXPR:
    case TRUTH_ANDIF_EXPR:
    case TRUTH_ORIF_EXPR:
      /* If we find one of these, then we can be sure
	 the adjust will be done for it (since it makes jumps).
	 Do it now, so that if this is inside an argument
	 of a function, we don't get the stack adjustment
	 after some other args have already been pushed.  */
      do_pending_stack_adjust ();
      return;

    case SAVE_EXPR:
      if (SAVE_EXPR_RTL (exp) != 0)
	return;
    }

  nops = tree_code_length[(int) TREE_CODE (exp)];
  for (i = 0; i < nops; i++)
    if (TREE_OPERAND (exp, i) != 0)
      {
	register int type = *tree_code_type[(int) TREE_CODE (TREE_OPERAND (exp, i))];
	if (type == 'e' || type == 'r')
	  preexpand_calls (TREE_OPERAND (exp, i));
      }
}

/* Generate instructions to call function FUNEXP and pass
   it the static chain.  NARGS is the "number of args",
   to put in the call instruction on machines that require this.
   Also generate the code to pop the args after returning,
   (ARGS_SIZE is size of stuff to pop, in bytes).  */

static void
gen_call_1 (funexp, context, nargs, args_size)
     rtx funexp;
     rtx context;
     int nargs;
     int args_size;
{
  funexp = protect_from_queue (funexp, 0);
  if (context)
    context = protect_from_queue (context, 0);

  /* Function variable in language with nested functions.  */
  if (GET_MODE (funexp) == EPmode)
    {
      register rtx reg = gen_rtx (REG, Pmode, STATIC_CHAIN_REGNUM);
      emit_insn (gen_movsi (reg, gen_highpart (Pmode, funexp)));
      emit_insn (gen_rtx (USE, VOIDmode, reg));
      funexp = memory_address (QImode, gen_lowpart (Pmode, funexp));
      emit_call_insn (gen_call (gen_rtx (MEM, QImode, funexp),
				gen_rtx (CONST_INT, VOIDmode, nargs)));
    }
  else
    {
      if (context != 0)
	{
	  /* Unless function variable in C, or top level function constant */
	  register rtx reg = gen_rtx (REG, Pmode, STATIC_CHAIN_REGNUM);
	  emit_insn (gen_movsi (reg, lookup_static_chain (context)));
	  emit_insn (gen_rtx (USE, VOIDmode, reg));
	}
      emit_call_insn (gen_call (gen_rtx (MEM, QImode, 
					 memory_address (QImode, funexp)),
				gen_rtx (CONST_INT, VOIDmode, nargs)));
    }
  /* If returning from the subroutine does not automatically pop the args,
     we need an instruction to pop them sooner or later.
     Perhaps do it now; perhaps just record how much space to pop later.  */
  current_args_size -= args_size;
#ifndef RETURN_POPS_ARGS
  if (args_size != 0)
    {
      if (TARGET_DEFER_POP && current_args_size == 0)
	pending_stack_adjust += args_size;
      else
	adjust_stack (gen_rtx (CONST_INT, VOIDmode, args_size));
    }
#endif
}

/* At the start of a function, record that we have no previously-pushed
   arguments waiting to be popped.  */

clear_pending_stack_adjust ()
{
  pending_stack_adjust = 0;
}

/* At start of function, initialize.  */
clear_current_args_size ()
{
  current_args_size = 0;
}

/* Pop any previously-pushed arguments that have not been popped yet.  */

do_pending_stack_adjust ()
{
  if (current_args_size == 0)
    {
      if (pending_stack_adjust != 0)
	adjust_stack (gen_rtx (CONST_INT, VOIDmode, pending_stack_adjust));
      pending_stack_adjust = 0;
    }
}

/* Generate all the code for a function call
   and return an rtx for its value.
   Store the value in TARGET (specified as an rtx) if convenient.
   If the value is stored in TARGET then TARGET is returned.  */

static rtx
expand_call (exp, target)
     tree exp;
     rtx target;
{
  tree actparms = TREE_OPERAND (exp, 1);
  register tree p;
  int args_size = 0;
  register int i;
  register tree *argvec;
  int num_actuals;
  rtx structure_value_addr = 0;

  /* Don't let pending stack adjusts add up to too much.
     Also, do all pending adjustments now
     if there is any chance this might be a call to alloca.  */

  if (pending_stack_adjust >= 32
      || (pending_stack_adjust > 0
	  &&
	  /* Unless it's a call to a specific function that isn't alloca,
	     we must assume it might be alloca.  */
	  !(p = TREE_OPERAND (exp, 0),
	    TREE_CODE (p) == ADDR_EXPR
	    && TREE_CODE (TREE_OPERAND (p, 0)) == FUNCTION_DECL
	    && strcmp (IDENTIFIER_POINTER (DECL_NAME (TREE_OPERAND (p, 0))),
		       "alloca"))))
    do_pending_stack_adjust ();

  if (TYPE_MODE (TREE_TYPE (exp)) == BLKmode)
    {
      /* This call returns a big structure.  */
      if (target)
	structure_value_addr = XEXP (target, 0);
      else
	/* Make room on the stack to hold the value.  */
	structure_value_addr = get_structure_value_addr (expr_size (exp));
    }

  for (p = actparms, i = 0; p; p = TREE_CHAIN (p)) i++;
  num_actuals = i;
  argvec = (tree *) alloca (i * sizeof (tree));

#ifdef STACK_GROWS_DOWNWARD
  /* In this case, must reverse order of args
     so that we compute and pust the last arg first.  */
  for (p = actparms, i = num_actuals - 1; p; p = TREE_CHAIN (p), i--)
    argvec[i] = p;
#else
  for (p = actparms, i = 0; p; p = TREE_CHAIN (p), i++)
    argvec[i] = p;
#endif
  
  for (i = 0; i < num_actuals; i++)
    {
      register tree p = argvec[i];
      register tree pval = TREE_VALUE (p);

      /* Push the next argument.  Note that it has already been converted
	 if necessary to the type that the called function expects.  */

      if (TREE_CODE (pval) == ERROR_MARK)
	;
      else if (TYPE_MODE (TREE_TYPE (pval)) != BLKmode)
	{
	  register int size, used;

	  /* Argument is a scalar.
	     Push it, and if its size is less than the
	     amount of space allocated to it,
	     also bump stack pointer by the additional space.
	     Note that in C the default argument promotions
	     will prevent such mismatches.  */

	  used = size = GET_MODE_SIZE (TYPE_MODE (TREE_TYPE (pval)));
	  /* Compute how much space the push instruction will push.
	     On many machines, pushing a byte will advance the stack
	     pointer by a halfword.  */
	  size = PUSH_ROUNDING (size);
	  /* Compute how much space the argument should get:
	     round up to a multiple of the alignment for arguments.  */
	  used = (((size + PARM_BOUNDARY / BITS_PER_UNIT - 1)
		   / (PARM_BOUNDARY / BITS_PER_UNIT))
		  * (PARM_BOUNDARY / BITS_PER_UNIT));

#ifdef STACK_GROWS_DOWNWARD
	  if (size != used)
	    anti_adjust_stack (gen_rtx (CONST_INT, VOIDmode,
					used - size));
#endif

	  emit_push_insn (expand_expr (pval, 0, VOIDmode, 0),
			  TYPE_MODE (TREE_TYPE (pval)), 0, 0);

#ifndef STACK_GROWS_DOWNWARD
	  if (size != used)
	    anti_adjust_stack (gen_rtx (CONST_INT, VOIDmode,
					used - size));
#endif

	  /* Account for the space thus used.  */
	  args_size += used;
	  current_args_size += used;
	}
      else
	{
	  register rtx tem = expand_expr (pval, 0, VOIDmode, 0);
	  register tree size = size_in_bytes (TREE_TYPE (pval));
	  register tree used;
	  register int excess;

	  /* Pushing a nonscalar.  Round its size up to a multiple
	     of the allocation unit for arguments.  This part works
	     on variable-size objects since SIZE and USED are rtx's.  */

	  used = convert_units (convert_units (size, BITS_PER_UNIT, PARM_BOUNDARY),
				PARM_BOUNDARY, BITS_PER_UNIT);

	  if (!TREE_LITERAL (used))
	    abort ();

	  excess = TREE_INT_CST_LOW (used) - PUSH_ROUNDING (TREE_INT_CST_LOW (size));

#ifdef STACK_GROWS_DOWNWARD
	  if (excess != 0)
	    anti_adjust_stack (gen_rtx (CONST_INT, VOIDmode, excess));
#endif

	  emit_push_insn (tem, TYPE_MODE (TREE_TYPE (pval)),
			  expand_expr (size, 0, VOIDmode, 0),
			  (TYPE_ALIGN (TREE_TYPE (pval))
			   / BITS_PER_UNIT));

#ifndef STACK_GROWS_DOWNWARD
	  if (excess != 0)
	    anti_adjust_stack (gen_rtx (CONST_INT, VOIDmode, excess));
#endif
	  args_size += TREE_INT_CST_LOW (used);
	  current_args_size += TREE_INT_CST_LOW (used);
	}
    }

  /* Perform postincrements before actually calling the function.  */
  emit_queue ();

  /* Pass the function the address in which to return a structure value.  */
  if (structure_value_addr)
    {
      register rtx reg = gen_rtx (REG, Pmode, STRUCT_VALUE_REGNUM);
      emit_move_insn (reg, structure_value_addr);
      emit_insn (gen_rtx (USE, VOIDmode, reg));
    }

  gen_call_1 (expand_expr (TREE_OPERAND (exp, 0), 0, VOIDmode, 0),
/* ??? For Pascal, this must pass a context to get the static chain from
   in certain cases.  */
	      0,
	      args_size / GET_MODE_SIZE (SImode), args_size);

/* ???  Nothing has been done here to record control flow
   when contained functions can do nonlocal gotos.  */

  /* If value type not void, return an rtx for the value.  */

  if (TYPE_MODE (TREE_TYPE (exp)) == VOIDmode)
    return 0;

  if (structure_value_addr)
    {
      if (target)
	return target;
      return gen_rtx (MEM, BLKmode, structure_value_addr);
    }
  
  if (target && GET_MODE (target) == TYPE_MODE (TREE_TYPE (exp)))
    {
      copy_function_value (target);
      return target;
    }
  return function_value (TYPE_MODE (TREE_TYPE (exp)));
}

/* Expand conditional expressions.  */

/* Generate code to evaluate EXP and jump to LABEL if the value is zero.
   LABEL is an rtx of code CODE_LABEL, in this function and all the
   functions here.  */

jumpifnot (exp, label)
     tree exp;
     rtx label;
{
  do_jump (exp, label, 0);
}

/* Generate code to evaluate EXP and jump to LABEL if the value is nonzero.  */

jumpif (exp, label)
     tree exp;
     rtx label;
{
  do_jump (exp, 0, label);
}

/* Generate code to evaluate EXP and jump to IF_FALSE_LABEL if
   the result is zero, or IF_TRUE_LABEL if the result is one.
   Either of IF_FALSE_LABEL and IF_TRUE_LABEL may be zero,
   meaning fall through in that case.

   This function is responsible for optimizing cases such as
   &&, || and comparison operators in EXP.  */

do_jump (exp, if_false_label, if_true_label)
     tree exp;
     rtx if_false_label, if_true_label;
{
  register enum tree_code code = TREE_CODE (exp);
  /* Some cases need to create a label to jump to
     in order to properly fall through.
     These cases set DROP_THROUGH_LABEL nonzero.  */
  rtx drop_through_label = 0;
  rtx temp;
  rtx comparison = 0;

  emit_queue ();

  switch (code)
    {
    case ERROR_MARK:
      break;

    case INTEGER_CST:
      temp = integer_zerop (exp) ? if_false_label : if_true_label;
      if (temp)
	emit_jump (temp);
      break;

    case ADDR_EXPR:
      /* The address of something can never be zero.  */
      if (if_true_label)
	emit_jump (if_true_label);
      break;

    case NOP_EXPR:
      do_jump (TREE_OPERAND (exp, 0), if_false_label, if_true_label);
      break;

    case TRUTH_NOT_EXPR:
      do_jump (TREE_OPERAND (exp, 0), if_true_label, if_false_label);
      break;

    case TRUTH_ANDIF_EXPR:
      if (if_false_label == 0)
	if_false_label = drop_through_label = gen_label_rtx ();
      do_jump (TREE_OPERAND (exp, 0), if_false_label, 0);
      do_jump (TREE_OPERAND (exp, 1), if_false_label, if_true_label);
      break;

    case TRUTH_ORIF_EXPR:
      if (if_true_label == 0)
	if_true_label = drop_through_label = gen_label_rtx ();
      do_jump (TREE_OPERAND (exp, 0), 0, if_true_label);
      do_jump (TREE_OPERAND (exp, 1), if_false_label, if_true_label);
      break;

    case COMPOUND_EXPR:
      expand_expr (TREE_OPERAND (exp, 0), 0, VOIDmode, 0);
      emit_queue ();
      do_jump (TREE_OPERAND (exp, 1), if_false_label, if_true_label);
      break;

    case COND_EXPR:
      {
	register rtx label1 = gen_label_rtx ();
	drop_through_label = gen_label_rtx ();
	do_jump (TREE_OPERAND (exp, 0), label1, 0);
	/* Now the THEN-expression.  */
	do_jump (TREE_OPERAND (exp, 1),
		 if_false_label ? if_false_label : drop_through_label,
		 if_true_label ? if_true_label : drop_through_label);
	emit_label (label1);
	/* Now the ELSE-expression.  */
	do_jump (TREE_OPERAND (exp, 2),
		 if_false_label ? if_false_label : drop_through_label,
		 if_true_label ? if_true_label : drop_through_label);
      }
      break;

    case EQ_EXPR:
      comparison = compare (exp, EQ, EQ, EQ, EQ);
      break;

    case NE_EXPR:
      comparison = compare (exp, NE, NE, NE, NE);
      break;

    case LT_EXPR:
      comparison = compare (exp, LT, LTU, GT, GTU);
      break;

    case LE_EXPR:
      comparison = compare (exp, LE, LEU, GE, GEU);
      break;

    case GT_EXPR:
      comparison = compare (exp, GT, GTU, LT, LTU);
      break;

    case GE_EXPR:
      comparison = compare (exp, GE, GEU, LE, LEU);
      break;

    default:
      temp = expand_expr (exp, 0, VOIDmode, 0);
      do_pending_stack_adjust ();
      emit_cmp_insn (temp, gen_rtx (CONST_INT, GET_MODE (temp), 0),
		     0, 0);

      if (if_true_label)
	emit_jump_insn (gen_bne (if_true_label));
      if (if_false_label)
	{
	  if (if_true_label)
	    emit_jump (if_false_label);
	  else
	    emit_jump_insn (gen_beq (if_false_label));
	}
    }

  /* If COMPARISON is nonzero here, it is an rtx that can be substituted
     straight into a conditional jump instruction as the jump condition.
     Otherwise, all the work has been done already.  */

  if (comparison)
    if (if_true_label)
      {
	emit_jump_insn (gen_rtx (SET, VOIDmode, pc_rtx,
				 gen_rtx (IF_THEN_ELSE, VOIDmode, comparison,
					  gen_rtx (LABEL_REF, VOIDmode,
						   if_true_label),
					  pc_rtx)));
	if (if_false_label)
	  emit_jump (if_false_label);
      }
    else if (if_false_label)
      {
	emit_jump_insn (gen_rtx (SET, VOIDmode, pc_rtx,
				 gen_rtx (IF_THEN_ELSE, VOIDmode, comparison,
					  pc_rtx,
					  gen_rtx (LABEL_REF, VOIDmode,
						   if_false_label))));
      }

  if (drop_through_label)
    emit_label (drop_through_label);
}

/* Generate code for a comparison expression EXP
   (including code to compute the values to be compared)
   and set (CC0) according to the result.
   SIGNED_FORWARD should be the rtx operation for this comparison for
   signed data; UNSIGNED_FORWARD, likewise for use if data is unsigned.
   SIGNED_REVERSE and UNSIGNED_REVERSE are used if it is desirable
   to interchange the operands for the compare instruction.

   We force a stack adjustment unless there are currently
   things pushed on the stack that aren't yet used.  */

static rtx
compare (exp, signed_forward, unsigned_forward,
	 signed_reverse, unsigned_reverse)
     register tree exp;
     enum rtx_code signed_forward, unsigned_forward;
     enum rtx_code signed_reverse, unsigned_reverse;
{
  register rtx op0 = expand_expr (TREE_OPERAND (exp, 0), 0, VOIDmode, 0);
  register rtx op1 = expand_expr (TREE_OPERAND (exp, 1), 0, VOIDmode, 0);
  register enum machine_mode mode = GET_MODE (op0);
  int unsignedp;

  /* If one operand is 0, make it the second one.  */

  if (op0 == const0_rtx || op0 == fconst0_rtx || op0 == dconst0_rtx)
    {
      rtx tem = op0;
      op0 = op1;
      op1 = tem;
      signed_forward = signed_reverse;
      unsigned_forward = unsigned_reverse;
    }

  if (force_mem)
    {
      op0 = force_not_mem (op0);
      op1 = force_not_mem (op1);
    }

  do_pending_stack_adjust ();

  unsignedp = (type_unsigned_p (TREE_TYPE (TREE_OPERAND (exp, 0)))
	       || type_unsigned_p (TREE_TYPE (TREE_OPERAND (exp, 1))));

  emit_cmp_insn (op0, op1,
		 (mode == BLKmode) ? expr_size (TREE_OPERAND (exp, 0)) : 0,
		 unsignedp);

  return gen_rtx ((unsignedp ? unsigned_forward : signed_forward),
		  VOIDmode, cc0_rtx, const0_rtx);
}

/* Like compare but expects the values to compare as two rtx's.
   The decision as to signed or unsigned comparison must be made by the caller.
   BLKmode is not allowed.  */

static rtx
compare1 (op0, op1, forward_op, reverse_op, unsignedp)
     register rtx op0, op1;
     enum rtx_code forward_op, reverse_op;
     int unsignedp;
{
  register enum machine_mode mode = GET_MODE (op0);

  /* If one operand is 0, make it the second one.  */

  if (op0 == const0_rtx || op0 == fconst0_rtx || op0 == dconst0_rtx)
    {
      rtx tem = op0;
      op0 = op1;
      op1 = tem;
      forward_op = reverse_op;
    }

  if (force_mem)
    {
      op0 = force_not_mem (op0);
      op1 = force_not_mem (op1);
    }

  do_pending_stack_adjust ();

  emit_cmp_insn (op0, op1, 0, unsignedp);

  return gen_rtx (forward_op, VOIDmode, cc0_rtx, const0_rtx);
}

/* Generate code to jump to LABEL if OP1 and OP2 are equal.  */

void
do_jump_if_equal (op1, op2, label)
     rtx op1, op2, label;
{
  emit_cmp_insn (op1, op2, 0);
  emit_jump_insn (gen_beq (label));
}

/* Generate code to calculate EXP using a store-flag instruction
   and return an rtx for the result.
   If TARGET is nonzero, store the result there if convenient.

   Return zero if there is no suitable set-flag instruction
   available on this machine.  */

static rtx
do_store_flag (exp, target)
     tree exp;
     rtx target;
{
  register enum tree_code code = TREE_CODE (exp);
  register rtx comparison = 0;

  if (target == 0 || GET_MODE (target) != SImode)
    target = gen_reg_rtx (SImode);

  switch (code)
    {
#ifdef HAVE_seqsi
    case EQ_EXPR:
      if (HAVE_seqsi)
	comparison = compare (exp, EQ, EQ, EQ, EQ);
      break;
#endif

#ifdef HAVE_snesi
    case NE_EXPR:
      if (HAVE_snesi)
	comparison = compare (exp, NE, NE, NE, NE);
      break;
#endif

#if defined (HAVE_sltsi) && defined (HAVE_sltusi) && defined (HAVE_sgtsi) && defined (HAVE_sgtusi)
    case LT_EXPR:
      if (HAVE_sltsi && HAVE_sltusi && HAVE_sgtsi && HAVE_sgtusi)
	comparison = compare (exp, LT, LTU, GT, GTU);
      break;

    case GT_EXPR:
      if (HAVE_sltsi && HAVE_sltusi && HAVE_sgtsi && HAVE_sgtusi)
	comparison = compare (exp, GT, GTU, LT, LTU);
      break;
#endif

#if defined (HAVE_slesi) && defined (HAVE_sleusi) && defined (HAVE_sgesi) && defined (HAVE_sgeusi)
    case LE_EXPR:
      if (HAVE_slesi && HAVE_sleusi && HAVE_sgesi && HAVE_sgeusi)
	comparison = compare (exp, LE, LEU, GE, GEU);
      break;

    case GE_EXPR:
      if (HAVE_slesi && HAVE_sleusi && HAVE_sgesi && HAVE_sgeusi)
	comparison = compare (exp, GE, GEU, LE, LEU);
      break;
#endif
    }
  if (comparison == 0)
    return 0;

  emit_insn (gen_rtx (SET, VOIDmode, target, comparison));
  expand_bit_and (GET_MODE (target), target, const1_rtx, target);
  return target;
}

/* Generate a tablejump instruction (used for switch statements).  */

#ifdef HAVE_tablejump

/* INDEX is the value being switched on, with the lowest value
   in the table already subtracted.
   RANGE is the length of the jump table.
   TABLE_LABEL is a CODE_LABEL rtx for the table itself.
   DEFAULT_LABEL is a CODE_LABEL rtx to jump to if the
   index value is out of range.  */

void
do_tablejump (index, range, table_label, default_label)
     rtx index, range, table_label, default_label;
{
  register rtx temp;

  emit_cmp_insn (index, const0_rtx, 0);
  emit_jump_insn (gen_blt (default_label));
  emit_cmp_insn (range, index, 0);
  emit_jump_insn (gen_blt (default_label));
  index = memory_address (CASE_VECTOR_MODE,
			  gen_rtx (PLUS, Pmode,
				   gen_rtx (LABEL_REF, VOIDmode, table_label),
				   gen_rtx (MULT, Pmode, index,
					    gen_rtx (CONST_INT, VOIDmode,
						     GET_MODE_SIZE (CASE_VECTOR_MODE)))));
  temp = gen_reg_rtx (CASE_VECTOR_MODE);
  convert_move (temp, gen_rtx (MEM, CASE_VECTOR_MODE, index), 0);

  emit_jump_insn (gen_tablejump (temp));
}

#endif /* HAVE_tablejump */}
