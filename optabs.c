/* Expand the basic unary and binary arithmetic operations, for GNU compiler.
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
#include "insn-config.h"
#include "recog.h"

/* Each optab contains info on how this target machine
   can perform a particular operation
   for all sizes and kinds of operands.

   The operation to be performed is often specified
   by passing one of these optabs as an argument.

   See expr.h for documentation of these optabs.  */

optab add_optab;
optab sub_optab;
optab smul_optab;
optab umul_optab;
optab smul_widen_optab;
optab umul_widen_optab;
optab sdiv_optab;
optab sdivmod_optab;
optab udiv_optab;
optab udivmod_optab;
optab smod_optab;
optab umod_optab;
optab flodiv_optab;
optab and_optab;
optab andcb_optab;
optab ior_optab;
optab xor_optab;
optab ashl_optab;
optab lshr_optab;
optab lshl_optab;
optab ashr_optab;
optab rotl_optab;
optab rotr_optab;

optab mov_optab;
optab movstrict_optab;

optab neg_optab;
optab abs_optab;
optab one_cmpl_optab;

optab cmp_optab;
optab tst_optab;

/* Generate code to perform an operation specified by BINOPTAB
   on operands OP0 and OP1, with result having machine-mode MODE.

   UNSIGNEDP is for the case where we have to widen the operands
   to perform the operation.  It says to use zero-extension.

   If TARGET is nonzero, the value
   is generated there, if it is convenient to do so.
   In all cases an rtx is returned for the locus of the value;
   this may or may not be TARGET.  */

rtx
expand_binop (mode, binoptab, op0, op1, target, unsignedp, methods)
     enum machine_mode mode;
     optab binoptab;
     rtx op0, op1;
     rtx target;
     int unsignedp;
     enum optab_methods methods;
{
  register rtx temp;
  int target_is_not_an_operand = 0;

  /* We may get better code by generating the result in a register
     when the target is not one of the operands.  */
  if (target && ! rtx_equal_p (target, op1) && ! rtx_equal_p (target, op0))
    target_is_not_an_operand = 1;

  op0 = protect_from_queue (op0, 0);
  op1 = protect_from_queue (op1, 0);
  if (target)
    target = protect_from_queue (target, 1);

  if (force_mem)
    {
      op0 = force_not_mem (op0);
      op1 = force_not_mem (op1);
    }

  /* If operation is commutative,
     try to make the first operand a register.
     Even better, try to make it the same as the target.
     Also try to make the last operand a constant.  */
  if (binoptab == add_optab
      || binoptab == and_optab
      || binoptab == ior_optab
      || binoptab == xor_optab
      || binoptab == smul_optab
      || binoptab == umul_optab
      || binoptab == smul_widen_optab
      || binoptab == umul_widen_optab)
    {
      if (((target == 0 || GET_CODE (target) == REG)
	   ? ((GET_CODE (op1) == REG
	       && GET_CODE (op0) != REG)
	      || target == op1)
	   : rtx_equal_p (op1, target))
	  ||
	  GET_CODE (op0) == CONST_INT)
	{
	  temp = op1;
	  op1 = op0;
	  op0 = temp;
	}
    }

  /* If we can do it with a three-operand insn, do so.  */

  if (binoptab[(int) mode].insn_code != CODE_FOR_nothing)
    {
      if (target)
	temp = target;
      else
	temp = gen_reg_rtx (mode);

      if (GET_MODE (op0) != VOIDmode
	  && GET_MODE (op0) != insn_operand_mode[(int) binoptab[(int) mode].insn_code][1])
	op0 = convert_to_mode (insn_operand_mode[(int) binoptab[(int) mode].insn_code][1], op0);

      if (GET_MODE (op1) != VOIDmode
	  && GET_MODE (op1) != insn_operand_mode[(int) binoptab[(int) mode].insn_code][2])
	op1 = convert_to_mode (insn_operand_mode[(int) binoptab[(int) mode].insn_code][2], op1);

      emit_insn (GEN_FCN (binoptab[(int) mode].insn_code) (temp, op0, op1));
      return temp;
    }
  /* Otherwise, do it with a two-operand insn
     plus a move insn if the target is not an operand.  */

  /* It can't be open-coded in this mode.
     Use a library call if one is available and caller says that's ok.  */

  if (binoptab[(int) mode].lib_call
      && (methods == OPTAB_LIB || methods == OPTAB_LIB_WIDEN))
    {
      emit_library_call (gen_rtx (SYMBOL_REF, Pmode,
				  binoptab[(int) mode].lib_call),
			 2, op0, mode, op1, mode);
      target = temp = function_value (mode);
      return temp;
    }

  /* It can't be done in this mode.  Can we do it in a wider mode?  */

  if (! (methods == OPTAB_WIDEN || methods == OPTAB_LIB_WIDEN))
    return 0;			/* Caller says, don't even try.  */

  /* Compute the value of METHODS to pass to recursive calls.
     Don't allow widening to be tried recursively.  */

  methods = (methods == OPTAB_LIB_WIDEN ? OPTAB_LIB : OPTAB_DIRECT);

  if ((mode == HImode || mode == QImode)
      && (binoptab[(int) SImode].insn_code != CODE_FOR_nothing
	  || (methods == OPTAB_LIB && binoptab[(int) SImode].lib_call)))
    {
      temp = gen_reg_rtx (SImode);
      convert_move (temp, op0, unsignedp);
      op0 = temp;
      temp = gen_reg_rtx (SImode);
      convert_move (temp, op1, unsignedp);
      op1 = temp;

      target = expand_binop (SImode, binoptab, op0, op1, 0,
			     unsignedp, methods);
      return gen_lowpart (mode, target);
    }
  if ((mode == HImode || mode == QImode || mode == SImode)
      && (binoptab[(int) DImode].insn_code != CODE_FOR_nothing
	  || (methods == OPTAB_LIB && binoptab[(int) DImode].lib_call)))
    {
      temp = gen_reg_rtx (DImode);
      convert_move (temp, op0, unsignedp);
      op0 = temp;
      temp = gen_reg_rtx (DImode);
      convert_move (temp, op1, unsignedp);
      op1 = temp;

      target = expand_binop (DImode, binoptab, op0, op1, 0,
			     unsignedp, methods);
      return gen_lowpart (mode, target);
    }
  if (mode == SFmode
      && (binoptab[(int) DFmode].insn_code != CODE_FOR_nothing
	  || (methods == OPTAB_LIB && binoptab[(int) DFmode].lib_call)))
    {
      temp = gen_reg_rtx (DFmode);
      convert_move (temp, op0, 0);
      op0 = temp;
      temp = gen_reg_rtx (DFmode);
      convert_move (temp, op1, 0);
      op1 = temp;

      temp = expand_binop (DFmode, binoptab, op0, op1, 0, 0, methods);
      if (target == 0)
	target = gen_reg_rtx (SFmode);
      convert_move (target, temp, 0);
      return target;
    }
  return 0;
}

/* Generate code to perform an operation specified by BINOPTAB
   on operands OP0 and OP1, with two results to TARG1 and TARG2.
   We assume that the order of the operands for the instruction
   is TARG0, OP0, OP1, TARG1, which would fit a pattern like
   [(set TARG0 (operate OP0 OP1)) (set TARG1 (operate ...))].

   Either TARG0 or TARG1 may be zero, but what that means is that
   that result is not actually wanted.  We will generate it into
   a dummy pseudo-reg and discard it.  They may not both be zero.

   Returns 1 if this operation can be performed; 0 if not.  */

int
expand_twoval_binop (binoptab, op0, op1, targ0, targ1, unsignedp)
     optab binoptab;
     rtx op0, op1;
     rtx targ0, targ1;
     int unsignedp;
{
  register rtx temp;
  enum machine_mode mode = GET_MODE (targ0 ? targ0 : targ1);

  op0 = protect_from_queue (op0, 0);
  op1 = protect_from_queue (op1, 0);

  if (force_mem)
    {
      op0 = force_not_mem (op0);
      op1 = force_not_mem (op1);
    }

  if (targ0)
    targ0 = protect_from_queue (targ0, 1);
  else
    targ0 = gen_reg_rtx (mode);
  if (targ1)
    targ1 = protect_from_queue (targ1, 1);
  else
    targ1 = gen_reg_rtx (mode);

  if (binoptab[(int) mode].insn_code != CODE_FOR_nothing)
    {
      emit_insn (GEN_FCN (binoptab[(int) mode].insn_code)
		 (targ0, op0, op1, targ1));
      return 1;
    }

  /* It can't be done in this mode.  Can we do it in a wider mode?  */

  if ((mode == HImode || mode == QImode)
      && binoptab[(int) SImode].insn_code != CODE_FOR_nothing)
    {
      expand_twoval_binop_convert (binoptab, SImode, op0, op1,
				   targ0, targ1, unsignedp);
      return 1;
    }
  if ((mode == HImode || mode == QImode || mode == SImode)
      && binoptab[(int) DImode].insn_code != CODE_FOR_nothing)
    {
      expand_twoval_binop_convert (binoptab, DImode, op0, op1,
				   targ0, targ1, unsignedp);
      return 1;
    }
  if (mode == SFmode && binoptab[(int) DFmode].insn_code != CODE_FOR_nothing)
    {
      expand_twoval_binop_convert (binoptab, DFmode, op0, op1,
				   targ0, targ1, unsignedp);
      return 1;
    }
  return 0;
}

int
expand_twoval_binop_convert (binoptab, mode, op0, op1, targ0, targ1, unsignedp)
     register optab binoptab;
     register rtx op0, op1, targ0, targ1;
     int unsignedp;
{
  register rtx t0 = gen_reg_rtx (SImode);
  register rtx t1 = gen_reg_rtx (SImode);
  register rtx temp;

  temp = gen_reg_rtx (SImode);
  convert_move (temp, op0, unsignedp);
  op0 = temp;
  temp = gen_reg_rtx (SImode);
  convert_move (temp, op1, unsignedp);
  op1 = temp;

  expand_twoval_binop (binoptab, op0, op1, t0, t1, unsignedp);
  convert_move (targ0, t0, unsignedp);
  convert_move (targ1, t1, unsignedp);
  return 1;
}

/* Generate code to perform an operation specified by UNOPTAB
   on operand OP0, with result having machine-mode MODE.

   UNSIGNEDP is for the case where we have to widen the operands
   to perform the operation.  It says to use zero-extension.

   If TARGET is nonzero, the value
   is generated there, if it is convenient to do so.
   In all cases an rtx is returned for the locus of the value;
   this may or may not be TARGET.  */

rtx
expand_unop (mode, unoptab, op0, target, unsignedp)
     enum machine_mode mode;
     optab unoptab;
     rtx op0;
     rtx target;
     int unsignedp;
{
  register rtx temp;

  op0 = protect_from_queue (op0, 0);

  if (force_mem)
    {
      op0 = force_not_mem (op0);
    }

  if (target)
    target = protect_from_queue (target, 1);

  if (unoptab[(int) mode].insn_code != CODE_FOR_nothing)
    {
      if (target)
	temp = target;
      else
	temp = gen_reg_rtx (mode);

      if (GET_MODE (op0) != VOIDmode
	  && GET_MODE (op0) != insn_operand_mode[(int) unoptab[(int) mode].insn_code][1])
	op0 = convert_to_mode (insn_operand_mode[(int) unoptab[(int) mode].insn_code][1], op0);

      emit_insn (GEN_FCN (unoptab[(int) mode].insn_code) (temp, op0));
      return temp;
    }
  else if (unoptab[(int) mode].lib_call)
    {
      emit_library_call (gen_rtx (SYMBOL_REF, Pmode,
				  unoptab[(int) mode].lib_call),
			 1, op0, mode);
      target = temp = function_value (mode);
      return temp;
    }

  /* It can't be done in this mode.  Can we do it in a wider mode?  */

  if ((mode == HImode || mode == QImode)
      && (unoptab[(int) SImode].insn_code != CODE_FOR_nothing
	  || unoptab[(int) SImode].lib_call))
    {
      temp = gen_reg_rtx (SImode);
      convert_move (temp, op0, unsignedp);
      op0 = temp;

      target = expand_unop (SImode, unoptab, op0, 0, unsignedp);
      return gen_lowpart (mode, target);
    }
  if ((mode == HImode || mode == QImode || mode == SImode)
      && (unoptab[(int) DImode].insn_code != CODE_FOR_nothing
	  || unoptab[(int) DImode].lib_call))
    {
      temp = gen_reg_rtx (DImode);
      convert_move (temp, op0, unsignedp);
      op0 = temp;

      target = expand_unop (DImode, unoptab, op0, unsignedp);
      return gen_lowpart (mode, target);
    }
  if (mode == SFmode
      && (unoptab[(int) DFmode].insn_code != CODE_FOR_nothing
	  || unoptab[(int) DFmode].lib_call))
    {
      temp = gen_reg_rtx (DFmode);
      convert_move (temp, op0, 0);
      op0 = temp;

      temp = expand_unop (DFmode, unoptab, op0, 0, 0);
      if (target == 0)
	target = gen_reg_rtx (SFmode);
      convert_move (target, temp, 0);
      return target;
    }

  return 0;
}

/* Generate code to store zero in X.  */

void
emit_clr_insn (x)
     rtx x;
{
  emit_move_insn (x, const0_rtx);
}

/* Generate code to store 1 in X
   assuming it contains zero beforehand.  */

void
emit_0_to_1_insn (x)
     rtx x;
{
  emit_move_insn (x, const1_rtx);
}

/* Generate code to compare X with Y
   so that the condition codes are set.
   If they have mode BLKmode, then SIZE specifies the size of block.  */

void
emit_cmp_insn (x, y, size, unsignedp)
     rtx x, y;
     rtx size;
     int unsignedp;
{
  enum machine_mode mode = GET_MODE (x);
  if (mode == VOIDmode) mode = GET_MODE (y);
  /* They could both be VOIDmode if both args are immediate constants,
     but we should fold that at an earlier stage.
     With no special code here, this will call abort,
     reminding the programmer to implement such folding.  */

  emit_queue ();
  x = protect_from_queue (x, 0);
  y = protect_from_queue (y, 0);

  if (mode != BLKmode && force_mem)
    {
      x = force_not_mem (x);
      y = force_not_mem (y);
    }

  if (mode == BLKmode)
    {
      if (size == 0)
	abort ();
#ifdef HAVE_cmpstrsi
      if (HAVE_cmpstrsi)
	emit_insn (gen_cmpstr (x, y, convert_to_mode (SImode, size)));
      else
#else
	{
	  emit_library_call (gen_rtx (SYMBOL_REF, Pmode, "bcmp"),
			     3, x, Pmode, y, Pmode, size, Pmode);
	  emit_cmp_insn (hard_function_value (SImode), const0_rtx, 0, 0);
	}
#endif
    }
  else if ((y == const0_rtx || y == fconst0_rtx || y == dconst0_rtx)
	   && tst_optab[(int) mode].insn_code != CODE_FOR_nothing)
    emit_insn (GEN_FCN (tst_optab[(int) mode].insn_code) (x));
  else if (cmp_optab[(int) mode].insn_code != CODE_FOR_nothing)
    emit_insn (GEN_FCN (cmp_optab[(int) mode].insn_code) (x, y));
  else if ((mode == QImode || mode == HImode)
	   && cmp_optab[(int) SImode].insn_code != CODE_FOR_nothing)
    {
      x = convert_to_mode (SImode, x, unsignedp);
      y = convert_to_mode (SImode, y, unsignedp);
      emit_cmp_insn (x, y, 0, unsignedp);
    }
  else if ((mode == QImode || mode == HImode || mode == SImode)
	   && cmp_optab[(int) DImode].insn_code != CODE_FOR_nothing)
    {
      x = convert_to_mode (DImode, x, unsignedp);
      y = convert_to_mode (DImode, y, unsignedp);
      emit_cmp_insn (x, y, 0, unsignedp);
    }
  else if (mode == SFmode
	   && cmp_optab[(int) DFmode].insn_code != CODE_FOR_nothing)
    {
      x = convert_to_mode (DFmode, x, unsignedp);
      y = convert_to_mode (DFmode, y, unsignedp);
      emit_cmp_insn (x, y, 0, unsignedp);
    }
  else if (cmp_optab[(int) mode].lib_call)
    {
      emit_library_call (gen_rtx (SYMBOL_REF, Pmode,
				  cmp_optab[(int) mode].lib_call),
			 2, x, mode, y, mode);
      emit_cmp_insn (hard_function_value (SImode), const0_rtx, 0, 0);
    }
  else if ((mode == QImode || mode == HImode)
	   && (cmp_optab[(int) SImode].insn_code != CODE_FOR_nothing
	       || cmp_optab[(int) SImode].lib_call != 0))
    {
      x = convert_to_mode (SImode, x, unsignedp);
      y = convert_to_mode (SImode, y, unsignedp);
      emit_cmp_insn (x, y, 0, unsignedp);
    }
  else if ((mode == QImode || mode == HImode || mode == SImode)
	   && (cmp_optab[(int) DImode].insn_code != CODE_FOR_nothing
	       || cmp_optab[(int) DImode].lib_call != 0))
    {
      x = convert_to_mode (DImode, x, unsignedp);
      y = convert_to_mode (DImode, y, unsignedp);
      emit_cmp_insn (x, y, 0, unsignedp);
    }
  else if (mode == SFmode
	   && (cmp_optab[(int) DFmode].insn_code != CODE_FOR_nothing
	       || cmp_optab[(int) DFmode].lib_call != 0))
    {
      x = convert_to_mode (DFmode, x, unsignedp);
      y = convert_to_mode (DFmode, y, unsignedp);
      emit_cmp_insn (x, y, 0, unsignedp);
    }
  else
    abort ();
}

/* Generate code to compare X with zero
   so that the condition codes are set.  */

void
emit_tst_insn (x)
     rtx x;
{
  enum machine_mode mode = GET_MODE (x);

  emit_queue ();
  x = protect_from_queue (x, 0);

  if (tst_optab[(int) mode].insn_code != CODE_FOR_nothing)
    emit_insn (GEN_FCN (tst_optab[(int) mode].insn_code) (x));
  else if (cmp_optab[(int) mode].lib_call)
    {
      emit_library_call (gen_rtx (SYMBOL_REF, Pmode,
				  cmp_optab[(int) mode].lib_call),
			 2, x, mode, const0_rtx, mode);
      emit_cmp_insn (hard_function_value (SImode), const0_rtx, 0, 0);
    }
  else
    abort ();
}

/* These three functions generate an insn body and return it
   rather than emitting the insn.

   They do not protect from queued increments,
   because they may be used 1) in protect_from_queue itself
   and 2) in other passes where there is no queue.  */

/* Generate and return an insn body to add Y to X.  */

rtx
gen_add2_insn (x, y)
     rtx x, y;
{
  return (GEN_FCN (add_optab[(int) GET_MODE (x)].insn_code)
	  (x, copy_rtx (x), y));
}

/* Generate and return an insn body to subtract Y from X.  */

rtx
gen_sub2_insn (x, y)
     rtx x, y;
{
  return (GEN_FCN (sub_optab[(int) GET_MODE (x)].insn_code)
	  (x, copy_rtx (x), y));
}

/* Generate the body of an instruction to copy Y into X.  */

rtx
gen_move_insn (x, y)
     rtx x, y;
{
  return (GEN_FCN (mov_optab[(int) GET_MODE (x)].insn_code) (x, y));
}

/* can_fix_p and can_float_p say whether the target machine
   can directly convert a given fixed point type to
   a given floating point type, or vice versa.  */
static rtxfun fixtab[2][2];
static rtxfun floattab[2][2];

rtxfun
can_fix_p (fixmode, fltmode)
     enum machine_mode fltmode, fixmode;
{
  return fixtab[fltmode != SFmode][fixmode != SImode];
}

rtxfun
can_float_p (fltmode, fixmode)
     enum machine_mode fixmode, fltmode;
{
  return floattab[fltmode != SFmode][fixmode != SImode];
}

void
init_fixtab ()
{
#ifdef HAVE_fixsfsi2
  if (HAVE_fixsfsi2)
    fixtab[0][0] = gen_fixsfsi2;
#endif
#ifdef HAVE_fixsfdi2
  if (HAVE_fixsfdi2)
    fixtab[0][1] = gen_fixsfdi2;
#endif
#ifdef HAVE_fixdfsi2
  if (HAVE_fixdfsi2)
    fixtab[1][0] = gen_fixdfsi2;
#endif
#ifdef HAVE_fixdfdi2
  if (HAVE_fixdfdi2)
    fixtab[1][1] = gen_fixdfdi2;
#endif
}

void
init_floattab ()
{
#ifdef HAVE_floatsisf2
  if (HAVE_floatsisf2)
    floattab[0][0] = gen_floatsisf2;
#endif
#ifdef HAVE_floatdisf2
  if (HAVE_floatdisf2)
    floattab[0][1] = gen_floatdisf2;
#endif
#ifdef HAVE_floatsidf2
  if (HAVE_floatsidf2)
    floattab[1][0] = gen_floatsidf2;
#endif
#ifdef HAVE_floatdidf2
  if (HAVE_floatdidf2)
    floattab[1][1] = gen_floatdidf2;
#endif
}

/* Generate code to convert FROM to floating point
   and store in TO.  FROM must be fixed point.  */

void
expand_float (to, from)
     rtx to, from;
{
  register rtxfun fun;
  register rtx target;

  to = protect_from_queue (to, 1);
  from = protect_from_queue (from, 0);

  if (force_mem)
    {
      from = force_not_mem (from);
    }

  if (fun = can_float_p (GET_MODE (to), GET_MODE (from)))
    {
      emit_insn ((*fun) (to, from));
      return;
    }

  if (GET_MODE (to) == SFmode && (fun = can_float_p (GET_MODE (from), DFmode)))
    {
      register rtx temp = gen_reg_rtx (DFmode);
      emit_insn ((*fun) (temp, from));
      convert_move (to, temp, 0);
      return;
    }

  if (GET_MODE (from) != DImode)
    {
      register rtx tem = gen_reg_rtx (DImode);
      convert_move (tem, from, 0);
      from = tem;
    }

  if (fun = can_float_p (GET_MODE (to), GET_MODE (from)))
    {
      emit_insn ((*fun) (to, from));
      return;
    }

  if (fun = can_float_p (DFmode, DImode))
    {
      target = gen_reg_rtx (DFmode);
      emit_insn ((*fun) (target, from));
    }
  else
    {
      emit_library_call (gen_rtx (SYMBOL_REF, Pmode, "floatdidf"),
			 1, from, DImode);
      target = function_value (DFmode);
    }

  if (GET_MODE (to) == DFmode)
    emit_move_insn (to, target);
  else
    convert_move (to, target, 0);
}

/* Generate code to convert FROM to fixed point
   and store in TO.  FROM must be floating point.  */

void
expand_fix (to, from)
     register rtx to, from;
{
  register rtxfun fun;
  register rtx target;

  to = protect_from_queue (to, 1);
  from = protect_from_queue (from, 0);

  if (force_mem)
    {
      from = force_not_mem (from);
    }

  if (fun = can_fix_p (GET_MODE (to), GET_MODE (from)))
    {
      emit_insn ((*fun) (to, from));
      return;
    }

  if (GET_MODE (to) == SImode && (fun = can_fix_p (DImode, GET_MODE (from))))
    {
      register rtx temp = gen_reg_rtx (DImode);
      emit_insn ((*fun) (temp, from));
      convert_move (to, temp, 0);
      return;
    }

  if (GET_MODE (from) != DFmode)
    {
      register rtx tem = gen_reg_rtx (DFmode);
      convert_move (tem, from, 0);
      from = tem;
    }

  if (fun = can_fix_p (GET_MODE (to), GET_MODE (from)))
    {
      emit_insn ((*fun) (to, from));
      return;
    }

  if (fun = can_fix_p (DImode, DFmode))
    {
      target = gen_reg_rtx (DImode);
      emit_insn ((*fun) (target, from));
    }
  else
    {
      emit_library_call (gen_rtx (SYMBOL_REF, Pmode, "fixdfdi"),
			 1, from, DFmode);
      target = function_value (DImode);
    }

  if (GET_MODE (to) == DImode)
    emit_move_insn (to, target);
  else
    convert_move (to, target, 0);
}

static void
blank_optab (op)
     optab op;
{
  int i;
  for (i = 0; i < NUM_MACHINE_MODES; i++)
    op[i].insn_code = CODE_FOR_nothing;
}

/* Call this once to initialize the contents of the optabs
   appropriately for the current target machine.  */

void
init_optabs ()
{
  init_fixtab ();
  init_floattab ();
  init_comparisons ();

  blank_optab (add_optab);
  blank_optab (sub_optab);
  blank_optab (smul_optab);
  blank_optab (umul_optab);
  blank_optab (smul_widen_optab);
  blank_optab (umul_widen_optab);
  blank_optab (sdiv_optab);
  blank_optab (sdivmod_optab);
  blank_optab (udiv_optab);
  blank_optab (udivmod_optab);
  blank_optab (smod_optab);
  blank_optab (umod_optab);
  blank_optab (flodiv_optab);
  blank_optab (and_optab);
  blank_optab (andcb_optab);
  blank_optab (ior_optab);
  blank_optab (xor_optab);
  blank_optab (ashl_optab);
  blank_optab (ashr_optab);
  blank_optab (lshl_optab);
  blank_optab (lshr_optab);
  blank_optab (rotl_optab);
  blank_optab (rotr_optab);
  blank_optab (mov_optab);
  blank_optab (movstrict_optab);
  blank_optab (cmp_optab);
  blank_optab (tst_optab);
  blank_optab (neg_optab);
  blank_optab (abs_optab);
  blank_optab (one_cmpl_optab);

#ifdef HAVE_addqi3
  if (HAVE_addqi3)
    add_optab[(int) QImode].insn_code = CODE_FOR_addqi3;
#endif
#ifdef HAVE_addhi3
  if (HAVE_addhi3)
    add_optab[(int) HImode].insn_code = CODE_FOR_addhi3;
#endif
#ifdef HAVE_addsi3
  if (HAVE_addsi3)
    add_optab[(int) SImode].insn_code = CODE_FOR_addsi3;
#endif
#ifdef HAVE_adddi3
  if (HAVE_adddi3)
    add_optab[(int) DImode].insn_code = CODE_FOR_adddi3;
#endif
#ifdef HAVE_addsf3
  if (HAVE_addsf3)
    add_optab[(int) SFmode].insn_code = CODE_FOR_addsf3;
#endif
#ifdef HAVE_adddf3
  if (HAVE_adddf3)
    add_optab[(int) DFmode].insn_code = CODE_FOR_adddf3;
#endif
  add_optab[(int) DImode].lib_call = "adddi3";
  add_optab[(int) SFmode].lib_call = "addsf3";
  add_optab[(int) DFmode].lib_call = "adddf3";

#ifdef HAVE_subqi3
  if (HAVE_subqi3)
    sub_optab[(int) QImode].insn_code = CODE_FOR_subqi3;
#endif
#ifdef HAVE_subhi3
  if (HAVE_subhi3)
    sub_optab[(int) HImode].insn_code = CODE_FOR_subhi3;
#endif
#ifdef HAVE_subsi3
  if (HAVE_subsi3)
    sub_optab[(int) SImode].insn_code = CODE_FOR_subsi3;
#endif
#ifdef HAVE_subdi3
  if (HAVE_subdi3)
    sub_optab[(int) DImode].insn_code = CODE_FOR_subdi3;
#endif
#ifdef HAVE_subsf3
  if (HAVE_subsf3)
    sub_optab[(int) SFmode].insn_code = CODE_FOR_subsf3;
#endif
#ifdef HAVE_subdf3
  if (HAVE_subdf3)
    sub_optab[(int) DFmode].insn_code = CODE_FOR_subdf3;
#endif
  sub_optab[(int) DImode].lib_call = "subdi3";
  sub_optab[(int) SFmode].lib_call = "subsf3";
  sub_optab[(int) DFmode].lib_call = "subdf3";

#ifdef HAVE_mulqi3
  if (HAVE_mulqi3)
    smul_optab[(int) QImode].insn_code = CODE_FOR_mulqi3;
#endif
#ifdef HAVE_mulhi3
  if (HAVE_mulhi3)
    smul_optab[(int) HImode].insn_code = CODE_FOR_mulhi3;
#endif
#ifdef HAVE_mulsi3
  if (HAVE_mulsi3)
    smul_optab[(int) SImode].insn_code = CODE_FOR_mulsi3;
#endif
#ifdef HAVE_muldi3
  if (HAVE_muldi3)
    smul_optab[(int) DImode].insn_code = CODE_FOR_muldi3;
#endif
#ifdef HAVE_mulsf3
  if (HAVE_mulsf3)
    smul_optab[(int) SFmode].insn_code = CODE_FOR_mulsf3;
#endif
#ifdef HAVE_muldf3
  if (HAVE_muldf3)
    smul_optab[(int) DFmode].insn_code = CODE_FOR_muldf3;
#endif
  smul_optab[(int) SImode].lib_call = "mulsi3";
  smul_optab[(int) DImode].lib_call = "muldi3";
  smul_optab[(int) SFmode].lib_call = "mulsf3";
  smul_optab[(int) DFmode].lib_call = "muldf3";

#ifdef HAVE_mulqihi3
  if (HAVE_mulqihi3)
    smul_widen_optab[(int) HImode].insn_code = CODE_FOR_mulqihi3;
#endif
#ifdef HAVE_mulhisi3
  if (HAVE_mulhisi3)
    smul_widen_optab[(int) SImode].insn_code = CODE_FOR_mulhisi3;
#endif
#ifdef HAVE_mulsidi3
  if (HAVE_mulsidi3)
    smul_widen_optab[(int) DImode].insn_code = CODE_FOR_mulsidi3;
#endif

#ifdef HAVE_umulqi3
  if (HAVE_umulqi3)
    umul_optab[(int) QImode].insn_code = CODE_FOR_umulqi3;
#endif
#ifdef HAVE_umulhi3
  if (HAVE_umulhi3)
    umul_optab[(int) HImode].insn_code = CODE_FOR_umulhi3;
#endif
#ifdef HAVE_umulsi3
  if (HAVE_umulsi3)
    umul_optab[(int) SImode].insn_code = CODE_FOR_umulsi3;
#endif
#ifdef HAVE_umuldi3
  if (HAVE_umuldi3)
    umul_optab[(int) DImode].insn_code = CODE_FOR_umuldi3;
#endif
#ifdef HAVE_umulsf3
  if (HAVE_umulsf3)
    umul_optab[(int) SFmode].insn_code = CODE_FOR_umulsf3;
#endif
#ifdef HAVE_umuldf3
  if (HAVE_umuldf3)
    umul_optab[(int) DFmode].insn_code = CODE_FOR_umuldf3;
#endif
  umul_optab[(int) SImode].lib_call = "umulsi3";
  umul_optab[(int) DImode].lib_call = "umuldi3";
  umul_optab[(int) SFmode].lib_call = "umulsf3";
  umul_optab[(int) DFmode].lib_call = "umuldf3";

#ifdef HAVE_umulqihi3
  if (HAVE_umulqihi3)
    umul_widen_optab[(int) HImode].insn_code = CODE_FOR_umulqihi3;
#endif
#ifdef HAVE_umulhisi3
  if (HAVE_umulhisi3)
    umul_widen_optab[(int) SImode].insn_code = CODE_FOR_umulhisi3;
#endif
#ifdef HAVE_umulsidi3
  if (HAVE_umulsidi3)
    umul_widen_optab[(int) DImode].insn_code = CODE_FOR_umulsidi3;
#endif

#ifdef HAVE_divqi3
  if (HAVE_divqi3)
    sdiv_optab[(int) QImode].insn_code = CODE_FOR_divqi3;
#endif
#ifdef HAVE_divhi3
  if (HAVE_divhi3)
    sdiv_optab[(int) HImode].insn_code = CODE_FOR_divhi3;
#endif
#ifdef HAVE_divsi3
  if (HAVE_divsi3)
    sdiv_optab[(int) SImode].insn_code = CODE_FOR_divsi3;
#endif
#ifdef HAVE_divdi3
  if (HAVE_divdi3)
    sdiv_optab[(int) DImode].insn_code = CODE_FOR_divdi3;
#endif
  sdiv_optab[(int) SImode].lib_call = "divsi3";
  sdiv_optab[(int) DImode].lib_call = "divdi3";

#ifdef HAVE_udivqi3
  if (HAVE_udivqi3)
    udiv_optab[(int) QImode].insn_code = CODE_FOR_udivqi3;
#endif
#ifdef HAVE_udivhi3
  if (HAVE_udivhi3)
    udiv_optab[(int) HImode].insn_code = CODE_FOR_udivhi3;
#endif
#ifdef HAVE_udivsi3
  if (HAVE_udivsi3)
    udiv_optab[(int) SImode].insn_code = CODE_FOR_udivsi3;
#endif
#ifdef HAVE_udivdi3
  if (HAVE_udivdi3)
    udiv_optab[(int) DImode].insn_code = CODE_FOR_udivdi3;
#endif
  udiv_optab[(int) SImode].lib_call = "udivsi3";
  udiv_optab[(int) DImode].lib_call = "udivdi3";

#ifdef HAVE_divmodqi4
  if (HAVE_divmodqi4)
    sdivmod_optab[(int) QImode].insn_code = CODE_FOR_divmodqi4;
#endif
#ifdef HAVE_divmodhi4
  if (HAVE_divmodhi4)
    sdivmod_optab[(int) HImode].insn_code = CODE_FOR_divmodhi4;
#endif
#ifdef HAVE_divmodsi4
  if (HAVE_divmodsi4)
    sdivmod_optab[(int) SImode].insn_code = CODE_FOR_divmodsi4;
#endif
#ifdef HAVE_divmoddi4
  if (HAVE_divmoddi4)
    sdivmod_optab[(int) DImode].insn_code = CODE_FOR_divmoddi4;
#endif

#ifdef HAVE_udivmodqi4
  if (HAVE_udivmodqi4)
    udivmod_optab[(int) QImode].insn_code = CODE_FOR_udivmodqi4;
#endif
#ifdef HAVE_udivmodhi4
  if (HAVE_udivmodhi4)
    udivmod_optab[(int) HImode].insn_code = CODE_FOR_udivmodhi4;
#endif
#ifdef HAVE_udivmodsi4
  if (HAVE_udivmodsi4)
    udivmod_optab[(int) SImode].insn_code = CODE_FOR_udivmodsi4;
#endif
#ifdef HAVE_udivmoddi4
  if (HAVE_udivmoddi4)
    udivmod_optab[(int) DImode].insn_code = CODE_FOR_udivmoddi4;
#endif

#ifdef HAVE_modqi3
  if (HAVE_modqi3)
    smod_optab[(int) QImode].insn_code = CODE_FOR_modqi3;
#endif
#ifdef HAVE_modhi3
  if (HAVE_modhi3)
    smod_optab[(int) HImode].insn_code = CODE_FOR_modhi3;
#endif
#ifdef HAVE_modsi3
  if (HAVE_modsi3)
    smod_optab[(int) SImode].insn_code = CODE_FOR_modsi3;
#endif
#ifdef HAVE_moddi3
  if (HAVE_moddi3)
    smod_optab[(int) DImode].insn_code = CODE_FOR_moddi3;
#endif
  smod_optab[(int) SImode].lib_call = "modsi3";
  smod_optab[(int) DImode].lib_call = "moddi3";

#ifdef HAVE_umodqi3
  if (HAVE_umodqi3)
    umod_optab[(int) QImode].insn_code = CODE_FOR_umodqi3;
#endif
#ifdef HAVE_umodhi3
  if (HAVE_umodhi3)
    umod_optab[(int) HImode].insn_code = CODE_FOR_umodhi3;
#endif
#ifdef HAVE_umodsi3
  if (HAVE_umodsi3)
    umod_optab[(int) SImode].insn_code = CODE_FOR_umodsi3;
#endif
#ifdef HAVE_umoddi3
  if (HAVE_umoddi3)
    umod_optab[(int) DImode].insn_code = CODE_FOR_umoddi3;
#endif
  umod_optab[(int) SImode].lib_call = "umodsi3";
  umod_optab[(int) DImode].lib_call = "umoddi3";

#ifdef HAVE_divsf3
  if (HAVE_divsf3)
    flodiv_optab[(int) SFmode].insn_code = CODE_FOR_divsf3;
#endif
#ifdef HAVE_divdf3
  if (HAVE_divdf3)
    flodiv_optab[(int) DFmode].insn_code = CODE_FOR_divdf3;
#endif
  flodiv_optab[(int) SFmode].lib_call = "divsf3";
  flodiv_optab[(int) DFmode].lib_call = "divdf3";

#ifdef HAVE_andqi3
  if (HAVE_andqi3)
    and_optab[(int) QImode].insn_code = CODE_FOR_andqi3;
#endif
#ifdef HAVE_andhi3
  if (HAVE_andhi3)
    and_optab[(int) HImode].insn_code = CODE_FOR_andhi3;
#endif
#ifdef HAVE_andsi3
  if (HAVE_andsi3)
    and_optab[(int) SImode].insn_code = CODE_FOR_andsi3;
#endif
  and_optab[(int) DImode].lib_call = "anddi3";

#ifdef HAVE_andcbqi3
  if (HAVE_andcbqi3)
    andcb_optab[(int) QImode].insn_code = CODE_FOR_andcbqi3;
#endif
#ifdef HAVE_andcbhi3
  if (HAVE_andcbhi3)
    andcb_optab[(int) HImode].insn_code = CODE_FOR_andcbhi3;
#endif
#ifdef HAVE_andcbsi3
  if (HAVE_andcbsi3)
    andcb_optab[(int) SImode].insn_code = CODE_FOR_andcbsi3;
#endif
  andcb_optab[(int) DImode].lib_call = "andcbdi3";

#ifdef HAVE_iorqi3
  if (HAVE_iorqi3)
    ior_optab[(int) QImode].insn_code = CODE_FOR_iorqi3;
#endif
#ifdef HAVE_iorhi3
  if (HAVE_iorhi3)
    ior_optab[(int) HImode].insn_code = CODE_FOR_iorhi3;
#endif
#ifdef HAVE_iorsi3
  if (HAVE_iorsi3)
    ior_optab[(int) SImode].insn_code = CODE_FOR_iorsi3;
#endif
  ior_optab[(int) DImode].lib_call = "iordi3";

#ifdef HAVE_xorqi3
  if (HAVE_xorqi3)
    xor_optab[(int) QImode].insn_code = CODE_FOR_xorqi3;
#endif
#ifdef HAVE_xorhi3
  if (HAVE_xorhi3)
    xor_optab[(int) HImode].insn_code = CODE_FOR_xorhi3;
#endif
#ifdef HAVE_xorsi3
  if (HAVE_xorsi3)
    xor_optab[(int) SImode].insn_code = CODE_FOR_xorsi3;
#endif
  xor_optab[(int) DImode].lib_call = "xordi3";

#ifdef HAVE_ashlqi3
  if (HAVE_ashlqi3)
    ashl_optab[(int) QImode].insn_code = CODE_FOR_ashlqi3;
#endif
#ifdef HAVE_ashlhi3
  if (HAVE_ashlhi3)
    ashl_optab[(int) HImode].insn_code = CODE_FOR_ashlhi3;
#endif
#ifdef HAVE_ashlsi3
  if (HAVE_ashlsi3)
    ashl_optab[(int) SImode].insn_code = CODE_FOR_ashlsi3;
#endif
#ifdef HAVE_ashldi3
  if (HAVE_ashldi3)
    ashl_optab[(int) DImode].insn_code = CODE_FOR_ashldi3;
#endif
  ashl_optab[(int) SImode].lib_call = "ashlsi3";
  ashl_optab[(int) DImode].lib_call = "ashldi3";

#ifdef HAVE_ashrqi3
  if (HAVE_ashrqi3)
    ashr_optab[(int) QImode].insn_code = CODE_FOR_ashrqi3;
#endif
#ifdef HAVE_ashrhi3
  if (HAVE_ashrhi3)
    ashr_optab[(int) HImode].insn_code = CODE_FOR_ashrhi3;
#endif
#ifdef HAVE_ashrsi3
  if (HAVE_ashrsi3)
    ashr_optab[(int) SImode].insn_code = CODE_FOR_ashrsi3;
#endif
#ifdef HAVE_ashrdi3
  if (HAVE_ashrdi3)
    ashr_optab[(int) DImode].insn_code = CODE_FOR_ashrdi3;
#endif
  ashr_optab[(int) SImode].lib_call = "ashrsi3";
  ashr_optab[(int) DImode].lib_call = "ashrdi3";

#ifdef HAVE_lshlqi3
  if (HAVE_lshlqi3)
    lshl_optab[(int) QImode].insn_code = CODE_FOR_lshlqi3;
#endif
#ifdef HAVE_lshlhi3
  if (HAVE_lshlhi3)
    lshl_optab[(int) HImode].insn_code = CODE_FOR_lshlhi3;
#endif
#ifdef HAVE_lshlsi3
  if (HAVE_lshlsi3)
    lshl_optab[(int) SImode].insn_code = CODE_FOR_lshlsi3;
#endif
#ifdef HAVE_lshldi3
  if (HAVE_lshldi3)
    lshl_optab[(int) DImode].insn_code = CODE_FOR_lshldi3;
#endif
  lshl_optab[(int) SImode].lib_call = "lshlsi3";
  lshl_optab[(int) DImode].lib_call = "lshldi3";

#ifdef HAVE_lshrqi3
  if (HAVE_lshrqi3)
    lshr_optab[(int) QImode].insn_code = CODE_FOR_lshrqi3;
#endif
#ifdef HAVE_lshrhi3
  if (HAVE_lshrhi3)
    lshr_optab[(int) HImode].insn_code = CODE_FOR_lshrhi3;
#endif
#ifdef HAVE_lshrsi3
  if (HAVE_lshrsi3)
    lshr_optab[(int) SImode].insn_code = CODE_FOR_lshrsi3;
#endif
#ifdef HAVE_lshrdi3
  if (HAVE_lshrdi3)
    lshr_optab[(int) DImode].insn_code = CODE_FOR_lshrdi3;
#endif
  lshr_optab[(int) SImode].lib_call = "lshrsi3";
  lshr_optab[(int) DImode].lib_call = "lshrdi3";

#ifdef HAVE_rotlqi3
  if (HAVE_rotlqi3)
    rotl_optab[(int) QImode].insn_code = CODE_FOR_rotlqi3;
#endif
#ifdef HAVE_rotlhi3
  if (HAVE_rotlhi3)
    rotl_optab[(int) HImode].insn_code = CODE_FOR_rotlhi3;
#endif
#ifdef HAVE_rotlsi3
  if (HAVE_rotlsi3)
    rotl_optab[(int) SImode].insn_code = CODE_FOR_rotlsi3;
#endif
#ifdef HAVE_rotldi3
  if (HAVE_rotldi3)
    rotl_optab[(int) DImode].insn_code = CODE_FOR_rotldi3;
#endif
  rotl_optab[(int) SImode].lib_call = "rotlsi3";
  rotl_optab[(int) DImode].lib_call = "rotldi3";

#ifdef HAVE_rotrqi3
  if (HAVE_rotrqi3)
    rotr_optab[(int) QImode].insn_code = CODE_FOR_rotrqi3;
#endif
#ifdef HAVE_rotrhi3
  if (HAVE_rotrhi3)
    rotr_optab[(int) HImode].insn_code = CODE_FOR_rotrhi3;
#endif
#ifdef HAVE_rotrsi3
  if (HAVE_rotrsi3)
    rotr_optab[(int) SImode].insn_code = CODE_FOR_rotrsi3;
#endif
#ifdef HAVE_rotrdi3
  if (HAVE_rotrdi3)
    rotr_optab[(int) DImode].insn_code = CODE_FOR_rotrdi3;
#endif
  rotr_optab[(int) SImode].lib_call = "rotrsi3";
  rotr_optab[(int) DImode].lib_call = "rotrdi3";

#ifdef HAVE_negqi2
  if (HAVE_negqi2)
    neg_optab[(int) QImode].insn_code = CODE_FOR_negqi2;
#endif
#ifdef HAVE_neghi2
  if (HAVE_neghi2)
    neg_optab[(int) HImode].insn_code = CODE_FOR_neghi2;
#endif
#ifdef HAVE_negsi2
  if (HAVE_negsi2)
    neg_optab[(int) SImode].insn_code = CODE_FOR_negsi2;
#endif
#ifdef HAVE_negsf2
  if (HAVE_negsf2)
    neg_optab[(int) SFmode].insn_code = CODE_FOR_negsf2;
#endif
#ifdef HAVE_negdf2
  if (HAVE_negdf2)
    neg_optab[(int) DFmode].insn_code = CODE_FOR_negdf2;
#endif
  neg_optab[(int) SImode].lib_call = "negsi2"; 
  neg_optab[(int) DImode].lib_call = "negdi2";
  neg_optab[(int) SFmode].lib_call = "negsf2";
  neg_optab[(int) DFmode].lib_call = "negdf2";

#ifdef HAVE_absqi2
  if (HAVE_absqi2)
    abs_optab[(int) QImode].insn_code = CODE_FOR_absqi2;
#endif
#ifdef HAVE_abshi2
  if (HAVE_abshi2)
    abs_optab[(int) HImode].insn_code = CODE_FOR_abshi2;
#endif
#ifdef HAVE_abssi2
  if (HAVE_abssi2)
    abs_optab[(int) SImode].insn_code = CODE_FOR_abssi2;
#endif
#ifdef HAVE_abssf2
  if (HAVE_abssf2)
    abs_optab[(int) SFmode].insn_code = CODE_FOR_abssf2;
#endif
#ifdef HAVE_absdf2
  if (HAVE_absdf2)
    abs_optab[(int) DFmode].insn_code = CODE_FOR_absdf2;
#endif
  /* No library calls here!  If there is no abs instruction,
     expand_expr will generate a conditional negation.  */

#ifdef HAVE_one_cmplqi2
  if (HAVE_one_cmplqi2)
    one_cmpl_optab[(int) QImode].insn_code = CODE_FOR_one_cmplqi2;
#endif
#ifdef HAVE_one_cmplhi2
  if (HAVE_one_cmplhi2)
    one_cmpl_optab[(int) HImode].insn_code = CODE_FOR_one_cmplhi2;
#endif
#ifdef HAVE_one_cmplsi2
  if (HAVE_one_cmplsi2)
    one_cmpl_optab[(int) SImode].insn_code = CODE_FOR_one_cmplsi2;
#endif
  one_cmpl_optab[(int) SImode].lib_call = "one_cmplsi2"; 
  one_cmpl_optab[(int) DImode].lib_call = "one_cmpldi2";

#ifdef HAVE_movqi
  if (HAVE_movqi)
    mov_optab[(int) QImode].insn_code = CODE_FOR_movqi;
#endif
#ifdef HAVE_movhi
  if (HAVE_movhi)
    mov_optab[(int) HImode].insn_code = CODE_FOR_movhi;
#endif
#ifdef HAVE_movsi
  if (HAVE_movsi)
    mov_optab[(int) SImode].insn_code = CODE_FOR_movsi;
#endif
#ifdef HAVE_movdi
  if (HAVE_movdi)
    mov_optab[(int) DImode].insn_code = CODE_FOR_movdi;
#endif
#ifdef HAVE_movsf
  if (HAVE_movsf)
    mov_optab[(int) SFmode].insn_code = CODE_FOR_movsf;
#endif
#ifdef HAVE_movdf
  if (HAVE_movdf)
    mov_optab[(int) DFmode].insn_code = CODE_FOR_movdf;
#endif

#ifdef HAVE_movstrictqi
  if (HAVE_movstrictqi)
    movstrict_optab[(int) QImode].insn_code = CODE_FOR_movstrictqi;
#endif
#ifdef HAVE_movstricthi
  if (HAVE_movstricthi)
    movstrict_optab[(int) HImode].insn_code = CODE_FOR_movstricthi;
#endif
#ifdef HAVE_movstrictsi
  if (HAVE_movstrictsi)
    movstrict_optab[(int) SImode].insn_code = CODE_FOR_movstrictsi;
#endif
#ifdef HAVE_movstrictdi
  if (HAVE_movstrictdi)
    movstrict_optab[(int) DImode].insn_code = CODE_FOR_movstrictdi;
#endif

#ifdef HAVE_cmpqi
  if (HAVE_cmpqi)
    cmp_optab[(int) QImode].insn_code = CODE_FOR_cmpqi;
#endif
#ifdef HAVE_cmphi
  if (HAVE_cmphi)
    cmp_optab[(int) HImode].insn_code = CODE_FOR_cmphi;
#endif
#ifdef HAVE_cmpsi
  if (HAVE_cmpsi)
    cmp_optab[(int) SImode].insn_code = CODE_FOR_cmpsi;
#endif
#ifdef HAVE_cmpsf
  if (HAVE_cmpsf)
    cmp_optab[(int) SFmode].insn_code = CODE_FOR_cmpsf;
#endif
#ifdef HAVE_cmpdf
  if (HAVE_cmpdf)
    cmp_optab[(int) DFmode].insn_code = CODE_FOR_cmpdf;
#endif
#ifdef HAVE_tstqi
  if (HAVE_tstqi)
    tst_optab[(int) QImode].insn_code = CODE_FOR_tstqi;
#endif
#ifdef HAVE_tsthi
  if (HAVE_tsthi)
    tst_optab[(int) HImode].insn_code = CODE_FOR_tsthi;
#endif
#ifdef HAVE_tstsi
  if (HAVE_tstsi)
    tst_optab[(int) SImode].insn_code = CODE_FOR_tstsi;
#endif
#ifdef HAVE_tstsf
  if (HAVE_tstsf)
    tst_optab[(int) SFmode].insn_code = CODE_FOR_tstsf;
#endif
#ifdef HAVE_tstdf
  if (HAVE_tstdf)
    tst_optab[(int) DFmode].insn_code = CODE_FOR_tstdf;
#endif
  cmp_optab[(int) SImode].lib_call = "cmpsi2"; 
  cmp_optab[(int) DImode].lib_call = "cmpdi2";
  cmp_optab[(int) SFmode].lib_call = "cmpsf2";
  cmp_optab[(int) DFmode].lib_call = "cmpdf2";
}
