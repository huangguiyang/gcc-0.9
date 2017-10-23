;;- Machine description for GNU compiler
;;- Vax Version
;;   Copyright (C) 1987 Free Software Foundation, Inc.

;; This file is part of GNU CC.

;; GNU CC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU CC General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU CC, but only under the conditions described in the
;; GNU CC General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU CC so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.


;;- Instruction patterns.  When multiple patterns apply,
;;- the first one in the file is chosen.
;;-
;;- See file "rtl.def" for documentation on define_insn, match_*, et. al.
;;-
;;- cpp macro #define NOTICE_UPDATE_CC in file tm.h handles condition code
;;- updates for most instructions.

(define_insn "tstdf"
  [(set (cc0)
	(match_operand:DF 0 "general_operand" "gF"))]
  ""
  "tstd %0")

(define_insn "tstsf"
  [(set (cc0)
	(match_operand:SF 0 "general_operand" "gF"))]
  ""
  "tstf %0")

(define_insn "tstsi"
  [(set (cc0)
	(match_operand:SI 0 "general_operand" "g"))]
  ""
  "tstl %0")

(define_insn "tsthi"
  [(set (cc0)
	(match_operand:HI 0 "general_operand" "g"))]
  ""
  "tstw %0")

(define_insn "tstqi"
  [(set (cc0)
	(match_operand:QI 0 "general_operand" "g"))]
  ""
  "tstb %0")

(define_insn "cmpdf"
  [(set (cc0)
	(minus (match_operand:DF 0 "general_operand" "gF")
	       (match_operand:DF 1 "general_operand" "gF")))]
  ""
  "cmpd %0,%1")

(define_insn "cmpsf"
  [(set (cc0)
	(minus (match_operand:SF 0 "general_operand" "gF")
	       (match_operand:SF 1 "general_operand" "gF")))]
  ""
  "cmpf %0,%1")

(define_insn "cmpsi"
  [(set (cc0)
	(minus (match_operand:SI 0 "general_operand" "g")
	       (match_operand:SI 1 "general_operand" "g")))]
  ""
  "cmpl %0,%1")

(define_insn "cmphi"
  [(set (cc0)
	(minus (match_operand:HI 0 "general_operand" "g")
	       (match_operand:HI 1 "general_operand" "g")))]
  ""
  "cmpw %0,%1")

(define_insn "cmpqi"
  [(set (cc0)
	(minus (match_operand:QI 0 "general_operand" "g")
	       (match_operand:QI 1 "general_operand" "g")))]
  ""
  "cmpb %0,%1")

(define_insn ""
  [(set (cc0)
	(and:SI (match_operand:SI 0 "general_operand" "g")
		(match_operand:SI 1 "general_operand" "g")))]
  ""
  "bitl %0,%1")

(define_insn ""
  [(set (cc0)
	(and:HI (match_operand:HI 0 "general_operand" "g")
		(match_operand:HI 1 "general_operand" "g")))]
  ""
  "bitw %0,%1")

(define_insn ""
  [(set (cc0)
	(and:QI (match_operand:QI 0 "general_operand" "g")
		(match_operand:QI 1 "general_operand" "g")))]
  ""
  "bitb %0,%1")

(define_insn "movdf"
  [(set (match_operand:DF 0 "general_operand" "=g")
	(match_operand:DF 1 "general_operand" "gF"))]
  ""
  "*
{
  if (operands[1] == dconst0_rtx)
    return \"clrd %0\";
  return \"movd %1,%0\";
}")

(define_insn "movsf"
  [(set (match_operand:SF 0 "general_operand" "=g")
	(match_operand:SF 1 "general_operand" "gF"))]
  ""
  "*
{
  if (operands[1] == fconst0_rtx)
    return \"clrf %0\";
  return \"movf %1,%0\";
}")

(define_insn "movti"
  [(set (match_operand:TI 0 "general_operand" "=g")
	(match_operand:TI 1 "general_operand" "g"))]
  ""
  "movh %1,%0")

(define_insn "movdi"
  [(set (match_operand:DI 0 "general_operand" "=g")
	(match_operand:DI 1 "general_operand" "g"))]
  ""
  "movd %1,%0")

(define_insn "movsi"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(match_operand:SI 1 "general_operand" "g"))]
  ""
  "*
{ if (operands[1] == const1_rtx
      && GET_MODE (REG_NOTES (insn)) == (enum machine_mode) REG_WAS_0)
    return \"incl %0\";
  if (GET_CODE (operands[1]) == SYMBOL_REF || GET_CODE (operands[1]) == CONST)
    {
      if (push_operand (operands[0], SImode))
	return \"pushab %a1\";
      return \"movab %a1,%0\";
    }
  if (operands[1] == const0_rtx)
    return \"clrl %0\";
  if (GET_CODE (operands[1]) == CONST_INT
      && (unsigned) INTVAL (operands[1]) >= 64)
    {
      int i = INTVAL (operands[1]);
      if ((unsigned)(-i) < 64)
	{
	  operands[1] = gen_rtx (CONST_INT, VOIDmode, -i);
	  return \"mnegl %1,%0\";
	}
      if ((unsigned)i < 0x100)
	return \"movzbl %1,%0\";
      if (i >= -0x80 && i < 0)
	return \"cvtbl %1,%0\";
      if ((unsigned)i < 0x10000)
	return \"movzwl %1,%0\";
      if (i >= -0x8000 && i < 0)
	return \"cvtwl %1,%0\";
    }
  if (push_operand (operands[0], SImode))
    return \"pushl %1\";
  return \"movl %1,%0\";
}")

(define_insn "movhi"
  [(set (match_operand:HI 0 "general_operand" "=g")
	(match_operand:HI 1 "general_operand" "g"))]
  ""
  "*
{
 if (operands[1] == const1_rtx
      && GET_MODE (REG_NOTES (insn)) == (enum machine_mode) REG_WAS_0)
    return \"incw %0\";
  if (operands[1] == const0_rtx)
    return \"clrw %0\";
  if (GET_CODE (operands[1]) == CONST_INT
      && (unsigned) INTVAL (operands[1]) >= 64)
    {
      int i = INTVAL (operands[1]);
      if ((unsigned)(-i) < 64)
	{
	  operands[1] = gen_rtx (CONST_INT, VOIDmode, -i);
	  return \"mnegw %1,%0\";
	}
      if ((unsigned)i < 0x100)
	return \"movzbw %1,%0\";
      if (i >= -0x80 && i < 0)
	return \"cvtbw %1,%0\";
    }
  return \"movw %1,%0\";
}")

(define_insn "movqi"
  [(set (match_operand:QI 0 "general_operand" "=g")
	(match_operand:QI 1 "general_operand" "g"))]
  ""
  "*
{
  if (operands[1] == const0_rtx)
    return \"clrb %0\";
  return \"movb %1,%0\";
}")

;; The definition of this insn does not really explain what it does,
;; but it should suffice
;; that anything generated as this insn will be recognized as one
;; and that it won't successfully combine with anything.
(define_insn "movstrhi"
  [(set (match_operand:BLK 0 "general_operand" "=g")
	(match_operand:BLK 1 "general_operand" "g"))
   (use (match_operand:HI 2 "general_operand" "g"))
   (clobber (nil))			;;- Clobber everything in memory
   (clobber (reg 0))
   (clobber (reg 1))
   (clobber (reg 2))
   (clobber (reg 3))
   (clobber (reg 4))
   (clobber (reg 5))]
  ""
  "movc3 %2,%1,%0")

;;- load or push effective address 
;; These come after the move patterns
;; because we don't want pushl $1 turned into pushad 1.

(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=g")
	(match_operand:QI 1 "address_operand" "p"))]
  ""
  "*
{
  if (push_operand (operands[0], SImode))
    return \"pushab %a1\";
  return \"movab %a1,%0\";
}")

(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=g")
	(match_operand:HI 1 "address_operand" "p"))]
  ""
  "*
{
  if (push_operand (operands[0], SImode))
    return \"pushaw %a1\";
  return \"movaw %a1,%0\";
}")

(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=g")
	(match_operand:SI 1 "address_operand" "p"))]
  ""
  "*
{
  if (push_operand (operands[0], SImode))
    return \"pushal %a1\";
  return \"moval %a1,%0\";
}")

(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=g")
	(match_operand:SF 1 "address_operand" "p"))]
  ""
  "*
{
  if (push_operand (operands[0], SImode))
    return \"pushaf %a1\";
  return \"movaf %a1,%0\";
}")

(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=g")
	(match_operand:DF 1 "address_operand" "p"))]
  ""
  "*
{
  if (push_operand (operands[0], SImode))
    return \"pushad %a1\";
  return \"movad %a1,%0\";
}")

(define_insn "extendqihi2"
  [(set (match_operand:HI 0 "general_operand" "=g")
	(sign_extend:HI (match_operand:QI 1 "general_operand" "g")))]
  ""
  "cvtbw %1,%0")

(define_insn "extendqisi2"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(sign_extend:SI (match_operand:QI 1 "general_operand" "g")))]
  ""
  "cvtbl %1,%0")

(define_insn "floatqisf2"
  [(set (match_operand:SF 0 "general_operand" "=g")
	(float:SF (match_operand:QI 1 "general_operand" "g")))]
  ""
  "cvtbf %1,%0")

(define_insn "floatqidf2"
  [(set (match_operand:DF 0 "general_operand" "=g")
	(float:DF (match_operand:QI 1 "general_operand" "g")))]
  ""
  "cvtbd %1,%0")

(define_insn "trunchiqi2"
  [(set (match_operand:QI 0 "general_operand" "=g")
	(truncate:QI (match_operand:HI 1 "general_operand" "g")))]
  ""
  "cvtwb %1,%0")

(define_insn "extendhisi2"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(sign_extend:SI (match_operand:HI 1 "general_operand" "g")))]
  ""
  "cvtwl %1,%0")

(define_insn "floathisf2"
  [(set (match_operand:SF 0 "general_operand" "=g")
	(float:SF (match_operand:HI 1 "general_operand" "g")))]
  ""
  "cvtwf %1,%0")

(define_insn "floathidf2"
  [(set (match_operand:DF 0 "general_operand" "=g")
	(float:DF (match_operand:HI 1 "general_operand" "g")))]
  ""
  "cvtwd %1,%0")

(define_insn "truncsiqi2"
  [(set (match_operand:QI 0 "general_operand" "=g")
	(truncate:QI (match_operand:SI 1 "general_operand" "g")))]
  ""
  "cvtlb %1,%0")

(define_insn "truncsihi2"
  [(set (match_operand:HI 0 "general_operand" "=g")
	(truncate:HI (match_operand:SI 1 "general_operand" "g")))]
  ""
  "cvtlw %1,%0")

(define_insn "floatsisf2"
  [(set (match_operand:SF 0 "general_operand" "=g")
	(float:SF (match_operand:SI 1 "general_operand" "g")))]
  ""
  "cvtlf %1,%0")

(define_insn "floatsidf2"
  [(set (match_operand:DF 0 "general_operand" "=g")
	(float:DF (match_operand:SI 1 "general_operand" "g")))]
  ""
  "cvtld %1,%0")

(define_insn "fixsfqi2"
  [(set (match_operand:QI 0 "general_operand" "=g")
	(fix:QI (match_operand:SF 1 "general_operand" "gF")))]
  ""
  "cvtfb %1,%0")

(define_insn "fixsfhi2"
  [(set (match_operand:HI 0 "general_operand" "=g")
	(fix:HI (match_operand:SF 1 "general_operand" "gF")))]
  ""
  "cvtfw %1,%0")

(define_insn "fixsfsi2"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(fix:SI (match_operand:SF 1 "general_operand" "gF")))]
  ""
  "cvtfl %1,%0")

(define_insn "extendsfdf2"
  [(set (match_operand:DF 0 "general_operand" "=g")
	(float_extend:DF (match_operand:SF 1 "general_operand" "gF")))]
  ""
  "cvtfd %1,%0")

(define_insn "fixdfqi2"
  [(set (match_operand:QI 0 "general_operand" "=g")
	(fix:QI (match_operand:DF 1 "general_operand" "gF")))]
  ""
  "cvtdb %1,%0")

(define_insn "fixdfhi2"
  [(set (match_operand:HI 0 "general_operand" "=g")
	(fix:HI (match_operand:DF 1 "general_operand" "gF")))]
  ""
  "cvtdw %1,%0")

(define_insn "fixdfsi2"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(fix:SI (match_operand:DF 1 "general_operand" "gF")))]
  ""
  "cvtdl %1,%0")

(define_insn "truncdfsf2"
  [(set (match_operand:SF 0 "general_operand" "=g")
	(float_truncate:SF (match_operand:DF 1 "general_operand" "gF")))]
  ""
  "cvtdf %1,%0")

(define_insn "zero_extendqihi2"
  [(set (match_operand:HI 0 "general_operand" "=g")
	(zero_extend:HI (match_operand:QI 1 "general_operand" "g")))]
  ""
  "movzbw %1,%0")

(define_insn "zero_extendqisi2"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(zero_extend:SI (match_operand:QI 1 "general_operand" "g")))]
  ""
  "movzbl %1,%0")

(define_insn "zero_extendhisi2"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(zero_extend:SI (match_operand:HI 1 "general_operand" "g")))]
  ""
  "movzwl %1,%0")

;;- All kinds of add instructions.

(define_insn "adddf3"
  [(set (match_operand:DF 0 "general_operand" "=g")
	(plus:DF (match_operand:DF 1 "general_operand" "gF")
		 (match_operand:DF 2 "general_operand" "gF")))]
  ""
  "*
{
  if (rtx_equal_p (operands[0], operands[1]))
    return \"addd2 %2,%0\";
  if (rtx_equal_p (operands[0], operands[2]))
    return \"addd2 %1,%0\";
  return \"addd3 %1,%2,%0\";
}")

(define_insn "addsf3"
  [(set (match_operand:SF 0 "general_operand" "=g")
	(plus:SF (match_operand:SF 1 "general_operand" "gF")
		 (match_operand:SF 2 "general_operand" "gF")))]
  ""
  "*
{
  if (rtx_equal_p (operands[0], operands[1]))
    return \"addf2 %2,%0\";
  if (rtx_equal_p (operands[0], operands[2]))
    return \"addf2 %1,%0\";
  return \"addf3 %1,%2,%0\";
}")

(define_insn "addsi3"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(plus:SI (match_operand:SI 1 "general_operand" "g")
		 (match_operand:SI 2 "general_operand" "g")))]
  ""
  "*
{
  if (rtx_equal_p (operands[0], operands[1]))
    {
      if (operands[2] == const1_rtx)
	return \"incl %0\";
      if (GET_CODE (operands[1]) == CONST_INT
	  && INTVAL (operands[1]) == -1)
	return \"decl %0\";
      if (GET_CODE (operands[2]) == CONST_INT
	  && (unsigned) (- INTVAL (operands[2])) < 64)
	return \"subl2 $%n2,%0\";
      return \"addl2 %2,%0\";
    }
  if (rtx_equal_p (operands[0], operands[2]))
    return \"addl2 %1,%0\";
  if (GET_CODE (operands[2]) == CONST_INT
      && GET_CODE (operands[1]) == REG)
    {
      if (push_operand (operands[0], SImode))
        return \"pushab %c2(%1)\";
      return \"movab %c2(%1),%0\";
    }
  if (GET_CODE (operands[2]) == CONST_INT
      && (unsigned) (- INTVAL (operands[2])) < 64)
    return \"subl3 $%n2,%1,%0\";
  return \"addl3 %1,%2,%0\";
}")

(define_insn "addhi3"
  [(set (match_operand:HI 0 "general_operand" "=g")
	(plus:HI (match_operand:HI 1 "general_operand" "g")
		 (match_operand:HI 2 "general_operand" "g")))]
  ""
  "*
{
  if (rtx_equal_p (operands[0], operands[1]))
    {
      if (operands[2] == const1_rtx)
	return \"incw %0\";
      if (GET_CODE (operands[1]) == CONST_INT
	  && INTVAL (operands[1]) == -1)
	return \"decw %0\";
      if (GET_CODE (operands[2]) == CONST_INT
	  && (unsigned) (- INTVAL (operands[2])) < 64)
	return \"subw2 $%n2,%0\";
      return \"addw2 %2,%0\";
    }
  if (rtx_equal_p (operands[0], operands[2]))
    return \"addw2 %1,%0\";
  if (GET_CODE (operands[2]) == CONST_INT
      && (unsigned) (- INTVAL (operands[2])) < 64)
    return \"subw3 $%n2,%1,%0\";
  return \"addw3 %1,%2,%0\";
}")

(define_insn "addqi3"
  [(set (match_operand:QI 0 "general_operand" "=g")
	(plus:QI (match_operand:QI 1 "general_operand" "g")
		 (match_operand:QI 2 "general_operand" "g")))]
  ""
  "*
{
  if (rtx_equal_p (operands[0], operands[1]))
    {
      if (operands[2] == const1_rtx)
	return \"incb %0\";
      if (GET_CODE (operands[1]) == CONST_INT
	  && INTVAL (operands[1]) == -1)
	return \"decb %0\";
      if (GET_CODE (operands[2]) == CONST_INT
	  && (unsigned) (- INTVAL (operands[2])) < 64)
	return \"subb2 $%n2,%0\";
      return \"addb2 %2,%0\";
    }
  if (rtx_equal_p (operands[0], operands[2]))
    return \"addb2 %1,%0\";
  if (GET_CODE (operands[2]) == CONST_INT
      && (unsigned) (- INTVAL (operands[2])) < 64)
    return \"subb3 $%n2,%1,%0\";
  return \"addb3 %1,%2,%0\";
}")

;;- All kinds of subtract instructions.

(define_insn "subdf3"
  [(set (match_operand:DF 0 "general_operand" "=g")
	(minus:DF (match_operand:DF 1 "general_operand" "gF")
		  (match_operand:DF 2 "general_operand" "gF")))]
  ""
  "*
{
  if (rtx_equal_p (operands[0], operands[1]))
    return \"subd2 %2,%0\";
  return \"subd3 %2,%1,%0\";
}")

(define_insn "subsf3"
  [(set (match_operand:SF 0 "general_operand" "=g")
	(minus:SF (match_operand:SF 1 "general_operand" "gF")
		  (match_operand:SF 2 "general_operand" "gF")))]
  ""
  "*
{
  if (rtx_equal_p (operands[0], operands[1]))
    return \"subf2 %2,%0\";
  return \"subf3 %2,%1,%0\";
}")

(define_insn "subsi3"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(minus:SI (match_operand:SI 1 "general_operand" "g")
		  (match_operand:SI 2 "general_operand" "g")))]
  ""
  "*
{
  if (rtx_equal_p (operands[0], operands[1]))
    {
      if (operands[2] == const1_rtx)
	return \"decl %0\";
      return \"subl2 %2,%0\";
    }
  return \"subl3 %2,%1,%0\";
}")

(define_insn "subhi3"
  [(set (match_operand:HI 0 "general_operand" "=g")
	(minus:HI (match_operand:HI 1 "general_operand" "g")
		  (match_operand:HI 2 "general_operand" "g")))]
  ""
  "*
{
  if (rtx_equal_p (operands[0], operands[1]))
    {
      if (operands[2] == const1_rtx)
	return \"decw %0\";
      return \"subw2 %2,%0\";
    }
  return \"subw3 %2,%1,%0\";
}")

(define_insn "subqi3"
  [(set (match_operand:QI 0 "general_operand" "=g")
	(minus:QI (match_operand:QI 1 "general_operand" "g")
		  (match_operand:QI 2 "general_operand" "g")))]
  ""
  "*
{
  if (rtx_equal_p (operands[0], operands[1]))
    {
      if (operands[2] == const1_rtx)
	return \"decb %0\";
      return \"subb2 %2,%0\";
    }
  return \"subb3 %2,%1,%0\";
}")

;;- Multiply instructions.

(define_insn "muldf3"
  [(set (match_operand:DF 0 "general_operand" "=g")
	(mult:DF (match_operand:DF 1 "general_operand" "gF")
		 (match_operand:DF 2 "general_operand" "gF")))]
  ""
  "*
{
  if (rtx_equal_p (operands[0], operands[1]))
    return \"muld2 %2,%0\";
  if (rtx_equal_p (operands[0], operands[2]))
    return \"muld2 %1,%0\";
  return \"muld3 %1,%2,%0\";
}")

(define_insn "mulsf3"
  [(set (match_operand:SF 0 "general_operand" "=g")
	(mult:SF (match_operand:SF 1 "general_operand" "gF")
		 (match_operand:SF 2 "general_operand" "gF")))]
  ""
  "*
{
  if (rtx_equal_p (operands[0], operands[1]))
    return \"mulf2 %2,%0\";
  if (rtx_equal_p (operands[0], operands[2]))
    return \"mulf2 %1,%0\";
  return \"mulf3 %1,%2,%0\";
}")

(define_insn "mulsi3"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(mult:SI (match_operand:SI 1 "general_operand" "g")
		 (match_operand:SI 2 "general_operand" "g")))]
  ""
  "*
{
  if (rtx_equal_p (operands[0], operands[1]))
    return \"mull2 %2,%0\";
  if (rtx_equal_p (operands[0], operands[2]))
    return \"mull2 %1,%0\";
  return \"mull3 %1,%2,%0\";
}")

(define_insn "mulhi3"
  [(set (match_operand:HI 0 "general_operand" "=g")
	(mult:HI (match_operand:HI 1 "general_operand" "g")
		 (match_operand:HI 2 "general_operand" "g")))]
  ""
  "*
{
  if (rtx_equal_p (operands[0], operands[1]))
    return \"mulw2 %2,%0\";
  if (rtx_equal_p (operands[0], operands[2]))
    return \"mulw2 %1,%0\";
  return \"mulw3 %1,%2,%0\";
}")

(define_insn "mulqi3"
  [(set (match_operand:QI 0 "general_operand" "=g")
	(mult:QI (match_operand:QI 1 "general_operand" "g")
		 (match_operand:QI 2 "general_operand" "g")))]
  ""
  "*
{
  if (rtx_equal_p (operands[0], operands[1]))
    return \"mulb2 %2,%0\";
  if (rtx_equal_p (operands[0], operands[2]))
    return \"mulb2 %1,%0\";
  return \"mulb3 %1,%2,%0\";
}")

;;- Divide instructions.

(define_insn "divdf3"
  [(set (match_operand:DF 0 "general_operand" "=g")
	(div:DF (match_operand:DF 1 "general_operand" "gF")
		(match_operand:DF 2 "general_operand" "gF")))]
  ""
  "*
{
  if (rtx_equal_p (operands[0], operands[1]))
    return \"divd2 %2,%0\";
  return \"divd3 %2,%1,%0\";
}")

(define_insn "divsf3"
  [(set (match_operand:SF 0 "general_operand" "=g")
	(div:SF (match_operand:SF 1 "general_operand" "gF")
		(match_operand:SF 2 "general_operand" "gF")))]
  ""
  "*
{
  if (rtx_equal_p (operands[0], operands[1]))
    return \"divf2 %2,%0\";
  return \"divf3 %2,%1,%0\";
}")

(define_insn "divsi3"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(div:SI (match_operand:SI 1 "general_operand" "g")
		(match_operand:SI 2 "general_operand" "g")))]
  ""
  "*
{
  if (rtx_equal_p (operands[0], operands[1]))
    return \"divl2 %2,%0\";
  return \"divl3 %2,%1,%0\";
}")

(define_insn "divhi3"
  [(set (match_operand:HI 0 "general_operand" "=g")
	(div:HI (match_operand:HI 1 "general_operand" "g")
		(match_operand:HI 2 "general_operand" "g")))]
  ""
  "*
{
  if (rtx_equal_p (operands[0], operands[1]))
    return \"divw2 %2,%0\";
  return \"divw3 %2,%1,%0\";
}")

(define_insn "divqi3"
  [(set (match_operand:QI 0 "general_operand" "=g")
	(div:QI (match_operand:QI 1 "general_operand" "g")
		(match_operand:QI 2 "general_operand" "g")))]
  ""
  "*
{
  if (rtx_equal_p (operands[0], operands[1]))
    return \"divb2 %2,%0\";
  return \"divb3 %2,%1,%0\";
}")

;This is left out because it is very slow;
;we are better off programming around the "lack" of this insn.
;(define_insn "divmoddisi4"
;  [(set (match_operand:SI 0 "general_operand" "=g")
;	(div:SI (match_operand:DI 1 "general_operand" "g")
;		(match_operand:SI 2 "general_operand" "g")))
;   (set (match_operand:SI 3 "general_operand" "=g")
;	(mod:SI (match_operand:DI 1 "general_operand" "g")
;		(match_operand:SI 2 "general_operand" "g")))]
;  ""
;  "ediv %2,%1,%0,%3")

(define_insn "andcbsi3"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(and:SI (match_operand:SI 1 "general_operand" "g")
		(not:SI (match_operand:SI 2 "general_operand" "g"))))]
  ""
  "*
{
  if (rtx_equal_p (operands[0], operands[1]))
    return \"bicl2 %2,%0\";
  return \"bicl3 %2,%1,%0\";
}")

(define_insn "andcbhi3"
  [(set (match_operand:HI 0 "general_operand" "=g")
	(and:HI (match_operand:HI 1 "general_operand" "g")
		(not:HI (match_operand:HI 2 "general_operand" "g"))))]
  ""
  "*
{
  if (rtx_equal_p (operands[0], operands[1]))
    return \"bicw2 %2,%0\";
  return \"bicw3 %2,%1,%0\";
}")

(define_insn "andcbqi3"
  [(set (match_operand:QI 0 "general_operand" "=g")
	(and:QI (match_operand:QI 1 "general_operand" "g")
		(not:QI (match_operand:QI 2 "general_operand" "g"))))]
  ""
  "*
{
  if (rtx_equal_p (operands[0], operands[1]))
    return \"bicb2 %2,%0\";
  return \"bicb3 %2,%1,%0\";
}")

;; The following are needed because constant propagation can
;; create them starting from the bic insn patterns above.

(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=g")
	(and:SI (match_operand:SI 1 "general_operand" "g")
		(match_operand:SI 2 "general_operand" "g")))]
  "GET_CODE (operands[2]) == CONST_INT"
  "*
{ operands[2] = gen_rtx (CONST_INT, VOIDmode, ~INTVAL (operands[2]));
  if (rtx_equal_p (operands[1], operands[0]))
    return \"bicl2 %2,%0\";
  return \"bicl3 %2,%1,%0\";
}")

(define_insn ""
  [(set (match_operand:HI 0 "general_operand" "=g")
	(and:HI (match_operand:HI 1 "general_operand" "g")
		(match_operand:HI 2 "general_operand" "g")))]
  "GET_CODE (operands[2]) == CONST_INT"
  "*
{ operands[2] = gen_rtx (CONST_INT, VOIDmode, 0xffff & ~INTVAL (operands[2]));
  if (rtx_equal_p (operands[1], operands[0]))
    return \"bicw2 %2,%0\";
  return \"bicw3 %2,%1,%0\";
}")

(define_insn ""
  [(set (match_operand:QI 0 "general_operand" "=g")
	(and:QI (match_operand:QI 1 "general_operand" "g")
		(match_operand:QI 2 "general_operand" "g")))]
  "GET_CODE (operands[2]) == CONST_INT"
  "*
{ operands[2] = gen_rtx (CONST_INT, VOIDmode, 0xff & ~INTVAL (operands[2]));
  if (rtx_equal_p (operands[1], operands[0]))
    return \"bicb2 %2,%0\";
  return \"bicb3 %2,%1,%0\";
}")

;;- Bit set instructions.

(define_insn "iorsi3"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(ior:SI (match_operand:SI 1 "general_operand" "g")
		(match_operand:SI 2 "general_operand" "g")))]
  ""
  "*
{
  if (rtx_equal_p (operands[0], operands[1]))
    return \"bisl2 %2,%0\";
  if (rtx_equal_p (operands[0], operands[2]))
    return \"bisl2 %1,%0\";
  return \"bisl3 %2,%1,%0\";
}")

(define_insn "iorhi3"
  [(set (match_operand:HI 0 "general_operand" "=g")
	(ior:HI (match_operand:HI 1 "general_operand" "g")
		(match_operand:HI 2 "general_operand" "g")))]
  ""
  "*
{
  if (rtx_equal_p (operands[0], operands[1]))
    return \"bisw2 %2,%0\";
  if (rtx_equal_p (operands[0], operands[2]))
    return \"bisw2 %1,%0\";
  return \"bisw3 %2,%1,%0\";
}")

(define_insn "iorqi3"
  [(set (match_operand:QI 0 "general_operand" "=g")
	(ior:QI (match_operand:QI 1 "general_operand" "g")
		(match_operand:QI 2 "general_operand" "g")))]
  ""
  "*
{
  if (rtx_equal_p (operands[0], operands[1]))
    return \"bisb2 %2,%0\";
  if (rtx_equal_p (operands[0], operands[2]))
    return \"bisb2 %1,%0\";
  return \"bisb3 %2,%1,%0\";
}")

;;- xor instructions.

(define_insn "xorsi3"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(xor:SI (match_operand:SI 1 "general_operand" "g")
		(match_operand:SI 2 "general_operand" "g")))]
  ""
  "*
{
  if (rtx_equal_p (operands[0], operands[1]))
    return \"xorl2 %2,%0\";
  if (rtx_equal_p (operands[0], operands[2]))
    return \"xorl2 %1,%0\";
  return \"xorl3 %2,%1,%0\";
}")

(define_insn "xorhi3"
  [(set (match_operand:HI 0 "general_operand" "=g")
	(xor:HI (match_operand:HI 1 "general_operand" "g")
		(match_operand:HI 2 "general_operand" "g")))]
  ""
  "*
{
  if (rtx_equal_p (operands[0], operands[1]))
    return \"xorw2 %2,%0\";
  if (rtx_equal_p (operands[0], operands[2]))
    return \"xorw2 %1,%0\";
  return \"xorw3 %2,%1,%0\";
}")

(define_insn "xorqi3"
  [(set (match_operand:QI 0 "general_operand" "=g")
	(xor:QI (match_operand:QI 1 "general_operand" "g")
		(match_operand:QI 2 "general_operand" "g")))]
  ""
  "*
{
  if (rtx_equal_p (operands[0], operands[1]))
    return \"xorb2 %2,%0\";
  if (rtx_equal_p (operands[0], operands[2]))
    return \"xorb2 %1,%0\";
  return \"xorb3 %2,%1,%0\";
}")

(define_insn "negdf2"
  [(set (match_operand:DF 0 "general_operand" "=g")
	(neg:DF (match_operand:DF 1 "general_operand" "gF")))]
  ""
  "mnegd %1,%0")

(define_insn "negsf2"
  [(set (match_operand:SF 0 "general_operand" "=g")
	(neg:SF (match_operand:SF 1 "general_operand" "gF")))]
  ""
  "mnegf %1,%0")

(define_insn "negsi2"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(neg:SI (match_operand:SI 1 "general_operand" "g")))]
  ""
  "mnegl %1,%0")

(define_insn "neghi2"
  [(set (match_operand:HI 0 "general_operand" "=g")
	(neg:HI (match_operand:HI 1 "general_operand" "g")))]
  ""
  "mnegw %1,%0")

(define_insn "negqi2"
  [(set (match_operand:QI 0 "general_operand" "=g")
	(neg:QI (match_operand:QI 1 "general_operand" "g")))]
  ""
  "mnegb %1,%0")

(define_insn "one_cmplsi2"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(not:SI (match_operand:SI 1 "general_operand" "g")))]
  ""
  "mcoml %1,%0")

(define_insn "one_cmplhi2"
  [(set (match_operand:HI 0 "general_operand" "=g")
	(not:HI (match_operand:HI 1 "general_operand" "g")))]
  ""
  "mcomw %1,%0")

(define_insn "one_cmplqi2"
  [(set (match_operand:QI 0 "general_operand" "=g")
	(not:QI (match_operand:QI 1 "general_operand" "g")))]
  ""
  "mcomb %1,%0")

(define_insn "ashlsi3"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(ashift:SI (match_operand:SI 1 "general_operand" "g")
		   (match_operand:QI 2 "general_operand" "g")))]
  ""
  "ashl %2,%1,%0")

(define_insn "ashldi3"
  [(set (match_operand:DI 0 "general_operand" "=g")
	(ashift:DI (match_operand:DI 1 "general_operand" "g")
		   (match_operand:QI 2 "general_operand" "g")))]
  ""
  "ashq %2,%1,%0")

(define_insn "rotlsi3"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(rotate:SI (match_operand:SI 1 "general_operand" "g")
		   (match_operand:QI 2 "general_operand" "g")))]
  ""
  "rotl %2,%1,%0")

(define_insn "rotldi3"
  [(set (match_operand:DI 0 "general_operand" "=g")
	(rotate:DI (match_operand:DI 1 "general_operand" "g")
		   (match_operand:QI 2 "general_operand" "g")))]
  ""
  "rotq %2,%1,%0")

(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=g")
	(mult:SI (plus:SI (match_operand:SI 1 "general_operand" "g")
			  (match_operand:SI 2 "general_operand" "g"))
		 (match_operand:SI 3 "general_operand" "g")))]
  ""
  "index %1,$??,$??,%3,%2,%0")

(define_insn ""
  [(set (cc0)
	(minus
	 (sign_extract:SI (match_operand:QI 0 "general_operand" "g")
			  (match_operand:SI 1 "general_operand" "g")
			  (match_operand:SI 2 "general_operand" "g"))
	 (match_operand:SI 3 "general_operand" "g")))]
  ""
  "cmpv %2,%1,%0,%3")

(define_insn ""
  [(set (cc0)
	(minus
	 (zero_extract:SI (match_operand:QI 0 "general_operand" "g")
			  (match_operand:SI 1 "general_operand" "g")
			  (match_operand:SI 2 "general_operand" "g"))
	 (match_operand:SI 3 "general_operand" "g")))]
  ""
  "cmpzv %2,%1,%0,%3")

(define_insn "extv"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(sign_extract:SI (match_operand:QI 1 "general_operand" "g")
			 (match_operand:SI 2 "general_operand" "g")
			 (match_operand:SI 3 "general_operand" "g")))]
  ""
  "extv %3,%2,%1,%0")

(define_insn "extzv"
  [(set (match_operand:SI 0 "general_operand" "=g")
	(zero_extract:SI (match_operand:QI 1 "general_operand" "g")
			 (match_operand:SI 2 "general_operand" "g")
			 (match_operand:SI 3 "general_operand" "g")))]
  ""
  "extzv %3,%2,%1,%0")

(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=g")
	(sign_extract:SI (match_operand:SI 1 "general_operand" "r")
			 (match_operand:SI 2 "general_operand" "g")
			 (match_operand:SI 3 "general_operand" "g")))]
  ""
  "extv %3,%2,%1,%0")

(define_insn ""
  [(set (match_operand:SI 0 "general_operand" "=g")
	(zero_extract:SI (match_operand:SI 1 "general_operand" "r")
			 (match_operand:SI 2 "general_operand" "g")
			 (match_operand:SI 3 "general_operand" "g")))]
  ""
  "extzv %3,%2,%1,%0")

(define_insn "insv"
  [(set (zero_extract:SI (match_operand:QI 0 "general_operand" "=g")
			 (match_operand:SI 1 "general_operand" "g")
			 (match_operand:SI 2 "general_operand" "g"))
	(match_operand:SI 3 "general_operand" "g"))]
  ""
  "insv %3,%2,%1,%0")

(define_insn ""
  [(set (zero_extract:SI (match_operand:SI 0 "general_operand" "=r")
			 (match_operand:SI 1 "general_operand" "g")
			 (match_operand:SI 2 "general_operand" "g"))
	(match_operand:SI 3 "general_operand" "g"))]
  ""
  "insv %3,%2,%1,%0")

(define_insn "jump"
  [(set (pc)
	(label_ref (match_operand 0 "" "")))]
  ""
  "jbr %l0")

(define_insn "beq"
  [(set (pc)
	(if_then_else (eq (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "jeql %l0")

(define_insn "bne"
  [(set (pc)
	(if_then_else (ne (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "jneq %l0")

(define_insn "bgt"
  [(set (pc)
	(if_then_else (gt (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "jgtr %l0")

(define_insn "bgtu"
  [(set (pc)
	(if_then_else (gtu (cc0)
			   (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "jgtru %l0")

(define_insn "blt"
  [(set (pc)
	(if_then_else (lt (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "jlss %l0")

(define_insn "bltu"
  [(set (pc)
	(if_then_else (ltu (cc0)
			   (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "jlssu %l0")

(define_insn "bge"
  [(set (pc)
	(if_then_else (ge (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "jgeq %l0")

(define_insn "bgeu"
  [(set (pc)
	(if_then_else (geu (cc0)
			   (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "jgequ %l0")

(define_insn "ble"
  [(set (pc)
	(if_then_else (le (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "jleq %l0")

(define_insn "bleu"
  [(set (pc)
	(if_then_else (leu (cc0)
			   (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "jlequ %l0")

(define_insn ""
  [(set (pc)
	(if_then_else (eq (cc0)
			  (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "jneq %l0")

(define_insn ""
  [(set (pc)
	(if_then_else (ne (cc0)
			  (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "jeql %l0")

(define_insn ""
  [(set (pc)
	(if_then_else (gt (cc0)
			  (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "jleq %l0")

(define_insn ""
  [(set (pc)
	(if_then_else (gtu (cc0)
			   (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "jlequ %l0")

(define_insn ""
  [(set (pc)
	(if_then_else (lt (cc0)
			  (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "jgeq %l0")

(define_insn ""
  [(set (pc)
	(if_then_else (ltu (cc0)
			   (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "jgequ %l0")

(define_insn ""
  [(set (pc)
	(if_then_else (ge (cc0)
			  (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "jlss %l0")

(define_insn ""
  [(set (pc)
	(if_then_else (geu (cc0)
			   (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "jlssu %l0")

(define_insn ""
  [(set (pc)
	(if_then_else (le (cc0)
			  (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "jgtr %l0")

(define_insn ""
  [(set (pc)
	(if_then_else (leu (cc0)
			   (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "jgtru %l0")

;; Recognize jbs and jbc instructions.

(define_insn ""
  [(set (pc)
	(if_then_else
	 (ne (sign_extract:SI (match_operand:QI 0 "general_operand" "g")
			      (const_int 1)
			      (match_operand:SI 1 "general_operand" "g"))
	     (const_int 0))
	 (label_ref (match_operand 2 "" ""))
	 (pc)))]
  ""
  "jbs %1,%0,%l2")

(define_insn ""
  [(set (pc)
	(if_then_else
	 (eq (sign_extract:SI (match_operand:QI 0 "general_operand" "g")
			      (const_int 1)
			      (match_operand:SI 1 "general_operand" "g"))
	     (const_int 0))
	 (label_ref (match_operand 2 "" ""))
	 (pc)))]
  ""
  "jbc %1,%0,%l2")

(define_insn ""
  [(set (pc)
	(if_then_else
	 (ne (sign_extract:SI (match_operand:QI 0 "general_operand" "g")
			      (const_int 1)
			      (match_operand:SI 1 "general_operand" "g"))
	     (const_int 0))
	 (pc)
	 (label_ref (match_operand 2 "" ""))))]
  ""
  "jbc %1,%0,%l2")

(define_insn ""
  [(set (pc)
	(if_then_else
	 (eq (sign_extract:SI (match_operand:QI 0 "general_operand" "g")
			      (const_int 1)
			      (match_operand:SI 1 "general_operand" "g"))
	     (const_int 0))
	 (pc)
	 (label_ref (match_operand 2 "" ""))))]
  ""
  "jbs %1,%0,%l2")

(define_insn ""
  [(set (pc)
	(if_then_else
	 (ne (sign_extract:SI (match_operand:SI 0 "general_operand" "r")
			      (const_int 1)
			      (match_operand:SI 1 "general_operand" "g"))
	     (const_int 0))
	 (label_ref (match_operand 2 "" ""))
	 (pc)))]
  ""
  "jbs %1,%0,%l2")

(define_insn ""
  [(set (pc)
	(if_then_else
	 (eq (sign_extract:SI (match_operand:SI 0 "general_operand" "r")
			      (const_int 1)
			      (match_operand:SI 1 "general_operand" "g"))
	     (const_int 0))
	 (label_ref (match_operand 2 "" ""))
	 (pc)))]
  ""
  "jbc %1,%0,%l2")

(define_insn ""
  [(set (pc)
	(if_then_else
	 (ne (sign_extract:SI (match_operand:SI 0 "general_operand" "r")
			      (const_int 1)
			      (match_operand:SI 1 "general_operand" "g"))
	     (const_int 0))
	 (pc)
	 (label_ref (match_operand 2 "" ""))))]
  ""
  "jbc %1,%0,%l2")

(define_insn ""
  [(set (pc)
	(if_then_else
	 (eq (sign_extract:SI (match_operand:SI 0 "general_operand" "r")
			      (const_int 1)
			      (match_operand:SI 1 "general_operand" "g"))
	     (const_int 0))
	 (pc)
	 (label_ref (match_operand 2 "" ""))))]
  ""
  "jbs %1,%0,%l2")

(define_insn ""
  [(set (pc)
	(if_then_else
	 (ne (and:SI (match_operand:SI 0 "general_operand" "g")
		     (const_int 1))
	     (const_int 0))
	 (label_ref (match_operand 1 "" ""))
	 (pc)))]
  ""
  "jlbs %0,%l1")

(define_insn ""
  [(set (pc)
	(if_then_else
	 (eq (and:SI (match_operand:SI 0 "general_operand" "g")
		     (const_int 1))
	     (const_int 0))
	 (label_ref (match_operand 1 "" ""))
	 (pc)))]
  ""
  "jlbc %0,%l1")

(define_insn ""
  [(set (pc)
	(if_then_else
	 (ne (and:SI (match_operand:SI 0 "general_operand" "g")
		     (const_int 1))
	     (const_int 0))
	 (pc)
	 (label_ref (match_operand 1 "" ""))))]
  ""
  "jlbc %0,%l1")

(define_insn ""
  [(set (pc)
	(if_then_else
	 (eq (and:SI (match_operand:SI 0 "general_operand" "g")
		     (const_int 1))
	     (const_int 0))
	 (pc)
	 (label_ref (match_operand 1 "" ""))))]
  ""
  "jlbs %0,%l1")

;; These four entries allow a jlbc or jlbs to be made
;; by combination with a bic.
(define_insn ""
  [(set (pc)
	(if_then_else
	 (ne (and:SI (match_operand:SI 0 "general_operand" "g")
		     (not:SI (const_int -2)))
	     (const_int 0))
	 (label_ref (match_operand 1 "" ""))
	 (pc)))]
  ""
  "jlbs %0,%l1")

(define_insn ""
  [(set (pc)
	(if_then_else
	 (eq (and:SI (match_operand:SI 0 "general_operand" "g")
		     (not:SI (const_int -2)))
	     (const_int 0))
	 (label_ref (match_operand 1 "" ""))
	 (pc)))]
  ""
  "jlbc %0,%l1")

(define_insn ""
  [(set (pc)
	(if_then_else
	 (ne (and:SI (match_operand:SI 0 "general_operand" "g")
		     (not:SI (const_int -2)))
	     (const_int 0))
	 (pc)
	 (label_ref (match_operand 1 "" ""))))]
  ""
  "jlbc %0,%l1")

(define_insn ""
  [(set (pc)
	(if_then_else
	 (eq (and:SI (match_operand:SI 0 "general_operand" "g")
		     (not:SI (const_int -2)))
	     (const_int 0))
	 (pc)
	 (label_ref (match_operand 1 "" ""))))]
  ""
  "jlbs %0,%l1")

;; Subtract-and-jump and Add-and-jump insns.
;; These are not used when output is for the Unix assembler
;; because it does not know how to modify them to reach far.

;; Normal sob insns.

(define_insn ""
  [(set (pc)
	(if_then_else
	 (gt (minus:SI (match_operand:SI 0 "general_operand" "+g")
		       (const_int 1))
	     (const_int 0))
	 (label_ref (match_operand 1 "" ""))
	 (pc)))
   (set (match_dup 0)
	(minus:SI (match_dup 0)
		  (const_int 1)))]
  "!TARGET_UNIX_ASM"
  "jsobgtr %0,%l1")

(define_insn ""
  [(set (pc)
	(if_then_else
	 (ge (minus:SI (match_operand:SI 0 "general_operand" "+g")
		       (const_int 1))
	     (const_int 0))
	 (label_ref (match_operand 1 "" ""))
	 (pc)))
   (set (match_dup 0)
	(minus:SI (match_dup 0)
		  (const_int 1)))]
  "!TARGET_UNIX_ASM"
  "jsobgeq %0,%l1")

;; Reversed sob insns.

(define_insn ""
  [(set (pc)
	(if_then_else
	 (le (minus:SI (match_operand:SI 0 "general_operand" "+g")
		       (const_int 1))
	     (const_int 0))
	 (pc)
	 (label_ref (match_operand 1 "" ""))))
   (set (match_dup 0)
	(minus:SI (match_dup 0)
		  (const_int 1)))]
  "!TARGET_UNIX_ASM"
  "jsobgtr %0,%l1")

(define_insn ""
  [(set (pc)
	(if_then_else
	 (lt (minus:SI (match_operand:SI 0 "general_operand" "+g")
		       (const_int 1))
	     (const_int 0))
	 (pc)
	 (label_ref (match_operand 1 "" ""))))
   (set (match_dup 0)
	(minus:SI (match_dup 0)
		  (const_int 1)))]
  "!TARGET_UNIX_ASM"
  "jsobgeq %0,%l1")

;; Normal aob insns.
(define_insn ""
  [(set (pc)
	(if_then_else
	 (lt (minus (plus:SI (match_operand:SI 0 "general_operand" "+g")
			     (const_int 1))
		    (match_operand:SI 1 "general_operand" "g"))
	     (const_int 0))
	 (label_ref (match_operand 2 "" ""))
	 (pc)))
   (set (match_dup 0)
	(plus:SI (match_dup 0)
		 (const_int 1)))]
  "!TARGET_UNIX_ASM"
  "jaoblss %1,%0,%l2")

(define_insn ""
  [(set (pc)
	(if_then_else
	 (le (minus (plus:SI (match_operand:SI 0 "general_operand" "+g")
			     (const_int 1))
		    (match_operand:SI 1 "general_operand" "g"))
	     (const_int 0))
	 (label_ref (match_operand 2 "" ""))
	 (pc)))
   (set (match_dup 0)
	(plus:SI (match_dup 0)
		 (const_int 1)))]
  "!TARGET_UNIX_ASM"
  "jaobleq %1,%0,%l2")

;; Reverse aob insns.
(define_insn ""
  [(set (pc)
	(if_then_else
	 (ge (minus (plus:SI (match_operand:SI 0 "general_operand" "+g")
			     (const_int 1))
		    (match_operand:SI 1 "general_operand" "g"))
	     (const_int 0))
	 (pc)
	 (label_ref (match_operand 2 "" ""))))
   (set (match_dup 0)
	(plus:SI (match_dup 0)
		 (const_int 1)))]
  "!TARGET_UNIX_ASM"
  "jaoblss %1,%0,%l2")

(define_insn ""
  [(set (pc)
	(if_then_else
	 (gt (minus (plus:SI (match_operand:SI 0 "general_operand" "+g")
			     (const_int 1))
		    (match_operand:SI 1 "general_operand" "g"))
	     (const_int 0))
	 (pc)
	 (label_ref (match_operand 2 "" ""))))
   (set (match_dup 0)
	(plus:SI (match_dup 0)
		 (const_int 1)))]
  "!TARGET_UNIX_ASM"
  "jaobleq %1,%0,%l2")

(define_insn "call"
  [(call (match_operand:QI 0 "general_operand" "g")
	 (match_operand:QI 1 "general_operand" "g"))]
  ""
  "calls %1,%0")

(define_insn "return"
  [(return)]
  ""
  "ret")

(define_insn "casesi"
  [(set (pc)
	(if_then_else (le (minus:SI (match_operand:SI 0 "general_operand" "g")
				    (match_operand:SI 1 "general_operand" "g"))
			  (match_operand:SI 2 "general_operand" "g"))
		      (plus:SI (label_ref:SI (match_operand 3 "" ""))
			       (sign_extend:SI
				(mem:HI (plus:SI (pc)
						 (minus:SI (match_dup 0)
							   (match_dup 1))))))
		      (pc)))]
  ""
  "casel %0,%1,%2")

;; This arises from the preceding by simplification if operand 1 is zero.
(define_insn ""
  [(set (pc)
	(if_then_else (le (match_operand:SI 0 "general_operand" "g")
			  (match_operand:SI 1 "general_operand" "g"))
		      (plus:SI (label_ref:SI (match_operand 3 "" ""))
			       (sign_extend:SI
				(mem:HI (plus:SI (pc)
						 (match_dup 0)))))
		      (pc)))]
  ""
  "casel %0,$0,%1")

;;- Local variables:
;;- mode:emacs-lisp
;;- comment-start: ";;- "
;;- eval: (set-syntax-table (copy-sequence (syntax-table)))
;;- eval: (modify-syntax-entry ?[ "(]")
;;- eval: (modify-syntax-entry ?] ")[")
;;- eval: (modify-syntax-entry ?{ "(}")
;;- eval: (modify-syntax-entry ?} "){")
;;- End:
