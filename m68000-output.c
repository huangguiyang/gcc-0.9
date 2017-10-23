/* Subroutines for insn-output.c for Motorola 68000 family.
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


char *
output_btst (operands, countop, dataop, insn, signpos)
     rtx *operands;
     rtx countop, dataop;
     rtx insn;
     int signpos;
{
  operands[0] = countop;
  operands[1] = dataop;
  if (GET_CODE (countop) == CONST_INT)
    {
      register int count = INTVAL (countop);
      if (count == signpos)
	cc_status.flags = CC_NOT_POSITIVE | CC_Z_IN_NOT_N;
      else
	cc_status.flags = CC_NOT_NEGATIVE | CC_Z_IN_NOT_N;

      if (count == 31
	  && next_insn_tests_no_inequality (insn))
	return "tstl %1";
      if (count == 15
	  && next_insn_tests_no_inequality (insn))
	return "tstw %1";
      if (count == 7
	  && next_insn_tests_no_inequality (insn))
	return "tstb %1";

      cc_status.flags = CC_NOT_NEGATIVE;
    }
  return "btst %0,%1";
}

char *
output_move_double (operands)
     rtx *operands;
{
  int addr0 = 0, addr1 = 0;
  int inc0 = 0, inc1 = 0;
  rtx reg0 = 0, reg1 = 0;
  char *finish0, *finish1;
  int good1 = 0;

  if (CONSTANT_ADDRESS_P (operands[1]))
    {
      if (ADDRESS_REG_P (operands[0]))
	output_asm_insn ("movw #0,%0", operands);
      else
	output_asm_insn ("clrl %0", operands);
    }
  else if (GET_CODE (operands[1]) == CONST_DOUBLE)
    {
      rtx xoperands[2];
      xoperands[0] = operands[0];
      xoperands[1] = gen_rtx (CONST_INT, VOIDmode, XINT (operands[1], 0));
      if (XINT (operands[1], 0) == 0)
	output_asm_insn ("clrl %0", xoperands);
      else
	output_asm_insn ("movl %1,%0", xoperands);
    }
  else
    output_asm_insn ("movl %1,%0", operands);

  /* If operand 0 is not offsettable, load its address
     into a0.  Then likewise for a1.  */
  if (!REG_P (operands[0]) && !offsetable_address_p (operands[0]))
    {
      if (GET_CODE (operands[0]) == MEM
	  && GET_CODE (XEXP (operands[0], 0)) == POST_INC)
	{
	  inc0 = 4;
	  reg0 = XEXP (XEXP (operands[0], 0), 0);
	  operands[0] = gen_rtx (MEM, DFmode, reg0);
	}
      if (GET_CODE (operands[0]) == MEM
	  && GET_CODE (XEXP (operands[0], 0)) == POST_INC)
	{
	  inc0 = -4;
	  reg0 = XEXP (XEXP (operands[0], 0), 0);
	  operands[0] = gen_rtx (MEM, DFmode, gen_rtx (PLUS, SImode, reg0,
						       gen_rtx (CONST_INT, VOIDmode, 4)));
	}
      else
	{
	  output_asm_insn ("movl a0,sp@-;lea %0,a0", operands);
	  addr0 = 1;
	  operands[0] = gen_rtx (MEM, DFmode, gen_rtx (REG, SImode, 8));
	}
    }
  if (!REG_P (operands[1]) && !offsetable_address_p (operands[1]))
    {
      if (GET_CODE (operands[1]) == MEM
	  && GET_CODE (XEXP (operands[1], 0)) == POST_INC)
	{
	  inc1 = 4;
	  reg1 = XEXP (XEXP (operands[1], 0), 0);
	  operands[1] = gen_rtx (MEM, DFmode, reg1);
	}
      if (GET_CODE (operands[1]) == MEM
	  && GET_CODE (XEXP (operands[1], 0)) == POST_INC)
	{
	  inc1 = -4;
	  reg1 = XEXP (XEXP (operands[1], 0), 0);
	  operands[1] = gen_rtx (MEM, DFmode, gen_rtx (PLUS, SImode, reg1,
						       gen_rtx (CONST_INT, VOIDmode, 4)));
	}
      else if (GET_CODE (operands[1]) == CONST_DOUBLE)
	{
	  operands[1] = gen_rtx (CONST_INT, VOIDmode, XINT (operands[1], 1));
	  good1 = 1;
	}
      else
	{
	  output_asm_insn ("movl a1,sp@-;lea %1,a1", operands);
	  addr1 = 1;
	  operands[1] = gen_rtx (MEM, DFmode, gen_rtx (REG, SImode, 9));
	}
    }

  /* ADDR1 nonzero if must restore a1; likewise ADDR0 for a0.  */
  /* Now set up the operands for the second-half words,
     assuming both operands are offsetable.  */

  if (REG_P (operands[0]))
    operands[0] = gen_rtx (REG, SImode, REGNO (operands[0]) + 1);
  else
    operands[0] = adj_offsetable_operand (operands[0], 4);
  if (good1)
    ;
  else if (REG_P (operands[1]))
    operands[1] = gen_rtx (REG, SImode, REGNO (operands[1]) + 1);
  else
    operands[1] = adj_offsetable_operand (operands[1], 4);

  /* Now copy the second word.  */
  {
    char *s;
    if (GET_CODE (operands[1]) == CONST_INT && INTVAL (operands[1]) == 0)
      s = "clrl %0";
    else
      s = "movl %1,%0";

    if (!addr0 && !addr1 && inc0 == 0 && inc1 == 0)
      return s;
    output_asm_insn (s, operands);
  }

  /* Now restore regs we saved or increment or decrement regs that need it.  */
  operands[0] = reg0;
  operands[1] = reg1;
  if (addr0)
    finish0 = "movl sp@+,a0";
  else if (inc0 > 0)
    finish0 = "addql #4,%0";
  else if (inc0 < 0)
    finish0 = "subql #4,%0";
  else
    finish0 = 0;
  if (addr1)
    finish1 = "movl sp@+,a1";
  else if (inc1 > 0)
    finish1 = "addql #4,%1";
  else if (inc1 < 0)
    finish1 = "subql #4,%1";
  else
    finish1 = 0;
  if (finish0 == 0)
    return finish1;
  if (finish1 != 0)
    output_asm_insn (finish1, operands);
  return finish0;
}

char *
output_move_const_double (operands)
     rtx *operands;
{
  int code = standard_68881_constant_p (operands[1]);

  if (code != 0)
    {
      static char buf[40];

      sprintf (buf, "fmovecr #0x%x,%%0", code & 0xff);
      return buf;
    }
  return "fmoved %1,%0";
}

/* Return nonzero if X, a CONST_DOUBLE, has a value that we can get
   from the "fmovecr" instruction.
   The value, anded with 0xff, gives the code to use in fmovecr
   to get the desired constant.  */

int
standard_68881_constant_p (x)
     rtx x;
{
  union {double d; int i[2];} u;
  register double d;
  u.i[0] = XINT (x, 0);
  u.i[1] = XINT (x, 1);
  d = u.d;

  if (d == 0)
    return 0x0f;
  /* Note: there are various other constants available
     but it is a nuisance to put in their values here.  */
  if (d == 1)
    return 0x32;
  if (d == 10)
    return 0x33;
  if (d == 100)
    return 0x34;
  if (d == 10000)
    return 0x35;
  if (d == 1e8)
    return 0x36;
  if (d == 1e16)
    return 0x37;
  if (d == 1e32)
    return 0x38;
  if (d == 1e64)
    return 0x39;
  if (d == 1e128)
    return 0x3a;
  if (d == 1e256)
    return 0x3b;
  return 0;
}
