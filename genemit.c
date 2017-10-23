/* Generate code from machine description to emit insns as rtl.
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


#include <stdio.h>
#include "rtl.h"
#include <obstack.h>

struct obstack obstack;
struct obstack *current_obstack = &obstack;

#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free free
extern int xmalloc ();
extern void free ();

void fatal ();

int max_opno;
int register_constraints;
int insn_code_number;

#define max(a, b) ((a) > (b) ? (a) : (b))

void
max_operand_1 (x)
     rtx x;
{
  register RTX_CODE code = GET_CODE (x);
  register int i;
  register int len;
  register char *fmt;

  if (code == MATCH_OPERAND && XSTR (x, 2) != 0)
    register_constraints = 1;
  if (code == MATCH_OPERAND || code == MATCH_DUP)
    max_opno = max (max_opno, XINT (x, 0));

  fmt = GET_RTX_FORMAT (code);
  len = GET_RTX_LENGTH (code);
  for (i = 0; i < len; i++)
    {
      if (fmt[i] == 'e' || fmt[i] == 'u')
	max_operand_1 (XEXP (x, i));
    }
}

int
max_operand_vec (insn, arg)
     rtx insn;
     int arg;
{
  register int len = XVECLEN (insn, arg);
  register int i;

  max_opno = -1;

  for (i = 0; i < len; i++)
    max_operand_1 (XVECEXP (insn, arg, i));

  return max_opno + 1;
}

void
print_code (code)
     RTX_CODE code;
{
  register char *p1;
  for (p1 = GET_RTX_NAME (code); *p1; p1++)
    {
      if (*p1 >= 'a' && *p1 <= 'z')
	putchar (*p1 + 'A' - 'a');
      else
	putchar (*p1);
    }
}

void
gen_exp (x)
     rtx x;
{
  register RTX_CODE code;
  register int i;
  register int len;
  register char *fmt;

  if (x == 0)
    {
      printf ("0");
      return;
    }

  code = GET_CODE (x);

  if (code == MATCH_OPERAND)
    {
      printf ("operand%d", XINT (x, 0));
      return;
    }

  if (code == MATCH_DUP)
    {
      printf ("copy_rtx (operand%d)", XINT (x, 0));
      return;
    }

  if (code == ADDRESS)
    fatal ("ADDRESS expression code used in named instruction pattern");

  if (code == PC)
    {
      printf ("pc_rtx");
      return;
    }

  if (code == CC0)
    {
      printf ("cc0_rtx");
      return;
    }

  if (code == CONST_INT && INTVAL (x) == 0)
    {
      printf ("const0_rtx");
      return;
    }

  if (code == CONST_INT && INTVAL (x) == 1)
    {
      printf ("const1_rtx");
      return;
    }

  printf ("gen_rtx (");
  print_code (code);
  printf (", %smode", GET_MODE_NAME (GET_MODE (x)));

  fmt = GET_RTX_FORMAT (code);
  len = GET_RTX_LENGTH (code);
  for (i = 0; i < len; i++)
    {
      if (fmt[i] == '0')
	break;
      printf (", ");
      if (fmt[i] == 'e' || fmt[i] == 'u')
	gen_exp (XEXP (x, i));
      else if (fmt[i] == 'i')
	printf ("%d", XINT (x, i));
      else
	abort ();
    }
  printf (")");
}  

void
gen_insn (insn)
     rtx insn;
{
  int operands;
  register int i;

  /* Don't mention instructions whose names are the null string.
     They are in the machine description just to be recognized.  */
  if (strlen (XSTR (insn, 0)) == 0)
    return;

  /* Find out how many operands this function has,
     and also whether any of them have register constraints.  */
  register_constraints = 0;
  operands = max_operand_vec (insn, 1);

  /* Output the function name and argument declarations.  */
  printf ("rtx\ngen_%s (", XSTR (insn, 0));
  for (i = 0; i < operands; i++)
    printf (i ? ", operand%d" : "operand%d", i);
  printf (")\n");
  for (i = 0; i < operands; i++)
    printf ("     rtx operand%d;\n", i);
  printf ("{\n");

  /* Output code to construct and return the rtl for the instruction body */

  if (XVECLEN (insn, 1) == 1)
    {
      printf ("  return ");
      gen_exp (XVECEXP (insn, 1, 0));
      printf (";\n}\n\n");
    }
  else
    {
      printf ("  return gen_rtx (PARALLEL, VOIDmode, gen_rtvec (%d", XVECLEN (insn, 1));
      for (i = 0; i < XVECLEN (insn, 1); i++)
	{
	  printf (",\n\t\t");
	  gen_exp (XVECEXP (insn, 1, i));
	}
      printf ("));\n}\n\n");
    }
}

xmalloc (size)
{
  register int val = malloc (size);

  if (val == 0)
    abort ();

  return val;
}

int
xrealloc (ptr, size)
     char *ptr;
     int size;
{
  int result = realloc (ptr, size);
  if (!result)
    abort ();
  return result;
}

void
fatal (s, a1, a2)
{
  fprintf (stderr, "genemit: ");
  fprintf (stderr, s, a1, a2);
  fprintf (stderr, "\n");
  exit (1);
}

main (argc, argv)
     int argc;
     char **argv;
{
  rtx desc;
  FILE *infile;
  extern rtx read_rtx ();
  register int c;

  obstack_begin (current_obstack, 4060);

  if (argc <= 1)
    fatal ("No input file name.");

  infile = fopen (argv[1], "r");
  if (infile == 0)
    {
      perror (argv[1]);
      exit (1);
    }

  init_rtl ();

  /* Assign sequential codes to all entries in the machine description
     in parallel with the tables in insn-output.c.  */

  insn_code_number = 0;

  printf ("/* Generated automatically by the program `genemit'\n\
from the machine description file `md'.  */\n\n");

  printf ("#include \"rtl.h\"\n");
  printf ("#include \"insn-config.h\"\n");
  printf ("\nextern char *insn_operand_constraint[][MAX_RECOG_OPERANDS];\n\n");

  /* Read the machine description.  */

  while (1)
    {
      c = read_skip_spaces (infile);
      if (c == EOF)
	break;
      ungetc (c, infile);

      desc = read_rtx (infile);
      gen_insn (desc);
      ++insn_code_number;
    }

  return 0;
}
