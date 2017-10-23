/* Generate code from to output assembler insns as recognized from rtl.
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


/* This program reads the machine description for the compiler target machine
   and produces a file containing three things:

   1, An array of strings `insn_template' which is indexed by insn code number
   and contains the template for output of that insn,

   2. An array of ints `insn_n_operands' which is indexed by insn code number
   and contains the number of distinct operands in the pattern for that insn,

   3. An array of ints `insn_n_dups' which is indexed by insn code number
   and contains the number of match_dup's that appear in the insn's pattern.
   This says how many elements of `recog_dup_loc' are significant
   after an insn has been recognized.

   4. An array of arrays of machine modes, `insn_operand_machine_mode',
   indexed first by insn code number and second by operand number,
   containing the machine mode that that operand is supposed to have.

   5. An array of arrays of operand constraint strings,
   `insn_operand_constraint',
   indexed first by insn code number and second by operand number,
   containing the constraint for that operand.
   This array is generated only if register constraints appear in 
   match_operand rtx's.

   6. An array of arrays of chars which indicate which operands of
   which insn patterns appear within ADDRESS rtx's.  This array is
   called `insn_operand_address_p' and is generated only if there
   are *no* register constraints in the match_operand rtx's.

   7. An array of functions `insn_gen_function' which, indexed
   by insn code number, gives the function to generate a body
   for that patter, given operands as arguments.

   8. A function `output_insn_hairy' which is called with two arguments
   (an insn code number and a vector of operand value rtx's)
   and returns a template to use for output of that insn.
   This is used only in the cases where the template is not constant.
   These cases are specified by a * at the beginning of the template string
   in the machine description.  They are identified for the sake of
   other parts of the compiler by a zero element in `insn_template'.

The code number of an insn is simply its position in the machine description;
code numbers are assigned sequentially to entries in the description,
starting with code number 0.

Thus, the following entry in the machine description

    (define_insn "clrdf"
      [(set (match_operand:DF 0 "general_operand" "")
	    (const_int 0))]
      ""
      "clrd %0")

assuming it is the 25th entry present, would cause
insn_template[24] to be "c;rd %0", and insn_n_operands[24] to be 1.
It would not make an case in output_insn_hairy because the template
given in the entry is a constant (it does not start with `*').  */

#include <stdio.h>
#include "rtl.h"
#include <obstack.h>

/* No instruction can have more operands than this.
   Sorry for this arbitrary limit, but what machine will
   have an instruction with this many operands?  */

#define MAX_MAX_OPERANDS 40

struct obstack obstack;
struct obstack *current_obstack = &obstack;

#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free free
extern int xmalloc ();
extern void free ();

void fatal ();

/* insns in the machine description are assigned sequential code numbers
   that are used by insn-recog.c (produced by genrecog) to communicate
   to insn-output.c (produced by this program).  */

int next_code_number;

/* Record in this chain all information that we will output,
   associated with the code number of the insn.  */

struct data
{
  int code_number;
  char *name;
  char *template;		/* string such as "movl %1,%0" */
  int n_operands;		/* Number of operands this insn recognizes */
  int n_dups;			/* Number times match_dup appears in pattern */
  struct data *next;
  char *constraints[MAX_MAX_OPERANDS];
  char address_p[MAX_MAX_OPERANDS];
  enum machine_mode modes[MAX_MAX_OPERANDS];
};

/* This variable points to the first link in the chain.  */

struct data *insn_data;

/* Pointer to the last link in the chain, so new elements
   can be added at the end.  */

struct data *end_of_insn_data;

/* Nonzero if any match_operand has a constraint string;
   implies that REGISTER_CONSTRAINTS will be defined
   for this machine description.  */

int have_constraints;

void
output_prologue ()
{

  printf ("/* Generated automatically by the program `genoutput'\n\
from the machine description file `md'.  */\n\n");

  printf ("#include \"config.h\"\n");
  printf ("#include \"rtl.h\"\n");
  printf ("#include \"regs.h\"\n");
  printf ("#include \"conditions.h\"\n");
  printf ("#include \"insn-flags.h\"\n");
  printf ("#include \"insn-config.h\"\n\n");

  printf ("extern rtx adj_offsetable_operand();\n\
extern void output_asm_insn();\n\n");

  printf ("#include \"aux-output.c\"\n\n");

  printf ("char *\noutput_insn_hairy (code_number, operands, insn)\n");
  printf ("     int code_number;\n");
  printf ("     rtx *operands;\n");
  printf ("     rtx insn;\n{\n  switch (code_number)\n    {\n");
}

void
output_epilogue ()
{
  register struct data *d;

  printf ("    default: abort ();\n");
  printf ("    }\n}\n");

  printf ("\nchar *insn_template[] =\n  {\n");
  for (d = insn_data; d; d = d->next)
    {
      if (d->template)
	printf ("    \"%s\",\n", d->template);
      else
	printf ("    0,\n");
    }
  printf ("  };\n");

  printf ("\nrtx (*insn_gen_function[]) () =\n  {\n");
  for (d = insn_data; d; d = d->next)
    {
      if (d->name)
	printf ("    gen_%s,\n", d->name);
      else
	printf ("    0,\n");
    }
  printf ("  };\n");

  printf ("\nint insn_n_operands[] =\n  {\n");
  for (d = insn_data; d; d = d->next)
    {
      printf ("    %d,\n", d->n_operands);
    }
  printf ("  };\n");

  printf ("\nint insn_n_dups[] =\n  {\n");
  for (d = insn_data; d; d = d->next)
    {
      printf ("    %d,\n", d->n_dups);
    }
  printf ("  };\n");

  if (have_constraints)
    {
      printf ("\nchar *insn_operand_constraint[][MAX_RECOG_OPERANDS] =\n  {\n");
      for (d = insn_data; d; d = d->next)
	{
	  register int i;
	  printf ("    {");
	  for (i = 0; i < d->n_operands; i++)
	    printf (" \"%s\",", d->constraints[i]);
	  if (d->n_operands == 0)
	    printf (" 0");
	  printf (" },\n");
	}
      printf ("  };\n");
    }
  else
    {
      printf ("\nchar insn_operand_address_p[][MAX_RECOG_OPERANDS] =\n  {\n");
      for (d = insn_data; d; d = d->next)
	{
	  register int i;
	  printf ("    {");
	  for (i = 0; i < d->n_operands; i++)
	    printf (" %d,", d->address_p[i]);
	  if (d->n_operands == 0)
	    printf (" 0");
	  printf (" },\n");
	}
      printf ("  };\n");
    }

  printf ("\nenum machine_mode insn_operand_mode[][MAX_RECOG_OPERANDS] =\n  {\n");
  for (d = insn_data; d; d = d->next)
    {
      register int i;
      printf ("    {");
      for (i = 0; i < d->n_operands; i++)
	printf (" %smode,", GET_MODE_NAME (d->modes[i]));
      if (d->n_operands == 0)
	printf (" VOIDmode");
      printf (" },\n");
    }
  printf ("  };\n");
}

/* scan_operands (X) stores in max_opno the largest operand
   number present in X, if that is larger than the previous
   value of max_opno.  It stores all the constraints in `constraints'
   and all the machine modes in `modes'.  */

int max_opno;
int num_dups;
char *constraints[MAX_MAX_OPERANDS];
char address_p[MAX_MAX_OPERANDS];
enum machine_mode modes[MAX_MAX_OPERANDS];

void
scan_operands (part, this_address_p)
     rtx part;
     int this_address_p;
{
  register int i, j;
  register RTX_CODE code = GET_CODE (part);
  register char *format_ptr;

  if (code == MATCH_OPERAND)
    {
      if (XINT (part, 0) > max_opno)
	max_opno = XINT (part, 0);
      if (max_opno > MAX_MAX_OPERANDS)
	fatal ("Too many operands (%d) in one instruction pattern.\n",
	       max_opno + 1);
      modes[max_opno] = GET_MODE (part);
      constraints[max_opno] = XSTR (part, 2);
      if (XSTR (part, 2) != 0 && *XSTR (part, 2) != 0)
	have_constraints = 1;
      address_p[max_opno] = this_address_p;
      return;
    }

  if (code == MATCH_DUP)
    {
      ++num_dups;
      return;
    }

  if (code == ADDRESS)
    {
      scan_operands (XEXP (part, 0), 1);
      return;
    }

  format_ptr = GET_RTX_FORMAT (GET_CODE (part));

  for (i = 0; i < GET_RTX_LENGTH (GET_CODE (part)); i++)
    switch (*format_ptr++)
      {
      case 'e':
	scan_operands (XEXP (part, i), 0);
	break;
      case 'E':
	if (XVEC (part, i) != NULL)
	  for (j = 0; j < XVECLEN (part, i); j++)
	    scan_operands (XVECEXP (part, i, j), 0);
	break;
      }
}

/* Read the next insn.  Assign its code number.
   Record on insn_data the template and the number of arguments.
   If the insn has a hairy output action, output it now.  */

void
gen_insn (insn)
     rtx insn;
{
  register struct data *d = (struct data *) xmalloc (sizeof (struct data));
  register int i;

  d->code_number = next_code_number++;
  if (XSTR (insn, 0)[0])
    d->name = XSTR (insn, 0);
  else
    d->name = 0;

  /* Build up the list in the same order as the insns are seen
     in the machine description.  */
  d->next = 0;
  if (end_of_insn_data)
    end_of_insn_data->next = d;
  else
    insn_data = d;

  end_of_insn_data = d;

  max_opno = -1;
  num_dups = 0;

  bzero (constraints, sizeof constraints);
  bzero (address_p, sizeof address_p);
  bzero (modes, sizeof modes);
  for (i = 0; i < XVECLEN (insn, 1); i++)
    scan_operands (XVECEXP (insn, 1, i), 0);
  d->n_operands = max_opno + 1;
  d->n_dups = num_dups;
  bcopy (constraints, d->constraints, sizeof constraints);
  bcopy (address_p, d->address_p, sizeof address_p);
  bcopy (modes, d->modes, sizeof modes);

  /* We need to consider only the instructions whose assembler code template
     starts with a *.  These are the ones where the template is really
     C code to run to decide on a template to use.
     So for all others just return now.  */

  if (XSTR (insn, 3)[0] != '*')
    {
      d->template = XSTR (insn, 3);
      return;
    }

  d->template = 0;
  printf ("    case %d:\n", d->code_number);
  printf ("%s\n", &(XSTR (insn, 3)[1]));
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

  obstack_init (current_obstack);

  if (argc <= 1)
    fatal ("No input file name.");

  infile = fopen (argv[1], "r");
  if (infile == 0)
    {
      perror (argv[1]);
      exit (1);
    }

  init_rtl ();

  output_prologue ();
  next_code_number = 0;
  have_constraints = 0;

  /* Read the machine description.  */

  while (1)
    {
      c = read_skip_spaces (infile);
      if (c == EOF)
	break;
      ungetc (c, infile);

      desc = read_rtx (infile);
      gen_insn (desc);
    }

  output_epilogue ();

  return 0;
}
