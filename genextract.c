/* Generate code from machine description to extract operands from insn as rtl.
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

/* Number instruction patterns handled, starting at 0 for first one.  */

int insn_code_number;

/* Number the occurrences of MATCH_DUP in each instruction,
   starting at 0 for the first occurrence.  */

int dup_count;

/* While tree-walking an instruction pattern, we keep a chain
   of these `struct link's to record how to get down to the
   current position.  In each one, POS is the operand number
   or vector element number, and VEC is nonzero for a vector.  */

struct link
{
  struct link *next;
  int pos;
  int vec;
};

void
gen_insn (insn)
     rtx insn;
{
  register int i;

  dup_count = 0;

  /* Output the function name and argument declaration.  */
  /* It would be cleaner to make `int' the return type
     but 4.2 vax compiler doesn't accept that in the array
     that these functions are supposed to go in.  */
  printf ("int\nextract_%d (insn)\n     rtx insn;\n", insn_code_number);
  printf ("{\n");

  /* Walk the insn's pattern, remembering at all times the path
     down to the walking point.  */

  if (XVECLEN (insn, 1) == 1)
    walk_rtx (XVECEXP (insn, 1, 0), 0);
  else
    for (i = XVECLEN (insn, 1) - 1; i >= 0; i--)
      {
	struct link link;
	link.next = 0;
	link.pos = i;
	link.vec = 1;
	walk_rtx (XVECEXP (insn, 1, i), &link);
      }
  printf ("}\n\n");
}

walk_rtx (x, path)
     rtx x;
     struct link *path;
{
  register RTX_CODE code = GET_CODE (x);
  register int i;
  register int len;
  register char *fmt;
  struct link link;

  switch (code)
    {
    case PC:
    case CC0:
    case CONST_INT:
    case SYMBOL_REF:
      return;

    case MATCH_OPERAND:
      printf ("  recog_operand[%d] = *(recog_operand_loc[%d]\n    = &",
	      XINT (x, 0), XINT (x, 0));
      print_path (path);
      printf (");\n");
      break;

    case MATCH_DUP:
      printf ("  recog_dup_loc[%d] = &", dup_count);
      print_path (path);
      printf (";\n");
      printf ("  recog_dup_num[%d] = %d;\n", dup_count, XINT (x, 0));
      dup_count++;
      break;

    case ADDRESS:
      walk_rtx (XEXP (x, 0), path);
      return;
    }

  link.next = path;
  link.vec = 0;
  fmt = GET_RTX_FORMAT (code);
  len = GET_RTX_LENGTH (code);
  for (i = 0; i < len; i++)
    if (fmt[i] == 'e' || fmt[i] == 'u')
      {
	link.pos = i;
	walk_rtx (XEXP (x, i), &link);
      }
}

/* Given a PATH, representing a path down the instruction's
   pattern from the root to a certain point, output code to
   evaluate to the rtx at that point.  */

print_path (path)
     struct link *path;
{
  if (path == 0)
    printf ("insn");
  else if (path->vec)
    {
      printf ("XVECEXP (");
      print_path (path->next);
      printf (", 0, %d)", path->pos);
    }
  else
    {
      printf ("XEXP (");
      print_path (path->next);
      printf (", %d)", path->pos);
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
  register int c, i;

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

  printf ("/* Generated automatically by the program `genextract'\n\
from the machine description file `md'.  */\n\n");

  printf ("#include \"rtl.h\"\n\n");

  printf ("extern rtx recog_operand[];\n");
  printf ("extern rtx *recog_operand_loc[];\n");
  printf ("extern rtx *recog_dup_loc[];\n");
  printf ("extern char recog_dup_num[];\n\n");

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

  printf ("int (*insn_extract_fn[]) () =\n{ ");
  for (i = 0; i < insn_code_number; i++)
    {
      if (i % 4 != 0)
	printf (", ");
      else if (i != 0)
	printf (",\n  ");
      printf ("extract_%d", i);
    }
  printf ("\n};\n\n");

  printf ("void\ninsn_extract (insn)\n");
  printf ("     rtx insn;\n");
  printf ("{\n  if (INSN_CODE (insn) == -1) abort ();\n");
  printf ("  (*insn_extract_fn[INSN_CODE (insn)]) (PATTERN (insn));\n}\n");
  return 0;
}
