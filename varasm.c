/* Output variables, constants and external declarations, for GNU compiler.
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


/* This file handles generation of all the assembler code
   *except* the instructions of a function.
   This includes declarations of variables and their initial values.

   We also output the assembler code for constants stored in memory
   and are responsible for combining constants with the same value.  */

#include <stdio.h>
#include <stab.h>
#include "config.h"
#include "tree.h"
#include "c-tree.h"
#include "rtl.h"
#include "obstack.h"

#define MIN(a, b) ((a) < (b) ? (a) : (b))

extern FILE *asm_out_file;

extern struct obstack *current_obstack;
extern struct obstack permanent_obstack;
#define obstack_chunk_alloc xmalloc
extern int xmalloc ();

/* Number for making the label on the next
   constant that is stored in memory.  */

int const_labelno;

/* Number for making the label on the next
   static variable internal to a function.  */

int var_labelno;

extern FILE *asm_out_file;

static char *compare_constant_1 ();
static void record_constant_1 ();

/* Output a string of literal assembler code
   for an `asm' keyword used between functions.  */

void
assemble_asm (string)
     tree string;
{
  fprintf (asm_out_file, "\t%s\n", TREE_STRING_POINTER (string));
}

/* Output assembler code to make an external function name available.
   DECL is a FUNCTION_DECL.  */

void
assemble_function (decl)
     tree decl;
{
  if (TREE_PUBLIC (decl))
    fprintf (asm_out_file, ".globl _%s\n",
	     IDENTIFIER_POINTER (DECL_NAME (decl)));
}

/* Create the rtx to represent a function in calls to it.
   DECL is a FUNCTION_DECL node which describes which function.
   The rtl is stored in DECL.  */

void
make_function_rtl (decl)
     tree decl;
{
  DECL_RTL (decl)
    = gen_rtx (MEM, DECL_MODE (decl),
	       gen_rtx (SYMBOL_REF, Pmode,
			IDENTIFIER_POINTER (DECL_NAME (decl))));
}

/* Return size in bytes of an object of type TYPE.
   This size must be a constant.  */

static int
static_size_in_bytes (type)
     register tree type;
{
  register int size;

  if (! TREE_LITERAL (TYPE_SIZE (type)))
    abort ();
  size = TREE_INT_CST_LOW (TYPE_SIZE (type)) * TYPE_SIZE_UNIT (type);
  return (size + BITS_PER_UNIT - 1) / BITS_PER_UNIT;
}

/* Assemble everything that is needed for a variable declaration DECL.
   The variable must be in static storage or else external.
   TOP_LEVEL is nonzero if this variable has file scope.  */

assemble_variable (decl, top_level)
     tree decl;
     int top_level;
{
  register char *name = IDENTIFIER_POINTER (DECL_NAME (decl));
  register int i;

  /* Handle variables declared as undefined structure types.  */
  if (TREE_TYPE (decl) == error_mark_node)
    {
      DECL_RTL (decl) = gen_rtx (MEM, BLKmode, const0_rtx);
      return;
    }

  /* Can't use just the variable's own name for a variable
     whose scope is less than the whole file.
     Concatenate a distinguishing number.  */
  if (!top_level && !TREE_EXTERNAL (decl))
    {
      int labelno = var_labelno++;
      char *label;

      label = (char *) alloca (strlen (name) + 10);
      sprintf (label, "%s$%d", name, labelno);
      name = obstack_copy0 (current_obstack, label, strlen (label));
    }

  DECL_RTL (decl) = gen_rtx (MEM, DECL_MODE (decl),
			     gen_rtx (SYMBOL_REF, Pmode, name));

  /* No need to say anything for externals,
     since assembler considers all undefined symbols external.  */

  if (TREE_EXTERNAL (decl))
    return;

  if (DECL_INITIAL (decl) == 0)
    {
      if (! TREE_LITERAL (DECL_SIZE (decl)))
	abort ();
      if (TREE_PUBLIC (decl))
	fprintf (asm_out_file, ".comm _%s,%d\n", name,
		 TREE_INT_CST_LOW (DECL_SIZE (decl)) * DECL_SIZE_UNIT (decl)
		 / BITS_PER_UNIT);
      else
	fprintf (asm_out_file, ".lcomm _%s,%d\n", name,
		 TREE_INT_CST_LOW (DECL_SIZE (decl)) * DECL_SIZE_UNIT (decl)
		 / BITS_PER_UNIT);
      return;
    }

  if (TREE_PUBLIC (decl))
    fprintf (asm_out_file, ".globl _%s\n", name);

  output_addressed_constants (DECL_INITIAL (decl));

  if (TREE_READONLY (decl) && ! TREE_VOLATILE (decl))
    fprintf (asm_out_file, "%s\n", TEXT_SECTION_ASM_OP);
  else
    fprintf (asm_out_file, "%s\n", DATA_SECTION_ASM_OP);

  for (i = 0; DECL_ALIGN (decl) >= BITS_PER_UNIT << (i + 1); i++);

  ASM_OUTPUT_ALIGN (asm_out_file, i);

  fprintf (asm_out_file, "_%s:\n", name);
  output_constant (DECL_INITIAL (decl), static_size_in_bytes (TREE_TYPE (decl)));
}

/* Here we combine duplicate floating constants to make
   CONST_DOUBLE rtx's, and force those out to memory when necessary.  */

/* Chain of all CONST_DOUBLE rtx's constructed for the current function.
   They are chained through the third operand slot.  */

extern rtx real_constant_chain;

/* Return a CONST_DOUBLE rtx for a value specified by EXP,
   which must be a REAL_CST tree node.  Make only one CONST_DOUBLE
   for each distinct value.  */

rtx
immed_real_const (exp)
     tree exp;
{
  register rtx r;
  union {double d; int i[2];} u;
  register int i0, i1;
  register enum machine_mode mode = TYPE_MODE (TREE_TYPE (exp));

  /* Get the desired `double' value as two ints
     since that is how they are stored in a CONST_DOUBLE.  */

  u.d = TREE_REAL_CST (exp);
  i0 = u.i[0];
  i1 = u.i[1];

  /* Search the chain for an existing CONST_DOUBLE with the right value.
     If one is found, return it.  */

  for (r = real_constant_chain; r; r = XEXP (r, 3))
    if (XINT (r, 0) == i0 && XINT (r, 1) == i1
	&& GET_MODE (r) == mode)
      return r;

  /* No; make a new one and add it to the chain.  */

  r = gen_rtx (CONST_DOUBLE, mode, i0, i1, 0);
  XEXP (r, 3) = real_constant_chain;
  real_constant_chain = r;

  /* Associate exp and the rtx with each other.  */

  TREE_CST_RTL (exp) = r;
  XEXP (r, 4) = (rtx) exp;

  /* Store const0_rtx in slot 2 just so most things won't barf.
     Actual use of slot 2 is only through force_const_double_mem.  */

  XEXP (r, 2) = const0_rtx;

  return r;
}

/* Given a CONST_DOUBLE, cause a constant in memory to be created
   (unless we already have one for the same value)
   and return a MEM rtx to refer to it.  */

rtx
force_const_double_mem (r)
     rtx r;
{
  if (XEXP (r, 2) == const0_rtx)
    {
      output_constant_def (XEXP (r, 4));
      XEXP (r, 2) = TREE_CST_RTL ((tree) XEXP (r, 4));
    }
  return XEXP (r, 2);
}

/* Given an expression EXP with a constant value,
   reduce it to the sum of an assembler symbol and an integer.
   Store them both in the structure *VALUE.
   Abort if EXP does not reduce.  */

struct addr_const
{
  rtx base;
  int offset;
};

static void
decode_addr_const (exp, value)
     tree exp;
     struct addr_const *value;
{
  register tree target = TREE_OPERAND (exp, 0);
  register int offset = 0;
  register rtx x;

  while (TREE_CODE (target) == COMPONENT_REF)
    {
      offset += DECL_OFFSET (TREE_OPERAND (target, 1)) / BITS_PER_UNIT;
      target = TREE_OPERAND (target, 0);
    }

  if (TREE_CODE (target) == VAR_DECL
      || TREE_CODE (target) == FUNCTION_DECL)
    x = DECL_RTL (target);
  else if (TREE_LITERAL (target))
    x = TREE_CST_RTL (target);
  else
    abort ();

  if (GET_CODE (x) != MEM)
    abort ();
  x = XEXP (x, 0);

  value->base = x;
  value->offset = offset;
}

/* Uniquize all constants that appear in memory.
   Each constant in memory thus far output is recorded
   in `hash_table' with a `struct constant_descriptor'
   that contains a polish representation of the value of
   the constant.

   We cannot store the trees in the hash table
   because the trees may be temporary.  */

struct constant_descriptor
{
  struct constant_descriptor *next;
  int labelno;
  char contents[1];
};

#define HASHBITS 30
#define MAX_HASH_TABLE 1007
static struct constant_descriptor *hash_table[MAX_HASH_TABLE];

/* Compute a hash code for a constant expression.  */

int
const_hash (exp)
     tree exp;
{
  register char *p;
  register int len, hi, i;
  register enum tree_code code = TREE_CODE (exp);

  if (code == INTEGER_CST)
    {
      p = (char *) &TREE_INT_CST_LOW (exp);
      len = 2 * sizeof TREE_INT_CST_LOW (exp);
    }
  else if (code == REAL_CST)
    {
      p = (char *) &TREE_REAL_CST (exp);
      len = sizeof TREE_REAL_CST (exp);
    }
  else if (code == STRING_CST)
    p = TREE_STRING_POINTER (exp), len = TREE_STRING_LENGTH (exp);
  else if (code == COMPLEX_CST)
    return const_hash (TREE_REALPART (exp)) * 5
      + const_hash (TREE_IMAGPART (exp));
  else if (code == CONSTRUCTOR)
    {
      register tree link;
      hi = 5;
      for (link = TREE_OPERAND (exp, 0); link; link = TREE_CHAIN (link))
	hi = (hi * 603 + const_hash (TREE_VALUE (link))) % MAX_HASH_TABLE;
      return hi;
    }
  else if (code == ADDR_EXPR)
    {
      struct addr_const value;
      decode_addr_const (exp, &value);
      p = (char *) &value;
      len = sizeof value;
    }
  else if (code == PLUS_EXPR || code == MINUS_EXPR)
    return const_hash (TREE_OPERAND (exp, 0)) * 5
      +  const_hash (TREE_OPERAND (exp, 1));

  /* Compute hashing function */
  hi = len;
  for (i = 0; i < len; i++)
    hi = ((hi * 613) + (unsigned)(p[i]));

  hi &= (1 << HASHBITS) - 1;
  hi %= MAX_HASH_TABLE;
  return hi;
}

/* Compare a constant expression EXP with a constant-descriptor DESC.
   Return 1 if DESC describes a constant with the same value as EXP.  */

static int
compare_constant (exp, desc)
     tree exp;
     struct constant_descriptor *desc;
{
  return 0 != compare_constant_1 (exp, desc->contents);
}

/* Compare constant expression EXP with a substring P of a constant descriptor.
   If they match, return a pointer to the end of the substring matched.
   If they do not match, return 0.

   Since descriptors are written in polish prefix notation,
   this function can be used recursively to test one operand of EXP
   against a subdescriptor, and if it succeeds it returns the
   address of the subdescriptor for the next operand.  */

static char *
compare_constant_1 (exp, p)
     tree exp;
     char *p;
{
  register char *strp;
  register int len;
  register enum tree_code code = TREE_CODE (exp);

  if (code != (enum tree_code) *p++)
    return 0;

  if (code == INTEGER_CST)
    {
      strp = (char *) &TREE_INT_CST_LOW (exp);
      len = 2 * sizeof TREE_INT_CST_LOW (exp);
    }
  else if (code == REAL_CST)
    {
      /* Real constants are the same only if the same width of type.  */
      if (*p++ != TYPE_PRECISION (TREE_TYPE (exp)))
	return 0;
      strp = (char *) &TREE_REAL_CST (exp);
      len = sizeof TREE_REAL_CST (exp);
    }
  else if (code == STRING_CST)
    {
      strp = TREE_STRING_POINTER (exp);
      len = TREE_STRING_LENGTH (exp);
      if (bcmp (&TREE_STRING_LENGTH (exp), p,
		sizeof TREE_STRING_LENGTH (exp)))
	return 0;
      p += sizeof TREE_STRING_LENGTH (exp);
    }
  else if (code == COMPLEX_CST)
    {
      p = compare_constant_1 (TREE_REALPART (exp), p);
      if (p == 0) return 0;
      p = compare_constant_1 (TREE_IMAGPART (exp), p);
      return p;
    }
  else if (code == CONSTRUCTOR)
    {
      register tree link;
      for (link = TREE_OPERAND (exp, 0); link; link = TREE_CHAIN (link))
	if ((p = compare_constant_1 (TREE_VALUE (link), p)) == 0)
	  return 0;
      return p;
    }
  else if (code == ADDR_EXPR)
    {
      struct addr_const value;
      decode_addr_const (exp, &value);
      strp = (char *) &value;
      len = sizeof value;
    }
  else if (code == PLUS_EXPR || code == MINUS_EXPR)
    {
      if (*p++ != (char) code)
	return 0;
      p = compare_constant_1 (TREE_OPERAND (exp, 0), p);
      if (p == 0) return 0;
      p = compare_constant_1 (TREE_OPERAND (exp, 1), p);
      return p;
    }

  /* Compare constant contents.  */
  while (--len >= 0)
    if (*p++ != *strp++)
      return 0;

  return p;
}

/* Construct a constant descriptor for the expression EXP.
   It is up to the caller to enter the descriptor in the hash table.  */

static struct constant_descriptor *
record_constant (exp)
     tree exp;
{
  struct constant_descriptor *ptr = 0;
  int buf;

  obstack_grow (&permanent_obstack, &ptr, sizeof ptr);
  obstack_grow (&permanent_obstack, &buf, sizeof buf);
  record_constant_1 (exp);
  return (struct constant_descriptor *) obstack_finish (&permanent_obstack);
}

/* Add a description of constant expression EXP
   to the object growing in `permanent_obstack'.
   No need to return its address; the caller will get that
   from the obstack when the object is complete.  */

static void
record_constant_1 (exp)
     tree exp;
{
  register char *strp;
  register int len;
  register enum tree_code code = TREE_CODE (exp);

  obstack_1grow (&permanent_obstack, (unsigned char) code);

  if (code == INTEGER_CST)
    {
      strp = (char *) &TREE_INT_CST_LOW (exp);
      len = 2 * sizeof TREE_INT_CST_LOW (exp);
    }
  else if (code == REAL_CST)
    {
      obstack_1grow (&permanent_obstack, TYPE_PRECISION (TREE_TYPE (exp)));
      strp = (char *) &TREE_REAL_CST (exp);
      len = sizeof TREE_REAL_CST (exp);
    }
  else if (code == STRING_CST)
    {
      strp = TREE_STRING_POINTER (exp);
      len = TREE_STRING_LENGTH (exp);
      obstack_grow (&permanent_obstack, (char *) &TREE_STRING_LENGTH (exp),
		    sizeof TREE_STRING_LENGTH (exp));
    }
  else if (code == COMPLEX_CST)
    {
      record_constant_1 (TREE_REALPART (exp));
      record_constant_1 (TREE_IMAGPART (exp));
      return;
    }
  else if (code == CONSTRUCTOR)
    {
      register tree link;
      int length = list_length (TREE_OPERAND (exp, 0));
      obstack_grow (&permanent_obstack, (char *) length, sizeof length);

      for (link = TREE_OPERAND (exp, 0); link; link = TREE_CHAIN (link))
	record_constant_1 (TREE_VALUE (link));
      return;
    }
  else if (code == ADDR_EXPR)
    {
      struct addr_const value;
      decode_addr_const (exp, &value);
      strp = (char *) &value;
      len = sizeof value;
    }
  else if (code == PLUS_EXPR || code == MINUS_EXPR)
    {
      obstack_1grow (&permanent_obstack, (char) code);
      record_constant_1 (TREE_OPERAND (exp, 0));
      record_constant_1 (TREE_OPERAND (exp, 1));
      return;
    }

  /* Record constant contents.  */
  obstack_grow (&permanent_obstack, strp, len);
}

/* Return the constant-label-number for constant value EXP.
   If no constant equal to EXP has yet been output,
   define a new label and output assembler code for it.
   The hash_table records which constants already have label numbers.  */

int
get_or_assign_labelno (exp)
     tree exp;
{
  register int hash, i;
  register struct constant_descriptor *desc;

  /* Make sure any other constants whose addresses appear in EXP
     are assigned label numbers.  */

  output_addressed_constants (exp);

  /* Compute hash code of EXP.  Search the descriptors for that hash code
     to see if any of them describes EXP.  If yes, the descriptor records
     the label number already assigned.  */

  hash = const_hash (exp) % MAX_HASH_TABLE;

  for (desc = hash_table[hash]; desc; desc = desc->next)
    if (compare_constant (exp, desc))
      return desc->labelno;

  /* No constant equal to EXP is known to have been output.
     Make a constant descriptor to enter EXP in the hash table.
     Assign the label number and record it in the descriptor for
     future calls to this function to find.  */

  desc = record_constant (exp);
  desc->next = hash_table[hash];
  hash_table[hash] = desc;
  desc->labelno = const_labelno++;

  /* Now output assembler code to define that label
     and follow it with the data of EXP.  */

  /* First switch to text segment.  */
  fprintf (asm_out_file, "%s\n", TEXT_SECTION_ASM_OP);

  /* Align the location counter as required by EXP's data type.  */
  for (i = 0; TYPE_ALIGN (TREE_TYPE (exp)) >= BITS_PER_UNIT << (i + 1); i++);
  ASM_OUTPUT_ALIGN (asm_out_file, i);

  /* Output the label itself.  */
  fprintf (asm_out_file, "LC%d:\n", desc->labelno);

  /* Output the value of EXP.  */
  output_constant (exp,
		   (TREE_CODE (exp) == STRING_CST
		    ? TREE_STRING_LENGTH (exp)
		    : static_size_in_bytes (TREE_TYPE (exp))));

  return desc->labelno;
}

/* Return an rtx representing a reference to constant data in memory
   for the constant expression EXP.
   If assembler code for such a constant has already been output,
   return an rtx to refer to it.
   Otherwise, output such a constant in memory and generate
   an rtx for it.  The TREE_CST_RTL of EXP is set up to point to that rtx.  */

rtx
output_constant_def (exp)
     tree exp;
{
  register rtx def;
  register int labelno;
  char *labelstr;
  char label[10];

  if (TREE_CST_RTL (exp))
    return TREE_CST_RTL (exp);

  labelno = get_or_assign_labelno (exp);

  sprintf (label, "*LC%d", labelno);
  labelstr = obstack_copy0 (current_obstack, label, strlen (label));

  def = gen_rtx (SYMBOL_REF, Pmode, labelstr);

  TREE_CST_RTL (exp)
    = gen_rtx (MEM, TYPE_MODE (TREE_TYPE (exp)), def);

  return def;
}

/* Find all the constants whose addresses are referenced inside of EXP,
   and make sure assembler code with a label has been output for each one.  */

output_addressed_constants (exp)
     tree exp;
{
  if (TREE_CODE (exp) == ADDR_EXPR)
    {
      register tree constant = TREE_OPERAND (exp, 0);

      while (TREE_CODE (constant) == COMPONENT_REF)
	{
	  constant = TREE_OPERAND (constant, 0);
	}

      if (TREE_LITERAL (constant))
	/* No need to do anything here
	   for addresses of variables or functions.  */
	output_constant_def (constant);
    }
  else if (TREE_CODE (exp) == PLUS_EXPR
	   || TREE_CODE (exp) == MINUS_EXPR)
    {
      output_addressed_constants (TREE_OPERAND (exp, 0));
      output_addressed_constants (TREE_OPERAND (exp, 1));
    }
  else if (TREE_CODE (exp) == NOP_EXPR)
    {
      output_addressed_constants (TREE_OPERAND (exp, 0));
    }
  else if (TREE_CODE (exp) == CONSTRUCTOR)
    {
      register tree link;
      for (link = TREE_OPERAND (exp, 0); link; link = TREE_CHAIN (link))
	output_addressed_constants (TREE_VALUE (link));
    }
  else if (! TREE_LITERAL (exp))
    abort ();
}

/* Output assembler code for constant EXP to FILE, with no label.
   This includes the pseudo-op such as ".int" or ".byte", and a newline.
   Assumes output_addressed_constants has been done on EXP already.

   Generate exactly SIZE bytes of assembler data, padding at the end
   with zeros if necessary.  SIZE must always be specified.

   SIZE is important for structure constructors,
   since trailing members may have been omitted from the constructor.
   It is also important for initialization of arrays from string constants
   since the full length of the string constant might not be wanted.
   It is also needed for initialization of unions, where the initializer's
   type is just one member, and that may not be as long as the union.

   There a case in which we would fail to output exactly SIZE bytes:
   for a structure constructor that wants to produce more than SIZE bytes.
   But such constructors will never be generated for any possible input.  */

output_constant (exp, size)
     register tree exp;
     register int size;
{
  register enum tree_code code = TREE_CODE (TREE_TYPE (exp));
  if (size == 0)
    return;

  switch (code)
    {
    case INTEGER_TYPE:
    case ENUMERAL_TYPE:
    case POINTER_TYPE:
      if (--size == 0)
	fprintf (asm_out_file, "\t%s", ASM_CHAR_OP);
      else if (--size < 2)
	fprintf (asm_out_file, "\t%s", ASM_SHORT_OP);
      else
	{
	  fprintf (asm_out_file, "\t%s", ASM_INT_OP);
	  size -= 2;
	}
      output_arith_constant (exp);
      fprintf (asm_out_file, "\n");
      break;

    case REAL_TYPE:
      if (TREE_CODE (exp) != REAL_CST)
	yyerror ("initializer for floating value is not a floating constant");

      if (size < 4)
	break;
      else if (size < 8)
	{
	  ASM_OUTPUT_FLOAT (asm_out_file, TREE_REAL_CST (exp));
	  size -= 4;
	}
      else
	{
	  ASM_OUTPUT_DOUBLE (asm_out_file, TREE_REAL_CST (exp));
	  size -= 8;
	}
      break;

    case COMPLEX_TYPE:
      output_constant (TREE_REALPART (exp), size / 2);
      output_constant (TREE_IMAGPART (exp), size / 2);
      size -= (size / 2) * 2;
      break;

    case ARRAY_TYPE:
      if (TREE_CODE (exp) == CONSTRUCTOR)
	{
	  output_constructor (exp, size);
	  return;
	}
      else if (TREE_CODE (exp) == STRING_CST)
	{
	  register int i;
	  register unsigned char *p
	    = (unsigned char *) TREE_STRING_POINTER (exp);
	  int excess = 0;

	  if (size > TREE_STRING_LENGTH (exp))
	    {
	      excess = size - TREE_STRING_LENGTH (exp);
	      size = TREE_STRING_LENGTH (exp);
	    }

	  if (size > 0 && p[size - 1] == 0)
	    {
	      fprintf (asm_out_file, "\t.asciz \"");
	      size--;
	    }
	  else
	    fprintf (asm_out_file, "\t.ascii \"");

	  for (i = 0; i < size; i++)
	    {
	      register int c = p[i];
	      if (c == '\"' || c == '\\')
		putc ('\\', asm_out_file);
	      if (c >= ' ' && c < 0177)
		putc (c, asm_out_file);
	      else
		fprintf (asm_out_file, "\\%03o", c);
	    }
	  fprintf (asm_out_file, "\"\n");

	  size = excess;
	}
      else
	abort ();
      break;

    case RECORD_TYPE:
      if (TREE_CODE (exp) == CONSTRUCTOR)
	output_constructor (exp, size);
      else
	abort ();
      return;
    }

  if (size > 0)
    ASM_OUTPUT_SKIP (asm_out_file, size);
}

/* Subroutine of output_constant, used for CONSTRUCTORs
   (aggregate constants).
   Generate at least SIZE bytes, padding if necessary.  */

output_constructor (exp, size)
     tree exp;
     int size;
{
  register tree link, field = 0, elt;
  register int byte;
  int total_bytes = 0;
  int byte_offset = -1;

  if (TREE_CODE (TREE_TYPE (exp)) == RECORD_TYPE)
    field = TYPE_FIELDS (TREE_TYPE (exp));

  /* As LINK goes through the elements of the constant,
     FIELD goes through the structure fields, if the constant is a structure.
     But the constant could also be an array.  Then FIELD is zero.  */
  for (link = TREE_OPERAND (exp, 0);
       link;
       link = TREE_CHAIN (link),
       field = field ? TREE_CHAIN (field) : 0)
    {
      if (field == 0
	  || (DECL_MODE (field) != BImode))
	{
	  register int fieldsize;

	  /* An element that is not a bit-field.
	     Output any buffered-up bit-fields preceding it.  */
	  if (byte_offset >= 0)
	    {
	      fprintf (asm_out_file, "\t%s0x%x\n", ASM_CHAR_OP, byte);
	      total_bytes++;
	      byte_offset = -1;
	    }

	  /* Align to this element's alignment,
	     if it isn't aligned properly by its predecessors.  */
	  if (field && (total_bytes * BITS_PER_UNIT) % DECL_ALIGN (field) != 0)
	    {
	      int byte_align = DECL_ALIGN (field) / BITS_PER_UNIT;
	      int to_byte = (((total_bytes + byte_align - 1) / byte_align)
			     * byte_align);
	      ASM_OUTPUT_SKIP (asm_out_file, to_byte - total_bytes);
	      total_bytes = to_byte;
	    }

	  /* Output the element's initial value.  */
	  fieldsize = static_size_in_bytes (field ? TREE_TYPE (field)
					    : TREE_TYPE (TREE_TYPE (exp)));
	  output_constant (TREE_VALUE (link), fieldsize);

	  /* Count its size.  */
	  total_bytes += fieldsize;
	}
      else if (TREE_CODE (TREE_VALUE (link)) != INTEGER_CST)
	yyerror ("invalid initial value for %s field",
		 IDENTIFIER_POINTER (DECL_NAME (field)));
      else
	{
	  /* Element that is a bit-field.  */

	  int next_offset = DECL_OFFSET (field);
	  int end_offset
	    = (next_offset
	       + (TREE_INT_CST_LOW (DECL_SIZE (field))
		  * DECL_SIZE_UNIT (field)));

	  /* We must split the element into pieces that fall within
	     separate bytes, and combine each byte with previous or
	     following bit-fields.  */

	  /* next_offset is the offset n fbits from the begining of
	     the structure to the next bit of this element to be processed.
	     end_offset is the offset of the first bit past the end of
	     this element.  */
	  while (next_offset < end_offset)
	    {
	      int this_time;
	      int next_byte = next_offset / BITS_PER_UNIT;
	      int next_bit = next_offset % BITS_PER_UNIT;
	      if (byte_offset < 0)
		{
		  byte_offset = next_byte;
		  byte = 0;
		}
	      else
		while (next_byte != byte_offset)
		  {
		    fprintf (asm_out_file, "\t%s0x%x\n", ASM_CHAR_OP, byte);
		    byte_offset++;
		    total_bytes++;
		    byte = 0;
		  }
	      /* Number of bits we can process at once
		 (all part of the same byte).  */
	      this_time = MIN (end_offset - next_offset,
			       BITS_PER_UNIT - next_bit);
#ifdef BYTES_BIG_ENDIAN
	      /* On big-endian machine, take the most significant bits
		 first (of the bits that are significant)
		 and put them into bytes from the most significant end.  */
	      byte |= (((TREE_INT_CST_LOW (TREE_VALUE (link))
			 >> (end_offset - next_offset - this_time))
			& ((1 << this_time) - 1))
		       << (BITS_PER_UNIT - this_time - next_bit));
#else
	      /* On little-endian machines,
		 take first the least significant bits of the value
		 and pack them starting at the least significant
		 bits of the bytes.  */
	      byte |= ((TREE_INT_CST_LOW (TREE_VALUE (link))
			>> (next_offset - DECL_OFFSET (field)))
		       & ((1 << this_time) - 1)) << next_bit;
#endif
	      next_offset += this_time;
	    }
	}
    }
  if (byte_offset >= 0)
    {
      fprintf (asm_out_file, "\t%s0x%x\n", ASM_CHAR_OP, byte);
      byte_offset = -1;
      total_bytes++;
    }
  if (total_bytes < size)
    ASM_OUTPUT_SKIP (asm_out_file, size - total_bytes);
}

/* Output an expression in assembler language
   to represent the value of EXP.  EXP may contain integer constants,
   addresses (assembler labels), and addition and subtraction.
   Anything else causes a compiler error message, because the
   label is probably relocatable so the assembler and linker could
   not handle it.  */

output_arith_constant (exp)
     tree exp;
{
  register int size;

  switch (TREE_CODE (exp))
    {
    case NOP_EXPR:
    case CONVERT_EXPR:
      output_arith_constant (TREE_OPERAND (exp, 0));
      break;

    case INTEGER_CST:
      fprintf (asm_out_file, "%d", TREE_INT_CST_LOW (exp));
      break;

    case REAL_CST:
    case COMPLEX_CST:
    case STRING_CST:
      abort ();
      break;

    case ADDR_EXPR:
      {
	struct addr_const value;
	decode_addr_const (exp, &value);

	output_addr_const (asm_out_file, value.base);
	if (value.offset)
	  fprintf (asm_out_file, "+%d", value.offset);
      }
      break;

    case PLUS_EXPR:
    case MINUS_EXPR:
      fprintf (asm_out_file, ASM_OPEN_PAREN);
      output_arith_constant (TREE_OPERAND (exp, 0));
      if (TREE_CODE (exp) == PLUS_EXPR)
	fprintf (asm_out_file, "+");
      else
	fprintf (asm_out_file, "-");
      output_arith_constant (TREE_OPERAND (exp, 1));
      fprintf (asm_out_file, ASM_CLOSE_PAREN);
      break;

    default:
      yyerror ("Arithmetic too complicated on address in static initializer");
    }
}
