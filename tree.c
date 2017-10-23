/* Language-indepednent node constructors for parse phase of GNU compiler.
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


/* This file contains the low level primitives for operating on tree nodes,
   including allocation, list operations, interning of identifiers,
   construction of data type nodes and statement nodes,
   and construction of type conversion nodes.  It also contains
   tables index by tree code that describe how to take apart
   nodes of that code.

   It is intended to be language-independent, but occasionally
   calls language-dependent routines defined (for C) in typecheck.c.

   The low-level allocation routines oballoc and permalloc
   are used also for allocating many other kinds of objects
   by all passes of the compiler.  */

#include "config.h"
#include <stdio.h>
#include "tree.h"
#include "obstack.h"

#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free free

extern int xmalloc ();
extern void free ();

/* Tree nodes of permanent duration are allocated in this obstack.
   They are the identifier nodes, and everything outside of
   the bodies and parameters of function definitions.  */

struct obstack permanent_obstack;

/* The contents of the current function definition are allocated
   in this obstack, and all are freed at the end of the function.  */

struct obstack temporary_obstack;

/* This points at either permanent_obstack or temporary_obstack.  */

struct obstack *current_obstack;

/* Table indexed by tree code giving a string containing a character
   classifying the tree code.  Possibilities are
   t, d, s, c, r and e.  See tree.def for details.  */

#define DEFTREECODE(SYM, NAME, TYPE, LENGTH) TYPE,

char *tree_code_type[] = {
#include "tree.def"
};
#undef DEFTREECODE

/* Table indexed by tree code giving number of expression
   operands beyond the fixed part of the node structure.
   Not used for types or decls.  */

#define DEFTREECODE(SYM, NAME, TYPE, LENGTH) LENGTH,

int tree_code_length[] = {
#include "tree.def"
};
#undef DEFTREECODE

/* Counter for assigning unique ids to all tree nodes.  */

int tree_node_counter = 0;

/* Hash table for uniquizing IDENTIFIER_NODEs by name.  */

#define MAX_HASH_TABLE 1008
static tree hash_table[MAX_HASH_TABLE];	/* id hash buckets */

/* Init data for node creation, at the beginning of compilation.  */

void
init_tree ()
{
  obstack_init (&permanent_obstack);
  current_obstack = &permanent_obstack;
  tree_node_counter = 1;
  bzero (hash_table, sizeof hash_table);
}

/* Start allocating on the temporary (per function) obstack.
   This is done in start_function before parsing the function body.  */

temporary_allocation ()
{
  /* Set up the obstack: */
  obstack_init (&temporary_obstack);

  current_obstack = &temporary_obstack;
}

/* Go back to allocating on the permanent obstack
   and free everything in the temporary obstack.
   This is done in finish_function after fully compiling a function.  */

permanent_allocation ()
{
  /* Free up previous temporary obstack data */
  obstack_free (&temporary_obstack, NULL);

  current_obstack = &permanent_obstack;
}

/* Allocate SIZE bytes in the current obstack
   and return a pointer to them.
   In practice the current obstack is always the temporary one.  */

char *
oballoc (size)
     int size;
{
  return (char *) obstack_alloc (current_obstack, size);
}

/* Free the object PTR in the current obstack
   as well as everything allocated since PTR.
   In practice the current obstack is always the temporary one.  */

void
obfree (ptr)
     char *ptr;
{
  obstack_free (current_obstack, ptr);
}

/* Allocate SIZE bytes in the permanent obstack
   and return a pointer to them.  */

char *
permalloc (size)
     long size;
{
  char *object;

  return (char *) obstack_alloc (&permanent_obstack, size);
}

/* Return a newly allocated node of code CODE.
   Initialize the node's unique id and its TREE_PERMANENT flag.
   For decl and type nodes, some other fields are initialized.
   The rest of the node is initialized to zero.

   Achoo!  I got a code in the node.  */

tree
make_node (code)
     enum tree_code code;
{
  register tree t;
  register int type = *tree_code_type[(int) code];
  register int length;
  register struct obstack *obstack;
  register int i;

  switch (type)
    {
    case 'd':  /* A decl node */
      length = sizeof (struct tree_decl);
      break;

    case 't':  /* a type node */
      length = sizeof (struct tree_type);
      break;

    case 's':  /* a stmt node */
      length = sizeof (struct tree_shared)
	+ 2 * sizeof (int)
	  + tree_code_length[(int) code] * sizeof (char *);
      break;

    default:   /* an expression or constant.  */
      length = sizeof (struct tree_shared)
	+ tree_code_length[(int) code] * sizeof (char *);
    }

  obstack = (code != IDENTIFIER_NODE) ? current_obstack : &permanent_obstack;

  t = (tree) obstack_alloc (obstack, length);

  TREE_UID (t) = tree_node_counter++;
  TREE_TYPE (t) = 0;
  TREE_CHAIN (t) = 0;
  for (i = (length / sizeof (int)) - 1;
       i >= sizeof (struct tree_shared) / sizeof (int) - 1;
       i--)
    ((int *) t)[i] = 0;

  TREE_SET_CODE (t, code);
  if (obstack == &permanent_obstack)
    TREE_PERMANENT (t) = 1;

  if (type == 'd')
    {
      extern int lineno;

      DECL_ALIGN (t) = 1;
      DECL_SIZE_UNIT (t) = 1;
      DECL_VOFFSET_UNIT (t) = 1;
      DECL_SOURCE_LINE (t) = lineno;
      DECL_SOURCE_FILE (t) = input_filename;
    }

  if (type == 't')
    {
      TYPE_ALIGN (t) = 1;
      TYPE_SIZE_UNIT (t) = 1;
      TYPE_SEP_UNIT (t) = 1;
      TYPE_MAIN_VARIANT (t) = t;
    }

  if (type == 'c')
    {
      TREE_LITERAL (t) = 1;
    }

  return t;
}

/* Return a new node with the same contents as NODE
   except that its TREE_CHAIN is zero and it has a fresh uid.  */

tree
copy_node (node)
     tree node;
{
  register tree t;
  register enum tree_code code = TREE_CODE (node);
  register int length;
  register int i;

  switch (*tree_code_type[(int) code])
    {
    case 'd':  /* A decl node */
      length = sizeof (struct tree_decl);
      break;

    case 't':  /* a type node */
      length = sizeof (struct tree_type);
      break;

    case 's':
      length = sizeof (struct tree_shared)
	+ 2 * sizeof (int)
	  + tree_code_length[(int) code] * sizeof (char *);
      break;

    default:   /* a statement, expression or constant.  */
      length = sizeof (struct tree_shared)
	+ tree_code_length[(int) code] * sizeof (char *);
    }

  t = (tree) obstack_alloc (current_obstack, length);

  for (i = (length / sizeof (int)) - 1;
       i >= 0;
       i--)
    ((int *) t)[i] = ((int *) node)[i];

  TREE_UID (t) = tree_node_counter++;
  TREE_CHAIN (t) = 0;

  TREE_PERMANENT (t) = (current_obstack == &permanent_obstack);

  return t;
}

#define HASHBITS 30

/* Return an IDENTIFIER_NODE whose name is TEXT (a null-terminated string).
   If an identifier with that name has previously been referred to,
   the same node is returned this time.  */

tree
get_identifier (text)
     register char *text;
{
  register int hi;
  register int i;
  register tree idp;
  register int len;

  /* Compute length of text in len.  */
  for (len = 0; text[len]; len++);

  /* Compute hash code */
  hi = len;
  for (i = 0; i < len; i++)
    hi = ((hi * 613) + (unsigned)(text[i]));

  hi &= (1 << HASHBITS) - 1;
  hi %= MAX_HASH_TABLE;
  
  /* Search table for identifier */
  for (idp = hash_table[hi]; idp!=NULL; idp = TREE_CHAIN (idp))
    if (IDENTIFIER_LENGTH (idp) == len &&
	!strcmp (IDENTIFIER_POINTER (idp), text))
      return idp;		/* <-- return if found */

  /* Not found, create one, add to chain */
  idp = make_node (IDENTIFIER_NODE);
  IDENTIFIER_LENGTH (idp) = len;

  IDENTIFIER_POINTER (idp) = obstack_copy0 (&permanent_obstack, text, len);

  TREE_CHAIN (idp) = hash_table[hi];
  hash_table[hi] = idp;
  return idp;			/* <-- return if created */
}

/* Return a newly constructed INTEGER_CST node whose constant value
   is specified by the two ints LOW and HI.
   The TREE_TYPE is not initialized.  */

tree
build_int_2 (low, hi)
     int low, hi;
{
  register tree t = make_node (INTEGER_CST);
  TREE_INT_CST_LOW (t) = low;
  TREE_INT_CST_HIGH (t) = hi;
  TREE_TYPE (t) = integer_type_node;
  return t;
}

/* Return a REAL_CST node containing a value atoi (STR) * 10**EX.
   The TREE_TYPE is not initialized.  */

tree
build_real_from_string (str, ex)
     char *str;
     int ex;
{
  double r;
  int i;
  tree t;

  t = make_node (REAL_CST);
/* ??? This conversion code is not good.
   Use the new atof once it is done.  */
  sscanf (str, "%lf", &r);
  if (0 < ex) 
    for (i = 0; i < ex; i++)
      r = r * 10;
  else
    for (i = 0; i < -ex; i++)
      r = r / 10;

  TREE_REAL_CST (t) = r;
  return (t);
}

/* Return a newly constructed REAL_CST node whose value is D.
   The TREE_TYPE is not initialized.  */

tree
build_real (d)
     double d;
{
  tree v;

  v = make_node (REAL_CST);
  TREE_REAL_CST (v) = d;
  return v;
}

/* Return a newly constructed REAL_CST node whose value
   is the integer value of the INTEGER_CST node I.
   The TREE_TYPE is not initialized.  */

tree
build_real_from_int_cst (i)
     tree i;
{
  tree v;

  v = make_node (REAL_CST);
  TREE_REAL_CST (v)
    = (double) TREE_INT_CST_LOW (i)
      + ((double) (1 << (HOST_BITS_PER_INT / 2))
	 * (double) (1 << (HOST_BITS_PER_INT / 2))
	 * (double) TREE_INT_CST_HIGH (i));
  return v;
}

/* Return a newly constructed STRING_CST node whose value is
   the LEN characters at STR.
   The TREE_TYPE is not initialized.  */

tree
build_string (len, str)
     int len;
     char *str;
{
  register tree s = make_node (STRING_CST);
  TREE_STRING_LENGTH (s) = len;
  TREE_STRING_POINTER (s) = obstack_copy0 (current_obstack, str, len);
  return s;
}

/* Return a newly constructed COMPLEX_CST node whose value is
   specified by the real and imaginary parts REAL and IMAG.
   Both REAL and IMAG should be constant nodes.
   The TREE_TYPE is not initialized.  */

tree
build_complex (real, imag)
     tree real, imag;
{
  register tree t = make_node (COMPLEX_CST);
  TREE_REALPART (t) = real;
  TREE_IMAGPART (t) = imag;
  return t;
}

/* Return nonzero if the type FTYPE is unsigned (all possible values >= 0).
   Nonscalar types are considered unsigned; real types considered signed.  */

int
type_unsigned_p (ftype)
     tree ftype;
{
  register tree type = ftype;
  register tree t;

  if (TREE_CODE (ftype) == POINTER_TYPE)
    return 1;
  if (TREE_CODE (ftype) == REAL_TYPE)
    return 0;
  if (TREE_CODE (ftype) != INTEGER_TYPE
      && TREE_CODE (ftype) != ENUMERAL_TYPE)
    return 1;

  while (1)
    {
      t = TYPE_MIN_VALUE (type);
      if (TREE_CODE (t) == INTEGER_CST)
	return TREE_INT_CST_HIGH (t) >= 0;

      type = TREE_TYPE (type);
      if (type == 0)
	return 0;
    }
}

/* Return 1 if EXPR is the integer constant zero.  */

int
integer_zerop (expr)
     tree expr;
{
  return TREE_CODE (expr) == INTEGER_CST
    && TREE_INT_CST_LOW (expr) == 0
    && TREE_INT_CST_HIGH (expr) == 0;
}

/* Return 1 if EXPR is the integer constant one.  */

int
integer_onep (expr)
     tree expr;
{
  return TREE_CODE (expr) == INTEGER_CST
    && TREE_INT_CST_LOW (expr) == 1
    && TREE_INT_CST_HIGH (expr) == 0;
}

/* Return 1 if EXPR is an integer containing all 1's
   in as much precision as it contains.  */

int
integer_all_onesp (expr)
     tree expr;
{
  register int prec;
  register int uns;

  if (TREE_CODE (expr) != INTEGER_CST)
    return 0;

  uns = type_unsigned_p (TREE_TYPE (expr));
  if (!uns)
    return TREE_INT_CST_LOW (expr) == -1 && TREE_INT_CST_HIGH (expr) == -1;

  prec = TYPE_PRECISION (TREE_TYPE (expr));
  if (prec >= HOST_BITS_PER_INT)
    return TREE_INT_CST_LOW (expr) == -1
      && TREE_INT_CST_HIGH (expr) == (1 << (prec - HOST_BITS_PER_INT)) - 1;
  else
    return TREE_INT_CST_LOW (expr) == (1 << prec) - 1;
}

/* Return the length of a chain of nodes chained through TREE_CHAIN.
   We expect a null pointer to mark the end of the chain.
   This is the Lisp primitive `length'.  */

int
list_length (t)
     tree t;
{
  register tree tail;
  register int len = 0;

  for (tail = t; tail; tail = TREE_CHAIN (tail))
    len++;

  return len;
}

/* Concatenate two chains of nodes (chained through TREE_CHAIN)
   by modifying the last node in chain 1 to point to chain 2.
   This is the Lisp primitive `nconc'.  */

tree
chainon (op1, op2)
     tree op1, op2;
{
  tree t;

  if (op1)
    {
      for (t = op1; TREE_CHAIN (t); t = TREE_CHAIN (t))
	if (t == op2) abort ();	/* Circularity being created */
      TREE_CHAIN (t) = op2;
      return op1;
    }
  else return op2;
}

/* Return a newly created TREE_LIST node whose
   purpose and value fields are PARM and VALUE.  */

tree
build_tree_list (parm, value)
     tree parm, value;
{
  register tree t = make_node (TREE_LIST);
  TREE_PURPOSE (t) = parm;
  TREE_VALUE (t) = value;
  return t;
}

/* Return a newly created TREE_LIST node whose
   purpose and value fields are PARM and VALUE
   and whose TREE_CHAIN is CHAIN.  */

tree
tree_cons (purpose, value, chain)
     tree purpose, value, chain;
{
  register tree node = make_node (TREE_LIST);
  TREE_CHAIN (node) = chain;
  TREE_PURPOSE (node) = purpose;
  TREE_VALUE (node) = value;
  return node;
}

/* Return the last node in a chain of nodes (chained through TREE_CHAIN).  */

tree
tree_last (chain)
     register tree chain;
{
  register tree next;
  if (chain)
    while (next = TREE_CHAIN (chain))
      chain = next;
  return chain;
}

/* Reverse the order of elements in the chain T,
   and return the new head of the chain (old last element).  */

tree
nreverse (t)
     tree t;
{
  register tree prev = 0, decl, next;
  for (decl = t; decl; decl = next)
    {
      next = TREE_CHAIN (decl);
      TREE_CHAIN (decl) = prev;
      prev = decl;
    }
  return prev;
}

/* Return the size nominally occupied by an object of type TYPE
   when it resides in memory.  The value is measured in units of bytes,
   and its data type is that normally used for type sizes
   (which is the first type created by make_signed_type or
   make_unsigned_type).  */

tree
size_in_bytes (type)
     tree type;
{
  if (type == error_mark_node)
    return integer_zero_node;
  return convert_units (TYPE_SIZE (type), TYPE_SIZE_UNIT (type),
			BITS_PER_UNIT);
}

/* Return nonzero if arg is static -- a reference to an object in
   static storage.  This is not the same as the C meaning of `static'.  */

int
staticp (arg)
     tree arg;
{
  register enum tree_code code = TREE_CODE (arg);

  if ((code == VAR_DECL || code == FUNCTION_DECL)
      && (TREE_STATIC (arg) || TREE_EXTERNAL (arg)))
    return 1;

  if (code == COMPONENT_REF)
    return staticp (TREE_OPERAND (arg, 0));

  return 0;
}

/* Verify that an expression, REF, is a reference to data that makes sense
   to modify or take the address of
   (i.e., for processing the argument to unary & or the left arg to =).
 Error if REF is some other kind of expression.

 We can safely ignore the difference between "makes sense to modify"
 and "makes sense to take the address of", because attempting to
 take the address of a variable will force it into memory anyway.  */

int
lvalue_or_else (ref)
     tree ref;
{
  register enum tree_code code = TREE_CODE (ref);

  if (code == COMPONENT_REF)
    return lvalue_or_else (TREE_OPERAND (ref, 0));
  else if (code == INDIRECT_REF || code == ARRAY_REF || code == VAR_DECL
           || code == FUNCTION_DECL || code == PARM_DECL || code == RESULT_DECL
	   || code == ERROR_MARK)
    return 1;
  yyerror ("invalid lvalue (not a reference to data in memory)");
  return 0;
}

/* This should be applied to any node which may be used in more than one place,
   but must be evaluated only once.  Normally, the code generator would
   reevaluate the node each time; this forces it to compute it once and save
   the result.  This is done by encapsulating the node in a SAVE_EXPR.  */

tree
save_expr (expr)
     tree expr;
{
  register tree t = fold (expr);

  /* If the tree evaluates to a constant, then we don't what to hide that
     fact (i.e. this allows further folding, and direct checks for constants).
     Since it is no problem to reevaluate literals, we just return the 
     literal node. */

  if (TREE_LITERAL (t) || TREE_READONLY (t) || TREE_CODE (t) == SAVE_EXPR)
    return t;

  t = build2 (SAVE_EXPR, t, NULL);
  TREE_TYPE (t) = TREE_TYPE (expr);
  TREE_VOLATILE (t) = TREE_VOLATILE (expr);
  return t;
}

/* Stabilize a reference so that we can use it any number of times
   without causing its operands to be evaluated more than once.
   Returns the stabilized reference.  */

tree
stabilize_reference (ref)
     tree ref;
{
  register tree result;
  register enum tree_code code = TREE_CODE (ref);

  if (code == INDIRECT_REF)
    {
      result = build1 (INDIRECT_REF, save_expr (TREE_OPERAND (ref, 0)));
    }
  else if (code == COMPONENT_REF)
    {
      result = build2 (COMPONENT_REF,
		       stabilize_reference (TREE_OPERAND (ref, 0)),
		       TREE_OPERAND (ref, 1));
    }
  else if (code == ARRAY_REF)
    {
      result = build2 (ARRAY_REF, save_expr (TREE_OPERAND (ref, 0)),
		       save_expr (TREE_OPERAND (ref, 1)));
    }
  else if (code == VAR_DECL || code == PARM_DECL || code == RESULT_DECL)
    {
      result = ref;
    }
  else
    {
      if (code != ERROR_MARK)
	yyerror ("invalid lvalue (not a reference to data in memory)");
      return error_mark_node;
    }

  TREE_TYPE (result) = TREE_TYPE (ref);
  TREE_VOLATILE (result) = TREE_VOLATILE (ref);

  return result;
}

/* Low-level constructors for expressions.  */

/* Return a newly created expression-node of code SC and operand ARG1.
   Used for codes that want one operand.
   The TREE_TYPE is not initialized.  */

tree
build1 (sc, arg1)
     enum tree_code sc;
     tree arg1;
{
  register tree t = make_node (sc);

  TREE_OPERAND (t, 0) = arg1;
  return t;
}

/* Return a newly created expression-node of code SC
   and operands ARG1 and ARG2.
   Used for codes that want two operands.
   The TREE_TYPE is not initialized.  */

tree
build2 (sc, arg1, arg2)
     enum tree_code sc;
     tree arg1, arg2;
{
  register tree t = make_node (sc);

  TREE_OPERAND (t, 0) = arg1;
  TREE_OPERAND (t, 1) = arg2;
  return t;
}

/* Return a newly created expression-node of code SC
   and operands ARG1, ARG2 and ARG3.
   Used for codes that want three operands.
   The TREE_TYPE is not initialized.  */

tree
build3 (sc, arg1, arg2, arg3)
     int sc;
     tree arg1, arg2, arg3;
{
  register tree t = make_node (sc);

  TREE_OPERAND (t, 0) = arg1;
  TREE_OPERAND (t, 1) = arg2;
  TREE_OPERAND (t, 2) = arg3;
  return t;
}

/* Low-level constructors for statements.
   These constructors all expect source file name and line number
   as arguments, as well as enough arguments to fill in the data
   in the statement node.  */

tree
build_goto (filename, line, label)
     char *filename;
     int line;
     tree label;
{
  register tree t = make_node (GOTO_STMT);
  STMT_SOURCE_FILE (t) = filename;
  STMT_SOURCE_LINE (t) = line;
  STMT_BODY (t) = label;
  return t;
}

tree
build_return (filename, line, arg)
     char *filename;
     int line;
     tree arg;
{
  register tree t = make_node (RETURN_STMT);

  STMT_SOURCE_FILE (t) = filename;
  STMT_SOURCE_LINE (t) = line;
  STMT_BODY (t) = arg;
  return t;
}

tree
build_expr_stmt (filename, line, expr)
     char *filename;
     int line;
     tree expr;
{
  register tree t = make_node (EXPR_STMT);

  STMT_SOURCE_FILE (t) = filename;
  STMT_SOURCE_LINE (t) = line;
  STMT_BODY (t) = expr;
  return t;
}

tree
build_if (filename, line, cond, thenclause, elseclause)
     char *filename;
     int line;
     tree cond, thenclause, elseclause;
{
  register tree t = make_node (IF_STMT);

  STMT_SOURCE_FILE (t) = filename;
  STMT_SOURCE_LINE (t) = line;
  STMT_COND (t) = cond;
  STMT_THEN (t) = thenclause;
  STMT_ELSE (t) = elseclause;
  return t;
}

tree
build_exit (filename, line, cond)
     char *filename;
     int line;
     tree cond;
{
  register tree t = make_node (EXIT_STMT);
  STMT_SOURCE_FILE (t) = filename;
  STMT_SOURCE_LINE (t) = line;
  STMT_BODY (t) = cond;
  return t;
}

tree
build_asm_stmt (filename, line, asmcode)
     char *filename;
     int line;
     tree asmcode;
{
  register tree t = make_node (ASM_STMT);
  STMT_SOURCE_FILE (t) = filename;
  STMT_SOURCE_LINE (t) = line;
  STMT_BODY (t) = asmcode;
  return t;
}

tree
build_case (filename, line, object, cases)
     char *filename;
     int line;
     tree object, cases;
{
  register tree t = make_node (CASE_STMT);
  STMT_SOURCE_FILE (t) = filename;
  STMT_SOURCE_LINE (t) = line;
  STMT_CASE_INDEX (t) = object;
  STMT_CASE_LIST (t) = cases;
  return t;
}

tree
build_let (filename, line, vars, body, supercontext, tags)
     char *filename;
     int line;
     tree vars, body, supercontext, tags;
{
  register tree t = make_node (LET_STMT);
  STMT_SOURCE_FILE (t) = filename;
  STMT_SOURCE_LINE (t) = line;
  STMT_VARS (t) = vars;
  STMT_BODY (t) = body;
  STMT_SUPERCONTEXT (t) = supercontext;
  STMT_BIND_SIZE (t) = 0;
  STMT_TYPE_TAGS (t) = tags;
  return t;
}

tree
build_loop (filename, line, body)
     char *filename;
     int line;
     tree body;
{
  register tree t = make_node (LOOP_STMT);
  STMT_SOURCE_FILE (t) = filename;
  STMT_SOURCE_LINE (t) = line;
  STMT_BODY (t) = body;
  return t;
}

tree
build_compound (filename, line, body)
     char *filename;
     int line;
     tree body;
{
  register tree t = make_node (COMPOUND_STMT);
  STMT_SOURCE_FILE (t) = filename;
  STMT_SOURCE_LINE (t) = line;
  STMT_BODY (t) = body;
  return t;
}

/* Return a type like TYPE except that its TREE_READONLY is CONSTP
   and its TREE_VOLATILE is VOLATILEP.

   Such variant types already made are recorded so that duplicates
   are not made.

   A variant types should never be used as the type of an expression.
   Always copy the variant information into the TREE_READONLY
   and TREE_VOLATILE of the expression, and then give the expression
   as its type the "main variant", the variant whose TREE_READONLY
   and TREE_VOLATILE are zero.  Use TYPE_MAIN_VARIANT to find the
   main variant.  */

tree
build_type_variant (type, constp, volatilep)
     tree type;
     int constp, volatilep;
{
  register tree t, m = TYPE_MAIN_VARIANT (type);
  register struct obstack *ambient_obstack = current_obstack;

  /* Treat any nonzero argument as 1.  */
  constp = !!constp;
  volatilep = !!volatilep;

  /* First search the chain variants for one that is what we want.  */

  for (t = m; t; t = TYPE_NEXT_VARIANT (t))
    if (constp == TREE_READONLY (t)
	&& volatilep == TREE_VOLATILE (t))
      return t;

  /* We need a new one.  */
  current_obstack = TREE_PERMANENT (type) ? &permanent_obstack : &temporary_obstack;

  t = copy_node (type);
  TREE_READONLY (t) = constp;
  TREE_VOLATILE (t) = volatilep;
  TYPE_POINTER_TO (t) = 0;

  /* Add this type to the chain of variants of TYPE.  */
  TYPE_NEXT_VARIANT (t) = TYPE_NEXT_VARIANT (m);
  TYPE_NEXT_VARIANT (m) = t;

  current_obstack = ambient_obstack;
  return t;
}

/* Constructors for pointer, array and function types.
   (RECORD_TYPE, UNION_TYPE and ENUMERAL_TYPE nodes are
   constructed by language-dependent code, not here.)  */

tree
build_pointer_type (to_type)
     tree to_type;
{
  register tree t = TYPE_POINTER_TO (to_type);
  register struct obstack *ambient_obstack = current_obstack;

  /* First, if we already have a type for pointers to TO_TYPE, use it.  */

  if (t)
    return t;

  /* We need a new one.  If TO_TYPE is permanent, make this permanent too.  */
  current_obstack = (TREE_PERMANENT (to_type)
		     ? &permanent_obstack
		     : &temporary_obstack);

  t = make_node (POINTER_TYPE);
  TREE_TYPE (t) = to_type;

  /* Record this type as the pointer to TO_TYPE.  */
  TYPE_POINTER_TO (to_type) = t;

  /* If this type is permanent but we are really inside a function,
     lay it out now, so that the size, etc. are permanent too.  */
  if (current_obstack != ambient_obstack)
    layout_type (t);

  current_obstack = ambient_obstack;
  return t;
}

tree
build_array_type (elt_type, index_type)
     tree elt_type, index_type;
{
  register tree t = make_node (ARRAY_TYPE);

  if (TREE_CODE (elt_type) == FUNCTION_TYPE)
    {
      yyerror ("arrays of functions are not meaningful");
      elt_type = integer_type_node;
    }

  TREE_TYPE (t) = elt_type;
  TYPE_DOMAIN (t) = index_type;
  /* Make sure TYPE_POINTER_TO (elt_type) is filled in.  */
  build_pointer_type (elt_type);
  return t;
}

/* Build a function type, which is a FUNCTION_TYPE node.
  The TREE_TYPE of this node is the type of the value returned.
  If no value is returned, the TREE_TYPE may be 0 or it
  may be a node for a void type.
  ARG_TYPES is a chain of TREE_LIST nodes whose TREE_VALUEs
  are data type nodes for the arguments of the function.  */

tree
build_function_type (value_type, arg_types)
     tree value_type, arg_types;
{
  register tree t;

  if (TREE_CODE (value_type) == FUNCTION_DECL
      || TREE_CODE (value_type) == ARRAY_TYPE)
    {
      yyerror ("function return type cannot be function or array");
      value_type = integer_type_node;
    }

  t = make_node (FUNCTION_TYPE);
  TREE_TYPE (t) = value_type;
  TYPE_ARG_TYPES (t) = arg_types;
  return t;
}

/* Return OP, stripped of any conversions to wider types as much as is safe.
   Converting the value back to OP's type makes a value equivalent to OP.

   If FOR_TYPE is nonzero, we return a value which, if converted to
   type FOR_TYPE, would be equivalent to converting OP to type FOR_TYPE.

   OP must have integer, real or enumeral type.  Pointers are not allowed!

   There are some cases where the obvious value we could return
   would regenerate to OP if converted to OP's type, 
   but would not extend like OP to wider types.
   If FOR_TYPE indicates such extension is contemplated, we eschew such values.
   For example, if OP is (unsigned short)(signed char)-1,
   we avoid returning (signed char)-1 if FOR_TYPE is int,
   even though extending that to an unsigned short would regenerate OP,
   since the result of extending (signed char)-1 to (int)
   is different from (int) OP.  */

tree
get_unwidened (op, for_type)
     register tree op;
     tree for_type;
{
  /* Set UNS initially if converting OP to FOR_TYPE is a zero-extension.  */
  /* TYPE_PRECISION is safe in place of type_precision since
     pointer types are not allowed.  */
  register tree type = TREE_TYPE (op);
  register int final_prec = TYPE_PRECISION (for_type != 0 ? for_type : type);
  register int uns
    = (for_type != 0 && for_type != type
       && final_prec > TYPE_PRECISION (type)
       && type_unsigned_p (type));
  register tree win = op;

  while (TREE_CODE (op) == NOP_EXPR)
    {
      register int bitschange
	= TYPE_PRECISION (TREE_TYPE (op))
	  - TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (op, 0)));

      /* Truncations are many-one so cannot be removed.
	 Unless we are later going to truncate down even farther.  */
      if (bitschange < 0
	  && final_prec > TYPE_PRECISION (TREE_TYPE (op)))
	break;

      /* See what's inside this conversion.  If we decide to strip it,
	 we will set WIN.  */
      op = TREE_OPERAND (op, 0);

      /* If we have not stripped any zero-extensions (uns is 0),
	 we can strip any kind of extension.
	 If we have previously stripped a zero-extension,
	 only zero-extensions can safely be stripped.
	 Any extension can be stripped if the bits it would produce
	 are all going to be discarded later by truncating to FOR_TYPE.  */

      if (bitschange > 0)
	{
	  if (! uns || final_prec <= TYPE_PRECISION (TREE_TYPE (op)))
	    win = op;
	  /* type_unsigned_p says whether this is a zero-extension.
	     Let's avoid computing it if it does not affect WIN
	     and if UNS will not be needed again.  */
	  if ((uns || TREE_CODE (op) == NOP_EXPR)
	      && type_unsigned_p (TREE_TYPE (op)))
	    {
	      uns = 1;
	      win = op;
	    }
	}
    }

  if (TREE_CODE (op) == COMPONENT_REF)
      {
	int innerprec = (TREE_INT_CST_LOW (DECL_SIZE (TREE_OPERAND (op, 1)))
			 * DECL_SIZE_UNIT (TREE_OPERAND (op, 1)));
	type = type_for_size (innerprec, type_unsigned_p (TREE_TYPE (op)));

	/* We can get this structure field in the narrowest type it fits in
	   but the resulting extension to its nominal type (a fullword type)
	   must fit the same conditions as for other extensions.  */

	if (innerprec < TYPE_PRECISION (TREE_TYPE (op))
	    && (! uns || final_prec <= innerprec
		|| type_unsigned_p (TREE_TYPE (op))))
	  {
	    if (type != 0)
	      {
		win = build2 (COMPONENT_REF, TREE_OPERAND (op, 0),
			      TREE_OPERAND (op, 1));
		TREE_TYPE (win) = type;
	      }
	  }
      }
  return win;
}

/* Return OP or a simpler expression for a narrower value
   which can be sign-extended or zero-extended to give back OP.
   Store in *UNSIGNEDP_PTR either 1 if the value should be zero-extended
   or 0 if the value should be sign-extended.  */

tree
get_narrower (op, unsignedp_ptr)
     register tree op;
     int *unsignedp_ptr;
{
  register int uns = 0;
  int first = 1;
  register tree win = op;

  while (TREE_CODE (op) == NOP_EXPR)
    {
      register int bitschange
	= TYPE_PRECISION (TREE_TYPE (op))
	  - TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (op, 0)));

      /* Truncations are many-one so cannot be removed.  */
      if (bitschange < 0)
	break;

      /* See what's inside this conversion.  If we decide to strip it,
	 we will set WIN.  */
      op = TREE_OPERAND (op, 0);

      if (bitschange > 0)
	{
	  /* An extension: the outermost one can be stripped,
	     but remember whether it is zero or sign extension.  */
	  if (first)
	    uns = type_unsigned_p (TREE_TYPE (op));
	  /* Otherwise, if a sign extension has been stripped,
	     only sign extensions can now be stripped;
	     if a zero extension has been stripped, only zero-extensions.  */
	  else if (uns != type_unsigned_p (TREE_TYPE (op)))
	    break;
	  first = 0;
	}
      /* A change in nominal type can always be stripped.  */

      win = op;
    }

  if (TREE_CODE (op) == COMPONENT_REF)
    {
      int innerprec = (TREE_INT_CST_LOW (DECL_SIZE (TREE_OPERAND (op, 1)))
		       * DECL_SIZE_UNIT (TREE_OPERAND (op, 1)));
      tree type = type_for_size (innerprec, type_unsigned_p (TREE_TYPE (op)));

      /* We can get this structure field in a narrower type that fits it,
	 but the resulting extension to its nominal type (a fullword type)
	 must satisfy the same conditions as for other extensions.  */

      if (innerprec < TYPE_PRECISION (TREE_TYPE (op))
	  && (first || uns == type_unsigned_p (TREE_TYPE (op)))
	  && type != 0)
	{
	  win = build2 (COMPONENT_REF, TREE_OPERAND (op, 0),
			TREE_OPERAND (op, 1));
	  TREE_TYPE (win) = type;
	}
    }
  *unsignedp_ptr = uns;
  return win;
}

/* Return the precision of a type, for arithmetic purposes.
   Supports all types on which arithmetic is possible
   (including pointer types).
   It's not clear yet what will be right for complex types.  */

int
type_precision (type)
     register tree type;
{
  return ((TREE_CODE (type) == INTEGER_TYPE
	   || TREE_CODE (type) == ENUMERAL_TYPE
	   || TREE_CODE (type) == REAL_TYPE)
	  ? TYPE_PRECISION (type) : BITS_PER_WORD);
}

/* Nonzero if integer constant C has a value that is permissible
   for type TYPE (an INTEGER_TYPE).  */

int
int_fits_type_p (c, type)
     tree c, type;
{
  if (type_unsigned_p (type))
    return (!INT_CST_LT_UNSIGNED (TYPE_MAX_VALUE (type), c)
	    && !INT_CST_LT_UNSIGNED (c, TYPE_MIN_VALUE (type)));
  else
    return (!INT_CST_LT (TYPE_MAX_VALUE (type), c)
	    && !INT_CST_LT (c, TYPE_MIN_VALUE (type)));
}

/* Subroutines of `convert'.  */

/* Change of width--truncation and extension of integers or reals--
   is represented with NOP_EXPR.  Proper functioning of many things
   assumes that no other conversions can be NOP_EXPRs.

   Conversion between integer and pointer is represented with CONVERT_EXPR.
   Converting integer to real uses FLOAT_EXPR
   and real to integer uses FIX_TRUNC_EXPR.  */

/* Generate an expression for a conversion using expression code CODE.
   It will convert EXPR to type TYPE.  */

static tree
build_convert (code, type, expr)
     enum tree_code code;
     tree type, expr;
{
  register tree tem = build1 (code, expr);

  TREE_TYPE (tem) = type;
  TREE_VOLATILE (tem) = TREE_VOLATILE (expr);
  return tem;
}

static tree
convert_to_pointer (type, expr)
     tree type, expr;
{
  register tree intype = TREE_TYPE (expr);
  register enum tree_code form = TREE_CODE (intype);
  
  if (integer_zerop (expr))
    {
      if (type == TREE_TYPE (null_pointer_node))
	return null_pointer_node;
      expr = build_int_2 (0, 0);
      TREE_TYPE (expr) = type;
      return expr;
    }

  if (form == POINTER_TYPE)
    return build_convert (NOP_EXPR, type, expr);

  if (intype == integer_type_node)
    return build_convert (CONVERT_EXPR, type, expr);

  if (form == INTEGER_TYPE || form == ENUMERAL_TYPE)
    return convert_to_pointer (type, convert (integer_type_node, expr));

  yyerror ("cannot convert to a pointer type");

  return null_pointer_node;
}

/* The result of this is always supposed to be a newly created tree node
   not in use in any existing structure.  */

static tree
convert_to_integer (type, expr)
     tree type, expr;
{
  register tree intype = TREE_TYPE (expr);
  register enum tree_code form = TREE_CODE (intype);
  extern tree build_binary_op_nodefault ();
  extern tree build_unary_op ();

  if (form == POINTER_TYPE)
    {
      if (integer_zerop (expr))
	expr = integer_zero_node;
      else
	expr = fold (build_convert (CONVERT_EXPR, integer_type_node, expr));
      intype = TREE_TYPE (expr);
      form = TREE_CODE (intype);
    }

  if (form == INTEGER_TYPE || form == ENUMERAL_TYPE)
    {
      register int outprec = TYPE_PRECISION (type);
      register int inprec = TYPE_PRECISION (intype);
      register enum tree_code ex_form = TREE_CODE (expr);

      if (outprec >= inprec)
	return build_convert (NOP_EXPR, type, expr);

/* Here detect when we can distribute the truncation down past some arithmetic.
   For example, if adding two longs and converting to an int,
   we can equally well convert both to ints and then add.
   For the operations handled here, such truncation distribution
   is always safe.
   It is desirable in these cases:
   1) when truncating down to full-word from a larger size
   2) when truncating takes no work.
   3) when at least one operand of the arithmetic has been extended
   (as by C's default conversions).  In this case we need two conversions
   if we do the arithmetic as already requested, so we might as well
   truncate both and then combine.  Perhaps that way we need only one.

   Note that in general we cannot do the arithmetic in a type
   shorter than the desired result of conversion, even if the operands
   are both extended from a shorter type, because they might overflow
   if combined in that type.  The exceptions to this--the times when
   two narrow values can be combined in their narrow type even to
   make a wider result--are handled by "shorten" in build_binary_op.  */

      switch (ex_form)
	{
	case RSHIFT_EXPR:
	  /* We can pass truncation down through right shifting
	     when the shift count is a negative constant.  */
	  if (TREE_CODE (TREE_OPERAND (expr, 1)) != INTEGER_CST
	      || TREE_INT_CST_LOW (TREE_OPERAND (expr, 1)) > 0)
	    break;
	  goto trunc1;

	case LSHIFT_EXPR:
	  /* We can pass truncation down through left shifting
	     when the shift count is a positive constant.  */
	  if (TREE_CODE (TREE_OPERAND (expr, 1)) != INTEGER_CST
	      || TREE_INT_CST_LOW (TREE_OPERAND (expr, 1)) < 0)
	    break;
	  /* In this case, shifting is like multiplication.  */

	case PLUS_EXPR:
	case MINUS_EXPR:
	case MULT_EXPR:
	case MAX_EXPR:
	case MIN_EXPR:
	case BIT_AND_EXPR:
	case BIT_IOR_EXPR:
	case BIT_XOR_EXPR:
	case BIT_ANDTC_EXPR:
	trunc1:
	  {
	    tree arg0 = get_unwidened (TREE_OPERAND (expr, 0), type);
	    tree arg1 = get_unwidened (TREE_OPERAND (expr, 1), type);

	    if (outprec >= BITS_PER_WORD
		|| TRULY_NOOP_TRUNCATION (outprec, inprec)
		|| inprec > TYPE_PRECISION (TREE_TYPE (arg0))
		|| inprec > TYPE_PRECISION (TREE_TYPE (arg1)))
	      {
		/* Do the arithmetic in type TYPEX,
		   then convert result to TYPE.  */
		register tree typex = type;

		/* Can't do arithmetic in enumeral types
		   so use an integer type that will hold the values.  */
		if (TREE_CODE (typex) == ENUMERAL_TYPE)
		  typex = type_for_size (TYPE_PRECISION (typex));

		/* But now perhaps TYPEX is as wide as INPREC.
		   In that case, do nothing special here.
		   (Otherwise would recurse infinitely in convert.  */
		if (TYPE_PRECISION (typex) != inprec)
		  {
		    /* Don't do unsigned arithmetic where signed was wanted,
		       or vice versa.  */
		    typex = (type_unsigned_p (TREE_TYPE (expr))
			     ? unsigned_type (typex) : signed_type (typex));
		    return convert (type,
				    build_binary_op_nodefault (ex_form,
							       convert (typex, arg0),
							       convert (typex, arg1)));
		  }
	      }
	  }
	  break;

	case EQ_EXPR:
	case NE_EXPR:
	case GT_EXPR:
	case GE_EXPR:
	case LT_EXPR:
	case LE_EXPR:
	case TRUTH_AND_EXPR:
	case TRUTH_OR_EXPR:
	case TRUTH_NOT_EXPR:
	  /* If we want result of comparison converted to a byte,
	     we can just regard it as a byte, since it is 0 or 1.  */
	  TREE_TYPE (expr) = type;
	  return expr;

	case NEGATE_EXPR:
	case BIT_NOT_EXPR:
	case ABS_EXPR:
	  {
	    register tree typex = type;

	    /* Can't do arithmetic in enumeral types
	       so use an integer type that will hold the values.  */
	    if (TREE_CODE (typex) == ENUMERAL_TYPE)
	      typex = type_for_size (TYPE_PRECISION (typex));

	    /* But now perhaps TYPEX is as wide as INPREC.
	       In that case, do nothing special here.
	       (Otherwise would recurse infinitely in convert.  */
	    if (TYPE_PRECISION (typex) != inprec)
	      {
		/* Don't do unsigned arithmetic where signed was wanted,
		   or vice versa.  */
		typex = (type_unsigned_p (TREE_TYPE (expr))
			 ? unsigned_type (typex) : signed_type (typex));
		return convert (type,
				build_unary_op (ex_form,
						convert (typex, TREE_OPERAND (expr, 0)),
						1));
	      }
	  }

	case NOP_EXPR:
	  /* If truncating after truncating, might as well do all at once.
	     If truncating after extending, we may get rid of wasted work.  */
	  return convert (type, get_unwidened (TREE_OPERAND (expr, 0), type));
	}

      return build_convert (NOP_EXPR, type, expr);
    }

  if (form == REAL_TYPE)
    return build_convert (FIX_TRUNC_EXPR, type, expr);

  yyerror ("aggregate value used where an integer was expected");

  {
    register tree tem = build_int_2 (0, 0);
    TREE_TYPE (tem) = type;
    return tem;
  }
}

static tree
convert_to_real (type, expr)
     tree type, expr;
{
  register enum tree_code form = TREE_CODE (TREE_TYPE (expr));

  if (form == REAL_TYPE)
    return build_convert (NOP_EXPR, type, expr);

  if (form == INTEGER_TYPE || form == ENUMERAL_TYPE)
    return build_convert (FLOAT_EXPR, type, expr);

  if (form == POINTER_TYPE)
    yyerror ("pointer value used where a float was expected");
  else
    yyerror ("aggregate value used where a float was expected");

  {
    register tree tem = make_node (REAL_CST);
    TREE_TYPE (tem) = type;
    TREE_REAL_CST (tem) = 0;
    return tem;
  }
}

/* Create an expression whose value is that of EXPR,
   converted to type TYPE.  The TREE_TYPE of the value
   is always TYPE.  This function implements all reasonable
   conversions; callers should filter out those that are
   not permitted by the language being compiled.  */

tree
convert (type, expr)
     tree type, expr;
{
  register tree e = expr;
  register enum tree_code code = TREE_CODE (type);

  if (type == TREE_TYPE (expr) || TREE_CODE (expr) == ERROR_MARK)
    return expr;
  if (TREE_CODE (TREE_TYPE (expr)) == VOID_TYPE)
    {
      yyerror ("void value not ignored as it ought to be");
      return error_mark_node;
    }
  if (code == VOID_TYPE)
    return build_convert (CONVERT_EXPR, type, e);
#if 0
  /* This is incorrect.  A truncation can't be stripped this way.
     Extensions will be stripped by the use of get_unwidened.  */
  if (TREE_CODE (expr) == NOP_EXPR)
    return convert (type, TREE_OPERAND (expr, 0));
#endif
  if (code == INTEGER_TYPE || code == ENUMERAL_TYPE)
    return fold (convert_to_integer (type, e));
  if (code == POINTER_TYPE)
    return fold (convert_to_pointer (type, e));
  if (code == REAL_TYPE)
    return fold (convert_to_real (type, e));

  yyerror ("conversion to non-scalar type requested");
  return error_mark_node;
}
