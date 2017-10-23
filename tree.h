/* Front-end tree definitions for GNU compiler.
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


/* codes of tree nodes */

#define DEFTREECODE(SYM, STRING, TYPE, NARGS)   SYM,

enum tree_code {
#include "tree.def"
};

#undef DEFTREECODE

typedef enum tree_code tree_code;

extern char *tree_code_type[];
extern int tree_code_length[];

/* Get the definition of `enum machine_mode' */

#ifndef HAVE_MACHINE_MODES
#define DEF_MACHMODE(SYM, NAME, TYPE, SIZE, UNIT)  SYM,

enum machine_mode {
#include "machmode.def"
};

#undef DEF_MACHMODE

#define HAVE_MACHINE_MODES

#endif /* not HAVE_MACHINE_MODES */

#ifndef NUM_MACHINE_MODES
#define NUM_MACHINE_MODES (int) MAX_MACHINE_MODE
#endif

/* Codes that identify the various built in functions
   so that expand_call can identify them quickly.  */

enum built_in_function
{
  NOT_BUILT_IN,
  BUILT_IN_ALLOCA,
  BUILT_IN_ABS,
  BUILT_IN_FABS,
  BUILT_IN_LABS,
  BUILT_IN_DIV,
  BUILT_IN_LDIV,
  BUILT_IN_FFLOOR,
  BUILT_IN_FCEIL,
  BUILT_IN_FMOD,
  BUILT_IN_FREM,
  BUILT_IN_MEMCPY,
  BUILT_IN_MEMCMP,
  BUILT_IN_MEMSET,
  BUILT_IN_FSQRT,
  BUILT_IN_GETEXP,
  BUILT_IN_GETMAN
};

#define TREE_UID(NODE) ((NODE)->shared.uid)
#define TREE_TYPE(NODE) ((NODE)->shared.type)
#define TREE_CHAIN(NODE) ((NODE)->shared.chain)
#define TREE_CODE(NODE) ((NODE)->shared.code)
#define TREE_SET_CODE(NODE, VALUE) ((NODE)->shared.code = (VALUE))

#define TREE_LITERAL(NODE) ((NODE)->shared.literal_attr)
#define TREE_STATIC(NODE) ((NODE)->shared.static_attr)
#define TREE_VOLATILE(NODE) ((NODE)->shared.volatile_attr)
#define TREE_THIS_VOLATILE(NODE) ((NODE)->shared.this_vol_attr)
#define TREE_READONLY(NODE) ((NODE)->shared.readonly_attr)
#define TREE_PUBLIC(NODE) ((NODE)->shared.public_attr)
#define TREE_PACKED(NODE) ((NODE)->shared.packed_attr)
#define TREE_NONLOCAL(NODE) ((NODE)->shared.nonlocal_attr)
#define TREE_EXTERNAL(NODE) ((NODE)->shared.external_attr)
#define TREE_PERMANENT(NODE) ((NODE)->shared.permanent_attr)
#define TREE_ADDRESSABLE(NODE) ((NODE)->shared.addressable_attr)
#define TREE_REGDECL(NODE) ((NODE)->shared.regdecl_attr)
#define TREE_UNSIGNED(NODE) ((NODE)->shared.unsigned_attr)

struct tree_shared
{
  int uid;
  union tree_node *chain;
  union tree_node *type;
  enum tree_code code : 8;	/* Give it a byte only */
    
/* the attributes: (special properties of node) */
  unsigned external_attr : 1;	/* name is external,
				   meaning storage address is assigned by
				   another module.  */
  unsigned public_attr : 1;	/* name is public in its module.
				   In C, this means, not declared "static" */
  unsigned static_attr : 1;	/* variable is static,
				   meaning storage gets a fixed address.  */
  unsigned volatile_attr : 1;	/* Expression's value can change unpredictably.
				   If any subexpression is volatile, this is
				   set, even if this operation itself
				   is not volatile.  */
  unsigned packed_attr : 1;	/* store this structure field in
				   a packed manner.  For FIELD_DECL nodes.
				   Could also be relevant for RECORD_TYPE,
				   ARRAY_TYPE, etc. in Pascal.  */
  unsigned readonly_attr : 1;	/* this var or field cannot be assigned.
				   Sometimes set in types, but never in a
				   type that is the type of an expression.  */
  unsigned literal_attr : 1;	/* value of expression is constant.
				   Appears on all ..._CST nodes,
				   and occasionally on expression nodes.  */
  unsigned nonlocal_attr : 1;	/* this name is ref'd from an inner block
				   Cannot happen in C because it does not
				   allow inner blocks, as of now.
				   For VAR_DECL nodes, and possibly
				   FUNCTION_DECL or LABEL_DECL nodes.  */
  unsigned permanent_attr : 1;	/* Nonzero means permanent node;
				   node will continue to exist for the
				   entire compiler run.  Otherwise it will be
				   recycled at the end of this function.  */
  unsigned addressable_attr : 1; /* Nonzero means address of this is needed.
				    So it cannot be in a register.
				    For VAR_DECL nodes.  */
  unsigned regdecl_attr : 1;	/* Nonzero means declared `register'.
				   For VAR_DECL nodes.  */
  unsigned this_vol_attr : 1;   /* Nonzero means this operation is
				   volatile (as opposed to an operand).  */
  unsigned unsigned_attr : 1;   /* Nonzero means this type or field
				   is unsigned.  */
};

/* In an INTEGER_CST node.  These two together make a 64 bit integer.
   If the data type is signed, the value is sign-extended to 64 bits
   even though not all of them may really be in use.
   In an unsigned constant shorter than 64 bits, the extra bits are 0.  */
#define TREE_INT_CST_LOW(NODE) ((NODE)->int_cst.int_cst_low)
#define TREE_INT_CST_HIGH(NODE) ((NODE)->int_cst.int_cst_high)

#define INT_CST_LT(A, B)  \
(TREE_INT_CST_HIGH (A) < TREE_INT_CST_HIGH (B)			\
 || (TREE_INT_CST_HIGH (A) == TREE_INT_CST_HIGH (B)		\
     && ((unsigned) TREE_INT_CST_LOW (A) < (unsigned) TREE_INT_CST_LOW (B))))

#define INT_CST_LT_UNSIGNED(A, B)  \
((unsigned) TREE_INT_CST_HIGH (A) < (unsigned) TREE_INT_CST_HIGH (B)	  \
 || ((unsigned) TREE_INT_CST_HIGH (A) == (unsigned) TREE_INT_CST_HIGH (B) \
     && ((unsigned) TREE_INT_CST_LOW (A) < (unsigned) TREE_INT_CST_LOW (B))))

struct tree_int_cst
{
  char shared[sizeof (struct tree_shared)];
  long int_cst_low;
  long int_cst_high;
};

/* In REAL_CST, STRING_CST and COMPLEX_CST nodes,
   and generally in all kinds of constants that could
   be given labels (rather than being immediate).  */

#define TREE_CST_RTL(NODE) ((NODE)->real_cst.rtl)

/* In a REAL_CST node.  */
#define TREE_REAL_CST(NODE) ((NODE)->real_cst.real_cst)

struct tree_real_cst
{
  char shared[sizeof (struct tree_shared)];
  struct rtx_def *rtl;	/* acts as link to register transfer language
				   (rtl) info */
  double real_cst;
};

/* In a STRING_CST */
#define TREE_STRING_LENGTH(NODE) ((NODE)->string.length)
#define TREE_STRING_POINTER(NODE) ((NODE)->string.pointer)

struct tree_string
{
  char shared[sizeof (struct tree_shared)];
  struct rtx_def *rtl;	/* acts as link to register transfer language
				   (rtl) info */
  int length;
  char *pointer;
};

/* In a COMPLEX_CST node.  */
#define TREE_REALPART(NODE) ((NODE)->complex.real)
#define TREE_IMAGPART(NODE) ((NODE)->complex.imag)

struct tree_complex
{
  char shared[sizeof (struct tree_shared)];
  struct rtx_def *rtl;	/* acts as link to register transfer language
				   (rtl) info */
  union tree_node *real;
  union tree_node *imag;
};

#define IDENTIFIER_LENGTH(NODE) ((NODE)->identifier.length)
#define IDENTIFIER_POINTER(NODE) ((NODE)->identifier.pointer)
#define IDENTIFIER_GLOBAL_VALUE(NODE) ((NODE)->identifier.global_value)
#define IDENTIFIER_LOCAL_VALUE(NODE) ((NODE)->identifier.local_value)
#define IDENTIFIER_LABEL_VALUE(NODE) ((NODE)->identifier.label_value)

struct tree_identifier
{
  char shared[sizeof (struct tree_shared)];
  int length;
  char *pointer;
  union tree_node *global_value;
  union tree_node *local_value;
  union tree_node *label_value;
};

/* In a TREE_LIST node.  */
#define TREE_PURPOSE(NODE) ((NODE)->exp.operands[0])
#define TREE_VALUE(NODE) ((NODE)->exp.operands[1])

/* In a SAVE_EXPR node.  */
#define SAVE_EXPR_RTL(NODE) (*(struct rtx_def **) &(NODE)->exp.operands[1])

/* In a CALL_EXPR node.  */
#define CALL_EXPR_RTL(NODE) (*(struct rtx_def **) &(NODE)->exp.operands[2])

#define TREE_OPERAND(NODE, I) ((NODE)->exp.operands[I])

struct tree_exp
{
  char shared[sizeof (struct tree_shared)];
  union tree_node *operands[1];
};

/* In a data type node.  */
#define TYPE_SIZE(NODE) ((NODE)->type.size)
#define TYPE_SIZE_UNIT(NODE) ((NODE)->type.size_unit)
#define TYPE_MODE(NODE) ((NODE)->type.mode)
#define TYPE_ALIGN(NODE) ((NODE)->type.align)
#define TYPE_VALUES(NODE) ((NODE)->type.values)
#define TYPE_DOMAIN(NODE) ((NODE)->type.values)
#define TYPE_FIELDS(NODE) ((NODE)->type.values)
#define TYPE_ARG_TYPES(NODE) ((NODE)->type.values)
#define TYPE_ELT_MODE(NODE) ((NODE)->shared.type)
#define TYPE_SEP(NODE) ((NODE)->type.sep)
#define TYPE_SEP_UNIT(NODE) ((NODE)->type.sep_unit)
#define TYPE_POINTER_TO(NODE) ((NODE)->type.pointer_to)
#define TYPE_MIN_VALUE(NODE) ((NODE)->type.sep)
#define TYPE_MAX_VALUE(NODE) ((NODE)->type.max)
#define TYPE_PRECISION(NODE) ((NODE)->type.sep_unit)
#define TYPE_PARSE_INFO(NODE) ((NODE)->type.parse_info)
#define TYPE_SYMTAB_ADDRESS(NODE) ((NODE)->type.symtab_address)
#define TYPE_NAME(NODE) ((NODE)->type.name)
#define TYPE_NEXT_VARIANT(NODE) ((NODE)->type.next_variant)
#define TYPE_MAIN_VARIANT(NODE) ((NODE)->type.main_variant)

struct tree_type
{
  char shared[sizeof (struct tree_shared)];
  union tree_node *values;
  union tree_node *sep;
  union tree_node *size;
  enum machine_mode mode;
  unsigned char size_unit;
  unsigned char align;
  unsigned char sep_unit;
  enum machine_mode elt_mode;
  union tree_node *pointer_to;
  int parse_info;
  int symtab_address;
  union tree_node *name;
  union tree_node *max;
  union tree_node *next_variant;
  union tree_node *main_variant;
};

#define DECL_VOFFSET(NODE) ((NODE)->decl.voffset)
#define DECL_VOFFSET_UNIT(NODE) ((NODE)->decl.voffset_unit)
#define DECL_OFFSET(NODE) ((NODE)->decl.offset)
#define DECL_FUNCTION_CODE(NODE) ((enum built_in_function) (NODE)->decl.offset)
#define DECL_SET_FUNCTION_CODE(NODE,VAL) ((NODE)->decl.offset = (int) (VAL))
#define DECL_NAME(NODE) ((NODE)->decl.name)
#define DECL_CONTEXT(NODE) ((NODE)->decl.context)
#define DECL_ARGUMENTS(NODE) ((NODE)->decl.arguments)
#define DECL_ARG_TYPE(NODE) ((NODE)->decl.arguments)
#define DECL_RESULT(NODE) ((NODE)->decl.result)
#define DECL_INITIAL(NODE) ((NODE)->decl.initial)
#define DECL_SOURCE_FILE(NODE) ((NODE)->decl.filename)
#define DECL_SOURCE_LINE(NODE) ((NODE)->decl.linenum)
#define DECL_SIZE(NODE) ((NODE)->decl.size)
#define DECL_SIZE_UNIT(NODE) ((NODE)->decl.size_unit)
#define DECL_ALIGN(NODE) ((NODE)->decl.align)
#define DECL_MODE(NODE) ((NODE)->decl.mode)
#define DECL_RTL(NODE) ((NODE)->decl.rtl)
#define DECL_BLOCK_SYMTAB_ADDRESS(NODE) ((NODE)->decl.block_symtab_address)
#define DECL_SYMTAB_INDEX(NODE) ((NODE)->decl.block_symtab_address)

struct tree_decl
{
  char shared[sizeof (struct tree_shared)];
  char *filename;
  int linenum;
  union tree_node *size;
  enum machine_mode mode;
  unsigned char size_unit;
  unsigned char align;
  unsigned char voffset_unit;
  union tree_node *name;
  union tree_node *context;
  int unused;
  int offset;
  union tree_node *voffset;
  union tree_node *arguments;
  union tree_node *result;
  union tree_node *initial;
  struct rtx_def *rtl;	/* acts as link to register transfer language
				   (rtl) info */
  int block_symtab_address;
};

/* For LABEL_STMT, GOTO_STMT, RETURN_STMT, LOOP_STMT,
   COMPOUND_STMT, ASM_STMT.  */
#define STMT_SOURCE_LINE(NODE) ((NODE)->stmt.linenum)
#define STMT_SOURCE_FILE(NODE) ((NODE)->stmt.filename)
#define STMT_BODY(NODE) ((NODE)->stmt.body)

struct tree_stmt
{
  char shared[sizeof (struct tree_shared)];
  char *filename;
  int linenum;
  union tree_node *body;
};

/* For IF_STMT.  */

/* #define STMT_SOURCE_LINE(NODE) */
/* #define STMT_SOURCE_FILE(NODE) */
#define STMT_COND(NODE) ((NODE)->if_stmt.cond)
#define STMT_THEN(NODE) ((NODE)->if_stmt.thenpart)
#define STMT_ELSE(NODE) ((NODE)->if_stmt.elsepart)

struct tree_if_stmt
{
  char shared[sizeof (struct tree_shared)];
  char *filename;
  int linenum;
  union tree_node *cond, *thenpart, *elsepart;
};

/* For LET_STMT and WITH_STMT.  */

/* #define STMT_SOURCE_LINE(NODE) */
/* #define STMT_SOURCE_FILE(NODE) */
/* #define STMT_BODY(NODE) */
#define STMT_VARS(NODE) ((NODE)->bind_stmt.vars)
#define STMT_SUPERCONTEXT(NODE) ((NODE)->bind_stmt.supercontext)
#define STMT_BIND_SIZE(NODE) ((NODE)->bind_stmt.bind_size)
#define STMT_TYPE_TAGS(NODE) ((NODE)->bind_stmt.type_tags)

struct tree_bind_stmt
{
  char shared[sizeof (struct tree_shared)];
  char *filename;
  int linenum;
  union tree_node *body, *vars, *supercontext, *bind_size, *type_tags;
};

/* For CASE_STMT.  */

#define STMT_CASE_INDEX(NODE) ((NODE)->case_stmt.index)
#define STMT_CASE_LIST(NODE) ((NODE)->case_stmt.case_list)

struct tree_case_stmt
{
  char shared[sizeof (struct tree_shared)];
  char *filename;
  int linenum;
  union tree_node *index, *case_list;
};

union tree_node
{
  struct tree_shared shared;
  struct tree_int_cst int_cst;
  struct tree_real_cst real_cst;
  struct tree_string string;
  struct tree_complex complex;
  struct tree_identifier identifier;
  struct tree_decl decl;
  struct tree_type type;
  struct tree_exp exp;
  struct tree_stmt stmt;
  struct tree_if_stmt if_stmt;
  struct tree_bind_stmt bind_stmt;
  struct tree_case_stmt case_stmt;
};

typedef union tree_node *tree;

#define NULL_TREE (tree) NULL

extern char *oballoc ();
extern char *permalloc ();

extern tree make_node ();
extern tree copy_node ();
extern tree get_identifier ();

extern tree build_int_2 ();
extern tree build_real ();
extern tree build_real_from_string ();
extern tree build_real_from_int_cst ();
extern tree build_complex ();
extern tree build_string ();
extern tree build1 ();
extern tree build2 ();
extern tree build3 ();
extern tree build_tree_list ();
extern tree build_goto ();
extern tree build_return ();
extern tree build_if ();
extern tree build_exit ();
extern tree build_asm_stmt ();
extern tree build_case ();
extern tree build_let ();
extern tree build_loop ();
extern tree build_compound ();
extern tree build_expr_stmt ();

extern tree make_signed_type ();
extern tree make_unsigned_type ();
extern void fixup_unsigned_type ();
extern tree build_pointer_type ();
extern tree build_array_type ();
extern tree build_function_type ();

extern tree build_binary_op ();
extern tree build_indirect_ref ();
extern tree build_unary_op ();

/* Given a type node TYPE, and CONSTP and VOLATILEP, return a type
   for the same kind of data as TYPE describes.
   Variants point to the "main variant" (which has neither CONST nor VOLATILE)
   via TYPE_MAIN_VARIANT, and it points to a chain of other variants
   so that duplicate variants are never made.
   Only main variants should ever appear as types of expressions.  */
extern tree build_type_variant ();

/* Given a ..._TYPE node, calculate the TYPE_SIZE, TYPE_SIZE_UNIT,
   TYPE_ALIGN and TYPE_MODE fields.
   If called more than once on one node, does nothing except
   for the first time.  */
extern void layout_type ();

/* Given a VAR_DECL, PARM_DECL, RESULT_DECL or FIELD_DECL node,
   calculates the DECL_SIZE, DECL_SIZE_UNIT, DECL_ALIGN and DECL_MODE
   fields.  Call this only once for any given decl node.

   Second argument is the boundary that this field can be assumed to
   be starting at (in bits).  Zero means it can be assumed aligned
   on any boundary that may be needed.  */
extern void layout_decl ();

/* Fold constants as much as possible in an expression.
   Returns the simplified expression.
   Acts only on the top level of the expression;
   if the argument itself cannot be simplified, its
   subexpressions are not changed.  */
extern tree fold ();

/* combine (tree_code, exp1, exp2) where EXP1 and EXP2 are constants
   returns a constant expression for the result of performing
   the operation specified by TREE_CODE on EXP1 and EXP2.  */
extern tree combine ();

extern tree convert ();
extern tree convert_units ();
extern tree size_in_bytes ();

/* Concatenate two lists (chains of TREE_LIST nodes) X and Y
   by making the last node in X point to Y.
   Returns X, except if X is 0 returns Y.  */
extern tree chainon ();

/* Make a new TREE_LIST node from specified PURPOSE, VALUE and CHAIN.  */
extern tree tree_cons ();

/* Return the last tree node in a chain.  */
extern tree tree_last ();

/* Reverse the order of elements in a chain, and return the new head.  */
extern tree nreverse ();

/* Returns the length of a chain of nodes
   (number of chain pointers to follow before reaching a null pointer).  */
extern int list_length ();

/* integer_zerop (tree x) is nonzero if X is an integer constant of value 0 */
extern int integer_zerop ();

/* integer_onep (tree x) is nonzero if X is an integer constant of value 1 */
extern int integer_onep ();

/* integer_all_onesp (tree x) is nonzero if X is an integer constant
   all of whose significant bits are 1.  */
extern int integer_all_onesp ();

/* type_unsigned_p (tree x) is nonzero if the type X is an unsigned type
   (all of its possible values are >= 0).
   If X is a pointer type, the value is 1.
   If X is a real type, the value is 0.  */
extern int type_unsigned_p ();

/* staticp (tree x) is nonzero if X is a reference to data allocated
   at a fixed address in memory.  */
extern int staticp ();

/* Gets an error if argument X is not an lvalue.
   Also returns 1 if X is an lvalue, 0 if not.  */
extern int lvalue_or_else ();

/* save_expr (EXP, CTX) returns an expression equivalent to EXP
   but it can be used multiple times within context CTX
   and only evaluate EXP once.  CTX should be a LET_STMT node.  */

extern tree save_expr ();

/* stabilize_reference (EXP, CTX) returns an reference equivalent to EXP
   but it can be used multiple times within context CTX
   and only evaluate the subexpressions once.
   CTX should be a LET_STMT node.  */

extern tree stabilize_reference ();

/* Return EXP, stripped of any conversions to wider types
   in such a way that the result of converting to type FOR_TYPE
   is the same as if EXP were converted to FOR_TYPE.
   If FOR_TYPE is 0, it signifies EXP's type.  */

extern tree get_unwidened ();

/* Return OP or a simpler expression for a narrower value
   which can be sign-extended or zero-extended to give back OP.
   Store in *UNSIGNEDP_PTR either 1 if the value should be zero-extended
   or 0 if the value should be sign-extended.  */

extern tree get_narrower ();

/* Given two integer or real types, return the type for their sum.  */

extern tree commontype ();

/* Given PRECISION and UNSIGNEDP, return a suitable type-tree
   for an integer type with at least that precision.
   The definition of this resides in language-specific code
   as the repertoir of available types may vary.  */

extern tree type_for_size ();

/* Given an integer type T, return a type like T but unsigned.
   If T is unsigned, the value is T.  */
extern tree unsigned_type ();

/* Given an integer type T, return a type like T but signed.
   If T is signed, the value is T.  */
extern tree signed_type ();

/* dump_tree (FILE, TREE)
   writes a description of TREE to stdio stream FILE, recursively
   describing all nodes that TREE points to.
   However, when describing a node of local duration,
   nodes of permanent duration reached from it are not mentioned.
   The idea is that all the permanent nodes are described at once.  */
extern void dump_tree ();

/* An integer constant with value 0 */
extern tree integer_zero_node;

/* An integer constant with value 1 */
extern tree integer_one_node;

/* A constant of type pointer-to-int and value 0 */
extern tree null_pointer_node;

/* A node of type ERROR_MARK.  */
extern tree error_mark_node;

/* The type node for the void type.  */
extern tree void_type_node;

/* The type node for the ordinary (signed) integer type.  */
extern tree integer_type_node;

/* The type node for the unsigned integer type.  */
extern tree unsigned_type_node;

/* Points to the name of the input file from which the current input
   being parsed originally came (before it went into cpp).  */
extern char *input_filename;

/* Nonzero for -pedantic switch: warn about anything
   that standard C forbids.  */
extern int pedantic;
