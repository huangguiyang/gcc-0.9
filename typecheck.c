/* Build expressions with type checking for C compiler.
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


/* This file is part of the C front end.
   It contains routines to build C expressions given their operands,
   including computing the types of the result, C-specific error checks,
   and some optimization.

   There are also routines to build RETURN_STMT nodes and CASE_STMT nodes,
   and to process initializations in declarations (since they work
   like a strange sort of assignment).  */

#include "config.h"
#include <stdio.h>
#include "tree.h"
#include "c-tree.h"

static tree convert_for_assignment ();
static tree shorten_compare ();
static void binary_op_error ();
tree process_init_constructor ();

/* Return the _TYPE node describing the data type
   of the data which NODE represents as a C expression.
   Note that this is never an ARRAY_TYPE node;
   a POINTER_TYPE node is returned instead. */

tree
datatype (node)
     tree node;
{
  register tree type = TREE_TYPE (node);
  if (TREE_CODE (type) == ARRAY_TYPE)
    return TYPE_POINTER_TO (TREE_TYPE (type));
  return type;
}

/* Return the common type of two integer or real types.
  This is the type for the result of most arithmetic operations
  if the operands are of those two types.
  The arguments can also be expressions, which stand for their types.

  We do not deal with enumeral types here because they are automatically
  converted to integer types by all the operations which use commontype.  */

tree
commontype (type1, type2)
     tree type1, type2;
{
  register tree t1 = TREE_TYPE (type1);
  register tree t2 = TREE_TYPE (type2);

  /* quickly notice if the two types are the same.  */

  if (t1 == t2) return t1;

  {
    register enum tree_code form1 = TREE_CODE (t1);
    register enum tree_code form2 = TREE_CODE (t2);
  
    /* If only one is real, use it as the result.  */
  
    if (form1 == REAL_TYPE && form2 != REAL_TYPE)
      return t1;
  
    if (form2 == REAL_TYPE && form1 != REAL_TYPE)
      return t2;

    /* Both real or both integers; use the one with greater precision.  */

    if (TYPE_PRECISION (t1) > TYPE_PRECISION (t2))
      return t1;
    else if (TYPE_PRECISION (t2) > TYPE_PRECISION (t1))
      return t2;

    /* Same precision.  Prefer longs to ints even when same size.  */

    if (t1 == long_unsigned_type_node
	|| t2 == long_unsigned_type_node)
      return long_unsigned_type_node;

    if (t1 == long_integer_type_node
	|| t2 == long_integer_type_node)
      return long_integer_type_node;

    /* Otherwise prefer the unsigned one.  */

    if (type_unsigned_p (t1))
      return t1;
    else return t2;
  }
}

/* Return 1 if TYPE1 and TYPE2 are compatible types for assignment
  or various other operations.  Used elsewhere only for pointer types,
  but used recursively on other kinds of types.  */

int
comptypes (type1, type2)
     tree type1, type2;
{
  register tree t1 = type1;
  register tree t2 = type2;

  /* suppress errors caused by previously reported errors */
  if (t1 == t2 || TREE_CODE (t1) == ERROR_MARK || TREE_CODE (t2) == ERROR_MARK)
    return 1;

  if (TREE_CODE (t1) != TREE_CODE (t2)) return 0;

  switch (TREE_CODE (t1))
    {
    case POINTER_TYPE:
      return (TREE_TYPE (t1) == TREE_TYPE (t2)
	      || comptypes (TREE_TYPE (t1), TREE_TYPE (t2)));

    case FUNCTION_TYPE:
      return ((TREE_TYPE (t1) == TREE_TYPE (t2)
	       || comptypes (TREE_TYPE (t1), TREE_TYPE (t2)))
	      && compparms (TYPE_ARG_TYPES (t1), TYPE_ARG_TYPES (t2)));

    case ARRAY_TYPE:
      /* Target types must match.  */
      if (!(TREE_TYPE (t1) == TREE_TYPE (t2)
	    || comptypes (TREE_TYPE (t1), TREE_TYPE (t2))))
	return 0;
      {
	tree d1 = TYPE_DOMAIN (t1);
	tree d2 = TYPE_DOMAIN (t2);

	/* Sizes must match unless one is missing.  */
	if (d1 == 0 || d2 == 0 || d1 == d2)
	  return 1;

	return ((TREE_INT_CST_LOW (TYPE_MIN_VALUE (d1))
		 == TREE_INT_CST_LOW (TYPE_MIN_VALUE (d2)))
		&& (TREE_INT_CST_HIGH (TYPE_MIN_VALUE (d1))
		    == TREE_INT_CST_HIGH (TYPE_MIN_VALUE (d2)))
		&& (TREE_INT_CST_LOW (TYPE_MAX_VALUE (d1))
		    == TREE_INT_CST_LOW (TYPE_MAX_VALUE (d2)))
		&& (TREE_INT_CST_HIGH (TYPE_MAX_VALUE (d1))
		    == TREE_INT_CST_HIGH (TYPE_MAX_VALUE (d2))));
      }
    }
  return 0;
}

/* Return 1 if two parameter type lists PARMS1 and PARMS2
   are equivalent in the sense that functions with those parameter types
   can have equivalent types.  */

int
compparms (parms1, parms2)
     tree parms1, parms2;
{
  register tree t1 = parms1, t2 = parms2;

  if (t1 == 0 && t2 == 0)
    return 1;

  if (t1 == 0)
    return compparms1 (t2);
  if (t2 == 0)
    return compparms1 (t1);

  while (t1 != 0 || t2 != 0)
    {
      if (t1 == 0 || t2 == 0)
	return 0;
      if (! comptypes (TREE_VALUE (t1), TREE_VALUE (t2)))
	return 0;
      if (t1 != 0)
	t1 = TREE_CHAIN (t1);
      if (t2 != 0)
	t2 = TREE_CHAIN (t2);
    }

  return 1;
}

/* Return 1 if PARMS specifies a fixed number of parameters
   and none of their types is affected by default promotions.  */

int
compparms1 (parms)
     tree parms;
{
  register tree t;
  for (t = parms; t; t = TREE_CHAIN (t))
    {
      register tree type = TREE_VALUE (t);

      if (TREE_CHAIN (t) == 0 && type != void_type_node)
	return 0;

      if (type == float_type_node)
	return 0;

      if (TREE_CODE (type) == INTEGER_TYPE
	  && TYPE_PRECISION (type) < TYPE_PRECISION (integer_type_node))
	return 0;
    }
  return 1;
}

/* Return 1 if TTL and TTR are essentially identical types
   so that a pointer to a TTR may be converted implicitly
   into a pointer to a TTL.

   In the case of pointers to arrays, it neglects to check
   that the array types pointed to have the same number of elements.  */

int
comp_target_types (ttl, ttr)
     tree ttl, ttr;
{
  ttr = TYPE_MAIN_VARIANT (ttr);
  ttl = TYPE_MAIN_VARIANT (ttl);
  if (ttr == ttl)
    return 1;

  if (TREE_CODE (ttr) == ARRAY_TYPE && TREE_CODE (ttl) == ARRAY_TYPE
      && comp_target_types (TREE_TYPE (ttr), TREE_TYPE (ttl)))
    return 1;

  if (TREE_CODE (ttr) == FUNCTION_TYPE && TREE_CODE (ttl) == FUNCTION_TYPE
      && comp_target_types (TREE_TYPE (ttr), TREE_TYPE (ttl)))
    return 1;

  return 0;
}

/* Return an unsigned type the same as TYPE in other respects.  */

tree
unsigned_type (type)
     tree type;
{
  if (type == char_type_node)
    return unsigned_char_type_node;
  if (type == integer_type_node)
    return unsigned_type_node;
  if (type == short_integer_type_node)
    return short_unsigned_type_node;
  if (type == long_integer_type_node)
    return long_unsigned_type_node;
  return type;
}

/* Return a signed type the same as TYPE in other respects.  */

tree
signed_type (type)
     tree type;
{
  if (type == unsigned_char_type_node)
    return char_type_node;
  if (type == unsigned_type_node)
    return integer_type_node;
  if (type == short_unsigned_type_node)
    return short_integer_type_node;
  if (type == long_unsigned_type_node)
    return long_integer_type_node;
  return type;
}

/* Return a type the same as TYPE except unsigned or
   signed according to UNSIGNEDP.  */

tree
signed_or_unsigned_type (unsignedp, type)
     int unsignedp;
     tree type;
{
  if (type == char_type_node)
    return unsignedp ? unsigned_char_type_node : char_type_node;
  if (type == integer_type_node)
    return unsignedp ? unsigned_type_node : integer_type_node;
  if (type == short_integer_type_node)
    return unsignedp ? short_unsigned_type_node : short_integer_type_node;
  if (type == long_integer_type_node)
    return unsignedp ? long_unsigned_type_node : long_integer_type_node;
  return type;
}

/* Return an integer type with BITS bits of precision,
   that is unsigned if UNSIGNEDP is nonzero, otherwise signed.  */

tree
type_for_size (bits, unsignedp)
     int bits;
     int unsignedp;
{
  if (bits <= TYPE_PRECISION (char_type_node))
    return unsignedp ? unsigned_char_type_node : char_type_node;

  if (bits <= TYPE_PRECISION (short_integer_type_node))
    return unsignedp ? short_unsigned_type_node : short_integer_type_node;

  if (bits <= TYPE_PRECISION (integer_type_node))
    return unsignedp ? unsigned_type_node : integer_type_node;

  if (bits <= TYPE_PRECISION (long_integer_type_node))
    return unsignedp ? long_unsigned_type_node : long_integer_type_node;

  return 0;
}

tree
c_sizeof (exp)
     tree exp;
{
  if (pedantic)
    {
      enum tree_code code = TREE_CODE (TREE_TYPE (exp));
      if (code == FUNCTION_TYPE)
	warning ("sizeof applied to a value of function type");
      if (code == VOID_TYPE)
	warning ("sizeof applied to a value of void type");
    }
  return size_in_bytes (exp);
}

/* Perform default promotions for C data used in expressions:
   arrays are converted to pointers;
   enumeral types or short or char, to int.
   In addition, manifest constants symbols are replaced by their values.  */

tree
default_conversion (exp)
     tree exp;
{
  register tree dt = TREE_TYPE (exp);
  register enum tree_code form = TREE_CODE (dt);

  if (TREE_CODE (exp) == CONST_DECL)
    exp = DECL_INITIAL (exp);

  if (form == ENUMERAL_TYPE
      || (form == INTEGER_TYPE
	  && (TYPE_PRECISION (dt)
	      < TYPE_PRECISION (integer_type_node))))
    return convert (integer_type_node, exp);
  if (form == VOID_TYPE)
    {
      yyerror ("void value not ignored as it ought to be");
      return error_mark_node;
    }
  if (form == FUNCTION_TYPE)
    {
      if (TREE_CODE (exp) == FUNCTION_DECL)
	return build_unary_op (ADDR_EXPR, exp, 0);

      yyerror ("function value acceptable only in function call");
      return error_mark_node;
    }
  if (form == ARRAY_TYPE)
    {
      register tree adr;
      if (TREE_CODE (exp) == INDIRECT_REF)
	return convert (TYPE_POINTER_TO (TREE_TYPE (dt)),
			TREE_OPERAND (exp, 0));
/* ??? This is not really quite correct
   in that the type of the operand of ADDR_EXPR
   is not the target type of the type of the ADDR_EXPR itself.
   Question is, can this lossage be avoided?  */
      adr = build1 (ADDR_EXPR, exp);
      mark_addressable (exp);
      TREE_TYPE (adr) = TYPE_POINTER_TO (TREE_TYPE (dt));
      TREE_LITERAL (adr) = staticp (exp);
      return adr;
    }
  return exp;
}

/* Prepare expr to be an argument to && or ||, or to be the condition
in an if or ?.  This preparation consists of taking the ordinary
representation of an expression expr and producing a valid pastel
boolean expression describing whether expr is nonzero.  We could
simply always do build_binary_op (NE_EXPR, expr, integer_zero_node),
but we optimize comparisons, &&, ||, and !  */

tree
truthvalue_conversion (expr)
     tree expr;
{
  register enum tree_code form = TREE_CODE (expr);

/* ??? This is redundant with code now in do_jump.
   Either decide that only TRUTH_ and relational operators are
   allowed inside TRUTH_ and conditional contexts,
   or eliminat ethe use of this function.  */

  if (form == EQ_EXPR && integer_zerop (TREE_OPERAND (expr, 1)))
    return build_unary_op (TRUTH_NOT_EXPR,
			   truthvalue_conversion (TREE_OPERAND (expr, 0)), 0);

  if (form == TRUTH_ANDIF_EXPR || form == TRUTH_ORIF_EXPR
      || form == TRUTH_AND_EXPR || form == TRUTH_OR_EXPR
      || form == TRUTH_NOT_EXPR
      || form == EQ_EXPR || form == NE_EXPR
      || form == LE_EXPR || form == GE_EXPR
      || form == LT_EXPR || form == GT_EXPR
      || form == ERROR_MARK)
    return expr;

  /* Unary minus has no effect on whether its argument is nonzero.  */
  if (form == NEGATE_EXPR || form == NOP_EXPR)
    return truthvalue_conversion (TREE_OPERAND (expr, 0));

  return build_binary_op (NE_EXPR, expr, integer_zero_node);
}

/* Given EXP, a tree node that is a reference to storage,
   return a new node with the same meaning
   but whose location will not be altered by any side
   effects elsewhere in the function.  */

tree
duplicate_reference (exp)
     tree exp;
{
  if (current_function_decl == 0)
    {
      yyerror ("nonconstant expression not inside function");
      return error_mark_node;
    }

  return stabilize_reference (exp, DECL_INITIAL (current_function_decl));
}

/* Make an expression to refer to the COMPONENT field of
   structure or union value DATUM.  COMPONENT is an IDENTIFIER_NODE.  */

tree
build_component_ref (datum, component)
     tree datum, component;
{
  register tree basename = datum;
  register tree basetype = TREE_TYPE (basename);
  register enum tree_code form = TREE_CODE (basetype);
  register tree field = NULL;
  register tree ref;

  /* First, see if there is a field or component with name COMPONENT. */

  if (form == RECORD_TYPE || form == UNION_TYPE)
    {
      /* Look up component name in the structure type definition.  */

      for (field = TYPE_FIELDS (basetype); field; field = TREE_CHAIN (field))
	{
	  if (DECL_NAME (field) == component)
	    break;
	}

      if (!field)
	{
	  yyerror (form == RECORD_TYPE
		   ? "structure has no member named %s"
		   : "union has no member named %s",
		   IDENTIFIER_POINTER (component));
	  return error_mark_node;
	}

      ref = build2 (COMPONENT_REF, basename, field);
      TREE_TYPE (ref) = TREE_TYPE (field);

      if (TREE_READONLY (basename) || TREE_READONLY (field))
	TREE_READONLY (ref) = 1;
      if (TREE_VOLATILE (basename) || TREE_VOLATILE (field))
	TREE_VOLATILE (ref) = 1;

      return ref;
    }
  else if (form != ERROR_MARK)
    yyerror ("request for member %s in something not a structure or union",
	    IDENTIFIER_POINTER (component));

  return error_mark_node;
}

/* Given an expression PTR for a pointer, return an expression
   for the value pointed to.  */

tree
build_indirect_ref (ptr)
     tree ptr;
{
  register tree pointer = default_conversion (ptr);
  register tree dt = TREE_TYPE (pointer);

  if (TREE_CODE (dt) == POINTER_TYPE)
    if (TREE_CODE (pointer) == ADDR_EXPR
	&& (TREE_TYPE (TREE_OPERAND (pointer, 0))
	    == TREE_TYPE (dt)))
      return TREE_OPERAND (pointer, 0);
    else
      {
	register tree ref = build1 (INDIRECT_REF, pointer);
	register tree t = resolve_tags (TREE_TYPE (dt));

	TREE_TYPE (ref) = TYPE_MAIN_VARIANT (t);
	TREE_READONLY (ref) = TREE_READONLY (t);
	TREE_VOLATILE (ref) = TREE_VOLATILE (t) || TREE_VOLATILE (pointer);
	TREE_THIS_VOLATILE (ref) = TREE_VOLATILE (t);
  	return ref;
      }
  else if (TREE_CODE (pointer) != ERROR_MARK)
    yyerror ("argument of unary * is not a pointer");
  return error_mark_node;
}

/* This handles expressions of the form "a[i]", which denotes
   an array reference.  Treat it as *(a+i).  */

tree
build_array_ref (array, index)
     tree array, index;
{
  if (TREE_CODE (index) == INTEGER_CST
      && TREE_CODE (TREE_TYPE (array)) == ARRAY_TYPE
      && TREE_CODE (array) != INDIRECT_REF)
    {
      register tree addr = build1 (ADDR_EXPR, array);
      register tree ref;
      TREE_TYPE (addr) = TYPE_POINTER_TO (TREE_TYPE (TREE_TYPE (array)));
      TREE_LITERAL (addr) = TREE_STATIC (array);
      ref = build2 (ARRAY_REF, addr, index);
      TREE_TYPE (ref) = TREE_TYPE (TREE_TYPE (array));
      return ref;
    }

  if (index)
    return build_indirect_ref (build_binary_op (PLUS_EXPR, array, index));

  yyerror ("subscript missing in array reference");
  return error_mark_node;
}

/* Build a function call to function FUNCTION with parameters PARAMS.
   PARAMS is a list--a chain of TREE_LIST nodes--in which the
   TREE_VALUE of each node is a parameter-expression.
   FUNCTION's data type may be a function type or a pointer-to-function.  */

tree
build_function_call (function, params)
     tree function, params;
{
  register tree fntype;
  register tree value_semantics;
  register tree coerced_params;
  tree actualparameterlist ();

  fntype = TREE_TYPE (function);

  /* since function names are converted automatically to pointers to functions,
     check here that we have a pointer to a function.  */

  if (TREE_CODE (fntype) == ERROR_MARK)
    return error_mark_node;

  if (TREE_CODE (fntype) == FUNCTION_TYPE)
    /* Allow calls to functions.
       (*fptr) (x) generates this,
       and so does foo (x), since we do not do a default_conversion.  */
    function = build_unary_op (ADDR_EXPR, function, 0);
  else if (TREE_CODE (fntype) == POINTER_TYPE
	   && TREE_CODE (TREE_TYPE (fntype)) == FUNCTION_TYPE)
    /* Allow calls to pointer-to-functions.
       This is what fpntr (x) generates.
       fpntr (x) is not strictly valid, but Berkeley compilers allow it.  */
    {
      /* fntype now gets the type of function pointed to.  */
      fntype = TREE_TYPE (fntype);
    }
  else
    {
      yyerror ("called object is not a function");
      return error_mark_node;
    }

  /* Convert the parameters to the types declared in the
     function prototype, or apply default promotions.  */

  coerced_params = actualparameterlist (TYPE_ARG_TYPES (fntype), params);

  /* Certain functions are built in.  If FUNCTION is one of them,
     create some other kind of expression, not a function call.  */

  if (TREE_CODE (TREE_OPERAND (function, 0)) == FUNCTION_DECL)
    switch (DECL_FUNCTION_CODE (TREE_OPERAND (function, 0)))
      {
      case BUILT_IN_ABS:
      case BUILT_IN_LABS:
      case BUILT_IN_FABS:
	if (coerced_params == 0)
	  return integer_zero_node;
	return build_unary_op (ABS_EXPR, TREE_VALUE (coerced_params), 0);
      }

  value_semantics = TREE_TYPE (fntype) ? TREE_TYPE (fntype) : void_type_node;
  
  {
    register tree result = 
      build2 (CALL_EXPR, function, coerced_params);

    TREE_TYPE (result) = value_semantics;
    TREE_VOLATILE (result) = 1;
    return result;
  }
}

/* Convert the actual parameter expressions in the list VALUES
   to the types in the list TYPELIST.
   If parmdecls is exhausted, or when an element has NULL as its type,
   perform the default conversions.

   This is also where warnings about wrong number of args are generated.
   
   Return a list of expressions for the parameters as converted.

   Both VALUES and the returned value are chains of TREE_LIST nodes
   with the elements of the list in the TREE_VALUE slots of those nodes.  */

tree
actualparameterlist (typelist, values)
     tree typelist, values;
{
  register tree typetail, valtail;
  register tree result = NULL;

  for (valtail = values, typetail = typelist;
       valtail;
       valtail = TREE_CHAIN (valtail))
    {
      register tree type = typetail ? TREE_VALUE (typetail) : 0;
      register tree val = TREE_VALUE (valtail);
      register tree parm;

      if (type == void_type_node)
	{
	  yyerror ("too many arguments to function");
	  break;
	}

      if (type != 0)
	parm = build_tree_list (0, convert (type, val));
      else if (TREE_CODE (TREE_TYPE (val)) == REAL_TYPE
               && (TYPE_PRECISION (TREE_TYPE (val))
	           < TYPE_PRECISION (double_type_node)))
	parm = build_tree_list (NULL_TREE,
				convert (double_type_node, val));
      else
	parm = build_tree_list (NULL_TREE, default_conversion (val));

      result = chainon (result, parm);
      if (typetail)
	typetail = TREE_CHAIN (typetail);
    }

  if (typetail != 0 && TREE_VALUE (typetail) != void_type_node)
    yyerror ("too few arguments to function call");

  return result;
}

/* Build a binary-operation expression, after performing default
   conversions on the operands.  CODE is the kind of expression to build.  */

tree
build_binary_op (code, arg1, arg2)
     enum tree_code code;
     tree arg1, arg2;
{
  return build_binary_op_nodefault (code, default_conversion (arg1),
				    default_conversion (arg2));
}

/* Build a binary-operation expression without default conversions.
   CODE is the kind of expression to build.
   This function differs from `build2' in several ways:
   the data type of the result is computed and recorded in it,
   warnings are generated if arg data types are invalid,
   special handling for addition and subtraction of pointers is known,
   and some optimization is done (operations on narrow ints
   are done in the narrower type when that gives the same result).
   Constant folding is also done before the result is returned.

   Note that the operands will never have enumeral types
   because either they have just had the default conversions performed
   or they have both just been converted to some other type in which
   the arithmetic is to be done.  */

tree
build_binary_op_nodefault (code, op1, op2)
     enum tree_code code;
     tree op1, op2;
{
  tree dt1 = datatype (op1), dt2 = datatype (op2);

  /* The expression codes of the data types of the arguments tell us
     whether the arguments are integers, floating, pointers, etc.  */
  register enum tree_code code1 = TREE_CODE (dt1);
  register enum tree_code code2 = TREE_CODE (dt2);

  /* Expression code to give to the expression when it is built.
     Normally this is CODE, which is what the caller asked for,
     but in some special cases we change it.  */
  register enum tree_code resultcode = code;

  /* Data type in which the computation is to be performed.
     In the simplest cases this is the common type of the arguments.  */
  register tree result_type = NULL;

  /* Nonzero means operands have already been type-converted
     in whatever way is necessary.
     Zero means they need to be converted to RESULT_TYPE.  */
  int converted = 0;

  /* Nonzero means after finally constructing the expression
     give it this type.  Otherwise, give it type RESULT_TYPE.  */
  tree final_type = 0;

  /* Nonzero if this is an operation like MIN or MAX which can
     safely be computed in short if both args are promoted shorts.
     Also implies COMMON.
     -1 indicates a bitwise operation; this makes a difference
     in the exact conditions for when it is safe to do the operation
     in a narrower mode.  */
  int shorten = 0;

  /* Nonzero if this is a comparison operation;
     if both args are promoted shorts, compare the original shorts.
     Also implies COMMON.  */
  int short_compare = 0;

  /* Nonzero if this is a right-shift operation, which can be computed on the
     original short and then promoted if the operand is a promoted short.  */
  int short_shift = 0;

  /* Nonzero means set RESULT_TYPE to the common type of the args.  */
  int common = 0;

  /* If an error was already reported for one of the arguments,
     avoid reporting another error.  */

  if (code1 == ERROR_MARK || code2 == ERROR_MARK)
    return op1;

  switch (code)
    {
    case PLUS_EXPR:
      /* Convert the int + pointer case into the pointer + int case.  */
      if (code2 == POINTER_TYPE)
	{
	  tree temp = op1;
	  op1 = op2;
	  op2 = temp;
	  temp = dt1;
	  dt1 = dt2;
	  dt2 = temp;
	  code1 = TREE_CODE (dt1);
	  code2 = TREE_CODE (dt2);
	}
      /* Handle the pointer + int case.  */
      if (code1 == POINTER_TYPE && code2 == INTEGER_TYPE)
	{
	  tree size_exp = (TREE_TYPE (dt1) == void_type_node ? integer_one_node
			   : size_in_bytes (TREE_TYPE (dt1)));

	  if (TREE_CODE (TREE_TYPE (dt1)) == VOID_TYPE)
	    warning ("pointer of type \"void *\" used in addition");
	  if (TREE_CODE (TREE_TYPE (dt1)) == FUNCTION_TYPE)
	    warning ("pointer to a function used in addition");

	  /* The result is a pointer of the same type that is being added.  */

	  result_type = dt1;

	  /* If what we are about to multiply by the size of the elements
	     contains a constant term, apply distributive law
	     and multiply that constant term separately.
	     This helps produce common subexpressions.  */

	  if ((TREE_CODE (op2) == PLUS_EXPR || TREE_CODE (op2) == MINUS_EXPR)
	      && ! TREE_LITERAL (op2)
	      && TREE_LITERAL (TREE_OPERAND (op2, 1))
	      && TREE_LITERAL (size_exp))
	    {
	      op1 = build_binary_op (TREE_CODE (op2), op1,
				     TREE_OPERAND (op2, 1));
	      op2 = TREE_OPERAND (op2, 0);
	    }

	  /* Replace the integer argument
	     with a suitable product by the object size.  */

	  op2 = build_binary_op (MULT_EXPR, op2, size_exp);

	  /* Inhibit later conversion of args.  Because we don't set COMMON,
	     RESULT_TYPE will be left as we have set it.  */

	  converted = 1;
	}
      else
	common = 1;
      break;

    case MINUS_EXPR:
      if (code1 == POINTER_TYPE)
	{
	  if (TREE_CODE (TREE_TYPE (dt1)) == VOID_TYPE)
	    warning ("pointer of type \"void *\" used in subtraction");
	  if (TREE_CODE (TREE_TYPE (dt1)) == FUNCTION_TYPE)
	    warning ("pointer to a function used in subtraction");
	}

      /* Subtraction of two similar pointers.
	 We must subtract them as integers, then divide by object size.  */

      if (code1 == POINTER_TYPE && dt1 == dt2)
	{
	  /* First do the subtraction as integers;
	     then drop through to build the divide operator.  */

	  op1 = build_binary_op (MINUS_EXPR,
				 convert (integer_type_node, op1),
				 convert (integer_type_node, op2));
	  op2 = (TREE_TYPE (dt1) == void_type_node ? integer_one_node
		 : size_in_bytes (TREE_TYPE (dt1)));

	  /* By altering RESULTCODE, we direct this function to build
	     the division operation.  If dividing by a power of 2,
	     use floor-division (rounding down) since that is what
	     a shift insn does.  Otherwise, since we can't use a shift anyway,
	     use whichever kind of rounding this machine does most easily.  */

	  if (TREE_CODE (op2) == INTEGER_CST
	      && exact_log2 (TREE_INT_CST_LOW (op2)))
	    resultcode = FLOOR_DIV_EXPR;
	  else
	    resultcode = EASY_DIV_EXPR;

	  /* Result of subtracting pointers is an int.  */

	  result_type = integer_type_node;

	  /* Arguments already have the types that should go in the
	     division expression.  */

	  converted = 1;
	}
      /* Handle pointer minus int.  Just like pointer plus int.  */
      else if (code1 == POINTER_TYPE && code2 == INTEGER_TYPE)
	{
	  result_type = dt1;
	  op2 = build_binary_op (MULT_EXPR, op2,
				 size_in_bytes (TREE_TYPE (dt1)));
	  converted = 1;
	}
      else
	common = 1;
      break;

    case MULT_EXPR:
      common = 1;
      break;

    case MAX_EXPR:
    case MIN_EXPR:
      shorten = 1;
      break;

    case TRUNC_DIV_EXPR:
      if ((code1 == INTEGER_TYPE || code1 == REAL_TYPE)
	       && (code2 == INTEGER_TYPE || code2 == REAL_TYPE))
	{
	  if (!(code1 == INTEGER_TYPE && code2 == INTEGER_TYPE))
	    resultcode = RDIV_EXPR;
	  else
	    shorten = 1;
	  common = 1;
	}
      break;

    case BIT_AND_EXPR:
    case BIT_ANDTC_EXPR:
    case BIT_IOR_EXPR:
    case BIT_XOR_EXPR:
      if (code1 == INTEGER_TYPE && code2 == INTEGER_TYPE)
	shorten = -1;
      /* If one operand is a constant, and the other is a short type
	 that has been converted to an int,
	 really do the work in the short type and then convert the
	 result to int.  If we are lucky, the constant will be 0 or 1
	 in the short type, making the entire operation go away.  */
      if (TREE_CODE (op1) == INTEGER_CST
	  && TREE_CODE (op2) == NOP_EXPR
	  && TYPE_PRECISION (dt2) > TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (op2, 0)))
	  && type_unsigned_p (TREE_TYPE (TREE_OPERAND (op2, 0))))
	{
	  final_type = result_type;
	  op2 = TREE_OPERAND (op2, 0);
	  result_type = TREE_TYPE (op2);
	}
      if (TREE_CODE (op2) == INTEGER_CST
	  && TREE_CODE (op1) == NOP_EXPR
	  && TYPE_PRECISION (dt1) > TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (op1, 0)))
	  && type_unsigned_p (TREE_TYPE (TREE_OPERAND (op1, 0))))
	{
	  final_type = result_type;
	  op1 = TREE_OPERAND (op1, 0);
	  result_type = TREE_TYPE (op1);
	}
      break;

    case TRUNC_MOD_EXPR:
      if (code1 == INTEGER_TYPE && code2 == INTEGER_TYPE)
	shorten = 1;
      break;

    case TRUTH_ANDIF_EXPR:
    case TRUTH_ORIF_EXPR:
    case TRUTH_AND_EXPR:
    case TRUTH_OR_EXPR:
      if ((code1 == INTEGER_TYPE || code1 == POINTER_TYPE || code1 == REAL_TYPE)
	  && (code2 == INTEGER_TYPE || code2 == POINTER_TYPE || code2 == REAL_TYPE))
	{
	  op1 = truthvalue_conversion (op1);
	  op2 = truthvalue_conversion (op2);
	  /* Result of these operations is always an int,
	     but that does not mean the operands should be
	     converted to ints!  */
	  result_type = integer_type_node;
	  converted = 1;
	}
      break;

      /* Shift operations: result has same type as first operand.
	 Also set SHORT_SHIFT if shifting rightward.  */

    case RSHIFT_EXPR:
      if (code1 == INTEGER_TYPE && code2 == INTEGER_TYPE)
	{
	  result_type = dt1;
	  if (TREE_CODE (op2) == INTEGER_CST
	      && TREE_INT_CST_LOW (op2) > 0)
	    short_shift = 1;
	}
      break;

    case LSHIFT_EXPR:
      if (code1 == INTEGER_TYPE && code2 == INTEGER_TYPE)
	{
	  result_type = dt1;
	  if (TREE_CODE (op2) == INTEGER_CST
	      && TREE_INT_CST_LOW (op2) < 0)
	    short_shift = 1;
	}
      break;

    case RROTATE_EXPR:
    case LROTATE_EXPR:
      if (code1 == INTEGER_TYPE && code2 == INTEGER_TYPE)
	result_type = dt1;
      break;

    case EQ_EXPR:
    case NE_EXPR:
      /* Result of comparison is always int,
	 but don't convert the args to int!  */
      result_type = integer_type_node;
      converted = 1;
      if ((code1 == INTEGER_TYPE || code1 == REAL_TYPE)
	  && (code2 == INTEGER_TYPE || code2 == REAL_TYPE))
	short_compare = 1;
      else if (code1 == POINTER_TYPE && code2 == POINTER_TYPE)
	{
	  register tree tt1 = TREE_TYPE (dt1);
	  register tree tt2 = TREE_TYPE (dt2);
	  /* Anything compares with void *.  void * compares with anything.
	     Otherwise, the targets must be the same
	     except for const and volatile.  */
	  if (! (tt1 == void_type_node
		 || tt2 == void_type_node
		 || comptypes (TYPE_MAIN_VARIANT (tt1), TYPE_MAIN_VARIANT (tt2))))
	    result_type = 0;
	}
      else if (code1 == POINTER_TYPE && TREE_CODE (op2) == INTEGER_CST
		&& integer_zerop (op2))
	op2 = null_pointer_node;
      else if (code2 == POINTER_TYPE && TREE_CODE (op1) == INTEGER_CST
		&& integer_zerop (op1))
	op1 = null_pointer_node;
      else
	/* If args are not valid, clear out RESULT_TYPE
	   to cause an error message later.  */
	result_type = 0;
      break;

    case LE_EXPR:
    case GE_EXPR:
    case LT_EXPR:
    case GT_EXPR:
      if ((code1 == INTEGER_TYPE || code1 == REAL_TYPE)
	   && (code2 == INTEGER_TYPE || code2 == REAL_TYPE))
	short_compare = 1;
      else if (code1 == POINTER_TYPE && comptypes (dt1, dt2))
	result_type = integer_type_node;
      converted = 1;
      break;
    }

  if ((code1 == INTEGER_TYPE || code1 == REAL_TYPE)
      && (code2 == INTEGER_TYPE || code2 == REAL_TYPE))
    {
      if (shorten || common || short_compare)
	result_type = commontype (op1, op2);

      /* For certain operations (which identify themselves by shorten != 0)
	 if both args were extended from the same smaller type,
	 do the arithmetic in that type and then extend.

	 shorten !=0 and !=1 indicates a bitwise operation.
	 For them, this optimization is safe only if
	 both args are zero-extended or both are sign-extended.
	 Otherwise, we might change the result.
	 Eg, (short)-1 | (unsigned short)-1 is (int)-1
	 but calculated in (unsigned short) it would be (unsigned short)-1.  */

      if (shorten)
	{
	  int unsigned0, unsigned1;
	  tree arg0 = get_narrower (op1, &unsigned0);
	  tree arg1 = get_narrower (op2, &unsigned1);
	  int uns = type_unsigned_p (result_type);
	  tree type;

	  final_type = result_type;

	  /* For bitwise operations, signedness of nominal type
	     does not matter.  Consider only how operands were extended.  */
	  if (shorten == -1)
	    uns = unsigned0;

	  /* Note that in all three cases below we refrain from optimizing
	     an unsigned operation on sign-extended args.
	     That would not be valid.  */

	  /* Both args variable: if both extended in same way
	     from same width, do it in that width.
	     Do it unsigned if args were zero-extended.  */
	  if ((TYPE_PRECISION (TREE_TYPE (arg0))
	       < TYPE_PRECISION (result_type))
	      && (TYPE_PRECISION (TREE_TYPE (arg1))
		  == TYPE_PRECISION (TREE_TYPE (arg0)))
	      && unsigned0 == unsigned1
	      && (unsigned0 || !uns))
	    result_type
	      = signed_or_unsigned_type (unsigned0,
					 commontype (arg0, arg1));
	  else if (TREE_CODE (arg0) == INTEGER_CST
		   && (unsigned1 || !uns)
		   && (TYPE_PRECISION (TREE_TYPE (arg1))
		       < TYPE_PRECISION (result_type))
		   && (type = signed_or_unsigned_type (unsigned1,
						       TREE_TYPE (arg1)),
		       int_fits_type_p (arg0, type)))
	    result_type = type;
	  else if (TREE_CODE (arg1) == INTEGER_CST
		   && (unsigned0 || !uns)
		   && (TYPE_PRECISION (TREE_TYPE (arg0))
		       < TYPE_PRECISION (result_type))
		   && (type = signed_or_unsigned_type (unsigned0,
						       TREE_TYPE (arg0)),
		       int_fits_type_p (arg1, type)))
	    result_type = type;
	}

      /* Shifts can be shortened if shifting right.  */

      if (short_shift)
	{
	  int unsigned_arg;
	  tree arg0 = get_narrower (op1, &unsigned_arg);

	  final_type = result_type;

	  if (TYPE_PRECISION (TREE_TYPE (arg0)) < TYPE_PRECISION (result_type)
	      /* If arg is sign-extended and then unsigned-shifted,
		 we can simulate this with a signed shift in arg's type
		 only if the extended result is at least twice as wide
		 as the arg.  Otherwise, the shift could use up all the
		 ones made by sign-extension and bring in zeros.
		 We can't optimize that case at all, but in most machines
		 it never happens because available widths are 2**N.  */
	      && (!type_unsigned_p (final_type)
		  || unsigned_arg
		  || 2 * TYPE_PRECISION (TREE_TYPE (arg0)) <= TYPE_PRECISION (result_type)))
	    {
	      /* Do an unsigned shift if the operand was zero-extended.  */
	      result_type
		= signed_or_unsigned_type (unsigned_arg,
					   TREE_TYPE (arg0));
	    }
	}

      /* Comparison operations are shortened too but differently.
	 They identify themselves by setting short_compare = 1.  */

      if (short_compare)
	{
	  /* Don't write &op1, etc., because that would prevent op1
	     from being kept in a register.
	     Instead, make copies of the our local variables and
	     pass the copies by reference, then copy them back afterward.  */
	  tree xop1 = op1, xop2 = op2, xresult_type = result_type;
	  enum tree_code xresultcode = resultcode;
	  tree val 
	    = shorten_compare (&xop1, &xop2, &xresult_type, &xresultcode);
	  if (val != 0)
	    return val;
	  op1 = xop1, op2 = xop2, result_type = xresult_type;
	  resultcode = xresultcode;
	}
    }

  /* At this point, RESULT_TYPE must be nonzero to avoid an error message.
     If CONVERTED is zero, both args will be converted to type RESULT_TYPE.
     Then the expression will be built.
     It will be given type FINAL_TYPE if that is nonzero;
     otherwise, it will be given type RESULT_TYPE.  */

  if (!result_type)
    {
      binary_op_error (code);
      return error_mark_node;
    }

  if (! converted)
    {
      if (TREE_TYPE (op1) != result_type)
	op1 = convert (result_type, op1); 
      if (TREE_TYPE (op2) != result_type)
	op2 = convert (result_type, op2); 
    }

  {
    register tree result = build2 (resultcode, op1, op2);
    register tree folded;

    TREE_TYPE (result) = result_type;
    folded = fold (result);
    if (folded == result)
      TREE_LITERAL (folded) = TREE_LITERAL (op1) & TREE_LITERAL (op2);
    TREE_VOLATILE (folded) = TREE_VOLATILE (op1) || TREE_VOLATILE (op2);
    if (final_type != 0)
      return convert (final_type, folded);
    return folded;
  }
}

/* Print an error message for invalid operands to arith operation CODE.  */

static void
binary_op_error (code)
     enum tree_code code;
{
  register char *opname;
  switch (code)
    {
    case PLUS_EXPR:
      opname = "+"; break;
    case MINUS_EXPR:
      opname = "-"; break;
    case MULT_EXPR:
      opname = "*"; break;
    case MAX_EXPR:
      opname = "max"; break;
    case MIN_EXPR:
      opname = "min"; break;
    case EQ_EXPR:
      opname = "=="; break;
    case NE_EXPR:
      opname = "!="; break;
    case LE_EXPR:
      opname = "<="; break;
    case GE_EXPR:
      opname = ">="; break;
    case LT_EXPR:
      opname = "<"; break;
    case GT_EXPR:
      opname = ">"; break;
    case LSHIFT_EXPR:
      opname = "<<"; break;
    case RSHIFT_EXPR:
      opname = ">>"; break;
    case TRUNC_MOD_EXPR:
      opname = "%"; break;
    case TRUNC_DIV_EXPR:
      opname = "/"; break;
    case BIT_AND_EXPR:
      opname = "&"; break;
    case BIT_IOR_EXPR:
      opname = "|"; break;
    case TRUTH_ANDIF_EXPR:
      opname = "&&"; break;
    case TRUTH_ORIF_EXPR:
      opname = "||"; break;
    case BIT_XOR_EXPR:
      opname = "^"; break;
    }
  yyerror ("invalid operands to binary %s", opname);
}

/* Subroutine of build_binary_op_nodefault, used for comparison operations.
   See if the operands have both been converted from subword integer types
   and, if so, perhaps change them both back to their original type.

   The arguments of this function are all pointers to local variables
   of build_binary_op_nodefault: OP1_PTR is &OP1, OP2_PTR is &OP2,
   RESTYPE_PTR is &RESULT_TYPE and RESCODE_PTR is &RESULTCODE.

   If this function returns nonzero, it means that the comparison has
   a constant value.  What this function returns is an expression for
   that value.  */

static tree
shorten_compare (op1_ptr, op2_ptr, restype_ptr, rescode_ptr)
     tree *op1_ptr, *op2_ptr;
     tree *restype_ptr;
     enum tree_code *rescode_ptr;
{
  register tree type;
  tree op1 = *op1_ptr;
  tree op2 = *op2_ptr;
  int unsignedp1, unsignedp2;
  tree primop1, primop2;
  enum tree_code code = *rescode_ptr;

  /* Throw away any conversions to wider types
     already present in the operands.  */

  primop1 = get_narrower (op1, &unsignedp1);
  primop2 = get_narrower (op2, &unsignedp2);

  /* If first arg is constant, swap the args (changing operation
     so value is preserved), for canonicalization.  */

  if (TREE_CODE (*restype_ptr) != REAL_TYPE
      && TREE_LITERAL (primop1))
    {
      register tree tem = primop1;
      register int temi = unsignedp1;
      primop1 = primop2;
      primop2 = tem;
      tem = op1;
      op1 = op2;
      op2 = tem;
      *op1_ptr = op1;
      *op2_ptr = op2;
      unsignedp1 = unsignedp2;
      unsignedp2 = temi;

      switch (code)
	{
	case LT_EXPR:
	  code = GT_EXPR;
	  break;
	case GT_EXPR:
	  code = LT_EXPR;
	  break;
	case LE_EXPR:
	  code = GE_EXPR;
	  break;
	case GE_EXPR:
	  code = LE_EXPR;
	  break;
	}
      *rescode_ptr = code;
    }

  /* If comparing an integer against a constant more bits wide,
     maybe we can deduce a value of 1 or 0 independent of the data.
     Or else truncate the constant now
     rather than extend the variable at run time.

     This is only interesting if the constant is the wider arg.
     Also, it is not safe if the constant is unsigned and the
     variable arg is signed, since in this case the variable
     would be sign-extended and then regarded as unsigned.
     Our technique fails in this case because the lowest/highest
     possible unsigned results don't follow naturally from the
     lowest/highest possible values of the variable operand.
     For just EQ_EXPR and NE_EXPR there is another technique that
     could be used: see if the constant can be faithfully represented
     in the other operand's type, by truncating it and reextending it
     and see if that preserves the constant's value.  */

  if (TREE_CODE (*restype_ptr) != REAL_TYPE
      && TREE_LITERAL (primop2)
      && TYPE_PRECISION (TREE_TYPE (primop1)) < TYPE_PRECISION (*restype_ptr))
    {
      int min_gt, max_gt, min_lt, max_lt;
      tree maxval, minval;
      /* 1 if comparison is nominally unsigned.  */
      int unsignedp = type_unsigned_p (*restype_ptr);
      tree val;

      type = signed_or_unsigned_type (unsignedp1, TREE_TYPE (primop1));

      maxval = TYPE_MAX_VALUE (type);
      minval = TYPE_MIN_VALUE (type);

      if (unsignedp && !unsignedp1)
	*restype_ptr = signed_type (*restype_ptr);

      if (TREE_TYPE (primop2) != *restype_ptr)
	primop2 = convert (*restype_ptr, primop2);
      if (type != *restype_ptr)
	{
	  minval = convert (*restype_ptr, minval);
	  maxval = convert (*restype_ptr, maxval);
	}

      if (unsignedp && unsignedp1)
	{
	  min_gt = INT_CST_LT_UNSIGNED (primop2, minval);
	  max_gt = INT_CST_LT_UNSIGNED (primop2, maxval);
	  min_lt = INT_CST_LT_UNSIGNED (minval, primop2);
	  max_lt = INT_CST_LT_UNSIGNED (maxval, primop2);
	}
      else
	{
	  min_gt = INT_CST_LT (primop2, minval);
	  max_gt = INT_CST_LT (primop2, maxval);
	  min_lt = INT_CST_LT (minval, primop2);
	  max_lt = INT_CST_LT (maxval, primop2);
	}

      val = 0;
      switch (code)
	{
	case NE_EXPR:
	  if (max_lt || min_gt)
	    val = integer_one_node;
	  break;

	case EQ_EXPR:
	  if (max_lt || min_gt)
	    val = integer_zero_node;
	  break;

	case LT_EXPR:
	  if (max_lt)
	    val = integer_one_node;
	  if (!min_lt)
	    val = integer_zero_node;
	  break;

	case GT_EXPR:
	  if (min_gt)
	    val = integer_one_node;
	  if (!max_gt)
	    val = integer_zero_node;
	  break;

	case LE_EXPR:
	  if (!max_gt)
	    val = integer_one_node;
	  if (min_gt)
	    val = integer_zero_node;
	  break;

	case GE_EXPR:
	  if (!min_lt)
	    val = integer_one_node;
	  if (max_lt)
	    val = integer_zero_node;
	  break;
	}

      /* If primop1 was sign-extended and unsigned comparison specd,
	 we did a signed comparison above using the signed type bounds.
	 But the comparison we output must be unsigned.

	 Also, for inequalities, VAL is no good; but if the signed
	 comparison had *any* fixed result, it follows that the
	 unsigned comparison just tests the sign in reverse
	 (positive values are LE, negative ones GE).
	 So we can generate an unsigned comparison
	 against an extreme value of the signed type.  */

      if (unsignedp && !unsignedp1)
	{
	  if (val != 0)
	    switch (code)
	      {
	      case LT_EXPR:
	      case GE_EXPR:
		primop2 = TYPE_MIN_VALUE (type);
		val = 0;
		break;

	      case LE_EXPR:
	      case GT_EXPR:
		primop2 = TYPE_MAX_VALUE (type);
		val = 0;
		break;
	      }
	  type = unsigned_type (type);
	}

      if (val == integer_zero_node)
	warning ("comparison is always 0 due to limited range of data type");
      if (val == integer_one_node)
	warning ("comparison is always 1 due to limited range of data type");
      if (val != 0)
	return val;

      /* Value is not predetermined, but do the comparison
	 in the type of the operand that is not constant.
	 TYPE is already properly set.  */
    }
  else if (TREE_CODE (*restype_ptr) == REAL_TYPE
	   && TYPE_PRECISION (TREE_TYPE (primop1)) == TYPE_PRECISION (TREE_TYPE (primop2)))
    type = TREE_TYPE (primop1);

  /* If args' natural types are both narrower than nominal type
     and both extend in the same manner, compare them
     in the type of the wider arg.
     Otherwise must actually extend both to the nominal
     common type lest different ways of extending
     alter the result.
     (eg, (short)-1 == (unsigned short)-1  should be 0.)  */

  else if (unsignedp1 == unsignedp2
	   && TYPE_PRECISION (TREE_TYPE (primop1)) < TYPE_PRECISION (*restype_ptr)
	   && TYPE_PRECISION (TREE_TYPE (primop2)) < TYPE_PRECISION (*restype_ptr))
    {
      type = commontype (primop1, primop2);
      type = signed_or_unsigned_type (unsignedp1
				      || type_unsigned_p (*restype_ptr),
				      type);
      /* Make sure shorter operand is extended the right way
	 to match the longer operand.  */
      primop1 = convert (signed_or_unsigned_type (unsignedp1, TREE_TYPE (primop1)),
			 primop1);
      primop2 = convert (signed_or_unsigned_type (unsignedp2, TREE_TYPE (primop2)),
			 primop2);
    }
  else
    {
      /* Here we must do the comparison on the nominal type
	 using the args exactly as we received them.  */
      type = *restype_ptr;
      primop1 = op1;
      primop2 = op2;
    }

  *op1_ptr = convert (type, primop1);
  *op2_ptr = convert (type, primop2);

  *restype_ptr = integer_type_node;

  return 0;
}

/* Construct and perhaps optimize a tree representation
   for a unary operation.  CODE, a tree_code, specifies the operation
   and XARG is the operand.  NOCONVERT nonzero suppresses
   the default promotions (such as from short to int).  */

tree
build_unary_op (code, xarg, noconvert)
     enum tree_code code;
     tree xarg;
     int noconvert;
{
  /* No default_conversion here.  It causes trouble for ADDR_EXPR.  */
  register tree arg = xarg;
  register tree argtype = 0;
  register enum tree_code typecode = TREE_CODE (TREE_TYPE (arg));
  char *errstring = NULL;
  int constant_flag = 0;
  int volatile_flag = TREE_VOLATILE (arg);

  switch (code)
    {
    case CONVERT_EXPR:
      /* This is used for unary plus, because a CONVERT_EXPR
	 is enough to prevent anybody from looking inside for
	 associativity, but won't generate any code.
	 Any argument is ok.  */
      break;

    case NEGATE_EXPR:
      if (!(typecode == INTEGER_TYPE || typecode == REAL_TYPE))
        errstring = "wrong type argument to unary minus";
      else if (!noconvert)
	arg = default_conversion (arg);
      break;

    case ABS_EXPR:
      if (!(typecode == INTEGER_TYPE || typecode == REAL_TYPE))
        errstring = "wrong type argument to abs";
      else if (!noconvert)
	arg = default_conversion (arg);
      break;

    case TRUTH_NOT_EXPR:
      if (typecode != INTEGER_TYPE && typecode != POINTER_TYPE)
        errstring = "wrong type argument to unary exclamation mark";
      arg = truthvalue_conversion (arg);
      if (TREE_CODE (arg) == NE_EXPR)
	{
	  TREE_SET_CODE (arg, EQ_EXPR);
	  return arg;
	}
      if (TREE_CODE (arg) == EQ_EXPR)
	{
	  TREE_SET_CODE (arg, NE_EXPR);
	  return arg;
	}
      if (TREE_CODE (arg) == TRUTH_NOT_EXPR)
	{
	  return TREE_OPERAND (arg, 0);
	}
      break;

    case NOP_EXPR:
      break;
      
    case PREINCREMENT_EXPR:
    case POSTINCREMENT_EXPR:
    case PREDECREMENT_EXPR:
    case POSTDECREMENT_EXPR:
      if (!lvalue_or_else (arg))
	return error_mark_node;
      volatile_flag = 1;
      if (!(typecode == INTEGER_TYPE || typecode == POINTER_TYPE))
	{
	  if (code == PREINCREMENT_EXPR || code == POSTINCREMENT_EXPR)
	    errstring ="wrong type argument to increment";
	  else
	    errstring ="wrong type argument to decrement";
	}
      else
	{
	  register tree inc = integer_one_node;
	  register tree result;
	  argtype = TREE_TYPE (arg);
	  if (typecode == POINTER_TYPE)
	    inc = size_in_bytes (TREE_TYPE (argtype));
	  result = build2 (code, arg, inc);
	  TREE_TYPE (result) = argtype;
	  return result;
	}
      break;

    case ADDR_EXPR:
      /* Let &* cancel out to simplify resulting code.  */
      if (TREE_CODE (arg) == INDIRECT_REF)
	return TREE_OPERAND (arg, 0);

      /* For &x[y], return x+y */
      if (TREE_CODE (arg) == ARRAY_REF)
	return build_binary_op (PLUS_EXPR, TREE_OPERAND (arg, 0),
				TREE_OPERAND (arg, 1));

      if (typecode != FUNCTION_TYPE && !lvalue_or_else (arg))
	return error_mark_node;

      if (typecode != ERROR_MARK)
	{
	  argtype = TREE_TYPE (arg);
	  if (TREE_READONLY (arg) || TREE_VOLATILE (arg))
	    argtype = build_type_variant (argtype,
					  TREE_READONLY (arg),
					  TREE_VOLATILE (arg));

	  argtype = build_pointer_type (argtype);
	  layout_type (argtype);

	  mark_addressable (arg);

	  /* Address of a static or external variable or
	     function counts as a constant */
	  if (staticp (arg))
	    constant_flag = 1;
	}
      else errstring = "dummy";  /* This won't be printed,
				    but being non-NULL is a flag */
      if (TREE_CODE (arg) == COMPONENT_REF)
	{
	  tree field = TREE_OPERAND (arg, 1);
	  tree addr = build_unary_op (ADDR_EXPR, TREE_OPERAND (arg, 0), 0);

	  if (TREE_PACKED (field))
	    {
	      yyerror ("Attempt to take address of bit-field structure member %s",
		       IDENTIFIER_POINTER (DECL_NAME (field)));
	      return error_mark_node;
	    }

	  if (DECL_OFFSET (field) != 0)
	    {
	      addr = build2 (PLUS_EXPR, addr,
			     build_int_2 ((DECL_OFFSET (field)
					   / BITS_PER_UNIT),
					  0));
	      TREE_TYPE (addr) = argtype;
	      addr = fold (addr);
	    }
	  else
	    addr = convert (argtype, addr);
	  TREE_LITERAL (addr) = constant_flag;
	  TREE_VOLATILE (addr) = volatile_flag;
	  return addr;
	}
    }

  if (!errstring)
    {
      register tree result = build1 (code, arg);
      TREE_TYPE (result) = argtype ? argtype : TREE_TYPE (arg);
      TREE_LITERAL (result) = constant_flag;
      TREE_VOLATILE (result) = volatile_flag;
      return fold (result);
    }

  if (typecode != ERROR_MARK)
    yyerror (errstring);
  return error_mark_node;
}

/* Mark EXP saying that we need to be able to take the
   address of it; it should not be allocated in a register.  */

mark_addressable (exp)
     tree exp;
{
  register tree x = exp;
  while (1)
    switch (TREE_CODE (x))
      {
      case COMPONENT_REF:
      case ARRAY_REF:
	x = TREE_OPERAND (x, 0);
	break;

      case VAR_DECL:
      case CONST_DECL:
      case PARM_DECL:
      case RESULT_DECL:
	if (TREE_REGDECL (x) && !TREE_ADDRESSABLE (x))
	  warning ("address requested for `%s', which is declared `register'",
		   IDENTIFIER_POINTER (DECL_NAME (x)));
	TREE_ADDRESSABLE (x) = 1;

      default:
	return;
    }
}

/* Build and return a conditional expression IFEXP ? OP1 : OP2.  */

tree
build_conditional_expr (ifexp, op1, op2)
     tree ifexp, op1, op2;
{
  register tree type1;
  register tree type2;
  register enum tree_code code1;
  register enum tree_code code2;
  register tree result_type = NULL;

  ifexp = default_conversion (ifexp);
  if (TREE_CODE (TREE_TYPE (op1)) != VOID_TYPE)
    op1 = default_conversion (op1);
  if (TREE_CODE (TREE_TYPE (op2)) != VOID_TYPE)
    op2 = default_conversion (op2);

  type1 = TREE_TYPE (op1);
  code1 = TREE_CODE (type1);
  type2 = TREE_TYPE (op2);
  code2 = TREE_CODE (type2);

  if (TREE_CODE (ifexp) == ERROR_MARK)
    return error_mark_node;
      
  if (type1 == type2)
    result_type = type1;
  else if (code1 == VOID_TYPE || code2 == VOID_TYPE)
    result_type = void_type_node;
  else if ((code1 == INTEGER_TYPE || code1 == REAL_TYPE)
           && (code2 == INTEGER_TYPE || code2 == REAL_TYPE))
    {
      result_type = commontype (op1, op2);

      op1 = convert (result_type, op1);
      op2 = convert (result_type, op2);
    }
  else if (code1 == POINTER_TYPE && code2 == POINTER_TYPE)
    {
      if (comptypes (type1, type2))
	result_type = TREE_TYPE (op1);
      else if (TREE_TYPE (TREE_TYPE (op1)) == void_type_node)
	result_type = TREE_TYPE (op1);
      else if (TREE_TYPE (TREE_TYPE (op2)) == void_type_node)
	result_type = TREE_TYPE (op2);
      else
	{
	  warning ("pointer type mismatch in conditional expression");
	  result_type = build_pointer_type (void_type_node);
	}
    }
  else if (code1 == POINTER_TYPE && TREE_CODE (op2) == INTEGER_CST)
    {
      if (!integer_zerop (op2))
	warning ("pointer/integer type mismatch in conditional expression");
      result_type = TREE_TYPE (op1);
      op2 = null_pointer_node;
    }
  else if (code2 == POINTER_TYPE && TREE_CODE (op1) == INTEGER_CST)
    {
      if (!integer_zerop (op1))
	warning ("pointer/integer type mismatch in conditional expression");
      result_type = TREE_TYPE (op2);
      op1 = null_pointer_node;
    }
    
  else if ((code1 == RECORD_TYPE || code1 == UNION_TYPE)
	   && TREE_TYPE (op1) == TREE_TYPE (op2))
    {
      result_type = TREE_TYPE (op1);
      if (TREE_LITERAL (ifexp))
	return (integer_zerop (ifexp) ? op2 : op1);

      if (TYPE_MODE (result_type) == BLKmode)
	{
	  register tree tempvar
	    = build_decl (VAR_DECL, NULL_TREE, result_type, 0, 0);
	  register tree xop1 = build_modify_expr (tempvar, op1);
	  register tree xop2 = build_modify_expr (tempvar, op2);
	  register tree result = build3 (COND_EXPR, ifexp, xop1, xop2);

	  pushdecl (tempvar);

	  TREE_TYPE (result) = result_type;
	  result = build2 (COMPOUND_EXPR, result, tempvar);
	  TREE_TYPE (result) = result_type;
	  TREE_VOLATILE (result)
	    = TREE_VOLATILE (ifexp) | TREE_VOLATILE (op1)
	      | TREE_VOLATILE (op2);
	  return result;
	}
    }

  if (!result_type)
    {
      yyerror ("type mismatch in conditional expression");
      return error_mark_node;
    }

  if (TREE_LITERAL (ifexp))
    return (integer_zerop (ifexp) ? op2 : op1);

  {
    register tree result = build3 (COND_EXPR, ifexp, op1, op2);
    TREE_TYPE (result) = result_type;
    TREE_VOLATILE (result)
      = TREE_VOLATILE (ifexp) | TREE_VOLATILE (op1)
	| TREE_VOLATILE (op2);
    return result;
  }
}

/* Given a list of expressions, return a compound expression
   that performs them all and returns the value of the last of them.  */

tree
build_compound_expr (list)
     tree list;
{
  register tree result, rest;

  if (TREE_CHAIN (list) == 0)
    return TREE_VALUE (list);

  rest = build_compound_expr (TREE_CHAIN (list));

  if (TREE_LITERAL (TREE_VALUE (list)))
    return rest;

  result = build2 (COMPOUND_EXPR, TREE_VALUE (list), rest);
  TREE_TYPE (result) = TREE_TYPE (rest);
  TREE_VOLATILE (result)
    = TREE_VOLATILE (TREE_VALUE (list)) | TREE_VOLATILE (rest);
  return result;
}

/* Build an expression representing a cast to type TYPE of expression EXPR.  */

tree
build_c_cast (type, expr)
     register tree type;
     tree expr;
{
  int constp = TREE_READONLY (type), volatilep = TREE_VOLATILE (type);
  register tree value;
  
  if (type == error_mark_node)
    return error_mark_node;

  value = convert (TYPE_MAIN_VARIANT (type), default_conversion (expr));
  /* As far as I know, it is not meaningful to cast something
     to a const or volatile type, because those are meaningful
     only for lvalues.
     But if it is meaningful, we must somehow return something
     whose TREE_READONLY or TREE_VOLATILE is set.
     That is not trivial because it is possible that VALUE == EXPR
     or is a shared constant.  */
  return value;
}

/* Build an assignment expression of lvalue LHS from value RHS.  */

tree
build_modify_expr (lhs, rhs)
     tree lhs, rhs;
{
  register tree result;
  tree newrhs = rhs;
  tree lhstype = TREE_TYPE (lhs);
  tree olhstype = lhstype;

  if (!lvalue_or_else (lhs)
      || TREE_CODE (lhs) == ERROR_MARK || TREE_CODE (rhs) == ERROR_MARK)
    return error_mark_node;

  /* Warn about storing in something that is `const'.  */

  if (TREE_READONLY (lhs))
    {
      if (TREE_CODE (lhs) == COMPONENT_REF)
	warning ("assignment of read-only member %s",
		 IDENTIFIER_POINTER (DECL_NAME (TREE_OPERAND (lhs, 1))));
      else if (TREE_CODE (lhs) == VAR_DECL)
	warning ("assignment of read-only variable %s",
		 IDENTIFIER_POINTER (DECL_NAME (lhs)));
      else
	warning ("assignment of read-only location");
    }

  /* If storing into a structure or union member,
     it has probably been given type `int'.
     Compute the type that would go with
     the actual amount of storage the member occupies.  */

  if (TREE_CODE (lhs) == COMPONENT_REF
      && (TREE_CODE (lhstype) == INTEGER_TYPE
	  || TREE_CODE (lhstype) == REAL_TYPE
	  || TREE_CODE (lhstype) == ENUMERAL_TYPE))
    lhstype = TREE_TYPE (get_unwidened (lhs, 0));

  newrhs = convert_for_assignment (lhstype, rhs, "assignment");

  /* If storing in a field that is in actuality a short or narrower than one,
     it would be silly if the rhs is actually a short that is widened to int.
     Remove any such widenings from the rhs.  */

  if (lhstype != TREE_TYPE (lhs))
    {
      newrhs = get_unwidened (newrhs, TREE_TYPE (lhs));
      TREE_TYPE (lhs) = TREE_TYPE (newrhs);
    }

  result = build2 (MODIFY_EXPR, lhs, newrhs);
  TREE_TYPE (result) = lhstype;
  TREE_VOLATILE (result) = 1;

  /* If we got the LHS in a different type for storing in,
     convert the result back to the nominal type of LHS
     so that the value we return always has the same type
     as the LHS argument.  */

  if (olhstype == TREE_TYPE (result))
    return result;
  return convert_for_assignment (olhstype, result, "assignment");
}

/* Convert value RHS to type TYPE as preparation for an assignment
   to an lvalue of type TYPE.
   The real work of conversion is done by `convert'.
   The purpose of this function is to generate error messages
   for assignments that are not allowed in C.
   ERRTYPE is a string to use in error messages:
   "assignment", "return", etc.  */

static tree
convert_for_assignment (type, rhs, errtype)
     tree type, rhs;
     char *errtype;
{
  register enum tree_code codel = TREE_CODE (type);
  register tree rhstype = datatype (rhs);
  register enum tree_code coder = TREE_CODE (rhstype);

  if (coder == ERROR_MARK)
    return rhs;
  if (coder == FUNCTION_TYPE)
    {
      if (TREE_CODE (rhs) == FUNCTION_DECL)
	return build_unary_op (ADDR_EXPR, rhs, 0);
      yyerror ("function type invalid in %s", errtype);
      return error_mark_node;
    }

  if (TREE_CODE (TREE_TYPE (rhs)) == ARRAY_TYPE)
    rhs = default_conversion (rhs);

  if (type == rhstype)
    return rhs;

  if (coder == VOID_TYPE)
    {
      yyerror ("void value not ignored as it ought to be");
      return error_mark_node;
    }
  if ((codel == INTEGER_TYPE || codel == REAL_TYPE)
       && (coder == INTEGER_TYPE || coder == REAL_TYPE))
    {
      return convert (type, rhs);
    }
  /* C says there is no difference between an enum type and int.
     Make it look that way.  */
  else if ((codel == INTEGER_TYPE || codel == ENUMERAL_TYPE)
       && (coder == INTEGER_TYPE || coder == ENUMERAL_TYPE))
    {
      return convert (type, rhs);
    }
  /* Conversions among pointers */
  else if (codel == POINTER_TYPE && coder == POINTER_TYPE)
    {
      register tree ttl = TREE_TYPE (type);
      register tree ttr = TREE_TYPE (rhstype);
      /* Anything converts to void *.  void * converts to anything.
	 Otherwise, the targets must be the same except that the
	 lhs target may be const or volatile while the rhs target isn't.  */
      if (!((ttl == void_type_node
	     && TREE_CODE (TREE_TYPE (ttr)) != FUNCTION_TYPE)
	    || (ttr == void_type_node
		&& TREE_CODE (TREE_TYPE (ttl)) != FUNCTION_TYPE)
	    || (comp_target_types (ttr, ttl)
		&& (TREE_READONLY (ttl) || ! TREE_READONLY (ttr))
		&& (TREE_VOLATILE (ttl) || ! TREE_VOLATILE (ttr)))))
	warning ("%s between incompatible pointer types", errtype);
      return convert (type, rhs);
    }
  else if (codel == POINTER_TYPE && coder == INTEGER_TYPE)
    {
      if (! integer_zerop (rhs))
	{
	  warning ("%s of pointer from integer lacks a cast", errtype);
	  return convert (type, rhs);
	}
      return null_pointer_node;
    }

  yyerror ("invalid types in %s", errtype);
  return error_mark_node;
}

/* Perform appropriate conversions on the initial value of a variable,
   and record it.  */

void
store_init_value (decl, init)
     tree decl, init;
{
  /* apply allowed conversions to the initial value, then record it.  */
  register tree value, type;
  register tree field;
  register enum tree_code code;

  if (TREE_CODE (init) == ERROR_MARK)
    return;

  type = TREE_TYPE (decl);

  while (1)
    {
      /* Even a structure or union, if automatic,
	 can be initialized from an expression of the same type.  */
      if (TREE_TYPE (init) == type
	  && ! TREE_STATIC (decl))
	{
	  DECL_INITIAL (decl) = init;
	  return;
	}

      /* If not a union and INIT is not the same type,
	 must do it the hard way.  */
      if (TREE_CODE (type) != UNION_TYPE)
	break;

      /* It is a union, and INIT is not the same type.
	 Initialize via the first member of the union.  */
      field = TYPE_FIELDS (type);
      if (field == 0)
	{
	  yyerror ("union with no members cannot be initialized");
	  return;
	}
      type = TREE_TYPE (field);
    }

  code = TREE_CODE (type);
  if (code == INTEGER_TYPE || code == REAL_TYPE || code == POINTER_TYPE
      || code == ENUMERAL_TYPE)
    {
      if (TREE_CODE (init) == CONSTRUCTOR)
	{
	  init = TREE_OPERAND (init, 0);
	  if (init == 0)
	    {
	      yyerror ("initializer for scalar variable is empty braces");
	      return;
	    }
	  if (TREE_CHAIN (init))
	    {
	      yyerror ("initializer for scalar variable has multiple elements");
	      return;
	    }
	  init = TREE_VALUE (init);
	  if (TREE_CODE (init) == CONSTRUCTOR)
	    {
	      yyerror ("initializer for scalar variable has nested braces");
	      return;
	    }
	}

      value = convert_for_assignment (type, init, "initialization");
      if (TREE_STATIC (decl) && ! TREE_LITERAL (value))
	yyerror ("initializer for static variable is not constant");
      else
	DECL_INITIAL (decl) = value;
      return;
    }

  /* Initializers for arrays and records only.  */
  value = process_init_constructor (type, &init);

  if (TREE_LITERAL (value))
    DECL_INITIAL (decl) = value;
  else
    yyerror ("components of aggregate initializer not constant");
}


/* Process a constructor for a variable of type type.
 elts is a list of constructor-elements to take values from,
 one for each ultimate scalar component.  Elements can also
 be constructors; a constructor applies to one component only.  */

tree
process_init_constructor (type, elts)
     tree type, *elts;
{
  tree tail = *elts;
  register tree next = tail;
  register enum tree_code code = TREE_CODE (type);
  int must_use_all = 0;
  register tree result = NULL;
  register int allconstant = 1;

  if (tail && TREE_CODE (tail) == TREE_LIST)
    next = TREE_VALUE (tail);

  /* Handle the case where the object has a single expression
     as its initial value.  */
  if (code == INTEGER_TYPE || code == REAL_TYPE || code == POINTER_TYPE
      || code == ENUMERAL_TYPE)
    {
      /* A single expression for the initial value.  */

      if (!next) return 0;

      *elts = TREE_CHAIN (tail);
      return convert_for_assignment (type, next, "initialization");
    }

  /* Initialization of an array of chars from a string constant.  */
  if (code == ARRAY_TYPE
      && TREE_TYPE (type) == char_type_node
      && next && TREE_CODE (next) == ADDR_EXPR
      && TREE_CODE (TREE_OPERAND (next, 0)) == STRING_CST
      )
    {
      register tree cst = TREE_OPERAND (next, 0);
      *elts = TREE_CHAIN (tail);
      TREE_TYPE (cst) = type;  /* make the constant be an array, so we get array copying */
      if (TYPE_DOMAIN (type) != 0
	  && TREE_LITERAL (TYPE_SIZE (type)))
	{
	  register int size
	    = TREE_INT_CST_LOW (TYPE_SIZE (type)) * TYPE_SIZE_UNIT (type);
	  size = (size + BITS_PER_UNIT - 1) / BITS_PER_UNIT;
	  /* Subtract 1 because it's ok to ignore the terminating null char
	     that is counted in the length of the constant.  */
	  if (size < TREE_STRING_LENGTH (cst) - 1)
	    warning ("initializer-string for array of chars is too long");
	}
      return cst;
    }

  /* if there is a separate level of braces for this aggregate,
     at the level of *elts that is just one element and we only use that one;
     but really we use the elements of the braced construct, and must use them all.  */

  if (next && TREE_CODE (next) == CONSTRUCTOR)
    {
      must_use_all = 1;
      *elts = TREE_CHAIN (tail);
      tail = TREE_OPERAND (next, 0);
    }

  /* now gobble elements, as many as needed, and make a constructor or initial value
     for each element of this aggregate.  Chain them together in result.
     If there are too few, use 0 for each scalar ultimate component.  */

  if (code == ARRAY_TYPE)
    {
      register tree domain = TYPE_DOMAIN (type);
      register long len;
      register int i;

      if (domain)
	len = TREE_INT_CST_LOW (TYPE_MAX_VALUE (domain))
	  - TREE_INT_CST_LOW (TYPE_MIN_VALUE (domain))
	    + 1;
      else
	len = -1;  /* Take as many as there are */

      for (i = 0; (len < 0 || i < len) && tail != 0; i++)
	{
	  register tree next1 =
	    process_init_constructor (TREE_TYPE (type), &tail);
	  result = chainon (result, build_tree_list (NULL_TREE, next1));
	  if (!TREE_LITERAL (next1))
	    allconstant = 0;
	}
    }
  if (code == RECORD_TYPE)
    {
      register tree field;

      for (field = TYPE_FIELDS (type); field && tail;
	   field = TREE_CHAIN (field))
	{
	  register tree next1
	    = process_init_constructor (TREE_TYPE (field), &tail);
	  result = chainon (result, build_tree_list (field, next1));
	  if (!TREE_LITERAL (next1))
	    allconstant = 0;
	}
    }

  if (must_use_all)
    {
      if (tail) warning ("excess elements in aggregate initializer");
    }
  else *elts = tail;

  result = build1 (CONSTRUCTOR, result);
  TREE_TYPE (result) = type;
  if (allconstant) TREE_LITERAL (result) = 1;
  return result;
}

/* Build and return a `return' statement.
   RETVAL is the expression for what to return,
   or a null pointer for `return;' with no value.

   The RETURN_STMT node that we construct contains an assignment
   (a MODIFY_EXPR) whose lhs is a RESULT_DECL node
   that represents the value to be returned by this function.  */

tree
build_return_stmt (filename, line, retval)
     char *filename;
     int line;
     tree retval;
{
  tree t;
  tree valtype = TREE_TYPE (TREE_TYPE (current_function_decl));

  if (!retval)
    t = NULL;
  else if (valtype == 0 || TREE_CODE (valtype) == VOID_TYPE)
    {
      warning ("value given in return statement in function returning void");
      t = NULL;
    }
  else
    {
      t = convert_for_assignment (valtype, retval, "return");
      current_function_returns_value = 1;
      t = build2 (MODIFY_EXPR, DECL_RESULT (current_function_decl), t);
      TREE_TYPE (t) = valtype;
    }

  return build_return (filename, line, t);
}

/* Build a CASE_STMT node to represent a C switch statement.
   This is done as soon as the `switch (ARG)' is seen,
   before reading the body of the switch.  Therefore, the CASE_STMT
   node that is built here has an empty list of cases.
   FILE and LINE identify the statement's position in the source file;
   TESTVAL is the expression that controls the switch.

   After the empty CASE_STMT is returned, it will be remembered as the
   value of `current_switch_stmt'.  As cases are read, `pushcase' will
   be used to add them to the CASE_STMT node.
   At the end of the body of the switch statement,
   finish_switch_stmt is called to digest the cases that were found.

   Note that a CASE_STMT is not really like a C switch statement;
   it is more like a computed goto.  It contains a control-expression
   and a list of value-label pairs; it compares the value of the
   control expression with each value in the list, and jumps to the
   corresponding label.  A C switch statement is represented as
   a compound statement containing two statements: a CASE_STMT
   and the switch body.  The labels in the CASE_STMT's list
   are all within the switch body; but nothing in the tree structure
   requires that to be true.  */

tree
build_switch_stmt (filename, line, testval)
     char *filename;
     int line;
     tree testval;
{
  register tree xtestval = testval;
  register enum tree_code code = TREE_CODE (TREE_TYPE (xtestval));

  if (code != INTEGER_TYPE && code != ENUMERAL_TYPE && code != ERROR_MARK)
    {
      yylineerror (line, "switch quantity not an integer");
      xtestval = error_mark_node;
    }

  return build_case (filename, line, xtestval, NULL_TREE);
}

/* Finish up a switch statement, after its body has been entirely read.
   STMT is the CASE_STMT node that represents the statement.
   DEFAULTLABEL is a LABEL_DECL node for after the body.
   If there was no `default:' label in the body,
   we pretend that there was one and that DEFAULTLABEL is its label.  */

void
finish_switch_stmt (stmt, defaultlabel)
     tree stmt;
     tree defaultlabel;
{
  register tree c;
  int have_default = 0;
  int error_printed = 0;

  for (c = STMT_CASE_LIST (stmt); c; c = TREE_CHAIN (c))
    {
      register tree tail;

      if (TREE_PURPOSE (c) == 0)
	{
	  if (have_default == 1)
	    yyerror ("duplicate default statements in switch statement");
	  have_default++;
	}
      else if (!error_printed)
	for (tail = TREE_CHAIN (c); tail; tail = TREE_CHAIN (tail))
	  {
	    if (TREE_PURPOSE (tail)
		&& (TREE_INT_CST_LOW (TREE_PURPOSE (c))
		    == TREE_INT_CST_LOW (TREE_PURPOSE (tail)))
		&& (TREE_INT_CST_HIGH (TREE_PURPOSE (c))
		    == TREE_INT_CST_HIGH (TREE_PURPOSE (tail))))
	      {
		yyerror ("duplicate case labels in switch statement");
		error_printed = 1;
		break;
	      }
	  }
    }

  if (have_default == 0)
    pushcase (0, defaultlabel);
}

/* Record a case or default label in a switch body.
   VALUE is the value of the case (a null pointer, for `default:').
   LABEL is a LABEL_DECL node which labels that position in the code.  */

void
pushcase (value, label)
     tree value, label;
{
  if (current_switch_stmt)
    STMT_CASE_LIST (current_switch_stmt) =
      tree_cons (value, STMT_BODY (label),
		 STMT_CASE_LIST (current_switch_stmt));

  else
    yyerror ("case tag not within a switch statement");
}
