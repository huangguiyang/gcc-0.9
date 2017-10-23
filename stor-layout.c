/* C-compiler utilities for types and variables storage layout
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
#include <stdio.h>

#include "tree.h"
#include "rtl.h"   /* For GET_MODE_SIZE */

#define MAX(x,y) ((x) > (y) ? (x) : (y))
#define MIN(x,y) ((x) < (y) ? (x) : (y))
#define CEIL(x,y) (((x) + (y) - 1) / (y))

/* Data type for the expressions representing sizes of data types.
   It is the first integer type laid out.
   In C, this is int.  */

tree sizetype;

#define GET_MODE_ALIGNMENT(MODE)   \
  MIN (BIGGEST_ALIGNMENT, 	   \
       MAX (1, (GET_MODE_UNIT_SIZE (MODE) * BITS_PER_UNIT)))

/* Chain of all permanent types we have allocated since last
   call to get_permanent_types.  */

tree permanent_type_chain;

/* Chain of all temporary types we have allocated in this function.  */

tree temporary_type_chain;

/* When the chains is not null, these point at the last
   types on the two chains.  */
tree permanent_type_end;
tree temporary_type_end;

/* Put the newly-made type T
   on either permanent_type_chain or temporary_type_chain.
   Types that are const or volatile variants of other types
   are not put on any chain, since in the gdb symbol segment
   we do not make those distinctions.

   If T is already on the chain, we do nothing.  */

void
chain_type (t)
     tree t;
{
  if (TYPE_MAIN_VARIANT (t) != t)
    return;
  if (TREE_CHAIN (t) != 0)
    return;
  if (TREE_PERMANENT (t))
    {
      if (t == permanent_type_end)
	return;
      if (permanent_type_chain == 0)
	permanent_type_end = t;
      TREE_CHAIN (t) = permanent_type_chain;
      permanent_type_chain = t;
    }
  else
    {
      if (t == temporary_type_end)
	return;
      if (temporary_type_chain == 0)
	temporary_type_end = t;
      TREE_CHAIN (t) = temporary_type_chain;
      temporary_type_chain = t;
    }
}

/* Get a chain of all permanent types made since this function
   was last called.  */

tree
get_permanent_types ()
{
  register tree tem = permanent_type_chain;
  permanent_type_chain = 0;
  permanent_type_end = 0;
  return tem;
}

/* Get a chain of all temporary types made since this function
   was last called.  */

tree
get_temporary_types ()
{
  register tree tem = temporary_type_chain;
  temporary_type_chain = 0;
  temporary_type_end = 0;
  return tem;
}

/* Return the greatest common divisor of M and N.  */

static
unsigned
gcd (m, n)
     register unsigned int m, n;
{
  register unsigned int r;

  while (1)
    {
      r = m % n;
      if (0 == r) break;
      m = n;
      n = r;
    }
  return n;
}

/* Return the machine mode to use for an aggregate of SIZE bits.

   Note!!!  We only use a non-BLKmode mode if the size matches exactly.
   There used to be the idea of using DImode for anything whose
   size was less than DImode but more than SImode.  This does not work
   because DImode moves cannot be used to store such objects in memory.  */

static
enum machine_mode
agg_mode (size)
     unsigned int size;
{
  register int units = size / BITS_PER_UNIT;
  register enum machine_mode t;

  if (size % BITS_PER_UNIT != 0)
    return BLKmode;

  for (t = QImode; (int) t <= (int) DImode;
       t = (enum machine_mode) ((int) t + 1))
    if (GET_MODE_SIZE (t) == units)
      return t;

  return BLKmode;
}

/* Return an INTEGER_CST with value V and type from `sizetype'.  */

static tree
build_int (v)
     int v;
{
  register tree t;

  t = build_int_2 (v, 0);
  TREE_TYPE (t) = sizetype;
  return t;
}

/* Combine operands OP1 and OP2 with arithmetic operation OPC.
   OPC is a tree code.  Data type is taken from `sizetype',
   If the operands are constant, so is the result.  */

static tree
genop (opc, op1, op2)
     enum tree_code opc;
     tree op1, op2;
{
  register tree t;

  if (TREE_LITERAL (op1) && TREE_LITERAL (op2))
    return combine (opc, op1, op2);

  t = build2 (opc, op1, op2);
  TREE_TYPE (t) = sizetype;
  t = fold (t);

  return t;
}

/* Convert a size which is SIZE when expressed in unit INUNITS
   into the units OUTUNITS.  Rounds up if conversion is not exact.
   If SIZE is constant, so is the result.  */

tree
convert_units (size, inunits, outunits)
   tree size;
   register int inunits, outunits;
{
  register tree t;

  if (inunits == outunits)
    return size;
  /* Check for inunits divisible by outunits.
     In that case, just multiply by their ratio.  */
  if (0 == (inunits % outunits))
    return genop (MULT_EXPR, size, build_int (inunits / outunits));
  /* The inverse case.  */
  if (0 == (outunits % inunits))
    return genop (CEIL_DIV_EXPR, size, build_int (outunits / inunits));
  /* The general case.  */
  t = genop (MULT_EXPR, size,
	     build_int (inunits)); /* convert to bits */
  return genop (CEIL_DIV_EXPR, t,
		build_int (outunits)); /* then to outunits */
}

/* Form a tree giving the value of
   CEIL ((CONST + VAR * VAR_UNIT) / BITS_PER_UNIT)
   where VAR is any expression and CONST and VAR_UNIT are integers.
   VAR may be null; that counts as zero.  */

static
tree
add_vc_sizes (constant, var, coeff)
     int constant;
     tree var;
     int coeff;
{
  register tree tmp1, tmp2;

  if (var == 0)
    return build_int (CEIL (constant, BITS_PER_UNIT));
  if (constant == 0)
    return convert_units (var, coeff, BITS_PER_UNIT);

  tmp1 = genop (PLUS_EXPR, genop (MULT_EXPR, var, integer_one_node),
		build_int (constant)); /* add */
  return genop (CEIL_DIV_EXPR, tmp1, build_int (BITS_PER_UNIT));
}

/* Set the size, mode and alignment of a ..._DECL node.
   Note that LABEL_DECL, TYPE_DECL and CONST_DECL nodes do not need this,
   and FUNCTION_DECL nodes have them set up in a special (and simple) way.
   Don't call layout_decl for them.

   KNOWN_ALIGN is the amount of alignment we can assume this
   decl has with no special effort.  It is relevant only for FIELD_DECLs
   and depends on the previous fields.
   All that matters about KNOWN_ALIGN is which powers of 2 divide it.
   If KNOWN_ALIGN is 0, it means, "as much alignment as you like":
   the record will be aligned to suit.  */

void
layout_decl (decl, known_align)
     tree decl;
     unsigned known_align;
{
  register tree type = TREE_TYPE (decl);
  register enum tree_code code = TREE_CODE (decl);
  int spec_size = DECL_SIZE_UNIT (decl);

  if (code != VAR_DECL && code != PARM_DECL && code != RESULT_DECL
      && code != FIELD_DECL)
    abort ();

  if (type == error_mark_node)
    type = void_type_node;
  if (TYPE_SIZE_UNIT (type) == 0)
    abort ();

  /* Usually the size and mode come from the data type without change.  */

  DECL_MODE (decl) = TYPE_MODE (type);
  DECL_SIZE (decl) = TYPE_SIZE (type);
  DECL_SIZE_UNIT (decl) = TYPE_SIZE_UNIT (type);

  /* Force alignment required for the data type.
     But if the decl itself wants greater alignment, don't override that.  */

  if (TYPE_ALIGN (type) > DECL_ALIGN (decl))
    DECL_ALIGN (decl) = TYPE_ALIGN (type);

  if (code == FIELD_DECL && spec_size != 0)
    {
      /* This is a bit-field.  We don't know how to handle
	 them except for integers and enums, and front end should
	 never generate them otherwise.  */

      if (! (TREE_CODE (type) == INTEGER_TYPE
	     || TREE_CODE (type) == ENUMERAL_TYPE))
	abort ();

      /* Mode is "integer bit field".  */
      DECL_MODE (decl) = BImode;
      /* Size is specified number of bits.  */
      DECL_SIZE (decl) = integer_one_node;
      DECL_SIZE_UNIT (decl) = spec_size;
      /* No alignment requirement.  */
      DECL_ALIGN (decl) = 1;

      TREE_UNSIGNED (decl) = type_unsigned_p (type);

      /* Promote the field's type to int or unsigned int
	 if it is narrower than that.  */
      if (TREE_CODE (type) == INTEGER_TYPE
	  && TYPE_PRECISION (type) < TYPE_PRECISION (integer_type_node))
	TREE_TYPE (decl) = type
	  = (TREE_UNSIGNED (decl) ? unsigned_type_node : integer_type_node);
    }

  /* See if we can use a scalar mode such as QImode or SImode
     in place of BLKmode or a packed byte mode.  */
  /* Conditions are: a fixed size that is correct for another mode
     and occupying a complete byte or bytes on proper boundary.  */
  if ((DECL_MODE (decl) == BLKmode
       || DECL_MODE (decl) == BImode)
      && TREE_CODE (TYPE_SIZE (type)) == INTEGER_CST)
    {
      register int packed_size 
	= TREE_INT_CST_LOW (DECL_SIZE (decl)) * DECL_SIZE_UNIT (decl);
      register enum machine_mode xmode = agg_mode (packed_size);

      if (xmode != BLKmode
	  && known_align % GET_MODE_ALIGNMENT (xmode) == 0)
	{
	  DECL_ALIGN (decl) = MAX (GET_MODE_ALIGNMENT (xmode),
				   DECL_ALIGN (decl));
	  DECL_MODE (decl) = xmode;
	  DECL_SIZE (decl) = build_int (GET_MODE_SIZE (xmode));
	  DECL_SIZE_UNIT (decl) = BITS_PER_UNIT;
	}
    }
}

/* Lay out a RECORD_TYPE type (a C struct).
   This means laying out the fields, determining their offsets,
   and computing the overall size and required alignment of the record.  */

static tree
layout_record (rec)
     tree rec;
{
  register tree field;
  int record_align = BITS_PER_UNIT;
  /* Record size so far is CONST_SIZE + VAR_SIZE * SIZE_UNIT bits,
     where CONST_SIZE is an integer
     and VAR_SIZE is a tree expression.
     If VAR_SIZE is null, the size is just CONST_SIZE.
     Naturally we try to avoid using VAR_SIZE.  */
  register int const_size = 0;
  register tree var_size = 0;
  register int size_unit = 8;

  for (field = TYPE_FIELDS (rec); field; field = TREE_CHAIN (field))
    {
      register int desired_align;

      /* Lay out the field so we know what alignment it needs.
	 For KNOWN_ALIGN, pass the number of bits from start of record
	 or some divisor of it.  */
	 
      layout_decl (field, var_size ? size_unit : const_size);
      desired_align = DECL_ALIGN (field);

      /* Record must have at least as much alignment as any field.
	 Otherwise, the alignment of the field within the record
	 is meaningless.  */

      record_align = MAX (record_align, desired_align);

      /* Does this field automatically have alignment it needs
	 by virtue of the fields that precede it and the record's
	 own alignment?  */

      if (const_size % desired_align != 0
	  || (size_unit % desired_align != 0
	      && var_size))
	{
	  /* No, we need to skip space before this field.
	     Bump the cumulative size to multiple of field alignment.  */

	  if (var_size == 0
	      || size_unit % desired_align == 0)
	    const_size
	      = CEIL (const_size, desired_align) * desired_align;
	  else
	    {
	      var_size
		= genop (PLUS_EXPR, var_size,
			 build_int (CEIL (const_size, size_unit)));
	      const_size = 0;
	      var_size = convert_units (var_size, size_unit, desired_align);
	      size_unit = desired_align;
	    }
	}

      /* Size so far becomes the offset of this field.  */

      DECL_OFFSET (field) = const_size;
      DECL_VOFFSET (field) = var_size;
      DECL_VOFFSET_UNIT (field) = size_unit;

      /* Now add size of this field to the size of the record.  */

      {
        register tree dsize = DECL_SIZE (field);

	if (TREE_LITERAL (dsize))
	  const_size += TREE_INT_CST_LOW (dsize) * DECL_SIZE_UNIT (field);
	else if (var_size == 0)
	  {
	    var_size = dsize;
	    size_unit = DECL_SIZE_UNIT (field);
	  }
	else
	  {
	    register int tunits = MIN (size_unit, DECL_SIZE_UNIT (field));
	    var_size
	      = genop (PLUS_EXPR,
		       convert_units (var_size, size_unit, tunits),
		       convert_units (dsize, DECL_SIZE_UNIT (field), tunits));
	  }
      }
    }

  /* Work out the total size and alignment of the record
     as one expression and store in the record type.
     Round it up to a multiple of the record's alignment.  */

  if (var_size == 0)
    TYPE_SIZE (rec)
      = build_int (CEIL (CEIL (const_size, record_align) * record_align,
			 size_unit));
  else
    {
      if (const_size)
	var_size
	  = genop (PLUS_EXPR, var_size,
		   build_int (CEIL (const_size, size_unit)));
      TYPE_SIZE (rec)
	= convert_units (var_size,
			 size_unit,
			 record_align);
      size_unit = record_align;
    }

  TYPE_SIZE (rec) = convert_units (TYPE_SIZE (rec), size_unit,
				   BITS_PER_UNIT);
  TYPE_SIZE_UNIT (rec) = BITS_PER_UNIT;
  TYPE_ALIGN (rec) = MIN (BIGGEST_ALIGNMENT, record_align);
}


/* Lay out a UNION_TYPE type.
   Lay out all the fields, set their offsets to zero,
   and compute the size and alignment of the union (maximum of any field).  */

static tree
layout_union (rec)
     tree rec;
{
  register tree field;
  int union_align = BITS_PER_UNIT;

  /* The size of the union, based on the fields scanned so far,
     is max (CONST_SIZE, VAR_SIZE).
     VAR_SIZE may be null; then CONST_SIZE by itself is the size.  */
  register int const_size = 0;
  register tree var_size = 0;

  for (field = TYPE_FIELDS (rec); field; field = TREE_CHAIN (field))
    {
      layout_decl (field, 0);
      DECL_OFFSET (field) = 0;
      DECL_VOFFSET (field) = 0;
      DECL_VOFFSET_UNIT (field) = BITS_PER_UNIT;

      /* Union must be at least as aligned as any field requires.  */

      union_align = MAX (union_align, DECL_ALIGN (field));

      /* Set union_size to max (decl_size, union_size).
	 There are more and less general ways to do this.
	 Use only CONST_SIZE unless forced to use VAR_SIZE.  */

      if (TREE_LITERAL (DECL_SIZE (field)))
	const_size = MAX (const_size,
			  TREE_INT_CST_LOW (DECL_SIZE (field))
			  * DECL_SIZE_UNIT (field));
      else if (var_size == 0)
	var_size = convert_units (DECL_SIZE (field),
				  DECL_SIZE_UNIT (field),
				  BITS_PER_UNIT);
      else
	var_size = genop (MAX_EXPR,
			  convert_units (DECL_SIZE (field),
					 DECL_SIZE_UNIT (field),
					 BITS_PER_UNIT),
			  var_size);
    }

  /* Determine the ultimate size of the union.  */
  if (NULL == var_size)
    TYPE_SIZE (rec) = build_int (CEIL (const_size, BITS_PER_UNIT));
  else if (const_size == 0)
    TYPE_SIZE (rec) = var_size;
  else
    TYPE_SIZE (rec) = genop (MAX_EXPR, var_size,
			     build_int (CEIL (const_size, BITS_PER_UNIT)));

  TYPE_SIZE_UNIT (rec) = BITS_PER_UNIT;
  TYPE_ALIGN (rec) = MIN (BIGGEST_ALIGNMENT, union_align);
}

/* Calculate the mode, size, and alignment for TYPE.
   For an array type, calculate the element separation as well.
   Record TYPE on the chain of permanent or temporary types
   so that dbxout will find out about it.  */

void
layout_type (type)
     tree type;
{
  if (type == 0)
    abort ();

  /* Do nothing if type has been laid out before.  */
  if (TYPE_SIZE (type))
    return;

  chain_type (type);

  switch (TREE_CODE (type))
    {
    case VOID_TYPE:
      TYPE_SIZE (type) = integer_zero_node;
      TYPE_SIZE_UNIT (type) = BITS_PER_UNIT;
      TYPE_ALIGN (type) = 1;
      TYPE_MODE (type) = VOIDmode;
      break;

    case INTEGER_TYPE:
    case ENUMERAL_TYPE:
      TYPE_MODE (type) = agg_mode (TYPE_PRECISION (type));
      TYPE_SIZE (type) = build_int (GET_MODE_SIZE (TYPE_MODE (type)));
      TYPE_SIZE_UNIT (type) = BITS_PER_UNIT;
      TYPE_ALIGN (type) = GET_MODE_ALIGNMENT (TYPE_MODE (type));
      if (TREE_INT_CST_HIGH (TYPE_MIN_VALUE (type)) >= 0)
	TREE_UNSIGNED (type) = 1;
      break;

    case REAL_TYPE:
      {
	register int prec = TYPE_PRECISION (type);
	if (prec <= GET_MODE_BITSIZE (SFmode))
	  TYPE_MODE (type) = SFmode;
	else if (prec <= GET_MODE_BITSIZE (DFmode))
	  TYPE_MODE (type) = DFmode;
	else
	  abort ();
      }
      TYPE_SIZE (type) = build_int (GET_MODE_SIZE (TYPE_MODE (type)));
      TYPE_SIZE_UNIT (type) = BITS_PER_UNIT;
      TYPE_ALIGN (type) = GET_MODE_ALIGNMENT (TYPE_MODE (type));
      break;

    case POINTER_TYPE:
      TYPE_MODE (type) = Pmode;
      TYPE_SIZE (type) = build_int (POINTER_SIZE / BITS_PER_UNIT);
      TYPE_SIZE_UNIT (type) = BITS_PER_UNIT;
      TYPE_ALIGN (type) = POINTER_BOUNDARY;
      break;

    case ARRAY_TYPE:
      {
	register tree index = TYPE_DOMAIN (type);
	register tree length;
	register tree element = TREE_TYPE (type);

	if (index == 0)
	  length = integer_zero_node;
	else
	  length = genop (PLUS_EXPR, integer_one_node,
			  genop (MINUS_EXPR, TYPE_MAX_VALUE (index),
				 TYPE_MIN_VALUE (index)));

	if (TREE_PACKED (type))
	  abort ();  /* ??? Not written yet since not needed for C.  */

	TYPE_SIZE_UNIT (type) = TYPE_SIZE_UNIT (element);
	TYPE_SIZE (type) = genop (MULT_EXPR, TYPE_SIZE (element), length);
	TYPE_SEP (type) = TYPE_SIZE (element);
	TYPE_SEP_UNIT (type) = TYPE_SIZE_UNIT (element);
	TYPE_ALIGN (type) = MAX (TYPE_ALIGN (element), BITS_PER_UNIT);
	TYPE_MODE (type) = BLKmode;
	if (TREE_CODE (TYPE_SIZE (type)) == INTEGER_CST)
	  {
	    TYPE_MODE (type) 
	      = agg_mode (TREE_INT_CST_LOW (TYPE_SIZE (type))
			  * TYPE_SIZE_UNIT (type));
	  }
	break;
      }

    case RECORD_TYPE:
      layout_record (type);
      TYPE_MODE (type) = BLKmode;
      if (TREE_CODE (TYPE_SIZE (type)) == INTEGER_CST)
	{
	  TYPE_MODE (type) 
	    = agg_mode (TREE_INT_CST_LOW (TYPE_SIZE (type))
			* TYPE_SIZE_UNIT (type));
	}
      break;

    case UNION_TYPE:
      layout_union (type);
      TYPE_MODE (type) = BLKmode;
      if (TREE_CODE (TYPE_SIZE (type)) == INTEGER_CST)
	{
	  TYPE_MODE (type) 
	    = agg_mode (TREE_INT_CST_LOW (TYPE_SIZE (type))
			* TYPE_SIZE_UNIT (type));
	}
      break;

    case FUNCTION_TYPE:
      TYPE_MODE (type) = EPmode;
      TYPE_SIZE (type) = build_int (2 * POINTER_SIZE / BITS_PER_UNIT);
      TYPE_SIZE_UNIT (type) = BITS_PER_UNIT;
      TYPE_ALIGN (type) = POINTER_BOUNDARY;
      break;

    default:
      abort ();
    } /* end switch */
}

/* Set the offsets of the parameters of a FUNCTION_DECL node.
   Assumes layout_decl has been done already on the PARM_DECLs themselves.  */

void
layout_parms (fndecl)
     tree fndecl;
{
  register tree parm;
  /* Constant term in the offset so far, measured in bits.  */
  register int const_offset = FIRST_PARM_OFFSET * BITS_PER_UNIT;
  /* Variable term in the offset so far; null if offset so far is constant.  */
  register tree var_offset = 0;
  /* Unit of measurement for VAR_OFFSET.  */
  register int offset_unit = 8;

  for (parm = DECL_ARGUMENTS (fndecl); parm; parm = TREE_CHAIN (parm))
    {
      /* The offset of the next parm is, in general,
	 CONST_OFFSET + VAR_OFFSET * OFFSET_UNITS.
	 If VAR_OFFSET is null, then CONST_OFFSET alone says everything.  */

      register int desired_align
	= MAX (PARM_BOUNDARY, TYPE_ALIGN (DECL_ARG_TYPE (parm)));

      if (const_offset % desired_align != 0
	  || (offset_unit % desired_align != 0
	      && var_offset))
	{
	  /* We need some padding to align this parm.
	     Round up the variable and constant parts of the offset
	     separately or together, whichever is best, to a multiple
	     of DESIRED_ALIGN.  */
	  if (var_offset == 0
	      || offset_unit % desired_align == 0)
	    const_offset
	      = CEIL (const_offset, desired_align) * desired_align;
	  else
	    {
	      var_offset
		= genop (PLUS_EXPR, var_offset,
			 build_int (CEIL (const_offset, offset_unit)));
	      const_offset = 0;
	      var_offset = convert_units (var_offset, offset_unit,
					  desired_align);
	      offset_unit = desired_align;
	    }
	}

      /* Set the parm's offset, constant and variable parts,
	 according to where we have reached.  */

      DECL_OFFSET (parm) = const_offset;
      DECL_VOFFSET (parm) = var_offset;
      DECL_VOFFSET_UNIT (parm) = offset_unit;

      /* Now add size of this parm to the offset-so-far.  */

      {
        register tree dsize = TYPE_SIZE (DECL_ARG_TYPE (parm));
	register int sizeunit = TYPE_SIZE_UNIT (DECL_ARG_TYPE (parm));

	/* Parm size is constant => add to constant part of offset.  */
	if (TREE_LITERAL (dsize))
	  const_offset += TREE_INT_CST_LOW (dsize) * sizeunit;
	else if (var_offset == 0)
	  {
	    /* Offset-so-far is constant but parm size is variable
	       => parm size becomes variable part of offset-so-far.  */
	    var_offset = dsize;
	    offset_unit = sizeunit;
	  }
	else
	  {
	    /* Add parm size into offset so far
	       after converting them to common units.  */
	    register int tunits = MIN (offset_unit, sizeunit);
	    var_offset
	      = genop (PLUS_EXPR,
		       convert_units (var_offset, offset_unit, tunits),
		       convert_units (dsize, sizeunit, tunits));
	  }
      }
    }
}

/* Create and return a type for signed integers of PRECISION bits.  */
 
tree
make_signed_type (precision)
     int precision;
{
  register tree type = make_node (INTEGER_TYPE);
  register tree low, high;

  TYPE_PRECISION (type) = precision;

  /* Create the extreme values based on the number of bits.  */

  TYPE_MIN_VALUE (type)
    = build_int_2 ((precision-BITS_PER_WORD > 0 ? 0 : (-1)<<(precision-1)),
		   (-1)<<(precision-BITS_PER_WORD-1 > 0
			  ? precision-BITS_PER_WORD-1
			  : 0));
  TYPE_MAX_VALUE (type)
    = build_int_2 ((precision-BITS_PER_WORD > 0 ? -1 : (1<<(precision-1))-1),
		   (precision-BITS_PER_WORD-1 > 0
		    ? (1<<(precision-BITS_PER_WORD-1))-1
		    : 0));

  /* Give this type's extreme values this type as their type.  */

  TREE_TYPE (TYPE_MIN_VALUE (type)) = type;
  TREE_TYPE (TYPE_MAX_VALUE (type)) = type;

  /* The first type made with this or `make_unsigned_type'
     is the type for size values.  */

  if (sizetype == 0)
    sizetype = type;

  /* Lay out the type: set its alignment, size, etc.  */

  layout_type (type);

  return type;
}

/* Create and return a type for unsigned integers of PRECISION bits.  */

tree
make_unsigned_type (precision)
     int precision;
{
  register tree type = make_node (INTEGER_TYPE);
  register tree low, high;

  TYPE_PRECISION (type) = precision;

  /* The first type made with this or `make_unsigned_type'
     is the type for size values.  */

  if (sizetype == 0)
    sizetype = type;

  fixup_unsigned_type (type);
  return type;
}

/* Set the extreme values of TYPE based on its precision in bits,
   the lay it out.  This is used both in `make_unsigned_type'
   and for enumeral types.  */

void
fixup_unsigned_type (type)
     tree type;
{
  register int precision = TYPE_PRECISION (type);

  TYPE_MIN_VALUE (type) = build_int_2 (0, 0);
  TYPE_MAX_VALUE (type)
    = build_int_2 (precision-BITS_PER_WORD >= 0 ? -1 : (1<<precision)-1,
		   precision-BITS_PER_WORD > 0
		   ? (1<<(precision-BITS_PER_WORD))-1 : 0);
  TREE_TYPE (TYPE_MIN_VALUE (type)) = type;
  TREE_TYPE (TYPE_MAX_VALUE (type)) = type;

  /* Lay out the type: set its alignment, size, etc.  */

  layout_type (type);
}
