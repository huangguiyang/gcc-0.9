/* Register Transfer Language (RTL) definitions for GNU C-Compiler
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


/* Register Transfer Language EXPRESSIONS CODES */

#define RTX_CODE	enum rtx_code
enum rtx_code  {

#define DEF_RTL_EXPR(ENUM, NAME, FORMAT)   ENUM ,
#include "rtl.def"		/* rtl expressions are documented here */
#undef DEF_RTL_EXPR

  LAST_AND_UNUSED_RTX_CODE};	/* A convienent way to get a value for
				   NUM_RTX_CODE.
				   Assumes default enum value assignement.  */

#define NUM_RTX_CODE ((int)LAST_AND_UNUSED_RTX_CODE)
				/* The cast here, saves many elsewhere.  */

extern int rtx_length[];
#define GET_RTX_LENGTH(CODE)		(rtx_length[(int)(CODE)])

extern char *rtx_name[];
#define GET_RTX_NAME(CODE)		(rtx_name[(int)(CODE)])

extern char *rtx_format[];
#define GET_RTX_FORMAT(CODE)		(rtx_format[(int)(CODE)])


/* Get the definition of `enum machine_mode' */

#ifndef HAVE_MACHINE_MODES
#define DEF_MACHMODE(SYM, NAME, TYPE, SIZE, UNIT)  SYM,

enum machine_mode {
#include "machmode.def"
MAX_MACHINE_MODE };

#undef DEF_MACHMODE

#define HAVE_MACHINE_MODES

#endif /* not HAVE_MACHINE_MODES */

#define NUM_MACHINE_MODE ((int)MAX_MACHINE_MODE)

extern char *mode_name[];
#define GET_MODE_NAME(MODE)		(mode_name[(int)(MODE)])

extern int mode_size[];
#define GET_MODE_SIZE(MODE)		(mode_size[(int)(MODE)])

extern int mode_unit_size[];
#define GET_MODE_UNIT_SIZE(MODE)	(mode_unit_size[(int)(MODE)])

#define GET_MODE_BITSIZE(MODE)  (BITS_PER_UNIT * mode_size[(int)(MODE)])

#define GET_MODE_MASK(MODE)  \
   ((1 << (BITS_PER_UNIT * mode_size[(int)(MODE)])) - 1)

/* Common union for an element of an rtx.  */

typedef union rtunion_def
{
  int rtint;
  char *rtstr;
  struct rtx_def *rtx;
  struct rtvec_def *rtvec;
  enum machine_mode rttype;
} rtunion;

/* RTL expression ("rtx").  */

typedef struct rtx_def
{
  /* The kind of expression this is.  */
  enum rtx_code code : 16;
  /* The kind of value the expression has.  */
  enum machine_mode mode : 8;
  /* 1 in an INSN if it can alter flow of control
     within this function.  Not yet used!  */
  unsigned int jump : 1;
  /* 1 in an INSN if it can call another function.  Not yet used!  */
  unsigned int call : 1;
  /* 1 if value of this expression will never change
     during the current function, even though it is not
     manifestly constant.  Not yet used!  */
  unsigned int unchanging : 1;
  /* 1 in a MEM expression if contents of memory are volatile.
     Not yet used!  */
  unsigned int volatile : 1;
  /* 1 in a MEM referring to a field of a structure (not a union!).
     0 if the MEM was a variable or the result of a * operator in C;
     1 if it was the result of a . or -> operator in C.
     Not yet used!  */
  unsigned int in_struct : 1;
  /* The first element of the operands of this rtx.
     The number of operands and their types are controlled
     by the `code' field, according to rtl.def.  */
  rtunion fld[1];
} *rtx;

#define NULL_RTX (rtx) NULL

#define GET_CODE(RTX)		((RTX)->code)
#define PUT_CODE(RTX, CODE)	((RTX)->code = (CODE))

#define GET_MODE(RTX)		((RTX)->mode)
#define PUT_MODE(RTX, MODE)	((RTX)->mode = (MODE))

/* RTL vector.  These appear inside RTX's when there is a need
   for a variable number of things.  The principle use is inside
   PARALLEL expressions.  */

typedef struct rtvec_def{
  unsigned num_elem;		/* number of elements */
  rtunion elem[1];
} *rtvec;

#define NULL_RTVEC (rtvec) NULL

#define GET_NUM_ELEM(RTVEC)		((RTVEC)->num_elem)
#define PUT_NUM_ELEM(RTVEC, NUM)	((RTVEC)->num_elem = (unsigned) NUM)

/* ----------------------------------------------------------------------
   FORMAT MACROS for access to rtx fields in specified data format.
   ---------------------------------------------------------------------- */

#define XEXP(RTX, N)	((RTX)->fld[N].rtx)
#define XINT(RTX, N)	((RTX)->fld[N].rtint)
#define XSTR(RTX, N)	((RTX)->fld[N].rtstr)
#define XVEC(RTX, N)	((RTX)->fld[N].rtvec)
#define XVECLEN(RTX, N)	((RTX)->fld[N].rtvec->num_elem)
#define XVECEXP(RTX,N,M)((RTX)->fld[N].rtvec->elem[M].rtx)

/* name of expression */
#define FORMAT_n(RTX)		(*GET_RTX_NAME(GET_CODE(RTX)))

/* string */
#define FORMAT_s(RTX, N)	((RTX)->fld[N].rtstr)

/* expression */
#define FORMAT_e(RTX, N)	((RTX)->fld[N].rtx)

/* elements of a vector of expressions */
#define PT_FORMAT_E(RTX, N)	((RTX)->fld[N].rtvec)
#define NUM_FORMAT_E(RTX, N)	((RTX)->fld[N].rtvec->num_elem)
#define FORMAT_E(RTX, N, M)	((RTX)->fld[N].rtvec->elem[M].rtx)

/* integer */
#define FORMAT_i(RTX, N)	((RTX)->fld[N].rtint)

/* pointed to insn uid */
#define PT_FORMAT_u(RTX, N)	((RTX)->fld[N].rtx)
#define FORMAT_u(RTX, N)	((RTX)->fld[N].rtx->fld[0].rtint)

/* vector of pointed to insn uids */
#define PT_FORMAT_U(RTX, N)	((RTX)->fld[N].rtvec)
#define NUM_FORMAT_U(RTX, N)	((RTX)->fld[N].rtvec->num_elem)
#define ELEM_FORMAT_U(RTX,N,M)	((RTX)->fld[N].rtvec->elem[M].rtx)
#define FORMAT_U(RTX, N, M) ((RTX)->fld[N].rtvec->elem[M].rtx->fld[0].rtint)

/* ----------------------------------------------------------------------
   ACCESS MACROS to particular fields of particular kinds of rtx's.
   ---------------------------------------------------------------------- */

/* Holds a unique number for each insn.
   These are not necessarily sequentially increasing.  */
#define INSN_UID(INSN)	((INSN)->fld[0].rtint)

#define PREV_INSN(INSN)	((INSN)->fld[1].rtx)
#define NEXT_INSN(INSN)	((INSN)->fld[2].rtx)

#define PATTERN(INSN)	((INSN)->fld[3].rtx)

/* Code number of instruction, from when it was recognized.
   Zero means this instruction has not been recognized yet.  */
#define INSN_CODE(INSN) ((INSN)->fld[4].rtint)

/* Set up in flow.c; empty before then.
   Holds a chain of INSN_LIST rtx's whose first operands point at
   previous insns with direct data-flow connections to this one.
   That means that those insns set variables whose next use is in this insn.
   They are always in the same basic block as this insn.  */
#define LOG_LINKS(INSN)		((INSN)->fld[5].rtx)

/* Holds a list of notes on what this insn does to various REGs.
   This is set up by flow.c; it is empty until then.
   It is a chain of EXPR_LIST rtx's, where the second operand
   is the chain pointer and the first operand is the REG being described.
   The mode field of the EXPR_LIST contains not a real machine mode
   but a value that says what this note says about the REG:
     REG_DEAD means that the REG dies in this insn.
     REG_INC means that the REG is autoincremented or autodecremented.
   Note that one insn can have both REG_DEAD and REG_INC for the same register
   if the register is preincremented or predecremented in the insn
   and not needed afterward.  This can probably happen.
     REG_CONST describes the insn as a whole; it says that the
   insn sets a register to a constant value and that if the
   register is spilled to the stack then the constant value
   should be substituted for it.
     REG_WAS_0 says that the specified register held 0 before this insn.  */

#define REG_NOTES(INSN)	((INSN)->fld[6].rtx)

enum reg_note { REG_DEAD = 1, REG_INC = 2, REG_CONST = 3, REG_WAS_0 = 4 };

#define CODE_LABEL_NUMBER(INSN)	((INSN)->fld[3].rtint)

#define LINE_NUMBER NOTE

#define NOTE_SOURCE_FILE(INSN)  ((INSN)->fld[3].rtstr)
#define NOTE_LINE_NUMBER(INSN) ((INSN)->fld[4].rtint)

/* Codes that appear in the NOTE_LINE_NUMBER field
   for kinds of notes that are not line numbers.  */

#define NOTE_INSN_FUNCTION_BEG 0
#define NOTE_INSN_DELETED -1
#define NOTE_INSN_BLOCK_BEG -2
#define NOTE_INSN_BLOCK_END -3
#define NOTE_INSN_LOOP_BEG -4
#define NOTE_INSN_LOOP_END -5

/* In jump.c, each label contains a count of the number
   of LABEL_REFs that point at it, so unused labels can be deleted.  */
#define LABEL_NUSES(LABEL) ((LABEL)->fld[4].rtx)

/* In jump.c, each JUMP_INSN can point to a label that it can jump to,
   so that if the JUMP_INSN is deleted, the label's LABEL_NUSES can
   be decremented and possibly the label can be deleted.  */
#define JUMP_LABEL(INSN)   ((INSN)->fld[7].rtx)

/* Once basic blocks are found in flow.c,
   each CODE_LABEL starts a chain that goes through
   all the LABEL_REFs that jump to that label.
   The chain eventually winds up at the CODE_LABEL; it is circular.  */
#define LABEL_REFS(LABEL) ((LABEL)->fld[4].rtx)

/* This is the field in the LABEL_REF through which the chain is linked.  */
#define LABEL_NEXTREF(REF) ((REF)->fld[1].rtx)

/* Once basic blocks are found in flow.c,
   Each LABEL_REF points to its containing instruction with this field.  */
#define CONTAINING_INSN(RTX) ((RTX)->fld[2].rtx)

/* For a REG rtx, REGNO extracts the register number.  */

#define REGNO(RTX) ((RTX)->fld[0].rtint)

/* For a CONST_INT rtx, INTVAL extracts the integer.  */

#define INTVAL(RTX) ((RTX)->fld[0].rtint)

/* For a SUBREG rtx, SUBREG_REG extracts the value we want a subreg of.
   SUBREG_WORD extracts the word-number.  */

#define SUBREG_REG(RTX) ((RTX)->fld[0].rtx)
#define SUBREG_WORD(RTX) ((RTX)->fld[1].rtint)

/* For a SET rtx, SET_DEST is the place that is set
   and SET_SRC is the value it is set to.  */
#define SET_DEST(RTX) ((RTX)->fld[0].rtx)
#define SET_SRC(RTX) ((RTX)->fld[1].rtx)

/* Generally useful functions.  */

extern rtx rtx_alloc ();
extern rtvec rtvec_alloc ();
extern rtx gen_rtx ();
extern rtx copy_rtx ();
extern rtvec gen_rtvec ();
extern rtvec gen_rtvec_v ();
extern rtx gen_reg_rtx ();
extern rtx gen_label_rtx ();
extern rtx gen_lowpart ();
extern rtx gen_highpart ();
extern int subreg_lowpart_p ();
extern rtx memory_address ();
extern rtx get_insns ();
extern rtx get_last_insn ();
extern rtx expand_expr ();
extern rtx output_constant_def ();
extern rtx immed_real_const ();
extern rtx force_const_double_mem ();
extern rtx get_parm_real_loc ();
extern rtx assign_stack_local ();
extern rtx protect_from_queue ();
extern void emit_queue ();
extern rtx emit_insn ();
extern rtx emit_jump_insn ();
extern rtx emit_call_insn ();
extern rtx emit_insn_before ();
extern rtx emit_insn_after ();
extern void emit_label ();
extern void emit_barrier ();
extern void emit_note ();
extern rtx prev_real_insn ();
extern rtx next_real_insn ();
extern rtx plus_constant ();
extern rtx find_equiv_reg ();
#ifdef BITS_PER_WORD
/* Conditional is to detect when config.h has been included.  */
extern enum reg_class reg_preferred_class ();
#endif

/* Standard pieces of rtx, to be substituted directly into things.  */
extern rtx pc_rtx;
extern rtx cc0_rtx;
extern rtx const0_rtx;
extern rtx const1_rtx;
extern rtx fconst0_rtx;
extern rtx dconst0_rtx;
