/* Definitions of target machine for GNU compiler.  Vax version.
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


/* Names to predefine in the preprocessor for this target machine.  */

#define CPP_PREDEFINES "-Dvax"

/* Run-time compilation parameters selecting different hardware subsets.  */

extern int target_flags;

/* Macros used in the machine description to test the flags.  */

/* Nonzero if compiling code that Unix assembler can assemble.  */
#define TARGET_UNIX_ASM (target_flags & 1)

/* Macro to define tables used to set the flags.
   This is a list in braces of pairs in braces,
   each pair being { "NAME", VALUE }
   where VALUE is the bits to set or minus the bits to clear.
   An empty string NAME is used to identify the default VALUE.  */

#define TARGET_SWITCHES  \
  { {"unix", 1},  \
    { "", 0}}

/* Target machine storage layout */

/* Define this if most significant bit is lowest numbered
   in instructions that operate on numbered bit-fields.
   This is not true on the vax.  */
/* #define BITS_BIG_ENDIAN */

/* Define this if most significant byte of a word is the lowest numbered.  */
/* That is not true on the vax.  */
/* #define BYTES_BIG_ENDIAN */

/* Define this if most significant word of a multiword number is numbered.  */
/* This is not true on the vax.  */
/* #define WORDS_BIG_ENDIAN */

/* Number of bits in an addressible storage unit */
#define BITS_PER_UNIT 8

/* Width in bits of a "word", which is the contents of a machine register.
   Note that this is not necessarily the width of data type `int';
   if using 16-bit ints on a 68000, this would still be 32.
   But on a machine with 16-bit registers, this would be 16.  */
#define BITS_PER_WORD 32

/* Width of a word, in units (bytes).  */
#define UNITS_PER_WORD 4

/* Width in bits of a pointer.
   See also the macro `Pmode' defined below.  */
#define POINTER_SIZE 32

/* Allocation boundary (in *bits*) for storing pointers in memory.  */
#define POINTER_BOUNDARY 32

/* Allocation boundary (in *bits*) for storing arguments in argument list.  */
#define PARM_BOUNDARY 32

/* Allocation boundary (in *bits*) for the code of a function.  */
#define FUNCTION_BOUNDARY 16

/* There is no point aligning anything to a rounder boundary than this.  */
#define BIGGEST_ALIGNMENT 32

/* Define this if move instructions will actually fail to work
   when given unaligned data.  */
/* #define STRICT_ALIGNMENT */

/* Standard register usage.  */

/* Number of actual hardware registers.
   The hardware registers are assigned numbers for the compiler
   from 0 to just below FIRST_PSEUDO_REGISTER.
   All registers that the compiler knows about must be given numbers,
   even those that are not normally considered general registers.  */
#define FIRST_PSEUDO_REGISTER 16

/* 1 for registers that have pervasive standard uses
   and are not available for the register allocator.
   On the vax, these are the AP, FP, SP and PC.  */
#define FIXED_REGISTERS {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1}

/* 1 for registers not available across function calls.
   These must include the FIXED_REGISTERS and also any
   registers that can be used without being saved.
   The latter must include the registers where values are returned
   and the register where structure-value addresses are passed.
   Aside from that, you can include as many other registers as you like.  */
#define CALL_USED_REGISTERS {1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1}

/* Return number of consecutive hard regs needed starting at reg REGNO
   to hold something of mode MODE.
   This is ordinarily the length in words of a value of mode MODE
   but can be less for certain modes in special long registers.
   On the vax, all registers are one word long.  */
#define HARD_REGNO_NREGS(REGNO, MODE)   \
 ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* Value is 1 if hard register REGNO can hold a value of machine-mode MODE.
   On the vax, all registers can hold all modes.  */
#define HARD_REGNO_MODE_OK(REGNO, MODE) 1

/* Value is 1 if it is a good idea to tie two pseudo registers
   when one has mode MODE1 and one has mode MODE2.
   If HARD_REGNO_MODE_OK could produce different values for MODE1 and MODE2,
   for any hard reg, then this must be 0 for correct output.  */
#define MODES_TIEABLE_P(MODE1, MODE2)  1

/* Specify the registers used for certain standard purposes.
   The values of these macros are register numbers.  */

/* Vax pc is overloaded on a register.  */
#define PC_REGNUM 15

/* Register to use for pushing function arguments.  */
#define STACK_POINTER_REGNUM 14

/* Base register for access to local variables of the function.  */
#define FRAME_POINTER_REGNUM 13

/* Base register for access to arguments of the function.  */
#define ARG_POINTER_REGNUM 12

/* Register in which static-chain is passed to a function.  */
#define STATIC_CHAIN_REGNUM 0

/* Register in which function's value is returned.
   Actually, multiple registers starting with this one may be used
   depending on the machine mode of the value.  */
#define FUNCTION_VALUE_REGNUM 0

/* Register in which address to store a structure value
   is passed to a function.  */
#define STRUCT_VALUE_REGNUM 1

/* Define the classes of registers for register constraints in the
   machine description.  Also define ranges of constants.

   One of the classes must always be named ALL_REGS and include all hard regs.
   If there is more than one class, another class must be named NO_REGS
   and contain no registers.

   The name GENERAL_REGS must be the name of a class (or an alias for
   another name such as ALL_REGS).  This is the class of registers
   that is allowed by "g" or "r" in a register constraint.
   Also, registers outside this class are allocated only when
   instructions express preferences for them.

   The classes must be numbered in nondecreasing order; that is,
   a larger-numbered class must never be contained completely
   in a smaller-numbered class.

   For any two classes, it is very desirable that there be another
   class that represents their union.  */
   
/* The vax has only one kind of registers, so NO_REGS and ALL_REGS
   are the only classes.  */

enum reg_class { NO_REGS, ALL_REGS, LIM_REG_CLASSES };

#define N_REG_CLASSES (int) LIM_REG_CLASSES

/* Since GENERAL_REGS is the same class as ALL_REGS,
   don't give it a different class number; just make it an alias.  */

#define GENERAL_REGS ALL_REGS

/* Give names of register classes as strings for dump file.   */

#define REG_CLASS_NAMES \
 {"NO_REGS", "ALL_REGS" }

/* Define which registers fit in which classes.
   This is an initializer for a vector of HARD_REG_SET
   of length N_REG_CLASSES.  */

#define REG_CLASS_CONTENTS {0, 0xffff}

/* The same information, inverted:
   Return the class number of the smallest class containing
   reg number REGNO.  This could be a conditional expression
   or could index an array.  */

#define REGNO_REG_CLASS(REGNO) ALL_REGS

/* Define a table that lets us find quickly all the reg classes
   containing a given one.  This is the initializer for an
   N_REG_CLASSES x N_REG_CLASSES array of reg class codes.
   Row N is a sequence containing all the class codes for
   classes that contain all the regs in class N.  Each row
   contains no duplicates, and is terminated by LIM_REG_CLASSES.  */

/* We give just a dummy for the first element, which is for NO_REGS.  */
#define REG_CLASS_SUPERCLASSES \
 {{LIM_REG_CLASSES}, {LIM_REG_CLASSES}}

/* The inverse relationship:
   for each class, a list of all reg classes contained in it.  */
#define REG_CLASS_SUBCLASSES \
 {{LIM_REG_CLASSES}, {LIM_REG_CLASSES}}

/* Define a table that lets us find quickly the class
   for the subunion of any two classes.

   We say "subunion" because the result need not be exactly
   the union; it may instead be a subclass of the union
   (though the closer to the union, the better).
   But if it contains anything beyond union of the two classes,
   you will lose!

   This is an initializer for an N_REG_CLASSES x N_REG_CLASSES
   array of reg class codes.  The subunion of classes C1 and C2
   is just element [C1, C2].  */

#define REG_CLASS_SUBUNION  \
{{NO_REGS, ALL_REGS}, \
 {ALL_REGS, ALL_REGS}}

/* The class value for index registers, and the one for base regs.  */

#define INDEX_REG_CLASS ALL_REGS
#define BASE_REG_CLASS ALL_REGS

/* Get reg_class from a letter such as appears in the machine description.  */

#define REG_CLASS_FROM_LETTER(C) NO_REGS

/* The letters I, J, K, L and M in a register constraint string
   can be used to stand for particular ranges of immediate operands.
   This macro defines what the ranges are.
   C is the letter, and VALUE is a constant value.
   Return 1 if VALUE is in the range specified by C.  */

#define CONST_OK_FOR_LETTER_P(VALUE, C)  0

/* Similar, but for floating constants, and defining letters G and H.
   Here VALUE is the CONST_DOUBLE rtx itself.  */

#define CONST_DOUBLE_OK_FOR_LETTER_P(VALUE, C) 1

/* Given an rtx X being reloaded into a reg required to be
   in class CLASS, return the class of reg to actually use.
   In general this is just CLASS; but on some machines
   in some cases it is preferable to use a more restrictive class.  */

#define PREFERRED_RELOAD_CLASS(X,CLASS)  (CLASS)

/* Stack layout; function entry, exit and calling.  */

/* Define this if pushing a word on the stack
   makes the stack pointer a smaller address.  */
#define STACK_GROWS_DOWNWARD

/* Define this if the nominal address of the stack frame
   is at the high-address end of the local variables;
   that is, each additional local variable allocated
   goes at a more negative offset in the frame.  */
#define FRAME_GROWS_DOWNWARD

/* Offset within stack frame to start allocating local variables at.
   If FRAME_GROWS_DOWNWARD, this is the offset to the END of the
   first local allocated.  Otherwise, it is the offset to the BEGINNING
   of the first local allocated.  */
#define STARTING_FRAME_OFFSET 0

/* If we generate an insn to push BYTES bytes,
   this says how many the stack pointer really advances by.
   On the vax, sp@- in a byte insn really pushes a word.  */
#define PUSH_ROUNDING(BYTES) (((BYTES) + 1) & ~1)

/* Offset of first parameter from the argument pointer register value.  */
#define FIRST_PARM_OFFSET 4

/* Define if returning from a function call automatically
   pops the arguments described by the number-of-args field in the call.  */

#define RETURN_POPS_ARGS

/* This macro generates the assembly code for function entry.
   FILE is a stdio stream to output the code to.
   SIZE is an int: how many units of temporary storage to allocate.
   Refer to the array `regs_ever_live' to determine which registers
   to save; `regs_ever_live[I]' is nonzero if register number I
   is ever used in the function.  This macro is responsible for
   knowing which registers should not be saved even if used.  */

#define FUNCTION_PROLOGUE(FILE, SIZE)     \
{ register int regno;						\
  register int mask = 0;					\
  static char dont_save_regs[] = CALL_USED_REGISTERS;		\
  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)	\
    if (regs_ever_live[regno] && !dont_save_regs[regno])	\
       mask |= 1 << regno;					\
  fprintf (FILE, "\t.word 0x%x\n", mask & ~077);		\
  if ((SIZE) >= 64) fprintf (FILE, "\tmovab %d(sp),sp\n", SIZE);\
  else if (SIZE) fprintf (FILE, "\tsubl2 $%d,sp\n", - (SIZE)); }

/* This macro generates the assembly code for function exit,
   on machines that need it.  If FUNCTION_EPILOGUE is not defined
   then individual return instructions are generated for each
   return statement.  Args are same as for FUNCTION_PROLOGUE.  */

/* #define FUNCTION_EPILOGUE(FILE, SIZE)  */

/* Addressing modes, and classification of registers for them.  */

#define HAVE_POST_INCREMENT
/* #define HAVE_POST_DECREMENT */

#define HAVE_PRE_DECREMENT
/* #define HAVE_PRE_INCREMENT */

/* These assume that REGNO is a hard or pseudo reg number.
   They give nonzero only if REGNO is a hard reg of the suitable class
   or a pseudo reg currently allocated to a suitable hard reg.
   These definitions are NOT overridden anywhere.  */

#define REGNO_OK_FOR_INDEX_P(regno)  \
((regno) < FIRST_PSEUDO_REGISTER || reg_renumber[regno] >= 0)
#define REGNO_OK_FOR_BASE_P(regno) \
((regno) < FIRST_PSEUDO_REGISTER || reg_renumber[regno] >= 0)

/* The macros REG_OK_FOR..._P assume that the arg is a REG rtx
   and check its validity for a certain class.
   We have two alternate definitions for each of them.
   The usual definition accepts all pseudo regs; the other rejects them all.
   The symbol REG_OK_STRICT causes the latter definition to be used.

   Most source files want to accept pseudo regs in the hope that
   they will get allocated to the class that the insn wants them to be in.
   Some source files that are used after register allocation
   need to be strict.  */

#ifndef REG_OK_STRICT

/* Nonzero if X is a hard reg that can be used as an index or if
   it is a pseudo reg.  */
#define REG_OK_FOR_INDEX_P(X) 1
/* Nonzero if X is a hard reg that can be used as a base reg
   of if it is a pseudo reg.  */
#define REG_OK_FOR_BASE_P(X) 1

#else

/* Nonzero if X is a hard reg that can be used as an index.  */
#define REG_OK_FOR_INDEX_P(X) REGNO_OK_FOR_INDEX_P (REGNO (X))
/* Nonzero if X is a hard reg that can be used as a base reg.  */
#define REG_OK_FOR_BASE_P(X) REGNO_OK_FOR_BASE_P (REGNO (X))

#endif

#define REG_OK_FOR_CLASS_P(X, C) 0

#define REGNO_OK_FOR_CLASS_P(X, C)  0

/* Maximum number of registers that can appear in a valid memory address.  */

#define MAX_REGS_PER_ADDRESS 2

/* GO_IF_LEGITIMATE_ADDRESS recognizes an RTL expression
   that is a valid memory address for an instruction.
   The MODE argument is the machine mode for the MEM expression
   that wants to use this address.

   The other macros defined here are used only in GO_IF_LEGITIMATE_ADDRESS,
   except for CONSTANT_ADDRESS_P which is actually machine-independent.  */

/* 1 if X is an address that we could indirect through.  */
#define INDIRECTABLE_ADDRESS_P(X)  \
  (CONSTANT_ADDRESS_P (X)						\
   || (GET_CODE (X) == REG && REG_OK_FOR_BASE_P (X))			\
   || (GET_CODE (X) == PLUS						\
       && GET_CODE (XEXP (X, 0)) == REG					\
       && REG_OK_FOR_BASE_P (XEXP (X, 0))				\
       && CONSTANT_ADDRESS_P (XEXP (X, 1))))

/* Go to ADDR if X is a valid address not using indexing.
   (This much is the easy part.)  */
#define GO_IF_NONINDEXED_ADDRESS(X, ADDR)  \
{ register rtx xfoob = (X);						\
  if (GET_CODE (xfoob) == REG) goto ADDR;				\
  if (INDIRECTABLE_ADDRESS_P (xfoob)) goto ADDR;			\
  xfoob = XEXP (X, 0);							\
  if (GET_CODE (X) == MEM && INDIRECTABLE_ADDRESS_P (xfoob))		\
    goto ADDR;								\
  if ((GET_CODE (X) == PRE_DEC || GET_CODE (X) == POST_INC)		\
      && GET_CODE (xfoob) == REG && REG_OK_FOR_BASE_P (xfoob))		\
    goto ADDR; }

/* 1 if PROD is either a reg times size of mode MODE
   or just a reg, if MODE is just one byte.
   This macro's expansion uses the temporary variables xfoo0 and xfoo1
   that must be declared in the surrounding context.  */
#define INDEX_TERM_P(PROD, MODE)   \
(GET_MODE_SIZE (MODE) == 1						\
 ? (GET_CODE (PROD) == REG && REG_OK_FOR_BASE_P (PROD))			\
 : (GET_CODE (PROD) == MULT						\
    &&									\
    (xfoo0 = XEXP (PROD, 0), xfoo1 = XEXP (PROD, 1),			\
     ((GET_CODE (xfoo0) == CONST_INT					\
       && INTVAL (xfoo0) == GET_MODE_SIZE (MODE)			\
       && GET_CODE (xfoo1) == REG					\
       && REG_OK_FOR_INDEX_P (xfoo1))					\
      ||								\
      (GET_CODE (xfoo1) == CONST_INT					\
       && INTVAL (xfoo1) == GET_MODE_SIZE (MODE)			\
       && GET_CODE (xfoo0) == REG					\
       && REG_OK_FOR_INDEX_P (xfoo0))))))

/* Go to ADDR if X is the sum of a register
   and a valid index term for mode MODE.  */
#define GO_IF_REG_PLUS_INDEX(X, MODE, ADDR)	\
{ register rtx xfooa;							\
  if (GET_CODE (X) == PLUS)						\
    { if (GET_CODE (XEXP (X, 0)) == REG					\
	  && REG_OK_FOR_BASE_P (XEXP (X, 0))				\
	  && (xfooa = XEXP (X, 1),					\
	      INDEX_TERM_P (xfooa, MODE)))				\
	goto ADDR;							\
      if (GET_CODE (XEXP (X, 1)) == REG					\
	  && REG_OK_FOR_BASE_P (XEXP (X, 1))				\
	  && (xfooa = XEXP (X, 0),					\
	      INDEX_TERM_P (xfooa, MODE)))				\
	goto ADDR; } }

#define GO_IF_LEGITIMATE_ADDRESS(MODE, X, ADDR)  \
{ register rtx xfoo, xfoo0, xfoo1;					\
  GO_IF_NONINDEXED_ADDRESS (X, ADDR);					\
  if (GET_CODE (X) == PLUS)						\
    { /* Handle <address>[index] represented with index-sum outermost */\
      xfoo = XEXP (X, 0);						\
      if (INDEX_TERM_P (xfoo, MODE))					\
	{ GO_IF_NONINDEXED_ADDRESS (XEXP (X, 1), ADDR); }		\
      xfoo = XEXP (X, 1);						\
      if (INDEX_TERM_P (xfoo, MODE))					\
	{ GO_IF_NONINDEXED_ADDRESS (XEXP (X, 0), ADDR); }		\
      /* Handle offset(reg)[index] with offset added outermost */	\
      if (CONSTANT_ADDRESS_P (XEXP (X, 0)))				\
	{ if (GET_CODE (XEXP (X, 1)) == REG				\
	      && REG_OK_FOR_BASE_P (XEXP (X, 1)))			\
	    goto ADDR;							\
	  GO_IF_REG_PLUS_INDEX (XEXP (X, 1), MODE, ADDR); }		\
      if (CONSTANT_ADDRESS_P (XEXP (X, 1)))				\
	{ if (GET_CODE (XEXP (X, 0)) == REG				\
	      && REG_OK_FOR_BASE_P (XEXP (X, 0)))			\
	    goto ADDR;							\
	  GO_IF_REG_PLUS_INDEX (XEXP (X, 0), MODE, ADDR); } } }

#define CONSTANT_ADDRESS_P(X)   \
  (GET_CODE (X) == LABEL_REF || GET_CODE (X) == SYMBOL_REF		\
   || GET_CODE (X) == CONST_INT						\
   || GET_CODE (X) == CONST)

#define REG_P(X)	\
  (GET_CODE (X) == REG)

/* Try machine-dependent ways of modifying an illegitimate address
   to be legitimate.  If we find one, return the new, valid address.
   This macro is used in only one place: `memory_address' in explow.c.

   OLDX is the address as it was before break_out_memory_refs was called.
   In some cases it is useful to look at this to decide what needs to be done.

   MODE and WIN are passed so that this macro can use
   GO_IF_LEGITIMATE_ADDRESS.

   It is always safe for this macro to do nothing.  It exists to recognize
   opportunities to optimize the output.

   For the vax, nothing needs to be done.  */

#define LEGITIMIZE_ADDRESS(X,OLDX,MODE,WIN)  {}

/* Specify the machine mode that this machine uses
   for the index in the tablejump instruction.  */
#define CASE_VECTOR_MODE HImode

/* Define this if the tablejump instruction expects the table
   to contain offsets from the address of the table.
   Do not define this if the table should contain absolute addresses.  */
#define CASE_VECTOR_PC_RELATIVE

/* Specify the tree operation to be used to convert reals to integers.  */
#define IMPLICIT_FIX_EXPR FIX_ROUND_EXPR

/* This is the kind of divide that is easiest to do in the general case.  */
#define EASY_DIV_EXPR TRUNC_DIV_EXPR

/* Max number of bytes we can move from memory to memory
   in one reasonably fast instruction.  */
#define MOVE_MAX 16

/* Define this if zero-extension is slow (more than one real instruction).  */
/* #define SLOW_ZERO_EXTEND */

/* Define if shifts truncate the shift count
   which implies one can omit a sign-extension or zero-extension
   of a shift count.  */
/* #define SHIFT_COUNT_TRUNCATED */

/* Value is 1 if truncating an integer of INPREC bits to OUTPREC bits
   is done just by pretending it is already truncated.  */
#define TRULY_NOOP_TRUNCATION(OUTPREC, INPREC) 1

/* Specify the machine mode that pointers have.
   After generation of rtl, the compiler makes no further distinction
   between pointers and any other objects of this machine mode.  */
#define Pmode SImode

/* A function address in a call instruction
   is a byte address (for indexing purposes)
   so give the MEM rtx a byte's mode.  */
#define FUNCTION_MODE QImode

/* Compute the cost of computing a constant rtl expression RTX
   whose rtx-code is CODE.  The body of this macro is a portion
   of a switch statement.  If the code is computed here,
   return it with a return statement.  Otherwise, break from the switch.  */

#define CONST_COSTS(RTX,CODE) \
  case CONST_INT:						\
    /* Constant zero is super cheap due to clr instruction.  */	\
    if (RTX == const0_rtx) return 0;				\
    if ((unsigned) INTVAL (RTX) < 077) return 1;		\
  case CONST:							\
  case LABEL_REF:						\
  case SYMBOL_REF:						\
    return 3;							\
  case CONST_DOUBLE:						\
    return 5;

/* Tell final.c how to eliminate redundant test instructions.  */

/* Here we define machine-dependent flags and fields in cc_status
   (see `conditions.h').  No extra ones are needed for the vax.  */

/* Store in cc_status the expressions
   that the condition codes will describe
   after execution of an instruction whose pattern is EXP.
   Do not alter them if the instruction would not alter the cc's.  */

/* On the 68000, all the insns to store in an address register
   fail to set the cc's.  However, in some cases these instructions
   can make it possibly invalid to use the saved cc's.  In those
   cases we clear out some or all of the saved cc's so they won't be used.  */

#define NOTICE_UPDATE_CC(EXP) \
{ if (GET_CODE (EXP) == SET)					\
    { if (GET_CODE (SET_DEST (EXP)) != PC)			\
	{ cc_status.flags = 0;					\
	  cc_status.value1 = SET_DEST (EXP);			\
	  cc_status.value2 = SET_SRC (EXP); } }			\
  else if (GET_CODE (EXP) == PARALLEL				\
	   && GET_CODE (XVECEXP (EXP, 0, 0)) == SET)		\
    { if (GET_CODE (SET_DEST (XVECEXP (EXP, 0, 0))) != PC)	\
	{ cc_status.flags = 0;					\
	  cc_status.value1 = SET_DEST (XVECEXP (EXP, 0, 0));	\
	  cc_status.value2 = SET_SRC (XVECEXP (EXP, 0, 0)); } }	\
  else CC_STATUS_INIT;						\
  if (cc_status.value1 && GET_CODE (cc_status.value1) == REG	\
      && cc_status.value2					\
      && reg_mentioned_p (cc_status.value1, cc_status.value2))	\
    cc_status.value2 = 0;					\
  if (cc_status.value1 && GET_CODE (cc_status.value1) == MEM	\
      && cc_status.value2					\
      && GET_CODE (cc_status.value2) == MEM)			\
    cc_status.value2 = 0; }
/* Actual condition, one line up, should be that value2's address
   depends on value1, but that is too much of a pain.  */

#define OUTPUT_JUMP(NORMAL, FLOAT, NO_OV)  \
{ if (cc_status.flags & CC_NO_OVERFLOW)				\
    return NO_OV;						\
  return NORMAL; }

/* Control the assembler format that we output.  */

#define TEXT_SECTION_ASM_OP ".text"

#define DATA_SECTION_ASM_OP ".data"

/* How to refer to registers in assembler output.
   This sequence is indexed by compiler's hard-register-number (see above).  */

#define REGISTER_NAMES \
{"r0", "r1", "r2", "r3", "r4", "r5", "r6", "r7", "r8", \
 "r9", "r10", "r11", "ap", "fp", "sp", "pc"}

/* How to renumber registers for dbx and gdb.
   Vax needs no change in the numeration.  */

#define DBX_REGISTER_NUMBER(REGNO) (REGNO)

/* This is how to output an assembler line defining a `double' constant.  */

#define ASM_OUTPUT_DOUBLE(FILE,VALUE)  \
  fprintf (FILE, "\t.double 0d%.12e\n", (VALUE))

/* This is how to output an assembler line defining a `float' constant.  */

#define ASM_OUTPUT_FLOAT(FILE,VALUE)  \
  fprintf (FILE, "\t.float 0f%.6e\n", (VALUE))

/* This is how to output an assembler line
   that says to advance the location counter
   to a multiple of 2**LOG bytes.  */

#define ASM_OUTPUT_ALIGN(FILE,LOG)  \
  fprintf (FILE, "\t.align %d\n", (LOG))

#define ASM_OUTPUT_SKIP(FILE,SIZE)  \
  fprintf (FILE, "\t.space %d\n", (SIZE))

/* Define the parentheses used to group arithmetic operations
   in assembler code.  */

#define ASM_OPEN_PAREN "("
#define ASM_CLOSE_PAREN ")"

/* Specify what to precede various sizes of constant with
   in the output file.  */

#define ASM_INT_OP ".long "
#define ASM_SHORT_OP ".word "
#define ASM_CHAR_OP ".byte "

/* Define results of standard character escape sequences.  */
#define TARGET_BELL 007
#define TARGET_BS 010
#define TARGET_TAB 011
#define TARGET_NEWLINE 012
#define TARGET_VT 013
#define TARGET_FF 014
#define TARGET_CR 015

/* Print an instruction operand X on file FILE.  */

#define PRINT_OPERAND(FILE, X)  \
{ if (GET_CODE (X) == REG)						\
    fprintf (FILE, "%s", reg_name [REGNO (X)]);				\
  else if (GET_CODE (X) == MEM)						\
    output_address (XEXP (X, 0));					\
  else if (GET_CODE (X) == CONST_DOUBLE)				\
    { union { double d; int i[2]; } u;					\
      u.i[0] = XINT (X, 0); u.i[1] = XINT (X, 1);			\
      fprintf (FILE, "$0d%.12e", u.d); }				\
  else { putc ('$', FILE); output_addr_const (FILE, X); }}

/* Print a memory operand whose address is X, on file FILE.  */

#define PRINT_OPERAND_ADDRESS(FILE, ADDR)  \
{ register rtx reg1, reg2, breg, ireg;					\
  register rtx addr = ADDR;						\
  rtx offset;								\
 retry:									\
  switch (GET_CODE (addr))						\
    {									\
    case MEM:								\
      fprintf (FILE, "*");						\
      addr = XEXP (addr, 0);						\
      goto retry;							\
    case REG:								\
      fprintf (FILE, "(%s)", reg_name [REGNO (addr)]);			\
      break;								\
    case PRE_DEC:							\
      fprintf (FILE, "-(%s)", reg_name [REGNO (XEXP (addr, 0))]);	\
      break;								\
    case POST_INC:							\
      fprintf (FILE, "(%s)+", reg_name [REGNO (XEXP (addr, 0))]);	\
      break;								\
    case PLUS:								\
      reg1 = 0;	reg2 = 0;						\
      ireg = 0;	breg = 0;						\
      offset = 0;							\
      if (CONSTANT_ADDRESS_P (XEXP (addr, 0))				\
	  || GET_CODE (XEXP (addr, 0)) == MEM)				\
	{								\
	  offset = XEXP (addr, 0);					\
	  addr = XEXP (addr, 1);					\
	}								\
      else if (CONSTANT_ADDRESS_P (XEXP (addr, 1))			\
	       || GET_CODE (XEXP (addr, 1)) == MEM)			\
	{								\
	  offset = XEXP (addr, 1);					\
	  addr = XEXP (addr, 0);					\
	}								\
      if (GET_CODE (addr) != PLUS) ;					\
      else if (GET_CODE (XEXP (addr, 0)) == MULT)			\
	{								\
	  reg1 = XEXP (addr, 0);					\
	  addr = XEXP (addr, 1);					\
	}								\
      else if (GET_CODE (XEXP (addr, 1)) == MULT)			\
	{								\
	  reg1 = XEXP (addr, 1);					\
	  addr = XEXP (addr, 0);					\
	}								\
      else if (GET_CODE (XEXP (addr, 0)) == REG)			\
	{								\
	  reg1 = XEXP (addr, 0);					\
	  addr = XEXP (addr, 1);					\
	}								\
      else if (GET_CODE (XEXP (addr, 1)) == REG)			\
	{								\
	  reg1 = XEXP (addr, 1);					\
	  addr = XEXP (addr, 0);					\
	}								\
      if (GET_CODE (addr) == REG || GET_CODE (addr) == MULT)		\
	{ if (reg1 == 0) reg1 = addr; else reg2 = addr; addr = 0; }	\
      if (offset != 0) { if (addr != 0) abort (); addr = offset; }	\
      if (reg1 != 0 && GET_CODE (reg1) == MULT)				\
	{ breg = reg2; ireg = reg1; }					\
      else if (reg2 != 0 && GET_CODE (reg2) == MULT)			\
	{ breg = reg1; ireg = reg2; }					\
      else if (reg2 != 0 || GET_CODE (addr) == MEM)			\
	{ breg = reg2; ireg = reg1; }					\
      else								\
	{ breg = reg1; ireg = reg2; }					\
      if (addr != 0)							\
	output_address (offset);					\
      if (breg != 0)							\
	{ if (GET_CODE (breg) != REG) abort ();				\
	  fprintf (FILE, "(%s)", reg_name[REGNO (breg)]); }		\
      if (ireg != 0)							\
	{ if (GET_CODE (ireg) == MULT) ireg = XEXP (ireg, 0);		\
	  if (GET_CODE (ireg) != REG) abort ();				\
	  fprintf (FILE, "[%s]", reg_name[REGNO (ireg)]); }		\
      break;								\
    default:								\
      output_addr_const (FILE, addr);					\
    }}
