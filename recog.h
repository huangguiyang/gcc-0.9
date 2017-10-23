/* Declarations for interface to insn recognizer and insn-output.c.
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


extern rtx recog_operand[];

extern rtx *recog_operand_loc[];

extern rtx *recog_dup_loc[];

extern char recog_dup_num[];

extern rtx recog_addr_dummy;

extern int recog ();

extern char *insn_template[];

extern int insn_n_operands[];

extern int insn_n_dups[];

extern char *insn_operand_constraint[][MAX_RECOG_OPERANDS];

extern char insn_operand_address_p[][MAX_RECOG_OPERANDS];

extern enum machine_mode insn_operand_mode[][MAX_RECOG_OPERANDS];

extern char *output_insn_hairy ();
