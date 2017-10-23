/* Communication with stupid.c.
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


/* Nonzero means use stupid register allocation
   and force into memory all user variables not declared `register'.  */

extern int obey_regdecls;

/* For the case of stupid register allocation,
   user register variables should have a life span
   equal to the extent of the block in which they are declared.
   stmt.c uses these variables to pass the information to stupid.c.

   `reg_birth_insn[N]' is the last insn before the beginning of
   the scope of pseudo-register N.  `reg_death_insn[N]' is the
   last insn in the scope.

   Both of these vectors are of length `first_temp_register',
   which is one plus the number of the last pseudo-register
   that corresponds to a user variable.  */

extern int first_temp_reg_num;

extern rtx *reg_birth_insn;

extern rtx *reg_death_insn;
