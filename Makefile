# Makefile for GNU C compiler.
#   Copyright (C) 1987 Free Software Foundation, Inc.

#This file is part of GNU CC.

#GNU CC is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY.  No author or distributor
#accepts responsibility to anyone for the consequences of using it
#or for whether it serves any particular purpose or works at all,
#unless he says so in writing.  Refer to the GNU CC General Public
#License for full details.

#Everyone is granted permission to copy, modify and redistribute
#GNU CC, but only under the conditions described in the
#GNU CC General Public License.   A copy of this license is
#supposed to have been given to you along with GNU CC so you
#can know your rights and responsibilities.  It should be in a
#file named COPYING.  Among other things, the copyright notice
#and this notice must be preserved on all copies.


CFLAGS = -g -I.
CC = cc

# How to link with obstack
OBSTACK=obstack.o
# Dependency on obstack
OBSTACK1=obstack.o

LIBS = $(OBSTACK) $(CLIB)
DIR = ../gcc

OBJS = toplev.o parse.tab.o tree.o print-tree.o \
 decl.o typecheck.o stor-layout.o fold-const.o \
 varasm.o rtl.o expr.o stmt.o expmed.o explow.o optabs.o \
 symout.o dbxout.o emit-rtl.o insn-emit.o \
 jump.o cse.o loop.o flow.o stupid.o combine.o \
 regclass.o local-alloc.o global-alloc.o reload.o reload1.o \
 final.o recog.o insn-recog.o insn-extract.o insn-output.o

# If you want to recompile everything, just do rm *.o.
# CONFIG_H = config.h tm.h
CONFIG_H =
RTL_H = rtl.h rtl.def machmode.def
TREE_H = tree.h tree.def machmode.def

all: gcc cc1 cpp

gcc: gcc.o $(OBSTACK1)
	ld -o gcc /lib/crt0.o gcc.o -lg $(LIBS) -lc

gcc.o: gcc.c $(CONFIG)
	$(CC) -c $(CFLAGS) gcc.c

cc1: $(OBJS) $(OBSTACK1)
	ld -o cc1 /lib/crt0.o $(OBJS) -lg $(LIBS) -lc

ccprof: $(OBJS) $(OBSTACK1)
	ld -o cc1 /usr/lib/gcrt0.o $(OBJS) -lg $(OBSTACK) /usr/lib/libc_p.a -lc

decl.o : decl.c $(CONFIG_H) $(TREE_H) parse.h c-tree.h
typecheck.o : typecheck.c $(CONFIG_H) $(TREE_H) c-tree.h
tree.o : tree.c $(CONFIG_H) $(TREE_H)
print-tree.o : print-tree.c $(CONFIG_H) $(TREE_H)
stor-layout.o : stor-layout.c $(CONFIG_H) $(TREE_H)
fold-const.o : fold-const.c $(CONFIG_H) $(TREE_H)
toplev.o : toplev.c $(CONFIG_H) $(TREE_H)
varasm.o : varasm.c $(CONFIG_H) $(TREE_H) $(RTL_H)

parse.tab.o : parse.tab.c $(CONFIG_H) $(TREE_H) parse.h c-tree.h

parse.tab.c : parse.y
	bison -v parse.y

rtl.o : rtl.c $(CONFIG_H) $(RTL_H)

stmt.o : stmt.c $(CONFIG_H) $(RTL_H) $(TREE_H) insn-flags.h stupid.h
expr.o : expr.c $(CONFIG_H) $(RTL_H) $(TREE_H) insn-flags.h insn-codes.h expr.h
expmed.o : expmed.c $(CONFIG_H) $(RTL_H) $(TREE_H) insn-flags.h insn-codes.h expr.h
explow.o : explow.c $(CONFIG_H) $(RTL_H) $(TREE_H) expr.h
optabs.o : optabs.c $(CONFIG_H) $(RTL_H) $(TREE_H) insn-flags.h insn-codes.h expr.h insn-config.h recog.h
symout.o : symout.c $(CONFIG_H) $(TREE_H) $(RTL_H) symseg.h
dbxout.o : symout.c $(CONFIG_H) $(TREE_H) $(RTL_H) c-tree.h

emit-rtl.o : emit-rtl.c $(CONFIG_H) $(RTL_H) regs.h recog.h insn-config.h

jump.o : jump.c $(CONFIG_H) $(RTL_H)
stupid.o : stupid.c $(CONFIG_H) $(RTL_H) regs.h  hard-reg-set.h stupid.h

cse.o : cse.c $(CONFIG_H) $(RTL_H) insn-config.h regs.h
loop.o : loop.c $(CONFIG_H) $(RTL_H) insn-config.h regs.h recog.h
flow.o : flow.c $(CONFIG_H) $(RTL_H) basic-block.h regs.h
combine.o : combine.c $(CONFIG_H) $(RTL_H) insn-config.h regs.h basic-block.h recog.h
regclass.o : regclass.c $(CONFIG_H) $(RTL_H) regs.h insn-config.h recog.h
local-alloc.o : local-alloc.c $(CONFIG_H) $(RTL_H) basic-block.h regs.h hard-reg-set.h
global-alloc.o : global-alloc.c $(CONFIG_H) $(RTL_H) basic-block.h regs.h hard-reg-set.h insn-config.h

reload.o : reload.c $(CONFIG_H) $(RTL_H) reload.h recog.h hard-reg-set.h insn-config.h regs.h
reload1.o : reload1.c $(CONFIG_H) $(RTL_H) reload.h regs.h hard-reg-set.h insn-config.h basic-block.h
final.o : final.c $(CONFIG_H) $(RTL_H) regs.h recog.h conditions.h
recog.o : recog.c $(CONFIG_H) $(RTL_H) regs.h recog.h hard-reg-set.h insn-config.h

# Now the source files that are generated from the machine description.

insn-config.h : md genconfig
	genconfig md > insn-config.h

insn-flags.h : md genflags
	genflags md > insn-flags.h

insn-codes.h : md gencodes
	gencodes md > insn-codes.h

insn-emit.o : insn-emit.c $(RTL_H) insn-config.h
	$(CC) -c $(CFLAGS) insn-emit.c

insn-emit.c : md genemit
	genemit md > insn-emit.c

insn-recog.o : insn-recog.c $(CONFIG_H) $(RTL_H) insn-config.h
	$(CC) -c $(CFLAGS) insn-recog.c

insn-recog.c : md genrecog
	genrecog md > insn-recog.c

insn-extract.o : insn-extract.c $(RTL_H)
	$(CC) -c $(CFLAGS) insn-extract.c

insn-extract.c : md genextract
	genextract md > insn-extract.c

insn-output.o : insn-output.c $(CONFIG_H) $(RTL_H) regs.h insn-config.h insn-flags.h conditions.h aux-output.c
	$(CC) -c $(CFLAGS) insn-output.c

insn-output.c : md genoutput
	genoutput md > insn-output.c

# Now the programs that generate those files.
# rtl.o is omitted as a dependency so that rtl.c can be recompiled
# to fix its tables without forcing us to regenerate insn-*.c
# and recompile them and regenerate insn-flags.h and recompile
# everything that depends on it.

genconfig : genconfig.o $(OBSTACK1)
	$(CC) -o genconfig -g genconfig.o rtl.o $(LIBS)

genconfig.o : genconfig.c rtl.def
	$(CC) -c $(CFLAGS) genconfig.c

genflags : genflags.o $(OBSTACK1)
	$(CC) -o genflags -g genflags.o rtl.o $(LIBS)

genflags.o : genflags.c rtl.def
	$(CC) -c $(CFLAGS) genflags.c

gencodes : gencodes.o $(OBSTACK1)
	$(CC) -o gencodes -g gencodes.o rtl.o $(LIBS)

gencodes.o : gencodes.c rtl.def
	$(CC) -c $(CFLAGS) gencodes.c

genemit : genemit.o $(OBSTACK1)
	$(CC) -o genemit -g genemit.o rtl.o $(LIBS)

genemit.o : genemit.c rtl.def
	$(CC) -c $(CFLAGS) genemit.c

genrecog : genrecog.o $(OBSTACK1)
	$(CC) -o genrecog -g genrecog.o rtl.o $(LIBS)

genrecog.o : genrecog.c rtl.def
	$(CC) -c $(CFLAGS) genrecog.c

genextract : genextract.o $(OBSTACK1)
	$(CC) -o genextract -g genextract.o rtl.o $(LIBS)

genextract.o : genextract.c rtl.def
	$(CC) -c $(CFLAGS) genextract.c

genoutput : genoutput.o $(OBSTACK1)
	$(CC) -o genoutput -g genoutput.o rtl.o $(LIBS)

genoutput.o : genoutput.c rtl.def
	$(CC) -c $(CFLAGS) genoutput.c

# Making the preprocessor
cpp: cccp
	-rm -f cpp
	ln cccp cpp
cccp: cccp.o y.tab.o
	cc -o cccp -g cccp.o y.tab.o
y.tab.o: y.tab.c
y.tab.c: cexp.y
	echo expect 40 shift/reduce conflicts
	yacc cexp.y
cccp.o: cccp.c

clean:
	-rm *.o
	-rm parse.tab.c insn-flags.h insn-config.h insn-codes.h
	-rm insn-output.c insn-recog.c insn-emit.c insn-extract.c
	-rm genemit genoutput genrecog genextract genflags gencodes genconfig
	-rm *.s *.s[0-9] *.co *.greg *.lreg *.combine *.flow *.cse *.jump *.rtl *.tree *.loop
	-rm parse.output core

# do make -f ../gcc/Makefile maketest DIR=../gcc
# in the intended test directory to make it a suitable test directory.
maketest:
	ln -s $(DIR)/*.[chy] .
	ln -s $(DIR)/*.def .
	ln -s $(DIR)/*.md .
	ln -s $(DIR)/.gdbinit .
	-ln -s $(DIR)/bison.simple .
	ln -s $(DIR)/gcc .
	ln -s $(DIR)/Makefile test-Makefile
	-rm tm.h aux-output.c
	make -f test-Makefile clean
# You must create the necessary links tm.h, md and aux-output.c