/* Top level of GNU C compiler
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


/* This is the top level of cc1.
   It parses command args, opens files, invokes the various passes
   in the proper order, and counts the time used by each.
   Error messages and low-level interface to malloc also handled here.  */

#include "config.h"
#include <stdio.h>
#include <signal.h>
#include <strings.h>
#include <sys/time.h>
#include <sys/resource.h>
#ifndef _TYPES_
#include <sys/types.h>
#endif
#include <sys/stat.h>
#include "tree.h"
#include "c-tree.h"
#include "rtl.h"

extern void dump_tree ();
extern int yydebug;

extern FILE *finput;

extern void init_lex ();
extern void init_decl_processing ();
extern void init_tree ();
extern void init_rtl ();
extern rtx expand_function ();
extern void init_optabs ();
extern void dump_flow_info ();
extern void dump_local_alloc ();

/* Bit flags that specify the machine subtype we are compiling for.
   Bits are tested using macros TARGET_... defined in the tm-...h file
   and set by `-m...' switches.  */

int target_flags;

/* Name of current real source file (what was input to cpp).
   # commands in the input that specify file names
   change this value.  */

extern char *input_filename;

/* Current line number in real source file.  */

extern int lineno;

/* FUNCTION_DECL for function now being parsed or compiled.  */

extern tree current_function_decl;

/* Name to use as base of names for dump output files.  */

char *dump_base_name;

/* Flags saying which kinds of debugging dump have been requested.  */

int tree_dump = 0;
int rtl_dump = 0;
int rtl_dump_and_exit = 0;
int jump_opt_dump = 0;
int cse_dump = 0;
int loop_dump = 0;
int flow_dump = 0;
int combine_dump = 0;
int local_reg_dump = 0;
int global_reg_dump = 0;

/* 1 => write gdb debugging output (using symout.c).  -g
   2 => write dbx debugging output (using dbxout.c).  -G  */

int write_symbols = 0;

/* Nonzero means do optimizations.  -opt.  */

int optimize = 0;

/* Nonzero for -optforcemem: load memory value into a register
   before arithmetic on it.  This makes better cse but slower compilation.  */

int force_mem = 0;

/* Nonzero for -optforcemem: load memory address into a register before
   reference to memory.  This makes better cse but slower compilation.  */

int force_addr = 0;

/* Nonzero means do stupid register allocation.  -noreg.
   This an OPTIMIZE are controlled by different switches in cc1,
   but normally cc controls them both with the -O switch.  */

int obey_regdecls = 0;

/* Don't print functions as they are compiled and don't print
   times taken by the various passes.  -quiet.  */

int quiet_flag = 0;

/* Don't print warning messages.  -w.  */

int inhibit_warnings = 0;

/* Number of error messages and warning messages so far.  */

int errorcount = 0;
int warningcount = 0;

/* Nonzero for -pedantic switch: warn about anything
   that standard C forbids.  */

int pedantic = 0;

/* Name for output file of assembly code, specified with -o.  */

char *asm_file_name;

/* Name for output file of GDB symbol segment, specified with -symout.  */

char *sym_file_name;

/* Output files for assembler code (real compiler output)
   and debugging dumps.  */

FILE *asm_out_file;
FILE *tree_dump_file;
FILE *rtl_dump_file;
FILE *jump_opt_dump_file;
FILE *cse_dump_file;
FILE *loop_dump_file;
FILE *flow_dump_file;
FILE *combine_dump_file;
FILE *local_reg_dump_file;
FILE *global_reg_dump_file;

/* Time accumulators, to count the total time spent in various passes.  */

int parse_time;
int varconst_time;
int expand_time;
int jump_time;
int cse_time;
int loop_time;
int flow_time;
int combine_time;
int local_alloc_time;
int global_alloc_time;
int final_time;
int symout_time;
int dump_time;

/* Return time used so far, in microseconds.  */

gettime ()
{
  struct rusage rusage;
  if (quiet_flag)
    return 0;
  getrusage (0, &rusage);
  return (rusage.ru_utime.tv_sec * 1000000 + rusage.ru_utime.tv_usec
	  + rusage.ru_stime.tv_sec * 1000000 + rusage.ru_stime.tv_usec);
}

#define TIMEVAR(VAR, BODY)    \
 { int otime = gettime (); BODY; VAR += gettime () - otime; }

print_time (str, total)
     char *str;
     int total;
{
  printf ("time in %s: %d.%06d\n", str, total / 1000000, total % 1000000);
}

/* Count an error or warning.  Return 1 if the message should be printed.  */

int
count_error (warningp)
     int warningp;
{
  if (warningp && inhibit_warnings)
    return 0;

  if (warningp)
    warningcount++;
  else
    errorcount++;

  /* If we are printing function names, make sure error message
     starts at beginning of line.  */
  if (!quiet_flag)
    fprintf (stderr, "\n");

  return 1;
}

/* Print a fatal error message.  NAME is the text.
   Also include a system error message based on `errno'.  */

int
pfatal_with_name (name)
{
  fprintf (stderr, "cc1: ");
  perror (name);
  exit (35);
}

void
fatal (s)
     char *s;
{
  yyerror (s, 0);
  exit (34);
}

/* Called when the start of a function definition is parsed,
   this function prints on stderr the name of the function.  */

void
announce_function (decl)
     tree decl;
{
  if (! quiet_flag)
    {
      fprintf (stderr, " %s", IDENTIFIER_POINTER (DECL_NAME (decl)));
      fflush (stderr);
    }
}

static tree last_error_function;

/* Report an error at the current line number.
   S and V are a string and an arg for `printf'.  */

void
yyerror (s, v)
     char *s;
     int v;			/* @@also used as pointer */
{
  yylineerror (lineno, s, v);
}

/* Report an error at line LINE.
   S and V are a string and an arg for `printf'.  */

yylineerror (line, s, v)
     int line;
     char *s;
     int v;
{
  count_error (0);

  if (last_error_function != current_function_decl)
    {
      fprintf (stderr, "In function %s:\n",
	       IDENTIFIER_POINTER (DECL_NAME (current_function_decl)));
      last_error_function = current_function_decl;
    }
  fprintf (stderr, "%s:%d: ", input_filename, line);
  fprintf (stderr, s, v);
  fprintf (stderr, "\n");
}

/* Report a warning at the current line number.
   S and V are a string and an arg for `printf'.  */

void
warning (s, v)
     char *s;
     int v;			/* @@also used as pointer */
{
  warning_with_line (lineno, s, v);
}

/* Report a warning at line LINE.
   S and V are a string and an arg for `printf'.  */

warning_with_line (line, s, v)
     int line;
     char *s;
     int v;
{
  if (count_error (1) == 0)
    return;

  if (last_error_function != current_function_decl)
    {
      fprintf (stderr, "In function %s:\n",
	       IDENTIFIER_POINTER (DECL_NAME (current_function_decl)));
      last_error_function = current_function_decl;
    }
  fprintf (stderr, "%s:%d: ", input_filename, line);

  fprintf (stderr, "warning: ");
  fprintf (stderr, s, v);
  fprintf (stderr, "\n");
}

/* When `malloc.c' is compiled with `rcheck' defined,
   it calls this function to report clobberage.  */

botch (s)
{
  abort ();
}

/* Same as `malloc' but report error if no memory available.  */

xmalloc (size)
     unsigned size;
{
  register int value = (int) malloc (size);
  if (value == 0)
    fatal ("Virtual memory exhausted.");
  return value;
}

/* Same as `realloc' but report error if no memory available.  */

int
xrealloc (ptr, size)
     char *ptr;
     int size;
{
  int result = realloc (ptr, size);
  if (!result)
    abort ();
  return result;
}

/* Return the logarithm of X, base 2, considering X unsigned,
   if X is a power of 2.  Otherwise, returns -1.  */

int
exact_log2 (x)
     register unsigned int x;
{
  register int log = 0;
  for (log = 0; log < HOST_BITS_PER_INT; log++)
    if (x == (1 << log))
      return log;
  return -1;
}

/* Given X, an unsigned number, return the largest int Y such that 2**Y <= X.
   If X is 0, return -1.  */

int
floor_log2 (x)
     register unsigned int x;
{
  register int log = 0;
  for (log = 0; log < HOST_BITS_PER_INT; log++)
    if ((x & ((-1) << log)) == 0)
      return log - 1;
  return HOST_BITS_PER_INT - 1;
}

/* Compile an entire file of output from cpp, named NAME.
   Write a file of assembly output and various debugging dumps.  */

static void
compile_file (name)
     char *name;
{
  tree globals;
  int start_time;
  int dump_base_name_length = strlen (dump_base_name);

  parse_time = 0;
  varconst_time = 0;
  expand_time = 0;
  jump_time = 0;
  cse_time = 0;
  loop_time = 0;
  flow_time = 0;
  combine_time = 0;
  local_alloc_time = 0;
  global_alloc_time = 0;
  final_time = 0;
  symout_time = 0;
  dump_time;

  /* Open input file.  */

  finput = fopen (name, "r");
  if (finput == 0)
    pfatal_with_name (name);

  /* Initialize data in various passes.  */

  init_tree ();
  init_lex ();
  init_rtl ();
  init_decl_processing ();
  init_optabs ();

  /* If tree dump desired, open the output file.  */
  if (tree_dump)
    {
      register char *dumpname = (char *) xmalloc (dump_base_name_length + 6);
      strcpy (dumpname, dump_base_name);
      strcat (dumpname, ".tree");
      tree_dump_file = fopen (dumpname, "w");
      if (tree_dump_file == 0)
	pfatal_with_name (dumpname);
    }

  /* If rtl dump desired, open the output file.  */
  if (rtl_dump)
    {
      register char *dumpname = (char *) xmalloc (dump_base_name_length + 6);
      strcpy (dumpname, dump_base_name);
      strcat (dumpname, ".rtl");
      rtl_dump_file = fopen (dumpname, "w");
      if (rtl_dump_file == 0)
	pfatal_with_name (dumpname);
    }

  /* If jump_opt dump desired, open the output file.  */
  if (jump_opt_dump)
    {
      register char *dumpname = (char *) xmalloc (dump_base_name_length + 6);
      strcpy (dumpname, dump_base_name);
      strcat (dumpname, ".jump");
      jump_opt_dump_file = fopen (dumpname, "w");
      if (jump_opt_dump_file == 0)
	pfatal_with_name (dumpname);
    }

  /* If cse dump desired, open the output file.  */
  if (cse_dump)
    {
      register char *dumpname = (char *) xmalloc (dump_base_name_length + 6);
      strcpy (dumpname, dump_base_name);
      strcat (dumpname, ".cse");
      cse_dump_file = fopen (dumpname, "w");
      if (cse_dump_file == 0)
	pfatal_with_name (dumpname);
    }

  /* If loop dump desired, open the output file.  */
  if (loop_dump)
    {
      register char *dumpname = (char *) xmalloc (dump_base_name_length + 6);
      strcpy (dumpname, dump_base_name);
      strcat (dumpname, ".loop");
      loop_dump_file = fopen (dumpname, "w");
      if (loop_dump_file == 0)
	pfatal_with_name (dumpname);
    }

  /* If flow dump desired, open the output file.  */
  if (flow_dump)
    {
      register char *dumpname = (char *) xmalloc (dump_base_name_length + 6);
      strcpy (dumpname, dump_base_name);
      strcat (dumpname, ".flow");
      flow_dump_file = fopen (dumpname, "w");
      if (flow_dump_file == 0)
	pfatal_with_name (dumpname);
    }

  /* If combine dump desired, open the output file.  */
  if (combine_dump)
    {
      register char *dumpname = (char *) xmalloc (dump_base_name_length + 10);
      strcpy (dumpname, dump_base_name);
      strcat (dumpname, ".combine");
      combine_dump_file = fopen (dumpname, "w");
      if (combine_dump_file == 0)
	pfatal_with_name (dumpname);
    }

  /* If local_reg dump desired, open the output file.  */
  if (local_reg_dump)
    {
      register char *dumpname = (char *) xmalloc (dump_base_name_length + 6);
      strcpy (dumpname, dump_base_name);
      strcat (dumpname, ".lreg");
      local_reg_dump_file = fopen (dumpname, "w");
      if (local_reg_dump_file == 0)
	pfatal_with_name (dumpname);
    }

  /* If global_reg dump desired, open the output file.  */
  if (global_reg_dump)
    {
      register char *dumpname = (char *) xmalloc (dump_base_name_length + 6);
      strcpy (dumpname, dump_base_name);
      strcat (dumpname, ".greg");
      global_reg_dump_file = fopen (dumpname, "w");
      if (global_reg_dump_file == 0)
	pfatal_with_name (dumpname);
    }

  /* Open assembler code output file.  */
 
  {
    register char *dumpname = (char *) xmalloc (dump_base_name_length + 6);
    strcpy (dumpname, dump_base_name);
    strcat (dumpname, ".s");
    asm_out_file = fopen (asm_file_name ? asm_file_name : dumpname, "w");
    if (asm_out_file == 0)
      pfatal_with_name (asm_file_name ? asm_file_name : dumpname);
  }

  input_filename = name;

  /* the beginning of the file is a new line; check for # */
  /* With luck, we discover the real source file's name from that
     and put it in input_filename.  */
  check_newline ();

  /* If GDB symbol table desired, open the GDB symbol output file.  */
  if (write_symbols == 1)
    {
      register char *dumpname = (char *) xmalloc (dump_base_name_length + 6);
      strcpy (dumpname, dump_base_name);
      strcat (dumpname, ".sym");
      if (sym_file_name == 0)
	sym_file_name = dumpname;
      symout_init (sym_file_name, asm_out_file, input_filename);
    }

  /* If dbx symbol table desired, initialize writing it
     and output the predefined types.  */
  if (write_symbols == 2)
    dbxout_init (asm_out_file, input_filename);

  /* Initialize yet another pass.  */

  init_final (input_filename);

  start_time = gettime ();

  /* Call the parser, which parses the entire file
     (calling rest_of_compilation for each function).  */

  yyparse ();

  /* Compilation is now finished except for writing
     what's left of the symbol table output.  */

  parse_time += gettime () - start_time;

  globals = getdecls ();

  /* Do dbx symbols */
  if (write_symbols == 2)
    TIMEVAR (symout_time,
	     {
	       dbxout_tags (gettags ());
	       dbxout_types (get_permanent_types ());
	     });

  /* Do gdb symbols */
  if (write_symbols == 1)
    TIMEVAR (symout_time,
	     {
	       struct stat statbuf;
	       fstat (fileno (finput), &statbuf);
	       symout_types (get_permanent_types ());
	       symout_top_blocks (globals, gettags ());
	       symout_finish (name, statbuf.st_ctime);
	     });

  /* Close non-debugging input and output files.  */

  fclose (finput);
  fclose (asm_out_file);

  if (!quiet_flag)
    fprintf (stderr,"\n");

  /* Dump the global nodes and close the tree dump file.  */
  if (tree_dump)
    {
      dump_tree (tree_dump_file, globals);
      fclose (tree_dump_file);
    }

  /* Close all other dump files.  */

  if (rtl_dump)
    fclose (rtl_dump_file);

  if (jump_opt_dump)
    fclose (jump_opt_dump_file);

  if (cse_dump)
    fclose (cse_dump_file);

  if (loop_dump)
    fclose (loop_dump_file);

  if (flow_dump)
    fclose (flow_dump_file);

  if (combine_dump)
    {
      dump_combine_total_stats (combine_dump_file);
      fclose (combine_dump_file);
    }

  if (local_reg_dump)
    fclose (local_reg_dump_file);

  if (global_reg_dump)
    fclose (global_reg_dump_file);

  /* Print the times.  */

  if (! quiet_flag)
    {
      print_time ("parse", parse_time);
      print_time ("expand", expand_time);
      print_time ("jump", jump_time);
      print_time ("cse", cse_time);
      print_time ("loop", loop_time);
      print_time ("flow", flow_time);
      print_time ("combine", combine_time);
      print_time ("local-alloc", local_alloc_time);
      print_time ("global-alloc", global_alloc_time);
      print_time ("final", final_time);
      print_time ("varconst", varconst_time);
      print_time ("symout", symout_time);
      print_time ("dump", dump_time);
    }
}

/* This is called from finish_function (within yyparse)
   after each top-level definition is parsed, and from
   finish_decl (also within yyparse) for each other top-level declaration.
   It is supposed to compile that function or variable
   and output the assembler code for it.
   After we return, the tree storage is freed.  */

void
rest_of_compilation (decl, top_level)
     tree decl;
     int top_level;
{
  register rtx insns;
  int start_time = gettime ();
  int tem;

  /* Declarations of variables, and of functions defined elsewhere.  */

  if ((TREE_CODE (decl) == VAR_DECL
       || (TREE_CODE (decl) == FUNCTION_DECL
	   && DECL_INITIAL (decl) == 0))
      && (TREE_STATIC  (decl) || TREE_EXTERNAL (decl)))
    {
      TIMEVAR (varconst_time,
	       {
		 assemble_variable (decl, top_level);
		 if (write_symbols == 2)
		   dbxout_symbol (decl, 0);
	       });
    }

  /* Function definitions are the real work
     (all the rest of this function).  */

  else if (TREE_CODE (decl) == FUNCTION_DECL
	   && DECL_INITIAL (decl))
    {
      /* Dump the function's tree if we are dumping trees.  */

      if (tree_dump)
	TIMEVAR (dump_time,
		 dump_tree (tree_dump_file, decl));

      /* Output some preliminaries for assembler.  */

      TIMEVAR (varconst_time, assemble_function (decl));

      /* Generate rtl code for this function (see stmt.c, expr.c).  */

      TIMEVAR (expand_time,
	       {
		 init_emit (write_symbols);
		 insns = expand_function (decl, !optimize);
	       });

      /* Dump the rtl code if we are dumping rtl.  */

      if (rtl_dump)
	TIMEVAR (dump_time,
		 {
		   fprintf (rtl_dump_file, "\n;; Function %s\n\n",
			    IDENTIFIER_POINTER (DECL_NAME (decl)));
		   print_rtl (rtl_dump_file, insns);
		   fflush (rtl_dump_file);
		 });

      if (rtl_dump_and_exit)
	goto exit_rest_of_compilation;

      /* Do jump optimization the first time, if -opt.  */

      if (optimize)
	TIMEVAR (jump_time, jump_optimize (insns, 0));

      /* Dump rtl code after jump, if we are doing that.  */

      if (jump_opt_dump)
	TIMEVAR (dump_time,
		 {
		   fprintf (jump_opt_dump_file, "\n;; Function %s\n\n",
			    IDENTIFIER_POINTER (DECL_NAME (decl)));
		   print_rtl (jump_opt_dump_file, insns);
		   fflush (jump_opt_dump_file);
		 });

      /* Perform common subexpression elimination.
	 Nonzero value from `cse_main' means that jumps were simplified
	 and some code may now be unreachable, so do
	 jump optimization again.  */

      if (optimize)
	{
	  TIMEVAR (cse_time, reg_scan (insns, max_reg_num ()));

	  TIMEVAR (cse_time, tem = cse_main (insns, max_reg_num ()));

	  if (tem)
	    TIMEVAR (jump_time, jump_optimize (insns, 0));
	}

      /* Dump rtl code after cse, if we are doing that.  */

      if (cse_dump)
	TIMEVAR (dump_time,
		 {
		   fprintf (cse_dump_file, "\n;; Function %s\n\n",
			    IDENTIFIER_POINTER (DECL_NAME (decl)));
		   print_rtl (cse_dump_file, insns);
		   fflush (cse_dump_file);
		 });

      /* Move constant computations out of loops.  */

      if (optimize)
	{
	  TIMEVAR (loop_time, loop_optimize (insns, max_reg_num ()));
	}

      /* Dump rtl code after loop opt, if we are doing that.  */

      if (loop_dump)
	TIMEVAR (dump_time,
		 {
		   fprintf (loop_dump_file, "\n;; Function %s\n\n",
			    IDENTIFIER_POINTER (DECL_NAME (decl)));
		   print_rtl (loop_dump_file, insns);
		   fflush (loop_dump_file);
		 });

      /* Now we choose between stupid (pcc-like) register allocation
	 (if we got the -noreg switch and not -opt)
	 and smart register allocation.  */

      if (optimize)		/* Stupid allocation probably won't work */
	obey_regdecls = 0;	/* if optimizations being done.  */

      /* Print function header into flow dump now
	 because doing the flow analysis makes some of the dump.  */

      if (flow_dump)
	TIMEVAR (dump_time,
		 {
		   fprintf (flow_dump_file, "\n;; Function %s\n\n",
			    IDENTIFIER_POINTER (DECL_NAME (decl)));
		 });

      if (obey_regdecls)
	{
	  TIMEVAR (flow_time,
		   {
		     regclass (insns, max_reg_num ());
		     stupid_life_analysis (insns, max_reg_num (),
					   flow_dump_file);
		   });
	}
      else
	{
	  /* Do control and data flow analysis,
	     and write some of the results to dump file.  */

	  TIMEVAR (flow_time, flow_analysis (insns, max_reg_num (),
					     flow_dump_file));
	}

      /* Dump rtl after flow analysis.  */

      if (flow_dump)
	TIMEVAR (dump_time,
		 {
		   print_rtl (flow_dump_file, insns);
		   fflush (flow_dump_file);
		 });

      /* If -opt, try combining insns through substitution.  */

      if (optimize)
	TIMEVAR (combine_time, combine_instructions (insns, max_reg_num ()));

      /* Dump rtl code after insn combination.  */

      if (combine_dump)
	TIMEVAR (dump_time,
		 {
		   fprintf (combine_dump_file, "\n;; Function %s\n\n",
			    IDENTIFIER_POINTER (DECL_NAME (decl)));
		   dump_combine_stats (combine_dump_file);
		   print_rtl (combine_dump_file, insns);
		   fflush (combine_dump_file);
		 });

      /* Unless we did stupid register allocation,
	 allocate pseudo-regs that are used only within 1 basic block.  */

      if (!obey_regdecls)
	TIMEVAR (local_alloc_time,
		 {
		   regclass (insns, max_reg_num ());
		   local_alloc ();
		 });

      /* Dump rtl code after allocating regs within basic blocks.  */

      if (local_reg_dump)
	TIMEVAR (dump_time,
		 {
		   fprintf (local_reg_dump_file, "\n;; Function %s\n\n",
			    IDENTIFIER_POINTER (DECL_NAME (decl)));
		   dump_flow_info (local_reg_dump_file);
		   dump_local_alloc (local_reg_dump_file);
		   print_rtl (local_reg_dump_file, insns);
		   fflush (local_reg_dump_file);
		 });

      if (global_reg_dump)
	TIMEVAR (dump_time,
		 fprintf (global_reg_dump_file, "\n;; Function %s\n\n",
			  IDENTIFIER_POINTER (DECL_NAME (decl))));

      /* Unless we did stupid register allocation,
	 allocate remaining pseudo-regs, then do the reload pass
	 fixing up any insns that are invalid.  */

      TIMEVAR (global_alloc_time,
	       {
		 if (!obey_regdecls)
		   global_alloc (global_reg_dump ? global_reg_dump_file : 0);
		 else
		   reload (insns, 0,
			   global_reg_dump ? global_reg_dump_file : 0);
	       });

      if (global_reg_dump)
	TIMEVAR (dump_time,
		 {
		   dump_global_regs (global_reg_dump_file);
		   print_rtl (global_reg_dump_file, insns);
		   fflush (global_reg_dump_file);
		 });

      /* One more attempt to remove jumps to .+1
	 left by dead-store-elimination.
	 Also do cross-jumping this time.  */

      if (optimize)
	TIMEVAR (jump_time, jump_optimize (insns, 1));

      /* Now turn the rtl into assembler code.  */

      TIMEVAR (final_time,
	       {
		 final (insns, asm_out_file,
			IDENTIFIER_POINTER (DECL_NAME (decl)),
			write_symbols, optimize);
		 fflush (asm_out_file);
	       });

      /* Write GDB symbols if requested */

      if (write_symbols == 1)
	TIMEVAR (symout_time,
		 {
		   symout_types (get_permanent_types ());
		   symout_types (get_temporary_types ());

		   DECL_BLOCK_SYMTAB_ADDRESS (decl)
		     = symout_function (DECL_INITIAL (decl),
					DECL_ARGUMENTS (decl), 0);
		 });

      /* Write DBX symbols if requested */

      if (write_symbols == 2)
	TIMEVAR (symout_time, dbxout_function (decl));
    }

 exit_rest_of_compilation:

  /* The parsing time is all the time spent in yyparse
     *except* what is spent in this function.  */

  parse_time -= gettime () - start_time;
}

/* Entry point of cc1.  Decode command args, then call compile_file.
   Exit code is 34 if fatal error, else 33 if have error messages,
   else 1 if have warnings, else 0.  */

int
main (argc, argv, envp)
     int argc;
     char **argv;
     char **envp;
{
  register int i;
  char *filename;

  target_flags = 0;
  set_target_switch ("");

  for (i = 1; i < argc; i++)
    if (argv[i][0] == '-')
      {
	register char *str = argv[i] + 1;
	if (str[0] == 'Y')
	  str++;

	if (str[0] == 'm')
	  set_target_switch (&str[1]);
	else if (!strcmp (str, "dumpbase"))
	  {
	    dump_base_name = argv[++i];
	  }
	else if (str[0] == 'd')
	  {
	    register char *p = &str[1];
	    while (*p)
	      switch (*p++)
		{
		case 'c':
		  combine_dump = 1;
		  break;
		case 'f':
		  flow_dump = 1;
		  break;
		case 'g':
		  global_reg_dump = 1;
		  break;
		case 'j':
		  jump_opt_dump = 1;
		  break;
		case 'l':
		  local_reg_dump = 1;
		  break;
		case 'L':
		  loop_dump = 1;
		  break;
		case 'r':
		  rtl_dump = 1;
		  break;
		case 's':
		  cse_dump = 1;
		  break;
		case 't':
		  tree_dump = 1;
		  break;
		case 'y':
		  yydebug = 1;
		  break;
		}
	  }
	else if (!strcmp (str, "quiet"))
	  quiet_flag = 1;
	else if (!strcmp (str, "opt"))
	  optimize = 1;
	else if (!strcmp (str, "optforcemem"))
	  force_mem = 1;
	else if (!strcmp (str, "optforceaddr"))
	  force_addr = 1;
	else if (!strcmp (str, "noreg"))
	  obey_regdecls = 1;
	else if (!strcmp (str, "w"))
	  inhibit_warnings = 1;
	else if (!strcmp (str, "g"))
	  write_symbols = 1;
	else if (!strcmp (str, "G"))
	  write_symbols = 2;
	else if (!strcmp (str, "symout"))
	  {
	    if (write_symbols == 0)
	      write_symbols = 1;
	    sym_file_name = argv[++i];
	  }
	else if (!strcmp (str, "o"))
	  {
	    asm_file_name = argv[++i];
	  }
	else
	  yylineerror (0, "Invalid switch, %s.", argv[i]);
      }
    else
      filename = argv[i];

  if (filename == 0)
    fatal ("no input file specified");

  if (dump_base_name == 0)
    dump_base_name = filename;
  compile_file (filename);

  if (errorcount)
    return 33;
  else
    return (warningcount > 0);
}

/* Decode -m switches.  */

/* Here is a table, controlled by the tm-...h file, listing each -m switch
   and which bits in `target_switches' it should set or clear.
   If VALUE is positive, it is bits to set.
   If VALUE is negative, -VALUE is bits to clear.
   (The sign bit is not used so there is no confusion.)  */

struct {char *name; int value;} target_switches []
  = TARGET_SWITCHES;

/* Decode the switch -mNAME.  */

set_target_switch (name)
     char *name;
{
  register int j = 0;
  for (j = 0; j < sizeof target_switches / sizeof target_switches[0]; j++)
    if (!strcmp (target_switches[j].name, name))
      {
	if (target_switches[j].value < 0)
	  target_flags &= ~-target_switches[j].value;
	else
	  target_flags |= target_switches[j].value;
	break;
      }
}
