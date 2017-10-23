/* Compiler driver program that can handle many languages.
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


/* This program is the user interface to the C compiler and possibly to
other compilers.  It is used because compilation is a complicated procedure
which involves running several programs and passing temporary files between
them, forwarding the users switches to those programs selectively,
and deleting the temporary files at the end.

CC recognizes how to compile each input file by suffixes in the file names.
Once it knows which kind of compilation to perform, the procedure for
compilation is specified by a string called a "spec".

Specs are strings containing lines, each of which (if not blank)
is made up of a program name, and arguments separated by spaces.
The program name must be exact and start from root, since no path
is searched and it is unreliable to depend on the current working directory.
Redirection of input or output is not supported; the subprograms must
accept filenames saying what files to read and write.

In addition, the specs can contain %-sequences to substitute variable text
or for conditional text.  Here is a table of all defined %-sequences.
Note that spaces are not generated automatically around the results of
expanding these sequences; therefore, you can concatenate them together
or with constant text in a single argument.

 %%	substitute one % into the program name or argument.
 %i     substitute the name of the input file being processed.
 %b     substitute the basename of the input file being processed.
	This is the substring up to (and not including) the last period.
 %g     substitute the temporary-file-name-base.  This is a string chosen
	once per compilation.  Different temporary file names are made by
	concatenation of constant strings on the end, as in `%g.s'.
	%g also has the same effect of %d.
 %d	marks the argument containing or following the %d as a
	temporary file name, so that that file will be deleted at the
	end of CC.  Unlike %g, this contributes no text to the argument.
 %w	marks the argument containing or following the %w as the
	"output file" of this compilation.  This puts the argument
	into the sequence of arguments that %o will substitute later.
 %o	substitutes the names of all the output files, with spaces
	automatically placed around them.  You should write spaces
	around the %o as well or the results are undefined.
	%o is for use in the specs for running the linker.
	Input files whose names have no recognized suffix are not compiled
	at all, but they are included among the output files, so they will
	be linked.
 %p	substitutes the standard macro predefinitions for the
	current target machine.  Use this when running cpp.
 %{S}   substitutes the -S switch, if that switch was given to CC.
	If that switch was not specified, this substitutes nothing.
 %{S*}  substitutes all the switches specified to CC whose names start
	with -S.  This is used for -o, -D, -I, etc; switches that take
	arguments.  CC considers `-o foo' as being one switch whose
	name starts with `o'.  %{o*} would substitute this text,
	including the space; thus, two arguments would be generated.
 %{S:X} substitutes X, but only if the -S switch was given to CC.
 %{!S:X} substitutes X, but only if the -S switch was NOT given to CC.

The conditional text X in a %{S:X} or %{!S:X} construct may contain
other nested % constructs or spaces, or even newlines.
They are processed as usual, as described above.

Note that it is built into CC which switches take arguments and which
do not.  You might think it would be useful to generalize this to
allow each compiler's spec to say which switches take arguments.  But
this cannot be done in a consistent fashion.  CC cannot even decide
which input files have been specified without knowing which switches
take arguments, and it must know which input files to compile in order
to tell which compilers to run.

CC also knows implicitly that arguments starting in `-l' are to
be treated as output files, and passed to the linker in their proper
position among the other output files.

*/

/* This defines which switches take arguments.  */

#define SWITCH_TAKES_ARG(CHAR)      \
  (CHAR == 'D' || CHAR == 'U' || CHAR == 'o' || CHAR == 'e' || CHAR == 'T'  \
   || CHAR == 'u' || CHAR == 'I' || CHAR == 'Y' || CHAR == 'd' || CHAR == 'm')

#include <stdio.h>
#include <sys/wait.h>
#include <signal.h>
#include <sys/file.h>
#include "obstack.h"
#include "config.h"

#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free free
extern int xmalloc ();
extern void free ();

/* If a stage of compilation returns an exit status >= 32,
   compilation of that file ceases.  */

#define MIN_FATAL_STATUS 32

/* This is the obstack which we use to allocate many strings.  */

struct obstack obstack;

char *handle_braces ();
char *save_string ();
char *concat ();
int do_spec ();
int do_spec_1 ();
int give_string ();

/* This structure says how to run one compiler, and when to do so.  */

struct compiler
{
  char *suffix;			/* Use this compiler for input files
				   whose names end in this suffix.  */
  char *spec;			/* To use this compiler, pass this spec
				   to do_spec.  */
};

/* Here are the specs for compiling files with various known suffixes.
   A file that does not end in any of these suffixes will be passed
   unchanged to the loader, but that is all.  */

struct compiler compilers[] =
{
	/* Note that we use the "cc1" from $PATH. */
  {".c",
   "cpp %{C} %p %{pedantic} %{D*} %{U*} %{I*} %i %{!E:%g.cpp}\n\
%{!E:cc1 %g.cpp -quiet -dumpbase %i %{Y*} %{d*} %{m*} %{w} %{pedantic}\
		     %{O:-opt}%{!O:-noreg}\
		     %{g:-G}\
		     -o %{S:%b}%{!S:%g}.s\n\
     %{!S:as %{R} -o %{!c:%d}%w%b.o %g.s\n }}"},
  {".s",
   "%{!S:as %{R} %i -o %{!c:%d}%w%b.o\n }"},
  /* Mark end of table */
  {0, 0}
};

/* Here is the spec for running the linker, after compiling all files.  */
char *link_spec = "%{!c:%{!E:%{!S:ld %{o*}\
 %{A} %{d} %{e*} %{M} %{N} %{n} %{r} %{s} %{S} %{T*} %{t} %{u*} %{X} %{x} %{z}\
 /lib/crt0.o %o %{g:-lg} -lc\n }}}";

/* Record the names of temporary files we tell compilers to write,
   and delete them at the end of the run.  */

/* This is the common prefix we use to make temp file names.
   It is chosen once for each run of this program.
   It is substituted into a spec by %g.
   Thus, all temp file names contain this prefix.
   In practice, all temp file names start with this prefix.
   The prefix starts with `/tmp'.  */

char *temp_filename;

/* Length of the prefix.  */

int temp_filename_length;

/* Define the list of temporary files to delete.  */

struct temp_file
{
  char *name;
  struct temp_file *next;
};

struct temp_file *temp_file_queue;

/* Record FILENAME as a file to be deleted automatically.  */

void
record_temp_file (filename)
     char *filename;
{
  register struct temp_file *temp;
  register char *name;
  temp = (struct temp_file *) xmalloc (sizeof (struct temp_file));
  name = (char *) xmalloc (strlen (filename) + 1);
  strcpy (name, filename);
  temp->next = temp_file_queue;
  temp->name = name;
  temp_file_queue = temp;
}

/* Delete all the temporary files whose names we previously recorded.  */

void
delete_temp_files ()
{
  register struct temp_file *temp;
  for (temp = temp_file_queue; temp; temp = temp->next)
    {
#ifdef DEBUG
      int i;
      printf ("Delete %s? (y or n) ", temp->name);
      fflush (stdout);
      i = getchar ();
      if (i != '\n')
	while (getchar () != '\n') ;
      if (i == 'y' || i == 'Y')
#endif /* DEBUG */
	unlink (temp->name);
    }
  temp_file_queue = 0;
}

/* Compute a string to use as the base of all temporary file names.
   It is substituted for %g.  */

void
choose_temp_base ()
{
  register char *foo = "/tmp/ccXXXXXX";
  temp_filename = (char *) xmalloc (strlen (foo) + 1);
  strcpy (temp_filename, foo);
  mktemp (temp_filename);
  temp_filename_length = strlen (temp_filename);
}

/* Accumulate a command (program name and args), and run it.  */

/* Vector of pointers to arguments in the current line of specifications.  */

char **argbuf;

/* Number of elements allocated in argbuf.  */

int argbuf_length;

/* Number of elements in argbuf currently in use (containing args).  */

int argbuf_index;

/* Flag indicating whether we should print the command and arguments */

unsigned char vflag;

/* User-specified prefix to attach to command names,
   or 0 if none specified.  */

char *user_exec_prefix = 0;

/* Default prefixes to attach to command names.  */

char *standard_exec_prefix = "/usr/local/lib/gcc-";
char *standard_exec_prefix_1 = "/usr/lib/gcc-";

/* Clear out the vector of arguments (after a command is executed).  */

void
clear_args ()
{
  argbuf_index = 0;
}

/* Add one argument to the vector at the end.
   This is done when a space is seen or at the end of the line.  */

void
store_arg (arg, tempnamep)
     char *arg;
     int tempnamep;
{
  if (argbuf_index + 1 == argbuf_length)
    {
      argbuf = (char **) realloc (argbuf, (argbuf_length *= 2) * sizeof (char *));
    }

  argbuf[argbuf_index++] = arg;
  argbuf[argbuf_index] = 0;

  if (tempnamep)
    record_temp_file (arg);
}

/* Execute the command specified by the arguments on the current line of spec.
   Returns 0 if successful, -1 if failed.  */

int
execute ()
{
  int pid;
  union wait status;
  int size;
  char *temp;
  int win = 0;

  size = strlen (standard_exec_prefix);
  if (user_exec_prefix != 0 && strlen (user_exec_prefix) > size)
    size = strlen (user_exec_prefix);
  if (strlen (standard_exec_prefix_1) > size)
    size = strlen (standard_exec_prefix_1);
  size += strlen (argbuf[0]) + 1;
  temp = (char *) alloca (size);

  /* Determine the filename to execute.  */

  if (user_exec_prefix)
    {
      strcpy (temp, user_exec_prefix);
      strcat (temp, argbuf[0]);
      win = (access (temp, X_OK) == 0);
    }

  if (!win)
    {
      strcpy (temp, standard_exec_prefix);
      strcat (temp, argbuf[0]);
      win = (access (temp, X_OK) == 0);
    }

  if (!win)
    {
      strcpy (temp, standard_exec_prefix_1);
      strcat (temp, argbuf[0]);
      win = (access (temp, X_OK) == 0);
    }

  if (vflag)
    {
      int i;
      for (i = 0; argbuf[i]; i++)
	{
	  if (i == 0 && win)
	    fprintf (stderr, " %s", temp);
	  else
	    fprintf (stderr, " %s", argbuf[i]);
	}
      fprintf (stderr, "\n");
#ifdef DEBUG
      fprintf (stderr, "\nGo ahead? (y or n) ");
      fflush (stderr);
      i = getchar ();
      if (i != '\n')
	while (getchar () != '\n') ;
      if (i != 'y' && i != 'Y')
	return 0;
#endif				/* DEBUG */
    }

  pid = vfork ();
  if (pid < 0)
    pfatal_with_name ("vfork");
  if (pid == 0)
    {
      if (win)
	execv (temp, argbuf);
      else
	execvp (argbuf[0], argbuf);
      perror_with_name (argbuf[0]);
      _exit (65);
    }
  wait (&status);
  if (WIFSIGNALED (status))
    fatal ("Program %s got fatal signal %d.", argbuf[0], status.w_termsig);
  if ((WIFEXITED (status) && status.w_retcode >= MIN_FATAL_STATUS)
      || WIFSIGNALED (status))
    return -1;
  return 0;
}

/* Find all the switches given to us
   and make a vector describing them.
   The elements of the vector a strings, one per switch given.
   The string includes a minus sign, the switch name,
   and possibly an argument after that, with a space between them
   if that the user gave the switch its argument as two args.  */

char **switches;

int n_switches;

/* Also a vector of input files specified.  */

char **infiles;

int n_infiles;

/* And a vector of corresponding output files is made up later.  */

char **outfiles;

char *
make_switch (p1, s1, p2, s2)
     char *p1;
     int s1;
     char *p2;
     int s2;
{
  register char *new;
  if (p2 && s2 == 0)
    s2 = strlen (p2);
  new = (char *) xmalloc (s1 + s2 + 2);
  bcopy (p1, new, s1);
  if (p2)
    {
      new[s1++] = ' ';
      bcopy (p2, new + s1, s2);
    }
  new[s1 + s2] = 0;
  return new;
}

/* Create the vector `switches' and its contents.
   Store its length in `n_switches'.  */

void
process_command (argc, argv)
     int argc;
     char **argv;
{
  register int i;
  n_switches = 0;
  n_infiles = 0;

  /* Scan argv twice.  Here, the first time, just count how many switches
     there will be in their vector, and how many input files in theirs.
     Here we also parse the switches that cc itself uses (e.g. -v).  */

  for (i = 1; i < argc; i++)
    {
      if (argv[i][0] == '-' && argv[i][1] != 'l')
	{
	  register char *p = &argv[i][1];
	  register int c = *p;

	  switch (c)
	    {
	    case 'B':
	      user_exec_prefix = p + 1;
	      break;

	    case 'v':	/* Print commands as we execute them */
	      vflag++;

	    default:
	      n_switches++;

	      if (SWITCH_TAKES_ARG (c) && p[1] == 0)
		i++;
	    }
	}
      else
	n_infiles++;
    }

  /* Then create the space for the vectors and scan again.  */

  switches = (char **) xmalloc ((n_switches + 1) * sizeof (char *));
  infiles = (char **) xmalloc ((n_infiles + 1) * sizeof (char *));
  n_switches = 0;
  n_infiles = 0;

  /* This, time, copy the text of each switch and store a pointer
     to the copy in the vector of switches.
     Store all the infiles in their vector.  */

  for (i = 1; i < argc; i++)
    {
      if (argv[i][0] == '-' && argv[i][1] != 'l')
	{
	  register char *p = &argv[i][1];
	  register int c = *p;

	  if (c == 'B')
	    continue;
	  if (SWITCH_TAKES_ARG (c) && p[1] == 0)
	    switches[n_switches++] = make_switch (p, 1, argv[++i], 0);
	  else
	    switches[n_switches++] = make_switch (p, strlen (p),
						  (char *)0, 0);
	}
      else
	infiles[n_infiles++] = argv[i];
    }

  switches[n_switches] = 0;
  infiles[n_infiles] = 0;
}

/* Process a spec string, accumulating and running commands.  */

/* These variables describe the input file name.
   input_file_number is the index on outfiles of this file,
   so that the output file name can be stored for later use by %o.
   input_basename is the start of the part of the input file
   sans all directory names, and basename_length is the number
   of characters starting there excluding the suffix .c or whatever.  */

char *input_filename;
int input_file_number;
int input_filename_length;
int basename_length;
char *input_basename;

/* These are variables used within do_spec and do_spec_1.  */

/* Nonzero if an arg has been started and not yet terminated
   (with space, tab or newline).  */
int arg_going;

/* Nonzero means %d or %g has been seen; the next arg to be terminated
   is a temporary file name.  */
int delete_this_arg;

/* Nonzero means %w has been seen; the next arg to be terminated
   is the output file name of this compilation.  */
int this_is_output_file;

/* Process the spec SPEC and run the commands specified therein.
   Returns 0 if the spec is successfully processed; -1 if failed.  */

int
do_spec (spec)
     char *spec;
{
  int value;

  clear_args ();
  arg_going = 0;
  delete_this_arg = 0;
  this_is_output_file = 0;

  value = do_spec_1 (spec, 0);
  if (value == 0)
    value = do_spec_1 ("\n", 0);
  return value;
}

/* Process the sub-spec SPEC as a portion of a larger spec.
   This is like processing a whole spec except that we do
   not initialize at the beginning and we do not supply a
   newline by default at the end.
   NOPERCENT nonzero means don't process %-sequences in SPEC;
   in this case, % is treated as an ordinary character.
   This is used while substituting switches.  */

int
do_spec_1 (spec, nopercent)
     char *spec;
     int nopercent;
{
  register char *p = spec;
  register int c;
  char *string;

  while (c = *p++)
    switch (c)
      {
      case '\n':
	/* End of line: finish any pending argument,
	   then run the pending command if one has been started.  */
	if (arg_going)
	  {
	    obstack_1grow (&obstack, 0);
	    string = obstack_finish (&obstack);
	    store_arg (string, delete_this_arg);
	    if (this_is_output_file)
	      outfiles[input_file_number] = string;
	  }
	arg_going = 0;
	if (argbuf_index)
	  {
	    int value = execute ();
	    if (value)
	      return value;
	  }
	/* Reinitialize for a new command, and for a new argument.  */
	clear_args ();
	arg_going = 0;
	delete_this_arg = 0;
	this_is_output_file = 0;
	break;

      case '\t':
      case ' ':
	/* Space or tab ends an argument if one is pending.  */
	if (arg_going)
	  {
	    obstack_1grow (&obstack, 0);
	    string = obstack_finish (&obstack);
	    store_arg (string, delete_this_arg);
	    if (this_is_output_file)
	      outfiles[input_file_number] = string;
	  }
	/* Reinitialize for a new argument.  */
	arg_going = 0;
	delete_this_arg = 0;
	this_is_output_file = 0;
	break;

      case '%':
	if (! nopercent)
	  switch (c = *p++)
	    {
	    case 0:
	      fatal ("Invalid specification!  Bug in cc.");

	    case 'i':
	      obstack_grow (&obstack, input_filename, input_filename_length);
	      arg_going = 1;
	      break;

	    case 'b':
	      obstack_grow (&obstack, input_basename, basename_length);
	      arg_going = 1;
	      break;

	    case 'p':
	      do_spec_1 (CPP_PREDEFINES, 1);
	      break;

	    case 'g':
	      obstack_grow (&obstack, temp_filename, temp_filename_length);
	      delete_this_arg = 1;
	      arg_going = 1;
	      break;

	    case 'd':
	      delete_this_arg = 1;
	      break;

	    case 'w':
	      this_is_output_file = 1;
	      break;

	    case 'o':
	      {
		register int f;
		for (f = 0; f < n_infiles; f++)
		  store_arg (outfiles[f], 0);
	      }
	      break;

	    case '{':
	      p = handle_braces (p);
	      if (p == 0)
		return -1;
	      break;

	    case '%':
	      obstack_1grow (&obstack, '%');
	      break;

	    default:
	      abort ();
	    }
	else
	  {
	    obstack_1grow (&obstack, c);
	    arg_going = 1;
	  }
	break;

      default:
	/* Ordinary character: put it into the current argument.  */
	obstack_1grow (&obstack, c);
	arg_going = 1;
      }

      return 0;		/* End of string */
}

/* Return 0 if we call do_spec_1 and that returns -1.  */

char *
handle_braces (p)
     register char *p;
{
  register char *q;
  int negate = *p == '!';
  char *filter;

  if (negate) ++p;

  filter = p;
  while (*p != ':' && *p != '}') p++;
  if (*p != '}')
    {
      register int count = 1;
      q = p + 1;
      while (count > 0)
	{
	  if (*q == '{')
	    count++;
	  else if (*q == '}')
	    count--;
	  else if (*q == 0)
	    abort ();
	  q++;
	}
    }
  else
    q = p + 1;

  if (p[-1] == '*')
    {
      /* Substitute all matching switches as separate args.  */
      register int i;
      --p;
      for (i = 0; i < n_switches; i++)
	if (!strncmp (switches[i], filter, p - filter))
	  {
	    if (give_switch (i) < 0)
	      return 0;
	  }
    }
  else
    {
      /* Test for presence of the specified switch.  */
      register int i;
      int present = 0;
      for (i = 0; i < n_switches; i++)
	{
	  if (!strncmp (switches[i], filter, p - filter))
	    {
	      present = 1;
	      break;
	    }
	}
      /* If it is as desired (present for %{s...}, absent for %{-s...})
	 then substitute either the switch or the specified
	 conditional text.  */
      if (present != negate)
	{
	  if (*p == '}')
	    {
	      if (give_switch (i) < 0)
		return 0;
	    }
	  else
	    {
	      if (do_spec_1 (save_string (p + 1, q - p - 2), 0) < 0)
		return 0;
	    }
	}
    }

  return q;
}

int
give_switch (switchnum)
     int switchnum;
{
  do_spec_1 ("-", 0);
  do_spec_1 (switches[switchnum], 0);
  do_spec_1 (" ", 0);
}

/* On fatal signals, delete all the temporary files.  */

void
fatal_error (signum)
     int signum;
{
  signal (signum, SIG_DFL);
  delete_temp_files ();
  /* Get the same signal again, this time not handled,
     so its normal effect occurs.  */
  kill (getpid (), signum);
}

int
main (argc, argv)
     int argc;
     char **argv;
{
  register int i;
  int value;
  int nolink = 0;

  signal (SIGINT, fatal_error);
  signal (SIGKILL, fatal_error);

  argbuf_length = 10;
  argbuf = (char **) xmalloc (argbuf_length * sizeof (char *));

  obstack_init (&obstack);

  choose_temp_base ();

  /* Make a table of what switches there are (switches, n_switches).
     Make a table of specified input files (infiles, n_infiles).  */

  process_command (argc, argv);

  /* Make a place to record the compiler output file names
     that correspond to the input files.  */

  outfiles = (char **) xmalloc (n_infiles * sizeof (char *));
  bzero (outfiles, n_infiles * sizeof (char *));

  for (i = 0; i < n_infiles; i++)
    {
      /* First figure out which compiler from the file's suffix.  */
      
      register struct compiler *cp;

      /* Tell do_spec what to substitute for %i.  */

      input_filename = infiles[i];
      input_filename_length = strlen (input_filename);
      input_file_number = i;

      /* Use the same thing in %o, unless cp->spec says otherwise.  */

      outfiles[i] = input_filename;

      for (cp = compilers; cp->spec; cp++)
	{
	  if (strlen (cp->suffix) < input_filename_length
	      && !strcmp (cp->suffix,
			  infiles[i] + input_filename_length
			  - strlen (cp->suffix)))
	    {
	      /* Ok, we found an applicable compiler.  Run its spec.  */
	      /* First say how much of input_filename to substitute for %b  */
	      register char *p;

	      input_basename = input_filename;
	      for (p = input_filename; *p; p++)
		if (*p == '/')
		  input_basename = p + 1;
	      basename_length = (input_filename_length - strlen (cp->suffix)
				 - (input_basename - input_filename));
	      value = do_spec (cp->spec);
	      if (value < 0)
		nolink = 1;
	      break;
	    }
	}

      /* If this file's name does not contain a recognized suffix,
	 don't do anything to it, but do feed it to the link spec
	 since its name is in outfiles.  */
    }

  /* Run ld to link all the compiler output files.  */

  if (! nolink)
    do_spec (link_spec);

  /* Delete all the temporary files we made.  */

  delete_temp_files ();

  return 0;
}

xmalloc (size)
     int size;
{
  register int value = malloc (size);
  if (value == 0)
    fatal ("Virtual memory full.");
  return value;
}

xrealloc (ptr, size)
     int ptr, size;
{
  register int value = realloc (ptr, size);
  if (value == 0)
    fatal ("Virtual memory full.");
  return value;
}

fatal (msg, arg1, arg2)
     char *msg, *arg1, *arg2;
{
  error (msg, arg1, arg2);
  delete_temp_files ();
  exit (1);
}

error (msg, arg1, arg2)
     char *msg, *arg1, *arg2;
{
  fprintf (stderr, "cc: ");
  fprintf (stderr, msg, arg1, arg2);
  fprintf (stderr, "\n");
}

/* Return a newly-allocated string whose contents concatenate those of s1, s2, s3.  */

char *
concat (s1, s2, s3)
     char *s1, *s2, *s3;
{
  int len1 = strlen (s1), len2 = strlen (s2), len3 = strlen (s3);
  char *result = (char *) xmalloc (len1 + len2 + len3 + 1);

  strcpy (result, s1);
  strcpy (result + len1, s2);
  strcpy (result + len1 + len2, s3);
  *(result + len1 + len2 + len3) = 0;

  return result;
}

char *
save_string (s, len)
     char *s;
     int len;
{
  register char *result = (char *) xmalloc (len + 1);

  bcopy (s, result, len);
  result[len] = 0;
  return result;
}

pfatal_with_name (name)
     char *name;
{
  extern int errno, sys_nerr;
  extern char *sys_errlist[];
  char *s;

  if (errno < sys_nerr)
    s = concat ("", sys_errlist[errno], " for %s");
  else
    s = "cannot open %s";
  fatal (s, name);
}

perror_with_name (name)
     char *name;
{
  extern int errno, sys_nerr;
  extern char *sys_errlist[];
  char *s;

  if (errno < sys_nerr)
    s = concat ("", sys_errlist[errno], " for %s");
  else
    s = "cannot open %s";
  error (s, name);
}
