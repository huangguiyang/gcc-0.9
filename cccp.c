/* C Compatible Compiler Preprocessor (CCCP)
Copyright (C) 1986, 1987, Free Software Foundation, Inc.
                    Written by Paul Rubin, June 1986
		    Adapted to ANSI C, Richard Stallman, Jan 1987

			   NO WARRANTY

  BECAUSE THIS PROGRAM IS LICENSED FREE OF CHARGE, WE PROVIDE ABSOLUTELY
NO WARRANTY, TO THE EXTENT PERMITTED BY APPLICABLE STATE LAW.  EXCEPT
WHEN OTHERWISE STATED IN WRITING, FREE SOFTWARE FOUNDATION, INC,
RICHARD M. STALLMAN AND/OR OTHER PARTIES PROVIDE THIS PROGRAM "AS IS"
WITHOUT WARRANTY OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING,
BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
FITNESS FOR A PARTICULAR PURPOSE.  THE ENTIRE RISK AS TO THE QUALITY
AND PERFORMANCE OF THE PROGRAM IS WITH YOU.  SHOULD THE PROGRAM PROVE
DEFECTIVE, YOU ASSUME THE COST OF ALL NECESSARY SERVICING, REPAIR OR
CORRECTION.

 IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW WILL RICHARD M.
STALLMAN, THE FREE SOFTWARE FOUNDATION, INC., AND/OR ANY OTHER PARTY
WHO MAY MODIFY AND REDISTRIBUTE THIS PROGRAM AS PERMITTED BELOW, BE
LIABLE TO YOU FOR DAMAGES, INCLUDING ANY LOST PROFITS, LOST MONIES, OR
OTHER SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE
USE OR INABILITY TO USE (INCLUDING BUT NOT LIMITED TO LOSS OF DATA OR
DATA BEING RENDERED INACCURATE OR LOSSES SUSTAINED BY THIRD PARTIES OR
A FAILURE OF THE PROGRAM TO OPERATE WITH ANY OTHER PROGRAMS) THIS
PROGRAM, EVEN IF YOU HAVE BEEN ADVISED OF THE POSSIBILITY OF SUCH
DAMAGES, OR FOR ANY CLAIM BY ANY OTHER PARTY.

		GENERAL PUBLIC LICENSE TO COPY

  1. You may copy and distribute verbatim copies of this source file
as you receive it, in any medium, provided that you conspicuously
and appropriately publish on each copy a valid copyright notice
"Copyright (C) 1987, Free Software Foundation"; and include
following the copyright notice a verbatim copy of the above disclaimer
of warranty and of this License.  You may charge a distribution fee for the
physical act of transferring a copy.

  2. You may modify your copy or copies of this source file or
any portion of it, and copy and distribute such modifications under
the terms of Paragraph 1 above, provided that you also do the following:

    a) cause the modified files to carry prominent notices stating
    that you changed the files and the date of any change; and

    b) cause the whole of any work that you distribute or publish,
    that in whole or in part contains or is a derivative of this
    program or any part thereof, to be licensed at no charge to all
    third parties on terms identical to those contained in this
    License Agreement (except that you may choose to grant more
    extensive warranty protection to third parties, at your option).

    c) You may charge a distribution fee for the physical act of
    transferring a copy, and you may at your option offer warranty
    protection in exchange for a fee.

  3. You may copy and distribute this program or any portion of it in
compiled, executable or object code form under the terms of Paragraphs
1 and 2 above provided that you do the following:

    a) cause each such copy to be accompanied by the
    corresponding machine-readable source code, which must
    be distributed under the terms of Paragraphs 1 and 2 above; or,

    b) cause each such copy to be accompanied by a
    written offer, with no time limit, to give any third party
    free (except for a nominal shipping charge) a machine readable
    copy of the corresponding source code, to be distributed
    under the terms of Paragraphs 1 and 2 above; or,

    c) in the case of a recipient of this program in compiled, executable
    or object code form (without the corresponding source code) you
    shall cause copies you distribute to be accompanied by a copy
    of the written offer of source code which you received along
    with the copy you received.

  4. You may not copy, sublicense, distribute or transfer this program
except as expressly provided under this License Agreement.  Any attempt
otherwise to copy, sublicense, distribute or transfer this program is void and
your rights to use the program under this License agreement shall be
automatically terminated.  However, parties who have received computer
software programs from you with this License Agreement will not have
their licenses terminated so long as such parties remain in full compliance.

  5. If you wish to incorporate parts of this program into other free
programs whose distribution conditions are different, write to the Free
Software Foundation at 1000 Mass Ave, Cambridge, MA 02138.  We have not yet
worked out a simple rule that can be stated here, but we will often permit
this.  We will be guided by the two goals of preserving the free status of
all derivatives our free software and of promoting the sharing and reuse of
software.


 In other words, you are welcome to use, share and improve this program.
 You are forbidden to forbid anyone else to use, share and improve
 what you give them.   Help stamp out software-hoarding!  */

typedef unsigned char U_CHAR;

#ifdef EMACS
#define NO_SHORTNAMES
#include "../src/config.h"
#ifdef open
#undef open
#undef read
#undef write
#endif /* open */
#endif /* EMACS */

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <ctype.h>
#include <stdio.h>
#ifndef USG
#include <sys/time.h>		/* for __DATE__ and __TIME__ */
#else
#define index strchr
#define rindex strrchr
#include <time.h>
#include <fcntl.h>
#endif /* USG */


/* External declarations.  */

void bcopy (), bzero ();
int bcmp ();

/* Forward declarations.  */

int do_define (), do_line (), do_include (), do_undef (), do_error (),
  do_pragma (), do_if (), do_xifdef (), do_else (),
  do_elif (), do_endif ();

struct hashnode *install ();
struct hashnode *lookup ();

char *xmalloc (), *xrealloc (), *xcalloc ();
void fatal (), pfatal_with_name (), perror_with_name ();

U_CHAR *grow_outbuf ();
int handle_directive ();

U_CHAR *macarg1 ();
char *macarg ();

U_CHAR *skip_to_end_of_comment ();
U_CHAR *skip_quoted_string ();

#define FATAL_EXIT_CODE 33	/* gnu cc command understands this */

/* Name under which this program was invoked.  */

char *progname;

/* Maximum length of directory names in the search path
   for include files.  */

int max_include_len = 14;

/* Nonzero means copy comments into the output file.  */

int put_out_comments = 0;

/* Nonzero means don't process the ANSI trigraph sequences.  */

int no_trigraphs = 0;

/* Nonzero means don't output line number information.  */

int no_line_commands;

/* Nonzero means inhibit output of the preprocessed text
   and instead output the definitions of all user-defined macros
   in a form suitable for use as input to cccp.  */

int dump_macros;

/* Nonzero means give all the error messages the ANSI standard requires.  */

int pedantic;

/* Nonzero causes output not to be done,
   but directives such as #define that have side effects
   are still obeyed.  */

int no_output;

/* I/O buffer structure.
   The `fname' field is nonzero for source files and #include files
   and for the dummy text used for -D and -U.
   It is zero for rescanning results of macro expansion
   and for expanding macro arguments.  */
#define INPUT_STACK_MAX 100
struct file_buf {
  char *fname;
  int lineno;
  int length;
  U_CHAR *buf;
  U_CHAR *bufp;
  /* Macro that this level is the expansion of.
     Included so that we can reenable the macro
     at the end of this level.  */
  struct hashnode *macro;
  /* Object to be freed at end of input at this level.  */
  U_CHAR *free;
} instack[INPUT_STACK_MAX];

/* Current nesting level of input sources.
   `instack[indepth]' is the level currently being read.  */
int indepth = -1;

typedef struct file_buf FILE_BUF;

/* The output buffer.  Its LENGTH field is the amount of room allocated
   for the buffer, not the number of chars actually present.  To get
   that, subtract outbuf.buf from outbuf.bufp. */

#define OUTBUF_SIZE 10	/* initial size of output buffer */
FILE_BUF outbuf;

/* Grow output buffer OBUF points at
   so it can hold at least NEEDED more chars.  */

#define check_expand(OBUF, NEEDED)  \
  (((OBUF)->length - ((OBUF)->bufp - (OBUF)->buf) <= (NEEDED))   \
   ? grow_outbuf ((OBUF), (NEEDED)) : 0)

struct directory_stack
  {
    struct directory_stack *next;
    char *fname;
  };

/* #include "file" starts with the first entry in the stack */
/* #include <file> starts with the second. */
/* -I directories are added after the first */
struct directory_stack default_includes[2] =
  {
    { &default_includes[1], "." },
    { 0, "/usr/include" }
  };
struct directory_stack *include = &default_includes[0];

/* Structure allocated for every #define.  For a simple replacement
   such as
   	#define foo bar ,
   nargs = -1, the `pattern' list is null, and the expansion is just
   the replacement text.  Nargs = 0 means a functionlike macro with no args,
   e.g.,
       #define getchar() getc (stdin) .
   When there are args, the expansion is the replacement text with the
   args squashed out, and the reflist is a list describing how to
   build the output from the input: e.g., "3 chars, then the 1st arg,
   then 9 chars, then the 3rd arg, then 0 chars, then the 2nd arg".
   The chars here come from the expansion.  Whatever is left of the
   expansion after the last arg-occurrence is copied after that arg.
   Note that the reflist can be arbitrarily long---
   its length depends on the number of times the arguments appear in
   the replacement text, not how many args there are.  Example:
   #define f(x) x+x+x+x+x+x+x would have replacement text "++++++" and
   pattern list
     { (0, 1), (1, 1), (1, 1), ..., (1, 1), NULL }
   where (x, y) means (nchars, argno). */

typedef struct definition DEFINITION;
struct definition {
  int nargs;
  int length;			/* length of expansion string */
  U_CHAR *expansion;
  struct reflist {
    struct reflist *next;
    char stringify;		/* nonzero if this arg was preceded by a
				   # operator. */
    char raw_before;		/* Nonzero if a ## operator before arg. */
    char raw_after;		/* Nonzero if a ## operator after arg. */
    int nchars;			/* Number of literal chars to copy before
				   this arg occurrence.  */
    int argno;			/* Number of arg to substitute (origin-0) */
  } *pattern;
  /* Names of macro args, concatenated in reverse order
     with comma-space between them.
     The only use of this is that we warn on redefinition
     if this differs between the old and new definitions.  */
  U_CHAR *argnames;
};

/* different kinds of things that can appear in the value field
   of a hash node.  Actually, this may be useless now. */
union hashval {
  int ival;
  char *cpval;
  DEFINITION *defn;
};


/* The structure of a node in the hash table.  The hash table
   has entries for all tokens defined by #define commands (type T_MACRO),
   plus some special tokens like __LINE__ (these each have their own
   type, and the appropriate code is run when that type of node is seen.
   It does not contain control words like "#define", which are recognized
   by a separate piece of code. */

/* different flavors of hash nodes --- also used in keyword table */
enum node_type {
 T_DEFINE = 1,	/* the "#define" keyword */
 T_INCLUDE,	/* the "#include" keyword */
 T_IFDEF,	/* the "#ifdef" keyword */
 T_IFNDEF,	/* the "#ifndef" keyword */
 T_IF,		/* the "#if" keyword */
 T_ELSE,	/* "#else" */
 T_PRAGMA,	/* "#pragma" */
 T_ELIF,	/* "#else" */
 T_UNDEF,	/* "#undef" */
 T_LINE,	/* "#line" */
 T_ERROR,	/* "#error" */
 T_ENDIF,	/* "#endif" */
 T_SPECLINE,	/* special symbol "__LINE__" */
 T_DATE,	/* "__DATE__" */
 T_FILE,	/* "__FILE__" */
 T_TIME,	/* "__TIME__" */
 T_CONST,	/* Constant value, used by "__STDC__" */
 T_MACRO,	/* macro defined by "#define" */
 T_DISABLED,	/* macro temporarily turned off for rescan */
 T_SPEC_DEFINED /* special `defined' macro for use in #if statements */
 };

struct hashnode {
  struct hashnode *next;	/* double links for easy deletion */
  struct hashnode *prev;
  struct hashnode **bucket_hdr;	/* also, a back pointer to this node's hash
				   chain is kept, in case the node is the head
				   of the chain and gets deleted. */
  enum node_type type;		/* type of special token */
  int length;			/* length of token, for quick comparison */
  U_CHAR *name;			/* the actual name */
  union hashval value;		/* pointer to expansion, or whatever */
};

typedef struct hashnode HASHNODE;

/* Some definitions for the hash table.  The hash function MUST be
   computed as shown in hashf () below.  That is because the rescan
   loop computes the hash value `on the fly' for most tokens,
   in order to avoid the overhead of a lot of procedure calls to
   the hashf () function.  Hashf () only exists for the sake of
   politeness, for use when speed isn't so important. */

#define HASHSIZE 1403
HASHNODE *hashtab[HASHSIZE];
#define HASHSTEP(old, c) ((old << 2) + c)
#define MAKE_POS(v) (v & ~0x80000000) /* make number positive */

/* Symbols to predefine.  */

#ifdef PREDEFS
char *predefs = PREDEFS;
#else
char *predefs = "";
#endif

/* `struct directive' defines one #-directive, including how to handle it.  */

struct directive {
  int length;			/* Length of name */
  int (*func)();		/* Function to handle directive */
  char *name;			/* Name of directive */
  enum node_type type;		/* Code which describes which directive. */
};

/* Here is the actual list of #-directives, most-often-used first.  */

struct directive directive_table[] = {
  {  6, do_define, "define", T_DEFINE},
  {  2, do_if, "if", T_IF},
  {  5, do_xifdef, "ifdef", T_IFDEF},
  {  6, do_xifdef, "ifndef", T_IFNDEF},
  {  5, do_endif, "endif", T_ENDIF},
  {  4, do_else, "else", T_ELSE},
  {  4, do_elif, "elif", T_ELIF},
  {  4, do_line, "line", T_LINE},
  {  7, do_include, "include", T_INCLUDE},
  {  5, do_undef, "undef", T_UNDEF},
  {  5, do_error, "error", T_ERROR},
  {  6, do_pragma, "pragma", T_PRAGMA},
  {  -1, 0, "", (enum node_type) -1},
};

/* table to tell if char can be part of a C identifier. */
U_CHAR is_idchar[256];
/* table to tell if char can be first char of a c identifier. */
U_CHAR is_idstart[256];
/* table to tell if c is horizontal space.  isspace () thinks that
   newline is space; this is not a good idea for this program. */
U_CHAR is_hor_space[256];

#define SKIP_WHITE_SPACE(p) do { while (is_hor_space[*p]) p++; } while (0)
#define SKIP_ALL_WHITE_SPACE(p) do { while (isspace (*p)) p++; } while (0)


FILE_BUF expand_to_temp_buffer ();

DEFINITION *collect_expansion ();

int
main (argc, argv)
     int argc;
     char **argv;
{
  struct stat sbuf;
  char *in_fname, *out_fname;
  int f, i;
  FILE_BUF *fp;
  char **pend_files = (char **) alloca (argc * sizeof (char *));
  char **pend_defs = (char **) alloca (argc * sizeof (char *));
  char **pend_undefs = (char **) alloca (argc * sizeof (char *));
  int inhibit_predefs = 0;

  progname = argv[0];
  in_fname = NULL;
  out_fname = NULL;
  initialize_random_junk ();

  no_line_commands = 0;
  no_trigraphs = 1;
  dump_macros = 0;
  no_output = 0;

  bzero (pend_files, argc * sizeof (char *));
  bzero (pend_defs, argc * sizeof (char *));
  bzero (pend_undefs, argc * sizeof (char *));

  /* Process switches and find input file name.  */

  for (i = 1; i < argc; i++) {
    if (argv[i][0] != '-') {
      if (out_fname != NULL)
	fatal ("Usage: %s [switches] input output\n", argv[0]);
      else if (in_fname != NULL) {
	out_fname = argv[i];
	if (! freopen (out_fname, "w", stdout))
	  pfatal_with_name (out_fname);
      } else
	in_fname = argv[i];
    } else {
      switch (argv[i][1]) {

      case 'i':
	if (argv[i][2] != 0)
	  pend_files[i] = argv[i] + 2;
	else
	  pend_files[i] = argv[++i];
	break;

      case 'p':
	pedantic = 1;
	break;

      case 'd':
	dump_macros = 1;
	no_output = 1;
	break;

      case 'D':
	{
	  char *p, *p1;

	  if (argv[i][2] != 0)
	    p = argv[i] + 2;
	  else
	    p = argv[++i];

	  if ((p1 = (char *) index (p, '=')) != NULL)
	    *p1 = ' ';
	  pend_defs[i] = p;
	}
	break;

      case 'U':		/* JF #undef something */
	if (argv[i][2] != 0)
	  pend_undefs[i] = argv[i] + 2;
	else
	  pend_undefs[i] = argv[++i];
	break;

      case 'C':
	put_out_comments = 1;
	break;

      case 'E':			/* -E comes from cc -E; ignore it.  */
	break;

      case 'P':
	no_line_commands = 1;
	break;

      case 'T':			/* Enable ANSI trigraphs */
	no_trigraphs = 0;
	break;

      case 'I':			/* JF handle directory path right */
	{
	  struct directory_stack *dirtmp;

	  dirtmp = (struct directory_stack *)
	    xmalloc (sizeof (struct directory_stack));
	  dirtmp->next = include->next;
	  include->next = dirtmp;
	  if (argv[i][2] != 0)
	    dirtmp->fname = argv[i] + 2;
	  else
	    dirtmp->fname = argv[++i];
	  include = dirtmp;
	  if (strlen (dirtmp->fname) > max_include_len)
	    max_include_len = strlen (dirtmp->fname);
	}
	break;

      case 'u':
	/* Sun compiler passes undocumented switch "-undef".
	   Let's assume it means to inhibit the predefined symbols.  */
	inhibit_predefs = 1;
	break;

      case '\0': /* JF handle '-' as file name meaning stdin or stdout */
	if (in_fname == NULL) {
	  in_fname = "";
	  break;
	} else if (out_fname == NULL) {
	  out_fname = "stdout";
	  break;
	}	/* else fall through into error */

      default:
	fatal ("Illegal option %s\n", argv[i]);
      }
    }
  }

  /* Do standard #defines that identify processor type.  */

  if (!inhibit_predefs) {
    char *p = (char *) alloca (strlen (predefs) + 1);
    strcpy (p, predefs);
    while (*p) {
      char *q = p;
      while (*p && *p != ',') p++;
      if (*p != 0)
	*p++= 0;
      make_definition (q);
    }
  }

  /* Do defines specified with -D.  */
  for (i = 1; i < argc; i++)
    if (pend_defs[i])
      make_definition (pend_defs[i]);

  /* Do undefines specified with -U.  */
  for (i = 1; i < argc; i++)
    if (pend_undefs[i])
      make_undef (pend_undefs[i]);

  /* Initialize output buffer */

  outbuf.buf = (U_CHAR *) xmalloc (OUTBUF_SIZE);
  outbuf.bufp = outbuf.buf;
  outbuf.length = OUTBUF_SIZE;

  /* Scan the -i files before the main input.
     Much like #including them, but with no_output set
     so that only their macro definitions matter.  */

  no_output++;
  for (i = 1; i < argc; i++)
    if (pend_files[i]) {
      int fd = open (pend_files[i], O_RDONLY, 0666);
      if (fd < 0) {
	perror_with_name (pend_files[i]);
	return FATAL_EXIT_CODE;
      }
      finclude (fd, pend_files[i], &outbuf);
    }
  no_output--;

  /* Create an input stack level for the main input file
     and copy the entire contents of the file into it.  */

  fp = &instack[++indepth];

  /* JF check for stdin */
  if (in_fname == NULL || *in_fname == 0) {
    in_fname = "";
    f = 0;
  } else if ((f = open (in_fname, O_RDONLY)) < 0)
    goto perror;

  fstat (f, &sbuf);
  fp->fname = in_fname;
  fp->lineno = 1;
  /* JF all this is mine about reading pipes and ttys */
  if ((sbuf.st_mode & S_IFMT) != S_IFREG) {
    /* Read input from a file that is not a normal disk file.
       We cannot preallocate a buffer with the correct size,
       so we must read in the file a piece at the time and make it bigger.  */
    int size;
    int bsize;
    int cnt;
    U_CHAR *bufp;

    bsize = 2000;
    size = 0;
    fp->buf = (U_CHAR *) xmalloc (bsize + 2);
    bufp = fp->buf;
    for (;;) {
      cnt = read (f, bufp, bsize - size);
      if (cnt < 0) goto perror;	/* error! */
      if (cnt == 0) break;	/* End of file */
      size += cnt;
      bufp += cnt;
      if (bsize == size) {	/* Buffer is full! */
        bsize *= 2;
        fp->buf = (U_CHAR *) xrealloc (fp->buf, bsize + 2);
	bufp = fp->buf + size;	/* May have moved */
      }
    }
    fp->length = size;
  } else {
    /* Read a file whose size we can determine in advance.  */
    fp->length = sbuf.st_size;
    fp->buf = (U_CHAR *) alloca (sbuf.st_size + 2);

    if (read (f, fp->buf, sbuf.st_size) != sbuf.st_size)
      goto perror;
  }
  fp->bufp = fp->buf;

  /* Make sure data ends with a newline.  And put a null after it.  */

  if (fp->length > 0 && fp->buf[fp->length-1] != '\n')
    fp->buf[fp->length++] = '\n';
  fp->buf[fp->length] = '\0';

  /* Unless inhibited, convert trigraphs in the input.  */

  if (!no_trigraphs)
    trigraph_pcp (fp);

  output_line_command (fp, &outbuf, 0);

  /* Scan the input, processing macros and directives.  */

  rescan (&outbuf, 0);

  /* Now we have processed the entire input
     Write whichever kind of output has been requested.  */

  if (dump_macros)
    dump_all_macros ();
  else {
    write (fileno (stdout), outbuf.buf, outbuf.bufp - outbuf.buf);
  }

  return 0;

 perror:
  pfatal_with_name (argv[1]);
}

/* Pre-C-Preprocessor to translate ANSI trigraph idiocy in BUF
   before main CCCP processing.  Name `pcp' is also in honor of the
   drugs the trigraph designers must have been on.

   Using an extra pass through the buffer takes a little extra time,
   but is infinitely less hairy than trying to handle ??/" inside
   strings, etc. everywhere, and also makes sure that trigraphs are
   only translated in the top level of processing. */

trigraph_pcp (buf)
     FILE_BUF *buf;
{
  register U_CHAR c, *fptr, *bptr, *sptr;
  int len;

  fptr = bptr = sptr = buf->buf;
  while ((sptr = (U_CHAR *) index (sptr, '?')) != NULL) {
    if (*++sptr != '?')
      continue;
    switch (*++sptr) {
      case '=':
      c = '#';
      break;
    case '(':
      c = '[';
      break;
    case '/':
      c = '\\';
      break;
    case ')':
      c = ']';
      break;
    case '\'':
      c = '^';
      break;
    case '<':
      c = '{';
      break;
    case '!':
      c = '|';
      break;
    case '>':
      c = '}';
      break;
    case '-':
      c  = '~';
      break;
    case '?':
      sptr--;
      continue;
    default:
      continue;
    }
    len = sptr - fptr - 2;
    if (bptr != fptr && len > 0)
      bcopy (fptr, bptr, len);	/* BSD doc says bcopy () works right
				   for overlapping strings.  In ANSI
				   C, this will be memmove (). */
    bptr += len;
    *bptr++ = c;
    fptr = ++sptr;
  }
  len = buf->length - (fptr - buf->buf);
  if (bptr != fptr && len > 0)
    bcopy (fptr, bptr, len);
  buf->length -= fptr - bptr;
  buf->buf[buf->length] = '\0';
}

/*
 * The main loop of the program.
 *
 * Read characters from the input stack, transferring them to the
 * output buffer OP.
 *
 * Macros are expanded and push levels on the input stack.
 * At the end of such a level it is popped off and we keep reading.
 * At the end of any other kind of level, we return.
 * #-directives are handled, except within macros.
 *
 * If OUTPUT_MARKS is nonzero, keep Newline markers found in the input
 * and insert them when appropriate.  This is set while scanning macro
 * arguments before substitution.  It is zero when scanning for final output.
 *   There are three types of Newline markers:
 *   * Newline -  follows a macro name that was not expanded
 *     because it appeared inside an expansion of the same macro.
 *     This marker prevents future expansion of that identifier.
 *     When the input is rescanned into the final output, these are deleted.
 *     These are also deleted by ## concatenation.
 *   * Newline Space (or Newline and any other whitespace character)
 *     stands for a place that tokens must be separated or whitespace
 *     is otherwise desirable, but where the ANSI standard specifies there
 *     is no whitespace.  This marker turns into a Space (or whichever other
 *     whitespace char appears in the marker) in the final output,
 *     but it turns into nothing in an argument that is stringified with #.
 *     Such stringified arguments are the only place where the ANSI standard
 *     specifies with precision that whitespace may not appear.
 *
 * During this function, IP->bufp is kept cached in IBP for speed of access.
 * Likewise, OP->bufp is kept in OBP.  Before calling a subroutine
 * IBP, IP and OBP must be copied back to memory.  IP and IBP are
 * copied back with the RECACHE macro.  OBP must be copied back from OP->bufp
 * explicitly, and before RECACHE, since RECACHE uses OBP.
 */

rescan (op, output_marks)
     FILE_BUF *op;
     int output_marks;
{
  /* Character being scanned in main loop.  */
  register U_CHAR c;

  /* Length of pending accumulated identifier.  */
  register int ident_length = 0;

  /* Hash code of pending accumulated identifier.  */
  register int hash = 0;

  /* Current input level (&instack[indepth]).  */
  FILE_BUF *ip;

  /* Pointer for scanning input.  */
  register U_CHAR *ibp;

  /* Pointer to end of input.  End of scan is controlled by LIMIT.  */
  register U_CHAR *limit;

  /* Pointer for storing output.  */
  register U_CHAR *obp;

  /* REDO_CHAR is nonzero if we are processing an identifier
     after backing up over the terminating character.
     Sometimes we process an identifier without backing up over
     the terminating character, if the terminating character
     is not special.  Backing up is done so that the terminating character
     will be dispatched on again once the identifier is dealt with.  */
  int redo_char = 0;

  /* 1 if within an identifier inside of which a concatenation
     marker (Newline -) has been seen.  */
  int concatenated = 0;

  /* While scanning a comment or a string constant,
     this records the line it started on, for error messages.  */
  int start_line;

/* Pop the innermost input stack level, assuming it is a macro expansion.  */

#define POPMACRO \
do { ip->macro->type = T_MACRO;		\
     if (ip->free) free (ip->free);	\
     --indepth; } while (0)

/* Reload `rescan's local variables that describe the current
   level of the input stack.  */

#define RECACHE  \
do { ip = &instack[indepth];		\
     ibp = ip->bufp;			\
     limit = ip->buf + ip->length;	\
     op->bufp = obp;			\
     check_expand (op, limit - ibp);	\
     obp = op->bufp; } while (0)

  if (no_output && instack[indepth].fname != 0)
    skip_if_group (&instack[indepth], 1);

  obp = op->bufp;
  RECACHE;

  /* Our caller must always put a null after the end of
     the input at each input stack level.  */
  if (*limit != 0)
    abort ();

  while (1) {
    c = *ibp++;
    *obp++ = c;

    switch (c) {
    case '\\':
      if (ibp >= limit)
	break;
      if (*ibp == '\n') {
	/* Always merge lines ending with backslash-newline,
	   even in middle of identifier.  */
	++ibp;
	++ip->lineno;
	--obp;		/* remove backslash from obuf */
	break;
      }
      /* Otherwise, backslash suppresses specialness of following char,
	 so copy it here to prevent the switch from seeing it.
	 But first get any pending identifier processed.  */
      if (ident_length > 0)
	goto specialchar;
      *obp++ = *ibp++;
      break;

    case '#':
      /* If this is a macro definition, don't recognize
	 preprocessor directives.  */
      if (ip->macro != 0)
	goto randomchar;
      if (ident_length)
	goto specialchar;

      /* # keyword: a # must be first nonblank char on the line */
      {
	U_CHAR *bp;

	for (bp = ibp - 1; bp >= ip->buf; bp--)
	  if (*bp == '\n')
	    break;
	bp++;	/* Skip nl or move back into buffer */
	SKIP_WHITE_SPACE (bp);
	if (*bp != '#')
	  goto randomchar;
      }

      --obp;		/* Don't copy the '#' */

      ip->bufp = ibp;
      op->bufp = obp;
      if (! handle_directive (ip, op)) {
	if (no_output && instack[indepth].fname) {
	  skip_if_group (&instack[indepth], 1);
	  break;
	}
	++obp;		/* Copy the '#' after all */
	goto randomchar;
      }
      if (no_output && instack[indepth].fname)
	skip_if_group (&instack[indepth], 1);
      obp = op->bufp;
      RECACHE;
      break;

    case '\"':			/* skip quoted string */
    case '\'':
      /* A single quoted string is treated like a double -- some
	 programs (e.g., troff) are perverse this way */

      if (ident_length)
	goto specialchar;

      start_line = ip->lineno;

      /* Skip ahead to a matching quote.  */

      while (1) {
	if (ibp >= limit)
	  {
	    error ("Unterminated string constant starts at line %d",
		   line_for_error (start_line));
	    break;
	  }
	*obp++ = *ibp;
	switch (*ibp++) {
	case '\n':
	  ++ip->lineno;
	  ++op->lineno;
	  break;
	case '\\':
	  if (ibp >= limit)
	    break;
	  if (*ibp == '\n')
	    {
	      /* Backslash newline is replaced by nothing at all,
		 but keep the line counts correct.  */
	      --obp;
	      ++ibp;
	      ++ip->lineno;
	    }
	  else
	    *obp++ = *ibp++;
	  break;
	case '\"':
	case '\'':
	  if (ibp[-1] == c)
	    goto while2end;
	  break;
	}
      }
    while2end:
      break;

    case '/':
      if (*ibp != '*')
	goto randomchar;
      if (ip->macro != 0)
	goto randomchar;
      if (ident_length)
	goto specialchar;

      /* We have a comment.  Skip it, optionally copying it to output.  */

      start_line = ip->lineno;

      --ibp;			/* Back over the slash.  */
      --obp;

      /* Comments are equivalent to spaces.  */
      if (! put_out_comments)
	*obp++ = ' ';

      {
	U_CHAR *before_bp = ibp;

	while (ibp < limit) {
	  switch (*ibp++) {
	  case '*':
	    if (ibp >= limit || *ibp == '/')
	      goto comment_end;
	    break;
	  case '\n':
	    ++ip->lineno;
	    /* Copy the newline into the output buffer, in order to
	       avoid the pain of a #line every time a multiline comment
	       is seen.  */
	    if (!put_out_comments)
	      *obp++ = '\n';
	    ++op->lineno;
	  }
	}
      comment_end:

	if (ibp >= limit)
	  error ("Unterminated comment starts on line %d",
		 line_for_error (start_line));
	else {
	  ibp++;
	  if (put_out_comments) {
	    bcopy (before_bp, obp, ibp - before_bp);
	    obp += ibp - before_bp;
	  }
	}
      }
      break;

    case '0': case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8': case '9':
      /* If digit is not part of identifier, it starts a number,
	 which means that following letters are not an identifier.
	 "0x5" does not refer to an identifier "x5".
	 So copy all alphanumerics that follow without accumulating
	 as an identifier.  Periods also, for sake of "3.e7".  */

      if (ident_length == 0) {
	while (ibp < limit) {
	  c = *ibp++;
	  if (!isalnum (c) && c != '.') {
	    --ibp;
	    break;
	  }
	  *obp++ = c;
	}
	break;
      }
      /* fall through */

    case '_':
    case 'a': case 'b': case 'c': case 'd': case 'e': case 'f':
    case 'g': case 'h': case 'i': case 'j': case 'k': case 'l':
    case 'm': case 'n': case 'o': case 'p': case 'q': case 'r':
    case 's': case 't': case 'u': case 'v': case 'w': case 'x':
    case 'y': case 'z':
    case 'A': case 'B': case 'C': case 'D': case 'E': case 'F':
    case 'G': case 'H': case 'I': case 'J': case 'K': case 'L':
    case 'M': case 'N': case 'O': case 'P': case 'Q': case 'R':
    case 'S': case 'T': case 'U': case 'V': case 'W': case 'X':
    case 'Y': case 'Z':
      ident_length++;
      /* Compute step of hash function, to avoid a proc call on every token */
      hash = HASHSTEP (hash, c);
      break;

    case '\n':
      /* If reprocessing a macro expansion, newline is a special marker.  */
      if (ip->macro != 0) {
	/* Newline White is a "funny space" to separate tokens that are
	   supposed to be separate but without space between.
	   Here While means any horizontal whitespace character.
	   Newline - marks a recursive macro use that is not
	   supposed to be expandable.  */

	if (*ibp == '-') {
	  /* Newline - inhibits expansion of preceding token.
	     If expanding a macro arg, we keep the newline -.
	     In final output, it is deleted.  */
	  if (! concatenated) {
	    ident_length = 0;
	    hash = 0;
	  }
	  ibp++;
	  if (!output_marks) {
	    obp--;
	  } else {
	    /* If expanding a macro arg, keep the newline -.  */
	    *obp++ = '-';
	  }
	} else if (isspace (*ibp)) {
	  /* Newline Space does not prevent expansion of preceding token
	     so expand the preceding token and then come back.  */
	  if (ident_length > 0)
	    goto specialchar;

	  /* If generating final output, newline space makes a space.  */
	  if (!output_marks) {
	    obp[-1] = *ibp++;
	    /* And Newline Newline makes a newline, so count it.  */
	    if (obp[-1] == '\n')
	      op->lineno++;
	  } else {
	    /* If expanding a macro arg, keep the newline space.
	       If the arg gets stringified, newline space makes nothing.  */
	    *obp++ = *ibp++;
	  }
	} else abort ();	/* Newline followed by something random?  */
	break;
      }

      /* If there is a pending identifier, handle it and come back here.  */
      if (ident_length > 0)
	goto specialchar;

      /* Update the line counts and output a #line if necessary.  */
      ++ip->lineno;
      ++op->lineno;
      if (ip->lineno != op->lineno) {
	op->bufp = obp;
	output_line_command (ip, op, 1);
	check_expand (op, ip->length - (ip->bufp - ip->buf));
	obp = op->bufp;
      }
      break;

      /* Come here either after (1) a null character that is part of the input
	 or (2) at the end of the input, because there is a null there.  */
    case 0:
      if (ibp <= limit)
	/* Our input really contains a null character.  */
	goto randomchar;

      /* At end of a macro-expansion level, pop it and read next level.  */
      if (ip->macro != 0) {
	obp--;
	ibp--;
	POPMACRO;
	RECACHE;
	break;
      }

      /* If we don't have a pending identifier,
	 return at end of input.  */
      if (ident_length == 0)
	{
	  obp--;
	  ibp--;
	  op->bufp = obp;
	  ip->bufp = ibp;
	  return;
	}

      /* If we do have a pending identifier, just consider this null
	 a special character and arrange to dispatch on it again.
	 The second time, IDENT_LENGTH will be zero so we will return.  */

      /* Fall through */

specialchar:

      /* Handle the case of a character such as /, ', " or null
	 seen following an identifier.  Back over it so that
	 after the identifier is processed the special char
	 will be dispatched on again.  */

      ibp--;
      obp--;
      redo_char = 1;

    default:

randomchar:

      if (ident_length > 0) {
	register HASHNODE *hp;

	/* We have just seen an identifier end.  If it's a macro, expand it.

	   IDENT_LENGTH is the length of the identifier
	   and HASH is its hash code.

	   The identifier has already been copied to the output,
	   so if it is a macro we must remove it.

	   If REDO_CHAR is 0, the char that terminated the identifier
	   has been skipped in the output and the input.
	   OBP-IDENT_LENGTH-1 points to the identifier.
	   If the identifier is a macro, we must back over the terminator.

	   If REDO_CHAR is 1, the terminating char has already been
	   backed over.  OBP-IDENT_LENGTH points to the identifier.  */

	for (hp = hashtab[MAKE_POS (hash) % HASHSIZE]; hp != NULL;
	     hp = hp->next) {

	  if (hp->length == ident_length) {
	    U_CHAR *obufp_before_macroname;
	    int op_lineno_before_macroname;
	    register int i = ident_length;
	    register U_CHAR *p = hp->name;
	    register U_CHAR *q = obp - i;
	    int disabled;

	    if (! redo_char)
	      q--;

	    do {		/* All this to avoid a strncmp () */
	      if (*p++ != *q++)
		goto hashcollision;
	    } while (--i);

	    /* We found a use of a macro name.
	       see if the context shows it is a macro call.  */

	    /* Back up over terminating character if not already done.  */
	    if (! redo_char)
	      {
		ibp--;
		obp--;
	      }

	    obufp_before_macroname = obp - ident_length;
	    op_lineno_before_macroname = op->lineno;

	    /* Record whether the macro is disabled now,
	       even though we don't act on it until later,
	       because popping levels while looking for the `('
	       could reenable the macro.  */
	    disabled = hp->type == T_DISABLED;

	    /* If macro wants an arglist, verify that a '(' follows.
	       first skip all whitespace, copying it to the output
	       after the macro name.  then, if there is no '(',
	       decide this is not a macro call and leave things that way.  */
	    if ((hp->type == T_MACRO || hp->type == T_DISABLED)
		&& hp->value.defn->nargs >= 0)
	      {
		while (1) {
		  /* Scan forward over whitespace, copying it to the output.  */
		  if (ibp == limit && ip->macro != 0) {
		    POPMACRO;
		    RECACHE;
		  }
		  else if (isspace (*ibp)) {
		    *obp++ = *ibp++;
		    if (ibp[-1] == '\n')
		      {
			if (ip->macro == 0) {
			  ++ip->lineno;
			  ++op->lineno;
			}
			else if (!output_marks)
			  obp--;
		      }
		  }
		  else break;
		}
		if (*ibp != '(')
		  break;
	      }

	    /* This looks like a macro call, but if the macro was disabled,
	       just copy its name and put in a marker if requested.  */

	    if (disabled)
	      {
		if (output_marks) {
		  check_expand (op, limit - ibp + 2);
		  *obp++ = '\n';
		  *obp++ = '-';
		}
		break;
	      }

	    /* This is now known to be a macro call.
	       Discard the macro name from the output,
	       along with any following whitespace just copied.  */
	    obp = obufp_before_macroname;
	    op->lineno = op_lineno_before_macroname;

	    /* Expand the macro, reading arguments as needed,
	       and push the expansion on the input stack.  */
	    ip->bufp = ibp;
	    op->bufp = obp;
	    macroexpand (hp, op);

	    /* Reexamine input stack, since macroexpand has pushed
	       a new level on it.  */
	    obp = op->bufp;
	    RECACHE;
	    break;
	  }
hashcollision:
	       ;
	}			/* End hash-table-search loop */
	ident_length = hash = 0; /* Stop collecting identifier */
	redo_char = 0;
	concatenated = 0;
      }				/* End if (ident_length > 0) */
    }				/* End switch */
  }				/* End per-char loop */
}

/*
 * Rescan a string into a temporary buffer and return the result
 * as a FILE_BUF.  Note this function returns a struct, not a pointer.
 *
 * OUTPUT_MARKS nonzero means keep Newline markers found in the input
 * and insert such markers when appropriate.  See `rescan' for details.
 * OUTPUT_MARKS is 1 for macroexpanding a macro argument separately
 * before substitution; it is 0 for other uses.
 */
static FILE_BUF
expand_to_temp_buffer (buf, limit, output_marks)
     U_CHAR *buf, *limit;
     int output_marks;
{
  register FILE_BUF *ip;
  FILE_BUF obuf;
  int value;
  int length = limit - buf;
  U_CHAR *buf1;
  int odepth = indepth;

  if (length < 0)
    abort ();

  /* Set up the input on the input stack.  */

  buf1 = (U_CHAR *) alloca (length + 1);
  {
    register U_CHAR *p1 = buf;
    register U_CHAR *p2 = buf1;

    while (p1 != limit)
      *p2++ = *p1++;
  }
  buf1[length] = 0;

  ++indepth;

  ip = &instack[indepth];
  ip->fname = 0;
  ip->macro = 0;
  ip->free = 0;
  ip->length = length;
  ip->buf = ip->bufp = buf1;

  /* Set up to receive the output.  */

  obuf.length = length * 2 + 100; /* Usually enough.  Why be stingy?  */
  obuf.bufp = obuf.buf = (U_CHAR *) xmalloc (obuf.length);
  obuf.fname = 0;
  obuf.macro = 0;
  obuf.free = 0;

  ip->lineno = obuf.lineno = 1;

  /* Scan the input, create the output.  */

  rescan (&obuf, output_marks);

  /* Pop input stack to original state.  */
  --indepth;

  if (indepth != odepth)
    abort ();

  /* Record the output.  */
  obuf.length = obuf.bufp - obuf.buf;

  return obuf;
}

/*
 * Process a # directive.  Expects IP->bufp to point to the '#', as in
 * "#define foo bar".  Passes to the command handler
 * (do_define, do_include, etc.): the addresses of the 1st and
 * last chars of the command (starting immediately after the #
 * keyword), plus op and the keyword table pointer.  If the command
 * contains comments it is copied into a temporary buffer sans comments
 * and the temporary buffer is passed to the command handler instead.
 * Likewise for backslash-newlines.
 *
 * Returns nonzero if this was a known # directive.
 * Otherwise, returns zero, without advancing the input pointer.
 */

int
handle_directive (ip, op)
     FILE_BUF *ip, *op;
{
  register U_CHAR *bp, *cp;
  register struct directive *kt;
  register int ident_length;
  U_CHAR *resume_p;

  /* Nonzero means we must copy the entire command
     to get rid of comments or backslash-newlines.  */
  int copy_command = 0;

  bp = ip->bufp;
  SKIP_WHITE_SPACE (bp);
  cp = bp;
  while (is_idchar[*cp])
    cp++;
  ident_length = cp - bp;

  /*
   * Decode the keyword and call the appropriate expansion
   * routine, after moving the input pointer up to the next line.
   */
  for (kt = directive_table; kt->length > 0; kt++) {
    if (kt->length == ident_length && !strncmp (kt->name, bp, ident_length)) {
      register U_CHAR *buf;
      register U_CHAR *limit = ip->buf + ip->length;

      /* Find the end of this command (first newline not backslashed
	 and not in a string or comment).
	 Set COPY_COMMAND if the command must be copied
	 (it contains a backslash-newline or a comment).  */

      buf = bp = bp + ident_length;
      while (bp < limit) {
	register U_CHAR c = *bp++;
	switch (c) {
	case '\\':
	  if (bp < limit) {
	    if (*bp == '\n')
	      {
		ip->lineno++;
		copy_command = 1;
	      }
	    bp++;
	  }
	  break;

	case '\'':
	case '\"':
	  bp = skip_quoted_string (bp - 1, limit, ip->lineno, &ip->lineno, &copy_command);
	  break;

	case '/':
	  if (*bp == '*') {
	    U_CHAR *obp = bp - 1;
	    ip->bufp = bp + 1;
	    skip_to_end_of_comment (ip, &ip->lineno);
	    bp = ip->bufp;
	    /* No need to copy the command because of a comment at the end;
	       just don't include the comment in the directive.  */
	    if (bp == limit || *bp == '\n') {
	      bp = obp;
	      goto endloop1;
	    }
	    copy_command++;
	  }
	  break;

	case '\n':
	  --bp;		/* Point to the newline */
	  ip->bufp = bp;
	  goto endloop1;
	}
      }
      ip->bufp = bp;

    endloop1:
      resume_p = ip->bufp;
      /* BP is the end of the directive.
	 RESUME_P is the next interesting data after the directive.
	 A comment may come between.  */

      if (copy_command) {
	register U_CHAR *xp = buf;
	register U_CHAR *cp1;
	/* Need to copy entire command into temp buffer before dispatching */

	cp = (U_CHAR *) alloca (bp - buf + 5); /* room for cmd plus
						  some slop */
	buf = cp;

	/* Copy to the new buffer, deleting comments.  */

	while (xp < bp) {
	  register U_CHAR c = *xp++;
	  *cp++ = c;

	  switch (c) {
	  case '\n':
	    break;

	  case '\'':
	  case '\"':
	    {
	      register U_CHAR *bp1
		= skip_quoted_string (xp - 1, limit, ip->lineno, 0, 0);
	      while (xp != bp1)
		*cp++ = *xp++;
	    }
	    break;

	  case '/':
	    if (*xp == '*') {
	      cp--;
	      ip->bufp = xp + 1;
	      skip_to_end_of_comment (ip, 0);
	      xp = ip->bufp;
	    }
	  }
	}

	/* Now copy new buffer to itself, deleting
	   backslash-newline.  */

	cp1 = cp;
	*cp1 = 0;
	xp = buf;
	cp = buf;

	while (xp < cp1) {
	  register U_CHAR c = *xp++;
	  *cp++ = c;

	  if (c == '\\' && *xp == '\n') {
	    --cp;
	    ++xp;
	    if (cp != buf && isspace (cp[-1])) {
	      while (cp != buf && isspace (cp[-1])) cp--;
	      cp++;
	      SKIP_WHITE_SPACE (xp);
	    } else if (isspace (*xp)) {
	      *cp++ = *xp++;
	      SKIP_WHITE_SPACE (xp);
	    }
	  }
	}
      }
      else
	cp = bp;

      ip->bufp = resume_p;

      /* Call the appropriate command handler.  buf now points to
	 either the appropriate place in the input buffer, or to
	 the temp buffer if it was necessary to make one.  cp
	 points to the first char after the contents of the (possibly
	 copied) command, in either case. */
      (*kt->func) (buf, cp, op, kt);
      check_expand (op, ip->length - (ip->bufp - ip->buf));

      return 1;
    }
  }

  return 0;
}

static char *monthnames[] = {"jan", "feb", "mar", "apr", "may", "jun",
			     "jul", "aug", "sep", "oct", "nov", "dec",
			    };

/*
 * expand things like __FILE__.  Place the expansion into the output
 * buffer *without* rescanning.
 */
special_symbol (hp, op)
     HASHNODE *hp;
     FILE_BUF *op;
{
  char *buf;
  int i, len;
  FILE_BUF *ip = NULL;
  static struct tm *timebuf = NULL;
  struct tm *localtime ();

  int paren = 0;		/* For special `defined' keyword */

  for (i = indepth; i >= 0; i--)
    if (instack[i].fname != NULL) {
      ip = &instack[i];
      break;
    }
  if (ip == NULL) {
    error ("cccp error: not in any file?!");
    return;			/* the show must go on */
  }

  switch (hp->type) {
  case T_FILE:
    buf = (char *) alloca (3 + strlen (ip->fname));
    sprintf (buf, "\"%s\"", ip->fname);
    break;

  case T_CONST:
    buf = (char *) alloca (4 * sizeof (int));
    sprintf (buf, "%d", hp->value.ival);
    break;

  case T_SPECLINE:
    buf = (char *) alloca (10);
    sprintf (buf, "%d", ip->lineno);
    break;

  case T_DATE:
  case T_TIME:
    if (timebuf == NULL) {
      i = time (0);
      timebuf = localtime (&i);
    }
    buf = (char *) alloca (20);
    if (hp->type == T_DATE)
      sprintf (buf, "\"%s %2d %4d\"", monthnames[timebuf->tm_mon - 1],
	      timebuf->tm_mday, timebuf->tm_year + 1900);
    else
      sprintf (buf, "\"%02d:%02d:%02d\"", timebuf->tm_hour, timebuf->tm_min,
	      timebuf->tm_sec);
    break;

  case T_SPEC_DEFINED:
    buf = " 0 ";		/* Assume symbol is not defined */
    ip = &instack[indepth];
    SKIP_WHITE_SPACE (ip->bufp);
    if (*ip->bufp == '(') {
      paren++;
      ip->bufp++;			/* Skip over the paren */
      SKIP_WHITE_SPACE (ip->bufp);
    }

    if (!is_idstart[*ip->bufp])
      goto oops;
    if (lookup (ip->bufp, -1, -1))
      buf = " 1 ";
    while (is_idchar[*ip->bufp])
      ++ip->bufp;
    SKIP_WHITE_SPACE (ip->bufp);
    if (paren) {
      if (*ip->bufp != ')')
	goto oops;
      ++ip->bufp;
    }
    break;

oops:

    error ("`defined' must be followed by ident or (ident)");
    break;

  default:
    error ("cccp error: illegal special hash type"); /* time for gdb */
    abort ();
  }
  len = strlen (buf);
  check_expand (op, len);
  bcopy (buf, op->bufp, len);
  op->bufp += len;

  return;
}


/* Routines to handle #directives */

/*
 * Process include file by reading it in and calling rescan.
 * Expects to see "fname" or <fname> on the input.
 * add error checking and -i option later.
 */

do_include (buf, limit, op, keyword)
     U_CHAR *buf, *limit;
     FILE_BUF *op;
     struct directive *keyword;
{
  char *fname;		/* Dynamically allocated fname buffer */
  U_CHAR *fbeg, *fend;		/* Beginning and end of fname */
  U_CHAR term;			/* Terminator for fname */
  int err = 0;			/* Some error has happened */

  struct directory_stack *stackp;
  int flen;

  int f;			/* file number */
  char *other_dir;

  int retried = 0;		/* Have already tried macro
				   expanding the include line*/
  FILE_BUF trybuf;		/* It got expanded into here */

  f= -1;			/* JF we iz paranoid! */

get_filename:

  fbeg = buf;
  SKIP_WHITE_SPACE (fbeg);
  /* Discard trailing whitespace so we can easily see
     if we have parsed all the significant chars we were given.  */
  while (limit != fbeg && is_hor_space[limit[-1]]) limit--;

  switch (*fbeg++) {
  case '\"':
    fend = fbeg;
    while (fend != limit && *fend != '\"') {
      if (*fend == '\\') {
	if (fend + 1 == limit)
	  break;
	fend++;
      }
      fend++;
    }
    if (*fend == '\"' && fend + 1 == limit) {
      stackp = include;
      break;
    }
    goto fail;

  case '<':
    fend = fbeg;
    while (fend != limit && *fend != '>') fend++;
    if (*fend == '>' && fend + 1 == limit) {
      stackp = include->next;
      break;
    }
    goto fail;

  default:
  fail:
    if (retried) {
      error ("#include expects \"fname\" or <fname>");
      return;
    } else {
      trybuf = expand_to_temp_buffer (buf, limit, 0);
      buf = (U_CHAR *) alloca (trybuf.bufp - trybuf.buf + 1);
      bcopy (trybuf.buf, buf, trybuf.bufp - trybuf.buf);
      limit = buf + (trybuf.bufp - trybuf.buf);
      free (trybuf.buf);
      retried++;
      goto get_filename;
    }
  }

  flen = fend - fbeg;

  /* Set OTHER_DIR to the directory the current input file is in.
     That is one of the directories searched, sometimes.  */

  other_dir = NULL;
  if (stackp == include)
    {
      FILE_BUF *fp;
      for (fp = &instack[indepth]; fp >= instack; fp--)
	{
	  int n;
	  char *ep,*nam;
	  extern char *rindex ();

	  if ((nam = fp->fname) != NULL)
	    {
	      if ((ep = rindex (nam, '/')) != NULL)
		{
		  n = ep - nam;
		  other_dir = (char *) alloca (n + 1);
		  strncpy (other_dir, nam, n);
		  other_dir[n] = '\0';
		}
	      break;
	    }
	}
    }

  /* Search directory path, trying to open the file.
     Copy each filename tried into FNAME.  */

  fname = (char *) alloca (max_include_len + flen);
  for (; stackp; stackp = stackp->next)
    {
      if (other_dir)
	{
	  strcpy (fname, other_dir);
	  other_dir = 0;
	}
      else
	strcpy (fname, stackp->fname);
      strcat (fname, "/");
      strncat (fname, fbeg, flen);
      if ((f = open (fname, O_RDONLY)) >= 0)
	break;
    }

  if (f < 0)
    perror (fname);
  else {
    finclude (f, fname, op);
    close (f);
  }
}

/* Process the contents of include file FNAME, already open on descriptor F,
   with output to OP.  */

finclude (f, fname, op)
     int f;
     char *fname;
     FILE_BUF *op;
{
  struct stat sbuf;		/* To stat the include file */
  FILE_BUF *fp;			/* For input stack frame */
  int success = 0;

  if (fstat (f, &sbuf) < 0)
    goto nope;		/* Impossible? */

  fp = &instack[indepth + 1];
  bzero (fp, sizeof (FILE_BUF));
  fp->buf = (U_CHAR *) alloca (sbuf.st_size + 2);
  fp->fname = fname;
  fp->length = sbuf.st_size;
  fp->lineno = 1;
  fp->bufp = fp->buf;

  if (read (f, fp->buf, sbuf.st_size) != sbuf.st_size)
    goto nope;

  if (fp->length > 0 && fp->buf[fp->length-1] != '\n')
    fp->buf[fp->length++] = '\n';
  fp->buf[fp->length] = '\0';

  if (!no_trigraphs)
    trigraph_pcp (fp);

  success = 1;
  indepth++;

  output_line_command (fp, op, 0);
  rescan (op, 0);
  indepth--;
  output_line_command (&instack[indepth], op, 0);

nope:

  close (f);
  if (!success) {
    perror_with_name (fname);
  }
}

/* The arglist structure is built by do_define to tell
   collect_definition where the argument names begin.  That
   is, for a define like "#define f(x,y,z) foo+x-bar*y", the arglist
   would contain pointers to the strings x, y, and z.
   Collect_definition would then build a DEFINITION node,
   with reflist nodes pointing to the places x, y, and z had
   appeared.  So the arglist is just convenience data passed
   between these two routines.  It is not kept around after
   the current #define has been processed and entered into the
   hash table. */

struct arglist {
  struct arglist *next;
  U_CHAR *name;
  int length;
  int argno;
};

/* Process a #define command.
BUF points to the contents of the #define command, as a continguous string.
LIMIT points to the first character past the end of the definition.
KEYWORD is the keyword-table entry for #define.  */

do_define (buf, limit, op, keyword)
     U_CHAR *buf, *limit;
     FILE_BUF *op;
     struct directive *keyword;
{
  U_CHAR *bp;			/* temp ptr into input buffer */
  U_CHAR *symname;		/* remember where symbol name starts */
  int sym_length;		/* and how long it is */
  U_CHAR *def;			/* beginning of expansion */

  DEFINITION *defn;
  int arglengths = 0;		/* Accumulate lengths of arg names
				   plus number of args.  */
  int hashcode;

  bp = buf;

  while (is_hor_space[*bp])
    bp++;
  if (!is_idstart[*bp]) {
    error ("illegal macro name: must start with an alphabetic or '_'");
    goto nope;
  }
  symname = bp;			/* remember where it starts */
  while (is_idchar[*bp] && bp < limit)
    bp++;
  sym_length = bp - symname;

  /* lossage will occur if identifiers or control keywords are broken
     across lines using backslash.  This is not the right place to take
     care of that. */

  if (is_hor_space[*bp] || *bp == '\n' || bp >= limit) {
    /* simple expansion or empty definition; gobble it */
    if (is_hor_space[*bp])
      ++bp;		/* skip exactly one blank/tab char */
    /* now everything from bp before limit is the definition. */
    defn = collect_expansion (bp, limit, -1, 0);
    defn->argnames = (U_CHAR *) "";
  }
  else if (*bp == '(') {
    struct arglist *arg_ptrs = NULL;
    int argno = 0;

    bp++;			/* skip '(' */
    SKIP_WHITE_SPACE (bp);

    while (*bp != ')') {
      struct arglist *temp;

      temp = (struct arglist *) alloca (sizeof (struct arglist));
      temp->name = bp;
      temp->next = arg_ptrs;
      temp->argno = argno++;
      arg_ptrs = temp;
      while (is_idchar[*bp])
	bp++;
      temp->length = bp - temp->name;
      arglengths += temp->length + 2;
      SKIP_WHITE_SPACE (bp);	/* there should not be spaces here,
				   but let it slide if there are. */
      if (temp->length == 0 || (*bp != ',' && *bp != ')')) {
	error ("illegal parameter to macro");
	goto nope;
      }
      if (*bp == ',') {
	bp++;
	SKIP_WHITE_SPACE (bp);
      }
      if (bp >= limit) {
	error ("unterminated format parameter list in #define");
	goto nope;
      }
    }

    ++bp;			/* skip paren */
    /* Skip exactly one space or tab if any.  */
    if (bp < limit && (*bp == ' ' || *bp == '\t')) ++bp;
    /* now everything from bp before limit is the definition. */
    defn = collect_expansion (bp, limit, argno, arg_ptrs);

    /* Now set defn->argnames to the result of concatenating
       the argument names in reverse order
       with comma-space between them.  */
    defn->argnames = (U_CHAR *) xmalloc (arglengths);
    {
      struct arglist *temp;
      int i = 0;
      for (temp = arg_ptrs; temp; temp = temp->next)
	{
	  bcopy (temp->name, &defn->argnames[i], temp->length);
	  i += temp->length;
	  if (temp->next != 0) {
	    defn->argnames[i++] = ',';
	    defn->argnames[i++] = ' ';
	  }
	}
      defn->argnames[i] = 0;
    }
  } else {
    error ("#define symbol name not followed by SPC, TAB, or '('");
    goto nope;
  }

  hashcode = hashf (symname, sym_length, HASHSIZE);

  {
    HASHNODE *hp;
    DEFINITION *old_def;
    if ((hp = lookup (symname, sym_length, hashcode)) != NULL) {
      if (hp->type != T_MACRO
	  || compare_defs (defn, hp->value.defn)) {
	U_CHAR *msg;			/* what pain... */
	msg = (U_CHAR *) alloca (sym_length + 20);
	bcopy (symname, msg, sym_length);
	strcpy (msg + sym_length, " redefined");
	error (msg);
      }
      /* Replace the old definition.  */
      hp->type = T_MACRO;
      hp->value.defn = defn;
    } else
      install (symname, sym_length, T_MACRO, defn, hashcode);
  }

  return 0;

nope:

  return 1;
}

/*
 * return zero if two DEFINITIONs are isomorphic
 */
static
compare_defs (d1, d2)
     DEFINITION *d1, *d2;
{
  register struct reflist *a1, *a2;
  register U_CHAR *p1 = d1->expansion;
  register U_CHAR *p2 = d2->expansion;
  int first = 1;

  if (d1->nargs != d2->nargs)
    return 1;
  if (strcmp (d1->argnames, d2->argnames))
    return 1;
  for (a1 = d1->pattern, a2 = d2->pattern; a1 && a2;
       a1 = a1->next, a2 = a2->next) {
    if (!((a1->nchars == a2->nchars && ! strncmp (p1, p2, a1->nchars))
	  || ! comp_def_part (first, p1, a1->nchars, p2, a2->nchars, 0))
	|| a1->argno != a2->argno
	|| a1->stringify != a2->stringify
	|| a1->raw_before != a2->raw_before
	|| a1->raw_after != a2->raw_after)
      return 1;
    first = 0;
    p1 += a1->nchars;
    p2 += a2->nchars;
  }
  if (a1 != a2)
    return 1;
  if (comp_def_part (first, p1, d1->length - (p1 - d1->expansion),
		     p2, d2->length - (p2 - d2->expansion), 1))
    return 1;
  return 0;
}

/* Return 1 if two parts of two macro definitions are effectively different.
   One of the parts starts at BEG1 and has LEN1 chars;
   the other has LEN2 chars at BEG2.
   Any sequence of whitespace matches any other sequence of whitespace.
   FIRST means these parts are the first of a macro definition;
    so ignore leading whitespace entirely.
   LAST means these parts are the last of a macro definition;
    so ignore trailing whitespace entirely.  */

comp_def_part (first, beg1, len1, beg2, len2, last)
     int first;
     U_CHAR *beg1, *beg2;
     int len1, len2;
     int last;
{
  register U_CHAR *end1 = beg1 + len1;
  register U_CHAR *end2 = beg2 + len2;
  if (first) {
    while (beg1 != end1 && isspace (*beg1)) beg1++;
    while (beg2 != end2 && isspace (*beg2)) beg2++;
  }
  if (last) {
    while (beg1 != end1 && isspace (end1[-1])) end1--;
    while (beg2 != end2 && isspace (end2[-1])) end2--;
  }
  while (beg1 != end1 && beg2 != end2) {
    if (isspace (*beg1) && isspace (*beg2)) {
      while (beg1 != end1 && isspace (*beg1)) beg1++;
      while (beg2 != end2 && isspace (*beg2)) beg2++;
    } else if (*beg1 == *beg2) {
      beg1++; beg2++;
    } else break;
  }
  return (beg1 != end1) || (beg2 != end2);
}

/* Read a replacement list for a macro with parameters.
   Build the DEFINITION structure.
   Reads characters of text starting at BUF until LIMIT.
   ARGLIST specifies the formal parameters to look for
   in the text of the definition; NARGS is the number of args
   in that list, or -1 for a macro name that wants no argument list.
   MACRONAME is the macro name itself (so we can avoid recursive expansion)
   and NAMELEN is its length in characters.  */

/* Leading and trailing Space, Tab, etc. are converted to markers
   Newline Space, Newline Tab, etc.
   Newline Space makes a space in the final output
   but is discarded if stringified.  (Newline Tab is similar but
   makes a Tab instead.)

   If there is no trailing whitespace, a Newline Space is added at the end
   to prevent concatenation that would be contrary to the standard.  */

static DEFINITION *
collect_expansion (buf, end, nargs, arglist)
     U_CHAR *buf, *end;
     int nargs;
     struct arglist *arglist;
{
  DEFINITION *defn;
  register U_CHAR *p, *limit, *lastp, *exp_p;
  struct reflist *endpat = NULL;
  /* Pointer to first nonspace after last ## seen.  */
  U_CHAR *concat = 0;
  /* Pointer to first nonspace after last single-# seen.  */
  U_CHAR *stringify = 0;

  /* Scan thru the replacement list, ignoring comments and quoted
     strings, picking up on the macro calls.  It does a linear search
     thru the arg list on every potential symbol.  Profiling might say
     that something smarter should happen. */

  if (end < buf)
    abort ();

  defn = (DEFINITION *) xcalloc (1, sizeof (DEFINITION) + 2 * (end - buf + 2));

  defn->nargs = nargs;
  exp_p = defn->expansion = (U_CHAR *) defn + sizeof (DEFINITION);
  lastp = exp_p;

  /* Speed this loop up later? */

  p = buf;
  limit = end;

  /* Find the beginning of the trailing whitespace.  */
  while (p < limit && isspace (limit[-1])) limit--;

  /* Convert leading whitespace to Newline-markers.  */
  while (p < limit && isspace (*p)) {
    *exp_p++ = '\n';
    *exp_p++ = *p++;
  }

  /* Process the main body of the definition.  */
  while (p < limit) {
    int skipped_arg = 0;
    register U_CHAR c = *p++;

    *exp_p++ = c;

    switch (c) {
    case '\'':
    case '\"':
      {
	for (; p < limit && *p != c; p++) {
	  *exp_p++ = *p;
	  if (*p == '\\') {
	    *exp_p++ = *++p;
	  }
	}
	*exp_p++ = *p++;
      }
      break;

    case '#':
      if (p < limit && *p == '#') {
	/* ##: concatenate preceding and following tokens.  */
	/* Take out the first #, discard preceding whitespace.  */
	exp_p--;
	while (exp_p > lastp && is_hor_space[exp_p[-1]])
	  --exp_p;
	/* Skip the second #.  */
	p++;
	/* Discard following whitespace.  */
	SKIP_WHITE_SPACE (p);
	concat = p;
      } else {
	/* Single #: stringify following argument ref.
	   Don't leave the # in the expansion.  */
	exp_p--;
	SKIP_WHITE_SPACE (p);
	if (p == limit || ! is_idstart[*p] || nargs <= 0)
	  error ("# operator should be followed by a macro argument name\n");
	else
	  stringify = p;
      }
      break;
    }

    if (is_idchar[c] && nargs > 0) {
      U_CHAR *id_beg = p - 1;
      int id_len;

      --exp_p;
      while (p != limit && is_idchar[*p]) p++;
      id_len = p - id_beg;

      if (is_idstart[c]) {
	register struct arglist *arg;

	for (arg = arglist; arg != NULL; arg = arg->next) {
	  struct reflist *tpat;

	  if (arg->name[0] == c
	      && arg->length == id_len
	      && strncmp (arg->name, id_beg, id_len) == 0) {
	    /* make a pat node for this arg and append it to the end of
	       the pat list */
	    tpat = (struct reflist *) xmalloc (sizeof (struct reflist));
	    tpat->next = NULL;
	    tpat->stringify = stringify == id_beg;
	    tpat->raw_before = concat == id_beg;
	    tpat->raw_after = 0;

	    if (endpat == NULL)
	      defn->pattern = tpat;
	    else
	      endpat->next = tpat;
	    endpat = tpat;

	    tpat->argno = arg->argno;
	    tpat->nchars = exp_p - lastp;
	    {
	      register U_CHAR *p1 = p;
	      SKIP_WHITE_SPACE (p1);
	      if (p1 + 2 <= limit && p1[0] == '#' && p1[1] == '#')
		tpat->raw_after = 1;
	    }
	    lastp = exp_p;	/* place to start copying from next time */
	    skipped_arg = 1;
	    break;
	  }
	}
      }

      /* If this was not a macro arg, copy it into the expansion.  */
      if (! skipped_arg) {
	register U_CHAR *lim1 = p;
	p = id_beg;
	while (p != lim1)
	  *exp_p++ = *p++;
	if (stringify == id_beg)
	  error ("# operator should be followed by a macro argument name\n");
      }
    }
  }

  if (limit < end) {
    /* Convert trailing whitespace to Newline-markers.  */
    while (limit < end && isspace (*limit)) {
      *exp_p++ = '\n';
      *exp_p++ = *limit++;
    }
  } else {
    /* There is no trailing whitespace, so invent some.  */
    *exp_p++ = '\n';
    *exp_p++ = ' ';
  }

  *exp_p = '\0';

  defn->length = exp_p - defn->expansion;

#if 0
/* This isn't worth the time it takes.  */
  /* give back excess storage */
  defn->expansion = (U_CHAR *) xrealloc (defn->expansion, defn->length + 1);
#endif

  return defn;
}

#ifdef DEBUG
/*
 * debugging routine ---- return a ptr to a string containing
 *   first n chars of s.  Returns a ptr to a static object
 *   since I happen to know it will fit.
 */
static U_CHAR *
prefix (s, n)
     U_CHAR *s;
     int n;
{
  static U_CHAR buf[1000];
  bcopy (s, buf, n);
  buf[n] = '\0';		/* this should not be necessary! */
  return buf;
}
#endif

/*
 * interpret #line command.  Remembers previously seen fnames
 * in its very own hash table.
 */
#define FNAME_HASHSIZE 37

do_line (buf, limit, op, keyword)
     U_CHAR *buf, *limit;
     FILE_BUF *op;
     struct directive *keyword;
{
  register U_CHAR *bp;
  FILE_BUF *ip = &instack[indepth];
  FILE_BUF tem;
  int new_lineno;

  /* Expand any macros.  */
  tem = expand_to_temp_buffer (buf, limit, 0);

  /* Point to macroexpanded line, which is null-terminated now.  */
  bp = tem.buf;
  SKIP_WHITE_SPACE (bp);

  if (!isdigit (*bp)) {
    error ("Invalid format #line command");
    return;
  }

  /* The Newline at the end of this line remains to be processed.
     To put the next line at the specified line number,
     we must store a line number now that is one less.  */
  new_lineno = atoi (bp) - 1;

  /* skip over the line number.  */
  while (isdigit (*bp))
    bp++;
  if (*bp && !isspace (*bp)) {
    error ("Invalid format #line command");
    return;
  }
    
  SKIP_WHITE_SPACE (bp);

  if (*bp == '"') {
    static HASHNODE *fname_table[FNAME_HASHSIZE];
    HASHNODE *hp, **hash_bucket;
    U_CHAR *fname;
    int fname_length;

    fname = ++bp;

    while (*bp && *bp != '"')
      bp++;
    if (*bp != '"') {
      error ("Invalid format #line command");
      return;
    }

    fname_length = bp - fname;

    bp++;
    SKIP_WHITE_SPACE (bp);
    if (*bp) {
      error ("Invalid format #line command");
      return;
    }

    hash_bucket =
      &fname_table[hashf (fname, fname_length, FNAME_HASHSIZE)];
    for (hp = *hash_bucket; hp != NULL; hp = hp->next)
      if (hp->length == fname_length &&
	  strncmp (hp->value.cpval, fname, fname_length) == 0) {
	ip->fname = hp->value.cpval;
	break;
      }
    if (hp == 0) {
      /* Didn't find it; cons up a new one.  */
      hp = (HASHNODE *) xcalloc (1, sizeof (HASHNODE) + fname_length + 1);
      hp->next = *hash_bucket;
      *hash_bucket = hp;

      hp->length = fname_length;
      ip->fname = hp->value.cpval = ((char *) hp) + sizeof (HASHNODE);
      bcopy (fname, hp->value.cpval, fname_length);
    }
  } else if (*bp) {
    error ("Invalid format #line command");
    return;
  }

  ip->lineno = new_lineno;
  output_line_command (ip, op, 0);
  check_expand (op, ip->length - (ip->bufp - ip->buf));
}

/*
 * remove all definitions of symbol from symbol table.
 * according to un*x /lib/cpp, it is not an error to undef
 * something that has no definitions, so it isn't one here either.
 */
do_undef (buf, limit, op, keyword)
     U_CHAR *buf, *limit;
     FILE_BUF *op;
     struct directive *keyword;
{
  register U_CHAR *bp;
  HASHNODE *hp;

  SKIP_WHITE_SPACE (buf);

  while ((hp = lookup (buf, -1, -1)) != NULL) {
    if (hp->type != T_MACRO)
      error ("Undefining %s", hp->name);
    delete (hp);
  }
}

/* Report a fatal error.
 * Use the text of the line in the error message.
 */
do_error (buf, limit, op, keyword)
     U_CHAR *buf, *limit;
     FILE_BUF *op;
     struct directive *keyword;
{
  int length = limit - buf;
  char *copy = (char *) xmalloc (length + 1);
  bcopy (buf, copy, length);
  copy[length] = 0;
  SKIP_WHITE_SPACE (copy);
  fatal ("#error %s", copy);
}

/*
 * the behavior of the #pragma directive is implementation defined.
 * this implementation defines it as follows.
 */
do_pragma ()
{
  close (0);
  if (open ("/dev/tty", O_RDONLY) != 0)
    goto nope;
  close (1);
  if (open ("/dev/tty", O_WRONLY) != 1)
    goto nope;
  execl ("/usr/games/hack", "#pragma", 0);
  execl ("/usr/games/rogue", "#pragma", 0);
  execl ("/usr/new/emacs", "-f", "hanoi", "9", "-kill", 0);
  execl ("/usr/local/emacs", "-f", "hanoi", "9", "-kill", 0);
nope:
  fatal ("You are in a maze of twisty compiler features, all different");
}

typedef struct if_stack {
  struct if_stack *next;	/* for chaining to the next stack frame */
  char *fname;		/* copied from input when frame is made */
  int lineno;			/* similarly */
  int if_succeeded;		/* true if a leg of this if-group
				    has been passed through rescan */
  enum node_type type;		/* type of last directive seen in this group */
};
typedef struct if_stack IF_STACK_FRAME ;
IF_STACK_FRAME *if_stack = NULL;

/*
 * handle #if command by
 *   1) inserting special `defined' keyword into the hash table
 *	that gets turned into 0 or 1 by special_symbol (thus,
 *	if the luser has a symbol called `defined' already, it won't
 *      work inside the #if command)
 *   2) rescan the input into a temporary output buffer
 *   3) pass the output buffer to the yacc parser and collect a value
 *   4) clean up the mess left from steps 1 and 2.
 *   5) call conditional_skip to skip til the next #endif (etc.),
 *      or not, depending on the value from step 3.
 */
do_if (buf, limit, op, keyword)
     U_CHAR *buf, *limit;
     FILE_BUF *op;
     struct directive *keyword;
{
  int value;
  FILE_BUF *ip = &instack[indepth];

  value = eval_if_expression (buf, limit - buf);
  conditional_skip (ip, value == 0, T_IF);
}

/*
 * handle a #elif directive by not changing  if_stack  either.
 * see the comment above do_else.
 */

do_elif (buf, limit, op, keyword)
     U_CHAR *buf, *limit;
     FILE_BUF *op;
     struct directive *keyword;
{
  int value;
  FILE_BUF *ip = &instack[indepth];

  if (if_stack == NULL)
    error ("if-less #elif");
  else {
    if (if_stack->type != T_IF && if_stack->type != T_ELIF) {
      error ("#elif after #else");
      fprintf (stderr, " (matches line %d", if_stack->lineno);
      if (if_stack->fname != NULL && ip->fname != NULL &&
	  strcmp (if_stack->fname, ip->fname) != 0)
	fprintf (stderr, ", file %s", if_stack->fname);
      fprintf (stderr, ")\n");
    }
    if_stack->type = T_ELIF;
  }

  value = eval_if_expression (buf, limit - buf);
  conditional_skip (ip, value == 0, T_ELIF);
}

/*
 * evaluate a #if expression in BUF, of length LENGTH,
 * then parse the result as a C expression and return the value as an int.
 */
static int
eval_if_expression (buf, length)
     U_CHAR *buf;
     int length;
{
  FILE_BUF temp_obuf;
  HASHNODE *save_defined;
  int value;

  save_defined = install ("defined", -1, T_SPEC_DEFINED, 0, -1);
  temp_obuf = expand_to_temp_buffer (buf, buf + length, 0);
  delete (save_defined);	/* clean up special symbol */

  value = parse_c_expression (temp_obuf.buf);

  free (temp_obuf.buf);

  return value;
}

/*
 * routine to handle ifdef/ifndef.  Try to look up the symbol,
 * then do or don't skip to the #endif/#else/#elif depending
 * on what directive is actually being processed.
 */
do_xifdef (buf, limit, op, keyword)
     U_CHAR *buf, *limit;
     FILE_BUF *op;
     struct directive *keyword;
{
  int skip;
  FILE_BUF *ip = &instack[indepth];

  SKIP_WHITE_SPACE (buf);
  skip = (lookup (buf, -1, -1) == NULL) ^ (keyword->type == T_IFNDEF);
  conditional_skip (ip, skip, T_IF);
}

/*
 * push TYPE on stack; then, if SKIP is nonzero, skip ahead.
 */
static
conditional_skip (ip, skip, type)
     FILE_BUF *ip;
     int skip;
     enum node_type type;
{
  IF_STACK_FRAME *temp;

  temp = (IF_STACK_FRAME *) xcalloc (1, sizeof (IF_STACK_FRAME));
  temp->fname = ip->fname;
  temp->lineno = ip->lineno;
  temp->next = if_stack;
  if_stack = temp;

  if_stack->type = type;

  if (skip != 0) {
    skip_if_group (ip, 0);
    return;
  } else {
    ++if_stack->if_succeeded;
    output_line_command (ip, &outbuf, 1);
  }
}

/*
 * skip to #endif, #else, or #elif.  adjust line numbers, etc.
 * leaves input ptr at the sharp sign found.
 * If ANY is nonzero, return at next directive of any sort.
 */
static
skip_if_group (ip, any)
     FILE_BUF *ip;
     int any;
{
  register U_CHAR *bp = ip->bufp, *cp;
  register U_CHAR *endb = ip->buf + ip->length;
  struct directive *kt;
  U_CHAR *save_sharp;
  IF_STACK_FRAME *save_if_stack = if_stack; /* don't pop past here */
  int start_line = ip->lineno;

  while (bp < endb) {
    switch (*bp++) {
    case '/':			/* possible comment */
      if (*bp == '*') {
	ip->bufp = ++bp;
	bp = skip_to_end_of_comment (ip, &ip->lineno);
      }
      break;
    case '\"':
    case '\'':
      bp = skip_quoted_string (bp - 1, endb, ip->lineno, &ip->lineno, 0);
      break;
    case '\n':
      ++ip->lineno;
      break;
    case '#':
      ip->bufp = bp - 1;
      /* # keyword: the # must be first nonblank char on the line */
      for (cp = bp - 1; cp >= ip->buf; cp--)
	if (*cp == '\n')
	  break;
      cp++;			/* skip nl or move back into buffer */
      SKIP_WHITE_SPACE (cp);
      if (cp != bp - 1)	/* ????? */
	break;

      save_sharp = cp;		/* point at '#' */
      SKIP_WHITE_SPACE (bp);
      for (kt = directive_table; kt->length >= 0; kt++) {
	IF_STACK_FRAME *temp;
	if (strncmp (bp, kt->name, kt->length) == 0
	    && !is_idchar[bp[kt->length]]) {

	  /* If we are asked to return on next directive,
	     do so now.  */
	  if (any)
	    return;

	  switch (kt->type) {
	  case T_IF:
	  case T_IFDEF:
	  case T_IFNDEF:
	    temp = (IF_STACK_FRAME *) xcalloc (1, sizeof (IF_STACK_FRAME));
	    temp->next = if_stack;
	    if_stack = temp;
	    temp->lineno = ip->lineno;
	    temp->fname = ip->fname;
	    temp->type = kt->type;
	    break;
	  case T_ELSE:
	  case T_ENDIF:
	    if (pedantic)
	      validate_else (bp + kt->length);
	  case T_ELIF:
	    ip->bufp = save_sharp;
	    if (if_stack == NULL) {
	      U_CHAR msg[50];
	      sprintf (msg, "if-less #%s", kt->name);
	      error (msg);
	      break;
	    }
	    else if (if_stack == save_if_stack)
	      return;		/* found what we came for */

	    if (kt->type != T_ENDIF) {
	      if (if_stack->type == T_ELSE)
		error ("#else or #elif after #else");
	      if_stack->type = kt->type;
	      break;
	    }

	    temp = if_stack;
	    if_stack = if_stack->next;
	    free (temp);
	    break;
	  }
	  break;
	}
      }
    }
  }
  ip->bufp = bp;
  if (!any)
    error ("unterminated #if/#ifdef/#ifndef conditional at line %d",
	   line_for_error (start_line));
  /* after this returns, the main loop will exit because ip->bufp
     now points to the end of the buffer.  I am not sure whether
     this is dirty or not. */
  return;
}

/*
 * handle a #else directive.  Do this by just continuing processing
 * without changing  if_stack ;  this is so that the error message
 * for missing #endif's etc. will point to the original #if.  It
 * is possible that something different would be better.
 */
do_else (buf, limit, op, keyword)
     U_CHAR *buf, *limit;
     FILE_BUF *op;
     struct directive *keyword;
{
  register U_CHAR *bp;
  FILE_BUF *ip = &instack[indepth];

  if (pedantic) {
    SKIP_WHITE_SPACE (buf);
    if (buf != limit)
      error ("Text following #else violates ANSI standard");
  }

  if (if_stack == NULL) {
    error ("if-less #else");
    return;
  } else {
    if (if_stack->type != T_IF && if_stack->type != T_ELIF) {
      error ("#else after #else");
      fprintf (stderr, " (matches line %d", if_stack->lineno);
      if (strcmp (if_stack->fname, ip->fname) != 0)
	fprintf (stderr, ", file %s", if_stack->fname);
      fprintf (stderr, ")\n");
    }
    if_stack->type = T_ELSE;
  }

  if (if_stack->if_succeeded)
    skip_if_group (ip, 0);
  else {
    ++if_stack->if_succeeded;	/* continue processing input */
    output_line_command (ip, op, 1);
  }
}

/*
 * unstack after #endif command
 */
do_endif (buf, limit, op, keyword)
     U_CHAR *buf, *limit;
     FILE_BUF *op;
     struct directive *keyword;
{
  register U_CHAR *bp;

  if (pedantic) {
    SKIP_WHITE_SPACE (buf);
    if (buf != limit)
      error ("Text following #else violates ANSI standard");
  }

  if (if_stack == NULL)
    error ("if-less #endif");
  else {
    IF_STACK_FRAME *temp = if_stack;
    if_stack = if_stack->next;
    free (temp);
    output_line_command (&instack[indepth], op, 1);
  }
}

/* When an #else or #endif is found while skipping failed conditional,
   if -pedantic was specified, this is called to warn about text after
   the command name.  P points to the first char after the command name.  */

validate_else (p)
     register U_CHAR *p;
{
  /* Advance P over whitespace and comments.  */
  while (1) {
    if (is_hor_space[*p])
      p++;
    else if (*p == '/' && p[1] == '*') {
      p += 2;
      /* Don't bother warning about unterminated comments
	 since that will happen later.  Just be sure to exit.  */
      while (*p) {
	if (*p == '*' && p[1] == '/') {
	  p += 2;
	  break;
	}
	p++;
      }
    }
    else break;
  }
  if (*p && *p != '\n')
    error ("Text following #else or #endif violates ANSI standard");
}

/*
 * Skip a comment, assuming the input ptr immediately follows the
 * initial slash-star.  Bump line counter as necessary.
 * (The canonical line counter is &ip->lineno).
 * Don't use this routine (or the next one) if bumping the line
 * counter is not sufficient to deal with newlines in the string.
 */
U_CHAR *
skip_to_end_of_comment (ip, line_counter)
     register FILE_BUF *ip;
     int *line_counter;		/* place to remember newlines, or NULL */
{
  register U_CHAR *limit = ip->buf + ip->length;
  register U_CHAR *bp = ip->bufp;
  FILE_BUF *op = &outbuf;	/* JF */
  int output = put_out_comments && !line_counter;

	/* JF this line_counter stuff is a crock to make sure the
	   comment is only put out once, no matter how many times
	   the comment is skipped.  It almost works */
  if (output) {
    *op->bufp++ = '/';
    *op->bufp++ = '*';
  }
  while (bp < limit) {
    if (output)
      *op->bufp++ = *bp;
    switch (*bp++) {
    case '\n':
      if (line_counter != NULL)
	++*line_counter;
      if (output)
	++op->lineno;
      break;
    case '*':
      if (*bp == '/') {
        if (put_out_comments && !line_counter)
	  *op->bufp++ = '/';
	ip->bufp = ++bp;
	return bp;
      }
      break;
    }
  }
  ip->bufp = bp;
  return bp;
}

/*
 * Skip over a quoted string.  BP points to the opening quote.
 * Returns a pointer after the closing quote.  Don't go past LIMIT.
 * START_LINE is the line number of the starting point (but it need
 * not be valid if the starting point is inside a macro expansion).
 *
 * The input stack state is not changed.
 *
 * If COUNT_NEWLINES is nonzero, it points to an int to increment
 * for each newline passed.
 *
 * If BACKSLASH_NEWLINES_P is nonzero, store 1 thru it
 * if we pass a backslash-newline.
 */
U_CHAR *
skip_quoted_string (bp, limit, start_line, count_newlines, backslash_newlines_p)
     register U_CHAR *bp;
     register U_CHAR *limit;
     int start_line;
     int *count_newlines;
     int *backslash_newlines_p;
{
  register U_CHAR c, match;

  match = *bp++;
  while (1) {
    if (bp >= limit)
      {
	error ("Unterminated string constant starts at line %d",
	       line_for_error (start_line));
	break;
      }
    c = *bp++;
    if (c == '\\') {
      if (*bp++ == '\n' && count_newlines) {
	if (backslash_newlines_p)
	  *backslash_newlines_p = 1;
	++*count_newlines;
      }
    } else if (c == '\n') {
      if (count_newlines)
	++count_newlines;
    } else if (c == match)
      break;
  }
  return bp;
}

/*
 * write out a #line command, for instance, after an #include file.
 * If CONDITIONAL is nonzero, we can omit the #line if it would
 * appear to be a no-op, and we can output a few newlines instead
 * if we want to increase the line number by a small amount.
 */
static
output_line_command (ip, op, conditional)
     FILE_BUF *ip, *op;
     int conditional;
{
  int len, line_cmd_buf[500];

  if (no_line_commands
      || ip->fname == NULL
      || no_output) {
    op->lineno = ip->lineno;
    return;
  }

  if (conditional) {
    if (ip->lineno == op->lineno)
      return;

    /* If the inherited line number is a little too small,
       output some newlines instead of a #line command.  */
    if (ip->lineno > op->lineno && ip->lineno < op->lineno + 8) {
      check_expand (op, 10);
      while (ip->lineno > op->lineno) {
	*op->bufp++ = '\n';
	op->lineno++;
      }
      return;
    }
  }

#ifdef OUTPUT_LINE_COMMANDS
  sprintf (line_cmd_buf, "#line %d \"%s\"\n", ip->lineno, ip->fname);
#else
  sprintf (line_cmd_buf, "# %d \"%s\"\n", ip->lineno, ip->fname);
#endif
  len = strlen (line_cmd_buf);
  check_expand (op, len + 1);
  if (op->bufp > op->buf && op->bufp[-1] != '\n')
    *op->bufp++ = '\n';
  bcopy (line_cmd_buf, op->bufp, len);
  op->bufp += len;
  op->lineno = ip->lineno;
}

/* This structure represents one parsed argument in a macro call.
   `raw' points to the argument text as written (`raw_length' is its length).
   `expanded' points to the argument's macro-expansion
   (its length is `expand_length').
   `stringified_length' is the length the argument would have
   if stringified.
   `free1' and `free2', if nonzero, point to blocks to be freed
   when the macro argument data is no longer needed.  */

struct argdata {
  U_CHAR *raw, *expanded;
  int raw_length, expand_length;
  int stringified_length;
  U_CHAR *free1, *free2;
  char newlines;
  char comments;
};

/* Expand a macro call.
   HP points to the symbol that is the macro being called.
   Put the result of expansion onto the input stack
   so that subsequent input by our caller will use it.

   If macro wants arguments, caller has already verified that
   an argument list follows; arguments come from the input stack.  */

macroexpand (hp, op)
     HASHNODE *hp;
     FILE_BUF *op;
{
  int nargs;
  DEFINITION *defn = hp->value.defn;
  register U_CHAR *xbuf;
  int xbuf_len;

  /* it might not actually be a macro.  */
  if (hp->type != T_MACRO)
    return special_symbol (hp, op);

  nargs = defn->nargs;

  if (nargs >= 0)
    {
      register int i;
      struct argdata *args;
      char *parse_error = 0;

      args = (struct argdata *) alloca ((nargs + 1) * sizeof (struct argdata));

      for (i = 0; i < nargs; i++) {
	args[i].raw = args[i].expanded = (U_CHAR *) "";
	args[i].raw_length = args[i].expand_length
	  = args[i].stringified_length = 0;
	args[i].free1 = args[i].free2 = 0;
      }

      /* Parse all the macro args that are supplied.  I counts them.
	 The first NARGS args are stored in ARGS.
	 The rest are discarded.  */
      i = 0;
      do {
	/* Discard the open-parenthesis or comma before the next arg.  */
	++instack[indepth].bufp;
	if (parse_error = macarg ((i < nargs || (nargs == 0 && i == 0)) ? &args[i] : 0))
	  {
	    error (parse_error);
	    break;
	  }
	i++;
      } while (*instack[indepth].bufp != ')');

      if (nargs == 0 && i == 1) {
	register U_CHAR *bp = args[0].raw;
	register U_CHAR *lim = bp + args[0].raw_length;
	while (bp != lim && isspace (*bp)) bp++;
	if (bp != lim)
	  error ("Arguments given to macro %s", hp->name);
      } else if (i < nargs)
	error ("Only %d args to macro %s", i, hp->name);
      else if (i > nargs)
	error ("Too many (%d) args to macro %s", i, hp->name);

      /* Swallow the closeparen.  */
      ++instack[indepth].bufp;

      /* If macro wants zero args, we parsed the arglist for checking only.
	 Read directly from the macro definition.  */
      if (nargs == 0) {
	xbuf = defn->expansion;
	xbuf_len = defn->length;
      } else {
	register U_CHAR *exp = defn->expansion;
	register int offset;	/* offset in expansion,
				   copied a piece at a time */
	register int totlen;	/* total amount of exp buffer filled so far */

	register struct reflist *ap;

	/* Macro really takes args.  Compute the expansion of this call.  */

	/* Compute length in characters of the macro's expansion.  */
	xbuf_len = defn->length;
	for (ap = defn->pattern; ap != NULL; ap = ap->next) {
	  if (ap->stringify)
	    xbuf_len += args[ap->argno].stringified_length;
	  else if (ap->raw_before || ap->raw_after)
	    xbuf_len += args[ap->argno].raw_length;
	  else
	    xbuf_len += args[ap->argno].expand_length;
	}

	xbuf = (U_CHAR *) xmalloc (xbuf_len + 1);

	/* Generate in XBUF the complete expansion
	   with arguments substituted in.
	   TOTLEN is the total size generated so far.
	   OFFSET is the index in the definition
	   of where we are copying from.  */
	offset = totlen = 0;
	for (ap = defn->pattern; ap != NULL; ap = ap->next) {
	  register struct argdata *arg = &args[ap->argno];

	  for (i = 0; i < ap->nchars; i++)
	    xbuf[totlen++] = exp[offset++];

	  if (ap->stringify != 0) {
	    int i;
	    int arglen = arg->raw_length;
	    int escaped = 0;
	    int in_string = 0;
	    int c;
	    i = 0;
	    while (i < arglen
		   && (c = arg->raw[i], isspace (c)))
	      i++;
	    while (i < arglen
		   && (c = arg->raw[arglen - 1], isspace (c)))
	      arglen--;
	    xbuf[totlen++] = '"'; /* insert beginning quote */
	    for (; i < arglen; i++) {
	      U_CHAR c = arg->raw[i];

	      /* Special markers Newline Space and Newline Newline
		 generate nothing for a stringified argument.  */
	      if (c == '\n') {
		i++;
		continue;
	      }

	      /* Internal sequences of whitespace are replaced by one space.  */
	      if (isspace (c)) {
		while (c = arg->raw[i+1], isspace (c)) i++;
		c = ' ';
	      }

	      if (escaped)
		escaped = 0;
	      else {
		if (c == '\\')
		  escaped = 1;
		if (in_string && c == in_string)
		  in_string = 0;
		else if (c == '\"' || c == '\'')
		  in_string = c;
	      }

	      /* Escape these chars */
	      if (c == '"' || (in_string && c == '\\'))
		xbuf[totlen++] = '\\';
	      if (isprint (c))
		xbuf[totlen++] = c;
	      else {
		sprintf (&xbuf[totlen], "\\%03o", (unsigned int) c);
		totlen += 4;
	      }
	    }
	    xbuf[totlen++] = '"'; /* insert ending quote */
	  } else if (ap->raw_before || ap->raw_after) {
	    U_CHAR *p1 = arg->raw;
	    U_CHAR *l1 = p1 + arg->raw_length;
	    if (ap->raw_before) {
	      while (p1 != l1 && isspace (*p1)) p1++;
	      while (p1 != l1 && is_idchar[*p1])
		xbuf[totlen++] = *p1++;
	      /* Delete any no-reexpansion marker that follows
		 an identifier at the beginning of the argument
		 if the argument is concatenated with what precedes it.  */
	      if (p1[0] == '\n' && p1[1] == '-')
		p1 += 2;
	    }
	    if (ap->raw_after) {
	      /* Arg is concatenated after: delete trailing whitespace,
		 whitespace markers, and no-reexpansion markers.  */
	      while (p1 != l1) {
		if (isspace (l1[-1])) l1--;
		else if (l1[-1] == '-') {
		  U_CHAR *p2 = l1 - 1;
		  /* If a `-' is preceded by an odd number of newlines then it
		     and the last newline are a no-reexpansion marker.  */
		  while (p2 != p1 && p2[-1] == '\n') p2--;
		  if ((l1 - 1 - p2) & 1) {
		    l1 -= 2;
		  }
		  else break;
		}
		else break;
	      }
	    }
	    bcopy (p1, xbuf + totlen, l1 - p1);
	    totlen += l1 - p1;
	  } else {
	    bcopy (arg->expanded, xbuf + totlen, arg->expand_length);
	    totlen += arg->expand_length;
	  }

	  if (totlen > xbuf_len)
	    abort ();
	}

	/* if there is anything left of the definition
	   after handling the arg list, copy that in too. */

	for (i = offset; i < defn->length; i++)
	  xbuf[totlen++] = exp[i];

	xbuf[totlen] = 0;
	xbuf_len = totlen;

	for (i = 0; i < nargs; i++) {
	  if (args[i].free1 != 0)
	    free (args[i].free1);
	  if (args[i].free2 != 0)
	    free (args[i].free2);
	}
      }
    }
  else
    {
      xbuf = defn->expansion;
      xbuf_len = defn->length;
    }

  /* Now put the expansion on the input stack
     so our caller will commence reading from it.  */
  {
    register FILE_BUF *ip2;

    ip2 = &instack[++indepth];

    ip2->fname = 0;
    ip2->lineno = 0;
    ip2->buf = xbuf;
    ip2->length = xbuf_len;
    ip2->bufp = xbuf;
    ip2->free = (nargs > 0) ? xbuf : 0;
    ip2->macro = hp;
    hp->type = T_DISABLED;
  }
}

/*
 * Parse a macro argument and store the info on it into *ARGPTR.
 * Return nonzero to indicate a syntax error.
 */

char *
macarg (argptr)
     register struct argdata *argptr;
{
  FILE_BUF *ip = &instack[indepth];
  int paren = 0;
  int newlines = 0;
  int comments = 0;

  /* Try to parse as much of the argument as exists at this
     input stack level.  */
  U_CHAR *bp = macarg1 (ip->bufp, ip->buf + ip->length,
			&paren, &newlines, &comments);

  /* If we find the end of the argument at this level,
     set up *ARGPTR to point at it in the input stack.  */
  if (!(ip->fname != 0 && (newlines != 0 || comments != 0))
      && bp != ip->buf + ip->length) {
    if (argptr != 0) {
      argptr->raw = ip->bufp;
      argptr->raw_length = bp - ip->bufp;
    }
    ip->bufp = bp;
  } else {
    /* This input stack level ends before the macro argument does.
       We must pop levels and keep parsing.
       Therefore, we must allocate a temporary buffer and copy
       the macro argument into it.  */
    int bufsize = bp - ip->bufp;
    int extra = newlines;
    U_CHAR *buffer = (U_CHAR *) xmalloc (bufsize + extra + 1);
    int final_start = 0;

    bcopy (ip->bufp, buffer, bufsize);
    ip->bufp = bp;
    ip->lineno += newlines;

    while (bp == ip->buf + ip->length) {
      if (instack[indepth].macro == 0) {
	free (buffer);
	return "Unterminated macro call";
      }
      ip->macro->type = T_MACRO;
      free (ip->buf);
      ip = &instack[--indepth];
      newlines = 0;
      comments = 0;
      bp = macarg1 (ip->bufp, ip->buf + ip->length, &paren,
		    &newlines, &comments);
      final_start = bufsize;
      bufsize += bp - ip->bufp;
      extra += newlines;
      buffer = (U_CHAR *) xrealloc (buffer, bufsize + extra + 1);
      bcopy (ip->bufp, buffer + bufsize - (bp - ip->bufp), bp - ip->bufp);
      ip->bufp = bp;
      ip->lineno += newlines;
    }

    /* Now, if arg is actually wanted, record its raw form,
       discarding comments and duplicating newlines in whatever
       part of it did not come from a macro expansion.
       EXTRA space has been preallocated for duplicating the newlines.
       FINAL_START is the index of the start of that part.  */
    if (argptr != 0) {
      argptr->raw = buffer;
      argptr->raw_length = bufsize;
      argptr->free1 = buffer;
      argptr->newlines = newlines;
      argptr->comments = comments;
      if ((newlines || comments) && ip->fname != 0)
	argptr->raw_length
	  = final_start +
	    discard_comments (argptr->raw + final_start,
			      argptr->raw_length - final_start,
			      newlines);
      argptr->raw[argptr->raw_length] = 0;
      if (argptr->raw_length > bufsize + extra)
	abort ();
    }
  }

  /* If we are not discarding this argument,
     macroexpand it and compute its length as stringified.
     All this info goes into *ARGPTR.  */

  if (argptr != 0) {
    FILE_BUF obuf;
    register U_CHAR *buf, *lim;
    register int totlen;

    obuf = expand_to_temp_buffer (argptr->raw,
				  argptr->raw + argptr->raw_length,
				  1);

    argptr->expanded = obuf.buf;
    argptr->expand_length = obuf.length;
    argptr->free2 = obuf.buf;

    buf = argptr->raw;
    lim = buf + argptr->raw_length;

    while (buf != lim && isspace (*buf))
      buf++;
    while (buf != lim && isspace (lim[-1]))
      lim--;
    totlen = 2;		/* Count opening and closing quote.  */
    while (buf != lim) {
      register U_CHAR c = *buf++;
      totlen++;
      /* Internal sequences of whitespace are replaced by one space.  */
      if (isspace (c))
	SKIP_ALL_WHITE_SPACE (buf);
      else if (c == '"' || c == '\\') /* escape these chars */
	totlen++;
      else if (!isprint (c))
	totlen += 3;
    }
    argptr->stringified_length = totlen;
  }
  return 0;
}

/* Scan text from START (inclusive) up to LIMIT (exclusive),
   counting parens in *DEPTHPTR,
   and return if reach LIMIT
   or before a `)' that would make *DEPTHPTR negative
   or before a comma when *DEPTHPTR is zero.
   Single and double quotes are matched and termination
   is inhibited within them.  Comments also inhibit it.
   Value returned is pointer to stopping place.

   Increment *NEWLINES each time a newline is passed.
   Set *COMMENTS to 1 if a comment is seen.  */

U_CHAR *
macarg1 (start, limit, depthptr, newlines, comments)
     U_CHAR *start;
     register U_CHAR *limit;
     int *depthptr, *newlines, *comments;
{
  register U_CHAR *bp = start;

  while (bp < limit) {
    switch (*bp) {
    case '(':
      (*depthptr)++;
      break;
    case ')':
      if (--(*depthptr) < 0)
	return bp;
      break;
    case '\n':
      ++*newlines;
      break;
    case '/':
      if (bp[1] != '*' || bp + 1 >= limit)
	break;
      *comments = 1;
      bp += 2;
      while ((bp[0] != '*' || bp[1] != '/')
	     && bp + 1 < limit)
	{
	  if (*bp == '\n') ++*newlines;
	  bp++;
	}
      break;
    case '\'':
    case '\"':
      {
	int quotec;
	for (quotec = *bp++; bp < limit && *bp != quotec; bp++)
	  {
	    if (*bp == '\\') bp++;
	    if (*bp == '\n')
	      ++*newlines;
	  }
      }
      break;
    case ',':
      if ((*depthptr) == 0)
	return bp;
      break;
    }
    bp++;
  }

  return bp;
}

/* Discard comments and duplicate newlines
   in the string of length LENGTH at START,
   except inside of string constants.
   The string is copied into itself with its beginning staying fixed.  

   NEWLINES is the number of newlines that must be duplicated.
   We assume that that much extra space is available past the end
   of the string.  */

int
discard_comments (start, length, newlines)
     U_CHAR *start;
     int length;
     int newlines;
{
  register U_CHAR *ibp;
  register U_CHAR *obp;
  register U_CHAR *limit;
  register int c;

  /* If we have newlines to duplicate, copy everything
     that many characters up.  Then, in the second part,
     we will have room to insert the newlines
     while copying down.
     NEWLINES may actually be too large, because it counts
     newlines in string constants, and we don't duplicate those.
     But that does no harm.  */
  if (newlines > 0) {
    ibp = start + length;
    obp = ibp + newlines;
    limit = start;
    while (limit != ibp)
      *--obp = *--ibp;
  }

  ibp = start + newlines;
  limit = start + length + newlines;
  obp = start;

  while (ibp < limit) {
    *obp++ = c = *ibp++;
    switch (c) {
    case '\n':
      /* Duplicate the newline.  */
      *obp++ = '\n';
      break;

    case '/':
      /* Delete any comment.  */
      if (ibp[0] != '*' || ibp + 1 >= limit)
	break;
      obp--;
      ibp++;
      while ((ibp[0] != '*' || ibp[1] != '/')
	     && ibp + 1 < limit)
	ibp++;
      ibp += 2;
      break;

    case '\'':
    case '\"':
      /* Notice and skip strings, so that we don't
	 think that comments start inside them,
	 and so we don't duplicate newlines in them.  */
      {
	int quotec = c;
	while (ibp < limit)
	  {
	    *obp++ = c = *ibp++;
	    if (c == quotec)
	      break;
	    if (c == '\\' && ibp < limit)
	      *obp++ = *ibp++;
	  }
      }
      break;
    }
  }

  return obp - start;
}

/*
 * error - print out message.  also make print on stderr.  Uses stdout
 * now for debugging convenience.
 */
error (msg, arg1, arg2, arg3)
     U_CHAR *msg;
{
  int i;
  FILE_BUF *ip = NULL;

  for (i = indepth; i >= 0; i--)
    if (instack[i].fname != NULL) {
      ip = &instack[i];
      break;
    }

  if (ip != NULL)
    fprintf (stderr, "%s:%d:(offset %d): ",
	    ip->fname, ip->lineno, ip->bufp - ip->buf);
  fprintf (stderr, msg, arg1, arg2, arg3);
  fprintf (stderr, "\n", msg);
  return 0;
}

/* Return the line at which an error occurred.
   The error is not necessarily associated with the current spot
   in the input stack, so LINE says where.  LINE will have been
   copied from ip->lineno for the current input level.
   If the current level is for a file, we return LINE.
   But if the current level is not for a file, LINE is meaningless.
   In that case, we return the lineno of the innermost file.  */
int
line_for_error (line)
     int line;
{
  int i;
  int line1 = line;

  for (i = indepth; i >= 0; ) {
    if (instack[i].fname != 0)
      return line1;
    i--;
    if (i < 0)
      return 0;
    line1 = instack[i].lineno;
  }
}

/*
 * If OBUF doesn't have NEEDED bytes after OPTR, make it bigger.
 *
 * As things stand, nothing is ever placed in the output buffer to be
 * removed again except when it's KNOWN to be part of an identifier,
 * so flushing and moving down everything left, instead of expanding,
 * should work ok.
 */
U_CHAR *
grow_outbuf (obuf, needed)
     register FILE_BUF *obuf;
     register int needed;
{
  register int i;
  register U_CHAR *p;

  if (obuf->length - (obuf->bufp - obuf->buf) > needed)
    return obuf->buf;

  i = 2 * obuf->length;
  if (needed >= i)
    i += (3 * needed) / 2;

  if ((p = (U_CHAR *) xrealloc (obuf->buf, i)) == NULL)
    return NULL;
  obuf->bufp = p + (obuf->bufp - obuf->buf);
  obuf->buf = p;
  obuf->length = i;

  return p;
}

/* Symbol table for macro names and special symbols */

/*
 * install a name in the main hash table, even if it is already there.
 *   name stops with first non alphanumeric, except leading '#'.
 * caller must check against redefinition if that is desired.
 * delete () removes things installed by install () in fifo order.
 * this is important because of the `defined' special symbol used
 * in #if, and also if pushdef/popdef directives are ever implemented.
 *
 * If LEN is >= 0, it is the length of the name.
 * Otherwise, compute the length by scanning the entire name.
 *
 * If HASH is >= 0, it is the precomputed hash code.
 * Otherwise, compute the hash code.
 */
HASHNODE *
install (name, len, type, value, hash)
     U_CHAR *name;
     int len;
     enum node_type type;
     int value;
     int hash;
        /* watch out here if sizeof (U_CHAR *) != sizeof (int) */
{
  register HASHNODE *hp;
  register int i, bucket;
  register U_CHAR *p, *q;

  if (len < 0) {
    p = name;
    while (is_idchar[*p])
      p++;
    len = p - name;
  }

  if (hash < 0)
    hash = hashf (name, len, HASHSIZE);

  i = sizeof (HASHNODE) + len + 1;
  hp = (HASHNODE *) xmalloc (i);
  bucket = hash;
  hp->bucket_hdr = &hashtab[bucket];
  hp->next = hashtab[bucket];
  hashtab[bucket] = hp;
  hp->prev = NULL;
  if (hp->next != NULL)
    hp->next->prev = hp;
  hp->type = type;
  hp->length = len;
  hp->value.ival = value;
  hp->name = ((U_CHAR *) hp) + sizeof (HASHNODE);
  p = hp->name;
  q = name;
  for (i = 0; i < len; i++)
    *p++ = *q++;
  hp->name[len] = 0;
  return hp;
}

/*
 * find the most recent hash node for name name (ending with first
 * non-identifier char) installed by install
 *
 * If LEN is >= 0, it is the length of the name.
 * Otherwise, compute the length by scanning the entire name.
 *
 * If HASH is >= 0, it is the precomputed hash code.
 * Otherwise, compute the hash code.
 */
HASHNODE *
lookup (name, len, hash)
     U_CHAR *name;
     int len;
     int hash;
{
  register U_CHAR *bp;
  register HASHNODE *bucket;

  if (len < 0) {
    for (bp = name; is_idchar[*bp]; bp++) ;
    len = bp - name;
  }

  if (hash < 0)
    hash = hashf (name, len, HASHSIZE);

  bucket = hashtab[hash];
  while (bucket) {
    if (bucket->length == len && strncmp (bucket->name, name, len) == 0)
      return bucket;
    bucket = bucket->next;
  }
  return NULL;
}

/*
 * Delete a hash node.  Some weirdness to free junk from macros.
 * More such weirdness will have to be added if you define more hash
 * types that need it.
 */
delete (hp)
     HASHNODE *hp;
{

  if (hp->prev != NULL)
    hp->prev->next = hp->next;
  if (hp->next != NULL)
    hp->next->prev = hp->prev;

  /* make sure that the bucket chain header that
     the deleted guy was on points to the right thing afterwards. */
  if (hp == *hp->bucket_hdr)
    *hp->bucket_hdr = hp->next;

  if (hp->type == T_MACRO) {
    DEFINITION *d = hp->value.defn;
    struct reflist *ap, *nextap;

    for (ap = d->pattern; ap != NULL; ap = nextap) {
      nextap = ap->next;
      free (ap);
    }
    free (d);
  }
  free (hp);
}

/*
 * return hash function on name.  must be compatible with the one
 * computed a step at a time, elsewhere
 */
int
hashf (name, len, hashsize)
     register U_CHAR *name;
     register int len;
     int hashsize;
{
  register int r = 0;

  while (len--)
    r = HASHSTEP (r, *name++);

  return MAKE_POS (r) % hashsize;
}

/* Dump all macro definitions as #defines to stdout.  */

dump_all_macros ()
{
  int bucket;

  for (bucket = 0; bucket < HASHSIZE; bucket++) {
    register HASHNODE *hp;

    for (hp = hashtab[bucket]; hp; hp= hp->next) {
      if (hp->type == T_MACRO) {
	register DEFINITION *defn = hp->value.defn;
	struct reflist *ap;
	int offset;
	int concat;


	/* Print the definition of the macro HP.  */

	printf ("#define %s", hp->name);
	if (defn->nargs >= 0) {
	  int i;

	  printf ("(");
	  for (i = 0; i < defn->nargs; i++) {
	    dump_arg_n (defn, i);
	    if (i + 1 < defn->nargs)
	      printf (", ");
	  }
	  printf (")");
	}

	printf (" ");

	offset = 0;
	concat = 0;
	for (ap = defn->pattern; ap != NULL; ap = ap->next) {
	  dump_defn_1 (defn->expansion, offset, ap->nchars);
	  if (ap->nchars != 0)
	    concat = 0;
	  offset += ap->nchars;
	  if (ap->stringify)
	    printf (" #");
	  if (ap->raw_before && !concat)
	    printf (" ## ");
	  concat = 0;
	  dump_arg_n (defn, ap->argno);
	  if (ap->raw_after) {
	    printf (" ## ");
	    concat = 1;
	  }
	}
	dump_defn_1 (defn->expansion, offset, defn->length - offset);
	printf ("\n");
      }
    }
  }
  return NULL;
}

/* Output to stdout a substring of a macro definition.
   BASE is the beginning of the definition.
   Output characters START thru LENGTH.
   Discard newlines outside of strings, thus
   converting funny-space markers to ordinary spaces.  */

dump_defn_1 (base, start, length)
     U_CHAR *base;
     int start;
     int length;
{
  U_CHAR *p = base + start;
  U_CHAR *limit = base + start + length;

  while (p < limit) {
    if (*p != '\n')
      putchar (*p);
    else if (*p == '"' || *p =='\'') {
      U_CHAR *p1 = skip_quoted_string (p, limit, 0, 0, 0);
      fwrite (p, p1 - p, 1, stdout);
      p = p1 - 1;
    }
    p++;
  }
}

/* Print the name of argument number ARGNUM of macro definition DEFN.
   Recall that DEFN->argnames contains all the arg names
   concatenated in reverse order with comma-space in between.  */

dump_arg_n (defn, argnum)
     DEFINITION *defn;
     int argnum;
{
  register U_CHAR *p = defn->argnames;
  register U_CHAR *end;

  while (argnum + 1 < defn->nargs) {
    p = (U_CHAR *) index (p, ' ') + 1;
    argnum++;
  }

  while (*p && *p != ',') {
    putchar (*p);
    p++;
  }
}

/*
 * initialize random junk in the hash table and maybe other places
 */
initialize_random_junk ()
{
  register int i;

  /*
   * Set up is_idchar and is_idstart tables.  These should be
   * faster than saying (is_alpha (c) || c == '_'), etc.
   * Must do set up these things before calling any routines tthat
   * refer to them.
   */
  for (i = 'a'; i <= 'z'; i++) {
    ++is_idchar[i - 'a' + 'A'];
    ++is_idchar[i];
    ++is_idstart[i - 'a' + 'A'];
    ++is_idstart[i];
  }
  for (i = '0'; i <= '9'; i++)
    ++is_idchar[i];
  ++is_idchar['_'];
  ++is_idstart['_'];

  /* horizontal space table */
  ++is_hor_space[' '];
  ++is_hor_space['\t'];
  ++is_hor_space['\v'];
  ++is_hor_space['\f'];
  ++is_hor_space['\b'];
  ++is_hor_space['\r'];

  install ("__LINE__", -1, T_SPECLINE, 0, -1);
  install ("__DATE__", -1, T_DATE, 0, -1);
  install ("__FILE__", -1, T_FILE, 0, -1);
  install ("__TIME__", -1, T_TIME, 0, -1);
  install ("__STDC__", -1, T_CONST, 1, -1);
}

/*
 * process a given definition string, for initialization
 * If STR is just an identifier, define it with value 1.
 * If STR has anything after the identifier, then it should
 * be identifier-space-definition.
 */
make_definition (str)
     U_CHAR *str;
{
  FILE_BUF *ip;
  struct directive *kt;
  U_CHAR *buf, *p;

  buf = str;
  p = str;
  while (is_idchar[*p]) p++;
  if (*p == 0) {
    buf = (U_CHAR *) alloca (p - buf + 4);
    strcpy (buf, str);
    strcat (buf, " 1");
  }
  
  ip = &instack[++indepth];
  ip->fname = "*Initialization*";

  ip->buf = ip->bufp = buf;
  ip->length = strlen (buf);
  ip->lineno = 1;
  ip->macro = 0;
  ip->free = 0;

  for (kt = directive_table; kt->type != T_DEFINE; kt++)
    ;

  /* pass NULL as output ptr to do_define since we KNOW it never
     does any output.... */
  do_define (buf, buf + strlen (buf) , NULL, kt);
  --indepth;
}

/* JF, this does the work for the -U option */
make_undef (str)
     U_CHAR *str;
{
  FILE_BUF *ip;
  struct directive *kt;

  ip = &instack[++indepth];
  ip->fname = "*undef*";

  ip->buf = ip->bufp = str;
  ip->length = strlen (str);
  ip->lineno = 1;
  ip->macro = 0;
  ip->free = 0;

  for (kt = directive_table; kt->type != T_UNDEF; kt++)
    ;

  do_undef (str,str + strlen (str) - 1, NULL, kt);
  --indepth;
}


#ifndef BSD
#ifndef BSTRING

void
bzero (b, length)
     register char *b;
     register int length;
{
#ifdef VMS
  short zero = 0;
  long max_str = 65535;

  while (length > max_str)
    {
      (void) LIB$MOVC5 (&zero, &zero, &zero, &max_str, b);
      length -= max_str;
      b += max_str;
    }
  (void) LIB$MOVC5 (&zero, &zero, &zero, &length, b);
#else
  while (length-- > 0)
    *b++ = 0;
#endif /* not VMS */
}

void
bcopy (b1, b2, length)
     register char *b1;
     register char *b2;
     register int length;
{
#ifdef VMS
  long max_str = 65535;

  while (length > max_str)
    {
      (void) LIB$MOVC3 (&max_str, b1, b2);
      length -= max_str;
      b1 += max_str;
      b2 += max_str;
    }
  (void) LIB$MOVC3 (&length, b1, b2);
#else
  while (length-- > 0)
    *b2++ = *b1++;
#endif /* not VMS */
}

int
bcmp (b1, b2, length)	/* This could be a macro! */
     register char *b1;
     register char *b2;
      register int length;
 {
#ifdef VMS
   struct dsc$descriptor_s src1 = {length, DSC$K_DTYPE_T, DSC$K_CLASS_S, b1};
   struct dsc$descriptor_s src2 = {length, DSC$K_DTYPE_T, DSC$K_CLASS_S, b2};

   return STR$COMPARE (&src1, &src2);
#else
   while (length-- > 0)
     if (*b1++ != *b2++)
       return 1;

   return 0;
#endif /* not VMS */
}
#endif /* not BSTRING */
#endif /* not BSD */


void
fatal (str, arg)
     char *str, *arg;
{
  fprintf (stderr, "%s: ", progname);
  fprintf (stderr, str, arg);
  fprintf (stderr, "\n");
  exit (FATAL_EXIT_CODE);
}

void
perror_with_name (name)
     char *name;
{
  extern int errno, sys_nerr;
  extern char *sys_errlist[];

  fprintf (stderr, "%s: ", progname);
  if (errno < sys_nerr)
    fprintf (stderr, "%s for %s\n", sys_errlist[errno], name);
  else
    fprintf (stderr, "cannot open %s\n", sys_errlist[errno], name);
}

void
pfatal_with_name (name)
     char *name;
{
  perror_with_name (name);
  exit (FATAL_EXIT_CODE);
}


static void
memory_full ()
{
  fatal ("Memory exhausted.");
}


char *
xmalloc (size)
     int size;
{
  extern char *malloc ();
  register char *ptr = malloc (size);
  if (ptr != 0) return (ptr);
  memory_full ();
  /*NOTREACHED*/
}

char *
xrealloc (old, size)
     char *old;
     int size;
{
  extern char *realloc ();
  register char *ptr = realloc (old, size);
  if (ptr != 0) return (ptr);
  memory_full ();
  /*NOTREACHED*/
}

char *
xcalloc (number, size)
     int number, size;
{
  extern char *malloc ();
  register int total = number * size;
  register char *ptr = malloc (total);
  if (ptr != 0)
    {
      if (total > 100)
	bzero (ptr, total);
      else {
	/* It's not too long, so loop, zeroing by longs.
	   It must be safe because malloc values are always well aligned.  */
	register long *zp = (long *) ptr;
	register long *zl = (long *) (ptr + total - 4);
	register int i = total - 4;
	while (zp < zl)
	  *zp++ = 0;
	if (i < 0)
	  i = 0;
	while (i < total)
	  ptr[i++] = 0;
      }
      return ptr;
    }
  memory_full ();
  /*NOTREACHED*/
}
