/* YACC parser for C syntax.
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


/*  To whomever it may concern: I have heard that such a thing was once
 written by AT&T, but I have never seen it.  */

%{
#include "config.h"
#include "tree.h"
#include "parse.h"
#include "c-tree.h"
%}

%start program

%union {long itype; tree ttype; enum tree_code code}

/* all identifiers that are not reserved words
   and are not declared typedefs in the current block */
%token IDENTIFIER
/* all identifiers that are declared typedefs in the current block.
   In some contexts, they are treated just like IDENTIFIER,
   but they can also serve as typespecs in declarations.  */
%token TYPENAME

/* reserved words that specify storage class.
   yylval contains an IDENTIFER_NODE which indicates which one.  */
%token SCSPEC

/* reserved words that specify type.
   yylval contains an IDENTIFER_NODE which indicates which one.  */
%token TYPESPEC

/* reserved words that modify type: "const" or "volatile".
   yylval contains an IDENTIFER_NODE which indicates which one.  */
%token TYPEMOD

/* character or numeric constants.
   yylval is the node for the constant.  */
%token CONSTANT

/* String constants in raw form.
   yylval is a STRING_CST node.  */
%token STRING

/* "...", used for functions with variable arglists.  */
%token ELLIPSIS

/* the reserved words */
%token SIZEOF ENUM STRUCT UNION IF ELSE WHILE DO FOR SWITCH CASE DEFAULT
%token BREAK CONTINUE RETURN GOTO ASM

/* Define the operator tokens and their precedences.
   The value is an integer because, if used, it is the tree code
   to use in the expression made from the operator.  */

%right <code> ASSIGN '='
%right <code> '?' ':'
%left <code> OROR
%left <code> ANDAND
%left <code> '|'
%left <code> '^'
%left <code> '&'
%left <code> EQCOMPARE
%left <code> ARITHCOMPARE
%left <code> LSHIFT RSHIFT
%left <code> '+' '-'
%left <code> '*' '/' '%'
%right <code> UNARY PLUSPLUS MINUSMINUS
%left HYPERUNARY
%left <code> POINTSAT '.'

%type <code> unop

%type <ttype> identifier IDENTIFIER TYPENAME CONSTANT expr nonnull_exprlist exprlist
%type <ttype> expr_no_commas primary string STRING
%type <ttype> typed_declspecs scspecs typespecs typespec SCSPEC TYPESPEC TYPEMOD
%type <ttype> initdecls notype_initdecls initdcl notype_initdcl
%type <ttype> init initlist

%type <ttype> declarator
%type <ttype> notype_declarator after_type_declarator

%type <ttype> structsp component_decl_list component_decl components component_declarator
%type <ttype> enumlist enumerator
%type <ttype> typename absdcl absdcl1 typemods
%type <ttype> stmts
%type <ttype> pushlevel compstmt stmt xexpr parmlist parms parm identifiers

%{
/* the declaration found for the last IDENTIFIER token read in.
   yylex must look this up to detect typedefs, which get token type TYPENAME,
   so it is left around in case the identifier is not a typedef but is
   used in a context which makes it a reference to a variable.  */
static tree lastiddecl;

tree current_function_decl, current_switch_stmt, current_block;
tree current_continue_label, current_break_label;
tree continue_label_stack, break_label_stack;

static void pushbreak(), popbreak();
static tree finish_compound_stmt();
static tree make_pointer_declarator ();
static tree combine_strings ();

/* list of types and structure classes of the current declaration */
tree current_declspecs;

char *input_filename;		/* file being read */
%}

%%
program:
	extdefs
	;

/* the reason for the strange actions in this rule
 is so that notype_initdecls when reached via datadef
 can find a valid list of type and sc specs in $0. */

extdefs:
	{$<ttype>$ = NULL_TREE} extdef
	| extdefs {$<ttype>$ = NULL_TREE} extdef
	;

extdef:
	fndef
	| datadef
	| ASM '(' STRING ')' ';'
		{ assemble_asm ($3); }
	;

datadef:
	  setspecs notype_initdecls ';'
        | scspecs setspecs notype_initdecls ';'
	  {}
	| typed_declspecs setspecs initdecls ';'
	  {}
        | scspecs ';'
	  { yyerror ("empty declaration"); }
	| typed_declspecs ';'
	  { shadow_tag ($1); }
	| error ';'
	| error '}'
	| ';'
	;

fndef:
	  typed_declspecs setspecs declarator
		{ if (! start_function ($1, $3))
		    YYFAIL; }
	  xdecls
		{ store_parm_decls (); }
	  compstmt
		{ finish_function (input_filename, @7.first_line, $7); }
	| typed_declspecs setspecs declarator error
		{ }
	| scspecs setspecs notype_declarator
		{ if (! start_function ($1, $3))
		    YYFAIL; }
	  xdecls
		{ store_parm_decls (); }
	  compstmt
		{ finish_function (input_filename, @7.first_line, $7); }
	| scspecs setspecs notype_declarator error
		{ }
	| setspecs notype_declarator
		{ if (! start_function (0, $2))
		    YYFAIL; }
	  xdecls
		{ store_parm_decls (); }
	  compstmt
		{ finish_function (input_filename, @6.first_line, $6); }
	| setspecs notype_declarator error
	;

identifier:
	IDENTIFIER
	| TYPENAME
	;

unop:     '&'
		{ $$ = ADDR_EXPR; }
	| '-'
		{ $$ = NEGATE_EXPR; }
	| '+'
		{ $$ = CONVERT_EXPR; }
	| PLUSPLUS
		{ $$ = PREINCREMENT_EXPR; }
	| MINUSMINUS
		{ $$ = PREDECREMENT_EXPR; }
	| '~'
		{ $$ = BIT_NOT_EXPR; }
	| '!'
		{ $$ = TRUTH_NOT_EXPR; }
	;

expr:	nonnull_exprlist
		{ $$ = build_compound_expr($1); }
	;

exprlist:
	  /* empty */
		{ $$ = NULL_TREE; }
	| nonnull_exprlist
	;

nonnull_exprlist:
	expr_no_commas
		{ $$ = build_tree_list (NULL_TREE, $1); }
	| nonnull_exprlist ',' expr_no_commas
		{ chainon ($1, build_tree_list (NULL_TREE, $3)); }
	;

expr_no_commas:
	primary
	| '*' expr_no_commas   %prec UNARY
		{ $$ = build_indirect_ref ($2); }
	| unop expr_no_commas  %prec UNARY
		{ $$ = build_unary_op ($1, $2, 0); }
	| '(' typename ')' expr_no_commas  %prec UNARY
		{ $$ = build_c_cast (groktypename($2), $4); }
	| SIZEOF expr_no_commas  %prec UNARY
		{ $$ = c_sizeof (TREE_TYPE ($2)); }
	| SIZEOF '(' typename ')'  %prec HYPERUNARY
		{ $$ = c_sizeof (groktypename($3)); }
	| expr_no_commas '+' expr_no_commas
		{ $$ = build_binary_op ($2, $1, $3); }
	| expr_no_commas '-' expr_no_commas
		{ $$ = build_binary_op ($2, $1, $3); }
	| expr_no_commas '*' expr_no_commas
		{ $$ = build_binary_op ($2, $1, $3); }
	| expr_no_commas '/' expr_no_commas
		{ $$ = build_binary_op ($2, $1, $3); }
	| expr_no_commas '%' expr_no_commas
		{ $$ = build_binary_op ($2, $1, $3); }
	| expr_no_commas LSHIFT expr_no_commas
		{ $$ = build_binary_op ($2, $1, $3); }
	| expr_no_commas RSHIFT expr_no_commas
		{ $$ = build_binary_op ($2, $1, $3); }
	| expr_no_commas ARITHCOMPARE expr_no_commas
		{ $$ = build_binary_op ($2, $1, $3); }
	| expr_no_commas EQCOMPARE expr_no_commas
		{ $$ = build_binary_op ($2, $1, $3); }
	| expr_no_commas '&' expr_no_commas
		{ $$ = build_binary_op ($2, $1, $3); }
	| expr_no_commas '|' expr_no_commas
		{ $$ = build_binary_op ($2, $1, $3); }
	| expr_no_commas '^' expr_no_commas
		{ $$ = build_binary_op ($2, $1, $3); }
	| expr_no_commas ANDAND expr_no_commas
		{ $$ = build_binary_op (TRUTH_ANDIF_EXPR, $1, $3); }
	| expr_no_commas OROR expr_no_commas
		{ $$ = build_binary_op (TRUTH_ORIF_EXPR, $1, $3); }
	| expr_no_commas '?' expr ':' expr_no_commas
		{ $$ = build_conditional_expr($1, $3, $5); }
	| expr_no_commas '=' expr_no_commas
		{ $$ = build_modify_expr($1, $3); }
	| expr_no_commas ASSIGN expr_no_commas
		{ register tree tem 
		    = duplicate_reference ($1);
		  $$ = build_modify_expr(tem, build_binary_op ($2, tem, $3)); }
	;

primary:
	IDENTIFIER
		{ $$ = lastiddecl;
		  if (!$$)
		    {
		      if (yychar == YYEMPTY)
			yychar = YYLEX;
		      if (yychar == '(')
			$$ = implicitly_declare($1);
		      else
			{
			  yyerror("variable %s used but not declared",
				  IDENTIFIER_POINTER ($1));
			  $$ = error_mark_node;
			}
		    }
		  if (TREE_CODE ($$) == CONST_DECL)
		    $$ = DECL_INITIAL ($$);
		}
	| CONSTANT
	| string
		{ $$ = combine_strings ($1); }
	| '(' expr ')'
		{ $$ = $2; }
	| '(' error ')'
		{ $$ = error_mark_node; }
	| primary '(' exprlist ')'   %prec '.'
		{ $$ = build_function_call ($1, $3); }
	| primary '[' expr ']'   %prec '.'
		{ $$ = build_array_ref ($1, $3); }
	| primary '.' identifier
		{ $$ = build_component_ref($1, $3); }
	| primary POINTSAT identifier
		{ $$ = build_component_ref(build_indirect_ref ($1), $3); }
	| primary PLUSPLUS
		{ $$ = build_unary_op (POSTINCREMENT_EXPR, $1, 0); }
	| primary MINUSMINUS
		{ $$ = build_unary_op (POSTDECREMENT_EXPR, $1, 0); }
	;

/* Produces a STRING_CST with perhaps more STRING_CSTs chained onto it.  */
string:
	  STRING
	| string STRING
		{ $$ = chainon ($1, $2); }
	;

xdecls:
	/* empty */
	| decls
	;

decls:
	decl
	| errstmt
	| decls decl
	| decl errstmt
	;

/* records the type and storage class specs to use for processing
   the declarators that follow */
setspecs: /* empty */
		{ current_declspecs = $<ttype>0; }
	;

decl:
	typed_declspecs setspecs initdecls ';'
		{}
	| scspecs setspecs notype_initdecls ';'
		{}
	| typed_declspecs ';'
		{ shadow_tag ($1); }
	| scspecs ';'
		{ warning ("empty declaration"); }
	;

/* declspecs which contain at least one type specifier.
   A typedef'd name following these is taken as a name to be declared.  */
typed_declspecs:
	  typespec
		{ $$ = build_tree_list (NULL_TREE, $1); }
	| scspecs typespec
		{ $$ = tree_cons (NULL_TREE, $2, $1); }
	| typed_declspecs TYPESPEC
		{ $$ = tree_cons (NULL_TREE, $2, $1); }
	| typed_declspecs TYPEMOD
		{ $$ = tree_cons (NULL_TREE, $2, $1); }
	| typed_declspecs structsp
		{ $$ = tree_cons (NULL_TREE, $2, $1); }
	| typed_declspecs SCSPEC
		{ $$ = tree_cons (NULL_TREE, $2, $1); }
	;

/* declspecs which contain no type specifiers.
The identifier to which they apply must not be a typedef'd name.  */
scspecs:  SCSPEC
		{ $$ = build_tree_list (NULL_TREE, $1); }
	| scspecs SCSPEC
		{ $$ = tree_cons (NULL_TREE, $2, $1); }
	;

/* used instead of declspecs where storage classes are not allowed
   (typenames, structure components, and parameters) */
typespecs:
	  typespec
		{ $$ = build_tree_list (NULL_TREE, $1); }
	| typespecs TYPESPEC
		{ $$ = tree_cons (NULL_TREE, $2, $1); }
	| typespecs TYPEMOD
		{ $$ = tree_cons (NULL_TREE, $2, $1); }
	| typespecs structsp
		{ $$ = tree_cons (NULL_TREE, $2, $1); }
	;

typespec: TYPESPEC
	| TYPEMOD
	| structsp
	| TYPENAME
	;

initdecls:
	initdcl
	| initdecls ',' initdcl
	;

notype_initdecls:
	notype_initdcl
	| notype_initdecls ',' initdcl
	;

initdcl:
	  declarator '='
		{ $<ttype>$ = start_decl ($1, current_declspecs, 1); }
	  init
/* Note how the declaration of the variable is in effect while its init is parsed! */
		{ finish_decl (input_filename, @1.first_line, $<ttype>3, $4); }
	| declarator
		{ tree d = start_decl ($1, current_declspecs, 0);
		  finish_decl (input_filename, @1.first_line, d, NULL_TREE); }
	;

notype_initdcl:
	  notype_declarator '='
		{ $<ttype>$ = start_decl ($1, current_declspecs, 1); }
	  init
/* Note how the declaration of the variable is in effect while its init is parsed! */
		{ finish_decl (input_filename, @1.first_line, $<ttype>3, $4); }
	| notype_declarator
		{ tree d = start_decl ($1, current_declspecs, 0);
		  finish_decl (input_filename, @1.first_line, d, NULL_TREE); }
	;

init:
	expr_no_commas
	| '{' initlist '}'
		{ $$ = build1 (CONSTRUCTOR, nreverse ($2)); }
	| '{' initlist ',' '}'
		{ $$ = build1 (CONSTRUCTOR, nreverse ($2)); }
	;

/* This chain is built in reverse order,
   and put in forward order where initlist is used.  */
initlist:
	init
		{ $$ = build_tree_list (NULL_TREE, $1); }
	| initlist ',' init
		{ $$ = tree_cons (NULL, $3, $1); }
	;

/* Any kind of declarator (thus, all declarators allowed
   after an explicit typespec).  */

declarator:
	  after_type_declarator
	| notype_declarator
	;

/* A declarator that is allowed only after an explicit typespec.  */

after_type_declarator:
	  after_type_declarator '(' parmlist ')'  %prec '.'
		{ $$ = build2 (CALL_EXPR, $1, $3); }
	| after_type_declarator '(' identifiers ')'  %prec '.'
		{ $$ = build2 (CALL_EXPR, $1, $3); }
	| after_type_declarator '(' error ')'  %prec '.'
		{ $$ = build2 (CALL_EXPR, $1, NULL_TREE); }
	| after_type_declarator '[' expr ']'  %prec '.'
		{ $$ = build2 (ARRAY_REF, $1, $3); }
	| after_type_declarator '[' ']'  %prec '.'
		{ $$ = build2 (ARRAY_REF, $1, NULL_TREE); }
	| TYPENAME
	;

/* A declarator allowed whether or not there has been
   an explicit typespec.  These cannot redeclare a typedef-name.  */

notype_declarator:
	  notype_declarator '(' parmlist ')'  %prec '.'
		{ $$ = build2 (CALL_EXPR, $1, $3); }
	| notype_declarator '(' identifiers ')'  %prec '.'
		{ $$ = build2 (CALL_EXPR, $1, $3); }
	| notype_declarator '(' error ')'  %prec '.'
		{ $$ = build2 (CALL_EXPR, $1, NULL_TREE); }
	| '(' notype_declarator ')'
		{ $$ = $2; }
	| '*' typemods notype_declarator  %prec UNARY
		{ $$ = make_pointer_declarator ($2, $3); }
	| notype_declarator '[' expr ']'  %prec '.'
		{ $$ = build2 (ARRAY_REF, $1, $3); }
	| notype_declarator '[' ']'  %prec '.'
		{ $$ = build2 (ARRAY_REF, $1, NULL_TREE); }
	| IDENTIFIER
	;

structsp:
	  STRUCT identifier '{' component_decl_list '}'
		{ $$ = build_struct (RECORD_TYPE, input_filename, @1.first_line, $2, $4, 0); }
	| STRUCT '{' component_decl_list '}'
		{ $$ = build_struct (RECORD_TYPE, input_filename, @1.first_line, NULL_TREE, $3, 0); }
	| STRUCT identifier
		{ $$ = build_struct (RECORD_TYPE, input_filename, @1.first_line, $2, NULL_TREE, 1); }
	| UNION identifier '{' component_decl_list '}'
		{ $$ = build_struct (UNION_TYPE, input_filename, @1.first_line, $2, $4, 0); }
	| UNION '{' component_decl_list '}'
		{ $$ = build_struct (UNION_TYPE, input_filename, @1.first_line, NULL_TREE, $3, 0); }
	| UNION identifier
		{ $$ = build_struct (UNION_TYPE, input_filename, @1.first_line, $2, NULL_TREE, 1); }
	| ENUM identifier '{'
		{ $$ = start_enum ($2); }
	  enumlist '}'
		{ $$ = finish_enum ($<ttype>4, nreverse ($5)); }
	| ENUM '{'
		{ $$ = start_enum (NULL_TREE); }
	  enumlist '}'
		{ $$ = finish_enum ($<ttype>3, nreverse ($4)); }
	| ENUM identifier
		{ $$ = xref_enum ($2); }
	;

component_decl_list:   /* empty */
		{ $$ = NULL_TREE; }
	| component_decl
	| component_decl_list ';' component_decl
		{ $$ = chainon ($1, $3); }
	| component_decl_list ';'
	;

component_decl:
	typespecs setspecs components
		{ $$ = $3; }
	| error
		{ $$ == NULL_TREE; }
	;

components:
	  /* empty */
		{ $$ = NULL_TREE; }
	| component_declarator
	| components ',' component_declarator
		{ $$ = chainon ($1, $3); }
	;

component_declarator:
	declarator
		{ $$ = grokfield (input_filename, @1.first_line, $1, current_declspecs, NULL_TREE); }
	| declarator ':' expr_no_commas
		{ $$ = grokfield (input_filename, @1.first_line, $1, current_declspecs, $3); }
	| ':' expr_no_commas
		{ $$ = grokfield (input_filename, @1.first_line, NULL_TREE, current_declspecs, $2); }
	;

/* We chain the enumerators in reverse order.
   They are put in forward order where enumlist is used.
   (The order used to be significant, but no longer is so.
   However, we still maintain the order, just to be clean.)  */

enumlist:
	  enumerator
	| enumlist ',' enumerator
		{ $$ = chainon ($3, $1); }
	| enumlist ','
	;


enumerator:
	  identifier
		{ $$ = build_enumerator ($1, NULL_TREE); }
	| identifier '=' expr_no_commas
		{ $$ = build_enumerator ($1, $3); }
	;

typename:
	typespecs absdcl
		{ $$ = build_tree_list ($1, $2); }
	;
	
absdcl:   /* an absolute declarator */
	/* empty */
		{ $$ = NULL_TREE; }
	| absdcl1
	;

typemods:
	  /* empty */
		{ $$ = NULL_TREE; }
	| typemods TYPEMOD
		{ $$ = tree_cons (NULL_TREE, $2, $1); }
	;

absdcl1:  /* a nonempty absolute declarator */
	  '(' absdcl1 ')'
		{ $$ = $2; }
	| '*' typemods absdcl1  %prec UNARY
		{ $$ = make_pointer_declarator ($2, $3); }
	| '*' typemods  %prec UNARY
		{ $$ = make_pointer_declarator ($2, NULL_TREE); }
	| absdcl1 '(' parmlist ')'  %prec '.'
		{ $$ = build2 (CALL_EXPR, $1, $3); }
	| absdcl1 '[' expr ']'  %prec '.'
		{ $$ = build2 (ARRAY_REF, $1, $3); }
	| absdcl1 '[' ']'  %prec '.'
		{ $$ = build2 (ARRAY_REF, $1, NULL_TREE); }
	| '(' parmlist ')'  %prec '.'
		{ $$ = build2 (CALL_EXPR, NULL_TREE, $2); }
	| '[' expr ']'  %prec '.'
		{ $$ = build2 (ARRAY_REF, NULL_TREE, $2); }
	| '[' ']'  %prec '.'
		{ $$ = build2 (ARRAY_REF, NULL_TREE, NULL_TREE); }
	;

/* at least one statement, the first of which parses without error.  */
/* stmts is used only after decls, so an invalid first statement
   is actually regarded as an invalid decl and part of the decls.  */

/* To speed things up, we actually chain the statements in
   reverse order and return them that way.
   They are put into forward order where stmts is used.  */
stmts:
	stmt
	| stmts stmt
		{ $$ = chainon ($2, $1); }
	| stmts errstmt
	;

errstmt:  error ';'
	;

/* build the LET_STMT node before parsing its contents,
  so that any LET_STMTs within the context can have their display pointers
  set up to point at this one.  */

pushlevel:  /* empty */
		{ pushlevel();
		  $$ = current_block;
		  current_block
		    = build_let (input_filename, 0, 0, 0, $$, 0);
		}
	;

compstmt: '{' '}'
		{ $$ = build_compound (input_filename, @1.first_line, 0); }
	| '{' pushlevel decls stmts '}'
		{ $$ = finish_compound_stmt (current_block, nreverse ($4),
					     $2, @1.first_line); }
	| '{' pushlevel decls '}'
		{ $$ = finish_compound_stmt (current_block, NULL_TREE,
					     $2, @1.first_line); }
	| '{' pushlevel error '}'
		{ $$ = error_mark_node;
		  current_block = $2;
		  poplevel(); }
	| '{' pushlevel stmts '}'
		{ $$ = finish_compound_stmt (current_block, nreverse ($3), $2,
					     @1.first_line); }
	;

stmt:	  compstmt
	| expr ';'
		{ $$ = build_expr_stmt (input_filename, @1.first_line, $1); }
	| IF '(' expr ')' stmt
		{ $$ = build_if (input_filename, @1.first_line, default_conversion ($3), $5, 0); } 
	| IF '(' expr ')' stmt ELSE stmt
		{ $$ = build_if (input_filename, @1.first_line, default_conversion ($3), $5, $7); }
	| WHILE
		{ pushbreak(1); }
	  '(' expr ')' stmt
		{ $$ = build_loop (input_filename, @1.first_line,
			 chainon (build_exit (input_filename, @4.first_line,
					      default_conversion ($4)),
				  chainon ($6, current_continue_label)));
		  $$ = build_compound (input_filename, @1.first_line, chainon ($$, current_break_label));
		  popbreak(1); }
	| DO
		{ pushbreak(1); }
	  stmt WHILE '(' expr ')' ';'
		{ $$ = build_loop (input_filename, @1.first_line,
			 chainon ($3, chainon(current_continue_label,
					      build_exit (input_filename, @6.first_line,
							  default_conversion ($6)))));
		  $$ = build_compound (input_filename, @1.first_line, chainon ($$, current_break_label));
		  popbreak(1); }
	| FOR 
		{ pushbreak(1); }
	  '(' xexpr ';' xexpr ';' xexpr ')' stmt
		{ $$ = build_compound (input_filename, @1.first_line,
			 chainon ($4 ? build_expr_stmt (input_filename, @4.first_line, $4) : NULL_TREE,
			   build_loop (input_filename, @1.first_line,
			     chainon ($6 ? build_exit (input_filename, @6.first_line,
						       default_conversion ($6))
					 : NULL_TREE,
			       chainon (chainon ($10, current_continue_label),
				        $8 ? build_expr_stmt (input_filename, @8.first_line, $8) : NULL_TREE)))));
		  $$ = build_compound (input_filename, @1.first_line, chainon ($$, current_break_label));
		  popbreak(1); }
	| SWITCH '(' expr ')'
		{ $<ttype>$ = current_switch_stmt;
		  pushbreak(0);
		  current_switch_stmt
		    = build_switch_stmt (input_filename, @1.first_line,
					 default_conversion ($3)); }
	  stmt
		{ $$ = build_compound (input_filename, @1.first_line, 
				chainon(current_switch_stmt,
					chainon($6, current_break_label)));
		  finish_switch_stmt (current_switch_stmt, current_break_label);
		  popbreak (0);
		  current_switch_stmt = $<ttype>5; }
	| CASE expr ':' stmt
		{ register tree value = fold($2);
		  tree l = build_label (input_filename, @1.first_line, NULL_TREE, current_block);

		  if (TREE_CODE (value) != INTEGER_CST)
		    {
		      yyerror("case label does not reduce to an integer constant");
		      value = error_mark_node;
		    }
		  pushcase(value, l);
		  $$ = build_compound (input_filename, @1.first_line, chainon(l, $4));
		}
	| DEFAULT ':' stmt
		{
		  tree l = build_label (input_filename, @1.first_line, 0, current_block);
		  pushcase(NULL_TREE, l);
		  $$ = build_compound (input_filename, @1.first_line, chainon(l, $3));
		}
	| BREAK ';'
		{ if (current_break_label)
		    $$ = build_goto (input_filename, @1.first_line, STMT_BODY (current_break_label));
		  else
		    {
		      yyerror("break statement not within a do, for, while or switch statement");
		      $$ = error_mark_node;
		    }
		}
	| CONTINUE ';'	
		{ if (current_continue_label)
		    $$ = build_goto (input_filename, @1.first_line, STMT_BODY (current_continue_label));
		  else
		    {
		      yyerror("continue statement not within a do, for or while statement");
		      $$ = error_mark_node;
		    }
		}
	| RETURN ';'
	        { $$ = build_return (input_filename, @1.first_line, NULL_TREE); }
	| RETURN expr ';'
	        { $$ = build_return_stmt (input_filename, @1.first_line, $2); }
	| GOTO identifier ';'
		{ pushgoto($$ = build_goto (input_filename, @1.first_line, $2)); }
	| ASM '(' STRING ')' ';'
		{ $$ = build_asm_stmt (input_filename, @1.first_line, $3); }
	| identifier ':' stmt
		{ $$ = build_compound (input_filename, @1.first_line, chainon (build_label (input_filename, @1.first_line, $1, current_block), $3)); }
	| ';'
		{ $$ = build_compound (input_filename, @1.first_line, 0); }
	;

xexpr:
	/* empty */
		{ $$ = NULL_TREE; }
	| expr
	;

/* This is what appears inside the parens in a function declarator.
   Is value is represented in the format that grokdeclarator expects.  */
parmlist:  /* empty */
		{ $$ = NULL_TREE; }
	| parms
  		{ $$ = chainon ($1, build_tree_list (NULL_TREE,
						     void_type_node)); }
	| parms ',' ELLIPSIS
	;

/* A nonempty list of parameter declarations or type names.  */
parms:	
	parm
		{ $$ = build_tree_list (NULL_TREE, $1); }
	| parms ',' parm
		{ $$ = chainon ($1, build_tree_list (NULL_TREE, $3)); }
	;

parm:
	  typespecs declarator
		{ $$ = build_tree_list ($1, $2)	; }
	| typespecs absdcl
		{ $$ = build_tree_list ($1, $2); }
	;

/* A nonempty list of identifiers.  */
identifiers:	
	IDENTIFIER
		{ $$ = build_tree_list (NULL_TREE, $1); }
	| identifiers ',' IDENTIFIER
		{ $$ = chainon ($1, build_tree_list (NULL_TREE, $3)); }
	;
%%

static tree
finish_compound_stmt(block, stmts, outer_block, line)
     tree block, stmts, outer_block;
     int line;
{
  register tree decls = getdecls();
  register tree tags = gettags ();

  if (decls || tags)
    {
      finish_block (block, decls, tags, stmts);
      poplevel();
      current_block = outer_block;
      STMT_SOURCE_LINE (block) = line;
      return block;
    }
  else
    {
      current_block = outer_block;
      poplevel();
      return build_compound (input_filename, line, stmts);  
    }
}



static void
pushbreak(a)
int a;
{
  if (current_break_label)
    TREE_CHAIN (current_break_label) = break_label_stack;
  break_label_stack = current_break_label;
  current_break_label = build_label (0, 0, NULL_TREE, current_block);

  if (a)
    {
      if (current_continue_label)
	TREE_CHAIN (current_continue_label) = continue_label_stack;
      continue_label_stack = current_continue_label;
      current_continue_label = build_label (0, 0, NULL_TREE, current_block);
    }
}

static void
popbreak(a)
int a;
{
  current_break_label = break_label_stack;
  if (current_break_label)
    break_label_stack = TREE_CHAIN (break_label_stack);

  if (a)
    {
      current_continue_label = continue_label_stack;
      if (current_continue_label)
	continue_label_stack = TREE_CHAIN (continue_label_stack);
    }

  if (current_break_label)
    TREE_CHAIN (current_break_label) = NULL;
  if (current_continue_label)
    TREE_CHAIN (current_continue_label) = NULL;
}

/* Return something to represent absolute declarators containing a *.
   TARGET is the absolute declarator that the * contains.
   TYPEMODS is a list of modifiers such as const or volatile
   to apply to the pointer type, represented as identifiers.

   We return an INDIRECT_REF whose "contents" are TARGET
   and whose type is the modifier list.  */
   
static tree
make_pointer_declarator (typemods, target)
     tree typemods, target;
{
  register tree t = build1 (INDIRECT_REF, target);
  TREE_TYPE (t) = typemods;
  return t;
}

/* Given a chain of STRING_CST nodes,
   concatenate them into one STRING_CST
   and then return an ADDR_EXPR for it.  */

static tree
combine_strings (strings)
     tree strings;
{
  register tree value, t;

  if (TREE_CHAIN (strings))
    {
      /* More than one in the chain, so concatenate.  */
      register char *p, *q;
      register int length = 1;

      /* Don't include the \0 at the end of each substring,
	 except for the last one.  */
      for (t = strings; t; t = TREE_CHAIN (t))
	length += TREE_STRING_LENGTH (t) - 1;

      p = (char *) oballoc (length);

      q = p;
      for (t = strings; t; t = TREE_CHAIN (t))
	{
	  bcopy (TREE_STRING_POINTER (t), q, TREE_STRING_LENGTH (t) - 1);
	  q += TREE_STRING_LENGTH (t) - 1;
	}
      *q = 0;

      value = make_node (STRING_CST);
      TREE_TYPE (value) = TREE_TYPE (strings);
      TREE_STRING_POINTER (value) = p;
      TREE_STRING_LENGTH (value) = length;
      TREE_LITERAL (value) = 1;
    }
  else
    value = strings;	  

  value = build1 (ADDR_EXPR, value);
  TREE_TYPE (value) = string_type_node;
  TREE_LITERAL (value) = 1;
  return value;
}

int lineno;			/* current line number in file being read */

FILE *finput;			/* input file.
				   Normally a pipe from the preprocessor.  */

/* lexical analyzer */

static int maxtoken;		/* Current length of token buffer */
static char *token_buffer;	/* Pointer to token buffer */

/* frw[i] is index in rw of the first word whose length is i. */

#define MAXRESERVED 9

static char frw[10] =
  { 0, 0, 0, 2, 5, 13, 19, 27, 29, 33 };

static char *rw[] =
  { "if", "do", "int", "for", "asm",
    "case", "char", "auto", "goto", "else", "long", "void", "enum",
    "float", "short", "union", "break", "while", "const",
    "double", "static", "extern", "struct", "return", "sizeof", "switch", "signed",
    "typedef", "default",
    "unsigned", "continue", "register", "volatile" };

static short rtoken[] =
  { IF, DO, TYPESPEC, FOR, ASM,
    CASE, TYPESPEC, SCSPEC, GOTO, ELSE, TYPESPEC, TYPESPEC, ENUM,
    TYPESPEC, TYPESPEC, UNION, BREAK, WHILE, TYPEMOD,
    TYPESPEC, SCSPEC, SCSPEC, STRUCT, RETURN, SIZEOF, SWITCH, TYPESPEC,
    SCSPEC, DEFAULT,
    TYPESPEC, CONTINUE, SCSPEC, TYPEMOD };

/* This table corresponds to rw and rtoken.
   Its element is an index in ridpointers  */

#define NORID (enum rid) 0

static enum rid rid[] =
  { NORID, NORID, RID_INT, NORID, NORID,
    NORID, RID_CHAR, RID_AUTO, NORID, NORID, RID_LONG, RID_VOID, NORID,
    RID_FLOAT, RID_SHORT, NORID, NORID, NORID, RID_CONST,
    RID_DOUBLE, RID_STATIC, RID_EXTERN, NORID, NORID, NORID, NORID, RID_SIGNED,
    RID_TYPEDEF, NORID,
    RID_UNSIGNED, NORID, RID_REGISTER, RID_VOLATILE };

/* The elements of `ridpointers' are identifier nodes
   for the reserved type names and storage classes.  */

tree ridpointers[(int) RID_MAX];

static tree line_identifier;   /* The identifier node named "line" */

void check_newline();

void
init_lex()
{
  extern char *malloc();

  /* Start it at 0, because check_newline is called atthe very beginning
     and will increment it to 1.  */
  lineno = 0;
  current_function_decl = NULL;
  current_switch_stmt = NULL;
  current_block = NULL;
  current_break_label = NULL;
  current_continue_label = NULL;
  break_label_stack = NULL;
  continue_label_stack = NULL;
  line_identifier = get_identifier("line");

  maxtoken = 40;
  token_buffer = malloc((unsigned)(maxtoken+1));
  ridpointers[(int) RID_INT] = get_identifier("int");
  ridpointers[(int) RID_CHAR] = get_identifier("char");
  ridpointers[(int) RID_VOID] = get_identifier("void");
  ridpointers[(int) RID_FLOAT] = get_identifier("float");
  ridpointers[(int) RID_DOUBLE] = get_identifier("double");
  ridpointers[(int) RID_SHORT] = get_identifier("short");
  ridpointers[(int) RID_LONG] = get_identifier("long");
  ridpointers[(int) RID_UNSIGNED] = get_identifier("unsigned");
  ridpointers[(int) RID_SIGNED] = get_identifier("signed");
  ridpointers[(int) RID_CONST] = get_identifier("const");
  ridpointers[(int) RID_VOLATILE] = get_identifier("volatile");
  ridpointers[(int) RID_AUTO] = get_identifier("auto");
  ridpointers[(int) RID_STATIC] = get_identifier("static");
  ridpointers[(int) RID_EXTERN] = get_identifier("extern");
  ridpointers[(int) RID_TYPEDEF] = get_identifier("typedef");
  ridpointers[(int) RID_REGISTER] = get_identifier("register");
}

static int
skip_white_space()
{
  register int c;
  register int inside;

  c = getc(finput);

  for (;;)
    {
      switch (c)
	{
	case '/':
	  c = getc(finput);
	  if (c != '*')
	    {
	      ungetc(c, finput);
	      return '/';
	    }

	  c = getc(finput);

	  inside = 1;
	  while (inside)
	    {
	      if (c == '*')
		{
		  while (c == '*')
		    c = getc(finput);

		  if (c == '/')
		    {
		      inside = 0;
		      c = getc(finput);
		    }
		}
	      else if (c == '\n')
		{
		  lineno++;
		  c = getc(finput);
		}
	      else if (c == EOF)
		yyerror("unterminated comment");
	      else
		c = getc(finput);
	    }

	  break;

	case '\n':
	  check_newline();

	case ' ':
	case '\t':
	case '\f':
	case '\r':
	case '\b':
	  c = getc(finput);
	  break;

	case '\\':
	  c = getc(finput);
	  if (c == '\n')
	    lineno++;
	  else
	    yyerror("stray '\\' in program");
	  c = getc(finput);
	  break;

	default:
	  return (c);
	}
    }
}



/* make the token buffer longer, preserving the data in it.
p should point to just beyond the last valid character in the old buffer
and the value points to the corresponding place in the new one.  */

static char *
extend_token_buffer(p)
char *p;
{
  register char *newbuf;
  register char *value;
  int newlength = maxtoken * 2 + 10;
  register char *p2, *p1;
  extern char *malloc();

  newbuf = malloc((unsigned)(newlength+1));

  p2 = newbuf;
  p1 = newbuf + newlength + 1;
  while (p1 != p2) *p2++ = 0;

  value = newbuf;
  p2 = token_buffer;
  while (p2 != p)
   *value++ = *p2++;

  token_buffer = newbuf;

  maxtoken = newlength;

  return (value);
}



/* At the beginning of a line,
   increment the line number
   and handle a #line directive immediately following  */

void
check_newline ()
{
  register int c;
  register int token;

  while (1)
    {
      c = getc (finput);
      lineno++;

      if (c != '#')
	{
	  /* If no #, unread the character,
	     except don't bother if it is whitespace.  */
	  if (c == ' ' || c == '\t')
	    return;
	  ungetc (c, finput);
	  return;
	}

      /* Skip whitespace after the #.  */

      while (1)
	{
	  c = getc (finput);
	  if (! (c == ' ' || c == '\t'))
	    break;
	}

      /* If the # is the only nonwhite char on the line,
	 just ignore it.  Check the new newline.  */
      if (c == '\n')
	continue;

      /* Something follows the #; read a token.  */

      ungetc (c, finput);
      token = yylex ();

      if (token == CONSTANT
	  && TREE_CODE (yylval.ttype) == INTEGER_CST)
	{
	  /* subtract one, because it is the following line that
	     gets the specified number */

	  int l = TREE_INT_CST_LOW (yylval.ttype) - 1;

	  token = yylex ();
	  if (token != STRING || TREE_CODE (yylval.ttype) != STRING_CST)
	    yyerror ("invalid #line");

	  input_filename
	    = (char *) permalloc (TREE_STRING_LENGTH (yylval.ttype) + 1);
	  strcpy (input_filename, TREE_STRING_POINTER (yylval.ttype));
	  lineno = l;
	}
      else
	yyerror ("undefined or invalid # directive");

      /* skip the rest of this line.  */
      while ((c = getc (finput)) != '\n');
    }
}



#define isalnum(char) ((char >= 'a' && char <= 'z') || (char >= 'A' && char <= 'Z') || (char >= '0' && char <= '9'))
#define isdigit(char) (char >= '0' && char <= '9')
#define ENDFILE -1  /* token that represents end-of-file */


static int
readescape ()
{
  register int c = getc (finput);
  register int count, code;

  switch (c)
    {
    case 'x':
      code = 0;
      count = 0;
      while (1)
	{
	  c = getc (finput);
	  if (!(c >= 'a' && c <= 'f')
	      && !(c >= 'A' && c <= 'F')
	      && !(c >= '0' && c <= '9'))
	    {
	      ungetc (c, finput);
	      break;
	    }
	  if (c >= 'a' && c <= 'z')
	    c -= 'a' - 'A';
	  code *= 16;
	  if (c >= 'a' && c <= 'f')
	    code += c - 'a' + 10;
	  if (c >= 'A' && c <= 'F')
	    code += c - 'A' + 10;
	  if (c >= '0' && c <= '9')
	    code += c - '0';
	  count++;
	  if (count == 3)
	    break;
	}
      if (count == 0)
	yyerror ("\\x used with no following hex digits");
      return code;

    case '0':  case '1':  case '2':  case '3':  case '4':
    case '5':  case '6':  case '7':
      code = 0;
      count = 0;
      while ((c <= '7') && (c >= '0') && (count++ < 3))
	{
	  code = (code * 8) + (c - '0');
	  c = getc (finput);
	}
      ungetc (c, finput);
      return code;

    case '\\': case '\'': case '"':
      return c;

    case '\n':
      lineno++;
      return -1;

    case 'n':
      return TARGET_NEWLINE;

    case 't':
      return TARGET_TAB;

    case 'r':
      return TARGET_CR;

    case 'f':
      return TARGET_FF;

    case 'b':
      return TARGET_BS;

    case 'a':
      return TARGET_BELL;

    case 'v':
      return TARGET_VT;
    }
  return c;
}


static int
yylex()
{
  register int c;
  register char *p;
  register int value;

  c = skip_white_space();

  yylloc.first_line = lineno;

  switch (c)
    {
    case EOF:
      value = ENDFILE; break;

    case 'A':  case 'B':  case 'C':  case 'D':  case 'E':
    case 'F':  case 'G':  case 'H':  case 'I':  case 'J':
    case 'K':  case 'L':  case 'M':  case 'N':  case 'O':
    case 'P':  case 'Q':  case 'R':  case 'S':  case 'T':
    case 'U':  case 'V':  case 'W':  case 'X':  case 'Y':
    case 'Z':
    case 'a':  case 'b':  case 'c':  case 'd':  case 'e':
    case 'f':  case 'g':  case 'h':  case 'i':  case 'j':
    case 'k':  case 'l':  case 'm':  case 'n':  case 'o':
    case 'p':  case 'q':  case 'r':  case 's':  case 't':
    case 'u':  case 'v':  case 'w':  case 'x':  case 'y':
    case 'z':
    case '_':
      p = token_buffer;
      while (isalnum(c) || (c == '_'))
	{
	  if (p >= token_buffer + maxtoken)
	    p = extend_token_buffer(p);
	  *p++ = c;
	  c = getc(finput);
	}

      *p = 0;
      ungetc(c, finput);

      value = IDENTIFIER;
      yylval.itype = 0;

      if (p - token_buffer <= MAXRESERVED)
	{
	  register int lim = frw [p - token_buffer + 1];
	  register int i;

	  for (i = frw[p - token_buffer]; i < lim; i++)
	    if (rw[i][0] == token_buffer[0] && !strcmp(rw[i], token_buffer))
	      {
		if (rid[i])
		  yylval.ttype = ridpointers[(int) rid[i]];
		value = (int) rtoken[i];
		break;
	      }
	}

      if (value == IDENTIFIER)
	{
          yylval.ttype = get_identifier(token_buffer);
	  lastiddecl = lookup_name (yylval.ttype);

	  if (lastiddecl != 0 && TREE_CODE (lastiddecl) == TYPE_DECL)
	    value = TYPENAME;
	}

      break;

    case '0':  case '1':  case '2':  case '3':  case '4':
    case '5':  case '6':  case '7':  case '8':  case '9':
    case '.':
      {
	int base = 10;
	int count = 0;
	/* for multi-precision arithmetic, we store only 8 live bits in each short,
	   giving us 64 bits of reliable precision */
	short shorts[8];
	char *floatflag = NULL;  /* set nonzero if we learn this is a floating constant */
				 /* in fact, it points to the first fractional digit.  */

	for (count = 0; count < 8; count++)
	  shorts[count] = 0;

	p = token_buffer;
	*p++ = c;

	if (c == '0')
	  {
	    *p++ = (c = getc(finput));
	    if ((c == 'x') || (c == 'X'))
	      {
		base = 16;
		*p++ = (c = getc(finput));
	      }
	    else
	      {
		base = 8;
	      }
	  }

	while (c == '.'
	       || (isalnum (c) && (c != 'l') && (c != 'L')
		   && (c != 'u') && (c != 'U')))
	  {
	    if (c == '.')
	      {
		floatflag = p - 1;
		p[-1] = c = getc(finput); /* omit the decimal point from
				     the token buffer.  */
		/* Accept '.' as the start of a floating-point number
		   only when it is followed by a digit.
		   Otherwise, unread the following non-digit
		   and use the '.' as a structural token.  */
		if (floatflag == token_buffer && !isdigit (c))
		  {
		    if (c == '.')
		      {
			c = getc (finput);
			if (c == '.')
			  return ELLIPSIS;
			yyerror ("syntax error");
		      }
		    ungetc (c, finput);
		    return '.';
		  }
	      }
	    else
	      {
		if (isdigit(c))
		  {
		    c = c - '0';
		  }
		else if (base <= 10)
		  {
		    if ((c&~040) == 'E')
		      {
			if (floatflag == 0)
			  floatflag = p - 1;
			break;   /* start of exponent */
		      }
		    yyerror("nondigits in number and not hexadecimal");
		    c = 0;
		  }
		else if (c >= 'a')
		  {
		    c = c - 'a' + 10;
		  }
		else
		  {
		    c = c - 'A' + 10;
		  }
		if (c >= base)
		  yyerror("numeric constant contains digits beyond the radix");
	    
		for (count = 0; count < 8; count++)
		  {
		    (shorts[count] *= base);
		    if (count)
		      {
			shorts[count] += (shorts[count-1] >> 8);
			shorts[count-1] &= (1<<8)-1;
		      }
		    else shorts[0] += c;
		  }
    
		*p++ = (c = getc(finput));
	      }
	  }

	/* Remove terminating char from the token buffer and delimit the string */
	*--p = 0;

	if (floatflag)
	  {
	    register ex = -(p - floatflag);  /* exponent is minus # digits after decimal pt */

	    tree type = double_type_node;

	    /* read explicit exponent if any, and add into ex. */

	    if ((c == 'e') || (c == 'E'))
	      {
		register int exval = 0;
		register int exsign = 1;

		c = getc(finput);
		if ((c == '+') || (c == '-'))
		  {
		    if (c == '-') exsign = -1;
		    c = getc(finput);
		  }
	        while (isdigit(c))
		  {
		    exval *= 10;
		    exval += c - '0';
		    c = getc(finput);
		  }
		ex += exsign*exval;
	      }

	    while (1)
	      {
		if (c == 'f' || c == 'F')
		  type = float_type_node;
		else if (c == 'l' || c == 'L')
		  type = long_double_type_node;
		else break;
		c = getc (finput);
	      }

	    ungetc(c, finput);

	    yylval.ttype = build_real_from_string (token_buffer, ex);
	    TREE_TYPE (yylval.ttype) = type;
	  }
	else
	  {
	    tree type;
	    int spec_unsigned = 0;
	    int spec_long = 0;

	    while (1)
	      {
		if (c == 'u' || c == 'U')
		  {
		    spec_unsigned = 1;
		    c = getc (finput);
		  }
		else if (c == 'l' || c == 'L')
		  {
		    spec_long = 1;
		    c = getc (finput);
		  }
		else break;
	      }

	    ungetc (c, finput);

	    /* This is simplified by the fact that our constant
	       is always positive.  */
	    yylval.ttype
	      = build_int_2 ((shorts[3]<<24) + (shorts[2]<<16) + (shorts[1]<<8) + shorts[0],
			     (shorts[7]<<24) + (shorts[6]<<16) + (shorts[5]<<8) + shorts[4]);
    
	    if (!spec_long && !spec_unsigned
		&& int_fits_type_p (yylval.ttype, integer_type_node))
	      type = integer_type_node;

	    else if (!spec_long && base != 10
		&& int_fits_type_p (yylval.ttype, unsigned_type_node))
	      type = unsigned_type_node;

	    else if (!spec_unsigned
		&& int_fits_type_p (yylval.ttype, long_integer_type_node))
	      type = long_integer_type_node;

	    else
	      type = long_unsigned_type_node;

	    TREE_TYPE (yylval.ttype) = type;
	  }

	value = CONSTANT; break;
      }

    case '\'':
      c = getc(finput);
      {
	register int code = 0;

      tryagain:

	if (c == '\\')
	  {
	    c = readescape ();
	    if (c < 0)
	      goto tryagain;
	  }
	code = c;
	c = getc (finput);
	if (c != '\'')
	  yyerror("malformatted character constant");

	if (char_type_node == unsigned_char_type_node
	    || (c >> (BITS_PER_UNIT - 1)) == 0)
	  yylval.ttype = build_int_2 (code, 0);
	else
	  yylval.ttype = build_int_2 (code | (1 << BITS_PER_UNIT), -1);

	TREE_TYPE (yylval.ttype) = char_type_node;
	value = CONSTANT; break;
      }

    case '"':
      {
	c = getc(finput);
	p = token_buffer;

	while (c != '"')
	  {
	    if (c == '\\')
	      {
		c = readescape ();
		if (c < 0)
		  goto skipnewline;
	      }
	    else if (c == '\n')
	      {
		lineno++;
	      }

	    if (p == token_buffer + maxtoken)
	      p = extend_token_buffer(p);
	    *p++ = c;

	  skipnewline:
	    c = getc (finput);
	  }

	*p++ = 0;

	yylval.ttype = build_string (p - token_buffer, token_buffer);
	TREE_TYPE (yylval.ttype) = char_array_type_node;

	value = STRING; break;
      }
      
    case '+':
    case '-':
    case '&':
    case '|':
    case '<':
    case '>':
    case '*':
    case '/':
    case '%':
    case '^':
    case '!':
    case '=':
      {
	register int c1;

      combine:

	switch (c)
	  {
	  case '+':
	    yylval.code = PLUS_EXPR; break;
	  case '-':
	    yylval.code = MINUS_EXPR; break;
	  case '&':
	    yylval.code = BIT_AND_EXPR; break;
	  case '|':
	    yylval.code = BIT_IOR_EXPR; break;
	  case '*':
	    yylval.code = MULT_EXPR; break;
	  case '/':
	    yylval.code = TRUNC_DIV_EXPR; break;
	  case '%':
	    yylval.code = TRUNC_MOD_EXPR; break;
	  case '^':
	    yylval.code = BIT_XOR_EXPR; break;
	  case LSHIFT:
	    yylval.code = LSHIFT_EXPR; break;
	  case RSHIFT:
	    yylval.code = RSHIFT_EXPR; break;
	  case '<':
	    yylval.code = LT_EXPR; break;
	  case '>':
	    yylval.code = GT_EXPR; break;
	  }	

	c1 = getc(finput);

	if (c1 == '=')
	  {
	    switch (c)
	      {
	      case '<':
		value = ARITHCOMPARE; yylval.code = LE_EXPR; goto done;
	      case '>':
		value = ARITHCOMPARE; yylval.code = GE_EXPR; goto done;
	      case '!':
		value = EQCOMPARE; yylval.code = NE_EXPR; goto done;
	      case '=':
		value = EQCOMPARE; yylval.code = EQ_EXPR; goto done;
	      }	
	    value = ASSIGN; goto done;
	  }
	else if (c == c1)
	  switch (c)
	    {
	    case '+':
	      value = PLUSPLUS; goto done;
	    case '-':
	      value = MINUSMINUS; goto done;
	    case '&':
	      value = ANDAND; goto done;
	    case '|':
	      value = OROR; goto done;
	    case '<':
	      c = LSHIFT;
	      goto combine;
	    case '>':
	      c = RSHIFT;
	      goto combine;
	    }
	else if ((c == '-') && (c1 == '>'))
	  { value = POINTSAT; goto done; }
	ungetc (c1, finput);

	if ((c == '<') || (c == '>'))
	  value = ARITHCOMPARE;
	else value = c;
	goto done;
      }

    default:
      value = c;
    }

done:
  yylloc.last_line = lineno;

  return (value);
}
