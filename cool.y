*
 * cool_parser.y
 * Definition for the COOL language parser.
 */
%{
#include "cool-io.h"        // includes standard I/O library
#include "cool-tree.h"      // syntax tree definitions
#include "stringtab.h"      // string table for symbols
#include "utilities.h"      // utility functions

/* Token location types */
#define YYLTYPE int        /* the type of locations */
#define yylloc curr_lineno /* current line number for token locations */
extern int lineno;         /* line number before creating a syntax tree node */

/* Default actions for locations. Set lineno to the location of the first token. */
#define YYLLOC_DEFAULT(Cur, Rhs, N) \
  Cur = Rhs[1];                     \
  lineno = Cur;

#define SET_LINENO(NodeLoc) \
  lineno = NodeLoc;

extern char *filename;

void syntax_error(char s); / function to handle syntax errors */
extern int lex();           /* entry point to the lexer */

/* Parser output */
Program root_ast;       /* root of the syntax tree */
Classes parsed_classes; /* used for semantic analysis */
int num_errors = 0;     /* count of lexing and parsing errors */
%}

/* Union for yacc to hold parsed values */
%union {
  Boolean bool_val;
  Symbol sym_val;
  Program prog_val;
  Class_ cls_val;
  Classes cls_list_val;
  Feature feat_val;
  Features feat_list_val;
  Formal frm_val;
  Formals frm_list_val;
  Case case_val;
  Cases case_list_val;
  Expression expr_val;
  Expressions expr_list_val;
  char *error_msg;
}

/* Token declarations with associated types */
%token CLASS 258 ELSE 259 FI 260 IF 261 IN 262
%token INHERITS 263 LET 264 LOOP 265 POOL 266 THEN 267 WHILE 268
%token CASE 269 ESAC 270 OF 271 DARROW 272 NEW 273 ISVOID 274
%token <sym_val> STR_CONST 275 INT_CONST 276
%token <bool_val> BOOL_CONST 277
%token <sym_val> TYPEID 278 OBJECTID 279
%token ASSIGN 280 NOT 281 LE 282 ERROR 283

/* Types for non-terminals */
%type <prog_val> program
%type <cls_list_val> class_list
%type <cls_val> class
%type <feat_list_val> feature_list
%type <feat_val> feature
%type <frm_list_val> formal_list
%type <frm_val> formal
%type <case_list_val> case_list
%type <case_val> case
%type <expr_list_val> expression_list dispatch_list
%type <expr_val> expression

/* Operator precedence */
%nonassoc LE '='
%left NOT
%left '+' '-'
%left '*' '/'
%left ISVOID
%left '~'

%%

/* Grammar rules and actions */
program
  : class_list { @$ = @1; root_ast = program($1); };

class_list
  : class      { $$ = single_Classes($1); parsed_classes = $$; }
  | class_list class { $$ = append_Classes($1, single_Classes($2)); parsed_classes = $$; }
  | error ';' {};

class
  : CLASS TYPEID '{' feature_list '}' ';'
    { $$ = class_($2, idtable.add_string("Object"), $4, stringtable.add_string(filename)); }
  | CLASS TYPEID INHERITS TYPEID '{' feature_list '}' ';'
    { $$ = class_($2, $4, $6, stringtable.add_string(filename)); };

feature_list
  : { $$ = nil_Features(); } /* empty feature list */
  | feature_list feature { $$ = append_Features($1, single_Features($2)); }
  | error ';' {};

feature
  : OBJECTID ':' TYPEID ';'
    { $$ = attr($1, $3, no_expr()); }
  | OBJECTID ':' TYPEID ASSIGN expression ';'
    { $$ = attr($1, $3, $5); }
  | OBJECTID '(' formal_list ')' ':' TYPEID '{' expression '}' ';'
    { $$ = method($1, $3, $6, $8); };

formal_list
  : { $$ = nil_Formals(); } /* no formal parameters */
  | formal { $$ = single_Formals($1); }
  | formal_list ',' formal { $$ = append_Formals($1, single_Formals($3)); };

formal
  : OBJECTID ':' TYPEID { $$ = formal($1, $3); };

case_list
  : { $$ = nil_Cases(); } /* empty case list */
  | case_list case { $$ = append_Cases($1, single_Cases($2)); };

case
  : OBJECTID ':' TYPEID DARROW expression ';'
    { $$ = branch($1, $3, $5); };

expression_list
  : expression ';' { $$ = single_Expressions($1); }
  | expression_list expression ';' { $$ = append_Expressions($1, single_Expressions($2)); }
  | expression_list error ';' {}
  | expression_list error {}
  | error {};

dispatch_list
  : { $$ = nil_Expressions(); }
  | expression { $$ = single_Expressions($1); }
  | dispatch_list ',' expression { $$ = append_Expressions($1, single_Expressions($3)); }
  | dispatch_list error ';' {};

expression
  : OBJECTID ASSIGN expression { $$ = assign($1, $3); }
  | expression '@' TYPEID '.' OBJECTID '(' dispatch_list ')' { $$ = static_dispatch($1, $3, $5, $7); }
  | expression '.' OBJECTID '(' dispatch_list ')' { $$ = dispatch($1, $3, $5); }
  | OBJECTID '(' dispatch_list ')' { $$ = dispatch(object(idtable.add_string("self")), $1, $3); }
  | IF expression THEN expression ELSE expression FI { $$ = cond($2, $4, $6); }
  | WHILE expression LOOP expression POOL { $$ = loop($2, $4); }
  | '{' expression_list '}' { $$ = block($2); }
  | OBJECTID ':' TYPEID ',' expression { $$ = let($1, $3, no_expr(), $5); }
  | OBJECTID ':' TYPEID IN expression { $$ = let($1, $3, no_expr(), $5); }
  | LET OBJECTID ':' TYPEID ',' expression { $$ = let($2, $4, no_expr(), $6); }
  | OBJECTID ':' TYPEID ASSIGN expression ',' expression { $$ = let($1, $3, $5, $7); }
  | OBJECTID ':' TYPEID ASSIGN expression IN expression { $$ = let($1, $3, $5, $7); }
  | LET OBJECTID ':' TYPEID ASSIGN expression ',' expression { $$ = let($2, $4, $6, $8); }
  | LET OBJECTID ':' TYPEID IN expression { $$ = let($2, $4, no_expr(), $6); }
  | LET OBJECTID ':' TYPEID ASSIGN expression IN expression { $$ = let($2, $4, $6, $8); }
  | CASE expression OF case_list ESAC { $$ = typcase($2, $4); }
  | NEW TYPEID { $$ = new_($2); }
  | ISVOID expression { $$ = isvoid($2); }
  | expression '+' expression { $$ = plus($1, $3); }
  | expression '-' expression { $$ = sub($1, $3); }
  | expression '*' expression { $$ = mul($1, $3); }
  | expression '/' expression { $$ = divide($1, $3); }
  | '~' expression { $$ = neg($2); }
  | expression LE expression { $$ = leq($1, $3); }
  | expression '<' expression { $$ = lt($1, $3); }
  | expression '=' expression { $$ = eq($1, $3); }
  | NOT expression { $$ = comp($2); }
  | '(' expression ')' { $$ = $2; }
  | OBJECTID { $$ = object($1); }
  | INT_CONST { $$ = int_const($1); }
  | STR_CONST { $$ = string_const($1); }
  | BOOL_CONST { if($1) $$ = bool_const(true); else $$ = bool_const(false); };

%%

/* This function is called automatically when Bison detects a parse error. */
void syntax_error(char *s)
{
  extern int curr_lineno;

  cerr << "\"" << filename << "\", line " << curr_lineno << ": " \
    << s << " at or near ";
  print_cool_token(yychar);
  cerr << endl;
  num_errors++;

  if(num_errors>50) {fprintf(stdout, "More than 50 errors\n"); exit(1);}
}
