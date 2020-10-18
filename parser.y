%{
#include <cstdio>
#include <memory>
#include "ast.hpp"
#include "lexer.hpp"
#include "SymTable.hpp"
%}

%token T_and			"and"
%token T_bool			"bool"
%token T_char			"char"
%token T_decl			"decl"
%token T_def			"def"
%token T_else			"else"
%token T_elsif			"elsif"
%token T_end			"end"
%token T_exit			"exit"
%token T_false			"false"
%token T_for			"for"
%token T_head			"head"
%token T_if				"if"
%token T_int			"int"
%token T_list			"list"
%token T_mod			"mod"
%token T_new			"new"
%token T_nil			"nil"
%token T_nilq			"nil?"
%token T_not			"not"
%token T_or				"or"
%token T_ref			"ref"
%token T_return			"return"
%token T_skip			"skip"
%token T_tail			"tail"
%token T_true			"true"
%token T_le				"<="
%token T_ge				">="
%token T_ne				"<>"
%token T_assign			":="
%token T_int_const<num>
%token T_char_const<ch>
%token T_id<name>
%token T_string<str>

%left "or"
%left "and"
%nonassoc "not"
%nonassoc '=' "<>" '>' '<' "<=" ">="
%right '#'
%left '+' '-'
%left '*' '/' "mod"

%union {
	AST* t;
	char ch;
	char* str;
	int num;
}

%type<t> simple
%type<t> atom
%type<t> expr
%type<t> call
%type<str> T_id
%type<num> T_int_const
%type<ch> T_char_const
%type<str> T_string

%%

programm:
	func_def
;

func_def:
	"def" header ':' func_def_list stmt_list "end"
;

func_def_list:
	/* nothing */
|	func_def func_def_list
|	func_decl func_def_list
|	var_def func_def_list
;

stmt_list:
	stmt stmt_list2
;

stmt_list2:
	/* nothing */
|	stmt stmt_list2
;

header:
	type T_id '(' formal_list ')'
|	T_id '(' formal_list ')'
;

formal_list:
	/* nothing */
|	formal formal_list2
;

formal_list2:
	/* nothing */
|	';' formal formal_list2
;

formal:
	"ref" type T_id id_par_list
|	type T_id id_par_list
;

id_par_list:
	/* nothing */
|	',' T_id id_par_list
;

type:
	"int"
|	"bool"
|	"char"
|	type '[' ']'
|	"list" '[' type ']'
;

func_decl:
	"decl" header
;

var_def:
	type T_id id_list
;

id_list:
	/* nothing */
|	',' T_id id_list
;

stmt:
	simple
|	"exit"
|	"return" expr
|	if_stmt
|	for_stmt
;

simple:
	"skip" {$$ = new Skip();}
|	atom ":=" expr {$$ = new Assign($1, $3);}
|	call {$$ = $1;}
;

if_stmt:
	"if" expr ':' stmt_list elsif else "end"
;

elsif:
	/* nothing */
|	"elsif" expr ':' stmt_list elsif
;

else:
	/* nothing */
|	"else" ':' stmt_list
;

for_stmt:
	"for" simple_list ';' expr ';' simple_list ':' stmt_list "end"
;

simple_list:
	simple simple_list2
;

simple_list2:
	/* nothing */
|	',' simple simple_list2
;

call:
	T_id '(' ')'
|	T_id '(' expr_list ')'
;

expr_list:
	expr expr_list2
;

expr_list2:
	/* nothing */
|	',' expr expr_list2
;

atom:
	T_id {$$ = new Var(std::string $1);}
|	T_string {$$ = new String(std::string $1);}
|	atom '[' expr ']' {$$ = new Arr($1, $3);}
|	call {$$ = $1;}
;

expr:
	atom {$$ = $1;}
|	T_int_const {$$ = new Int($1);}
|	T_char_const {$$ = new Char($1);}
|	'(' expr ')' {$$ = $2;}
|	'+' expr {new UnOp(PLUS, $2);}
|	'-' expr {new UnOp(MINUS, $2);}
|	expr '+' expr {$$ = new BinOp($1, PLUS, $3);}
|	expr '-' expr {$$ = new BinOp($1, MINUS, $3);}
|	expr '*' expr {$$ = new BinOp($1, MUL, $3);}
|	expr '/' expr {$$ = new BinOp($1, DIV, $3);}
|	expr "mod" expr {$$ = new BinOp($1, MOD, $3);}
|	expr '=' expr {$$ = new BinOp($1, EQ, $3);}
|	expr "<>" expr {$$ = new BinOp($1, NOT_EQ, $3);}
|	expr '<' expr {$$ = new BinOp($1, LESS, $3);}
|	expr '>' expr {$$ = new BinOp($1, GREATER, $3);}
|	expr "<=" expr {$$ = new BinOp($1, LESS_EQ, $3);}
|	expr ">=" expr {$$ = new BinOp($1, GREATER_EQ, $3);}
|	"true" {$$ = new Bool(true);}
|	"false" {$$ = new Bool(false);}
|	"not" expr {new UnOp(NOT, $2);}
|	expr "and" expr {$$ = new BinOp($1, AND, $3);}
|	expr "or" expr {$$ = new BinOp($1, OR, $3);}
|	"new" type '[' expr ']' {$$ = new New($2, $4);}
|	expr '#' expr {$$ = new BinOp($1, CONS, $3);}
|	"nil" {$$ = new Nil();}
|	"nil?" '(' expr ')' {new UnOp(NILQ, $3);}
|	"head" '(' expr ')' {new UnOp(HEAD, $3);}
|	"tail" '(' expr ')'  {new UnOp(TAIL, $3);}
;

%%

int main() {
  int result = yyparse();
  if (result == 0) printf("Success.\n");
  else printf("Failed.\n");
  return 0;
}
