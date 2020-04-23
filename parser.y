%{
#include <cstdio>
#include "lexer.hpp"
%}
%define parse.error verbose
%locations
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
%token T_int_const
%token T_char_const
%token T_id
%token T_string

%left "or"
%left "and"
%nonassoc "not"
%nonassoc '=' "<>" '>' '<' "<=" ">=" 
%right '#'
%left '+' '-'
%left '*' '/' "mod"

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
	"skip"
|	atom ":=" expr
|	call
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
	T_id
|	T_string
|	atom '[' expr ']'
|	call
;

expr:
	atom
|	T_int_const
|	T_char_const
|	'(' expr ')'
|	'+' expr
|	'-' expr
|	expr '+' expr
|	expr '-' expr
|	expr '*' expr
|	expr '/' expr
|	expr "mod" expr
|	expr '=' expr
|	expr "<>" expr
|	expr '<' expr
|	expr '>' expr
|	expr "<=" expr
|	expr ">=" expr
|	"true"
|	"false"
|	"not" expr
|	expr "and" expr
|	expr "or" expr
|	"new" type '[' expr ']'
|	expr '#' expr
|	"nil"
|	"nil?" '(' expr ')'
|	"head" '(' expr ')'
|	"tail" '(' expr ')'
;

%%

int main() {
  int result = yyparse();
  if (result == 0) printf("Success.\n");
  else printf("Failed.\n");
  return 0;
}
