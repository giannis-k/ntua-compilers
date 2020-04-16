%{
#include <cstdio>
#include "lexer.hpp"
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
