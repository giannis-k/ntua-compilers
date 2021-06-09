%{
#include <cstdio>
#include <memory>
#include "ast.hpp"
#include "lexer.hpp"
#include "SymTable.hpp"

AST* root;

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
%token<num> T_int_const
%token<ch> T_char_const
%token<str> T_id
%token<str> T_string

%left "or"
%left "and"
%nonassoc "not"
%nonassoc '=' "<>" '>' '<' "<=" ">="
%right '#'
%left '+' '-'
%left '*' '/' "mod"

%union {
	AST* t;
	Parameter* par;
	Func* fun;
	Header* h;
	char ch;
	char* str;
	int num;
	Type* ty;
	std::vector<AST*>* vec;
	std::vector<Parameter*>* par_vec;
	std::vector<std::string>* vec_str;
}

%type<t> for_stmt
%type<vec> simple_list
%type<vec> stmt_list
%type<t> if_stmt
%type<t> elsif
%type<vec> elsif_list
%type<t> stmt
%type<vec> expr_list
%type<ty> type
%type<t> simple
%type<t> atom
%type<t> expr
%type<t> call
%type<vec_str> id_list
%type<t> var_def
%type<par> formal
%type<par_vec> formal_list
%type<h> header
%type<t> func_decl
%type<fun> func_def
%type<vec> func_def_list
%type<t> def
%type<fun>programm

%%

programm:
	func_def {$$ = $1; $$->setMain(); root = $$;}
;

func_def:
	"def" header ':' func_def_list stmt_list "end" {$$ = new Func($2, *$4, *$5);}
;

func_def_list:
	/* nothing */ {$$ = new std::vector<AST*>();}
|	func_def_list def {$$ = $1; $$->push_back($2);}
;

def:
	func_def {$$ = $1;}
|	func_decl {$$ = $1;}
|	var_def {$$ = $1;}
;

stmt_list:
	stmt {$$ = new std::vector<AST*>(1, $1);}
|	stmt_list stmt {$$ = $1; $$->push_back($2);}
;


header:
	type T_id '(' formal formal_list ')' {$5->push_back($4); $$ = new Header(std::shared_ptr<Type> $1, $2, *$5);}
|	type T_id '(' ')' {$$ = new Header(std::shared_ptr<Type> $1, $2);}
|	T_id '(' formal formal_list ')' {$4->push_back($3); $$ = new Header(std::make_shared<VoidType>(), $1, *$4);}
|	T_id '(' ')' {$$ = new Header(std::make_shared<VoidType>(), $1);}
;

formal_list:
	/* nothing */ {$$ = new std::vector<Parameter*>();}
|	formal_list ';' formal {$$ = $1; $$->push_back($3);}
;

formal:
	"ref" type T_id id_list {$4->push_back($3); $$ = new Parameter(std::shared_ptr<Type> $2, *$4, REF);}
|	type T_id id_list {$3->push_back($2); $$ = new Parameter(std::shared_ptr<Type> $1, *$3, VAL);}
;

type:
	"int" {$$ = new IntType();}
|	"bool" {$$ = new BoolType();}
|	"char" {$$ = new CharType();}
|	type '[' ']' {$$ = new ArrayType(std::shared_ptr<Type> $1);}
|	"list" '[' type ']' {$$ = new ListType(std::shared_ptr<Type> $3);}
;

func_decl:
	"decl" header {$$ = new Decl($2);}
;

var_def:
	type T_id id_list {$3->push_back($2); $$ = new VarDef(std::shared_ptr<Type> $1, *$3);}
;

id_list:
	/* nothing */ {$$ = new std::vector<std::string>();}
|	id_list ',' T_id {$$ = $1; $$->push_back($3);}
;

stmt:
	simple {$$ = $1;}
|	"exit" {$$ = new Exit();}
|	"return" expr {$$ = new Ret($2);}
|	if_stmt {$$ = $1;}
|	for_stmt {$$ = $1;}
;

simple:
	"skip" {$$ = new Skip();}
|	atom ":=" expr {$$ = new Assign($1, $3);}
|	call {$$ = $1;}
;

if_stmt:
	"if" expr ':' stmt_list elsif_list "end" {$$ = new If($2, *$4, *$5);}
|	"if" expr ':' stmt_list elsif_list "else" ':' stmt_list "end" {$$ = new If($2, *$4, *$5, *$8);}
;

elsif_list:
	/* nothing */ {$$ = new std::vector<AST*>();}
|	elsif_list elsif {$$ = $1; $$->push_back($2);}
;

elsif:
	"elsif" expr ':' stmt_list {$$ = new Elsif($2, *$4);}
;

for_stmt:
	"for" simple_list ';' expr ';' simple_list ':' stmt_list "end" {$$ = new For(*$2, $4, *$6, *$8);}
;

simple_list:
	simple {$$ = new std::vector<AST*>(1, $1);}
|	simple_list ',' simple {$$ = $1; $$->push_back($3);}
;


call:
	T_id '(' expr_list ')' {$$ = new Call(std::string $1, *$3);}
;

expr_list:
	/* nothing */ {$$ = new std::vector<AST*>();}
|	expr_list ',' expr {$$ = $1; $$->push_back($3);}
|	expr {$$ = new std::vector<AST*>(); $$->push_back($1);}
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
|	'+' expr {$$ = new UnOp(UPLUS, $2);}
|	'-' expr {$$ = new UnOp(UMINUS, $2);}
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
|	"not" expr {$$ = new UnOp(NOT, $2);}
|	expr "and" expr {$$ = new BinOp($1, AND, $3);}
|	expr "or" expr {$$ = new BinOp($1, OR, $3);}
|	"new" type '[' expr ']' {$$ = new New(std::shared_ptr<Type> $2, $4);}
|	expr '#' expr {$$ = new BinOp($1, CONS, $3);}
|	"nil" {$$ = new Nil();}
|	"nil?" '(' expr ')' {$$ = new UnOp(NILQ, $3);}
|	"head" '(' expr ')' {$$ = new UnOp(HEAD, $3);}
|	"tail" '(' expr ')'  {$$ = new UnOp(TAIL, $3);}
;

%%

int main(int argc, char **argv) {
  int result = yyparse();
  // if (result == 0) printf("Success.\n");
  // else
  // { 
  // 	printf("Failed.\n");
  // 	return 0;
  // }
  if(result!=0)
  {
  	printf("Parsing failed\n");
  	exit(1);
  }
  std::shared_ptr<SymTable> table = TableInit();
  root->sem(table);
  bool opt = (bool) argv[1][0];
  root->begin_compilation(opt);
  return 0;
}