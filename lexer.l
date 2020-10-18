%option noyywrap
%option nounput
%option noinput

%{
#define T_eof	0

#include <cstdio>
#include <cstdlib>
#include "lexer.hpp"
#include "parser.hpp"

int  line=1;
int  comment_num=0;

void fix_esc_char( char * );
void fix_esc_string( char * );
int hexToInt( char );

%}

L [A-Za-z]
D [0-9]
HEX [0-9|A-F|a-f]
W [ \t\r]
ESC \\(n|t|r|0|\\|\'|\"|x{HEX}{HEX})

%x COMMENT

%%

"and"		{ return T_and; }
"bool"		{ return T_bool; }
"char"		{ return T_char; }
"decl"		{ return T_decl; }
"def"		{ return T_def; }
"else"		{ return T_else; }
"elsif"		{ return T_elsif; }
"end"		{ return T_end; }
"exit"		{ return T_exit; }
"false"		{ return T_false; }
"for"		{ return T_for; }
"head"		{ return T_head; }
"if"		{ return T_if; }
"int"		{ return T_int; }
"list"		{ return T_list; }
"mod"		{ return T_mod; }
"new"		{ return T_new; }
"nil"		{ return T_nil; }
"nil?"		{ return T_nilq; }
"not"		{ return T_not; }
"or"		{ return T_or; }
"ref"		{ return T_ref; }
"return"	{ return T_return; }
"skip"		{ return T_skip; }
"tail"		{ return T_tail; }
"true"		{ return T_true; }

{L}({L}|{D}|_|\?)*		{yylval.str = strdup(yytext); return T_id; }

{D}+					{yylval.num = atoi(yytext); return T_int_const; }

\'([^\"\'\\]|{ESC})\'	{ fix_esc_char(yytext); yylval.ch = yytext[0]; return T_char_const; }

\"([^\n\"\'\\]|{ESC})*\" { fix_esc_string(yytext); yylval.str = strdup(yytext); return T_string; }

[+|\-|*|/|#|=|<|>]	{ return yytext[0]; }

"<="				{ return T_le; }
">="				{ return T_ge; }
"<>"				{ return T_ne; }

[\(|\)|\[|\]|,|;|:]	{ return yytext[0]; }

":="				{ return T_assign; }

%[^\n]*\n			{ ++line; }

"<*"				{ ++comment_num;  BEGIN(COMMENT);}
<COMMENT>"<*"		{ ++comment_num; }
<COMMENT>"*>"		{ --comment_num; if(comment_num==0) BEGIN(INITIAL); }
<COMMENT>"\n"		{ ++line; }
<COMMENT>"*"		{ /* no action */ }
<COMMENT>[^*\n]		{ /* no action */ }

{W}					{ /* no action */ }

\n						{ ++line; }

<<EOF>>					{ if(comment_num!=0) { fprintf(stderr, "Comment starts but doesn't close\n"); exit(1); } return T_eof; }

.						{ fprintf(stderr, "Illegal character with code %d in line %d\n", yytext[0], line); exit(1); }


%%

void fix_esc_char(char * yytext)
{
	int h1, h2;
	if(yytext[1]!='\\')
	{
		yytext[0]=yytext[1];
		yytext[1]='\0';
		return;
	}
	else
	{
		switch(yytext[2]){
			case 'n': {yytext[0]='\n'; yytext[1]='\0'; return;}
			case 't': {yytext[0]='\t'; yytext[1]='\0'; return;}
			case 'r': {yytext[0]='\r'; yytext[1]='\0'; return;}
			case '0': {yytext[0]='\0'; yytext[1]='\0'; return;}
			case '\\': {yytext[0]='\\'; yytext[1]='\0'; return;}
			case '\'': {yytext[0]='\''; yytext[1]='\0'; return;}
			case '\"': {yytext[0]='\"'; yytext[1]='\0'; return;}
			case 'x': {h1=hexToInt(yytext[3]); h2=hexToInt(yytext[4]); yytext[0]=16*h1+h2; yytext[1]='\0'; return;}
			default : {fprintf(stderr, "Illegal escape character in line %d\n", line); exit(1);}
		}
	}
}

void fix_esc_string(char * yytext)
{
	if(yytext[0]=='\"' && yytext[1]=='\"')
	{
		yytext[0]='\0';
		return;
	}
	int i=1, j, len, h1, h2;
	bool flag;
	yytext[0]=yytext[1];
	while(yytext[i]!='\0')
	{
		yytext[i]=yytext[i+1];
		++i;
	}
	yytext[i-1]='\0';
	len=i-1;
	for(i=0; i<len; ++i)
	{
		if(yytext[i]!='\\') continue;
		switch(yytext[i+1]){
			case 'n': {yytext[i]='\n'; --len; flag=false; break;}
			case 't': {yytext[i]='\t'; --len; flag=false; break;}
			case 'r': {yytext[i]='\r'; --len; flag=false; break;}
			case '0': {yytext[i]='\0'; --len; flag=false; break;}
			case '\\': {yytext[i]='\\'; --len; flag=false; break;}
			case '\'': {yytext[i]='\''; --len; flag=false; break;}
			case '\"': {yytext[i]='\"'; --len; flag=false; break;}
			case 'x': {h1=hexToInt(yytext[i+2]); h2=hexToInt(yytext[i+3]); yytext[i]=16*h1+h2; len-=3; flag=true; break;}
			default : {fprintf(stderr, "Illegal escape character in line %d\n", line); exit(1);}
		}
		j=i+1;
		if(flag)
		{
			while(yytext[j+2]!='\0')
			{
				yytext[j]=yytext[j+3];
				++j;
			}
		}
		else
		{
			while(yytext[j]!='\0')
			{
				yytext[j]=yytext[j+1];
				++j;
			}
			yytext[j-1]='\0';
		}
	}
	yytext[len-1]='\0';
}

int hexToInt(char a)
{
	if(a>='0' && a<='9')
		return a-'0';
	else if(a>='a' && a<='f')
		return a-'a'+10;
	else if(a>='A' && a<='F')
		return a-'A'+10;
	else
	{
		fprintf(stderr, "Illegal escape character in line %d\n", line);
		exit(1);
	}
	return -1;
}

void yyerror(const char *msg) {
  fprintf(stderr, "%s\n", msg);
  exit(1);
}
