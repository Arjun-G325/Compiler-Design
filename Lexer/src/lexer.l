%{
#include <iostream>
#include <cstdlib>
#include <string>
#include <vector>
#include "../../Parser/src/y.tab.h"

int line=1;
extern YYSTYPE yylval;

using namespace std;
%}

%option noyywrap
%x STRING CHAR

%%

\n { line++; }
[ \t]+ {}
"if" { return IF; }
"else" { return ELSE; }
"for" { return FOR; }
"while" { return WHILE; }
"do" { return DO; }
"return" { return RETURN; }
"goto" { return GOTO; }
"break" { return BREAK; }
"continue" { return CONTINUE; }
"switch" { return SWITCH; }
"case" { return CASE; }
"default" { return DEFAULT; }
"struct" { return STRUCT; }
"typedef" { return TYPEDEF; }
"static" { return STATIC; }
"const" { return CONST; }
"auto" { return AUTO; }
"class" { return CLASS; }
"private" { return PRIVATE; }
"protected" { return PROTECTED; }
"public" { return PUBLIC; }
"enum" { return ENUM; }
"union" { return UNION; }
"sizeof" { return SIZEOF; }
"extern" { return EXTERN; }

"void" { return T_VOID; }
"int" { return T_INT; }
"char" { return T_CHAR; }
"float" { return T_FLOAT; }
"double" { return T_DOUBLE; }
"unsigned int" { return T_UNSIGNED_INT; }

[a-zA-Z_][a-zA-Z0-9_]* { yylval.str=strdup(yytext); return IDENTIFIER; }

[0-9]+ { yylval.ival=atoi(yytext); return INT_LITERAL; }
([0-9]+)?\.[0-9]+([eE][-+]?[0-9]+)? { yylval.dval=atof(yytext); return FLOAT_LITERAL; }
[0-9]+\.[0-9]*([eE][-+]?[0-9]+)? { yylval.dval=atof(yytext); return FLOAT_LITERAL; }
[0-9]+[eE][-+]?[0-9]+ { yylval.dval=atof(yytext); return FLOAT_LITERAL; }

"[]" { return LAMBDA; }
"->" { return ARROW; }
"==" { return EQ; }
"!=" { return NEQ; }
"<<" { return SHL; }
">>" { return SHR; }
"<=" { return LEQ; }
">=" { return GEQ; }
"<" { return LT; }
">" { return GT; }
"&&" { return AND; }
"||" { return OR; }
"!" { return NOT; }
"&" { return AMPERSAND; }
"|" { return BIT_OR; }
"^" { return BIT_XOR; }
"~" { return BIT_NOT; }
"++" { return INC; }
"--" { return DEC; }
"+" { return PLUS; }
"-" { return MINUS; }
"*" { return ASTERISK; }
"/" { return DIV; }
"%" { return MOD; }
"=" { return ASSIGN; }
"::" { return COLON_COLON; }
"." { return DOT; }

"(" { return LPAREN; }
")" { return RPAREN; }
"{" { return LBRACE; }
"}" { return RBRACE; }
"[" { return LBRACKET; }
"]" { return RBRACKET; }
":" { return COLON; }
";" { return SEMICOLON; }
"," { return COMMA; }
"#" { return HASH; }
"?" { return QUESTION; }

\" { BEGIN(STRING); yymore(); }
<STRING>[^"\n\\]+ { yymore(); }
<STRING>\\. { yymore(); }
<STRING>\" { yylval.str=strdup(yytext); BEGIN(INITIAL); return STRING_LITERAL; }
<STRING>\n { cerr<<"Unterminated string literal at line "<<line<<endl; BEGIN(INITIAL); return ERROR; }
<STRING><<EOF>> { cerr<<"Unterminated string literal at line "<<line<<endl; BEGIN(INITIAL); return ERROR; }

\' { BEGIN(CHAR); yymore(); }
<CHAR>[^\\\n'] { yymore(); }
<CHAR>\\. { yymore(); }
<CHAR>\' { yylval.str=strdup(yytext); BEGIN(INITIAL); return CHAR_LITERAL; }
<CHAR>\n { cerr<<"Unterminated char literal at line "<<line<<endl; BEGIN(INITIAL); return ERROR; }
<CHAR><<EOF>> { cerr<<"Unterminated char literal at line "<<line<<endl; BEGIN(INITIAL); return ERROR; }

. {}

%%
