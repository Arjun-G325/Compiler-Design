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

%option noyywrap yylineno 
%x STRING

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
"enum"  { return ENUM; }
"union" { return UNION; }
"case" { return CASE; }
"default" { return DEFAULT; }
"struct" { return STRUCT; }
"typedef" { return TYPEDEF; }
"static" { return STATIC; }
"const" { return CONST; }
"auto" { return AUTO; }
"class" { return CLASS; }
"private" { return PRIVATE; }
"public" { return PUBLIC; }
"protected" { return PROTECTED; }
"bool" { return T_BOOL; }
"void" { return T_VOID; }
"int" { return T_INT; }
"char" { return T_CHAR; }
"float" { return T_FLOAT; }
"double" { return T_DOUBLE; }
"unsigned int" { return T_UNSIGNED_INT; }
"sizeof" { return SIZEOF; }
"va_list" { return VA_LIST; }
"va_arg" { return VA_ARG; }

[0-9]+\.[0-9]+ { yylval.dval = atof(yytext); return FLOAT_LITERAL; }
[0-9]+ { yylval.ival = atoi(yytext); return INT_LITERAL; }
[a-zA-Z_][a-zA-Z0-9_]* { yylval.str = strdup(yytext); return IDENTIFIER; }

'[^']*' {
    yylval.str = strdup(yytext);
    return CHAR_LITERAL;
}

\"(\\.|[^"\n])*\" {
    yylval.str = strdup(yytext);
    return STRING_LITERAL;
}

"+=" { return PLUS_ASSIGN; }
"-=" { return MINUS_ASSIGN; }
"*=" { return ASTERISK_ASSIGN; }
"/=" { return DIV_ASSIGN; }
"%=" { return MOD_ASSIGN; }
"&=" { return AND_ASSIGN; }
"|=" { return OR_ASSIGN; }
"^=" { return XOR_ASSIGN; }
"<<=" { return SHL_ASSIGN; }
">>=" { return SHR_ASSIGN; }

"<<" { return SHL; }
">>" { return SHR; }
"<=" { return LEQ; }
">=" { return GEQ; }
"<" { return LT; }
">" { return GT; }
"==" { return EQ; }
"!=" { return NEQ; }
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
"..." { return DOT_DOT_DOT; }
"." { return DOT; }
"->" { return ARROW; }

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

. { cerr<<"Unknown character: "<<yytext<<endl; }
%%
