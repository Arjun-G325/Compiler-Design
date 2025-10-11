%{
#include <iostream>
#include <cstdlib>
#include <string>
#include <vector>
#include "../../Parser/src/parser.tab.h"

int line=1;
extern YYSTYPE yylval;

using namespace std;
%}

%option noyywrap yylineno 
%x STRING
%x COMMENT

%%

\n { line++; }
[ \t]+ {}

"//".* {}
"/*"                    BEGIN(COMMENT);
<COMMENT>{
"*/"                    BEGIN(INITIAL);
\n                      { line++; }
.                       {}
}

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
"bool" { return BOOL; }
"void" { return VOID; }
"int" { return INT; }
"char" { return CHAR; }
"float" { return FLOAT; }
"double" { return DOUBLE; }
"unsigned int" { return INT; }
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

"+=" { return ADD_ASSIGN; }
"-=" { return SUB_ASSIGN; }
"*=" { return MUL_ASSIGN; }
"/=" { return DIV_ASSIGN; }
"%=" { return MOD_ASSIGN; }
"&=" { return AND_ASSIGN; }
"|=" { return OR_ASSIGN; }
"^=" { return XOR_ASSIGN; }
"<<=" { return SHL_ASSIGN; }
">>=" { return SHR_ASSIGN; }

"<<" { return LSHIFT_OP; }
">>" { return RSHIFT_OP; }
"<=" { return LE_OP; }
">=" { return GE_OP; }
"<" { return '<'; }
">" { return '>'; }
"==" { return EQ_OP; }
"!=" { return NE_OP; }
"&&" { return AND_OP; }
"||" { return OR_OP; }
"!" { return '!'; }
"&" { return '&'; }
"|" { return '|'; }
"^" { return '^'; }
"~" { return '~'; }
"++" { return INC_OP; }
"--" { return DEC_OP; }
"+" { return '+'; }
"-" { return '-'; }
"*" { return '*'; }
"/" { return '/'; }
"%" { return '%'; }
"=" { return ASSIGN; }
"::" { return COLON_COLON; }
"..." { return DOT_DOT_DOT; }
"." { return DOT; }
"->" { return PTR_OP; }

"(" { return LPAREN; }
")" { return RPAREN; }
"{" { return LBRACE; }
"}" { return RBRACE; }
"[" { return LBRACKET; }
"]" { return RBRACKET; }
":" { return ':'; }
";" { return SEMICOLON; }
"," { return COMMA; }
"#" { return '#'; }
"?" { return '?'; }

. { cerr<<"Unknown character: "<<yytext<<endl; }
%%
