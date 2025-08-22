%{
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include "tokens.h"

extern YYSTYPE yylval;
int line=1;
static char* duptext(const char* s){
    return s?strdup(s):NULL; 
}
%}
%option noyywrap nodefault

%%

\n                            { line++; }

"if"                          { return IF; }
"else"                        { return ELSE; }
"for"                         { return FOR; }
"while"                       { return WHILE; }
"return"                      { return RETURN; }
"goto"                        { return GOTO; }
"break"                       { return BREAK; }
"continue"                    { return CONTINUE; }
"switch"                      { return SWITCH; }
"case"                        { return CASE; }
"default"                     { return DEFAULT; }
"struct"                      { return STRUCT; }
"typedef"                     { return TYPEDEF; }
"static"                      { return STATIC; }
"const"                       { return CONST; }
"auto"                        { return AUTO; }
"class"                       { return CLASS; }
"private"                     { return PRIVATE; }
"protected"                   { return PROTECTED; }
"public"                      { return PUBLIC; }
"lambda"                      { return LAMBDA; }

"int"                         { return INT; }
"char"                        { return CHAR; }
"float"                       { return FLOAT; }
"double"                      { return DOUBLE; }
"void"                        { return VOID; }
"signed int"                  { return SIGNED_INT; }
"unsigned int"                { return UNSIGNED_INT; }

[a-zA-Z_][a-zA-Z0-9_]*        { yylval = duptext(yytext); return IDENTIFIER; }

([0-9]*\.[0-9]+|[0-9]+\.[0-9]*)([eE][-+]?[0-9]+)?|[0-9]+[eE][-+]?[0-9]+   { yylval = duptext(yytext); return FLOAT_LITERAL; }

[0-9]+                        { yylval = duptext(yytext); return INT_LITERAL; }

\"([^\\\n"]|\\.)*\"           { yylval = duptext(yytext); return STRING_LITERAL; }

\'([^\\\n']|\\[nt0\\'\"])\'   { yylval = duptext(yytext); return CHAR_LITERAL; }

"++"                          { return INC; }
"--"                          { return DEC; }

"=="                          { return EQ; }
"!="                          { return NEQ; }
"<="                          { return LEQ; }
">="                          { return GEQ; }
"<"                           { return LT; }
">"                           { return GT; }

"&&"                          { return AND; }
"||"                          { return OR; }
"!"                           { return NOT; }

"&"                           { return BIT_AND; }
"|"                           { return BIT_OR; }
"^"                           { return BIT_XOR; }
"~"                           { return BIT_NOT; }

"+"                           { return PLUS; }
"-"                           { return MINUS; }
"*"                           { return MUL; }
"/"                           { return DIV; }
"%"                           { return MOD; }

"("                           { return LPAREN; }
")"                           { return RPAREN; }
"{"                           { return LBRACE; }
"}"                           { return RBRACE; }
"["                           { return LBRACKET; }
"]"                           { return RBRACKET; }
":"                           { return COLON; }
";"                           { return SEMICOLON; }
","                           { return COMMA; }
"="                           { return ASSIGN; }

"/*"([^*]|\*+[^*/])*\*+"/"    {}
"//".*                        {}
[ \t\r]+                      {}

"/*"([^*]|\*+[^*/])*\*?       { fprintf(stderr,"Lexical error (unterminated comment) at line %d\n",line); return ERROR_TOK; }
\"([^\\\n"]|\\.)*             { fprintf(stderr,"Lexical error (unterminated string) at line %d\n",line); return ERROR_TOK; }
"''"                          { fprintf(stderr,"Lexical error (empty char literal) at line %d\n",line); return ERROR_TOK; }
\'[^\\\n']{2,}\'              { fprintf(stderr,"Lexical error (overlong char literal) at line %d\n",line); return ERROR_TOK; }
\'\\[^nt0\\\'"]\'             { fprintf(stderr,"Lexical error (invalid escape) at line %d\n",line); return ERROR_TOK; }
[0-9]+[a-zA-Z_]+              { fprintf(stderr,"Lexical error (invalid identifier) at line %d\n",line); return ERROR_TOK; }
[0-9]*\.(?:[0-9]*\.)+[0-9]*   { fprintf(stderr,"Lexical error (wrong float constant) at line %d\n",line); return ERROR_TOK; }
.                              { fprintf(stderr,"Lexical error (invalid char '%s') at line %d\n",yytext,line); return ERROR_TOK; }

%%

