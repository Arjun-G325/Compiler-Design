%{
#include <iostream>
#include <string>
#include "../../Parser/src/y.tab.h"

using namespace std;

int line = 1;
extern YYSTYPE yylval;
%}

%option noyywrap

%%

\n      { line++; }

"if"            { return IF; }
"else"          { return ELSE; }
"for"           { return FOR; }
"while"         { return WHILE; }
"return"        { return RETURN; }
"goto"          { return GOTO; }
"break"         { return BREAK; }
"continue"      { return CONTINUE; }
"switch"        { return SWITCH; }
"case"          { return CASE; }
"default"       { return DEFAULT; }
"struct"        { return STRUCT; }
"typedef"       { return TYPEDEF; }
"static"        { return STATIC; }
"const"         { return CONST; }
"auto"          { return AUTO; }
"class"         { return CLASS; }
"private"       { return PRIVATE; }
"protected"     { return PROTECTED; }
"public"        { return PUBLIC; }
"lambda"        { return LAMBDA; }

"int"           { return INT; }
"char"          { return CHAR; }
"float"         { return FLOAT; }
"double"        { return DOUBLE; }
"void"          { return VOID; }
"signed int"    { return SIGNED_INT; }
"unsigned int"  { return UNSIGNED_INT; }

[a-zA-Z_][a-zA-Z0-9_]*   { yylval.str = strdup(yytext); return IDENTIFIER; }

([0-9]*\.[0-9]+|[0-9]+\.[0-9]*)([eE][-+]?[0-9]+)?|[0-9]+[eE][-+]?[0-9]+ { yylval.str = strdup(yytext); return FLOAT_LITERAL; }

[0-9]+   { yylval.num = atoi(yytext); return INT_LITERAL; }

"+"     { return PLUS; }
"-"     { return MINUS; }
"*"     { return MUL; }
"/"     { return DIV; }
"%"     { return MOD; }

"=="    { return EQ; }
"!="    { return NEQ; }
"<"     { return LT; }
"<="    { return LEQ; }
">"     { return GT; }
">="    { return GEQ; }

"&&"    { return AND; }
"||"    { return OR; }
"!"     { return NOT; }

"&"     { return BIT_AND; }
"|"     { return BIT_OR; }
"^"     { return BIT_XOR; }
"~"     { return BIT_NOT; }

"++"    { return INC; }
"--"    { return DEC; }

"("     { return LPAREN; }
")"     { return RPAREN; }
"{"     { return LBRACE; }
"}"     { return RBRACE; }
"["     { return LBRACKET; }
"]"     { return RBRACKET; }
":"     { return COLON; }
";"     { return SEMICOLON; }
","     { return COMMA; }
"="     { return ASSIGN; }

\"([^\\\n"]|\\.)*\"   { yylval.str = strdup(yytext); return STRING_LITERAL; }
\'([^\\\n']|\\[nt0\\'\"])?\' { yylval.str = strdup(yytext); return CHAR_LITERAL; }

"/*"([^*]|\*+[^*/])*\*+"/"   {}
"//".*                      {}

[ \t\r]+    {}

"/*"([^*]|\*+[^*/])*\*?    { cerr << "Unterminated comment at line " << line << endl; return ERROR; }
\"([^\\\n"]|\\.)*          { cerr << "Unterminated string literal at line " << line << endl; return ERROR; }
"''"                       { cerr << "Empty char literal at line " << line << endl; return ERROR; }
\'[^\\\n']{2,}\'           { cerr << "Overlong char literal at line " << line << endl; return ERROR; }
\'\\[^nt0\\\'"]\'          { cerr << "Invalid escape sequence at line " << line << endl; return ERROR; }
[0-9]+[a-zA-Z_]+           { cerr << "Invalid identifier at line " << line << endl; return ERROR; }
[0-9]*\.(?:[0-9]*\.)+[0-9]* { cerr << "Malformed float constant at line " << line << endl; return ERROR; }
.                          { cerr << "Invalid char '" << yytext << "' at line " << line << endl; return ERROR; }

%%

