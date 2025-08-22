%{
#include <iostream>
#include <cstdlib>
#include <string>
#include "../../Parser/src/y.tab.h"

int line = 1;
extern YYSTYPE yylval;
%}

%option noyywrap
%x COMMENT
%x STRING
%x CHAR

%%

\n              { line++; }

"if"            { return IF; }
"else"          { return ELSE; }
"for"           { return FOR; }
"while"         { return WHILE; }
"do"            { retur DO; }
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

"int"           { return T_INT; }
"char"          { return T_CHAR; }
"float"         { return T_FLOAT; }
"double"        { return T_DOUBLE; }
"void"          { return T_VOID; }
"unsigned int"  { return T_UNSIGNED_INT; }

[a-zA-Z_][a-zA-Z0-9_]*  { yylval.str = strdup(yytext); return IDENTIFIER; }

[0-9]+                   { yylval.num = atoi(yytext); return INT_LITERAL; }

([0-9]+)?\\.[0-9]+([eE][-+]?[0-9]+)? { yylval.str = strdup(yytext); return FLOAT_LITERAL; }
[0-9]+\\.[0-9]*([eE][-+]?[0-9]+)?   { yylval.str = strdup(yytext); return FLOAT_LITERAL; }
[0-9]+[eE][-+]?[0-9]+               { yylval.str = strdup(yytext); return FLOAT_LITERAL; }

"=="    { return EQ; }
"!="    { return NEQ; }
"<="    { return LEQ; }
">="    { return GEQ; }
"<"     { return LT; }
">"     { return GT; }
"&&"    { return AND; }
"||"    { return OR; }
"!"     { return NOT; }
"&"     { return BIT_AND; }
"|"     { return BIT_OR; }
"^"     { return BIT_XOR; }
"~"     { return BIT_NOT; }
"++"    { return INC; }
"--"    { return DEC; }
"+"     { return PLUS; }
"-"     { return MINUS; }
"*"     { return MUL; }
"/"     { return DIV; }
"%"     { return MOD; }
"="     { return ASSIGN; }

"("     { return LPAREN; }
")"     { return RPAREN; }
"{"     { return LBRACE; }
"}"     { return RBRACE; }
"["     { return LBRACKET; }
"]"     { return RBRACKET; }
":"     { return COLON; }
";"     { return SEMICOLON; }
","     { return COMMA; }

"//".*                 { }

"/*"                   { BEGIN(COMMENT); }
<COMMENT>[^*\n]+       { }
<COMMENT>"*"+[^*/]*    { }
<COMMENT>"*"+"/"       { BEGIN(INITIAL); }
<COMMENT>\n            { line++; }
<COMMENT><<EOF>>       { std::cerr << "Unterminated comment at line " << line << std::endl; return ERROR; }

\"                     { BEGIN(STRING); yymore(); }
<STRING>[^"\n\\]+      { yymore(); }
<STRING>\\.            { yymore(); }
<STRING>\"             { yylval.str = strdup(yytext); BEGIN(INITIAL); return STRING_LITERAL; }
<STRING>\n             { std::cerr << "Unterminated string literal at line " << line << std::endl; BEGIN(INITIAL); return ERROR; }
<STRING><<EOF>>        { std::cerr << "Unterminated string literal at line " << line << std::endl; BEGIN(INITIAL); return ERROR; }

\'                     { BEGIN(CHAR); yymore(); }
<CHAR>[^\\\n']         { yymore(); }
<CHAR>\\.              { yymore(); }
<CHAR>\'               { yylval.str = strdup(yytext); BEGIN(INITIAL); return CHAR_LITERAL; }
<CHAR>\n               { std::cerr << "Unterminated char literal at line " << line << std::endl; BEGIN(INITIAL); return ERROR; }
<CHAR><<EOF>>          { std::cerr << "Unterminated char literal at line " << line << std::endl; BEGIN(INITIAL); return ERROR; }

\'\'                       { std::cerr << "Empty char literal at line " << line << std::endl; return ERROR; }
\'[^\\\n']{2,}\'           { std::cerr << "Overlong char literal at line " << line << std::endl; return ERROR; }
\'\\[^nt0\\\'"]\'          { std::cerr << "Invalid escape sequence at line " << line << std::endl; return ERROR; }

[ \t\r]+ { }

[0-9]+[a-zA-Z_]+         { std::cerr << "Invalid identifier at line " << line << std::endl; return ERROR; }
.                         { std::cerr << "Invalid char '" << yytext << "' at line " << line << std::endl; return ERROR; }

%%
