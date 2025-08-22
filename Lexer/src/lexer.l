%{
#include <iostream>
#include <cstdlib>
#include <string>
#include <vector>
#include "../../Parser/src/y.tab.h"

int line = 1;
extern YYSTYPE yylval;

std::vector<std::pair<std::string, std::string>> tokenTable;
%}

%option noyywrap
%x COMMENT
%x STRING
%x CHAR

%%

\n              { line++; }

"if"            { tokenTable.push_back({yytext,"KEYWORD"}); return IF; }
"else"          { tokenTable.push_back({yytext,"KEYWORD"}); return ELSE; }
"for"           { tokenTable.push_back({yytext,"KEYWORD"}); return FOR; }
"while"         { tokenTable.push_back({yytext,"KEYWORD"}); return WHILE; }
"do"            { tokenTable.push_back({yytext,"KEYWORD"}); return DO; }
"return"        { tokenTable.push_back({yytext,"KEYWORD"}); return RETURN; }
"goto"          { tokenTable.push_back({yytext,"KEYWORD"}); return GOTO; }
"break"         { tokenTable.push_back({yytext,"KEYWORD"}); return BREAK; }
"continue"      { tokenTable.push_back({yytext,"KEYWORD"}); return CONTINUE; }
"switch"        { tokenTable.push_back({yytext,"KEYWORD"}); return SWITCH; }
"case"          { tokenTable.push_back({yytext,"KEYWORD"}); return CASE; }
"default"       { tokenTable.push_back({yytext,"KEYWORD"}); return DEFAULT; }
"struct"        { tokenTable.push_back({yytext,"KEYWORD"}); return STRUCT; }
"typedef"       { tokenTable.push_back({yytext,"KEYWORD"}); return TYPEDEF; }
"static"        { tokenTable.push_back({yytext,"KEYWORD"}); return STATIC; }
"const"         { tokenTable.push_back({yytext,"KEYWORD"}); return CONST; }
"auto"          { tokenTable.push_back({yytext,"KEYWORD"}); return AUTO; }
"class"         { tokenTable.push_back({yytext,"KEYWORD"}); return CLASS; }
"private"       { tokenTable.push_back({yytext,"KEYWORD"}); return PRIVATE; }
"protected"     { tokenTable.push_back({yytext,"KEYWORD"}); return PROTECTED; }
"public"        { tokenTable.push_back({yytext,"KEYWORD"}); return PUBLIC; }
"lambda"        { tokenTable.push_back({yytext,"KEYWORD"}); return LAMBDA; }

"int"           { tokenTable.push_back({yytext,"TYPE"}); return T_INT; }
"char"          { tokenTable.push_back({yytext,"TYPE"}); return T_CHAR; }
"float"         { tokenTable.push_back({yytext,"TYPE"}); return T_FLOAT; }
"double"        { tokenTable.push_back({yytext,"TYPE"}); return T_DOUBLE; }
"void"          { tokenTable.push_back({yytext,"TYPE"}); return T_VOID; }
"unsigned int"  { tokenTable.push_back({yytext,"TYPE"}); return T_UNSIGNED_INT; }

[a-zA-Z_][a-zA-Z0-9_]*  { yylval.str = strdup(yytext); tokenTable.push_back({yytext,"IDENTIFIER"}); return IDENTIFIER; }

[0-9]+                   { yylval.ival = atoi(yytext); tokenTable.push_back({yytext,"INT_LITERAL"}); return INT_LITERAL; }

([0-9]+)?\.[0-9]+([eE][-+]?[0-9]+)? {
    yylval.fval = atof(yytext);
    tokenTable.push_back({yytext,"FLOAT_LITERAL"});
    return FLOAT_LITERAL;
}

[0-9]+\.[0-9]*([eE][-+]?[0-9]+)? {
    yylval.fval = atof(yytext);
    tokenTable.push_back({yytext,"FLOAT_LITERAL"});
    return FLOAT_LITERAL;
}

[0-9]+[eE][-+]?[0-9]+ {
    yylval.fval = atof(yytext);
    tokenTable.push_back({yytext,"FLOAT_LITERAL"});
    return FLOAT_LITERAL;
}

"=="    { tokenTable.push_back({yytext,"OPERATOR"}); return EQ; }
"!="    { tokenTable.push_back({yytext,"OPERATOR"}); return NEQ; }
"<="    { tokenTable.push_back({yytext,"OPERATOR"}); return LEQ; }
">="    { tokenTable.push_back({yytext,"OPERATOR"}); return GEQ; }
"<"     { tokenTable.push_back({yytext,"OPERATOR"}); return LT; }
">"     { tokenTable.push_back({yytext,"OPERATOR"}); return GT; }
"&&"    { tokenTable.push_back({yytext,"OPERATOR"}); return AND; }
"||"    { tokenTable.push_back({yytext,"OPERATOR"}); return OR; }
"!"     { tokenTable.push_back({yytext,"OPERATOR"}); return NOT; }
"&"     { tokenTable.push_back({yytext,"OPERATOR"}); return BIT_AND; }
"|"     { tokenTable.push_back({yytext,"OPERATOR"}); return BIT_OR; }
"^"     { tokenTable.push_back({yytext,"OPERATOR"}); return BIT_XOR; }
"~"     { tokenTable.push_back({yytext,"OPERATOR"}); return BIT_NOT; }
"++"    { tokenTable.push_back({yytext,"OPERATOR"}); return INC; }
"--"    { tokenTable.push_back({yytext,"OPERATOR"}); return DEC; }
"="     { tokenTable.push_back({yytext,"OPERATOR"}); return ASSIGN; }

"+"     { tokenTable.push_back({yytext,"OPERATOR"}); return PLUS; }
"-"     { tokenTable.push_back({yytext,"OPERATOR"}); return MINUS; }
"*"     { tokenTable.push_back({yytext,"OPERATOR"}); return MUL; }
"/"     { tokenTable.push_back({yytext,"OPERATOR"}); return DIV; }
"%"     { tokenTable.push_back({yytext,"OPERATOR"}); return MOD; }

"("     { tokenTable.push_back({yytext,"SYMBOL"}); return LPAREN; }
")"     { tokenTable.push_back({yytext,"SYMBOL"}); return RPAREN; }
"{"     { tokenTable.push_back({yytext,"SYMBOL"}); return LBRACE; }
"}"     { tokenTable.push_back({yytext,"SYMBOL"}); return RBRACE; }
"["     { tokenTable.push_back({yytext,"SYMBOL"}); return LBRACKET; }
"]"     { tokenTable.push_back({yytext,"SYMBOL"}); return RBRACKET; }
":"     { tokenTable.push_back({yytext,"SYMBOL"}); return COLON; }
";"     { tokenTable.push_back({yytext,"SYMBOL"}); return SEMICOLON; }
","     { tokenTable.push_back({yytext,"SYMBOL"}); return COMMA; }

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
<STRING>\"             { yylval.str = strdup(yytext); tokenTable.push_back({yytext,"STRING_LITERAL"}); BEGIN(INITIAL); return STRING_LITERAL; }
<STRING>\n             { std::cerr << "Unterminated string literal at line " << line << std::endl; BEGIN(INITIAL); return ERROR; }
<STRING><<EOF>>        { std::cerr << "Unterminated string literal at line " << line << std::endl; BEGIN(INITIAL); return ERROR; }

\'                     { BEGIN(CHAR); yymore(); }
<CHAR>[^\\\n']         { yymore(); }
<CHAR>\\.              { yymore(); }
<CHAR>\'               { yylval.str = strdup(yytext); tokenTable.push_back({yytext,"CHAR_LITERAL"}); BEGIN(INITIAL); return CHAR_LITERAL; }
<CHAR>\n               { std::cerr << "Unterminated char literal at line " << line << std::endl; BEGIN(INITIAL); return ERROR; }
<CHAR><<EOF>>          { std::cerr << "Unterminated char literal at line " << line << std::endl; BEGIN(INITIAL); return ERROR; }

\'\'                       { std::cerr << "Empty char literal at line " << line << std::endl; return ERROR; }
\'[^\\\n']{2,}\'           { std::cerr << "Overlong char literal at line " << line << std::endl; return ERROR; }
\'\\[^nt0\\\'"]\'          { std::cerr << "Invalid escape sequence at line " << line << std::endl; return ERROR; }

[ \t\r]+ { }

[0-9]+[a-zA-Z_]+         { std::cerr << "Invalid identifier at line " << line << std::endl; return ERROR; }
.                         { std::cerr << "Invalid char '" << yytext << "' at line " << line << std::endl; return ERROR; }

%%
