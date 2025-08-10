%{
#include <iostream>
#include <vector>
#include <string>
#include <iomanip>
using namespace std;

int line=1;
vector<pair<string,string>> tokens;
vector<string> errors;
%}

%%
\n    {line++;}
"if"|"else"|"for"|"while"|"return"|"goto"|"break"|"continue"|"switch"|"case"|"default"|"struct"|"typedef"|"static"|"const"|"auto"   {tokens.emplace_back("KEYWORD",yytext);}
"int"|"char"|"float"|"double"|"void"|"signed int"|"unsigned int"    {tokens.emplace_back("TYPE",yytext);}
[a-zA-Z_][a-zA-Z0-9_]*     {tokens.emplace_back("IDENTIFIER",yytext);}
([0-9]*\.[0-9]+|[0-9]+\.[0-9]*)([eE][-+]?[0-9]+)?|[0-9]+[eE][-+]?[0-9]+    {tokens.emplace_back("FLOAT_LITERAL",yytext);}
[0-9]+     {tokens.emplace_back("INT_LITERAL",yytext);}

"+"|"-"|"*"|"/"|"%"       {tokens.emplace_back("ARITH_OP",yytext);}
"=="|"!="|"<"|"<="|">"|">="   {tokens.emplace_back("REL_OP",yytext);}
"&&"|"||"|"!"      {tokens.emplace_back("LOGICAL_OP",yytext);}
"&"|"|"|"^"|"~"       {tokens.emplace_back("BITWISE_OP",yytext);}
"++"|"--"     {tokens.emplace_back("UNARY_OP",yytext);}

"("   {tokens.emplace_back("LPAREN",yytext);}
")"   {tokens.emplace_back("RPAREN",yytext);}
"{"   {tokens.emplace_back("LBRACE",yytext);}
"}"   {tokens.emplace_back("RBRACE",yytext);}
"["   {tokens.emplace_back("LBRACKET",yytext);}
"]"   {tokens.emplace_back("RBRACKET",yytext);}
":"   {tokens.emplace_back("COLON",yytext);}
";"   {tokens.emplace_back("SEMICOLON",yytext);}
","   {tokens.emplace_back("COMMA",yytext);}
"="   {tokens.emplace_back("ASSIGN",yytext);}
\"([^\\\n"]|\\.)*\"   {tokens.emplace_back("STRING_LITERAL",yytext);}
\'([^\\\n']|\\[nt0\\'\"])\'   {tokens.emplace_back("CHAR_LITERAL",yytext);}

"/*"([^*]|\*+[^*/])*\*+"/" {}
"//".*    {}
[ \t\r]+    {}

"/*"([^*]|\*+[^*/])*\*?    {errors.push_back("Unterminated comment at line "+to_string(line));}
\"([^\\\n"]|\\.)*     {errors.push_back("Unterminated string literal at line "+to_string(line));}
"''"      {errors.push_back("Empty character literal at line "+to_string(line));}
\'[^\\\n']{2,}\'   {errors.push_back("Overlong character literal at line "+to_string(line));}
\'\\[^nt0\\\'"]\'   {errors.push_back("Invalid escape sequence at line "+to_string(line));}
[0-9]+[a-zA-Z_]+    {errors.push_back("Invalid identifier at line "+to_string(line));}
[0-9]*\.(?:[0-9]*\.)+[0-9]*    {errors.push_back("Wrong float constant at line "+to_string(line));}
.     {errors.push_back(string("Invalid character '")+yytext+"' at line "+to_string(line));}

%%

int yywrap() {
    return 1;
}

int main() {
    yylex();
    if(!errors.empty()){
        cout<<"Lexical Errors:\n";
        for(const auto& e:errors) cout<<e<<'\n';
    } 
    else {
        cout<<left<<setw(16)<<"Lexeme"<<setw(16)<<"Token"<<'\n';
        for(const auto& t:tokens) cout<<left<<setw(16)<<t.second<<setw(16)<<t.first<<'\n';
    }
    return 0;
}

