%{
#include <iostream>
#include <cstdlib>
int yylex(void);
void yyerror(const char *s) {
    std::cerr << "Parse error: " << s << std::endl;
}
%}

%union {
    char* str;
    int   num;
}

%token <str> IDENTIFIER STRING_LITERAL CHAR_LITERAL FLOAT_LITERAL
%token <num> INT_LITERAL

%token IF ELSE FOR WHILE RETURN GOTO BREAK CONTINUE SWITCH CASE DEFAULT
%token STRUCT TYPEDEF STATIC CONST AUTO CLASS PRIVATE PROTECTED PUBLIC LAMBDA
%token T_INT T_CHAR T_FLOAT T_DOUBLE T_VOID T_SIGNED_INT T_UNSIGNED_INT
%token PLUS MINUS MUL DIV MOD
%token EQ NEQ LT LEQ GT GEQ
%token AND OR NOT
%token BIT_AND BIT_OR BIT_XOR BIT_NOT
%token INC DEC ASSIGN
%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET
%token COLON SEMICOLON COMMA
%token ERROR

%start program

%%
program:
    ;
%%

int main() {
    return yyparse();
}
