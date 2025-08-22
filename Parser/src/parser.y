%{
#include <cstdio>
#include <cstdlib>
#include <string>
#include <vector>
#include <iostream>
#include <cstring>
using namespace std;

struct Token {
    string lexeme;
    string type;
};
extern int yylex();
extern int yylineno;
extern char* yytext;
extern FILE* yyin;

extern vector<pair<string, string>> tokenTable;
int error_count = 0;

void yyerror(const char *s) {
    cerr << "Syntax Error at line " << yylineno << ": " << s << " near '" << yytext << "'" << endl;
    error_count++;
}
%}

%union {
    int ival;
    double fval;
    char* str;
}
%define parse.error verbose
%token CONST IF ELSE WHILE FOR RETURN BREAK CONTINUE GOTO SWITCH CASE DEFAULT DO SIZEOF
%token STATIC EXTERN REGISTER AUTO STRUCT UNION ENUM TYPEDEF CLASS PUBLIC PROTECTED PRIVATE LAMBDA
%token T_INT T_CHAR T_FLOAT T_DOUBLE T_VOID T_UNSIGNED_INT
%token <ival> INT_LITERAL
%token <fval> FLOAT_LITERAL
%token <str> IDENTIFIER
%token <str> STRING_LITERAL
%token <str> CHAR_LITERAL
%token ASSIGN PLUS MINUS MUL DIV MOD
%token EQ NEQ LT GT LEQ GEQ
%token AND OR NOT
%token BIT_AND BIT_OR BIT_XOR BIT_NOT SHL SHR
%token INC DEC
%token QUESTION COLON
%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET SEMICOLON COMMA DOT ARROW
%token ERROR
%type <ival> expression

%left OR
%left AND
%left EQ NEQ
%left LT LEQ GT GEQ
%left PLUS MINUS
%left MUL DIV MOD
%right NOT UMINUS

%start program

%%
program:
     external_declaration_list
    ;

external_declaration_list:
      external_declaration
    | external_declaration_list external_declaration
;

external_declaration:
      function_definition
    | declaration
;

function_definition:
      type_specifier IDENTIFIER LPAREN parameter_list RPAREN compound_statement
;

declaration:
      type_specifier init_declarator_list SEMICOLON
    | type_specifier SEMICOLON
;

type_specifier:
      T_INT
    | T_CHAR
    | T_FLOAT
    | T_DOUBLE
    | T_VOID
    | T_UNSIGNED_INT
;

init_declarator_list:
      init_declarator
    | init_declarator_list COMMA init_declarator
;
init_declarator:
      IDENTIFIER
    | IDENTIFIER ASSIGN expression
;
parameter_list:
    | parameter_declaration
    | parameter_list COMMA parameter_declaration
;

parameter_declaration:
      type_specifier IDENTIFIER
;

compound_statement:
    LBRACE statement_list_opt RBRACE
    ;

statement_list_opt:
    /* empty */
    | statement_list
    ;

statement_list:
    statement_list statement
    | statement
    ;

statement:
    expression_statement
    | compound_statement
    | selection_statement
    | iteration_statement
    | jump_statement
    | declaration SEMICOLON
    ;

expression_statement:
    expression_opt SEMICOLON
    ;

expression_opt:
    /* empty */
    | expression
    ;

selection_statement:
    IF LPAREN expression RPAREN statement
    | IF LPAREN expression RPAREN statement ELSE statement
    | SWITCH LPAREN expression RPAREN LBRACE case_list_opt RBRACE
    ;

case_list_opt:
     /* empty */
    | case_list
    ;

case_list:
     case_list case_label statement_list_opt
    | case_label statement_list_opt
    ;

case_label:
    CASE INT_LITERAL COLON
    | DEFAULT COLON
    ;

iteration_statement:
     WHILE LPAREN expression RPAREN statement
    | DO statement WHILE LPAREN expression RPAREN SEMICOLON
    | FOR LPAREN expression_opt SEMICOLON expression_opt SEMICOLON expression_opt RPAREN statement
    ;

jump_statement:
     RETURN expression_opt SEMICOLON
    | BREAK SEMICOLON
    | CONTINUE SEMICOLON
    | GOTO IDENTIFIER SEMICOLON
    ;

expression:
     assignment_expression
    | expression COMMA assignment_expression
    ;

assignment_expression:
    logical_or_expression
    | unary_expression ASSIGN assignment_expression
    ;

logical_or_expression:
    logical_and_expression
    | logical_or_expression OR logical_and_expression
    ;

logical_and_expression:
    equality_expression
    | logical_and_expression AND equality_expression
    ;

equality_expression:
     relational_expression
    | equality_expression EQ relational_expression
    | equality_expression NEQ relational_expression
    ;

relational_expression:
    additive_expression
    | relational_expression LT additive_expression
    | relational_expression LEQ additive_expression
    | relational_expression GT additive_expression
    | relational_expression GEQ additive_expression
    ;

additive_expression:
     multiplicative_expression
    | additive_expression PLUS multiplicative_expression
    | additive_expression MINUS multiplicative_expression
    ;

multiplicative_expression:
     unary_expression
    | multiplicative_expression MUL unary_expression
    | multiplicative_expression DIV unary_expression
    | multiplicative_expression MOD unary_expression
    ;

unary_expression:
     postfix_expression
    | INC unary_expression
    | DEC unary_expression
    | PLUS unary_expression
    | MINUS unary_expression
    | NOT unary_expression
    | BIT_NOT unary_expression
    ;

postfix_expression:
     primary_expression
    | postfix_expression LPAREN argument_list_opt RPAREN
    | postfix_expression LBRACKET expression RBRACKET
    | postfix_expression INC
    | postfix_expression DEC
    ;

class_definition:
      CLASS IDENTIFIER LBRACE class_body RBRACE
    ;

class_body:
      access_specifier COLON member_list
    | member_list
    ;

access_specifier:
      PUBLIC
    | PRIVATE
    | PROTECTED
    ;

member_list:
      member_list member_declaration
    | member_declaration
    ;

member_declaration:
      type_specifier IDENTIFIER SEMICOLON
    | function_definition
    ;

argument_list_opt:
     /* empty */
    | argument_list
    ;

argument_list:
    expression
    | argument_list COMMA expression
    ;

primary_expression:
    IDENTIFIER
    | INT_LITERAL
    | FLOAT_LITERAL
    | CHAR_LITERAL
    | STRING_LITERAL
    | LPAREN expression RPAREN
    ;

%%

int main(int argc, char** argv) {
    if (argc>1) {
        yyin=fopen(argv[1], "r");
        if (!yyin) {
            perror("fopen");
            return 1;
        }
    }
    yyparse();

    if (error_count==0) {
        cout<<"\nToken                Token_Type"<<endl;
        for (const auto& t:tokenTable) {
            cout<<t.first<<"                " <<t.second<<endl;
        }
    }
    return 0;
}
