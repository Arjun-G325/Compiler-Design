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

%token IF ELSE FOR WHILE DO RETURN GOTO BREAK CONTINUE SWITCH CASE DEFAULT
%token STRUCT TYPEDEF STATIC CONST AUTO CLASS PRIVATE PROTECTED PUBLIC LAMBDA
%token T_INT T_CHAR T_FLOAT T_DOUBLE T_VOID T_UNSIGNED_INT
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
program
    : external_declaration_list
    ;

external_declaration_list
    : external_declaration_list external_declaration
    | external_declaration
    ;

external_declaration
    : function_definition
    | declaration SEMICOLON
    ;

declaration
    : type_specifier declarator_list
    ;

type_specifier
    : T_INT
    | T_CHAR
    | T_FLOAT
    | T_DOUBLE
    | T_VOID
    | T_UNSIGNED_INT
    | STRUCT IDENTIFIER
    | CLASS IDENTIFIER
    ;

declarator_list
    : declarator
    | declarator_list COMMA declarator
    ;

declarator
    : IDENTIFIER
    | IDENTIFIER ASSIGN expression
    | IDENTIFIER LBRACKET INT_LITERAL RBRACKET
    ;

function_definition
    : type_specifier IDENTIFIER LPAREN parameter_list_opt RPAREN compound_statement
    ;

parameter_list_opt
    : /* empty */
    | parameter_list
    ;

parameter_list
    : parameter
    | parameter_list COMMA parameter
    ;

parameter
    : type_specifier IDENTIFIER
    ;

compound_statement
    : LBRACE statement_list_opt RBRACE
    ;

statement_list_opt
    : /* empty */
    | statement_list
    ;

statement_list
    : statement_list statement
    | statement
    ;

statement
    : expression_statement
    | compound_statement
    | selection_statement
    | iteration_statement
    | jump_statement
    | declaration SEMICOLON
    ;

expression_statement
    : expression_opt SEMICOLON
    ;

expression_opt
    : /* empty */
    | expression
    ;

selection_statement
    : IF LPAREN expression RPAREN statement
    | IF LPAREN expression RPAREN statement ELSE statement
    | SWITCH LPAREN expression RPAREN LBRACE case_list_opt RBRACE
    ;

case_list_opt
    : /* empty */
    | case_list
    ;

case_list
    : case_list case_label statement_list_opt
    | case_label statement_list_opt
    ;

case_label
    : CASE INT_LITERAL COLON
    | DEFAULT COLON
    ;

iteration_statement
    : WHILE LPAREN expression RPAREN statement
    | DO statement WHILE LPAREN expression RPAREN SEMICOLON
    | FOR LPAREN expression_opt SEMICOLON expression_opt SEMICOLON expression_opt RPAREN statement
    ;

jump_statement
    : RETURN expression_opt SEMICOLON
    | BREAK SEMICOLON
    | CONTINUE SEMICOLON
    | GOTO IDENTIFIER SEMICOLON
    ;

expression
    : assignment_expression
    | expression COMMA assignment_expression
    ;

assignment_expression
    : logical_or_expression
    | unary_expression ASSIGN assignment_expression
    ;

logical_or_expression
    : logical_and_expression
    | logical_or_expression OR logical_and_expression
    ;

logical_and_expression
    : equality_expression
    | logical_and_expression AND equality_expression
    ;

equality_expression
    : relational_expression
    | equality_expression EQ relational_expression
    | equality_expression NEQ relational_expression
    ;

relational_expression
    : additive_expression
    | relational_expression LT additive_expression
    | relational_expression LEQ additive_expression
    | relational_expression GT additive_expression
    | relational_expression GEQ additive_expression
    ;

additive_expression
    : multiplicative_expression
    | additive_expression PLUS multiplicative_expression
    | additive_expression MINUS multiplicative_expression
    ;

multiplicative_expression
    : unary_expression
    | multiplicative_expression MUL unary_expression
    | multiplicative_expression DIV unary_expression
    | multiplicative_expression MOD unary_expression
    ;

unary_expression
    : postfix_expression
    | INC unary_expression
    | DEC unary_expression
    | PLUS unary_expression
    | MINUS unary_expression
    | NOT unary_expression
    | BIT_NOT unary_expression
    ;

postfix_expression
    : primary_expression
    | postfix_expression LPAREN argument_list_opt RPAREN
    | postfix_expression LBRACKET expression RBRACKET
    | postfix_expression INC
    | postfix_expression DEC
    ;

argument_list_opt
    : /* empty */
    | argument_list
    ;

argument_list
    : expression
    | argument_list COMMA expression
    ;

primary_expression
    : IDENTIFIER
    | INT_LITERAL
    | FLOAT_LITERAL
    | CHAR_LITERAL
    | STRING_LITERAL
    | LPAREN expression RPAREN
    ;

%%

int main() {
    return yyparse();
}
