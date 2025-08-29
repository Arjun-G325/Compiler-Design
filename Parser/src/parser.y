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
    cerr << "Syntax Error at line " << yylineno << ": " 
         << s << " near '" << yytext << "'" << endl;
    error_count++;
}
%}

%union {
    int ival;
    double fval;
    char* str;
}

%token CONST IF ELSE WHILE FOR RETURN BREAK CONTINUE GOTO SWITCH CASE DEFAULT DO SIZEOF
%token STATIC EXTERN REGISTER AUTO STRUCT UNION ENUM TYPEDEF CLASS PUBLIC PROTECTED PRIVATE LAMBDA
%token T_INT T_CHAR T_FLOAT T_DOUBLE T_VOID T_UNSIGNED_INT
%token <ival> INT_LITERAL
%token <fval> FLOAT_LITERAL
%token <str> IDENTIFIER
%token <str> STRING_LITERAL
%token <str> CHAR_LITERAL
%token ARROW
%token ASSIGN PLUS MINUS MUL DIV MOD
%token EQ NEQ LT GT LEQ GEQ
%token AND OR NOT
%token BIT_AND BIT_OR BIT_XOR BIT_NOT SHL SHR
%token INC DEC
%token QUESTION COLON
%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET SEMICOLON COMMA DOT ARROW
%token ERROR   /* renamed from ERROR to avoid conflict */

%type <ival> expression assignment_expression

%left OR
%left AND
%left BIT_OR
%left BIT_XOR
%left BIT_AND
%left EQ NEQ
%left LT LEQ GT GEQ
%left SHL SHR
%left PLUS MINUS
%left MUL DIV MOD
%right NOT BIT_NOT UMINUS UPLUS
%right INC DEC
%nonassoc LBRACKET
%left DOT ARROW

%start program

%%

program
    : declaration_list
    ;

declaration_list
    : declaration_list declaration
    | declaration
    ;

declaration
    : type_specifier IDENTIFIER SEMICOLON
    | type_specifier IDENTIFIER ASSIGN expression SEMICOLON
    | function_definition
    ;

type_specifier
    : T_INT
    | T_FLOAT
    | T_CHAR
    | T_DOUBLE
    | T_VOID
    | T_UNSIGNED_INT
    ;

function_definition
    : type_specifier IDENTIFIER LPAREN parameter_list_opt RPAREN compound_statement
    ;

parameter_list_opt
    : parameter_list
    | /* empty */
    ;

parameter_list
    : parameter_list COMMA parameter
    | parameter
    ;

parameter
    : type_specifier IDENTIFIER
    ;

compound_statement
    : LBRACE statement_list_opt RBRACE
    ;

statement_list_opt
    : statement_list
    | /* empty */
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
    ;

expression_statement
    : expression_opt SEMICOLON
    ;

expression_opt
    : expression
    | /* empty */
    ;

selection_statement
    : IF LPAREN expression RPAREN statement
    | IF LPAREN expression RPAREN statement ELSE statement
    ;

iteration_statement
    : WHILE LPAREN expression RPAREN statement
    | FOR LPAREN expression_opt SEMICOLON expression_opt SEMICOLON expression_opt RPAREN statement
    | DO statement WHILE LPAREN expression RPAREN SEMICOLON
    ;

jump_statement
    : RETURN expression_opt SEMICOLON
    | BREAK SEMICOLON
    | CONTINUE SEMICOLON
    ;

expression
    : assignment_expression
    ;

assignment_expression
    : logical_or_expression
    | IDENTIFIER ASSIGN assignment_expression
    ;

logical_or_expression
    : logical_or_expression OR logical_and_expression
    | logical_and_expression
    ;

logical_and_expression
    : logical_and_expression AND equality_expression
    | equality_expression
    ;

equality_expression
    : equality_expression EQ relational_expression
    | equality_expression NEQ relational_expression
    | relational_expression
    ;

relational_expression
    : relational_expression LT additive_expression
    | relational_expression GT additive_expression
    | relational_expression LEQ additive_expression
    | relational_expression GEQ additive_expression
    | additive_expression
    ;

additive_expression
    : additive_expression PLUS multiplicative_expression
    | additive_expression MINUS multiplicative_expression
    | multiplicative_expression
    ;

multiplicative_expression
    : multiplicative_expression MUL unary_expression
    | multiplicative_expression DIV unary_expression
    | multiplicative_expression MOD unary_expression
    | unary_expression
    ;

unary_expression
    : primary_expression
    | MINUS unary_expression %prec UMINUS
    | PLUS unary_expression %prec UPLUS
    | NOT unary_expression
    | BIT_NOT unary_expression
    | INC unary_expression
    | DEC unary_expression
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

int main(int argc, char** argv) {
    if (argc > 1) {
        yyin = fopen(argv[1], "r");
        if (!yyin) {
            cerr << "Cannot open file: " << argv[1] << endl;
            return 1;
        }
    }
    yyparse();
    if (error_count == 0)
        cout << "Parsing completed successfully!" << endl;
    else
        cout << "Parsing finished with " << error_count << " errors." << endl;
    return 0;
}
