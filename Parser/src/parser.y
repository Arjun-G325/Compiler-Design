%{
#include <cstdio>
#include <cstdlib>
#include <string>
#include <vector>
#include <iostream>
#include <cstring>
#include <cmath>

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
%token ASSIGN PLUS MINUS MUL DIV MOD
%token EQ NEQ LT GT LEQ GEQ
%token AND OR NOT
%token BIT_AND BIT_OR BIT_XOR BIT_NOT SHL SHR
%token INC DEC
%token QUESTION COLON
%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET SEMICOLON COMMA DOT ARROW
%token HASH AMPERSAND ASTERISK
%token ERROR

%type <ival> expression postfix_expression primary_expression
%type <str> parameter_list_opt argument_list_opt parameter_list argument_list
%type <ival> statement

%right ASSIGN
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
    : type_specifier declaration_specifier SEMICOLON
    | function_definition
    | class_definition
    | struct_declaration
    | union_declaration
    | enum_declaration
    ;
declaration_specifier
    : IDENTIFIER
    | ASTERISK IDENTIFIER
    | declaration_specifier ASTERISK
    | IDENTIFIER LBRACKET expression RBRACKET
    | declaration_specifier LBRACKET expression RBRACKET
    ;

class_definition
    : CLASS IDENTIFIER LBRACE member_list RBRACE SEMICOLON
    | CLASS IDENTIFIER COLON IDENTIFIER LBRACE member_list RBRACE SEMICOLON
    ;
struct_declaration
    : STRUCT IDENTIFIER LBRACE member_list RBRACE SEMICOLON
    ;
union_declaration
    : UNION IDENTIFIER LBRACE member_list RBRACE SEMICOLON
    ;
enum_declaration
    : ENUM IDENTIFIER LBRACE enum_list RBRACE SEMICOLON
    ;
enum_list
    : enum_list COMMA IDENTIFIER
    | IDENTIFIER
    ;
member_list
    : member_list member
    | member
    ;
member
    : PUBLIC COLON
    | PRIVATE COLON
    | PROTECTED COLON
    | declaration
    ;
type_specifier
    : T_INT
    | T_FLOAT
    | T_CHAR
    | T_DOUBLE
    | T_VOID
    | T_UNSIGNED_INT
    | IDENTIFIER 
    | IDENTIFIER ASTERISK 
    ;
function_definition
    : type_specifier IDENTIFIER LPAREN parameter_list_opt RPAREN compound_statement
    | type_specifier ASTERISK IDENTIFIER LPAREN parameter_list_opt RPAREN compound_statement 
    ;
parameter_list_opt
    : parameter_list { $$ = $1; }
    | /* empty */ { $$ = NULL; }
    ;
parameter_list
    : parameter { $$ = NULL; }
    | parameter_list COMMA parameter { $$ = NULL; }
    | parameter_list COMMA DOT DOT DOT { $$ = NULL; } 
    ;
parameter
    : type_specifier IDENTIFIER
    | type_specifier ASTERISK IDENTIFIER
    | type_specifier IDENTIFIER LBRACKET RBRACKET
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
    : expression_statement { $$ = 0; }
    | compound_statement { $$ = 0; }
    | selection_statement { $$ = 0; }
    | iteration_statement { $$ = 0; }
    | jump_statement { $$ = 0; }
    | goto_statement { $$ = 0; }
    ;
expression_statement
    : expression_opt SEMICOLON
    | labeled_statement
    ;
expression_opt
    : expression
    | /* empty */
    ;
labeled_statement
    : IDENTIFIER COLON statement
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

goto_statement
    : GOTO IDENTIFIER SEMICOLON
    ;

expression
    : postfix_expression ASSIGN expression { $$ = $3; }
    | expression OR expression { $$ = $1 || $3; }
    | expression AND expression { $$ = $1 && $3; }
    | expression EQ expression { $$ = $1 == $3; }
    | expression NEQ expression { $$ = $1 != $3; }
    | expression LT expression { $$ = $1 < $3; }
    | expression GT expression { $$ = $1 > $3; }
    | expression LEQ expression { $$ = $1 <= $3; }
    | expression GEQ expression { $$ = $1 >= $3; }
    | expression PLUS expression { $$ = $1 + $3; }
    | expression MINUS expression { $$ = $1 - $3; }
    | expression MUL expression { $$ = $1 * $3; }
    | expression DIV expression { $$ = $1 / $3; }
    | expression MOD expression { $$ = fmod($1, $3); }
    | NOT expression { $$ = !$2; }
    | BIT_NOT expression { $$ = ~$2; }
    | MINUS expression %prec UMINUS { $$ = -$2; }
    | PLUS expression %prec UPLUS { $$ = $2; }
    | INC expression { $$ = ++$2; }
    | DEC expression { $$ = --$2; }
    | ASTERISK expression %prec UMINUS { $$ = 0; }
    | AMPERSAND expression %prec UMINUS { $$ = 0; }
    | SIZEOF LPAREN expression RPAREN { $$ = sizeof(int); }
    | postfix_expression
    ;

postfix_expression
    : primary_expression { $$ = $1; }
    | postfix_expression LBRACKET expression RBRACKET { $$ = 0; }
    | postfix_expression LPAREN argument_list_opt RPAREN { $$ = 0; }
    | postfix_expression DOT IDENTIFIER { $$ = 0; }
    | postfix_expression ARROW IDENTIFIER { $$ = 0; }
    ;
primary_expression
    : IDENTIFIER { $$ = 0; }
    | INT_LITERAL { $$ = $1; }
    | FLOAT_LITERAL { $$ = static_cast<int>($1); }
    | CHAR_LITERAL { $$ = static_cast<int>(*$1); }
    | STRING_LITERAL { $$ = 0; }
    | LPAREN expression RPAREN { $$ = $2; }
    ;
argument_list_opt
    : argument_list { $$ = $1; }
    | { $$ = NULL; }
    ;
argument_list
    : expression { $$ = NULL; }
    | argument_list COMMA expression { $$ = NULL; }
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
        cout << "Parsing finished with " << error_count << " errors."
             << endl;
    return 0;
}
