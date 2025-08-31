%{
#include <cstdio>
#include <cstdlib>
#include <string>
#include <vector>
#include <iostream>
#include <cstring>
#include <cmath>
#include <map>
#include <stack>
#include <vector>
#include <algorithm>
#include <iomanip>

using namespace std;
extern int yylex();
extern int yylineno;
extern char* yytext;
extern FILE* yyin;

extern vector<pair<string,string>> tokenTable;
// Declare a global variable to keep track of syntax errors
int errorCount = 0;
void yyerror(const char* s){
    cerr << "Syntax Error at line " << yylineno << ": " << s << " near '" << yytext << "'" << endl;
errorCount++;
}

// --- Symbol Table Structures and Functions ---

// Structure to hold information about a single symbol
struct SymbolInfo {
    string name;
    string type;
    string scope;
    int line;
};

vector<SymbolInfo> symbolTable;
stack<string> scopeStack;
string currentDeclarationType;
string declaratorKind;
bool inFunctionDefinition = false;

// Helper function to get the current scope from the stack as a formatted string
string getFormattedScope() {
    if (scopeStack.empty()) {
        return "global";
    }

    stack<string> tempStack = scopeStack;
    vector<string> scopeParts;
while (!tempStack.empty()) {
        scopeParts.push_back(tempStack.top());
        tempStack.pop();
    }
    reverse(scopeParts.begin(), scopeParts.end());

    string formattedScope;
for (size_t i = 0; i < scopeParts.size(); ++i) {
        formattedScope += scopeParts[i];
if (i < scopeParts.size() - 1) {
            formattedScope += "::";
}
    }
    return formattedScope;
}

// Helper function to add a new symbol to the table, ignoring the value
void addSymbol(const char* name, const string& type) {
    if (name) {
        symbolTable.push_back({string(name), type, getFormattedScope(), yylineno});
    }
}

// Helper function to print the entire symbol table in a formatted way
void printSymbolTable() {
    cout << "\n\n--- Symbol Table ---\n";
    cout << left << setw(20) << "Name" << setw(20) << "Type" << setw(40) << "Scope" << setw(10) << "Line" << endl;
    cout << string(90, '-') << endl;
    for (const auto& symbol : symbolTable) {
        cout << left << setw(20) << symbol.name
             << setw(20) << symbol.type
             << setw(40) << symbol.scope
             << setw(10) << symbol.line << endl;
    }
    cout << "--------------------\n";
}

%}

%union{
    int ival;
    double dval;
    char* str;
    int array_dim;
}
%token CONST IF ELSE WHILE FOR RETURN BREAK CONTINUE GOTO SWITCH CASE DEFAULT DO SIZEOF
%token STATIC EXTERN AUTO STRUCT UNION ENUM TYPEDEF CLASS PUBLIC PROTECTED PRIVATE
%token T_INT T_CHAR T_FLOAT T_DOUBLE T_VOID T_UNSIGNED_INT T_BOOL
%token VA_LIST VA_ARG VA_START VA_END
%token PLUS_ASSIGN MINUS_ASSIGN ASTERISK_ASSIGN DIV_ASSIGN MOD_ASSIGN
%token AND_ASSIGN OR_ASSIGN XOR_ASSIGN SHL_ASSIGN SHR_ASSIGN
%token <ival> INT_LITERAL
%token <dval> FLOAT_LITERAL
%token <str> IDENTIFIER
%token <str> STRING_LITERAL
%token <str> CHAR_LITERAL
%token ASSIGN PLUS MINUS ASTERISK DIV MOD
%token EQ NEQ LT GT LEQ GEQ
%token AND OR NOT
%token AMPERSAND BIT_OR BIT_XOR BIT_NOT SHL SHR
%token QUESTION COLON
%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET SEMICOLON COMMA DOT ARROW
%token HASH
%token COLON_COLON
%token DOT_DOT_DOT
%token ERROR
%token TILDE

%type <dval> expression conditional_expression logical_or_expression logical_and_expression
%type <dval> equality_expression relational_expression additive_expression multiplicative_expression
%type <dval> unary_expression assignment_expression cast_expression
%type <dval> bitwise_and_expression bitwise_xor_expression bitwise_or_expression
%type <str> init_declarator declarator direct_declarator
%type <str> type_specifier declaration_specifiers identifier
%type <str> compound_type_definition type_name abstract_declarator typedef_specifier
%type <dval> postfix_expression primary_expression
%type <str> labeled_statement goto_statement
%type <array_dim> array_declarator_list
%type <str> scoped_identifier

%type <str> storage_class_specifier type_qualifier type_qualifier_opt

%right ASSIGN PLUS_ASSIGN MINUS_ASSIGN ASTERISK_ASSIGN DIV_ASSIGN MOD_ASSIGN AND_ASSIGN OR_ASSIGN XOR_ASSIGN SHL_ASSIGN SHR_ASSIGN
%right QUESTION COLON
%left OR
%left AND
%left BIT_OR
%left BIT_XOR
%left AMPERSAND
%left EQ NEQ
%left LT LEQ GT GEQ
%left SHL SHR
%left PLUS MINUS
%left ASTERISK DIV MOD
%right NOT BIT_NOT UMINUS UPLUS UASTERISK UAMPERSAND SIZEOF
%left DOT ARROW LBRACKET LPAREN INC DEC

%nonassoc IF_WITHOUT_ELSE
%nonassoc ELSE
%nonassoc TYPENAME

%start program

%%

program
    : external_declaration_list
    ;
external_declaration_list
    : external_declaration
    | external_declaration_list external_declaration
    ;
external_declaration
    : declaration
    | function_definition
    | compound_statement
    |
class_definition
    | struct_declaration
    | union_declaration
    | enum_declaration
    |
typedef_declaration
    ;

function_definition
    : declaration_specifiers declarator
        {
            inFunctionDefinition = true;
            string func_type = currentDeclarationType;
            addSymbol($2, func_type.c_str());
            scopeStack.push(string($2));
            free($1);
            free($2);
        }
      compound_statement
        {
            scopeStack.pop();
            inFunctionDefinition = false;
        }
    |
declaration_specifiers declarator SEMICOLON
        {
            string func_type = currentDeclarationType;
            addSymbol($2, func_type.c_str());
            free($1);
            free($2);
        }
    ;

declaration
    : declaration_specifiers init_declarator_list SEMICOLON
        {
            free($1);
        }
    | declaration_specifiers SEMICOLON
        {
            free($1);
        }
    ;

declaration_specifiers
    : storage_class_specifier declaration_specifiers
        {
            $$ = $2; // Storage class doesn't affect the type name
        }
    | type_qualifier declaration_specifiers
        {
            string finalType = string($2);
            if (strcmp($1, "const") == 0) {
                finalType = "const " + finalType;
            }
            $$ = strdup(finalType.c_str());
            free($2);
        }
    | typedef_specifier
        {
            $$ = $1;
        }
    | type_specifier
        {
            currentDeclarationType = $1;
            $$ = $1;
        }
    ;

typedef_specifier
    : type_specifier
        {
            $$ = $1;
        }
    | compound_type_definition
        {
            $$ = $1;
        }
    ;

storage_class_specifier
    : STATIC { $$ = strdup(""); }
    | EXTERN { $$ = strdup(""); }
    ;

type_qualifier
    : CONST { $$ = strdup("const"); }
    ;

type_qualifier_opt
    : type_qualifier
    | /* empty */ { $$ = strdup(""); }
    ;

init_declarator_list
    : init_declarator
        {
            free($1);
        }
    | init_declarator_list COMMA init_declarator
        {
            free($3);
        }
    ;

init_declarator
    : declarator
        {
            string symbol_type = currentDeclarationType;
            if (declaratorKind == "array") {
                symbol_type += "[]";
            } else if (declaratorKind == "function pointer") {
                symbol_type += "(*)()";
            }
            addSymbol($1, symbol_type.c_str());
            $$ = $1;
        }
    | declarator ASSIGN assignment_expression
        {
            string symbol_type = currentDeclarationType;
            if (declaratorKind == "array") {
                symbol_type += "[]";
            } else if (declaratorKind == "function pointer") {
                symbol_type += "(*)()";
            }
            addSymbol($1, symbol_type.c_str());
            $$ = $1;
        }
    | declarator ASSIGN LBRACE initializer_list RBRACE
        {
            string symbol_type = currentDeclarationType;
            if (declaratorKind == "array") {
                symbol_type += "[]";
            } else if (declaratorKind == "function pointer") {
                symbol_type += "(*)()";
            }
            addSymbol($1, symbol_type.c_str());
            $$ = $1;
        }
    ;

initializer_list
    : initializer
    | initializer_list COMMA initializer
    ;

initializer
    : assignment_expression
    | LBRACE initializer_list RBRACE
    ;

declarator
    : direct_declarator
        {
            $$ = $1;
        }
    | ASTERISK type_qualifier_opt declarator
        {
            free($2);
            $$ = $3;
        }
    | ASTERISK declarator
        {
            $$ = $2;
        }
    | LPAREN declarator RPAREN
        {
            $$ = $2;
        }
    ;

direct_declarator
    : identifier
        {
            $$ = $1;
        }
    | TILDE identifier
        {
            string destructorName = "~" + string($2);
            $$ = strdup(destructorName.c_str());
            free($2);
        }
    | identifier array_declarator_list
        {
            declaratorKind = "array";
            $$ = $1;
        }
    | LPAREN declarator RPAREN
        {
            $$ = $2;
        }
    | LPAREN declarator RPAREN array_declarator_list
        {
            declaratorKind = "array";
            $$ = $2;
        }
    | direct_declarator LPAREN parameter_list RPAREN
        {
            if (!inFunctionDefinition) {
                declaratorKind = "function pointer";
            }
            $$ = $1;
        }
    | direct_declarator LPAREN RPAREN
        {
            if (!inFunctionDefinition) {
                declaratorKind = "function pointer";
            }
            $$ = $1;
        }
    | direct_declarator LPAREN DOT_DOT_DOT RPAREN
        {
            $$ = $1;
        }
    ;

array_declarator_list
    : LBRACKET expression RBRACKET
        {
            $$ = 1;
        }
    | LBRACKET RBRACKET
        {
            $$ = 1;
        }
    | LBRACKET expression RBRACKET array_declarator_list
        {
            $$ = 1 + $4;
        }
    | LBRACKET RBRACKET array_declarator_list
        {
            $$ = 1 + $3;
        }
    ;

class_definition
    : CLASS IDENTIFIER
        {
            addSymbol($2, "class");
            scopeStack.push("class " + string($2));
        }
      LBRACE member_list RBRACE SEMICOLON
        {
            scopeStack.pop();
            free($2);
        }
    | CLASS IDENTIFIER SEMICOLON
        {
            addSymbol($2, "class (forward declaration)");
            free($2);
        }
    | CLASS IDENTIFIER class_base_opt
        {
            addSymbol($2, "class");
            scopeStack.push("class " + string($2));
        }
      LBRACE member_list RBRACE SEMICOLON
        {
            scopeStack.pop();
            free($2);
        }
    ;

class_base_opt
    : COLON class_base
    | /* empty */
    ;

class_base
    : base_class
    | class_base COMMA base_class
    ;

base_class
    : access_specifier_base_opt IDENTIFIER
        {
            free($2);
        }
    ;

access_specifier_base_opt
    : PUBLIC
    | PROTECTED
    | PRIVATE
    | /* empty */
    ;

struct_declaration
    : STRUCT IDENTIFIER
        {
            addSymbol($2, "struct");
            scopeStack.push("struct " + string($2));
        }
      LBRACE member_list RBRACE SEMICOLON
        {
            scopeStack.pop();
            free($2);
        }
    | STRUCT IDENTIFIER SEMICOLON
        {
            addSymbol($2, "struct (forward declaration)");
            free($2);
        }
    ;

union_declaration
    : UNION IDENTIFIER
        {
            addSymbol($2, "union");
            scopeStack.push("union " + string($2));
        }
      LBRACE member_list RBRACE SEMICOLON
        {
            scopeStack.pop();
            free($2);
        }
    | UNION IDENTIFIER SEMICOLON
        {
            addSymbol($2, "union (forward declaration)");
            free($2);
        }
    ;

enum_declaration
    : ENUM IDENTIFIER
        {
            addSymbol($2, "enum");
            scopeStack.push("enum " + string($2));
        }
      LBRACE enum_list RBRACE SEMICOLON
        {
            scopeStack.pop();
            free($2);
        }
    | ENUM IDENTIFIER SEMICOLON
        {
            addSymbol($2, "enum (forward declaration)");
            free($2);
        }
    ;

enum_list
    : IDENTIFIER
        {
            addSymbol($1, "enum constant");
            free($1);
        }
    | enum_list COMMA IDENTIFIER
        {
            addSymbol($3, "enum constant");
            free($3);
        }
    ;

typedef_declaration
    : TYPEDEF declaration_specifiers declarator SEMICOLON
        {
            addSymbol($3, "typedef");
            free($2);
            free($3);
        }
    | TYPEDEF compound_type_definition declarator SEMICOLON
        {
            addSymbol($3, "typedef");
            free($2);
            free($3);
        }
    ;

member_list
    : member
    | member_list member
    ;

member
    : access_specifier COLON
    | declaration
    | function_definition
    | compound_statement
    ;

access_specifier
    : PUBLIC
    | PRIVATE
    | PROTECTED
    ;

identifier
    : scoped_identifier
        { $$ = $1; }
    ;

scoped_identifier
    : IDENTIFIER
        { $$ = $1; }
    | scoped_identifier COLON_COLON IDENTIFIER
        {
            string qualified_name = string($1) + "::" + string($3);
            $$ = strdup(qualified_name.c_str());
            free($1);
            free($3);
        }
    ;

type_specifier
    : T_INT { $$ = strdup("int"); }
    | T_FLOAT { $$ = strdup("float"); }
    | T_CHAR { $$ = strdup("char"); }
    | T_DOUBLE { $$ = strdup("double"); }
    | T_VOID { $$ = strdup("void"); }
    | T_UNSIGNED_INT { $$ = strdup("unsigned int"); }
    | T_BOOL { $$ = strdup("bool"); }
    | AUTO { $$ = strdup("auto"); }
    | VA_LIST { $$ = strdup("va_list"); }
    | STRUCT IDENTIFIER
        {
            string typeName = "struct " + string($2);
            $$ = strdup(typeName.c_str());
            free($2);
        }
    | UNION IDENTIFIER
        {
            string typeName = "union " + string($2);
            $$ = strdup(typeName.c_str());
            free($2);
        }
    | ENUM IDENTIFIER
        {
            string typeName = "enum " + string($2);
            $$ = strdup(typeName.c_str());
            free($2);
        }
    | scoped_identifier
        { $$ = $1; }
    ;

compound_type_definition
    : STRUCT IDENTIFIER
        {
            scopeStack.push("struct " + string($2));
        }
      LBRACE member_list RBRACE
        {
            string ret = "struct " + string($2);
            scopeStack.pop();
            $$ = strdup(ret.c_str());
            free($2);
        }
    | STRUCT LBRACE member_list RBRACE
        {
            $$ = strdup("struct anonymous_struct");
        }
    | UNION IDENTIFIER
        {
            scopeStack.push("union " + string($2));
        }
      LBRACE member_list RBRACE
        {
            string ret = "union " + string($2);
            scopeStack.pop();
            $$ = strdup(ret.c_str());
            free($2);
        }
    | UNION LBRACE member_list RBRACE
        {
            $$ = strdup("union anonymous_union");
        }
    | ENUM IDENTIFIER LBRACE enum_list RBRACE
        {
            string ret = "enum " + string($2);
            $$ = strdup(ret.c_str());
            free($2);
        }
    | ENUM LBRACE enum_list RBRACE
        {
            $$ = strdup("enum anonymous_enum");
        }
    ;

parameter_list
    : parameter_declaration
    | parameter_list COMMA parameter_declaration
    | parameter_list COMMA DOT_DOT_DOT
    | DOT_DOT_DOT
    ;

parameter_declaration
    : declaration_specifiers declarator
        {
            addSymbol($2, currentDeclarationType.c_str());
            free($1);
            free($2);
        }
    | declaration_specifiers
        {
            free($1);
        }
    ;

compound_statement
    : LBRACE { string block_name = "block@" + to_string(yylineno); scopeStack.push(block_name); }
      statement_list RBRACE
        {
            scopeStack.pop();
        }
    | LBRACE { string block_name = "block@" + to_string(yylineno); scopeStack.push(block_name); }
      RBRACE
        {
            scopeStack.pop();
        }
    ;

statement_list
    : statement
    | statement_list statement
    ;

statement
    : declaration
    | expression_statement
    | compound_statement
    | selection_statement
    | iteration_statement
    | jump_statement
    | goto_statement
    | labeled_statement
    | case_statement
    | default_statement
    ;

expression_statement
    : expression_opt SEMICOLON
    ;

expression_opt
    : /* empty */
    | expression
    ;

labeled_statement
    : identifier COLON statement
        {
            addSymbol($1, "label");
            free($1);
        }
    ;

selection_statement
    : IF LPAREN expression RPAREN statement %prec IF_WITHOUT_ELSE
    | IF LPAREN expression RPAREN statement ELSE statement
    | switch_statement
    ;

iteration_statement
    : WHILE LPAREN expression RPAREN statement
    | FOR LPAREN for_init_statement expression_opt SEMICOLON expression_opt RPAREN statement
    | DO statement WHILE LPAREN expression RPAREN SEMICOLON
    ;

for_init_statement
    : declaration
    | expression_opt SEMICOLON
    ;

jump_statement
    : RETURN expression_opt SEMICOLON
    | BREAK SEMICOLON
    | CONTINUE SEMICOLON
    ;

goto_statement
    : GOTO identifier SEMICOLON
        {
            free($2);
        }
    ;

switch_statement
    : SWITCH LPAREN expression RPAREN compound_statement
    ;

case_statement
    : CASE expression COLON statement
    ;

default_statement
    : DEFAULT COLON statement
    ;

expression
    : assignment_expression
        {
            $$ = $1;
        }
    | expression COMMA assignment_expression
        {
            $$ = $3;
        }
    ;

assignment_expression
    : conditional_expression
        {
            $$ = $1;
        }
    | unary_expression ASSIGN assignment_expression
        {
            $$ = $3;
        }
    | unary_expression PLUS_ASSIGN assignment_expression
        {
            $$ = $3;
        }
    | unary_expression MINUS_ASSIGN assignment_expression
        {
            $$ = $3;
        }
    | unary_expression ASTERISK_ASSIGN assignment_expression
        {
            $$ = $3;
        }
    | unary_expression DIV_ASSIGN assignment_expression
        {
            $$ = $3;
        }
    | unary_expression MOD_ASSIGN assignment_expression
        {
            $$ = fmod($1, $3);
        }
    | unary_expression AND_ASSIGN assignment_expression
        {
            $$ = $3;
        }
    | unary_expression OR_ASSIGN assignment_expression
        {
            $$ = $3;
        }
    | unary_expression XOR_ASSIGN assignment_expression
        {
            $$ = $3;
        }
    | unary_expression SHL_ASSIGN assignment_expression
        {
            $$ = $3;
        }
    | unary_expression SHR_ASSIGN assignment_expression
        {
            $$ = $3;
        }
    ;

conditional_expression
    : logical_or_expression
        {
            $$ = $1;
        }
    | logical_or_expression QUESTION expression COLON conditional_expression
        {
            $$ = $1 ? $3 : $5;
        }
    ;

logical_or_expression
    : logical_and_expression
        {
            $$ = $1;
        }
    | logical_or_expression OR logical_and_expression
        {
            $$ = $1 || $3;
        }
    ;

logical_and_expression
    : bitwise_or_expression
        {
            $$ = $1;
        }
    | logical_and_expression AND bitwise_or_expression
        {
            $$ = $1 && $3;
        }
    ;

bitwise_or_expression
    : bitwise_xor_expression
        {
            $$ = $1;
        }
    | bitwise_or_expression BIT_OR bitwise_xor_expression
        {
            $$ = (long)$1 | (long)$3;
        }
    ;

bitwise_xor_expression
    : bitwise_and_expression
        {
            $$ = $1;
        }
    | bitwise_xor_expression BIT_XOR bitwise_and_expression
        {
            $$ = (long)$1 ^ (long)$3;
        }
    ;

bitwise_and_expression
    : equality_expression
        {
            $$ = $1;
        }
    | bitwise_and_expression AMPERSAND equality_expression
        {
            $$ = (long)$1 & (long)$3;
        }
    ;

equality_expression
    : relational_expression
        {
            $$ = $1;
        }
    | equality_expression EQ relational_expression
        {
            $$ = $1 == $3;
        }
    | equality_expression NEQ relational_expression
        {
            $$ = $1 != $3;
        }
    ;

relational_expression
    : additive_expression
        {
            $$ = $1;
        }
    | relational_expression LT additive_expression
        {
            $$ = $1 < $3;
        }
    | relational_expression GT additive_expression
        {
            $$ = $1 > $3;
        }
    | relational_expression LEQ additive_expression
        {
            $$ = $1 <= $3;
        }
    | relational_expression GEQ additive_expression
        {
            $$ = $1 >= $3;
        }
    ;

additive_expression
    : multiplicative_expression
        {
            $$ = $1;
        }
    | additive_expression PLUS multiplicative_expression
        {
            $$ = $1 + $3;
        }
    | additive_expression MINUS multiplicative_expression
        {
            $$ = $1 - $3;
        }
    ;

multiplicative_expression
    : cast_expression
        {
            $$ = $1;
        }
    | multiplicative_expression ASTERISK cast_expression
        {
            $$ = $1 * $3;
        }
    | multiplicative_expression DIV cast_expression
        {
            $$ = $1 / $3;
        }
    | multiplicative_expression MOD cast_expression
        {
            $$ = fmod($1, $3);
        }
    ;

cast_expression
    : unary_expression
        {
            $$ = $1;
        }
    | LPAREN type_name RPAREN cast_expression
        {
            free($2);
            $$ = $4;
        }
    ;

type_name
    : type_specifier abstract_declarator
        {
            string ret = string($1) + string($2);
            $$ = strdup(ret.c_str());
            free($1);
            free($2);
        }
    | type_specifier
        {
            $$ = $1;
        }
    ;

abstract_declarator
    : ASTERISK
        {
            $$ = strdup("*");
        }
    | abstract_declarator ASTERISK
        {
            string ret = string($1) + "*";
            $$ = strdup(ret.c_str());
            free($1);
        }
    | LPAREN abstract_declarator RPAREN
        {
            $$ = $2;
        }
    | abstract_declarator LBRACKET expression_opt RBRACKET
        {
            $$ = $1;
        }
    | abstract_declarator LPAREN parameter_list_opt RPAREN
        {
            $$ = $1;
        }
    ;

parameter_list_opt
    : parameter_list
    | /* empty */
    ;

unary_expression
    : postfix_expression
        {
            $$ = $1;
        }
    | PLUS cast_expression %prec UPLUS
        {
            $$ = $2;
        }
    | MINUS cast_expression %prec UMINUS
        {
            $$ = -$2;
        }
    | INC unary_expression
        {
            $$ = $2 + 1;
        }
    | DEC unary_expression
        {
            $$ = $2 - 1;
        }
    | ASTERISK cast_expression %prec UASTERISK
        {
            $$ = 0;
        }
    | AMPERSAND cast_expression %prec UAMPERSAND
        {
            $$ = 0;
        }
    | NOT cast_expression
        {
            $$ = !$2;
        }
    | BIT_NOT cast_expression
        {
            $$ = ~(long)$2;
        }
    | SIZEOF LPAREN type_name RPAREN
        {
            free($3);
            $$ = sizeof(int);
        }
    | SIZEOF unary_expression %prec SIZEOF
        {
            $$ = sizeof(int);
        }
    | TILDE unary_expression
        {
            $$ = ~(long)$2;
        }
    ;

postfix_expression
    : primary_expression
        {
            $$ = $1;
        }
    | postfix_expression LBRACKET expression RBRACKET
        {
            $$ = 0;
        }
    | postfix_expression DOT identifier
        {
            free($3);
            $$ = 0;
        }
    | postfix_expression ARROW identifier
        {
            free($3);
            $$ = 0;
        }
    | postfix_expression LPAREN argument_list RPAREN
        {
            $$ = 0;
        }
    | postfix_expression LPAREN RPAREN
        {
            $$ = 0;
        }
    | postfix_expression INC
        {
            $$ = $1;
        }
    | postfix_expression DEC
        {
            $$ = $1;
        }
    | VA_ARG LPAREN assignment_expression COMMA type_specifier RPAREN
        {
            $$ = 0;
            free($5);
        }
    | VA_START LPAREN assignment_expression COMMA identifier RPAREN
        {
            $$ = 0;
        }
    | VA_END LPAREN assignment_expression RPAREN
        {
            $$ = 0;
        }
    ;

primary_expression
    : identifier
        {
            free($1);
            $$ = 0;
        }
    | INT_LITERAL
        {
            $$ = static_cast<double>($1);
        }
    | FLOAT_LITERAL
        {
            $$ = $1;
        }
    | CHAR_LITERAL
        {
            $$ = static_cast<double>(*$1);
            free($1);
        }
    | STRING_LITERAL
        {
            $$ = 0;
            free($1);
        }
    | LPAREN expression RPAREN
        {
            $$ = $2;
        }
    | lambda_expression
        {
            $$ = 0;
        }
    ;

lambda_expression
    : LBRACKET RBRACKET LPAREN parameter_list RPAREN compound_statement
    | LBRACKET RBRACKET LPAREN RPAREN compound_statement
    ;

argument_list
    : assignment_expression
    | argument_list COMMA assignment_expression
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

    scopeStack.push("global");
    yyparse();
    
    // Check for errors before printing the symbol table
    if (errorCount == 0) {
        printSymbolTable();
    } else {
        cerr << "Parsing failed with " << errorCount << " error(s). Symbol table not printed." << endl;
    }

    if (yyin && yyin != stdin) {
        fclose(yyin);
    }
    return 0;
}
