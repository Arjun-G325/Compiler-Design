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

using namespace std;

struct Symbol {
    string name;
    string type;
    string kind;
    string scope;
};

vector<Symbol> symbolTable;
stack<string> scopeStack;
string currentType;
int error_count=0;
void pushScope(const string& scopeName) {
    scopeStack.push(scopeName);
}

void popScope() {
    if(!scopeStack.empty()) {
        scopeStack.pop();
}
}

string currentScope() {
    if(scopeStack.empty()) {
        return "global";
}
    return scopeStack.top();
}

void addSymbol(const string& name,const string& type,const string& kind) {
    Symbol s;
s.name=name;
    s.type=type;
    s.kind=kind;
    s.scope=currentScope();
    symbolTable.push_back(s);
}

void printSymbolTable() {
    cout<<"Symbol Table:"<<endl;
for(const auto& s:symbolTable) {
        cout<<s.scope<<"::"<<s.name<<", Type: ";
if(s.kind=="function") {
            cout<<"function";
} else {
            cout<<s.type;
}
        cout<<endl;
    }
}

extern int yylex();
extern int yylineno;
extern char* yytext;
extern FILE* yyin;

extern vector<pair<string,string>> tokenTable;

void yyerror(const char *s) {
    cerr<<"Syntax Error at line "<<yylineno<<": "
        <<s<<" near '"<<yytext<<"'"<<endl;
error_count++;
}
%}

%union {
    int ival;
    double fval;
    char* str;
}

%token CONST IF ELSE WHILE FOR RETURN BREAK CONTINUE GOTO SWITCH CASE DEFAULT DO SIZEOF
%token STATIC EXTERN AUTO STRUCT UNION ENUM TYPEDEF CLASS PUBLIC PROTECTED PRIVATE LAMBDA
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
%type <str> parameter_list_opt argument_list_opt parameter_list argument_list declarator init_declarator init_declarator_list
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
%nonassoc LPAREN

%start program

%%

program
    : declaration_list
    ;
declaration_list
    : declaration_list declaration_or_definition
    | declaration_or_definition
    ;
declaration_or_definition
    : declaration
    | function_definition
    | class_definition
    | struct_declaration
    | union_declaration
    | enum_declaration
    ;
declaration
    : declaration_specifiers init_declarator_list SEMICOLON
    ;
declaration_specifiers
    : type_specifier
    | storage_class_specifier declaration_specifiers
    | type_qualifier declaration_specifiers
    | TYPEDEF declaration_specifiers
    ;
storage_class_specifier
    : AUTO { currentType="auto "+currentType; }
    | STATIC { currentType="static "+currentType;
}
    | EXTERN { currentType="extern "+currentType; }
    ;
type_qualifier
    : CONST { currentType="const "+currentType; }
    ;
function_definition
    : declaration_specifiers declarator LPAREN parameter_list_opt RPAREN {
        string functionName=std::string($2);
addSymbol(functionName,currentType,"function");
        pushScope(functionName);
    } compound_statement {
        popScope();
    }
    ;
init_declarator_list
    : init_declarator {
        string kind=(currentType.find("typedef")!=string::npos)?"type alias":"variable";
        addSymbol(std::string($1),currentType,kind);
        free($1);
}
    | init_declarator_list COMMA init_declarator {
        string kind=(currentType.find("typedef")!=string::npos)?"type alias":"variable";
        addSymbol(std::string($3),currentType,kind);
free($3);
    }
    ;
init_declarator
    : declarator { $$=$1; }
    | declarator ASSIGN expression { $$=$1; }
    ;
declarator
    : IDENTIFIER { $$=$1;
}
    | ASTERISK declarator { $$=$2; }
    | declarator LBRACKET expression RBRACKET { $$=$1;
}
    | declarator LPAREN parameter_list_opt RPAREN { $$=$1; }
    ;
class_definition
    : CLASS IDENTIFIER { pushScope(std::string($2)); } LBRACE member_list RBRACE SEMICOLON { addSymbol(std::string($2),"class","class"); free($2); popScope();
}
    | CLASS IDENTIFIER COLON IDENTIFIER { pushScope(std::string($2)); } LBRACE member_list RBRACE SEMICOLON { addSymbol(std::string($2),"class","class"); free($2); popScope();
}
    ;
struct_declaration
    : STRUCT IDENTIFIER { pushScope(std::string($2));
} LBRACE member_list RBRACE SEMICOLON { addSymbol(std::string($2),"struct","struct"); free($2); popScope(); }
    ;
union_declaration
    : UNION IDENTIFIER { pushScope(std::string($2)); } LBRACE member_list RBRACE SEMICOLON { addSymbol(std::string($2),"union","union"); free($2); popScope();
}
    ;
enum_declaration
    : ENUM IDENTIFIER LBRACE enum_list RBRACE SEMICOLON { addSymbol(std::string($2),"enum","enum"); free($2);
}
    ;
enum_list
    : enum_list COMMA IDENTIFIER { addSymbol(std::string($3),"enum","enum constant"); free($3);
}
    | IDENTIFIER { addSymbol(std::string($1),"enum","enum constant"); free($1); }
    ;
member_list
    : member_list member
    | member
    ;
member
    : PUBLIC COLON
    | PRIVATE COLON
    | PROTECTED COLON
    | declaration_or_definition
    ;
type_specifier
    : T_INT { currentType="int";
}
    | T_FLOAT { currentType="float"; }
    | T_CHAR { currentType="char";
}
    | T_DOUBLE { currentType="double"; }
    | T_VOID { currentType="void";
}
    | T_UNSIGNED_INT { currentType="unsigned int"; }
    | IDENTIFIER { currentType=std::string($1); free($1);
}
    | IDENTIFIER ASTERISK { currentType=std::string($1)+" *"; free($1); }
    ;
parameter_list_opt
    : parameter_list { $$=$1; }
    | /* empty */ { $$=NULL;
}
    ;
parameter_list
    : parameter { $$=NULL; }
    | parameter_list COMMA parameter { $$=NULL; }
    | parameter_list COMMA DOT DOT DOT { $$=NULL;
} /* Variable arguments */
    ;
parameter
    : type_specifier declarator { addSymbol(std::string($2),currentType,"variable"); free($2);
}
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
    : declaration { $$=0; }
    | expression_statement { $$=0;
}
    | compound_statement { $$=0; }
    | selection_statement { $$=0;
}
    | iteration_statement { $$=0; }
    | jump_statement { $$=0;
}
    | goto_statement { $$=0; }
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
    : postfix_expression ASSIGN expression { $$=$3; }
    | expression OR expression { $$=$1||$3;
}
    | expression AND expression { $$=$1&&$3; }
    | expression EQ expression { $$=$1==$3;
}
    | expression NEQ expression { $$=$1!=$3; }
    | expression LT expression { $$=$1<$3;
}
    | expression GT expression { $$=$1>$3; }
    | expression LEQ expression { $$=$1<=$3;
}
    | expression GEQ expression { $$=$1>=$3; }
    | expression PLUS expression { $$=$1+$3;
}
    | expression MINUS expression { $$=$1-$3; }
    | expression MUL expression { $$=$1*$3;
}
    | expression DIV expression { $$=$1/$3; }
    | expression MOD expression { $$=fmod($1,$3);
}
    | NOT expression { $$=!$2; }
    | BIT_NOT expression { $$=~$2;
}
    | MINUS expression %prec UMINUS { $$=-$2; }
    | PLUS expression %prec UPLUS { $$=$2; }
    | INC expression { $$=++$2; }
    | DEC expression { $$=--$2; }
    | ASTERISK expression %prec UMINUS { $$=0; }
    | AMPERSAND expression %prec UMINUS { $$=0; }
    | SIZEOF LPAREN expression RPAREN { $$=sizeof(int);
}
    | postfix_expression
    ;
postfix_expression
    : primary_expression { $$=$1;
}
    | postfix_expression LBRACKET expression RBRACKET { $$=0; }
    | postfix_expression LPAREN argument_list_opt RPAREN { $$=0; }
    | postfix_expression DOT IDENTIFIER { $$=0;
}
    | postfix_expression ARROW IDENTIFIER { $$=0; }
    ;
primary_expression
    : IDENTIFIER { $$=0; }
    | INT_LITERAL { $$=$1;
}
    | FLOAT_LITERAL { $$=static_cast<int>($1); }
    | CHAR_LITERAL { $$=static_cast<int>(*$1);
}
    | STRING_LITERAL { $$=0; }
    | LPAREN expression RPAREN { $$=$2;
}
    | LAMBDA LPAREN parameter_list_opt RPAREN compound_statement {
        pushScope("lambda");
        popScope();
    }
    ;
argument_list_opt
    : argument_list { $$=$1; }
    | /* empty */ { $$=NULL; }
    ;
argument_list
    : expression { $$=NULL;
}
    | argument_list COMMA expression { $$=NULL; }
    ;
%%

int main(int argc,char** argv) {
    if(argc>1) {
        yyin=fopen(argv[1],"r");
if(!yyin) {
            cerr<<"Cannot open file: "<<argv[1]<<endl;
            return 1;
}
    }
    pushScope("global");
    yyparse();
if(error_count==0) {
        cout<<"Parsing completed successfully!"<<endl;
        printSymbolTable();
} else {
        cout<<"Parsing finished with "<<error_count<<" errors."
             <<endl;
}
    return 0;
}
