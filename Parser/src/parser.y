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

using namespace std;
struct Symbol{
    string name;
    string type;
    string kind;
    string scope;
    string storageClass;
    string typeQualifier;
    string parentType;
};

vector<Symbol> symbolTable;
stack<string> scopeStack;
string currentType="";
string currentStorageClass="";
string currentTypeQualifier="";
string currentParentType="";
int error_count=0;
map<string, bool> is_type_name;
int pointer_level=0;
void pushScope(const string& scopeName){
    scopeStack.push(scopeName);
}

void popScope(){
    if(!scopeStack.empty()){
        scopeStack.pop();
    }
}

string currentScope(){
    if(scopeStack.empty()){
        return "global";
    }
    string full_scope="";
    stack<string> temp_stack=scopeStack;
    vector<string> scope_parts;
    while(!temp_stack.empty()){
        scope_parts.push_back(temp_stack.top());
        temp_stack.pop();
    }
    reverse(scope_parts.begin(),scope_parts.end());
    for(size_t i=0;i<scope_parts.size();++i){
        full_scope+=scope_parts[i];
        if(i<scope_parts.size()-1){
            full_scope+="::";
        }
    }
    return full_scope;
}

void addSymbol(const string& name,const string& type,const string& kind){
    Symbol s;
    s.name=name;
    s.type=type;
    s.kind=kind;
    s.scope=currentScope();
    s.storageClass=currentStorageClass;
    s.typeQualifier=currentTypeQualifier;
    s.parentType=currentParentType;
    symbolTable.push_back(s);

    if(kind=="type"||kind=="struct"||kind=="union"||kind=="class"||kind=="enum"||kind=="type alias"){
        is_type_name[name]=true;
    }
}

void printSymbolTable(){
    cout<<"Symbol Table:"<<endl;
    for(const auto& s:symbolTable){
        cout<<" Name: "<<s.name<<" - Type: ";
        if(s.kind=="function"){
            cout<<s.type;
        }else{
            if(!s.typeQualifier.empty()){
                cout<<s.typeQualifier<<" ";
            }
            if(!s.storageClass.empty()){
                cout<<s.storageClass<<" ";
            }
            cout<<s.type;
        }
        cout<<" - Scope: "<<s.scope;
        cout<<endl;
    }
}

extern int yylex();
extern int yylineno;
extern char* yytext;
extern FILE* yyin;

extern vector<pair<string,string>> tokenTable;
void yyerror(const char *s){
    cerr<<"Syntax Error at line "<<yylineno<<": "<<s<<" near '"<<yytext<<"'"<<endl;
    error_count++;
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
%token T_INT T_CHAR T_FLOAT T_DOUBLE T_VOID T_UNSIGNED_INT
%token <ival> INT_LITERAL
%token <dval> FLOAT_LITERAL
%token <str> IDENTIFIER
%token <str> STRING_LITERAL
%token <str> CHAR_LITERAL
%token ASSIGN PLUS MINUS ASTERISK DIV MOD
%token EQ NEQ LT GT LEQ GEQ
%token AND OR NOT
%token AMPERSAND BIT_OR BIT_XOR BIT_NOT SHL SHR
%token INC DEC
%token QUESTION COLON
%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET SEMICOLON COMMA DOT ARROW
%token HASH
%token COLON_COLON
%token DOT_DOT_DOT
%token ERROR

%type <dval> expression conditional_expression logical_or_expression logical_and_expression
%type <dval> equality_expression relational_expression additive_expression multiplicative_expression
%type <dval> unary_expression assignment_expression cast_expression
%type <str> init_declarator declarator
%type <str> direct_declarator
%type <str> type_specifier declaration_specifiers identifier type_name
%type <dval> postfix_expression primary_expression
%type <str> labeled_statement goto_statement
%type <array_dim> array_declarator_list fixed_dimension_list

%right ASSIGN
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
    | expression_statement
    | class_definition
    | struct_declaration
    | union_declaration
    | enum_declaration
    | typedef_declaration
    ;

function_definition
    : declaration_specifiers direct_declarator{
        string funcName=string($2);
        string funcType=string($1)+" function";
        addSymbol(funcName,funcType,"function");
        pushScope(funcName);
        currentStorageClass="";
        currentTypeQualifier="";
        free($1);
        free($2);
    }compound_statement{
        popScope();
    }
    | declaration_specifiers direct_declarator SEMICOLON{
        string funcName=string($2);
        string funcType=string($1)+" function";
        addSymbol(funcName,funcType,"function");
        currentStorageClass="";
        currentTypeQualifier="";
        free($1);
        free($2);
    }
    ;

declaration
    : declaration_specifiers init_declarator_list SEMICOLON{
        currentStorageClass="";
        currentTypeQualifier="";
        free($1);
    }
    | declaration_specifiers SEMICOLON{
        currentStorageClass="";
        currentTypeQualifier="";
        free($1);
    }
    ;

declaration_specifiers
    : type_specifier{
        currentType=string($1);
        $$=$1;
    }
    | storage_class_specifier declaration_specifiers{
        $$=$2;
    }
    | type_qualifier declaration_specifiers{
        $$=$2;
    }
    ;

storage_class_specifier
    : STATIC{
        currentStorageClass="static";
    }
    | EXTERN{
        currentStorageClass="extern";
    }
    ;

type_qualifier
    : CONST{
        currentTypeQualifier="const";
    }
    ;

init_declarator_list
    : init_declarator{
        string finalType=currentType;
        if(pointer_level>0){
            finalType+=" pointer";
        }
        addSymbol(string($1),finalType,"variable");
        free($1);
        pointer_level=0;
    }
    | init_declarator_list COMMA init_declarator{
        string finalType=currentType;
        if(pointer_level>0){
            finalType+=" pointer";
        }
        addSymbol(string($3),finalType,"variable");
        free($3);
        pointer_level=0;
    }
    ;
init_declarator
    : declarator{
        $$=$1;
    }
    | declarator ASSIGN assignment_expression{
        $$=$1;
    }
    | declarator ASSIGN initializer{
        $$=$1;
    }
    ;

initializer
    : LBRACE initializer_list_opt RBRACE
    ;
initializer_list_opt
    : initializer_list
    | /* empty */
    ;
initializer_list
    : assignment_expression
    | initializer_list COMMA assignment_expression
    ;
declarator
    : direct_declarator{
        $$=$1;
    }
    | ASTERISK declarator{
        pointer_level++;
        $$=$2;
    }
    ;

direct_declarator
    : identifier array_declarator_list{
        if($2>0){
            currentType+=" "+to_string($2)+"D array";
        }
        $$=$1;
    }
    | identifier{
        $$=$1;
    }
    | LPAREN declarator RPAREN array_declarator_list{
        if($4>0){
            currentType+=" "+to_string($4)+"D array";
        }
        $$=$2;
    }
    | LPAREN declarator RPAREN{
        $$=$2;
    }
    | direct_declarator LPAREN parameter_list_opt RPAREN{
        $$=$1;
    }
    ;
array_declarator_list
    : LBRACKET expression RBRACKET fixed_dimension_list{
        $$=1+$4;
    }
    | LBRACKET RBRACKET fixed_dimension_list{
        $$=1+$3;
    }
    | LBRACKET expression RBRACKET{
        $$=1;
    }
    | LBRACKET RBRACKET{
        $$=1;
    }
    ;

fixed_dimension_list
    : LBRACKET expression RBRACKET{
        $$=1;
    }
    | fixed_dimension_list LBRACKET expression RBRACKET{
        $$=$1+1;
    }
    ;

class_definition
    : CLASS IDENTIFIER{
        addSymbol(string($2),"class","class");
        currentParentType=string($2);
        pushScope(string($2));
        free($2);
    }class_base_opt LBRACE member_list RBRACE SEMICOLON{
        currentParentType="";
        popScope();
    }
    | CLASS IDENTIFIER SEMICOLON{
        addSymbol(string($2),"class","class");
        free($2);
    }
    ;

class_base_opt
    : COLON class_base
    | /* empty */
    ;

class_base
    : IDENTIFIER{
        free($1);
    }
    | class_base COMMA IDENTIFIER{
        free($3);
    }
    ;

struct_declaration
    : STRUCT IDENTIFIER{
        addSymbol(string($2),"struct","struct");
        currentParentType=string($2);
        pushScope(string($2));
        free($2);
    }LBRACE member_list RBRACE SEMICOLON{
        currentParentType="";
        popScope();
    }
    | STRUCT IDENTIFIER SEMICOLON{
        addSymbol(string($2),"struct","struct");
        free($2);
    }
    ;

union_declaration
    : UNION IDENTIFIER{
        addSymbol(string($2),"union","union");
        currentParentType=string($2);
        pushScope(string($2));
        free($2);
    }LBRACE member_list RBRACE SEMICOLON{
        currentParentType="";
        popScope();
    }
    ;

enum_declaration
    : ENUM IDENTIFIER{
        addSymbol(string($2),"enum","enum type");
        currentParentType=string($2);
        free($2);
    }LBRACE enum_list RBRACE SEMICOLON{
        currentParentType="";
    }
    | ENUM IDENTIFIER SEMICOLON{
        addSymbol(string($2),"enum","enum type");
        free($2);
    }
    ;

enum_list
    : enum_list COMMA IDENTIFIER{
        addSymbol(string($3),"enum","enum constant");
        free($3);
    }
    | IDENTIFIER{
        addSymbol(string($1),"enum","enum constant");
        free($1);
    }
    ;

typedef_declaration
    : TYPEDEF type_specifier declarator SEMICOLON{
        addSymbol(string($3),string($2),"type alias");
        free($2);
        free($3);
    }
    ;

member_list
    : member_list member
    | member
    | /* empty */
    ;
member
    : access_specifier COLON
    | declaration
    ;
access_specifier
    : PUBLIC
    | PRIVATE
    | PROTECTED
    ;
identifier
    : IDENTIFIER{
        $$=$1;
    }
    ;

type_name
    : IDENTIFIER %prec TYPENAME{
        string ident=string($1);
        if(is_type_name.count(ident)==0){
            yyerror("Undeclared type name");
        }
        $$=$1;
    }
    ;
type_specifier
    : T_INT{
        $$=strdup("int");
    }
    | T_FLOAT{
        $$=strdup("float");
    }
    | T_CHAR{
        $$=strdup("char");
    }
    | T_DOUBLE{
        $$=strdup("double");
    }
    | T_VOID{
        $$=strdup("void");
    }
    | T_UNSIGNED_INT{
        $$=strdup("unsigned int");
    }
    | AUTO{
        $$=strdup("auto");
    }
    | STRUCT IDENTIFIER{
        string typeName="struct "+string($2);
        is_type_name[typeName]=true;
        $$=strdup(typeName.c_str());
        free($2);
    }
    | UNION IDENTIFIER{
        string typeName="union "+string($2);
        is_type_name[typeName]=true;
        $$=strdup(typeName.c_str());
        free($2);
    }
    | ENUM IDENTIFIER{
        string typeName="enum "+string($2);
        is_type_name[typeName]=true;
        $$=strdup(typeName.c_str());
        free($2);
    }
    | type_name{
        $$=$1;
    }
    ;
parameter_list_opt
    : parameter_list
    | DOT_DOT_DOT
    | /* empty */
    ;

parameter_list
    : parameter
    | parameter COMMA parameter_list
    | parameter COMMA DOT_DOT_DOT
    ;
parameter
    : declaration_specifiers declarator{
        string paramType=string($1);
        if(pointer_level>0){
            paramType+=" pointer";
        }
        addSymbol(string($2),paramType,"variable");
        free($1);
        free($2);
        pointer_level=0;
    }
    | declaration_specifiers{
        free($1);
    }
    ;

compound_statement
    : LBRACE{
        pushScope("block");
    }statement_list_opt RBRACE{
        popScope();
    }
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
    : declaration
    | expression_statement
    | compound_statement
    | selection_statement
    | iteration_statement
    | jump_statement
    | goto_statement
    | labeled_statement
    ;

expression_statement
    : expression_opt SEMICOLON
    ;
expression_opt
    : expression
    | /* empty */
    ;
labeled_statement
    : identifier COLON statement{
        free($1);
    }
    ;

selection_statement
    : IF LPAREN expression RPAREN statement %prec IF_WITHOUT_ELSE
    | IF LPAREN expression RPAREN statement ELSE statement
    | SWITCH LPAREN expression RPAREN statement
    ;
iteration_statement
    : WHILE LPAREN expression RPAREN statement
    | FOR LPAREN{
        pushScope("for_init");
    }for_init_statement expression_opt SEMICOLON expression_opt RPAREN{
        popScope();
    }statement
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
    : GOTO identifier SEMICOLON{
        free($2);
    }
    ;

expression
    : assignment_expression{
        $$=$1;
    }
    | expression COMMA assignment_expression{
        $$=$3;
    }
    ;

assignment_expression
    : conditional_expression{
        $$=$1;
    }
    | unary_expression ASSIGN assignment_expression{
        $$=$3;
    }
    ;

conditional_expression
    : logical_or_expression{
        $$=$1;
    }
    | logical_or_expression QUESTION expression COLON conditional_expression{
        $$=$1?$3:$5;
    }
    ;

logical_or_expression
    : logical_and_expression{
        $$=$1;
    }
    | logical_or_expression OR logical_and_expression{
        $$=$1&&$3;
    }
    ;

logical_and_expression
    : equality_expression{
        $$=$1;
    }
    | logical_and_expression AND equality_expression{
        $$=$1&&$3;
    }
    ;

equality_expression
    : relational_expression{
        $$=$1;
    }
    | equality_expression EQ relational_expression{
        $$=$1==$3;
    }
    | equality_expression NEQ relational_expression{
        $$=$1!=$3;
    }
    ;

relational_expression
    : additive_expression{
        $$=$1;
    }
    | relational_expression LT additive_expression{
        $$=$1<$3;
    }
    | relational_expression GT additive_expression{
        $$=$1>$3;
    }
    | relational_expression LEQ additive_expression{
        $$=$1<=$3;
    }
    | relational_expression GEQ additive_expression{
        $$=$1>=$3;
    }
    ;

additive_expression
    : multiplicative_expression{
        $$=$1;
    }
    | additive_expression PLUS multiplicative_expression{
        $$=$1+$3;
    }
    | additive_expression MINUS multiplicative_expression{
        $$=$1-$3;
    }
    ;

multiplicative_expression
    : cast_expression{
        $$=$1;
    }
    | multiplicative_expression ASTERISK cast_expression{
        $$=$1*$3;
    }
    | multiplicative_expression DIV cast_expression{
        if($3==0){
            yyerror("Division by zero");
            $$=0;
        }else{
            $$=$1/$3;
        }
    }
    | multiplicative_expression MOD cast_expression{
        if($3==0){
            yyerror("Modulo by zero");
            $$=0;
        }else{
            $$=fmod($1,$3);
        }
    }
    ;

cast_expression
    : unary_expression{
        $$=$1;
    }
    | LPAREN type_name RPAREN cast_expression %prec TYPENAME{
        free($2);
        $$=$4;
    }
    ;
unary_expression
    : postfix_expression{
        $$=$1;
    }
    | PLUS cast_expression %prec UPLUS{
        $$=$2;
    }
    | MINUS cast_expression %prec UMINUS{
        $$=-$2;
    }
    | INC unary_expression{
        $$=$2+1;
    }
    | DEC unary_expression{
        $$=$2-1;
    }
    | ASTERISK cast_expression %prec UASTERISK{
        $$=0;
    }
    | AMPERSAND cast_expression %prec UAMPERSAND{
        $$=0;
    }
    | NOT cast_expression{
        $$=!$2;
    }
    | BIT_NOT cast_expression{
        $$=~(long)$2;
    }
    | SIZEOF LPAREN type_name RPAREN %prec TYPENAME{
        free($3);
        $$=sizeof(int);
    }
    | SIZEOF LPAREN expression RPAREN{
        $$=sizeof(int);
    }
    | SIZEOF unary_expression %prec SIZEOF{
        $$=sizeof(int);
    }
    ;
postfix_expression
    : primary_expression{
        $$=$1;
    }
    | postfix_expression LBRACKET expression RBRACKET{
        $$=0;
    }
    | postfix_expression DOT identifier{
        free($3);
        $$=0;
    }
    | postfix_expression ARROW identifier{
        free($3);
        $$=0;
    }
    | postfix_expression LPAREN argument_list_opt RPAREN{
        $$=0;
    }
    | postfix_expression INC{
        $$=$1;
    }
    | postfix_expression DEC{
        $$=$1;
    }
    ;
primary_expression
    : identifier{
        string ident=string($1);
        bool found=false;
        for(const auto& s:symbolTable){
            if(s.name==ident){
                found=true;
                if(s.kind=="struct"||s.kind=="union"||s.kind=="class"){
                    yyerror("Cannot use struct/union/class type directly in an expression");
                }
                break;
            }
        }
        if(!found){
            yyerror("Undeclared identifier");
        }
        free($1);
        $$=0;
    }
    | INT_LITERAL{
        $$=static_cast<double>($1);
    }
    | FLOAT_LITERAL{
        $$=$1;
    }
    | CHAR_LITERAL{
        $$=static_cast<double>(*$1);
        free($1);
    }
    | STRING_LITERAL{
        $$=0;
        free($1);
    }
    | LPAREN expression RPAREN{
        $$=$2;
    }
    | LBRACKET RBRACKET LPAREN parameter_list_opt RPAREN compound_statement{
        pushScope("lambda");
        popScope();
        $$=0;
    }
    ;

argument_list_opt
    : argument_list
    | /* empty */
    ;

argument_list
    : assignment_expression
    | argument_list COMMA assignment_expression
    ;

%%

int main(int argc,char** argv){
    if(argc>1){
        yyin=fopen(argv[1],"r");
        if(!yyin){
            cerr<<"Cannot open file: "<<argv[1]<<endl;
            return 1;
        }
    }
    pushScope("global");
    yyparse();
    if(error_count==0){
        cout<<"Parsing completed successfully!"<<endl;
        printSymbolTable();
    }else{
        cout<<"Parsing finished with "<<error_count<<" errors."<<endl;
    }
    if(yyin&&yyin!=stdin){
        fclose(yyin);
    }
}
