%code requires {
#include <string>
#include <vector>
#include <map>
#include <stack>
#include <iostream>

    struct Type;
    struct Symbol;
    struct ExprResult;
    struct InitializerItem;

    // Enum for access specifiers
    enum AccessSpecifier { AS_PUBLIC, AS_PRIVATE, AS_PROTECTED };

struct ExprResult {
        double dval;
        Type* type;
        Symbol* lvalue_symbol;
        bool is_const_expr;
ExprResult(double v = 0.0, Type* t = nullptr, Symbol* s = nullptr)
            : dval(v), type(t), lvalue_symbol(s), is_const_expr(false) {}
    };

struct InitializerItem {
        bool is_list;
        ExprResult* expr;
        std::vector<InitializerItem*>* list;

InitializerItem(ExprResult* e = nullptr) : is_list(false), expr(e), list(nullptr) {}
        InitializerItem(std::vector<InitializerItem*>* l) : is_list(true), expr(nullptr), list(l) {}

        ~InitializerItem() {
            if (is_list) {
                if (list) {
                    for (auto item : *list) {
                        delete item;
                    }
                    delete list;
                }
            } else {
                delete expr;
            }
        }
    };

enum TypeKind { TK_BASE, TK_STRUCT, TK_CLASS, TK_UNION, TK_ENUM, TK_FUNCTION };
    enum SymbolKind { SK_VARIABLE, SK_TYPEDEF_NAME, SK_FUNCTION, SK_ENUM_CONSTANT };

struct Type {
        TypeKind kind;
        std::string base_type;
        bool is_const;
        bool is_unsigned;
        int pointer_level;
std::map<std::string, Symbol*> members;
        std::vector<int> array_dimensions;
Type* return_type;
        std::vector<Type*> parameter_types;
Type(std::string base = "", TypeKind k = TK_BASE);
        std::string toString() const;
~Type();
    };

struct Symbol {
        std::string name;
        Type* type;
        SymbolKind kind;
        double dval;
bool has_return;
AccessSpecifier access; // Added for class member access control
    };

    // For two-pass member checking (within a class)
    struct MemberDef {
        std::string name;
Type* type;
        AccessSpecifier access;
        SymbolKind kind;
void* definition; // Can be a Symbol* for variables or a FunctionDef* for functions
    };

}

%{
#include "parser.tab.h"
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>

#include <string>
#include <vector>
#include <map>
#include <stack>

using namespace std;
extern int yylex();
extern int yylineno;
extern char* yytext;
extern FILE* yyin;
void yyerror(const char* s);

Type::Type(string base, TypeKind k) : kind(k), base_type(base), is_const(false), is_unsigned(false), pointer_level(0), return_type(nullptr) {}

Type::~Type() {
    delete return_type;
}

string Type::toString() const {
    string desc = "";
    if (is_const) desc += "const ";
    if (is_unsigned) desc += "unsigned ";
if (kind == TK_FUNCTION && return_type) {
        desc += return_type->toString() + " function(";
        for (size_t i = 0; i < parameter_types.size(); ++i) {
            desc += parameter_types[i]->toString();
            if (i < parameter_types.size() - 1) desc += ", ";
        }
        desc += ")";
    } else {
        desc += base_type;
        for (int i = 0; i < pointer_level; ++i) desc += "*";
        for (int dim : array_dimensions) {
            desc += "[" + (dim >= 0 ? to_string(dim) : "") + "]";
        }
    }
    return desc;
}


stack<map<string, Symbol*>> symbol_table;
map<string, Type*> type_table;
Symbol* g_current_function = nullptr;
vector<Symbol*>* g_current_param_list = nullptr;
bool g_allow_initializers = false;
int g_switch_depth = 0;
int g_loop_depth = 0;
map<string, bool> g_labels;
vector<string> g_unresolved_gotos;
Type* g_current_base_type = nullptr;
AccessSpecifier g_current_access_specifier = AS_PRIVATE; // Default for class
Type* g_current_class_scope = nullptr;
bool g_parsing_lambda = false;
Type* g_inferred_return_type = nullptr;


void enter_scope() { symbol_table.push(map<string, Symbol*>()); }
void exit_scope() { if (!symbol_table.empty()) symbol_table.pop(); }

Type* lookup_type(const string& name) {
    if (type_table.count(name)) return type_table.at(name);
    return nullptr;
}

Symbol* lookup_symbol(const string& name) {
    stack<map<string, Symbol*>> temp_stack = symbol_table;
    while (!temp_stack.empty()) {
        auto& current_scope = temp_stack.top();
        if (current_scope.count(name)) return current_scope.at(name);
        temp_stack.pop();
    }
    return nullptr;
}

void install_type(const string& name, Type* type) {
    if (type_table.count(name)) {
        yyerror(("Redefinition of type '" + name + "'").c_str());
    } else {
        type_table[name] = type;
    }
}

void install_symbol(Symbol* sym) {
    if (symbol_table.empty()) enter_scope();
    auto& current_scope = symbol_table.top();
    if (current_scope.count(sym->name)) {
        yyerror(("Redeclaration of '" + sym->name + "'").c_str());
    } else {
        current_scope[sym->name] = sym;
    }
}

void yyerror(const char*s) {
    cerr << "Parser Error at line " << yylineno << ": " << s << " near '" << yytext << "'" << endl;
}

%}

%token CONST IF ELSE WHILE FOR RETURN BREAK CONTINUE GOTO SWITCH CASE DEFAULT DO SIZEOF
%token STATIC EXTERN AUTO STRUCT UNION ENUM TYPEDEF CLASS PUBLIC PROTECTED PRIVATE
%token BOOL VOID INT CHAR FLOAT DOUBLE UNSIGNED
%token VA_LIST VA_ARG VA_START VA_END
%token <ival> INT_LITERAL
%token <dval> FLOAT_LITERAL
%token <str> IDENTIFIER STRING_LITERAL CHAR_LITERAL
%token ASSIGN ADD_ASSIGN SUB_ASSIGN MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN
%token AND_ASSIGN OR_ASSIGN XOR_ASSIGN SHL_ASSIGN SHR_ASSIGN
%token LSHIFT_OP RSHIFT_OP LE_OP GE_OP EQ_OP NE_OP AND_OP OR_OP
%token INC_OP DEC_OP PTR_OP DOT_DOT_DOT COLON_COLON
%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET SEMICOLON COMMA DOT

%union {
    double dval;
    int ival;
    char* str;
    Type* type_ptr;
    Symbol* symbol_ptr;
    std::vector<Symbol*>* symbol_list_ptr;
    ExprResult* expr_ptr;
    std::vector<ExprResult*>* arg_list_ptr;
    InitializerItem* initializer_item_ptr;
    std::vector<InitializerItem*>* initializer_item_list_ptr;
    std::vector<MemberDef*>* member_def_list_ptr;
    MemberDef* member_def_ptr;
    AccessSpecifier access_specifier_val;
}

%type <expr_ptr> expression expression_opt assignment_expression conditional_expression logical_or_expression
%type <expr_ptr> logical_and_expression bitwise_or_expression bitwise_xor_expression
%type <expr_ptr> bitwise_and_expression equality_expression relational_expression shift_expression
%type <expr_ptr> additive_expression multiplicative_expression cast_expression
%type <expr_ptr> unary_expression postfix_expression primary_expression lambda_expression

%type <type_ptr> type_specifier declaration_specifiers type_name abstract_declarator struct_or_union_specifier enum_specifier type_qualifier
%type <symbol_list_ptr> init_declarator_list parameter_list enumerator_list struct_declaration_list struct_declaration_item parameter_list_opt
%type <symbol_ptr> declarator direct_declarator init_declarator parameter_declaration enumerator
%type <str> identifier
%type <member_def_list_ptr> member_declaration_list member_declaration
%type <access_specifier_val> access_specifier

%type <arg_list_ptr> argument_list

%type <initializer_item_list_ptr> initializer_list initializer_items
%type <initializer_item_ptr> initializer

%right ASSIGN ADD_ASSIGN SUB_ASSIGN MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN AND_ASSIGN OR_ASSIGN XOR_ASSIGN SHL_ASSIGN SHR_ASSIGN
%right '?'
%left OR_OP
%left AND_OP
%left '|'
%left '^'
%left '&'
%left EQ_OP NE_OP
%left '<' '>' LE_OP GE_OP
%left LSHIFT_OP RSHIFT_OP
%left '+' '-'
%left '*' '/' '%'
%right '!' '~' INC_OP DEC_OP SIZEOF
%left DOT PTR_OP LPAREN RPAREN LBRACKET RBRACKET

%nonassoc IF_WITHOUT_ELSE
%nonassoc ELSE

%start program

%%

program
    : { enter_scope(); } external_declaration_list {
        Symbol* main_sym = lookup_symbol("main");
        if (!main_sym || main_sym->kind != SK_FUNCTION) {
            fprintf(stderr, "Error: undefined reference to 'main'\n");
        } else {
            // Check return type
            if (main_sym->type->return_type->base_type != "int") {
                fprintf(stderr, "Error: 'main' must return 'int'\n");
            }

            // Check parameters
            size_t num_params = main_sym->type->parameter_types.size();
            if (num_params != 0 && num_params != 2) {
                fprintf(stderr, "Error: 'main' must have 0 or 2 arguments\n");
            } else if (num_params == 2) {
                Type* argc_type = main_sym->type->parameter_types[0];
                Type* argv_type = main_sym->type->parameter_types[1];

                if (argc_type->base_type != "int" || argc_type->pointer_level != 0) {
                    fprintf(stderr, "Error: first argument to 'main' must be 'int'\n");
                }
                if (argv_type->base_type != "char" || (argv_type->pointer_level != 2 && !(argv_type->pointer_level == 1 && !argv_type->array_dimensions.empty()))) {
                    fprintf(stderr, "Error: second argument to 'main' must be 'char**' or 'char*[]'\n");
                }
            }
        }
        exit_scope();
    }
    ;

external_declaration_list
    : external_declaration
    | external_declaration_list external_declaration
    ;

external_declaration
    : declaration
    | function_definition
    | struct_declaration
    | union_declaration
    | enum_declaration
    | typedef_declaration
    | class_declaration
    ;

class_declaration
    : CLASS identifier LBRACE member_declaration_list RBRACE SEMICOLON
    {
        std::string class_name = std::string($2);
        if (lookup_type(class_name)) {
            yyerror(("Redefinition of class '" + class_name + "'").c_str());
        } else {
            Type* class_type = new Type(class_name, TK_CLASS);
            std::vector<MemberDef*>* member_defs = $4;
            for (MemberDef* member_def : *member_defs) {
                if (class_type->members.count(member_def->name)) {
                    yyerror(("Redeclaration of member '" + member_def->name + "'").c_str());
                } else {
                    Symbol* member_sym = new Symbol();
                    member_sym->name = member_def->name;
                    member_sym->type = member_def->type;
                    member_sym->kind = member_def->kind;
                    member_sym->access = member_def->access;
                    class_type->members[member_def->name] = member_sym;
                }
                delete member_def;
            }
            delete member_defs;
            install_type(class_name, class_type);
        }
        free($2);
    }
    ;

member_declaration_list
    : { $$ = new std::vector<MemberDef*>(); }
    | member_declaration_list member_declaration { if($2) { $1->insert($1->end(), $2->begin(), $2->end()); delete $2; } $$ = $1; }
    | member_declaration_list access_specifier ':' { g_current_access_specifier = $2; $$ = $1; }
    ;

access_specifier
    : PUBLIC    { $$ = AS_PUBLIC; }
    | PRIVATE   { $$ = AS_PRIVATE; }
    | PROTECTED { $$ = AS_PROTECTED; }
    ;

member_declaration
    : declaration_specifiers init_declarator_list SEMICOLON
    {
        Type* base_type = $1;
        vector<Symbol*>* symbols = $2;
        vector<MemberDef*>* members = new vector<MemberDef*>();
        if (base_type && symbols) {
            for (Symbol* sym : *symbols) {
                MemberDef* member = new MemberDef();
                member->name = sym->name;
                member->type = new Type(*base_type); // Copy base type properties
                // Merge declarator properties (like pointers)
                member->type->pointer_level = sym->type->pointer_level;
                member->type->array_dimensions = sym->type->array_dimensions;
                member->access = g_current_access_specifier;
                member->kind = SK_VARIABLE;
                members->push_back(member);
                delete sym; // Don't need the symbol anymore
            }
            $$ = members;
            delete symbols;
            delete base_type;
        } else {
            $$ = members; // empty list
            delete base_type;
            delete symbols;
        }
    }
    | declaration_specifiers declarator compound_statement {
        MemberDef* member = new MemberDef();
        Symbol* func_sym = $2;
        Type* return_type = $1;
        
        func_sym->kind = SK_FUNCTION;
        func_sym->type->kind = TK_FUNCTION;
        func_sym->type->return_type = return_type;
        if (g_current_param_list) {
            for (Symbol* p : *g_current_param_list) {
                func_sym->type->parameter_types.push_back(p->type);
            }
        }

        member->name = func_sym->name;
        member->type = func_sym->type;
        member->access = g_current_access_specifier;
        member->kind = SK_FUNCTION;
        
        $$ = new vector<MemberDef*>();
        $$->push_back(member);

        delete g_current_param_list;
        g_current_param_list = nullptr;
    }
    ;


enum_declaration
    : ENUM identifier LBRACE enumerator_list RBRACE SEMICOLON
    {
        string name = string($2);
        Type* new_type = new Type(name, TK_ENUM);
        install_type(name, new_type);
        free($2);
    }
    ;

enumerator_list
    : enumerator { $$= new vector<Symbol*>(); $$->push_back($1); }
    | enumerator_list COMMA enumerator { $1->push_back($3); $$ = $1; }
    ;

enumerator
    : identifier {
        Symbol* sym = new Symbol();
        sym->name = string($1);
        sym->type = new Type("int", TK_BASE);
        sym->kind = SK_ENUM_CONSTANT;
        sym->dval = 0; // You might want to track the actual enum value
        install_symbol(sym);
        $$ = sym;
        free($1);
    }
    ;

function_definition
    : declaration_specifiers declarator
      {
          g_labels.clear();
          g_unresolved_gotos.clear();
          Symbol* func_sym = $2;
          Type* return_type = $1;

          func_sym->kind = SK_FUNCTION;
          func_sym->type->kind = TK_FUNCTION;
          func_sym->type->return_type = return_type;
          if (g_current_param_list) {
              for (Symbol* p : *g_current_param_list) {
                  func_sym->type->parameter_types.push_back(p->type);
              }
          }
          install_symbol(func_sym);
          g_current_function = func_sym;
          enter_scope();
          if (g_current_param_list) {
              for (Symbol* p : *g_current_param_list) {
                  install_symbol(p);
              }
              delete g_current_param_list;
              g_current_param_list = nullptr;
          }
      }
      compound_statement
      {
          if (g_current_function && g_current_function->type->return_type->base_type != "void" && !g_current_function->has_return && g_current_function->name != "main") {
              yyerror(("Control reaches end of non-void function '" + g_current_function->name + "' without a return statement").c_str());
          }
          for (const auto& label_name : g_unresolved_gotos) {
              if (g_labels.find(label_name) == g_labels.end()) {
                  yyerror(("used label '" + label_name + "' was not defined").c_str());
              }
          }
          g_current_function = nullptr;
          exit_scope();
      }
    ;

declaration
    : declaration_specifiers init_declarator_list SEMICOLON
    {
        g_current_base_type = $1;
        vector<Symbol*>* symbols = $2;
        if (g_current_base_type && symbols) {
            for (Symbol* sym : *symbols) {
                // If the base type was NOT auto, we need to merge the base type info now.
                // If it WAS auto, the type was already fully resolved in init_declarator.
                if (g_current_base_type->base_type != "auto") {
                    sym->type->base_type = g_current_base_type->base_type;
                    sym->type->kind = g_current_base_type->kind;
                    sym->type->is_const = g_current_base_type->is_const || sym->type->is_const;
                    sym->type->is_unsigned = g_current_base_type->is_unsigned;
                    if (g_current_base_type->kind == TK_STRUCT || g_current_base_type->kind == TK_UNION || g_current_base_type->kind == TK_CLASS) {
                        sym->type->members = g_current_base_type->members;
                    }
                }
                install_symbol(sym);
            }
        }
        delete g_current_base_type;
        g_current_base_type = nullptr;
        delete symbols;
    }
    | declaration_specifiers SEMICOLON { delete $1; }
    ;

declaration_specifiers
    : storage_class_specifier declaration_specifiers { $$ = $2; }
    | type_qualifier declaration_specifiers { 
        if ($2) {
            if ($1->is_const) $2->is_const = true;
            if ($1->is_unsigned) $2->is_unsigned = true;
        }
        delete $1;
        $$ = $2;
    }
    | type_specifier { $$ = $1; }
    ;

storage_class_specifier: STATIC | EXTERN;

type_qualifier: CONST { $$ = new Type(); $$->is_const = true; } | UNSIGNED { $$ = new Type(); $$->is_unsigned = true; };

init_declarator_list
    : init_declarator { $$= new vector<Symbol*>(); $$->push_back($1); }
    | init_declarator_list COMMA init_declarator { $1->push_back($3); $$ = $1; }
    ;

init_declarator
    : declarator {
        if (g_current_base_type && g_current_base_type->base_type == "auto") {
            yyerror("declaration of 'auto' variable requires an initializer");
        }
        $$ = $1;
    }
    | declarator ASSIGN assignment_expression {
        Symbol* sym = $1;
        ExprResult* expr = $3;
        sym->dval = expr->dval;

        if (g_current_base_type && g_current_base_type->base_type == "auto") {
            if (expr->type && expr->type->kind == TK_FUNCTION) {
                // Lambda function assignment to auto
                sym->type = expr->type;
                sym->kind = SK_FUNCTION;
            } else {
                // Type deduction for auto variables.
                sym->type->base_type = expr->type->base_type;
                sym->type->kind = expr->type->kind;
                sym->type->members = expr->type->members;
                sym->type->is_const = g_current_base_type->is_const || sym->type->is_const;
                sym->type->pointer_level += expr->type->pointer_level;
                sym->type->array_dimensions.insert(sym->type->array_dimensions.end(),
                                                   expr->type->array_dimensions.begin(),
                                                   expr->type->array_dimensions.end());
            }
        }

        delete expr;
        $$ = sym;
    }
    | declarator ASSIGN initializer_list {
        Symbol* sym = $1;
        std::vector<InitializerItem*>* initializers = $3;
        if (!sym->type->array_dimensions.empty()) {
            int array_size = sym->type->array_dimensions[0];
            if (array_size >= 0 && initializers->size() > (size_t)array_size) {
                yyerror(("too many initializers for array of size " + to_string(array_size)).c_str());
            }
        }
        for (auto item : *initializers) {
            delete item;
        }
        delete initializers;
        $$ = sym;
    }
    ;

declarator
    : direct_declarator { $$ = $1; }
    | '*' declarator { 
        $2->type->pointer_level++; 
        $$ = $2; 
    }
    | declarator LPAREN parameter_list RPAREN {
        // Function pointer declaration: int (*func)(int, int)
        if ($1->type->pointer_level > 0) {
            // This is a function pointer
            $1->type->kind = TK_FUNCTION;
            $1->type->return_type = new Type(*$1->type);
            $1->type->return_type->pointer_level--;
            $1->type->base_type = $1->type->return_type->base_type;
            
            // Set the parameter types
            if ($3) {
                for (Symbol* param : *$3) {
                    $1->type->parameter_types.push_back(param->type);
                }
                delete $3;
            }
            $$ = $1;
        } else {
            // Regular function declaration
            $$ = $1;
            g_current_param_list = $3;
        }
    }
    | declarator LPAREN RPAREN {
        // Function pointer with no parameters: int (*func)()
        if ($1->type->pointer_level > 0) {
            // This is a function pointer
            $1->type->kind = TK_FUNCTION;
            $1->type->return_type = new Type(*$1->type);
            $1->type->return_type->pointer_level--;
            $1->type->base_type = $1->type->return_type->base_type;
            $$ = $1;
        } else {
            // Regular function declaration
            $$ = $1;
            g_current_param_list = nullptr;
        }
    }
    ;

direct_declarator
    : identifier {
        $$ = new Symbol();
        $$->name = string($1);
        $$->type = new Type();
        $$->kind = SK_VARIABLE;
        $$->dval = 0;
        $$->has_return = false;
        free($1);
    }
    | LPAREN declarator RPAREN { $$ = $2; }
    | direct_declarator LPAREN parameter_list RPAREN {
        $$ = $1;
        g_current_param_list = $3;
    }
    | direct_declarator LPAREN RPAREN { $$ = $1; g_current_param_list = nullptr; }
    | direct_declarator LBRACKET expression_opt RBRACKET {
        $$ = $1;
        int dim_size = -1;
        if ($3) {
            if (!$3->is_const_expr) {
                yyerror("Array dimension must be a constant expression");
            }
            dim_size = (int)$3->dval;
            delete $3;
        }
        $$->type->array_dimensions.push_back(dim_size);
    }
    ;

struct_declaration
    : STRUCT identifier LBRACE struct_declaration_list RBRACE SEMICOLON
    {
        string name = string($2);
        Type* new_type = new Type(name, TK_STRUCT);
        // Store the member declarations in the type
        for (Symbol* member : *$4) {
            new_type->members[member->name] = member;
        }
        install_type(name, new_type);
        free($2);
        delete $4;
    }
    | STRUCT identifier SEMICOLON
    {
        // Forward declaration: struct name;
        string name = string($2);
        Type* new_type = new Type(name, TK_STRUCT);
        install_type(name, new_type);
        free($2);
    }
    ;

union_declaration
    : UNION identifier LBRACE struct_declaration_list RBRACE SEMICOLON
    {
        string name = string($2);
        Type* new_type = new Type(name, TK_UNION);
        // Store the member declarations in the type
        for (Symbol* member : *$4) {
            new_type->members[member->name] = member;
        }
        install_type(name, new_type);
        free($2);
        delete $4;
    }
    | UNION identifier SEMICOLON
    {
        // Forward declaration: union name;
        string name = string($2);
        Type* new_type = new Type(name, TK_UNION);
        install_type(name, new_type);
        free($2);
    }
    ;

typedef_declaration
    : TYPEDEF declaration_specifiers declarator SEMICOLON
    {
        Type* base_type = $2;
        Symbol* sym = $3;
        if (base_type && sym) {
            sym->kind = SK_TYPEDEF_NAME;
            sym->type->base_type = base_type->base_type;
            sym->type->is_const = base_type->is_const;
            sym->type->is_unsigned = base_type->is_unsigned;
            install_symbol(sym);
            type_table[sym->name] = sym->type;
        }
        delete base_type;
    }
    | TYPEDEF struct_or_union_specifier identifier SEMICOLON
    {
        Type* struct_type = $2;
        string name = string($3);
        Symbol* sym = new Symbol();
        sym->name = name;
        sym->type = struct_type;
        sym->kind = SK_TYPEDEF_NAME;
        install_symbol(sym);
        type_table[name] = struct_type;
        free($3);
    }
    | TYPEDEF enum_specifier identifier SEMICOLON
    {
        Type* enum_type = $2;
        string name = string($3);
        Symbol* sym = new Symbol();
        sym->name = name;
        sym->type = enum_type;
        sym->kind = SK_TYPEDEF_NAME;
        install_symbol(sym);
        type_table[name] = enum_type;
        free($3);
    }
    ;

declaration_list: declaration | declaration_list declaration;

struct_declaration_list
    : struct_declaration_item { $$ = $1; }
    | struct_declaration_list struct_declaration_item { $1->insert($1->end(), $2->begin(), $2->end()); delete $2; $$ = $1; }
    ;

struct_declaration_item
    : declaration_specifiers init_declarator_list SEMICOLON
    {
        Type* base_type = $1;
        vector<Symbol*>* symbols = $2;
        if (base_type && symbols) {
            for (Symbol* member_sym : *symbols) {
                member_sym->type->base_type = base_type->base_type;
                member_sym->type->kind = base_type->kind;
                member_sym->type->is_const = base_type->is_const || member_sym->type->is_const;
                if (base_type->kind == TK_STRUCT || base_type->kind == TK_UNION) {
                    member_sym->type->members = base_type->members;
                }
            }
            $$ = symbols;
            delete base_type;
        } else {
            $$ = new vector<Symbol*>();
            delete base_type;
            delete symbols;
        }
    }
    ;

struct_declaration: declaration_specifiers init_declarator_list SEMICOLON;

identifier: IDENTIFIER { $$ = $1; };

type_specifier
    : VOID   { $$ = new Type("void", TK_BASE); }
    | CHAR   { $$ = new Type("char", TK_BASE); }
    | INT    { $$ = new Type("int", TK_BASE); }
    | FLOAT  { $$ = new Type("float", TK_BASE); }
    | DOUBLE { $$ = new Type("double", TK_BASE); }
    | BOOL   { $$ = new Type("bool", TK_BASE); }
    | AUTO   { $$ = new Type("auto", TK_BASE); }
    | struct_or_union_specifier { $$ = $1; }
    | enum_specifier { $$ = $1; }
    | identifier {
        string name = string($1);
        Symbol* sym = lookup_symbol(name);
        if (sym && sym->kind == SK_TYPEDEF_NAME) {
            $$ = new Type(*sym->type);
        } else {
            Type* t = lookup_type(name);
            if (t) {
                $$ = new Type(*t); // Copy the complete type
            } else {
                yyerror(("Unknown type name '" + name + "'").c_str());
                $$ = new Type("error");
            }
        }
        free($1);
    }
    ;
    
enum_specifier
    : ENUM identifier
    {
        string name = string($2);
        Type* t = lookup_type(name);
        if (t && t->kind == TK_ENUM) {
            $$ = new Type(*t);
        } else {
            yyerror(("Unknown enum type '" + name + "'").c_str());
            $$ = new Type("error");
        }
        free($2);
    }
    ;

struct_or_union_specifier
    : STRUCT identifier
    {
        string name = string($2);
        Type* t = lookup_type(name);
        if (t && t->kind == TK_STRUCT) {
            $$ = new Type(*t);
        } else {
            // Create a new struct type
            $$ = new Type(name, TK_STRUCT);
            install_type(name, $$);
        }
        free($2);
    }
    | UNION identifier
    {
        string name = string($2);
        Type* t = lookup_type(name);
        if (t && t->kind == TK_UNION) {
            $$ = new Type(*t);
        } else {
            // Create a new union type
            $$ = new Type(name, TK_UNION);
            install_type(name, $$);
        }
        free($2);
    }
    | STRUCT LBRACE struct_declaration_list RBRACE
    {
        // Anonymous struct
        $$ = new Type("", TK_STRUCT);
        // Store the member declarations in the type
        for (Symbol* member : *$3) {
            $$->members[member->name] = member;
        }
        delete $3;
    }
    | UNION LBRACE struct_declaration_list RBRACE
    {
        // Anonymous union
        $$ = new Type("", TK_UNION);
        // Store the member declarations in the type
        for (Symbol* member : *$3) {
            $$->members[member->name] = member;
        }
        delete $3;
    }
    | STRUCT identifier LBRACE struct_declaration_list RBRACE
    {
        string name = string($2);
        Type* new_type = new Type(name, TK_STRUCT);
        for (Symbol* member : *$4) {
            new_type->members[member->name] = member;
        }
        install_type(name, new_type);
        $$ = new_type;
        free($2);
        delete $4;
    }
    | UNION identifier LBRACE struct_declaration_list RBRACE
    {
        string name = string($2);
        Type* new_type = new Type(name, TK_UNION);
        for (Symbol* member : *$4) {
            new_type->members[member->name] = member;
        }
        install_type(name, new_type);
        $$ = new_type;
        free($2);
        delete $4;
    }
    ;

compound_statement
    : LBRACE { enter_scope(); } statement_list RBRACE { exit_scope(); }
    | LBRACE { enter_scope(); } RBRACE { exit_scope(); }
    ;

statement_list: statement | statement_list statement;

statement
    : expression_statement
    | compound_statement
    | selection_statement
    | iteration_statement
    | jump_statement
    | declaration
    | labeled_statement
    ;

expression_statement: expression_opt SEMICOLON;

expression_opt
    : expression { $$ = $1; }
    | /* empty */ { $$ = nullptr; }
    ;

selection_statement
    : IF LPAREN expression RPAREN statement %prec IF_WITHOUT_ELSE
    | IF LPAREN expression RPAREN statement ELSE statement
    | SWITCH LPAREN expression RPAREN { g_switch_depth++; } statement { g_switch_depth--; }
    ;

iteration_statement
    : WHILE LPAREN expression RPAREN { g_loop_depth++; } statement { g_loop_depth--; }
    | FOR LPAREN expression_opt SEMICOLON expression_opt SEMICOLON expression_opt RPAREN { g_loop_depth++; } statement { g_loop_depth--; }
    | FOR LPAREN declaration expression_opt SEMICOLON expression_opt RPAREN { g_loop_depth++; } statement { g_loop_depth--; }
    | DO { g_loop_depth++; } statement WHILE LPAREN expression RPAREN SEMICOLON { g_loop_depth--; }
    ;

jump_statement
    : RETURN expression_opt SEMICOLON {
        if (g_parsing_lambda) {
            if ($2) { // expression is present
                g_inferred_return_type = new Type(*$2->type);
            } else {
                g_inferred_return_type = new Type("void");
            }
        } else if (!g_current_function) {
            yyerror("'return' statement not in function");
        } else {
            g_current_function->has_return = true;
            Type* func_return_type = g_current_function->type->return_type;
            ExprResult* return_expr = $2;
            if (return_expr) {
                if (func_return_type->base_type == "void") {
                    yyerror("Cannot return a value from a void function");
                } else if (return_expr->type->base_type != func_return_type->base_type) {
                    yyerror("Return type mismatch in function");
                }
                delete return_expr;
            } else {
                if (func_return_type->base_type != "void") {
                    yyerror("Non-void function must return a value");
                }
            }
        }
    }
    | BREAK SEMICOLON {
        if (g_loop_depth == 0 && g_switch_depth == 0) {
            yyerror("'break' statement not in loop or switch statement");
        }
    }
    | CONTINUE SEMICOLON
    | GOTO identifier SEMICOLON {
        string label_name = string($2);
        if (g_labels.find(label_name) == g_labels.end()) {
            g_unresolved_gotos.push_back(label_name);
        }
        free($2);
    }
    ;

labeled_statement
    : identifier ':' statement {
        string label_name = string($1);
        if (g_labels.count(label_name)) {
            yyerror(("redefinition of label '" + label_name + "'").c_str());
        } else {
            g_labels[label_name] = true;
        }
        free($1);
    }
    | CASE conditional_expression ':' statement {
        if (g_switch_depth == 0) {
            yyerror("'case' label not within a switch statement");
        }
        if ($2 && !$2->is_const_expr) {
            yyerror("case label does not reduce to a constant");
        }
        delete $2;
    }
    | DEFAULT ':' statement {
        if (g_switch_depth == 0) {
            yyerror("'default' label not within a switch statement");
        }
    }
    ;

expression
    : assignment_expression { $$ = $1; }
    | expression COMMA assignment_expression { delete $1; $$ = $3; }
    ;

assignment_expression
    : conditional_expression { $$ = $1; }
    | unary_expression ASSIGN assignment_expression {
        ExprResult* lhs = $1;
        ExprResult* rhs = $3;
        if (!lhs->lvalue_symbol) {
            yyerror("lvalue required as left operand of assignment");
            $$ = new ExprResult(0.0, nullptr, nullptr);
        } else if (!lhs->type->array_dimensions.empty()) {
            yyerror("assignment to array type");
            $$ = new ExprResult(0.0, nullptr, nullptr);
        } else {
            lhs->lvalue_symbol->dval = rhs->dval;
            $$ = new ExprResult(rhs->dval, rhs->type, nullptr);
        }
        delete lhs;
        delete rhs;
    }
    | unary_expression ADD_ASSIGN assignment_expression {
        ExprResult* lhs = $1;
        ExprResult* rhs = $3;
        if (!lhs->lvalue_symbol) {
            yyerror("lvalue required as left operand of assignment");
            $$ = new ExprResult(0.0, nullptr, nullptr);
        } else {
            lhs->lvalue_symbol->dval += rhs->dval;
            $$ = new ExprResult(lhs->lvalue_symbol->dval, lhs->type, nullptr);
        }
        delete lhs;
        delete rhs;
    }
    | unary_expression SUB_ASSIGN assignment_expression {
        ExprResult* lhs = $1;
        ExprResult* rhs = $3;
        if (!lhs->lvalue_symbol) {
            yyerror("lvalue required as left operand of assignment");
            $$ = new ExprResult(0.0, nullptr, nullptr);
        } else {
            lhs->lvalue_symbol->dval -= rhs->dval;
            $$ = new ExprResult(lhs->lvalue_symbol->dval, lhs->type, nullptr);
        }
        delete lhs;
        delete rhs;
    }
    | unary_expression MUL_ASSIGN assignment_expression {
        ExprResult* lhs = $1;
        ExprResult* rhs = $3;
        if (!lhs->lvalue_symbol) {
            yyerror("lvalue required as left operand of assignment");
            $$ = new ExprResult(0.0, nullptr, nullptr);
        } else {
            lhs->lvalue_symbol->dval *= rhs->dval;
            $$ = new ExprResult(lhs->lvalue_symbol->dval, lhs->type, nullptr);
        }
        delete lhs;
        delete rhs;
    }
    | unary_expression DIV_ASSIGN assignment_expression {
        ExprResult* lhs = $1;
        ExprResult* rhs = $3;
        if (!lhs->lvalue_symbol) {
            yyerror("lvalue required as left operand of assignment");
            $$ = new ExprResult(0.0, nullptr, nullptr);
        } else {
            lhs->lvalue_symbol->dval /= rhs->dval;
            $$ = new ExprResult(lhs->lvalue_symbol->dval, lhs->type, nullptr);
        }
        delete lhs;
        delete rhs;
    }
    ;

conditional_expression
    : logical_or_expression { $$ = $1; }
    | logical_or_expression '?' expression ':' conditional_expression {
        $$ = $5;
        delete $1;
        delete $3;
    }
    ;

logical_or_expression
    : logical_and_expression { $$ = $1; }
    | logical_or_expression OR_OP logical_and_expression {
        $$ = new ExprResult($1->dval || $3->dval, new Type("bool"));
        delete $1;
        delete $3;
    }
    ;

logical_and_expression
    : bitwise_or_expression { $$ = $1; }
    | logical_and_expression AND_OP bitwise_or_expression {
        $$ = new ExprResult($1->dval && $3->dval, new Type("bool"));
        delete $1;
        delete $3;
    }
    ;

bitwise_or_expression
    : bitwise_xor_expression { $$ = $1; }
    | bitwise_or_expression '|' bitwise_xor_expression {
        $$ = new ExprResult((double)((int)$1->dval | (int)$3->dval), new Type("int"));
        delete $1;
        delete $3;
    }
    ;

bitwise_xor_expression
    : bitwise_and_expression { $$ = $1; }
    | bitwise_xor_expression '^' bitwise_and_expression {
        $$ = new ExprResult((double)((int)$1->dval ^ (int)$3->dval), new Type("int"));
        delete $1;
        delete $3;
    }
    ;

bitwise_and_expression
    : equality_expression { $$ = $1; }
    | bitwise_and_expression '&' equality_expression {
        $$ = new ExprResult((double)((int)$1->dval & (int)$3->dval), new Type("int"));
        delete $1;
        delete $3;
    }
    ;

equality_expression
    : relational_expression { $$ = $1; }
    | equality_expression EQ_OP relational_expression {
        $$ = new ExprResult($1->dval == $3->dval, new Type("bool"));
        delete $1;
        delete $3;
    }
    | equality_expression NE_OP relational_expression {
        $$ = new ExprResult($1->dval != $3->dval, new Type("bool"));
        delete $1;
        delete $3;
    }
    ;

relational_expression
    : shift_expression { $$ = $1; }
    | relational_expression '<' shift_expression {
        $$ = new ExprResult($1->dval < $3->dval, new Type("bool"));
        delete $1;
        delete $3;
    }
    | relational_expression '>' shift_expression {
        $$ = new ExprResult($1->dval > $3->dval, new Type("bool"));
        delete $1;
        delete $3;
    }
    | relational_expression LE_OP shift_expression {
        $$ = new ExprResult($1->dval <= $3->dval, new Type("bool"));
        delete $1;
        delete $3;
    }
    | relational_expression GE_OP shift_expression {
        $$ = new ExprResult($1->dval >= $3->dval, new Type("bool"));
        delete $1;
        delete $3;
    }
    ;

shift_expression
    : additive_expression { $$ = $1; }
    | shift_expression LSHIFT_OP additive_expression {
        $$ = new ExprResult((double)((int)$1->dval << (int)$3->dval), new Type("int"));
        delete $1;
        delete $3;
    }
    | shift_expression RSHIFT_OP additive_expression {
        $$ = new ExprResult((double)((int)$1->dval >> (int)$3->dval), new Type("int"));
        delete $1;
        delete $3;
    }
    ;

additive_expression
    : multiplicative_expression { $$ = $1; }
    | additive_expression '+' multiplicative_expression {
        $$ = new ExprResult($1->dval + $3->dval, $1->type);
        delete $3;
    }
    | additive_expression '-' multiplicative_expression {
        $$ = new ExprResult($1->dval - $3->dval, $1->type);
        delete $3;
    }
    ;

multiplicative_expression
    : cast_expression { $$ = $1; }
    | multiplicative_expression '*' cast_expression {
        $$ = new ExprResult($1->dval * $3->dval, $1->type);
        delete $3;
    }
    | multiplicative_expression '/' cast_expression {
        $$ = new ExprResult($1->dval / $3->dval, $1->type);
        delete $3;
    }
    | multiplicative_expression '%' cast_expression {
        $$ = new ExprResult((double)((int)$1->dval % (int)$3->dval), new Type("int"));
        delete $1;
        delete $3;
    }
    ;

cast_expression
    : unary_expression { $$ = $1; }
    | LPAREN type_name RPAREN cast_expression {
        // Here you would perform a type cast.
        // For simplicity, we just return the expression with the new type.
        $4->type = $2;
        $$ = $4;
    }
    ;
    
type_name
    : declaration_specifiers { $$ = $1; }
    | declaration_specifiers abstract_declarator {
        Type* base_type = $1;
        Type* declarator_type = $2;
        declarator_type->base_type = base_type->base_type;
        declarator_type->kind = base_type->kind;
        declarator_type->is_const = base_type->is_const;
        declarator_type->is_unsigned = base_type->is_unsigned;
        if (base_type->kind == TK_STRUCT || base_type->kind == TK_UNION || base_type->kind == TK_CLASS) {
            declarator_type->members = base_type->members;
        }
        $$ = declarator_type;
        delete base_type;
    }
    ;

abstract_declarator
    : '*' { $$ = new Type(); $$->pointer_level = 1; }
    | '*' abstract_declarator {
        $$ = $2;
        $$->pointer_level++;
    }
    | LBRACKET expression_opt RBRACKET {
        $$ = new Type();
        int dim_size = -1;
        if ($2) {
            if (!$2->is_const_expr) {
                yyerror("Array dimension must be a constant expression");
            }
            dim_size = (int)$2->dval;
            delete $2;
        }
        $$->array_dimensions.push_back(dim_size);
    }
    | abstract_declarator LBRACKET expression_opt RBRACKET {
        $$ = $1;
        int dim_size = -1;
        if ($3) {
            if (!$3->is_const_expr) {
                yyerror("Array dimension must be a constant expression");
            }
            dim_size = (int)$3->dval;
            delete $3;
        }
        $$->array_dimensions.push_back(dim_size);
    }
    ;

unary_expression
    : postfix_expression { $$ = $1; }
    | INC_OP unary_expression {
        // Pre-increment
        if ($2->lvalue_symbol) {
            $2->lvalue_symbol->dval++;
            $$ = new ExprResult($2->lvalue_symbol->dval, $2->type, $2->lvalue_symbol);
        } else {
            yyerror("lvalue required for pre-increment");
            $$ = new ExprResult(0.0, nullptr, nullptr);
        }
        delete $2;
    }
    | DEC_OP unary_expression {
        // Pre-decrement
        if ($2->lvalue_symbol) {
            $2->lvalue_symbol->dval--;
            $$ = new ExprResult($2->lvalue_symbol->dval, $2->type, $2->lvalue_symbol);
        } else {
            yyerror("lvalue required for pre-decrement");
            $$ = new ExprResult(0.0, nullptr, nullptr);
        }
        delete $2;
    }
    | '&' cast_expression {
        // Address-of operator
        Type* new_type = new Type(*$2->type);
        new_type->pointer_level++;
        $$ = new ExprResult(0.0, new_type, nullptr);
        delete $2;
    }
    | '*' cast_expression {
        // Dereference operator
        if ($2->type->pointer_level == 0) {
            yyerror("Cannot dereference a non-pointer type");
            $$ = new ExprResult(0.0, nullptr, nullptr);
        } else {
            Type* new_type = new Type(*$2->type);
            new_type->pointer_level--;
            $$ = new ExprResult($2->dval, new_type, $2->lvalue_symbol);
        }
        delete $2;
    }
    | '+' cast_expression { $$ = $2; }
    | '-' cast_expression { $$ = new ExprResult(-$2->dval, $2->type, nullptr); delete $2; }
    | '~' cast_expression { $$ = new ExprResult((double)(~((int)$2->dval)), new Type("int")); delete $2; }
    | '!' cast_expression { $$ = new ExprResult(!$2->dval, new Type("bool")); delete $2; }
    | SIZEOF unary_expression {
        // You would need to implement size calculation based on type
        $$ = new ExprResult(4.0, new Type("int"), nullptr); // Placeholder
        delete $2;
    }
    | SIZEOF LPAREN type_name RPAREN {
        // You would need to implement size calculation based on type
        $$ = new ExprResult(4.0, new Type("int"), nullptr); // Placeholder
        delete $3;
    }
    ;

postfix_expression
    : primary_expression { $$ = $1; }
    | postfix_expression LBRACKET expression RBRACKET {
        // Array subscripting
        $$ = $1;
        delete $3;
    }
    | postfix_expression LPAREN RPAREN {
        // Function call with no arguments
        $$ = new ExprResult(0.0, new Type("int"), nullptr);
        delete $1;
    }
    | postfix_expression LPAREN argument_list RPAREN {
        // Function call with arguments
        $$ = new ExprResult(0.0, new Type("int"), nullptr);
        delete $1;
        delete $3;
    }
    | postfix_expression DOT identifier {
        ExprResult* struct_expr = $1;
        string member_name = string($3);
        Type* struct_type = struct_expr->type;
        
        if (struct_type->kind != TK_STRUCT && struct_type->kind != TK_UNION && struct_type->kind != TK_CLASS) {
            yyerror("request for member of non-aggregate type");
            $$ = new ExprResult();
        } else if (struct_type->members.find(member_name) == struct_type->members.end()) {
            yyerror(("no member named '" + member_name + "' in aggregate").c_str());
            $$ = new ExprResult();
        } else {
            Symbol* member_sym = struct_type->members.at(member_name);
            $$ = new ExprResult(member_sym->dval, member_sym->type, member_sym);
        }
        
        delete struct_expr;
        free($3);
    }
    | postfix_expression PTR_OP identifier {
        ExprResult* ptr_expr = $1;
        string member_name = string($3);
        Type* ptr_type = ptr_expr->type;

        if (ptr_type->pointer_level == 0) {
            yyerror("left of '->' must be a pointer");
            $$ = new ExprResult();
        } else {
            Type* struct_type = new Type(*ptr_type);
            struct_type->pointer_level--;

            if (struct_type->kind != TK_STRUCT && struct_type->kind != TK_UNION && struct_type->kind != TK_CLASS) {
                yyerror("request for member of non-aggregate type");
                $$ = new ExprResult();
            } else if (struct_type->members.find(member_name) == struct_type->members.end()) {
                yyerror(("no member named '" + member_name + "' in aggregate").c_str());
                $$ = new ExprResult();
            } else {
                Symbol* member_sym = struct_type->members.at(member_name);
                $$ = new ExprResult(member_sym->dval, member_sym->type, member_sym);
            }
            delete struct_type;
        }

        delete ptr_expr;
        free($3);
    }
    | postfix_expression INC_OP {
        // Post-increment
        if ($1->lvalue_symbol) {
            $$ = new ExprResult($1->lvalue_symbol->dval, $1->type, $1->lvalue_symbol);
            $1->lvalue_symbol->dval++;
        } else {
            yyerror("lvalue required for post-increment");
            $$ = new ExprResult(0.0, nullptr, nullptr);
        }
        delete $1;
    }
    | postfix_expression DEC_OP {
        // Post-decrement
        if ($1->lvalue_symbol) {
            $$ = new ExprResult($1->lvalue_symbol->dval, $1->type, $1->lvalue_symbol);
            $1->lvalue_symbol->dval--;
        } else {
            yyerror("lvalue required for post-decrement");
            $$ = new ExprResult(0.0, nullptr, nullptr);
        }
        delete $1;
    }
    ;

primary_expression
    : identifier {
        Symbol* sym = lookup_symbol(string($1));
        if (!sym) {
            yyerror(("'" + string($1) + "' undeclared").c_str());
            $$ = new ExprResult(0.0, new Type("error"), nullptr);
        } else {
            $$ = new ExprResult(sym->dval, sym->type, sym);
        }
        free($1);
    }
    | INT_LITERAL { $$ = new ExprResult((double)$1, new Type("int")); $$->is_const_expr = true; }
    | FLOAT_LITERAL { $$ = new ExprResult($1, new Type("float")); $$->is_const_expr = true; }
    | CHAR_LITERAL { $$ = new ExprResult((double)$1[1], new Type("char")); $$->is_const_expr = true; free($1); }
    | STRING_LITERAL { $$ = new ExprResult(0.0, new Type("char")); $$->type->pointer_level = 1; free($1); }
    | LPAREN expression RPAREN { $$ = $2; }
    | lambda_expression { $$ = $1; }
    ;

lambda_expression
    : LBRACKET RBRACKET LPAREN parameter_list_opt RPAREN 
      {
          g_parsing_lambda = true;
          g_inferred_return_type = nullptr;
          enter_scope();
          if ($4) {
              for (Symbol* p : *$4) {
                  install_symbol(p);
              }
          }
      }
      compound_statement
      {
          exit_scope();
          g_parsing_lambda = false;
          Type* func_type = new Type("", TK_FUNCTION);
          if (g_inferred_return_type) {
              func_type->return_type = g_inferred_return_type;
          } else {
              func_type->return_type = new Type("void");
          }
          if ($4) {
              for (Symbol* p : *$4) {
                  func_type->parameter_types.push_back(p->type);
              }
              delete $4;
          }
          Symbol* lambda_sym = new Symbol();
          lambda_sym->name = "__lambda"; // Anonymous
          lambda_sym->type = func_type;
          lambda_sym->kind = SK_FUNCTION;
          $$ = new ExprResult(0.0, func_type, lambda_sym);
      }
    ;

parameter_list_opt
    : /* empty */ { $$ = nullptr; }
    | parameter_list { $$ = $1; }
    ;

parameter_list
    : parameter_declaration { $$= new vector<Symbol*>(); $$->push_back($1); }
    | parameter_list COMMA parameter_declaration { $1->push_back($3); $$ = $1; }
    ;

parameter_declaration
    : declaration_specifiers declarator {
        Type* base_type = $1;
        Symbol* sym = $2;
        // Merge the base type info into the symbol's type, which has pointer/array info.
        sym->type->base_type = base_type->base_type;
        sym->type->kind = base_type->kind;
        sym->type->is_const = base_type->is_const || sym->type->is_const;
        sym->type->is_unsigned = base_type->is_unsigned;
        if (base_type->kind == TK_STRUCT || base_type->kind == TK_UNION || base_type->kind == TK_CLASS) {
            sym->type->members = base_type->members;
        }
        delete base_type;
        $$ = sym;
    }
    ;

argument_list
    : assignment_expression { $$= new vector<ExprResult*>(); $$->push_back($1); }
    | argument_list COMMA assignment_expression { $1->push_back($3); $$ = $1; }
    ;

initializer_list
    : LBRACE initializer_items RBRACE { $$ = $2; }
    | LBRACE RBRACE { $$ = new std::vector<InitializerItem*>(); }
    | LBRACE initializer_items COMMA RBRACE { $$ = $2; }
    ;

initializer_items
    : initializer { $$= new std::vector<InitializerItem*>(); $$->push_back($1); }
    | initializer_items COMMA initializer { $1->push_back($3); $$ = $1; }
    ;

initializer
    : assignment_expression { $$ = new InitializerItem($1); }
    | initializer_list { $$ = new InitializerItem($1); }
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
    return 0;
}
