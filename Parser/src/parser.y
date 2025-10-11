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
Type::Type(string base, TypeKind k) : kind(k), base_type(base), is_const(false), pointer_level(0), return_type(nullptr) {}

Type::~Type() {
    delete return_type;
}

string Type::toString() const {
    string desc = "";
    if (is_const) desc += "const ";
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

void enter_scope() { symbol_table.push(map<string, Symbol*>()); }
void exit_scope() { if (!symbol_table.empty()) symbol_table.pop();
}

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

void yyerror(const char* s) {
    cerr << "Parser Error at line " << yylineno << ": " << s << " near '" << yytext << "'" << endl;
}

%}

%token CONST IF ELSE WHILE FOR RETURN BREAK CONTINUE GOTO SWITCH CASE DEFAULT DO SIZEOF
%token STATIC EXTERN AUTO STRUCT UNION ENUM TYPEDEF CLASS PUBLIC PROTECTED PRIVATE
%token BOOL VOID INT CHAR FLOAT DOUBLE
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
}

%type <expr_ptr> expression expression_opt assignment_expression conditional_expression logical_or_expression
%type <expr_ptr> logical_and_expression bitwise_or_expression bitwise_xor_expression
%type <expr_ptr> bitwise_and_expression equality_expression relational_expression shift_expression
%type <expr_ptr> additive_expression multiplicative_expression cast_expression
%type <expr_ptr> unary_expression postfix_expression primary_expression

%type <type_ptr> type_specifier declaration_specifiers type_name abstract_declarator struct_or_union_specifier enum_specifier
%type <symbol_list_ptr> init_declarator_list parameter_list enumerator_list struct_declaration_list
%type <symbol_ptr> declarator direct_declarator init_declarator parameter_declaration struct_declaration_item enumerator
%type <str> identifier

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
    : { enter_scope();
    } external_declaration_list {
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
    : enumerator { $$ = new vector<Symbol*>(); $$->push_back($1); }
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
          g_current_function = nullptr;
          exit_scope();
      }
    ;
declaration
    : declaration_specifiers init_declarator_list SEMICOLON
    {
        Type* base_type = $1;
        vector<Symbol*>* symbols = $2;
        if (base_type && symbols) {
            for (Symbol* sym : *symbols) {
                // Correctly merge the base type information (from declaration_specifiers)
                // into the symbol's type (which has pointer/array info from declarator),
                // instead of replacing it.
                sym->type->base_type = base_type->base_type;
                sym->type->kind = base_type->kind;
                sym->type->is_const = base_type->is_const || sym->type->is_const;

                // If the base type is a struct or union, copy its member list.
                if (base_type->kind == TK_STRUCT || base_type->kind == TK_UNION) {
                    sym->type->members = base_type->members;
                }
                
                install_symbol(sym);
            }
        }
        delete base_type;
        delete symbols;
    }
    | declaration_specifiers SEMICOLON { delete $1; }
    ;
declaration_specifiers
    : storage_class_specifier declaration_specifiers { $$ = $2; }
    | type_qualifier declaration_specifiers { if($2) $2->is_const = true; $$ = $2; }
    | type_specifier { $$ = $1;
    }
    ;

storage_class_specifier: STATIC | EXTERN | AUTO;
type_qualifier: CONST;
init_declarator_list
    : init_declarator { $$ = new vector<Symbol*>(); $$->push_back($1); }
    | init_declarator_list COMMA init_declarator { $1->push_back($3); $$ = $1; }
    ;
init_declarator
    : declarator { $$ = $1; }
    | declarator ASSIGN assignment_expression {
        $1->dval = $3->dval;
        delete $3;
        $$ = $1;
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
    : direct_declarator { $$ = $1;
    }
    | '*' declarator { $2->type->pointer_level++; $$ = $2; }
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
    | TYPEDEF struct_or_union_specifier LBRACE struct_declaration_list RBRACE identifier SEMICOLON
    {
        // Handle: typedef union data { int a; } data_t;
        Type* struct_type = $2;
        string alias_name = string($6);
        
        Symbol* sym = new Symbol();
        sym->name = alias_name;
        sym->type = struct_type;
        sym->kind = SK_TYPEDEF_NAME;
        install_symbol(sym);
        type_table[alias_name] = struct_type;
        free($6);
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
    : struct_declaration_item { $$ = new vector<Symbol*>(); $$->push_back($1); }
    | struct_declaration_list struct_declaration_item { $1->push_back($2); $$ = $1; }
    ;
struct_declaration_item
    : declaration_specifiers init_declarator_list SEMICOLON
    {
        Type* base_type = $1;
        vector<Symbol*>* symbols = $2;
        if (base_type && symbols && !symbols->empty()) {
            // For struct members, we take the first declarator and use it as the member
            Symbol* member_sym = (*symbols)[0];
            member_sym->type->base_type = base_type->base_type;
            member_sym->type->kind = base_type->kind;
            member_sym->type->is_const = base_type->is_const || member_sym->type->is_const;

            // If the base type is a struct or union, copy its member list.
            if (base_type->kind == TK_STRUCT || base_type->kind == TK_UNION) {
                member_sym->type->members = base_type->members;
            }
            
            $$ = member_sym;
            // Clean up the rest
            for (size_t i = 1; i < symbols->size(); ++i) {
                delete (*symbols)[i];
            }
            delete symbols;
            delete base_type;
        } else {
            $$ = nullptr;
            delete base_type;
            delete symbols;
        }
    }
    ;
struct_declaration: declaration_specifiers init_declarator_list SEMICOLON;
identifier: IDENTIFIER { $$ = $1; };
type_specifier
    : VOID   { $$ = new Type("void", TK_BASE);
    }
    | CHAR   { $$ = new Type("char", TK_BASE); }
    | INT    { $$ = new Type("int", TK_BASE); }
    | FLOAT  { $$ = new Type("float", TK_BASE); }
    | DOUBLE { $$ = new Type("double", TK_BASE);
    }
    | BOOL   { $$ = new Type("bool", TK_BASE); }
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
    ;
compound_statement
    : LBRACE { enter_scope(); } statement_list RBRACE { exit_scope();
    }
    | LBRACE { enter_scope(); } RBRACE { exit_scope(); }
    ;

statement_list: statement |
    statement_list statement;
statement
    : expression_statement
    | compound_statement
    | selection_statement
    | iteration_statement
    | jump_statement
    | declaration
    ;
expression_statement: expression_opt SEMICOLON;
expression_opt
    : expression { $$ = $1; }
    | /* empty */ { $$ = nullptr; }
    ;
selection_statement
    : IF LPAREN expression RPAREN statement %prec IF_WITHOUT_ELSE
    | IF LPAREN expression RPAREN statement ELSE statement
    | SWITCH LPAREN expression RPAREN statement
    ;
iteration_statement
    : WHILE LPAREN expression RPAREN statement
    | FOR LPAREN expression_opt SEMICOLON expression_opt SEMICOLON expression_opt RPAREN statement
    | FOR LPAREN declaration expression_opt SEMICOLON expression_opt RPAREN statement
    | DO statement WHILE LPAREN expression RPAREN SEMICOLON
    ;
jump_statement
    : RETURN expression_opt SEMICOLON {
        if (!g_current_function) {
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
    | BREAK SEMICOLON
    | CONTINUE SEMICOLON
    | GOTO identifier SEMICOLON
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
    | logical_or_expression OR_OP logical_and_expression { $$ = new ExprResult($1->dval || $3->dval, new Type("bool")); delete $1; delete $3;
    }
    ;

logical_and_expression
    : bitwise_or_expression { $$ = $1; }
    | logical_and_expression AND_OP bitwise_or_expression { $$ = new ExprResult($1->dval && $3->dval, new Type("bool")); delete $1; delete $3;
    }
    ;

bitwise_or_expression
    : bitwise_xor_expression { $$ = $1; }
    | bitwise_or_expression '|' bitwise_xor_expression { $$ = new ExprResult((double)((long)$1->dval | (long)$3->dval), $1->type); delete $3; }
    ;
bitwise_xor_expression
    : bitwise_and_expression { $$ = $1; }
    | bitwise_xor_expression '^' bitwise_and_expression { $$ = new ExprResult((double)((long)$1->dval ^ (long)$3->dval), $1->type); delete $3; }
    ;
bitwise_and_expression
    : equality_expression { $$ = $1; }
    | bitwise_and_expression '&' equality_expression { $$ = new ExprResult((double)((long)$1->dval & (long)$3->dval), $1->type); delete $3; }
    ;
equality_expression
    : relational_expression { $$ = $1; }
    | equality_expression EQ_OP relational_expression { $$ = new ExprResult($1->dval == $3->dval, new Type("bool")); delete $1; delete $3;
    }
    | equality_expression NE_OP relational_expression { $$ = new ExprResult($1->dval != $3->dval, new Type("bool")); delete $1;
    delete $3; }
    ;

relational_expression
    : shift_expression { $$ = $1;
    }
    | relational_expression '<' shift_expression { $$ = new ExprResult($1->dval < $3->dval, new Type("bool")); delete $1;
    delete $3; }
    | relational_expression '>' shift_expression { $$ = new ExprResult($1->dval > $3->dval, new Type("bool"));
    delete $1; delete $3; }
    | relational_expression LE_OP shift_expression { $$ = new ExprResult($1->dval <= $3->dval, new Type("bool"));
    delete $1; delete $3; }
    | relational_expression GE_OP shift_expression { $$ = new ExprResult($1->dval >= $3->dval, new Type("bool"));
    delete $1; delete $3; }
    ;

shift_expression
    : additive_expression { $$ = $1;
    }
    | shift_expression LSHIFT_OP additive_expression { $$ = new ExprResult((double)((long)$1->dval << (long)$3->dval), $1->type); delete $3;
    }
    | shift_expression RSHIFT_OP additive_expression { $$ = new ExprResult((double)((long)$1->dval >> (long)$3->dval), $1->type); delete $3;
    }
    ;

additive_expression
    : multiplicative_expression { $$ = $1; }
    | additive_expression '+' multiplicative_expression { $$ = new ExprResult($1->dval + $3->dval, $1->type); delete $3; }
    | additive_expression '-' multiplicative_expression { $$ = new ExprResult($1->dval - $3->dval, $1->type); delete $3; }
    ;
multiplicative_expression
    : cast_expression { $$ = $1; }
    | multiplicative_expression '*' cast_expression { $$ = new ExprResult($1->dval * $3->dval, $1->type); delete $3; }
    | multiplicative_expression '/' cast_expression { $$ = new ExprResult($1->dval / $3->dval, $1->type); delete $3; }
    | multiplicative_expression '%' cast_expression { $$ = new ExprResult((double)((long)$1->dval % (long)$3->dval), $1->type); delete $3; }
    ;
cast_expression
    : unary_expression { $$ = $1; }
    | LPAREN type_name RPAREN cast_expression {
        // Only proceed with cast if type_name is valid
        if ($2->base_type != "error") {
            $$ = $4;
            $$->type = $2;
        } else {
            // This might be a parenthesized expression, backtrack
            YYERROR;
        }
      }
    ;
type_name: type_specifier { $$ = $1; } |
    type_specifier abstract_declarator { $$ = $1; };
abstract_declarator: '*' { $$ = nullptr; } |
    '*' abstract_declarator { $$ = $2; };
unary_expression
    : postfix_expression { $$ = $1;
    }
    | INC_OP unary_expression { $$ = $2; }
    | DEC_OP unary_expression { $$ = $2; }
    | '-' cast_expression %prec SIZEOF { $$ = new ExprResult(-$2->dval, $2->type); }
    | '!' cast_expression { $$ = new ExprResult(!$2->dval, new Type("bool")); delete $2; }
    | '&' cast_expression %prec SIZEOF {
        ExprResult* operand = $2;
        if (!operand->lvalue_symbol) {
            yyerror("lvalue required as unary '&' operand");
            $$ = new ExprResult(0.0, new Type("error"), nullptr);
        } else {
            Type* new_type = new Type(*operand->type);
            new_type->pointer_level++;
            $$ = new ExprResult(0.0, new_type, nullptr); // Result is not an l-value
        }
        delete operand;
    }
    | '*' cast_expression %prec SIZEOF {
        ExprResult* operand = $2;
        if (operand->type->pointer_level == 0 && operand->type->array_dimensions.empty()) {
            yyerror("invalid type argument of unary '*' (have 'int')");
            $$ = new ExprResult(0.0, new Type("error"), nullptr);
        } else {
            Type* new_type = new Type(*operand->type);
            if (new_type->pointer_level > 0) {
                new_type->pointer_level--;
            } else { // It's an array
                new_type->array_dimensions.erase(new_type->array_dimensions.begin());
            }
            // The result of dereferencing is an l-value
            $$ = new ExprResult(operand->lvalue_symbol ? operand->lvalue_symbol->dval : 0.0, new_type, operand->lvalue_symbol);
        }
        delete operand;
    }
    ;
postfix_expression
    : primary_expression { $$ = $1; }
    | postfix_expression LBRACKET expression RBRACKET {
    ExprResult* array_expr = $1;
    ExprResult* index_expr = $3;
    Type* new_type = nullptr;
    Symbol* result_symbol = nullptr;

    if (array_expr->type->pointer_level == 0 && array_expr->type->array_dimensions.empty()) {
        yyerror("subscripted value is neither array nor pointer");
        new_type = new Type("error");
    } else {
        new_type = new Type(*array_expr->type);
        // Handle array bounds checking for the first dimension
        // Only check bounds in non-initializer contexts (when we have an lvalue symbol)
        if (!new_type->array_dimensions.empty() && array_expr->lvalue_symbol) {
            int array_size = new_type->array_dimensions[0];
            // Check if we have a constant expression for bounds checking
            if (index_expr->is_const_expr && array_size >= 0) {
                int index_value = (int)index_expr->dval;
                if (index_value < 0 || index_value >= array_size) {
                    yyerror(("array index " + to_string(index_value) + " out of bounds [0, " + to_string(array_size - 1) + "]").c_str());
                }
            }
        }
        
        // Remove the first dimension
        if (!new_type->array_dimensions.empty()) {
            new_type->array_dimensions.erase(new_type->array_dimensions.begin());
        }
        
        // If no dimensions left and pointer level is 0, it becomes the base type (element access)
        if (new_type->array_dimensions.empty() && new_type->pointer_level == 0) {
            // This is now accessing an individual element - it's an lvalue
            result_symbol = array_expr->lvalue_symbol;
        } else {
            // Still an array or pointer - preserve lvalue if it was one
            result_symbol = array_expr->lvalue_symbol;
        }
    }
    
    $$ = new ExprResult(array_expr->lvalue_symbol ? array_expr->lvalue_symbol->dval : 0.0, new_type, result_symbol);
    delete array_expr;
    delete index_expr;
}
    | postfix_expression DOT identifier {
        // Handle struct member access: obj.member
        ExprResult* struct_expr = $1;
        string member_name = string($3);
        
        if (struct_expr->type->kind != TK_STRUCT && struct_expr->type->kind != TK_UNION) {
            yyerror("member access requires struct or union type");
            $$ = new ExprResult(0.0, new Type("error"), nullptr);
        } else if (struct_expr->type->pointer_level > 0) {
            yyerror("member access requires struct/union type, not pointer to struct/union (use -> instead)");
            $$ = new ExprResult(0.0, new Type("error"), nullptr);
        } else {
            // Look up the member in the struct/union
            auto it = struct_expr->type->members.find(member_name);
            if (it == struct_expr->type->members.end()) {
                yyerror(("no member named '" + member_name + "' in struct/union").c_str());
                $$ = new ExprResult(0.0, new Type("error"), nullptr);
            } else {
                Symbol* member_sym = it->second;
                // The result is an lvalue if the struct expression was an lvalue
                Symbol* result_sym = struct_expr->lvalue_symbol ? member_sym : nullptr;
                $$ = new ExprResult(member_sym->dval, member_sym->type, result_sym);
            }
        }
        delete struct_expr;
        free($3);
    }
    | postfix_expression PTR_OP identifier {
        // Handle struct pointer member access: ptr->member
        ExprResult* ptr_expr = $1;
        string member_name = string($3);
        
        // Check if it's a pointer to struct/union
        if (ptr_expr->type->pointer_level == 0 || 
            (ptr_expr->type->kind != TK_STRUCT && ptr_expr->type->kind != TK_UNION)) {
            yyerror("pointer member access requires pointer to struct or union");
            $$ = new ExprResult(0.0, new Type("error"), nullptr);
        } else {
            // Get the base type (the struct/union being pointed to)
            Type* base_type = new Type(*ptr_expr->type);
            base_type->pointer_level--;
            
            // Look up the member in the struct/union
            auto it = base_type->members.find(member_name);
            if (it == base_type->members.end()) {
                yyerror(("no member named '" + member_name + "' in struct/union").c_str());
                $$ = new ExprResult(0.0, new Type("error"), nullptr);
            } else {
                Symbol* member_sym = it->second;
                // ptr->member is equivalent to (*ptr).member, so it's an lvalue
                $$ = new ExprResult(member_sym->dval, member_sym->type, member_sym);
            }
            delete base_type;
        }
        delete ptr_expr;
        free($3);
    }
    | postfix_expression LPAREN argument_list RPAREN {
        ExprResult* func_expr = $1;
        vector<ExprResult*>* args = $3;
        Symbol* func_sym = lookup_symbol(func_expr->lvalue_symbol->name);

        if (!func_sym || func_sym->kind != SK_FUNCTION) {
            yyerror(("'" + func_expr->lvalue_symbol->name + "' is not a function").c_str());
        } else {
            vector<Type*>& param_types = func_sym->type->parameter_types;
            if (args->size() != param_types.size()) {
                yyerror(("Wrong number of arguments to function '" + func_sym->name + "'").c_str());
            } else {
                for (size_t i = 0; i < args->size(); ++i) {
                    if ((*args)[i]->type->base_type != param_types[i]->base_type) {
                        yyerror(("Type mismatch for argument " + to_string(i+1) + " in call to '" + func_sym->name + "'").c_str());
                    }
                }
            }
        }
        for(auto arg: *args) delete arg;
        delete args;
        $$ = new ExprResult(0.0, func_sym->type->return_type, nullptr);
        delete func_expr;
      }
    | postfix_expression LPAREN RPAREN {
        ExprResult* func_expr = $1;
        Symbol* func_sym = lookup_symbol(func_expr->lvalue_symbol->name);
        if (!func_sym || func_sym->kind != SK_FUNCTION) {
            yyerror(("'" + func_expr->lvalue_symbol->name + "' is not a function").c_str());
        } else {
            if (func_sym->type->parameter_types.size() != 0) {
                yyerror(("Wrong number of arguments to function '" + func_sym->name + "'").c_str());
            }
        }
        $$ = new ExprResult(0.0, func_sym->type->return_type, nullptr);
        delete func_expr;
    }
    | postfix_expression INC_OP { $$ = $1; }
    | postfix_expression DEC_OP { $$ = $1; }
    ;
primary_expression
    : identifier {
        Symbol* sym = lookup_symbol($1);
        if (!sym) {
            yyerror(("Undeclared identifier '" + string($1) + "'").c_str());
            $$ = new ExprResult(0, new Type("error"), nullptr);
        } else {
            $$ = new ExprResult(sym->dval, sym->type, sym);
        }
        free($1);
    }
    | INT_LITERAL    { $$ = new ExprResult($1, new Type("int"), nullptr); $$->is_const_expr = true; }
    | FLOAT_LITERAL  { $$ = new ExprResult($1, new Type("float"), nullptr); $$->is_const_expr = true; }
    | CHAR_LITERAL   { $$ = new ExprResult(0, new Type("char"), nullptr); $$->is_const_expr = true; }
    | STRING_LITERAL { $$ = new ExprResult(0, new Type("char*"), nullptr); }
    | LPAREN expression RPAREN { $$ = $2; }
    ;
parameter_list
    : parameter_declaration { $$ = new vector<Symbol*>(); $$->push_back($1); }
    | parameter_list COMMA parameter_declaration { $1->push_back($3); $$ = $1; }
    ;
parameter_declaration
    : declaration_specifiers declarator {
        Symbol* sym = $2;
        Type* base_type = $1;
        // Merge the base type info into the symbol's type, which has pointer/array info.
        sym->type->base_type = base_type->base_type;
        sym->type->kind = base_type->kind;
        sym->type->is_const = base_type->is_const || sym->type->is_const;
        if (base_type->kind == TK_STRUCT || base_type->kind == TK_UNION) {
            sym->type->members = base_type->members;
        }
        delete base_type;
        $$ = sym;
    }
    ;
argument_list
    : assignment_expression { $$ = new vector<ExprResult*>(); $$->push_back($1); }
    | argument_list COMMA assignment_expression { $1->push_back($3); $$ = $1; }
    ;
initializer_list
    : LBRACE initializer_items RBRACE { $$ = $2; }
    | LBRACE RBRACE { $$ = new std::vector<InitializerItem*>(); }
    ;

initializer_items
    : initializer { $$ = new std::vector<InitializerItem*>(); $$->push_back($1); }
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
