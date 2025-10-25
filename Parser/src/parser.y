%code requires {
#include <string>
#include <vector>
#include <map>
#include <stack>
#include <iostream>
#include <fstream>
#include <cmath>

class TACGenerator {
private:
    std::ofstream outfile;
    int temp_count;
    int label_count;
    
public:
    TACGenerator(const std::string& filename);
    ~TACGenerator();
    
    // Temporary variable generation
    std::string newTemp();
    std::string newIntTemp();
    std::string newFloatTemp();
    
    // Label generation
    std::string newLabel();
    
    // Basic TAC operations with type awareness
    void emit(const std::string& result, const std::string& arg1, const std::string& op, const std::string& arg2);
    void emitIntOp(const std::string& result, const std::string& arg1, const std::string& op, const std::string& arg2);
    void emitFloatOp(const std::string& result, const std::string& arg1, const std::string& op, const std::string& arg2);
    void emitAssignment(const std::string& result, const std::string& value);
    void emitIntAssignment(const std::string& result, const std::string& value);
    void emitFloatAssignment(const std::string& result, const std::string& value);
    void emitUnary(const std::string& result, const std::string& op, const std::string& arg);
    void emitIntUnary(const std::string& result, const std::string& op, const std::string& arg);
    void emitFloatUnary(const std::string& result, const std::string& op, const std::string& arg);
    
    // Control flow
    void emitLabel(const std::string& label);
    void emitGoto(const std::string& label);
    void emitIfGoto(const std::string& condition, const std::string& label);
    void emitIfFalseGoto(const std::string& condition, const std::string& label);
    
    // Function operations
    void emitParam(const std::string& param);
    void emitCall(const std::string& result, const std::string& function, int num_params);
    void emitReturn(const std::string& value);
    void emitReturnVoid();
    void emitFunctionBegin(const std::string& function_name);
    void emitFunctionEnd(const std::string& function_name);
    
    // Array operations
    void emitArrayAccess(const std::string& result, const std::string& array, const std::string& index);
    void emitArrayStore(const std::string& array, const std::string& index, const std::string& value);
    
    // Pointer operations
    void emitAddressOf(const std::string& result, const std::string& var);
    void emitDereference(const std::string& result, const std::string& ptr);
    void emitPointerStore(const std::string& ptr, const std::string& value);
    
    // Structure operations
    void emitMemberAccess(const std::string& result, const std::string& struct_var, const std::string& member);
    void emitPointerMemberAccess(const std::string& result, const std::string& struct_ptr, const std::string& member);
    
    // Type casting
    void emitCast(const std::string& result, const std::string& value, const std::string& type);
    void emitIntToFloat(const std::string& result, const std::string& int_val);
    void emitFloatToInt(const std::string& result, const std::string& float_val);
    
    void emitComment(const std::string& comment);
    
    void emitRaw(const std::string& instruction);
    
    // Helper function to determine operation type
    std::string getOperationType(const std::string& type1, const std::string& type2);
};

extern TACGenerator* tac_gen;

struct Type;
struct Symbol;
struct ExprResult;
struct InitializerItem;
struct MemberDef;

typedef std::vector<Symbol*> SymbolList;
typedef std::vector<ExprResult*> ExprList;
typedef std::vector<InitializerItem*> InitializerList;
typedef std::vector<MemberDef*> MemberDefList;

    // Enum for access specifiers
    enum AccessSpecifier { AS_PUBLIC, AS_PRIVATE, AS_PROTECTED };

struct ExprResult {
        double dval;
        Type* type;
        Symbol* lvalue_symbol;
        bool is_const_expr;
        std::string tac_var;  // TAC variable name
        std::string string_val;
ExprResult(double v = 0.0, Type* t = nullptr, Symbol* s = nullptr)
            : dval(v), type(t), lvalue_symbol(s), is_const_expr(false), tac_var("") {}
    };

struct InitializerItem {
        bool is_list;
        ExprResult* expr;
        std::vector<InitializerItem*>* list;

InitializerItem(ExprResult* e = nullptr) : is_list(false), expr(e), list(nullptr) {}
        InitializerItem(std::vector<InitializerItem*>* l) : is_list(true), expr(nullptr), list(l) {}

        ~InitializerItem() {
    if (is_list && list) {
        for (auto item : *list) {
            delete item;
        }
        delete list;
    } else if (!is_list) {
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
    int getSize() const;
    bool isFloatType() const { return base_type == "float" || base_type == "double"; }
    bool isIntType() const { return base_type == "int" || base_type == "char" || base_type == "short" || base_type == "long" || base_type == "bool"; }
};

struct Symbol {
        std::string name;
        Type* type;
        SymbolKind kind;
        double dval;
        bool has_return;
        AccessSpecifier access;
        std::string init_expr;
        bool has_initializer = false;
    };
  
   inline int Type::getSize() const {
        int base_size = 0;
        
        // Base type sizes
        if (base_type == "char") base_size = 1;
        else if (base_type == "short") base_size = 2;
        else if (base_type == "int") base_size = 4;
        else if (base_type == "long") base_size = 8;
        else if (base_type == "float") base_size = 4;
        else if (base_type == "double") base_size = 8;
        else if (base_type == "void") base_size = 0;
        else if (kind == TK_STRUCT || kind == TK_CLASS) {
            // Calculate struct/class size (sum of all members)
            for (const auto& member : members) {
                base_size += member.second->type->getSize();
            }
        }
        else if (kind == TK_UNION) {
            // Calculate union size (max of all members)
            for (const auto& member : members) {
                int member_size = member.second->type->getSize();
                if (member_size > base_size) base_size = member_size;
            }
        }
        else if (kind == TK_ENUM) {
            base_size = 4; // Enums are typically int-sized
        }
        else if (kind == TK_FUNCTION) {
            return 8; // Function pointers
        }
        
        // Pointer types are always pointer-sized (8 bytes on 64-bit)
        if (pointer_level > 0) {
            return 8;
        }
        
        // Array dimensions multiply the size
        int total_size = base_size;
        for (int dim : array_dimensions) {
            if (dim > 0) total_size *= dim;
        }
        
        return total_size;
    }

    struct MemberDef {
        std::string name;
Type* type;
        AccessSpecifier access;
        SymbolKind kind;
void* definition;
    };
      
    // For control flow TAC generation
    struct ControlLabels {
        std::string break_label;
        std::string continue_label;
        std::string cond_label;   
        std::string body_label;    
        ExprResult* incr_expr;
    };

    struct ForLoopLabels {
        std::string cond_label;
        std::string body_label; 
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
AccessSpecifier g_current_access_specifier = AS_PRIVATE;
Type* g_current_class_scope = nullptr;
bool g_parsing_lambda = false;
Type* g_inferred_return_type = nullptr;

// Control flow label stack
stack<ControlLabels> control_stack;
std::stack<std::pair<string, string>> if_stack;

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

std::vector<Type*> get_printf_specifier_types(const std::string& format_string) {
    std::vector<Type*> types;
    for (size_t i = 0; i < format_string.length(); ++i) {
        if (format_string[i] == '%') {
            if (i + 1 < format_string.length()) {
                if (format_string[i+1] == '%') {
                    i++; // Skip '%%'
                    continue;
                }
                
                // Check for %lf
                if (i + 2 < format_string.length() && format_string.substr(i+1, 2) == "lf") {
                    types.push_back(new Type("double")); // %lf expects double
                    i += 2;
                }
                // Check for %d
                else if (format_string[i+1] == 'd') {
                    types.push_back(new Type("int"));
                    i++;
                }
                // Check for %f
                else if (format_string[i+1] == 'f') {
                    // Note: C promotes floats to doubles in variadic functions
                    types.push_back(new Type("double")); 
                    i++;
                }
                // Check for %s
                else if (format_string[i+1] == 's') {
                    Type* t = new Type("char");
                    t->pointer_level = 1; // %s expects char*
                    types.push_back(t);
                    i++;
                }
                // Check for %p
                else if (format_string[i+1] == 'p') {
                    Type* t = new Type("void");
                    t->pointer_level = 1; // %p expects void*
                    types.push_back(t);
                    i++;
                }
            }
        }
    }
    return types;
}

/**
 * @brief Parses a scanf format string and returns a vector of expected pointer types.
 */
std::vector<Type*> get_scanf_specifier_types(const std::string& format_string) {
    std::vector<Type*> types;
    for (size_t i = 0; i < format_string.length(); ++i) {
        if (format_string[i] == '%') {
            if (i + 1 < format_string.length()) {
                if (format_string[i+1] == '%') {
                    i++; // Skip '%%'
                    continue;
                }

                // Check for %lf
                if (i + 2 < format_string.length() && format_string.substr(i+1, 2) == "lf") {
                    Type* t = new Type("double");
                    t->pointer_level = 1; // %lf expects double*
                    types.push_back(t);
                    i += 2;
                }
                // Check for %d
                else if (format_string[i+1] == 'd') {
                    Type* t = new Type("int");
                    t->pointer_level = 1; // %d expects int*
                    types.push_back(t);
                    i++;
                }
                // Check for %f
                else if (format_string[i+1] == 'f') {
                    Type* t = new Type("float"); // %f expects float* (no promotion)
                    t->pointer_level = 1;
                    types.push_back(t);
                    i++;
                }
                // Check for %s
                else if (format_string[i+1] == 's') {
                    Type* t = new Type("char");
                    t->pointer_level = 1; // %s expects char*
                    types.push_back(t);
                    i++;
                }
                // Check for %p
                else if (format_string[i+1] == 'p') {
                    Type* t = new Type("void");
                    t->pointer_level = 2; // %p expects void**
                    types.push_back(t);
                    i++;
                }
            }
        }
    }
    return types;
}


/**
 * @brief Performs semantic checks and TAC generation for function calls.
 */
ExprResult* handle_function_call(ExprResult* func_expr, ExprList* args) {
    std::string func_name = func_expr->tac_var;
    int num_params = args->size();
    ExprResult* result = new ExprResult();

    // --- Special Handling for printf ---
    if (func_name == "printf") {
        if (num_params == 0) {
            yyerror("printf requires at least one argument (the format string)");
        } else {
            ExprResult* format_arg = args->at(0);
            if (!format_arg->string_val.empty()) {
                // Format string is a literal, we can check types
                std::string format_string = format_arg->string_val;
                std::vector<Type*> expected_types = get_printf_specifier_types(format_string);
                int num_specifiers = expected_types.size();
                int num_provided_args = num_params - 1;

                if (num_specifiers != num_provided_args) {
                    std::string err = "printf: " + std::to_string(num_specifiers) + 
                                      " format specifiers given, but " + 
                                      std::to_string(num_provided_args) + " arguments provided";
                    yyerror(err.c_str());
                } else {
                    // Counts match, now check types
                    for (int i = 0; i < num_specifiers; ++i) {
                        Type* expected = expected_types[i];
                        Type* provided = args->at(i + 1)->type;
                        bool type_mismatch = false;

                        // Check base type (with promotion rules)
                        if (expected->base_type != provided->base_type) {
                            // %d (int) can take char/short (promoted to int)
                            if (expected->base_type == "int" && (provided->base_type == "char" || provided->base_type == "short")) {
                                // OK
                            }
                            // %f/%lf (double) can take float (promoted to double)
                            else if (expected->base_type == "double" && provided->base_type == "float") {
                                // OK
                            } else {
                                type_mismatch = true;
                            }
                        }
                        
                        // Check pointer level
                        if (expected->pointer_level != provided->pointer_level) {
                            // %p (void*) can take any pointer
                            if (expected->base_type == "void" && expected->pointer_level == 1 && provided->pointer_level > 0) {
                                // OK
                            }
                            // %s (char*) can take (const char*)
                            else if (expected->base_type == "char" && expected->pointer_level == 1 &&
                                     provided->base_type == "char" && provided->pointer_level == 1) {
                                // OK (ignoring const)
                            }
                            else {
                                type_mismatch = true;
                            }
                        }

                        if (type_mismatch) {
                            std::string err = "printf: type mismatch for argument " + std::to_string(i + 2);
                            yyerror(err.c_str());
                        }
                        
                        delete expected; // Clean up the type we created
                    }
                }
            }
            // else: Format string is a variable, we can't check types at compile time.
        }
        result->type = new Type("int"); // printf returns an int
    }
    
    // --- Special Handling for scanf ---
    else if (func_name == "scanf") {
        if (num_params == 0) {
            yyerror("scanf requires at least one argument (the format string)");
        } else {
            ExprResult* format_arg = args->at(0);
            if (!format_arg->string_val.empty()) {
                // Format string is a literal, check types
                std::string format_string = format_arg->string_val;
                std::vector<Type*> expected_types = get_scanf_specifier_types(format_string);
                int num_specifiers = expected_types.size();
                int num_provided_args = num_params - 1;

                if (num_specifiers != num_provided_args) {
                    std::string err = "scanf: " + std::to_string(num_specifiers) + 
                                      " format specifiers given, but " + 
                                      std::to_string(num_provided_args) + " arguments provided";
                    yyerror(err.c_str());
                } else {
                    // Counts match, now check types
                    for (int i = 0; i < num_specifiers; ++i) {
                        Type* expected = expected_types[i];
                        Type* provided = args->at(i + 1)->type;

                        // scanf requires exact pointer type matches
                        bool type_mismatch = (expected->base_type != provided->base_type) ||
                                             (expected->pointer_level != provided->pointer_level);

                        if (type_mismatch) {
                            std::string err = "scanf: type mismatch for argument " + std::to_string(i + 2) +
                                              ". Argument must be a pointer matching the specifier.";
                            yyerror(err.c_str());
                        }
                        
                        delete expected; // Clean up
                    }
                }
            } else {
                // Can't check types, but at least check that they are all pointers
                for (int i = 1; i < num_params; ++i) {
                    if (args->at(i)->type->pointer_level == 0) {
                        yyerror("scanf: argument after format string must be a pointer");
                    }
                }
            }
        }
        result->type = new Type("int"); // scanf returns an int
    }
    
    // --- Special Handling for malloc ---
    else if (func_name == "malloc") {
        if (num_params != 1) {
            yyerror("malloc: takes exactly one argument");
        }
        // TODO: check that argument is an integer type
        result->type = new Type("void"); // malloc returns void*
        result->type->pointer_level = 1;
    }

    // --- Special Handling for free ---
    else if (func_name == "free") {
        if (num_params != 1) {
            yyerror("free: takes exactly one argument");
        }
        if (args->at(0)->type->pointer_level == 0) {
             yyerror("free: argument must be a pointer");
        }
        result->type = new Type("void"); // free returns void
    }

    // --- Default Function Call Handling ---
    else {
        Symbol* func_sym = lookup_symbol(func_name);
        if (func_sym && func_sym->kind == SK_FUNCTION) {
            // Found symbol, check parameters
            if (func_sym->type->parameter_types.size() != (size_t)num_params) {
                std::string err = "function '" + func_name + "': incorrect number of arguments (expected " +
                                  std::to_string(func_sym->type->parameter_types.size()) +
                                  ", got " + std::to_string(num_params) + ")";
                yyerror(err.c_str());
            }
            // TODO: Add type checking for each parameter
            result->type = new Type(*func_sym->type->return_type);
        } else {
            // Unknown function (e.g., implicit declaration)
            // Default to returning int per C89 rules
            result->type = new Type("int");
        }
    }

    // --- TAC Generation (Common to all calls) ---
    
    // 1. Emit parameters in reverse order (C-style stack push)
    for (auto it = args->rbegin(); it != args->rend(); ++it) {
        tac_gen->emitParam((*it)->tac_var);
    }
    
    // 2. Emit the call
    std::string temp_var;
    if (result->type->isFloatType()) {
        temp_var = tac_gen->newFloatTemp();
    } else {
        temp_var = tac_gen->newIntTemp();
    }
    tac_gen->emitCall(temp_var, func_name, num_params);
    
    // 3. Store the result
    result->tac_var = temp_var;
    
    // Clean up argument expressions
    for (ExprResult* arg : *args) {
        delete arg;
    }
    
    return result;
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
    SymbolList* symbol_list_ptr;   
    ExprResult* expr_ptr;
    ExprResult* expr_val;
    ExprList* arg_list_ptr;            
    InitializerItem* initializer_item_ptr;
    InitializerList* initializer_item_list_ptr; 
    MemberDefList* member_def_list_ptr;          
    MemberDef* member_def_ptr;
    AccessSpecifier access_specifier_val;
    std::string* string_ptr;
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
%type <string_ptr> do_while_head

%nonassoc IF_WITHOUT_ELSE
%nonassoc ELSE

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

%start program

%%

program
    : { 
        enter_scope(); 
        tac_gen = new TACGenerator("/dev/stdout");
        tac_gen->emitComment("Three Address Code Generated");
    } external_declaration_list {
        Symbol* main_sym = lookup_symbol("main");
        if (!main_sym || main_sym->kind != SK_FUNCTION) {
            fprintf(stderr, "Error: undefined reference to 'main'\n");
        } else {
            if (main_sym->type->return_type->base_type != "int") {
                fprintf(stderr, "Error: 'main' must return 'int'\n");
            }

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
        delete tac_gen;
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
        tac_gen->emitComment("Class declaration: " + class_name);
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
                member->type = new Type(*base_type);
                member->type->pointer_level = sym->type->pointer_level;
                member->type->array_dimensions = sym->type->array_dimensions;
                member->access = g_current_access_specifier;
                member->kind = SK_VARIABLE;
                members->push_back(member);
                delete sym;
            }
            $$ = members;
            delete symbols;
            delete base_type;
        } else {
            $$ = members;
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
        tac_gen->emitComment("Enum declaration: " + name);
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
        sym->dval = 0;
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
          
          // Function begin
          tac_gen->emitFunctionBegin(func_sym->name);
          
          enter_scope();
          if (g_current_param_list) {
              for (Symbol* p : *g_current_param_list) {
                  install_symbol(p);
                  // Parameter declaration
                  tac_gen->emitComment("Parameter: " + p->name);
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
          
          // Function end
          tac_gen->emitFunctionEnd(g_current_function->name);
          
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
                if (g_current_base_type->base_type != "auto") {
                    sym->type->base_type = g_current_base_type->base_type;
                    sym->type->kind = g_current_base_type->kind;
                    sym->type->is_const = g_current_base_type->is_const || sym->type->is_const;
                    sym->type->is_unsigned = g_current_base_type->is_unsigned;
                    if (g_current_base_type->kind == TK_STRUCT || g_current_base_type->kind == TK_UNION || g_current_base_type->kind == TK_CLASS) {
                        sym->type->members = g_current_base_type->members;
                    }
                }
                
                // Emit with size information
                int size = sym->type->getSize();
                tac_gen->emitComment("Variable declaration: " + sym->name + " : " + 
                                    sym->type->toString() + " (size: " + to_string(size) + " bytes)");
                
                if (sym->has_initializer && !sym->init_expr.empty()) {
                    if (sym->type->isFloatType()) {
                        tac_gen->emitFloatAssignment(sym->name, sym->init_expr);
                    } else {
                        tac_gen->emitIntAssignment(sym->name, sym->init_expr);
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
            sym->type = expr->type;
            sym->kind = SK_FUNCTION;
        } else {
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
    
    // Store the initializer expression to emit later
    sym->init_expr = expr->tac_var;
    sym->has_initializer = true;
    
    // Emit assignment with proper type annotation
    if (sym->type->isFloatType()) {
        if (expr->type->isFloatType()) {
            tac_gen->emitFloatAssignment(sym->name, expr->tac_var);
        } else {
            // Float variable assigned with int value - convert
            string temp = tac_gen->newFloatTemp();
            tac_gen->emitIntToFloat(temp, expr->tac_var);
            tac_gen->emitFloatAssignment(sym->name, temp);
        }
    } else {
        if (expr->type->isFloatType()) {
            // Int variable assigned with float value - convert and truncate
            string temp = tac_gen->newIntTemp();
            tac_gen->emitFloatToInt(temp, expr->tac_var);
            tac_gen->emitIntAssignment(sym->name, temp);
        } else {
            tac_gen->emitIntAssignment(sym->name, expr->tac_var);
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
        
        tac_gen->emitComment("Variable declaration: " + sym->name + " : " + sym->type->toString());
        tac_gen->emitComment("Array initialization: " + sym->name);
        int index = 0;
        for (auto item : *initializers) {
            if (!item->is_list && item->expr && !item->expr->tac_var.empty()) {
                tac_gen->emitArrayStore(sym->name, to_string(index), item->expr->tac_var);
            }
            index++;
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
        if ($1->type->pointer_level > 0) {
            $1->type->kind = TK_FUNCTION;
            $1->type->return_type = new Type(*$1->type);
            $1->type->return_type->pointer_level--;
            $1->type->base_type = $1->type->return_type->base_type;
            
            if ($3) {
                for (Symbol* param : *$3) {
                    $1->type->parameter_types.push_back(param->type);
                }
                delete $3;
            }
            $$ = $1;
        } else {
            $$ = $1;
            g_current_param_list = $3;
        }
    }
    | declarator LPAREN RPAREN {
        if ($1->type->pointer_level > 0) {
            $1->type->kind = TK_FUNCTION;
            $1->type->return_type = new Type(*$1->type);
            $1->type->return_type->pointer_level--;
            $1->type->base_type = $1->type->return_type->base_type;
            $$ = $1;
        } else {
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
        tac_gen->emitComment("Struct declaration: " + name);
        Type* new_type = new Type(name, TK_STRUCT);
        for (Symbol* member : *$4) {
            new_type->members[member->name] = member;
        }
        install_type(name, new_type);
        free($2);
        delete $4;
    }
    | STRUCT identifier SEMICOLON
    {
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
        tac_gen->emitComment("Union declaration: " + name);
        Type* new_type = new Type(name, TK_UNION);
        for (Symbol* member : *$4) {
            new_type->members[member->name] = member;
        }
        install_type(name, new_type);
        free($2);
        delete $4;
    }
    | UNION identifier SEMICOLON
    {
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
            tac_gen->emitComment("Typedef: " + sym->name);
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
                $$ = new Type(*t);
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
            $$ = new Type(name, TK_UNION);
            install_type(name, $$);
        }
        free($2);
    }
    | STRUCT LBRACE struct_declaration_list RBRACE
    {
        $$ = new Type("", TK_STRUCT);
        for (Symbol* member : *$3) {
            $$->members[member->name] = member;
        }
        delete $3;
    }
    | UNION LBRACE struct_declaration_list RBRACE
    {
        $$ = new Type("", TK_UNION);
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
   : IF LPAREN expression RPAREN
      {
          ExprResult* cond = $3;
          string false_label = tac_gen->newLabel();
          
          if_stack.push({false_label, ""});
          
          if (!cond->tac_var.empty()) {
              tac_gen->emitIfFalseGoto(cond->tac_var, false_label);
          }
          delete cond;
      }
      statement
      else_handler
    | SWITCH LPAREN expression RPAREN 
      {
          g_switch_depth++;
          ExprResult* switch_expr = $3;
          string end_label = tac_gen->newLabel();
          control_stack.push({end_label, ""}); 
          tac_gen->emitComment("Switch expression");
          delete switch_expr;
      } 
      statement 
      {
          g_switch_depth--;
          string end_label = control_stack.top().break_label;
          control_stack.pop();
          tac_gen->emitLabel(end_label);
      }
    ;

else_handler:
    /* empty */
    {
        string false_label = if_stack.top().first;
        if_stack.pop();
        tac_gen->emitLabel(false_label);
    }
    %prec IF_WITHOUT_ELSE
    |
    ELSE
    {
        string false_label = if_stack.top().first;
        if_stack.pop();
        
        string end_label = tac_gen->newLabel();
        
        if_stack.push({false_label, end_label});
        
        tac_gen->emitGoto(end_label);
        tac_gen->emitLabel(false_label);
    }
    statement
    {
        string end_label = if_stack.top().second;
        if_stack.pop();
        tac_gen->emitLabel(end_label);
    }
    ;

iteration_statement
    : WHILE LPAREN {
        string begin_label = tac_gen->newLabel();
        string end_label = tac_gen->newLabel();
        control_stack.push({end_label, begin_label});
        tac_gen->emitLabel(begin_label);
    } expression RPAREN {
        g_loop_depth++;
        ExprResult* cond = $4;
        string end_label = control_stack.top().break_label;
        if (!cond->tac_var.empty()) {
            tac_gen->emitIfFalseGoto(cond->tac_var, end_label);
        }
        delete cond;
    } statement {
        g_loop_depth--;
        string begin_label = control_stack.top().continue_label;
        string end_label = control_stack.top().break_label;
        tac_gen->emitGoto(begin_label);
        tac_gen->emitLabel(end_label);
        control_stack.pop();
    }
    | FOR LPAREN expression_opt SEMICOLON {
        string cond_label = tac_gen->newLabel();
        string end_label = tac_gen->newLabel();
        string continue_label = tac_gen->newLabel();
        string body_label = tac_gen->newLabel();
        
        if ($3) delete $3;
        
        tac_gen->emitLabel(cond_label);
        control_stack.push({end_label, continue_label, cond_label, body_label});
    } expression_opt SEMICOLON {
        ExprResult* cond = $6;
        string end_label = control_stack.top().break_label;
        string body_label = control_stack.top().body_label;
        
        if (cond && !cond->tac_var.empty()) {
            tac_gen->emitIfFalseGoto(cond->tac_var, end_label);
        }
        tac_gen->emitGoto(body_label);
        
        tac_gen->emitLabel(control_stack.top().continue_label);
        delete cond;
    } expression_opt RPAREN {
        g_loop_depth++;
        ExprResult* incr = $9;
        if (incr) delete incr;
        
        string cond_label = control_stack.top().cond_label;
        tac_gen->emitGoto(cond_label);
        
        tac_gen->emitLabel(control_stack.top().body_label);
    } statement {
        g_loop_depth--;
        string continue_label = control_stack.top().continue_label;
        string end_label = control_stack.top().break_label;
        tac_gen->emitGoto(continue_label);
        tac_gen->emitLabel(end_label);
        control_stack.pop();
    }
    | FOR LPAREN declaration {
        string cond_label = tac_gen->newLabel();
        string end_label = tac_gen->newLabel();
        string continue_label = tac_gen->newLabel();
        string body_label = tac_gen->newLabel();
        
        tac_gen->emitLabel(cond_label);
        control_stack.push({end_label, continue_label, cond_label, body_label});
    } expression_opt SEMICOLON {
        ExprResult* cond = $5;
        string end_label = control_stack.top().break_label;
        string body_label = control_stack.top().body_label;
        
        if (cond && !cond->tac_var.empty()) {
            tac_gen->emitIfFalseGoto(cond->tac_var, end_label);
        }
        tac_gen->emitGoto(body_label);
        
        tac_gen->emitLabel(control_stack.top().continue_label);
        delete cond;
    } expression_opt RPAREN {
        g_loop_depth++;
        ExprResult* incr = $8;
        if (incr) delete incr;
        
        string cond_label = control_stack.top().cond_label;
        tac_gen->emitGoto(cond_label);
        
        tac_gen->emitLabel(control_stack.top().body_label);
    } statement {
        g_loop_depth--;
        string continue_label = control_stack.top().continue_label;
        string end_label = control_stack.top().break_label;
        tac_gen->emitGoto(continue_label);
        tac_gen->emitLabel(end_label);
        control_stack.pop();
    }
    | DO do_while_head statement WHILE LPAREN expression RPAREN SEMICOLON {
        g_loop_depth--; 
        std::string* begin_label_ptr = $2; 
        
        ExprResult* cond = $6; 
        string cond_label = control_stack.top().continue_label;
        string end_label = control_stack.top().break_label;
        
        tac_gen->emitLabel(cond_label);
        if (cond && !cond->tac_var.empty()) {
            tac_gen->emitIfGoto(cond->tac_var, *begin_label_ptr);
        }
        tac_gen->emitLabel(end_label);
        control_stack.pop();
        delete cond;
        
        delete begin_label_ptr;
    }
    ;
do_while_head
    : {
        g_loop_depth++;
        string begin_label = tac_gen->newLabel();
        string end_label = tac_gen->newLabel();
        string cond_label = tac_gen->newLabel();
        control_stack.push({end_label, cond_label});
        tac_gen->emitLabel(begin_label);
        
        $$ = new std::string(begin_label);
    }
    ;

jump_statement
    : GOTO identifier SEMICOLON
    {
        string label_name = string($2);
        if (g_labels.find(label_name) == g_labels.end()) {
            g_unresolved_gotos.push_back(label_name);
        }
        tac_gen->emitGoto(label_name);
        free($2);
    }
    | CONTINUE SEMICOLON
    {
        if (g_loop_depth == 0) {
            yyerror("continue statement not within a loop");
        } else {
            tac_gen->emitGoto(control_stack.top().continue_label);
        }
    }
    | BREAK SEMICOLON
    {
        if (g_loop_depth == 0 && g_switch_depth == 0) {
            yyerror("break statement not within loop or switch");
        } else {
            tac_gen->emitGoto(control_stack.top().break_label);
        }
    }
    | RETURN expression_opt SEMICOLON
    {
        if (g_current_function) {
            g_current_function->has_return = true;
            ExprResult* expr = $2;
            if (expr) {
                if (g_current_function->type->return_type->base_type == "void") {
                    yyerror(("function '" + g_current_function->name + "' returns a value, but its return type is void").c_str());
                }
                tac_gen->emitReturn(expr->tac_var);
                delete expr;
            } else {
                if (g_current_function->type->return_type->base_type != "void") {
                     yyerror(("non-void function '" + g_current_function->name + "' should return a value").c_str());
                }
                tac_gen->emitReturnVoid();
            }
        } else {
            yyerror("return statement not within a function");
        }
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
        tac_gen->emitLabel(label_name);
        free($1);
    }
    | CASE conditional_expression ':' statement {
        if (g_switch_depth == 0) {
            yyerror("'case' label not within a switch statement");
        }
        if ($2 && !$2->is_const_expr) {
            yyerror("case label does not reduce to a constant");
        }
        string case_label = tac_gen->newLabel();
        tac_gen->emitLabel(case_label);
        delete $2;
    }
    | DEFAULT ':' statement {
        if (g_switch_depth == 0) {
            yyerror("'default' label not within a switch statement");
        }
        string default_label = tac_gen->newLabel();
        tac_gen->emitLabel(default_label);
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
            
            // Handle type conversion in assignment
            if (lhs->type->isFloatType()) {
                if (rhs->type->isFloatType()) {
                    tac_gen->emitFloatAssignment(lhs->lvalue_symbol->name, rhs->tac_var);
                } else {
                    // Float variable assigned with int value
                    string temp = tac_gen->newFloatTemp();
                    tac_gen->emitIntToFloat(temp, rhs->tac_var);
                    tac_gen->emitFloatAssignment(lhs->lvalue_symbol->name, temp);
                }
            } else {
                if (rhs->type->isFloatType()) {
                    // Int variable assigned with float value
                    string temp = tac_gen->newIntTemp();
                    tac_gen->emitFloatToInt(temp, rhs->tac_var);
                    tac_gen->emitIntAssignment(lhs->lvalue_symbol->name, temp);
                } else {
                    tac_gen->emitIntAssignment(lhs->lvalue_symbol->name, rhs->tac_var);
                }
            }
            
            $$ = new ExprResult(rhs->dval, rhs->type, nullptr);
            $$->tac_var = lhs->lvalue_symbol->name;
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
            
            string temp;
            if (lhs->type->isFloatType() || rhs->type->isFloatType()) {
                temp = tac_gen->newFloatTemp();
                string arg1 = lhs->lvalue_symbol->name;
                string arg2 = rhs->tac_var;
                if (lhs->type->isIntType() && rhs->type->isFloatType()) {
                    string converted = tac_gen->newFloatTemp();
                    tac_gen->emitIntToFloat(converted, lhs->lvalue_symbol->name);
                    arg1 = converted;
                } else if (lhs->type->isFloatType() && rhs->type->isIntType()) {
                    string converted = tac_gen->newFloatTemp();
                    tac_gen->emitIntToFloat(converted, rhs->tac_var);
                    arg2 = converted;
                }
                tac_gen->emitFloatOp(temp, arg1, "+", arg2);
                tac_gen->emitFloatAssignment(lhs->lvalue_symbol->name, temp);
            } else {
                temp = tac_gen->newIntTemp();
                tac_gen->emitIntOp(temp, lhs->lvalue_symbol->name, "+", rhs->tac_var);
                tac_gen->emitIntAssignment(lhs->lvalue_symbol->name, temp);
            }
            
            $$ = new ExprResult(lhs->lvalue_symbol->dval, lhs->type, nullptr);
            $$->tac_var = lhs->lvalue_symbol->name;
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
            
            string temp;
            if (lhs->type->isFloatType() || rhs->type->isFloatType()) {
                temp = tac_gen->newFloatTemp();
                string arg1 = lhs->lvalue_symbol->name;
                string arg2 = rhs->tac_var;
                if (lhs->type->isIntType() && rhs->type->isFloatType()) {
                    string converted = tac_gen->newFloatTemp();
                    tac_gen->emitIntToFloat(converted, lhs->lvalue_symbol->name);
                    arg1 = converted;
                } else if (lhs->type->isFloatType() && rhs->type->isIntType()) {
                    string converted = tac_gen->newFloatTemp();
                    tac_gen->emitIntToFloat(converted, rhs->tac_var);
                    arg2 = converted;
                }
                tac_gen->emitFloatOp(temp, arg1, "-", arg2);
                tac_gen->emitFloatAssignment(lhs->lvalue_symbol->name, temp);
            } else {
                temp = tac_gen->newIntTemp();
                tac_gen->emitIntOp(temp, lhs->lvalue_symbol->name, "-", rhs->tac_var);
                tac_gen->emitIntAssignment(lhs->lvalue_symbol->name, temp);
            }
            
            $$ = new ExprResult(lhs->lvalue_symbol->dval, lhs->type, nullptr);
            $$->tac_var = lhs->lvalue_symbol->name;
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
            
            string temp;
            if (lhs->type->isFloatType() || rhs->type->isFloatType()) {
                temp = tac_gen->newFloatTemp();
                string arg1 = lhs->lvalue_symbol->name;
                string arg2 = rhs->tac_var;
                if (lhs->type->isIntType() && rhs->type->isFloatType()) {
                    string converted = tac_gen->newFloatTemp();
                    tac_gen->emitIntToFloat(converted, lhs->lvalue_symbol->name);
                    arg1 = converted;
                } else if (lhs->type->isFloatType() && rhs->type->isIntType()) {
                    string converted = tac_gen->newFloatTemp();
                    tac_gen->emitIntToFloat(converted, rhs->tac_var);
                    arg2 = converted;
                }
                tac_gen->emitFloatOp(temp, arg1, "*", arg2);
                tac_gen->emitFloatAssignment(lhs->lvalue_symbol->name, temp);
            } else {
                temp = tac_gen->newIntTemp();
                tac_gen->emitIntOp(temp, lhs->lvalue_symbol->name, "*", rhs->tac_var);
                tac_gen->emitIntAssignment(lhs->lvalue_symbol->name, temp);
            }
            
            $$ = new ExprResult(lhs->lvalue_symbol->dval, lhs->type, nullptr);
            $$->tac_var = lhs->lvalue_symbol->name;
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
            
            string temp;
            if (lhs->type->isFloatType() || rhs->type->isFloatType()) {
                temp = tac_gen->newFloatTemp();
                string arg1 = lhs->lvalue_symbol->name;
                string arg2 = rhs->tac_var;
                if (lhs->type->isIntType() && rhs->type->isFloatType()) {
                    string converted = tac_gen->newFloatTemp();
                    tac_gen->emitIntToFloat(converted, lhs->lvalue_symbol->name);
                    arg1 = converted;
                } else if (lhs->type->isFloatType() && rhs->type->isIntType()) {
                    string converted = tac_gen->newFloatTemp();
                    tac_gen->emitIntToFloat(converted, rhs->tac_var);
                    arg2 = converted;
                }
                tac_gen->emitFloatOp(temp, arg1, "/", arg2);
                tac_gen->emitFloatAssignment(lhs->lvalue_symbol->name, temp);
            } else {
                temp = tac_gen->newIntTemp();
                tac_gen->emitIntOp(temp, lhs->lvalue_symbol->name, "/", rhs->tac_var);
                tac_gen->emitIntAssignment(lhs->lvalue_symbol->name, temp);
            }
            
            $$ = new ExprResult(lhs->lvalue_symbol->dval, lhs->type, nullptr);
            $$->tac_var = lhs->lvalue_symbol->name;
        }
        delete lhs;
        delete rhs;
    }
    ;

    conditional_expression
    : logical_or_expression { $$ = $1; }
    | logical_or_expression '?' expression ':' conditional_expression {
        ExprResult* cond = $1;
        ExprResult* true_expr = $3;
        ExprResult* false_expr = $5;
        
        string false_label = tac_gen->newLabel();
        string end_label = tac_gen->newLabel();
        string result_temp;
        
        if (true_expr->type->isFloatType() || false_expr->type->isFloatType()) {
            result_temp = tac_gen->newFloatTemp();
        } else {
            result_temp = tac_gen->newIntTemp();
        }
        
        if (!cond->tac_var.empty()) {
            tac_gen->emitIfFalseGoto(cond->tac_var, false_label);
        }
        
        if (!true_expr->tac_var.empty()) {
            if (true_expr->type->isFloatType()) {
                tac_gen->emitFloatAssignment(result_temp, true_expr->tac_var);
            } else {
                tac_gen->emitIntAssignment(result_temp, true_expr->tac_var);
            }
        }
        tac_gen->emitGoto(end_label);
        
        tac_gen->emitLabel(false_label);
        if (!false_expr->tac_var.empty()) {
            if (false_expr->type->isFloatType()) {
                tac_gen->emitFloatAssignment(result_temp, false_expr->tac_var);
            } else {
                tac_gen->emitIntAssignment(result_temp, false_expr->tac_var);
            }
        }
        tac_gen->emitLabel(end_label);
        
        Type* result_type;
        if (true_expr->type->isFloatType() || false_expr->type->isFloatType()) {
            result_type = new Type("float");
        } else {
            result_type = new Type("int");
        }
        
        $$ = new ExprResult(cond->dval ? true_expr->dval : false_expr->dval, result_type);
        $$->tac_var = result_temp;
        
        delete cond;
        delete true_expr;
        delete false_expr;
    }
    ;

logical_or_expression
    : logical_and_expression { $$ = $1; }
    | logical_or_expression OR_OP logical_and_expression {
        string temp = tac_gen->newIntTemp();
        if (!$1->tac_var.empty() && !$3->tac_var.empty()) {
            tac_gen->emitIntOp(temp, $1->tac_var, "||", $3->tac_var);
        }
        
        $$ = new ExprResult($1->dval || $3->dval, new Type("bool"));
        $$->tac_var = temp;
        delete $1;
        delete $3;
    }
    ;

logical_and_expression
    : bitwise_or_expression { $$ = $1; }
    | logical_and_expression AND_OP bitwise_or_expression {
        string temp = tac_gen->newIntTemp();
        if (!$1->tac_var.empty() && !$3->tac_var.empty()) {
            tac_gen->emitIntOp(temp, $1->tac_var, "&&", $3->tac_var);
        }
        
        $$ = new ExprResult($1->dval && $3->dval, new Type("bool"));
        $$->tac_var = temp;
        delete $1;
        delete $3;
    }
    ;

bitwise_or_expression
    : bitwise_xor_expression { $$ = $1; }
    | bitwise_or_expression '|' bitwise_xor_expression {
        string temp = tac_gen->newIntTemp();
        if (!$1->tac_var.empty() && !$3->tac_var.empty()) {
            tac_gen->emitIntOp(temp, $1->tac_var, "|", $3->tac_var);
        }
        
        $$ = new ExprResult((double)((int)$1->dval | (int)$3->dval), new Type("int"));
        $$->tac_var = temp;
        delete $1;
        delete $3;
    }
    ;

bitwise_xor_expression
    : bitwise_and_expression { $$ = $1; }
    | bitwise_xor_expression '^' bitwise_and_expression {
        string temp = tac_gen->newIntTemp();
        if (!$1->tac_var.empty() && !$3->tac_var.empty()) {
            tac_gen->emitIntOp(temp, $1->tac_var, "^", $3->tac_var);
        }
        
        $$ = new ExprResult((double)((int)$1->dval ^ (int)$3->dval), new Type("int"));
        $$->tac_var = temp;
        delete $1;
        delete $3;
    }
    ;

bitwise_and_expression
    : equality_expression { $$ = $1; }
    | bitwise_and_expression '&' equality_expression {
        string temp = tac_gen->newIntTemp();
        if (!$1->tac_var.empty() && !$3->tac_var.empty()) {
            tac_gen->emitIntOp(temp, $1->tac_var, "&", $3->tac_var);
        }
        
        $$ = new ExprResult((double)((int)$1->dval & (int)$3->dval), new Type("int"));
        $$->tac_var = temp;
        delete $1;
        delete $3;
    }
    ;

equality_expression
    : relational_expression { $$ = $1; }
    | equality_expression EQ_OP relational_expression {
        string temp = tac_gen->newIntTemp();
        if (!$1->tac_var.empty() && !$3->tac_var.empty()) {
            tac_gen->emitIntOp(temp, $1->tac_var, "==", $3->tac_var);
        }
        
        $$ = new ExprResult($1->dval == $3->dval, new Type("bool"));
        $$->tac_var = temp;
        delete $1;
        delete $3;
    }
    | equality_expression NE_OP relational_expression {
        string temp = tac_gen->newIntTemp();
        if (!$1->tac_var.empty() && !$3->tac_var.empty()) {
            tac_gen->emitIntOp(temp, $1->tac_var, "!=", $3->tac_var);
        }
        
        $$ = new ExprResult($1->dval != $3->dval, new Type("bool"));
        $$->tac_var = temp;
        delete $1;
        delete $3;
    }
    ;

relational_expression
    : shift_expression { $$ = $1; }
    | relational_expression '<' shift_expression {
        string temp = tac_gen->newIntTemp();
        if (!$1->tac_var.empty() && !$3->tac_var.empty()) {
            tac_gen->emitIntOp(temp, $1->tac_var, "<", $3->tac_var);
        }
        
        $$ = new ExprResult($1->dval < $3->dval, new Type("bool"));
        $$->tac_var = temp;
        delete $1;
        delete $3;
    }
    | relational_expression '>' shift_expression {
        string temp = tac_gen->newIntTemp();
        if (!$1->tac_var.empty() && !$3->tac_var.empty()) {
            tac_gen->emitIntOp(temp, $1->tac_var, ">", $3->tac_var);
        }
        
        $$ = new ExprResult($1->dval > $3->dval, new Type("bool"));
        $$->tac_var = temp;
        delete $1;
        delete $3;
    }
    | relational_expression LE_OP shift_expression {
        string temp = tac_gen->newIntTemp();
        if (!$1->tac_var.empty() && !$3->tac_var.empty()) {
            tac_gen->emitIntOp(temp, $1->tac_var, "<=", $3->tac_var);
        }
        
        $$ = new ExprResult($1->dval <= $3->dval, new Type("bool"));
        $$->tac_var = temp;
        delete $1;
        delete $3;
    }
    | relational_expression GE_OP shift_expression {
        string temp = tac_gen->newIntTemp();
        if (!$1->tac_var.empty() && !$3->tac_var.empty()) {
            tac_gen->emitIntOp(temp, $1->tac_var, ">=", $3->tac_var);
        }
        
        $$ = new ExprResult($1->dval >= $3->dval, new Type("bool"));
        $$->tac_var = temp;
        delete $1;
        delete $3;
    }
    ;

shift_expression
    : additive_expression { $$ = $1; }
    | shift_expression LSHIFT_OP additive_expression {
        string temp = tac_gen->newIntTemp();
        if (!$1->tac_var.empty() && !$3->tac_var.empty()) {
            tac_gen->emitIntOp(temp, $1->tac_var, "<<", $3->tac_var);
        }
        
        $$ = new ExprResult((double)((int)$1->dval << (int)$3->dval), new Type("int"));
        $$->tac_var = temp;
        delete $1;
        delete $3;
    }
    | shift_expression RSHIFT_OP additive_expression {
        string temp = tac_gen->newIntTemp();
        if (!$1->tac_var.empty() && !$3->tac_var.empty()) {
            tac_gen->emitIntOp(temp, $1->tac_var, ">>", $3->tac_var);
        }
        
        $$ = new ExprResult((double)((int)$1->dval >> (int)$3->dval), new Type("int"));
        $$->tac_var = temp;
        delete $1;
        delete $3;
    }
    ;

additive_expression
    : multiplicative_expression { $$ = $1; }
    | additive_expression '+' multiplicative_expression {
        string temp;
        Type* result_type;
        
        if ($1->type->isFloatType() || $3->type->isFloatType()) {
            temp = tac_gen->newFloatTemp();
            result_type = new Type("float");
            
            string arg1 = $1->tac_var;
            string arg2 = $3->tac_var;
            
            if ($1->type->isIntType() && $3->type->isFloatType()) {
                string converted = tac_gen->newFloatTemp();
                tac_gen->emitIntToFloat(converted, $1->tac_var);
                arg1 = converted;
            } else if ($1->type->isFloatType() && $3->type->isIntType()) {
                string converted = tac_gen->newFloatTemp();
                tac_gen->emitIntToFloat(converted, $3->tac_var);
                arg2 = converted;
            }
            
            tac_gen->emitFloatOp(temp, arg1, "+", arg2);
        } else {
            temp = tac_gen->newIntTemp();
            result_type = new Type("int");
            tac_gen->emitIntOp(temp, $1->tac_var, "+", $3->tac_var);
        }
        
        $$ = new ExprResult($1->dval + $3->dval, result_type);
        $$->tac_var = temp;
        delete $1;
        delete $3;
    }
    | additive_expression '-' multiplicative_expression {
        string temp;
        Type* result_type;
        
        if ($1->type->isFloatType() || $3->type->isFloatType()) {
            temp = tac_gen->newFloatTemp();
            result_type = new Type("float");
            
            string arg1 = $1->tac_var;
            string arg2 = $3->tac_var;
            
            if ($1->type->isIntType() && $3->type->isFloatType()) {
                string converted = tac_gen->newFloatTemp();
                tac_gen->emitIntToFloat(converted, $1->tac_var);
                arg1 = converted;
            } else if ($1->type->isFloatType() && $3->type->isIntType()) {
                string converted = tac_gen->newFloatTemp();
                tac_gen->emitIntToFloat(converted, $3->tac_var);
                arg2 = converted;
            }
            
            tac_gen->emitFloatOp(temp, arg1, "-", arg2);
        } else {
            temp = tac_gen->newIntTemp();
            result_type = new Type("int");
            tac_gen->emitIntOp(temp, $1->tac_var, "-", $3->tac_var);
        }
        
        $$ = new ExprResult($1->dval - $3->dval, result_type);
        $$->tac_var = temp;
        delete $1;
        delete $3;
    }
    ;

multiplicative_expression
    : cast_expression { $$ = $1; }
    | multiplicative_expression '*' cast_expression {
        string temp;
        Type* result_type;
        
        if ($1->type->isFloatType() || $3->type->isFloatType()) {
            temp = tac_gen->newFloatTemp();
            result_type = new Type("float");
            
            string arg1 = $1->tac_var;
            string arg2 = $3->tac_var;
            
            if ($1->type->isIntType() && $3->type->isFloatType()) {
                string converted = tac_gen->newFloatTemp();
                tac_gen->emitIntToFloat(converted, $1->tac_var);
                arg1 = converted;
            } else if ($1->type->isFloatType() && $3->type->isIntType()) {
                string converted = tac_gen->newFloatTemp();
                tac_gen->emitIntToFloat(converted, $3->tac_var);
                arg2 = converted;
            }
            
            tac_gen->emitFloatOp(temp, arg1, "*", arg2);
        } else {
            temp = tac_gen->newIntTemp();
            result_type = new Type("int");
            tac_gen->emitIntOp(temp, $1->tac_var, "*", $3->tac_var);
        }
        
        $$ = new ExprResult($1->dval * $3->dval, result_type);
        $$->tac_var = temp;
        delete $1;
        delete $3;
    }
    | multiplicative_expression '/' cast_expression {
        string temp;
        Type* result_type;
        
        if ($1->type->isFloatType() || $3->type->isFloatType()) {
            temp = tac_gen->newFloatTemp();
            result_type = new Type("float");
            
            string arg1 = $1->tac_var;
            string arg2 = $3->tac_var;
            
            if ($1->type->isIntType() && $3->type->isFloatType()) {
                string converted = tac_gen->newFloatTemp();
                tac_gen->emitIntToFloat(converted, $1->tac_var);
                arg1 = converted;
            } else if ($1->type->isFloatType() && $3->type->isIntType()) {
                string converted = tac_gen->newFloatTemp();
                tac_gen->emitIntToFloat(converted, $3->tac_var);
                arg2 = converted;
            }
            
            tac_gen->emitFloatOp(temp, arg1, "/", arg2);
        } else {
            temp = tac_gen->newIntTemp();
            result_type = new Type("int");
            tac_gen->emitIntOp(temp, $1->tac_var, "/", $3->tac_var);
        }
        
        $$ = new ExprResult($1->dval / $3->dval, result_type);
        $$->tac_var = temp;
        delete $1;
        delete $3;
    }
    | multiplicative_expression '%' cast_expression {
        if ($1->type->isFloatType() || $3->type->isFloatType()) {
            yyerror("invalid operands to binary % (have 'float' and 'float')");
            $$ = new ExprResult(0.0, new Type("int"));
        } else {
            string temp = tac_gen->newIntTemp();
            tac_gen->emitIntOp(temp, $1->tac_var, "%", $3->tac_var);
            
            $$ = new ExprResult(fmod($1->dval, $3->dval), new Type("int"));
            $$->tac_var = temp;
        }
        delete $1;
        delete $3;
    }
    ;

cast_expression
    : unary_expression { $$ = $1; }
    | LPAREN type_name RPAREN cast_expression {
        string temp;
        if ($2->isFloatType()) {
            temp = tac_gen->newFloatTemp();
            if ($4->type->isIntType()) {
                tac_gen->emitIntToFloat(temp, $4->tac_var);
            } else {
                tac_gen->emitFloatAssignment(temp, $4->tac_var);
            }
        } else {
            temp = tac_gen->newIntTemp();
            if ($4->type->isFloatType()) {
                tac_gen->emitFloatToInt(temp, $4->tac_var);
            } else {
                tac_gen->emitIntAssignment(temp, $4->tac_var);
            }
        }
        
        $4->type = $2;
        $4->tac_var = temp;
        $$ = $4;
    }
    ;

unary_expression
    : postfix_expression { $$ = $1; }
    | INC_OP unary_expression {
        if ($2->lvalue_symbol) {
            $2->lvalue_symbol->dval++;
            
            string temp;
            if ($2->type->isFloatType()) {
                temp = tac_gen->newFloatTemp();
                tac_gen->emitFloatOp(temp, $2->lvalue_symbol->name, "+", "1.0");
                tac_gen->emitFloatAssignment($2->lvalue_symbol->name, temp);
            } else {
                temp = tac_gen->newIntTemp();
                tac_gen->emitIntOp(temp, $2->lvalue_symbol->name, "+", "1");
                tac_gen->emitIntAssignment($2->lvalue_symbol->name, temp);
            }
            
            $$ = new ExprResult($2->lvalue_symbol->dval, $2->type, $2->lvalue_symbol);
            $$->tac_var = $2->lvalue_symbol->name;
        } else {
            yyerror("lvalue required for pre-increment");
            $$ = new ExprResult(0.0, nullptr, nullptr);
        }
        delete $2;
    }
    | DEC_OP unary_expression {
        if ($2->lvalue_symbol) {
            $2->lvalue_symbol->dval--;
            
            string temp;
            if ($2->type->isFloatType()) {
                temp = tac_gen->newFloatTemp();
                tac_gen->emitFloatOp(temp, $2->lvalue_symbol->name, "-", "1.0");
                tac_gen->emitFloatAssignment($2->lvalue_symbol->name, temp);
            } else {
                temp = tac_gen->newIntTemp();
                tac_gen->emitIntOp(temp, $2->lvalue_symbol->name, "-", "1");
                tac_gen->emitIntAssignment($2->lvalue_symbol->name, temp);
            }
            
            $$ = new ExprResult($2->lvalue_symbol->dval, $2->type, $2->lvalue_symbol);
            $$->tac_var = $2->lvalue_symbol->name;
        } else {
            yyerror("lvalue required for pre-decrement");
            $$ = new ExprResult(0.0, nullptr, nullptr);
        }
        delete $2;
    }
    | '&' cast_expression {
        string temp = tac_gen->newIntTemp();
        if ($2->lvalue_symbol) {
            tac_gen->emitAddressOf(temp, $2->lvalue_symbol->name);
        }
        
        Type* new_type = new Type(*$2->type);
        new_type->pointer_level++;
        $$ = new ExprResult(0.0, new_type, nullptr);
        $$->tac_var = temp;
        delete $2;
    }
    | '*' cast_expression {
        if ($2->type->pointer_level == 0) {
            yyerror("Cannot dereference a non-pointer type");
            $$ = new ExprResult(0.0, nullptr, nullptr);
        } else {
            string temp;
            Type* base_type = new Type(*$2->type);
            base_type->pointer_level--;
            
            if (base_type->isFloatType()) {
                temp = tac_gen->newFloatTemp();
            } else {
                temp = tac_gen->newIntTemp();
            }
            
            if (!$2->tac_var.empty()) {
                tac_gen->emitDereference(temp, $2->tac_var);
            }
            
            $$ = new ExprResult($2->dval, base_type, $2->lvalue_symbol);
            $$->tac_var = temp;
        }
        delete $2;
    }
    | '+' cast_expression { 
        $$ = $2; 
    }
    | '-' cast_expression { 
        string temp;
        if ($2->type->isFloatType()) {
            temp = tac_gen->newFloatTemp();
            tac_gen->emitFloatUnary(temp, "-", $2->tac_var);
        } else {
            temp = tac_gen->newIntTemp();
            tac_gen->emitIntUnary(temp, "-", $2->tac_var);
        }
        
        $$ = new ExprResult(-$2->dval, $2->type, nullptr); 
        $$->tac_var = temp;
        delete $2; 
    }
    | '~' cast_expression { 
        if ($2->type->isFloatType()) {
            yyerror("invalid operand type for ~ (float)");
            $$ = new ExprResult(0.0, new Type("int"));
        } else {
            string temp = tac_gen->newIntTemp();
            tac_gen->emitIntUnary(temp, "~", $2->tac_var);
            
            $$ = new ExprResult((double)(~((int)$2->dval)), new Type("int")); 
            $$->tac_var = temp;
        }
        delete $2; 
    }
    | '!' cast_expression { 
        string temp = tac_gen->newIntTemp();
        if ($2->type->isFloatType()) {
            tac_gen->emitFloatUnary(temp, "!", $2->tac_var);
        } else {
            tac_gen->emitIntUnary(temp, "!", $2->tac_var);
        }
        
        $$ = new ExprResult(!$2->dval, new Type("bool")); 
        $$->tac_var = temp;
        delete $2; 
    }
    | SIZEOF unary_expression {
        tac_gen->emitComment("sizeof expression");
        $$ = new ExprResult(4.0, new Type("int"), nullptr);
        $$->tac_var = "4";
        delete $2;
    }
    | SIZEOF LPAREN type_name RPAREN {
        tac_gen->emitComment("sizeof(" + $3->toString() + ")");
        $$ = new ExprResult(4.0, new Type("int"), nullptr);
        $$->tac_var = "4";
        delete $3;
    }
    ;

postfix_expression
    : primary_expression { $$ = $1; }
    | postfix_expression LBRACKET expression RBRACKET {
        string temp;
        if ($1->type->isFloatType()) {
            temp = tac_gen->newFloatTemp();
        } else {
            temp = tac_gen->newIntTemp();
        }
        
        if ($1->lvalue_symbol && !$3->tac_var.empty()) {
            tac_gen->emitArrayAccess(temp, $1->lvalue_symbol->name, $3->tac_var);
        }
        
        $$ = $1;
        $$->tac_var = temp;
        delete $3;
    }
| postfix_expression LPAREN RPAREN 
    {
        ExprList* empty_args = new ExprList();
        $$ = handle_function_call($1, empty_args);
        delete $1;
        delete empty_args;
    }
    | postfix_expression LPAREN argument_list RPAREN 
    {
        $$ = handle_function_call($1, $3);
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
            
            string temp;
            if (member_sym->type->isFloatType()) {
                temp = tac_gen->newFloatTemp();
            } else {
                temp = tac_gen->newIntTemp();
            }
            
            if (struct_expr->lvalue_symbol) {
                tac_gen->emitMemberAccess(temp, struct_expr->lvalue_symbol->name, member_name);
            }
            
            $$ = new ExprResult(member_sym->dval, member_sym->type, member_sym);
            $$->tac_var = temp;
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
                
                string temp;
                if (member_sym->type->isFloatType()) {
                    temp = tac_gen->newFloatTemp();
                } else {
                    temp = tac_gen->newIntTemp();
                }
                
                if (!ptr_expr->tac_var.empty()) {
                    tac_gen->emitPointerMemberAccess(temp, ptr_expr->tac_var, member_name);
                }
                
                $$ = new ExprResult(member_sym->dval, member_sym->type, member_sym);
                $$->tac_var = temp;
            }
            delete struct_type;
        }

        delete ptr_expr;
        free($3);
    }
    | postfix_expression INC_OP {
        if ($1->lvalue_symbol) {
            string old_value;
            if ($1->type->isFloatType()) {
                old_value = tac_gen->newFloatTemp();
                tac_gen->emitFloatAssignment(old_value, $1->lvalue_symbol->name);
                
                string temp = tac_gen->newFloatTemp();
                tac_gen->emitFloatOp(temp, $1->lvalue_symbol->name, "+", "1.0");
                tac_gen->emitFloatAssignment($1->lvalue_symbol->name, temp);
            } else {
                old_value = tac_gen->newIntTemp();
                tac_gen->emitIntAssignment(old_value, $1->lvalue_symbol->name);
                
                string temp = tac_gen->newIntTemp();
                tac_gen->emitIntOp(temp, $1->lvalue_symbol->name, "+", "1");
                tac_gen->emitIntAssignment($1->lvalue_symbol->name, temp);
            }
            
            $$ = new ExprResult($1->lvalue_symbol->dval, $1->type, $1->lvalue_symbol);
            $$->tac_var = old_value;
            $1->lvalue_symbol->dval++;
        } else {
            yyerror("lvalue required for post-increment");
            $$ = new ExprResult(0.0, nullptr, nullptr);
        }
        delete $1;
    }
    | postfix_expression DEC_OP {
        if ($1->lvalue_symbol) {
            string old_value;
            if ($1->type->isFloatType()) {
                old_value = tac_gen->newFloatTemp();
                tac_gen->emitFloatAssignment(old_value, $1->lvalue_symbol->name);
                
                string temp = tac_gen->newFloatTemp();
                tac_gen->emitFloatOp(temp, $1->lvalue_symbol->name, "-", "1.0");
                tac_gen->emitFloatAssignment($1->lvalue_symbol->name, temp);
            } else {
                old_value = tac_gen->newIntTemp();
                tac_gen->emitIntAssignment(old_value, $1->lvalue_symbol->name);
                
                string temp = tac_gen->newIntTemp();
                tac_gen->emitIntOp(temp, $1->lvalue_symbol->name, "-", "1");
                tac_gen->emitIntAssignment($1->lvalue_symbol->name, temp);
            }
            
            $$ = new ExprResult($1->lvalue_symbol->dval, $1->type, $1->lvalue_symbol);
            $$->tac_var = old_value;
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
            $$->tac_var = sym->name;
        }
        free($1);
    }
    | INT_LITERAL { 
        $$ = new ExprResult((double)$1, new Type("int")); 
        $$->is_const_expr = true; 
        $$->tac_var = to_string($1);
    }
    | FLOAT_LITERAL { 
        $$ = new ExprResult($1, new Type("float")); 
        $$->is_const_expr = true; 
        $$->tac_var = to_string($1);
    }
    | CHAR_LITERAL { 
        $$ = new ExprResult((double)$1[1], new Type("char")); 
        $$->is_const_expr = true; 
        $$->tac_var = "'" + string(1, $1[1]) + "'";
        free($1); 
    }
    | STRING_LITERAL
    {
        $$ = new ExprResult();
        $$->type = new Type("char", TK_BASE);
        $$->type->pointer_level = 1;
        $$->type->is_const = true;
        
        $$->string_val = std::string($1).substr(1, std::string($1).length() - 2); 
        
        $$->tac_var = tac_gen->newIntTemp(); 
        
        tac_gen->emitAssignment($$->tac_var, std::string($1)); 
        
        free($1);
    }
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
          tac_gen->emitComment("Lambda begin");
      }
      compound_statement
      {
          exit_scope();
          g_parsing_lambda = false;
          tac_gen->emitComment("Lambda end");
          
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
          lambda_sym->name = "__lambda";
          lambda_sym->type = func_type;
          lambda_sym->kind = SK_FUNCTION;
          $$ = new ExprResult(0.0, func_type, lambda_sym);
          $$->tac_var = "__lambda";
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

type_name
    : declaration_specifiers { $$ = $1; }
    | declaration_specifiers abstract_declarator {
        Type* base_type = $1;
        Type* declarator_type = $2;
        declarator_type->base_type = base_type->base_type;
        declarator_type->kind = base_type->kind;
        declarator_type->is_const = base_type->is_const;
        declarator_type->is_unsigned = base_type->is_unsigned;
        if (base_type->kind == TK_STRUCT || base_type->kind ==
        TK_UNION || base_type->kind == TK_CLASS) {
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

%%

TACGenerator* tac_gen = nullptr;

TACGenerator::TACGenerator(const std::string& filename) 
    : temp_count(0), label_count(0) {
    outfile.open(filename);
    if (!outfile.is_open()) {
        std::cerr << "Error: Cannot open TAC output file: " << filename << std::endl;
        exit(1);
    }
}

TACGenerator::~TACGenerator() {
    if (outfile.is_open()) {
        outfile.close();
    }
}

std::string TACGenerator::newTemp() {
    return "t" + std::to_string(temp_count++);
}

std::string TACGenerator::newIntTemp() {
    return "i" + std::to_string(temp_count++) + " [int]";
}

std::string TACGenerator::newFloatTemp() {
    return "f" + std::to_string(temp_count++) + " [float]";
}

std::string TACGenerator::newLabel() {
    return "L" + std::to_string(label_count++);
}

void TACGenerator::emit(const std::string& result, const std::string& arg1, 
                        const std::string& op, const std::string& arg2) {
    outfile << result << " = " << arg1 << " " << op << " " << arg2 << std::endl;
}

void TACGenerator::emitIntOp(const std::string& result, const std::string& arg1, 
                        const std::string& op, const std::string& arg2) {
    outfile << result << " = " << arg1 << " " << op << " " << arg2 << "  [INT]" << std::endl;
}

void TACGenerator::emitFloatOp(const std::string& result, const std::string& arg1, 
                        const std::string& op, const std::string& arg2) {
    outfile << result << " = " << arg1 << " " << op << " " << arg2 << "  [FLOAT]" << std::endl;
}

void TACGenerator::emitAssignment(const std::string& result, const std::string& value) {
    outfile << result << " = " << value << std::endl;
}

void TACGenerator::emitIntAssignment(const std::string& result, const std::string& value) {
    outfile << result << " = " << value << "  [int]" << std::endl;
}

void TACGenerator::emitFloatAssignment(const std::string& result, const std::string& value) {
    outfile << result << " = " << value << "  [float]" << std::endl;
}

void TACGenerator::emitUnary(const std::string& result, const std::string& op, 
                             const std::string& arg) {
    outfile << result << " = " << op << arg << std::endl;
}

void TACGenerator::emitIntUnary(const std::string& result, const std::string& op, 
                             const std::string& arg) {
    outfile << result << " = " << op << arg << "  [INT]" << std::endl;
}

void TACGenerator::emitFloatUnary(const std::string& result, const std::string& op, 
                             const std::string& arg) {
    outfile << result << " = " << op << arg << "  [FLOAT]" << std::endl;
}

void TACGenerator::emitLabel(const std::string& label) {
    outfile << label << ":" << std::endl;
}

void TACGenerator::emitGoto(const std::string& label) {
    outfile << "goto " << label << std::endl;
}

void TACGenerator::emitIfGoto(const std::string& condition, const std::string& label) {
    outfile << "if " << condition << " goto " << label << std::endl;
}

void TACGenerator::emitIfFalseGoto(const std::string& condition, const std::string& label) {
    outfile << "ifFalse " << condition << " goto " << label << std::endl;
}

void TACGenerator::emitParam(const std::string& param) {
    outfile << "param " << param << std::endl;
}

void TACGenerator::emitCall(const std::string& result, const std::string& function, 
                            int num_params) {
    if (result.empty()) {
        outfile << "call " << function << ", " << num_params << std::endl;
    } else {
        outfile << result << " = call " << function << ", " << num_params << std::endl;
    }
}

void TACGenerator::emitReturn(const std::string& value) {
    outfile << "return " << value << std::endl;
}

void TACGenerator::emitReturnVoid() {
    outfile << "return" << std::endl;
}

void TACGenerator::emitFunctionBegin(const std::string& function_name) {
    outfile << "\nBeginFunc " << function_name << std::endl;
}

void TACGenerator::emitFunctionEnd(const std::string& function_name) {
    outfile << "EndFunc " << function_name << "\n" << std::endl;
}

void TACGenerator::emitArrayAccess(const std::string& result, const std::string& array, 
                                   const std::string& index) {
    outfile << result << " = " << array << "[" << index << "]" << std::endl;
}

void TACGenerator::emitArrayStore(const std::string& array, const std::string& index, 
                                  const std::string& value) {
    outfile << array << "[" << index << "] = " << value << std::endl;
}

void TACGenerator::emitAddressOf(const std::string& result, const std::string& var) {
    outfile << result << " = &" << var << std::endl;
}

void TACGenerator::emitDereference(const std::string& result, const std::string& ptr) {
    outfile << result << " = *" << ptr << std::endl;
}

void TACGenerator::emitPointerStore(const std::string& ptr, const std::string& value) {
    outfile << "*" << ptr << " = " << value << std::endl;
}

void TACGenerator::emitMemberAccess(const std::string& result, const std::string& struct_var, 
                                   const std::string& member) {
    outfile << result << " = " << struct_var << "." << member << std::endl;
}

void TACGenerator::emitPointerMemberAccess(const std::string& result, 
                                          const std::string& struct_ptr, 
                                          const std::string& member) {
    outfile << result << " = " << struct_ptr << "->" << member << std::endl;
}

void TACGenerator::emitCast(const std::string& result, const std::string& value, 
                           const std::string& type) {
    outfile << result << " = (" << type << ") " << value << std::endl;
}

void TACGenerator::emitIntToFloat(const std::string& result, const std::string& int_val) {
    outfile << result << " = (float)" << int_val << "  [INT->FLOAT]" << std::endl;
}

void TACGenerator::emitFloatToInt(const std::string& result, const std::string& float_val) {
    outfile << result << " = (int)" << float_val << "  [FLOAT->INT]" << std::endl;
}

void TACGenerator::emitComment(const std::string& comment) {
    outfile << "// " << comment << std::endl;
}

void TACGenerator::emitRaw(const std::string& instruction) {
    outfile << instruction << std::endl;
}

std::string TACGenerator::getOperationType(const std::string& type1, const std::string& type2) {
    if (type1.find("float") != std::string::npos || type2.find("float") != std::string::npos ||
        type1.find("double") != std::string::npos || type2.find("double") != std::string::npos) {
        return "FLOAT";
    }
    return "INT";
}

int main(int argc, char** argv) {
    if (argc > 1) {
        yyin = fopen(argv[1], "r");
        if (!yyin) {
            cerr << "Cannot open file: " << argv[1] << endl;
            return 1;
        }
    }
    yyparse();
    if (tac_gen) {
        delete tac_gen;
    }
    return 0;
}
