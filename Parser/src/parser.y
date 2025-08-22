%{
#include <iostream>
#include <cstdlib>

int yylex(void);
void yyerror(const char *s) {
    std::cerr << "Parse error: " << s << std::endl;
}
%}

%token DUMMY

%start program

%%
program:
    ;
%%

