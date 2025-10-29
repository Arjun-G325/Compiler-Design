# Compiler-Design
Toy compiler for a C-like language.

Prerequisites:

flex

bison

spim
    
To use:

Ensure that Code_generator,Lexer and Parser folders all are present in system

Execute run.sh in Code_generator folder

This will give .asm files for each test.c file

Then run spim -f "file_name" for each assembly file to execute it
