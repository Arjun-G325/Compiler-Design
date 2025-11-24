# Compiler-Design
Toy compiler for a C-like language.

Prerequisites:

flex (sudo apt install flex)

bison (sudo apt install bison)

spim (sudo apt install spim)
    
To use:

Ensure that Code_generator,Lexer and Parser folders all are present in system in the same folder

Execute run.sh in Code_generator folder after making it executable using chmod +x run.sh

This will give .asm files for each test.c file

Then run "spim -f file_name.asm" for each assembly file to execute it
