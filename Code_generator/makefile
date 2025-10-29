# Compiler and tools
CXX = g++
CXXFLAGS = -std=c++11 -Wall -Wextra -O2
FLEX = flex
BISON = bison

# Directories
LEXER_DIR = ../Lexer/src
PARSER_DIR = ../Parser/src
CODE_GEN_DIR = ./src

# Source files
LEXER_SRC = $(LEXER_DIR)/lexer.l
PARSER_SRC = $(PARSER_DIR)/parser.y
CODE_GEN_SRC = $(CODE_GEN_DIR)/code_generator.cpp

# Default target
all: lex.yy.c parser.tab.c parser.tab.h parser code_generator

# Build lexer from lexer.l
lex.yy.c: $(LEXER_SRC)
	@echo "[*] Generating lexer from lexer.l..."
	@$(FLEX) -o lex.yy.c $(LEXER_SRC)

# Build parser from parser.y
parser.tab.c parser.tab.h: $(PARSER_SRC)
	@echo "[*] Generating parser from parser.y..."
	@$(BISON) -d -t -v -o parser.tab.c $(PARSER_SRC)

# Build parser binary
parser: lex.yy.c parser.tab.c parser.tab.h
	@echo "[*] Building parser binary..."
	@$(CXX) $(CXXFLAGS) -o parser lex.yy.c parser.tab.c -lfl

# Build code generator (code_generator.cpp is in src/ directory)
code_generator: $(CODE_GEN_SRC)
	@echo "[*] Building code generator..."
	@$(CXX) $(CXXFLAGS) -o code_generator $(CODE_GEN_SRC)

# Clean all generated files
clean:
	@echo "[*] Cleaning all generated files..."
	@rm -f lex.yy.c
	@rm -f parser.tab.c parser.tab.h parser.output
	@rm -f parser
	@rm -f code_generator
	@rm -rf tac_output asm_output

.PHONY: all clean