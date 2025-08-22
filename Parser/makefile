CXX = g++
FLEX = flex
YACC = yacc

PARSER_SRC = src/parser.y
LEXER_SRC = ../Lexer/src/lexer.l

PARSER_C = src/y.tab.c
PARSER_H = src/y.tab.h
LEXER_C = src/lex.yy.c

TARGET = parser

parser: $(PARSER_C) $(LEXER_C)
	$(CXX) $(PARSER_C) $(LEXER_C) -o $(TARGET)

$(PARSER_C) $(PARSER_H): $(PARSER_SRC)
	cd src && $(YACC) -d parser.y

$(LEXER_C): $(LEXER_SRC) $(PARSER_H)
	$(FLEX) -o $(LEXER_C) $(LEXER_SRC)

clean:
	rm -f $(TARGET) $(PARSER_C) $(PARSER_H) $(LEXER_C)

