FLEX = flex
CXX = g++
CXXFLAGS  = -Wall -Wno-unused-function -O2

SRC_DIR   = src
LEXER_SRC = $(SRC_DIR)/lexer.l
LEXER_GEN = $(SRC_DIR)/lex.yy.c
LEXER_BIN = $(SRC_DIR)/lexer

.PHONY: lexer lexer-clean

lexer:
	@if [ -f "$(LEXER_BIN)" ] && [ "$(LEXER_SRC)" -ot "$(LEXER_BIN)" ]; then \
		echo "Lexer is already built"; \
	else \
		$(FLEX) --nounput -o $(LEXER_GEN) $(LEXER_SRC) && \
		$(CXX) $(CXXFLAGS) $(LEXER_GEN) -o $(LEXER_BIN); \
	fi

lexer-clean:
	rm -f $(LEXER_GEN) $(LEXER_BIN)

