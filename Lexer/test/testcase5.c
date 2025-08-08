int 1abc = 10;              // Invalid identifier (starts with digit)
int #var = 5;               // Invalid character
float wrong = 3.14.15;      // Wrong float constant
char bad = 'ab';            // Overlong char literal
char none = '';             // Empty char literal
char esc = '\z';            // Invalid escape sequence
string unterminated = "Oops // Unterminated string
/* unclosed comment

x@ = 10;                    // Invalid character
