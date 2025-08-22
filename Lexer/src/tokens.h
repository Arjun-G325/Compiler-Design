#ifndef TOKENS_H
#define TOKENS_H

// These names will be declared properly in parser.y via %token.
// This file is just a central list to avoid typos.

#define IDENTIFIER     258
#define INT_LITERAL    259
#define FLOAT_LITERAL  260
#define STRING_LITERAL 261
#define CHAR_LITERAL   262

#define IF 263
#define ELSE 264
#define FOR 265
#define WHILE 266
#define RETURN 267
#define GOTO 268
#define BREAK 269
#define CONTINUE 270
#define SWITCH 271
#define CASE 272
#define DEFAULT 273
#define STRUCT 274
#define TYPEDEF 275
#define STATIC 276
#define CONST 277
#define AUTO 278
#define CLASS 279
#define PRIVATE 280
#define PROTECTED 281
#define PUBLIC 282
#define LAMBDA 283

#define INT 284
#define CHAR 285
#define FLOAT 286
#define DOUBLE 287
#define VOID 288
#define SIGNED_INT 289
#define UNSIGNED_INT 290

#define PLUS 291
#define MINUS 292
#define MUL 293
#define DIV 294
#define MOD 295
#define EQ 296
#define NEQ 297
#define LT 298
#define LEQ 299
#define GT 300
#define GEQ 301
#define AND 302
#define OR 303
#define NOT 304
#define BIT_AND 305
#define BIT_OR 306
#define BIT_XOR 307
#define BIT_NOT 308
#define INC 309
#define DEC 310
#define ASSIGN 311

#define LPAREN 312
#define RPAREN 313
#define LBRACE 314
#define RBRACE 315
#define LBRACKET 316
#define RBRACKET 317
#define COLON 318
#define SEMICOLON 319
#define COMMA 320
#define ERROR 321

#endif
