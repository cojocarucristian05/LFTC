#pragma once

enum Atom {
	ID,

	// keywords
	TYPE_CHAR,
	TYPE_DOUBLE,
	ELSE,
	IF,
	TYPE_INT,
	RETURN,
	STRUCT,
	VOID,
	WHILE,

	// constants
	INT,
	DOUBLE,
	CHAR,
	STRING,
	
	// delimiters
	COMMA,
	SEMICOLON,
	LPAR,
	RPAR,
	LBRACKET,
	RBRACKET,
	LACC,
	RACC,
	END,

	// operators
	ADD,
	SUB,
	MUL,
	DIV,
	DOT,
	AND,
	OR,
	NOT,
	ASSIGN,
	EQUAL,
	NOTEQ,
	LESS,
	LESSEQ,
	GREATER,
	GREATEREQ,

	// space
	SPACE,
	LINECOMMENT,
};

typedef struct Token {
	int code;		// ID, TYPE_CHAR, ...
	int line;		// the line from the input file
	union {
		char *text;		// the chars for ID, STRING (dynamically allocated)
		int i;			// the value for INT
		char c;			// the value for CHAR
		double d;		// the value for DOUBLE
	};
	struct Token *next;		// next token in a simple linked list
} Token;

Token *tokenize(const char *pch);
void showTokens(const Token *tokens);