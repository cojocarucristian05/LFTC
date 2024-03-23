#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>

#include "lexer.h"
#include "utils.h"

Token *tokens;	// single linked list of tokens
Token *lastTk;		// the last token in list

int line = 1;		// the current line in the input file

// adds a token to the end of the tokens list and returns it
// sets its code and line
Token *addTk(int code) {
	Token *tk = safeAlloc(sizeof(Token));
	tk->code = code;
	tk->line = line;
	tk->next = NULL;
	if(lastTk) {
		lastTk->next = tk;
	} else {
		tokens = tk;
	}
	lastTk = tk;
	return tk;
}

/**
 * functie care extrage sirul de caractere aflat intre cei doi pointeri (start si end)
*/
char *extract(const char *begin, const char *end){
	// calculam dimensiunea in bytes pentru noul sir
    size_t length = end - begin;
    
    // alocam memorie pentru noul sir
    char *substring = (char*)safeAlloc(length + 1); // +1 for NULL-termination
    
    // copiem sirul de caractere cuprins intre pointerii start si end
    strncpy(substring, begin, length);
    
    // adaugam terminator de sir
    substring[length] = '\0';
    
    return substring;
}

/**
 * functie ce parcurge continutul fisierului de intrare si extrage atomii lexicali
*/
Token *tokenize(const char *pch){
	const char *start;
	Token *tk;
    char *endptr;

    for(;;){
		switch(*pch){
			case ' ':
            case '\t': {
                pch++;
                break;
            }
			case '\r': {
				if(pch[1]=='\n') 
                    pch++;
            }
			case '\n': {
				line++;
				pch++;
				break;
            }
			case '\0': {
                addTk(END); 
                return tokens;
            }
			case ',': {
                addTk(COMMA); 
                pch++;
                break;
            }
			case '=': {
				if(pch[1] == '=') {
					addTk(EQUAL);
					pch+=2;
				}else{
					addTk(ASSIGN);
					pch++;
				}
				break;
            }
			case '(': {
                addTk(LPAR); 
                pch++; 
                break; 
            }
			case ')': {
                addTk(RPAR); 
                pch++; 
                break;
            }
            case '[': {
                addTk(LBRACKET); 
                pch++; 
                break; 
            }
			case ']': {
                addTk(RBRACKET); 
                pch++; 
                break;
            }
			case '{': {
                addTk(LACC); 
                pch++; 
                break;
            }
			case '}': {
                addTk(RACC);
                pch++; 
                break;
            }
    		case ';': {
                addTk(SEMICOLON);
                pch++;
                break;
            }
			case '+': {
                addTk(ADD);
                pch++;
                break;
            }
			case '-': {
                addTk(SUB); 
                pch++;
                break;
            }
			case '*': {
                addTk(MUL);
                pch++;
                break;
            }
            case '/': {
                if (pch[1] == '/') {
                    // Comentariu de tipul //
                    while (*pch && *pch != '\n') pch++; // Ignoră restul liniei
                } else if (pch[1] == '*') {
                    // Comentariu de tipul /* */
                    pch += 2; // Trecem peste /*
                    while (*pch && !(*pch == '*' && pch[1] == '/')) pch++; // Ignoră tot textul până la */
                    pch += 2; // Trecem peste */
                } else {
                    // Operatorul /
                    addTk(DIV);
                    pch++;
                }
                break;
            }
			case '.': {
                addTk(DOT); 
                pch++; 
                break;
            }
			case '&': {
				if(pch[1] == '&'){
					addTk(AND);
					pch += 2;
				}else{
					err("invalid char: %c (%d)", *pch, *pch);
				}
				break;
            }
			case '|': {
				if(pch[1] == '|'){
					addTk(OR);
					pch += 2;
				}else{
					err("invalid char: %c (%d)", *pch, *pch);
				}
				break;
            }
			case '!': {
			    if(pch[1]=='='){
					addTk(NOTEQ);
					pch+=2;
				}else{
					addTk(NOT);
					pch++;
				}
				break;
            }
			case '<': {
				if(pch[1] == '='){
					addTk(LESSEQ);
					pch += 2;
				}else{
					addTk(LESS);
					pch++;
				}
				break;
            }
			case '>': {
				if(pch[1] == '='){
					addTk(GREATEREQ);
					pch += 2;
				}else{
					addTk(GREATER);
					pch++;
				}
				break;
            }
			case '\'': {
        	    if (pch[2] == '\'') {
                    addTk(CHAR)->c = pch[1];
                    pch += 3;
                } else {
                    err("invalid char: %c (%d)", *pch, *pch);
                }
                break;
            }
			case '"': {
 			    for (start = ++pch; *pch && *pch != '"'; ++pch) {}
    				if (*pch == '"') {
   					     addTk(STRING)->text = extract(start, pch);
    				    ++pch; // Trecem peste al doilea caracter '"'
 				    } else {
   					     err("unclosed string literal");
   					}
   			    break;
            }
			default: {
				if(isalpha(*pch)||*pch=='_'){
					for(start=pch++;isalnum(*pch)||*pch=='_';pch++){}
					char *text = extract(start, pch);
					if(strcmp(text,"char")==0) addTk(TYPE_CHAR);
					else if(strcmp(text,"double")==0) addTk(TYPE_DOUBLE);
					else if(strcmp(text,"else")==0) addTk(ELSE);
					else if(strcmp(text,"if")==0) addTk(IF);
					else if(strcmp(text,"int")==0) addTk(TYPE_INT);
					else if(strcmp(text,"return")==0) addTk(RETURN);
					else if(strcmp(text,"struct")==0) addTk(STRUCT);
					else if(strcmp(text,"void")==0) addTk(VOID);
					else if(strcmp(text,"while")==0) addTk(WHILE);
					else if(strcmp(text,"else")==0) addTk(ELSE);
					else{
						tk = addTk(ID);
						tk->text = text;
					}
				} else if (isdigit(*pch) || *pch == '-') {
                    int hasDot = 0;
                    int hasE = 0;
                    if (*pch == '-') {
                        pch++;
                    }
                    for (start = pch; isdigit(*pch) || *pch == '.' || *pch == 'e' || *pch == 'E' || *pch == '+' || *pch == '-'; pch++) {
                        if (*pch == '.') {
                            if (hasDot) {
                                err("invalid number");
                            }
                            hasDot = 1;
                        } else if (*pch == 'e' || *pch == 'E') {
                            if (hasE) {
                                err("invalid number");
                            }
                            hasE = 1;
                        }
                    }
                    char *text = extract(start, pch);
                    if (hasDot || hasE) {
                        double val = strtod(text, &endptr);
                        if (endptr != text) {
                            addTk(DOUBLE)->d = val;
                        } else {
                            err("invalid number");
                        }
                    } else {
                        long val = strtol(text, &endptr, 10);
                        if (endptr != text) {
                            addTk(INT)->i = (int)val;
                        } else {
                            err("invalid number");
                        }
                    }
                } else {
                    err("invalid char: %c (%d)", *pch, *pch);
                }
            }
        }
    }
}

/**
 * functie care scrie intr-un fisier atomii lexicali optinuti
*/
void showTokens(const Token *tokens) {
	FILE *file = fopen("tests/output.txt", "w"); // Deschide fișierul în modul de scriere ("w")
    if (file == NULL) {
        printf("Eroare la deschiderea fișierului.");
        return;
    }

    for (const Token *tk = tokens; tk; tk = tk->next) {
        fprintf(file, "%d\t", tk->line); // Scrie numărul liniei în fișier
        switch (tk->code) {
            case ID: fprintf(file, "ID:%s\n", tk->text); break;
            case TYPE_CHAR: fprintf(file, "TYPE_CHAR\n"); break;
            case COMMA: fprintf(file, "COMMA\n"); break;
            case END: fprintf(file, "END\n"); break;
            case ASSIGN: fprintf(file, "ASSIGN\n"); break;
            case EQUAL: fprintf(file, "EQUAL\n"); break;
			case NOTEQ: fprintf(file, "NOTEQ\n"); break;
            case INT: fprintf(file, "INT:%d\n", tk->i); break;
            case TYPE_INT: fprintf(file, "TYPE_INT\n"); break;
            case CHAR: fprintf(file, "CHAR:%c\n", tk->c); break;
            case DOUBLE: fprintf(file, "DOUBLE:%g\n", tk->d); break;
            case TYPE_DOUBLE: fprintf(file, "TYPE_DOUBLE\n"); break;
            case STRING: fprintf(file, "STRING:%s\n", tk->text); break;
            case IF: fprintf(file, "IF\n"); break;
            case ELSE: fprintf(file, "ELSE\n"); break;
            case WHILE: fprintf(file, "WHILE\n"); break;
            case RETURN: fprintf(file, "RETURN\n"); break;
            case STRUCT: fprintf(file, "STRUCT\n"); break;
            case VOID: fprintf(file, "VOID\n"); break;
            case ADD: fprintf(file, "ADD\n"); break;
            case SUB: fprintf(file, "SUB\n"); break;
            case MUL: fprintf(file, "MUL\n"); break;
            case DIV: fprintf(file, "DIV\n"); break;
            case DOT: fprintf(file, "DOT\n"); break;
            case AND: fprintf(file, "AND\n"); break;
            case OR: fprintf(file, "OR\n"); break;
            case NOT: fprintf(file, "NOT\n"); break;
            case LESSEQ: fprintf(file, "LESSEQ\n"); break;
            case GREATEREQ: fprintf(file, "GREATEREQ\n"); break;
            case LESS: fprintf(file, "LESS\n"); break;
            case GREATER: fprintf(file, "GREATER\n"); break;
            case LPAR: fprintf(file, "LPAR\n"); break;
            case RPAR: fprintf(file, "RPAR\n"); break;
            case LBRACKET: fprintf(file, "LBRACKET\n"); break;
            case RBRACKET: fprintf(file, "RBRACKET\n"); break;
            case LACC: fprintf(file, "LACC\n"); break;
            case RACC: fprintf(file, "RACC\n"); break;
            case SEMICOLON: fprintf(file, "SEMICOLON\n"); break;
            default: fprintf(file, "Unknown token\n");
        }
    }

    if(fclose(file) != 0) {
        printf("Eroare inchidere fisier!\n");
    }
}
