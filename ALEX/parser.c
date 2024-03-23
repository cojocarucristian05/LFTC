#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdbool.h>

#include "parser.h"

Token *iTk;		// the iterator in the tokens list
Token *consumedTk;		// the last consumed token

bool consume(int code);
bool structDef();
bool varDef();
bool typeBase();
bool arrayDecl();
bool fnDef();
bool fnParam();
bool stm();
bool stmCompound();
bool expr();
bool exprAssign();
bool exprOr();
bool exprAnd();
bool exprEq();
bool exprRel();
bool exprAdd();
bool exprMul();
bool exprCast();
bool exprUnary();
bool exprPostfix();
bool exprPrimary();

// functii auxiliare pentru a elimina recursivitatea stanga
bool exprOrPrim();
bool exprAndPrim();
bool exprEqPrim();
bool exprRelPrim();
bool exprAddPrim();
bool exprMulPrim();
bool exprPostfixPrim();

void tkerr(const char *fmt,...) {
	fprintf(stderr, "error in line %d: ", iTk->line);
	va_list va;
	va_start(va, fmt);
	vfprintf(stderr, fmt, va);
	va_end(va);
	fprintf(stderr, "\n");
	exit(EXIT_FAILURE);
}

bool consume(int code) {
	if (iTk->code == code) {
		consumedTk = iTk;
		iTk = iTk->next;
		return true;
	}
	return false;
}

/**
 * structDef: STRUCT ID LACC varDef* RACC SEMICOLON 
 */
bool structDef() {
	// printf("#structDef %d\n",iTk->code);//pentru debugging !!!
	Token *start = iTk;
	if (consume(STRUCT)) {
		if (consume(ID)) {
			if (consume(LACC)) {
				for (;;) {
					if (varDef()) {}
					else break;
				}
				if (consume(RACC)) {
					if (consume(SEMICOLON)) {
						return true;
					} else {
						tkerr("Lipseste ';' dupa definerea structurii");
					}
				} else {
					tkerr("Lipseste '}' la finalul structurii");
				}
			}
		} else {
			tkerr("Lipseste numele structurii");
		}
	}
	iTk = start;
	return false;
}

/**
 * varDef: typeBase ID arrayDecl? SEMICOLON
*/
bool varDef() {
	// printf("#varDef %d\n",iTk->code);//pentru debugging !!!
	Token *start = iTk;
	if (typeBase()) {
		if (consume(ID)) {
			if (arrayDecl()) {}
			if (consume(SEMICOLON)) {
				return true;
			} else {
				tkerr("Lipseste ';' dupa definerea variabilei");
			}
		} else {
			tkerr("Lipseste numele variabilei");
		}
	}
	iTk = start;
	return false;
}

/**
 * typeBase: TYPE_INT | TYPE_DOUBLE | TYPE_CHAR | STRUCT ID
*/
bool typeBase() {
	// printf("#typeBase %d\n",iTk->code);//pentru debugging !!!
	Token *start = iTk;
	if (consume(TYPE_INT)) {
		return true;
	}
	if (consume(TYPE_DOUBLE)) {
		return true;
	}
	if (consume(TYPE_CHAR)) {
		return true;
	}
	if (consume(STRUCT)) {
		if (consume(ID)) {
			return true;
		} else {
			tkerr("Lipseste numele structurii");
		}
	}
	iTk = start;
	return false;
}

/**
 * arrayDecl: LBRACKET INT? RBRACKET
*/
bool arrayDecl() {
	// printf("#arrayDecl %d\n",iTk->code);//pentru debugging !!!
	Token *start = iTk;
	if (consume(LBRACKET)) {
		if (consume(INT)) {}
		if (consume(RBRACKET)) {
			return true;
		} else {
			tkerr("Lipseste ']' la declararea tabloului");
		}
	}
	iTk = start;
	return false;
}

/**
 * fnDef: ( typeBase | VOID ) ID
 *				 LPAR ( fnParam ( COMMA fnParam )* )? RPAR
 *				 stmCompound
*/
bool fnDef() {
	// printf("#fnDef %d\n",iTk->code);//pentru debugging !!!
	Token *start = iTk;
	if (typeBase() || consume(VOID)) {
		if (consume(ID)) {
			if (consume(LPAR)) {
				if (fnParam()) {
					for (;;) {
						if (consume(COMMA)) {
							if (fnParam()) {}
							else return false;
						} else break;
					}
				}
				if (consume(RPAR)) {
					if (stmCompound()) {
						return true;
					} else {
						tkerr("Lipseste corpul functiei");
					}
				} else {
					tkerr("Lipseste ')' la finalul functiei");
				}
			}
		}
	}
	iTk = start;
	return false;
}

/**
 * fnParam: typeBase ID arrayDecl?
*/
bool fnParam() {
	// printf("#fnParam %d\n",iTk->code);//pentru debugging !!!
	Token *start = iTk;
	if (typeBase()) {
		if (consume(ID)) {
			if (arrayDecl()) {}
			return true;
		} else {
			tkerr("Lipseste numele parametrului");
		}
	}
	iTk = start;
	return false;
}

/**
 * stm: stmCompound
 *		 | IF LPAR expr RPAR stm ( ELSE stm )?
 *		 | WHILE LPAR expr RPAR stm
 *		 | RETURN expr? SEMICOLON
 *		 | expr? SEMICOLON
*/
bool stm() {
	// printf("#stm %d\n",iTk->code);//pentru debugging !!!
	Token *start = iTk;
	if (stmCompound()) {
		return true;
	}
	if (consume(IF)) {
		if (consume(LPAR)) {
			if (expr()) {
				if (consume(RPAR)) {
					if (stm()) {
						if (consume(ELSE)) {
							if (stm()) {}
							else return false;
						}
						return true;
					}
				} else {
					tkerr("Lipseste ')' dupa expresie");
				}
			} else {
				tkerr("Lipseste expresia");
			} 
		} else {
			tkerr("Lipseste '(' inainte de expresie");
		}
		iTk = start;
	}
	if (consume(WHILE)) {
		if (consume(LPAR)) {
			if (expr()) {
				if (consume(RPAR)) {
					if (stm()) {
						return true;
					}
				} else {
					tkerr("Lipseste ')' dupa expresie");
				}
			}  else {
				tkerr("Lipseste expresia");
			} 
		} else {
			tkerr("Lipseste '(' inainte de expresie");
		}
		iTk = start;
	}
	if (consume(RETURN)) {
		if (expr()) {}
		if (consume(SEMICOLON)) {
			return true;
		} else {
			tkerr("Lipseste ';' dupa return");
		}
		iTk = start;
	}
	if (expr()) {}
	if (consume(SEMICOLON)) {
		return true;
	}
	iTk = start;
	return false;
}

/**
 * stmCompound: LACC ( varDef | stm )* RACC
*/
bool stmCompound() {
	// printf("#stmCompound %d\n",iTk->code);//pentru debugging !!!
	Token *start = iTk;
	if (consume(LACC)) {
		for (;;) {
			if (varDef() || stm()) {}
			else break;
		}
		if (consume(RACC)) {
			return true;
		} else {
			tkerr("Lipseste ')'");
		}
	}
	iTk = start;
	return false;
}

/**
 * expr: exprAssign
*/
bool expr() {
	// printf("#expr %d\n",iTk->code);//pentru debugging !!!
	Token *start = iTk;
	if (exprAssign()) {
		return true;
	}
	iTk = start;
	return false;
}

/**
 * exprAssign: exprUnary ASSIGN exprAssign | exprOr
*/
bool exprAssign() {
	// printf("#exprAssign  %d\n",iTk->code);//pentru debugging !!!
	Token *start = iTk;
	if (exprUnary()) {
		if (consume(ASSIGN)) {
			if (exprAssign()) {
				return true;
			}
		}
		iTk = start;
	}
	if (exprOr()) {
		return true;
	}
	iTk = start;
	return false;
}

/**
 * exprOr: exprOr OR exprAnd | exprAnd
*/
bool exprOr(){
	// printf("#exprOr  %d\n",iTk->code);//pentru debugging !!!
	Token *start = iTk;
	if(exprAnd()){
		if(exprOrPrim()){
			return true;//am ajuns la capatul regulii
		}
	}
	iTk = start;
	return false;
}

bool exprOrPrim() { //este recursiva
    // printf("#exprOrPrim %d\n", iTk->code); // pentru debugging !!!
	if (consume(OR)) {
		if(exprAnd()) {
			if(exprOrPrim()) {
				return true;
			}
		}
	}
	return true; //epsilon-ul nostru
}

/**
 * exprAnd: exprAnd AND exprEq | exprEq
*/
bool exprAnd() {
    // printf("#exprAnd %d\n", iTk->code); // pentru debugging !!!
	Token *start = iTk;
	if (exprEq()) {
		if (exprAndPrim()) {
			return true;
		}
    }

	iTk = start;
    return false;
}

bool exprAndPrim() {
	// printf("#exprAndPrim %d\n", iTk->code);
    if (consume(AND)) {
        if (exprEq()) {
            if (exprAndPrim()) {
                return true;
            }
        }
    }
    return true; // epsilon-ul nostru
}

/**
 * exprEq: exprEq ( EQUAL | NOTEQ ) exprRel | exprRel
*/
bool exprEq() {
    // printf("#exprEq %d\n", iTk->code); // pentru debugging !!!
	Token *start = iTk;
    if (exprRel()) {
        if (exprEqPrim()) {
			return true;
		}
    }
	iTk = start;
    return false;
}

bool exprEqPrim() {
	// printf("#exprEqPrim %d\n", iTk->code);
    if (consume(EQUAL) || consume(NOTEQ)) {
        if (exprRel()) {
            if (exprEqPrim()) {
                return true;
            }
        }
    }
    return true; // epsilon-ul nostru
}

/**
 * exprRel: exprRel ( LESS | LESSEQ | GREATER | GREATEREQ ) exprAdd | exprAdd
*/
bool exprRel() {
    // printf("#exprRel %d\n", iTk->code); // pentru debugging !!!
	Token *start = iTk;
    if (exprAdd()) {
        if (exprRelPrim()) {
			return true;
		}
    }
	iTk = start;
    return false;
}

bool exprRelPrim() {
	// printf("#exprRelPrim %d\n", iTk->code);
    if (consume(LESS) || consume(LESSEQ) || consume(GREATER) || consume(GREATEREQ)) {
        if (exprAdd()) {
            if (exprRelPrim()) {
                return true;
            }
        }
    }
    return true; // epsilon-ul nostru
}


/**
 * exprAdd: exprAdd ( ADD | SUB ) exprMul | exprMul
*/
bool exprAdd() {
    // printf("#exprAdd %d\n", iTk->code); // pentru debugging !!!
	Token *start = iTk;
    if (exprMul()) {
        if (exprAddPrim()) {
			return true;
		}
    }
	iTk = start;
    return false;
}

bool exprAddPrim() {
	// printf("#exprAddPrim %d\n", iTk->code);
    if (consume(ADD) || consume(SUB)) {
        if (exprMul()) {
            if (exprAddPrim()) {
                return true;
            }
        }
    } 
    return true; // epsilon-ul nostru
}


/**
 * exprMul: exprMul ( MUL | DIV ) exprCast | exprCast
*/
bool exprMul() {
    // printf("#exprMul %d\n", iTk->code); // pentru debugging !!!
	Token *start = iTk;
    if (exprCast()) {
        if (exprMulPrim()) {
			return true;
		}
    }
	iTk = start;
    return false;
}

bool exprMulPrim() {
	// printf("#exprMulPrim %d\n", iTk->code);
    if (consume(MUL) || consume(DIV)) {
        if (exprCast()) {
            if (exprMulPrim()) {
                return true;
            }
        }
    }
    return true; // epsilon-ul nostru
}


/**
 * exprCast: LPAR typeBase arrayDecl? RPAR exprCast | exprUnary
*/
bool exprCast() {
	// printf("#exprCast %d\n", iTk->code); // pentru debugging !!!
	Token *start = iTk;
	if (consume(LPAR)) {
		if (typeBase()) {
			if (arrayDecl()) {}
			if (consume(RPAR)) {
				return exprCast();
			}
		}
		iTk = start;
	}
	if (exprUnary()) {
		return true;
	}
	iTk = start;
	return false;
}

/**
 * exprUnary: ( SUB | NOT ) exprUnary | exprPostfix
*/
bool exprUnary() {
	// printf("#exprUnary %d\n",iTk->line);
	Token *start = iTk;
	if (consume(SUB) || consume(NOT)) {
		if (exprUnary()) {
			return true;
		} 
		iTk = start;
	}
	if (exprPostfix()) {
		return true;
	}
	iTk = start;
	return false;
}

/**
 * exprPostfix: exprPostfix LBRACKET expr RBRACKET
 *		| exprPostfix DOT ID
 *		| exprPrimary
*/
bool exprPostfix() {
    // printf("#exprPostfix %d\n", iTk->code); // pentru debugging !!!
	Token *start = iTk;
    if (exprPrimary()) {
        if (exprPostfixPrim()) {
			return true;
		}
    }
	iTk = start;
    return false;
}

bool exprPostfixPrim() {
	// printf("#exprPostfixPrim %d\n", iTk->code);
	Token *start = iTk;
    if (consume(LBRACKET)) {
        if (expr()) {
            if (consume(RBRACKET)) {
                if (exprPostfixPrim()) {
                    return true;
                }
            }
        }
		iTk = start;
    }

    if (consume(DOT)) {
        if (consume(ID)) {
            if (exprPostfixPrim()) {
                return true;
            }
        }
		iTk = start;
    }

    return true; // epsilon-ul nostru
}


/**
 * exprPrimary: ID ( LPAR ( expr ( COMMA expr )* )? RPAR )?
 *		| INT | DOUBLE | CHAR | STRING | LPAR expr RPAR
*/
bool exprPrimary() {
	// printf("#exprPrimary %d\n", iTk->code);
	Token *start = iTk;
	if (consume(ID)) {
		if (consume(LPAR)) {
			if (expr()) {
				for (;;) {
					if (consume(COMMA)) {
						if (expr()) {} 
						else return false;
					} else break;
				}
				if (consume(RPAR)) {}
				else return false;
			}
		}
		// iTk = start;
		return true;
	}
	if (consume(INT)) {
		return true;
	}
	if (consume(DOUBLE)) {
		return true;
	}
	if (consume(CHAR)) {
		return true;
	}
	if (consume(STRING)) {
		return true;
	}
	if (consume(LPAR)) {
		if (expr()) {
			if (consume(RPAR)) {
				return true;
			} else {
				tkerr("Lipseste ')' la finalul expresiei");
			}
		} else {
			tkerr("Lipseste expresia");
		}
		iTk = start;
	}
	iTk = start;
	return false;
}

/**
 * unit: ( structDef | fnDef | varDef )* END
*/
bool unit() {
	for (;;) {
		if (structDef()) {}
		else if (fnDef()) {}
		else if (varDef()) {}
		else break;
	}
	if (consume(END)) {
		return true;
	} else {
		tkerr("Syntax error");
	}
	return false;
}

void parse(Token *tokens){
	iTk = tokens;
	if (!unit()) {}
}