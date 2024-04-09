#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdbool.h>

#include "parser.h"
#include "ad.h"
#include "vm.h"
#include "lexer.h"
#include "utils.h"

Token *iTk;		// the iterator in the tokens list
Token *consumedTk;		// the last consumed token

Symbol *owner = NULL;

bool consume(int code);
bool structDef();
bool varDef();
bool typeBase(Type *t);
bool arrayDecl(Type *t);
bool fnDef();
bool fnParam();
bool stm();
bool stmCompound(bool newDomain);
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
			Token *tkName = consumedTk;
			if (consume(LACC)) {
				Symbol *s = findSymbolInDomain(symTable, tkName->text);
				if(s) tkerr("symbol redefinition: %s", tkName->text);
				s = addSymbolToDomain(symTable,newSymbol(tkName->text, SK_STRUCT));
				s->type.tb = TB_STRUCT;
				s->type.s = s;
				s->type.n = -1;
				pushDomain();
				owner = s;

				for (;;) {
					if (varDef()) {}
					else break;
				}
				if (consume(RACC)) {
					if (consume(SEMICOLON)) {
						owner = NULL;
						dropDomain();
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
	Type t;
	if (typeBase(&t)) {
		if (consume(ID)) {
			Token *tkName = consumedTk;
			if (arrayDecl(&t)) {
				if (t.n == 0) tkerr("a vector variable must have a specified dimension");
			}
			if (consume(SEMICOLON)) {
				Symbol *var=findSymbolInDomain(symTable,tkName->text);
				if(var)tkerr("symbol redefinition: %s",tkName->text);
				var=newSymbol(tkName->text,SK_VAR);
				var->type=t;
				var->owner=owner;
				addSymbolToDomain(symTable,var);
				if (owner) {
					switch(owner->kind) {
						case SK_FN:
							var->varIdx=symbolsLen(owner->fn.locals);
							addSymbolToList(&owner->fn.locals,dupSymbol(var));
							break;
						case SK_STRUCT:
							var->varIdx=typeSize(&owner->type);
							addSymbolToList(&owner->structMembers,dupSymbol(var));
							break;
						default:
							break;
					}
				} else {
					var->varMem=safeAlloc(typeSize(&t));
				}
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
bool typeBase(Type *t) {
	// printf("#typeBase %d\n",iTk->code);//pentru debugging !!!
	t->n = -1;
	Token *start = iTk;
	if (consume(TYPE_INT)) {
		t->tb=TB_INT;
		return true;
	}
	if (consume(TYPE_DOUBLE)) {
		t->tb=TB_DOUBLE;
		return true;
	}
	if (consume(TYPE_CHAR)) {
		t->tb=TB_CHAR;
		return true;
	}
	if (consume(STRUCT)) {
		if (consume(ID)) {
			Token *tkName = consumedTk;
			t->tb = TB_STRUCT;
			t->s = findSymbol(tkName->text);
			if(!t->s) tkerr("structura nedefinita: %s",tkName->text);
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
bool arrayDecl(Type *t){
	if(consume(LBRACKET)){
		if(consume(INT)){
			Token *tkSize=consumedTk;
			t->n=tkSize->i;
		} else {
			t->n=0; // array fara dimensiune: int v[]
		}
		if(consume(RBRACKET)){
			return true;
		}else tkerr("missing ] or invalid expression inside [...]");
	}
return false;
}


/**
 * fnDef: ( typeBase | VOID ) ID
 *				 LPAR ( fnParam ( COMMA fnParam )* )? RPAR
 *				 stmCompound
*/
bool fnDef() {
	// printf("#fnDef %d\n",iTk->code);//pentru debugging !!!
	Type t;
	Token *start = iTk;
	if (typeBase(&t)) {
		if (consume(ID)) {
			Token *tkName = consumedTk;
			if (consume(LPAR)) {
				Symbol *fn=findSymbolInDomain(symTable,tkName->text);
				if(fn)tkerr("symbol redefinition: %s",tkName->text);
				fn=newSymbol(tkName->text,SK_FN);
				fn->type=t;
				addSymbolToDomain(symTable,fn);
				owner=fn;
				pushDomain();
				if (fnParam()) {
					for (;;) {
						if (consume(COMMA)) {
							if (fnParam()) {}
							else tkerr("Lipseste parametrul functiei dupa ,");
						} else break;
					}
				}
				if (consume(RPAR)) {
					if (stmCompound(false)) {
						dropDomain();
						owner=NULL;
						return true;
					} else {
						tkerr("Lipseste corpul functiei");
					}
				} else {
					tkerr("Lipseste ')' la finalul functiei");
				}
			}
		} else {
			tkerr("Lipseste numele functiei");
		}
	} else if (consume(VOID)) {
		t.tb=TB_VOID;
		if (consume(ID)) {
			Token *tkName = consumedTk;
			if (consume(LPAR)) {
				Symbol *fn=findSymbolInDomain(symTable,tkName->text);
				if(fn)tkerr("symbol redefinition: %s",tkName->text);
				fn=newSymbol(tkName->text,SK_FN);
				fn->type=t;
				addSymbolToDomain(symTable,fn);
				owner=fn;
				pushDomain();
				if (fnParam()) {
					for (;;) {
						if (consume(COMMA)) {
							if (fnParam()) {}
							else tkerr("Lipseste parametrul functiei dupa ,");
						} else break;
					}
				}
				if (consume(RPAR)) {
					if (stmCompound(false)) {
						dropDomain();
						owner=NULL;
						return true;
					} else {
						tkerr("Lipseste corpul functiei");
					}
				} else {
					tkerr("Lipseste ')' la finalul functiei");
				}
			}
		} else {
			tkerr("Lipseste numele functiei");
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
	Type t;
	if (typeBase(&t)) {
		if (consume(ID)) {
			Token *tkName = consumedTk;
			if (arrayDecl(&t)) {
				t.n=0;
			}
			Symbol *param=findSymbolInDomain(symTable,tkName->text);
			if(param)tkerr("symbol redefinition: %s",tkName->text);
			param=newSymbol(tkName->text,SK_PARAM);
			param->type=t;
			param->owner=owner;
			param->paramIdx=symbolsLen(owner->fn.params);
			// parametrul este adaugat atat la domeniul curent, cat si la parametrii fn
			addSymbolToDomain(symTable,param);
			addSymbolToList(&owner->fn.params,dupSymbol(param));
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
	if (stmCompound(true)) {
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
	if (expr()) {
		if (consume(SEMICOLON)) {
			return true;
		} else {
			tkerr("Lipseste ';'");
		}
	}
	if (consume(SEMICOLON)) {
		return true;
	}
	iTk = start;
	return false;
}

/**
 * stmCompound: LACC ( varDef | stm )* RACC
*/
bool stmCompound(bool newDomain) {
	// printf("#stmCompound %d\n",iTk->code);//pentru debugging !!!
	Token *start = iTk;
	if (consume(LACC)) {
		if(newDomain)pushDomain();
		for (;;) {
			if (varDef() || stm()) {}
			else break;
		}
		if (consume(RACC)) {
			if(newDomain)dropDomain();
			return true;
		} else {
			tkerr("Lipseste '}'");
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
			} else {
				tkerr("Lipseste expresia dupa semnul =");
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
		} else {
			tkerr("Lipseste expresia dupa semnul '||'");
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
        } else {
			tkerr("Lipseste expresia dupa semnul '&&'");
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
        } else {
			tkerr("Lipseste expresia dupa '== ' sau '!='");
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
        } else {
			tkerr("Lipseste expresia dupa semnul '<' sau '>' sau '<=' sau '>=");
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
        } else {
			tkerr("Lipseste expresia dupa semnul '+'");
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
        } else {
			tkerr("Lipseste expresia dupa semnul '*'");
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
	Type t;
	if (consume(LPAR)) {
		if (typeBase(&t)) {
			if (arrayDecl(&t)) {}
			if (consume(RPAR)) {
				return exprCast();
			} else {
				tkerr("Lipseste ')'");
			}
		} else {
			tkerr("Lipseste expresia dupa semnul '}'");
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
		} else {
			tkerr("Expresoe invalida dupa '-' sau '!'");
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
                } else {
					tkerr("Expresie invalida dupa ']'");
				}
            } else {
				tkerr("Lipseste ']' dupa expresie");
			}
        }
		iTk = start;
    }

    if (consume(DOT)) {
        if (consume(ID)) {
            if (exprPostfixPrim()) {
                return true;
            } else {
				tkerr("Lipseste expresia dupa nume campului");
			}
        } else {
			tkerr("Lipseste numele campului ce se doreste a fi cautat");
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
						else {
							tkerr("Lipseste expresia dupa ','");
						};
					} else break;
				}
			}
			if (consume(RPAR)) {
				return true;
			}
			else {
				tkerr("Lipseste ')' in apelul functiei");
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