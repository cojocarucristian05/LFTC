#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdbool.h>

#include "parser.h"
#include "ad.h"
#include "at.h"
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
bool expr(Ret *r);
bool exprAssign(Ret *r);
bool exprOr(Ret *r);
bool exprAnd(Ret *r);
bool exprEq(Ret *r);
bool exprRel(Ret *r);
bool exprAdd(Ret *r);
bool exprMul(Ret *r);
bool exprCast(Ret *r);
bool exprUnary(Ret *r);
bool exprPostfix(Ret *r);
bool exprPrimary(Ret *r);

// functii auxiliare pentru a elimina recursivitatea stanga
bool exprOrPrim(Ret *r);
bool exprAndPrim(Ret *r);
bool exprEqPrim(Ret *r);
bool exprRelPrim(Ret *r);
bool exprAddPrim(Ret *r);
bool exprMulPrim(Ret *r);
bool exprPostfixPrim(Ret *r);

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
	Ret rCond, rExpr;
	Token *start = iTk;
	if (stmCompound(true)) {
		return true;
	}
	if (consume(IF)) {
		if (consume(LPAR)) {
			if (expr(&rCond)) {
				if (!canBeScalar(&rCond)) {
					tkerr("the if condition must be a scalar value");
				}
				if (consume(RPAR)) {
					if (stm()) {
						if (consume(ELSE)) {
							if (stm()) {
								return true;
							}
							return false;
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
			if (expr(&rCond)) {
				if (!canBeScalar(&rCond)) {
					tkerr("the while condition must be a scalar value");
				}
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
		if (expr(&rExpr)) {
			if (owner->type.tb == TB_VOID) {
				tkerr("a void function cannot return a value");
			}
			if (!canBeScalar(&rExpr)) {
				tkerr("the return value must be a scalar value");
			}
			if (!convTo(&rExpr.type, &owner->type)) {
				tkerr("cannot convert the return expression type to the function return type");
			}
		} else {
			if(owner->type.tb != TB_VOID) {
				tkerr("a non-void function must return a value");
			}
		}
		if (consume(SEMICOLON)) {
			return true;
		} else {
			tkerr("Lipseste ';' dupa return");
		}
		iTk = start;
	}
	if (expr(&rExpr)) {
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
		if (newDomain) {
			pushDomain();
		}
		for (;;) {
			if (varDef() || stm()) {}
			else break;
		}
		if (consume(RACC)) {
			if (newDomain) {
				dropDomain();
			}
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
bool expr(Ret *r) {
	// printf("#expr %d\n",iTk->code);//pentru debugging !!!
	Token *start = iTk;
	if (exprAssign(r)) {
		return true;
	}
	iTk = start;
	return false;
}

/**
 * exprAssign: exprUnary ASSIGN exprAssign | exprOr
*/
bool exprAssign(Ret *r) {
	// printf("#exprAssign  %d\n",iTk->code);//pentru debugging !!!
	Ret rDst;
	Token *start = iTk;
	if (exprUnary(&rDst)) {
		if (consume(ASSIGN)) {
			if (exprAssign(r)) {
				if (!rDst.lval) {
					tkerr("the assign destination must be a left-value");
				}
				if (rDst.ct) {
					tkerr("the assign destination cannot be constant");
				}
				if (!canBeScalar(&rDst)) {
					tkerr("the assign destination must be scalar");
				}
				if (!canBeScalar(r)) {
					tkerr("the assign source must be scalar");
				}
				if (!convTo(&r->type,&rDst.type)) {
					tkerr("the assign source cannot be converted to destination");
				}
				r->lval = false;
				r->ct = true;
				return true;
			} else {
				tkerr("Lipseste expresia dupa semnul =");
			}
		}
		iTk = start;
	}
	if (exprOr(r)) {
		return true;
	}
	iTk = start;
	return false;
}

/**
 * exprOr: exprOr OR exprAnd | exprAnd
*/
bool exprOr(Ret *r){
	// printf("#exprOr  %d\n",iTk->code);//pentru debugging !!!
	Token *start = iTk;
	if(exprAnd(r)) {
		if(exprOrPrim(r)){
			return true;//am ajuns la capatul regulii
		}
	}
	iTk = start;
	return false;
}

bool exprOrPrim(Ret *r) { //este recursiva
    // printf("#exprOrPrim %d\n", iTk->code); // pentru debugging !!!
	if (consume(OR)) {
		Ret right;
		if(exprAnd(&right)) {
			Type tDst;
			if (!arithTypeTo(&r->type, &right.type, &tDst)) {
				tkerr("invalid operand type for ||");
			}
			*r = (Ret){{TB_INT,NULL,-1},false,true};
			if(exprOrPrim(r)) {
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
bool exprAnd(Ret *r) {
    // printf("#exprAnd %d\n", iTk->code); // pentru debugging !!!
	Token *start = iTk;
	if (exprEq(r)) {
		if (exprAndPrim(r)) {
			return true;
		}
    }

	iTk = start;
    return false;
}

bool exprAndPrim(Ret *r) {
	// printf("#exprAndPrim %d\n", iTk->code);
    if (consume(AND)) {
		Ret right;
        if (exprEq(&right)) {
			Type tDst;
			if (!arithTypeTo(&r->type, &right.type, &tDst)) {
				tkerr("invalid operand type for &&"); 
			}
			*r = (Ret){{TB_INT,NULL,-1},false,true};
            if (exprAndPrim(r)) {
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
bool exprEq(Ret *r) {
    // printf("#exprEq %d\n", iTk->code); // pentru debugging !!!
	Token *start = iTk;
    if (exprRel(r)) {
        if (exprEqPrim(r)) {
			return true;
		}
    }
	iTk = start;
    return false;
}

bool exprEqPrim(Ret *r) {
	// printf("#exprEqPrim %d\n", iTk->code);
    if (consume(EQUAL) || consume(NOTEQ)) {
		Ret right;
        if (exprRel(&right)) {
			Type tDst;
            if (!arithTypeTo(&r->type, &right.type, &tDst)) {
				tkerr("invalid operand type for == or !="); 
			}
			*r = (Ret){{TB_INT,NULL,-1},false,true};
            if (exprEqPrim(r)) {
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
bool exprRel(Ret *r) {
    // printf("#exprRel %d\n", iTk->code); // pentru debugging !!!
	Token *start = iTk;
    if (exprAdd(r)) {
        if (exprRelPrim(r)) {
			return true;
		}
    }
	iTk = start;
    return false;
}

bool exprRelPrim(Ret *r) {
	// printf("#exprRelPrim %d\n", iTk->code);
    if (consume(LESS) || consume(LESSEQ) || consume(GREATER) || consume(GREATEREQ)) {
        Ret right;
		if (exprAdd(&right)) {
			Type tDst;
            if (!arithTypeTo(&r->type, &right.type, &tDst)) {
				tkerr("invalid operand type for <, <=, >, >="); 
			}
			*r = (Ret){{TB_INT,NULL,-1},false,true};
            if (exprRelPrim(r)) {
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
bool exprAdd(Ret *r) {
    // printf("#exprAdd %d\n", iTk->code); // pentru debugging !!!
	Token *start = iTk;
    if (exprMul(r)) {
        if (exprAddPrim(r)) {
			return true;
		}
    }
	iTk = start;
    return false;
}

bool exprAddPrim(Ret *r) {
	// printf("#exprAddPrim %d\n", iTk->code);
    if (consume(ADD) || consume(SUB)) {
		Ret right;
        if (exprMul(&right)) {
			Type tDst;
            if (!arithTypeTo(&r->type, &right.type, &tDst)) {
				tkerr("invalid operand type for + or -"); 
			}
			*r = (Ret){tDst,false,true};
            if (exprAddPrim(r)) {
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
bool exprMul(Ret *r) {
    // printf("#exprMul %d\n", iTk->code); // pentru debugging !!!
	Token *start = iTk;
    if (exprCast(r)) {
        if (exprMulPrim(r)) {
			return true;
		}
    }
	iTk = start;
    return false;
}

bool exprMulPrim(Ret *r) {
	// printf("#exprMulPrim %d\n", iTk->code);
    if (consume(MUL) || consume(DIV)) {
		Ret right;
        if (exprCast(&right)) {
			Type tDst;
            if (!arithTypeTo(&r->type, &right.type, &tDst)) {
				tkerr("invalid operand type for * or /"); 
			}
			*r = (Ret){tDst,false,true};
            if (exprMulPrim(r)) {
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
bool exprCast(Ret *r) {
	// printf("#exprCast %d\n", iTk->code); // pentru debugging !!!
	Token *start = iTk;
	if (consume(LPAR)) {
		Type t;
        Ret op;
		if (typeBase(&t)) {
			if (arrayDecl(&t)) {}
			if (consume(RPAR)) {
				if(exprCast(&op)) {
					if (t.tb == TB_STRUCT) {
						tkerr("cannot convert to a struct type"); 
					}
					if (op.type.tb == TB_STRUCT) {
						tkerr("cannot convert a struct");
					} 
					if (op.type.n >= 0 && t.n < 0) {
						tkerr("an array can be converted only to another array"); 
					}
					if (op.type.n < 0 && t.n >= 0) {
						tkerr("a scalar can be converted only to another scalar"); 
					}
					*r = (Ret){t,false,true};
					return true;
				} else {
					tkerr("Lipseste expresia de la Type Cast");
				}
			} else {
				tkerr("Lipseste ')'");
			}
		} else {
			tkerr("Lipseste expresia dupa semnul '}'");
		}
		iTk = start;
	}
	if (exprUnary(r)) {
		return true;
	}
	iTk = start;
	return false;
}

/**
 * exprUnary: ( SUB | NOT ) exprUnary | exprPostfix
*/
bool exprUnary(Ret *r) {
	// printf("#exprUnary %d\n",iTk->line);
	Token *start = iTk;
	if (consume(SUB) || consume(NOT)) {
		if (exprUnary(r)) {
			if (!canBeScalar(r)) {
				tkerr("unary - or ! must have a scalar operand");
			}
			r->lval = false;
			r->ct = true;
			return true;
		} else {
			tkerr("Expresoe invalida dupa '-' sau '!'");
		}
		iTk = start;
	}
	if (exprPostfix(r)) {
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
bool exprPostfix(Ret *r) {
    // printf("#exprPostfix %d\n", iTk->code); // pentru debugging !!!
	Token *start = iTk;
    if (exprPrimary(r)) {
        if (exprPostfixPrim(r)) {
			return true;
		}
    }
	iTk = start;
    return false;
}

bool exprPostfixPrim(Ret *r) {
	// printf("#exprPostfixPrim %d\n", iTk->code);
	Token *start = iTk;
    if (consume(LBRACKET)) {
		Ret idx;
        if (expr(&idx)) {
            if (consume(RBRACKET)) {
				if (r->type.n < 0) {
					tkerr("only an array can be indexed");
				}
				Type tInt = {TB_INT,NULL,-1};
                if (!convTo(&idx.type, &tInt)) {
					tkerr("the index is not convertible to int");
				}
				r->type.n = -1;
                r->lval = true;
                r->ct = false;
                if (exprPostfixPrim(r)) {
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
			Token *tkName = consumedTk;
            if (r->type.tb != TB_STRUCT) {
				tkerr("a field can only be selected from a struct");
			}
			Symbol *s = findSymbolInList(r->type.s->structMembers, tkName->text);
            if (!s) {
				tkerr("the structure %s does not have a field %s",r->type.s->name,tkName->text);
			}
			*r = (Ret){s->type,true,s->type.n>=0};
            if (exprPostfixPrim(r)) {
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
bool exprPrimary(Ret *r) {
	// printf("#exprPrimary %d\n", iTk->code);
	Token *start = iTk;
	if (consume(ID)) {
		Token *tkName = consumedTk;
		Symbol *s = findSymbol(tkName->text);
		if (!s) {
			tkerr("undefined id: %s", tkName->text);
		}
		if (consume(LPAR)) {
			if(s->kind!=SK_FN)tkerr("only a function can be called");
			Ret rArg;
			Symbol *param=s->fn.params;
			if (expr(&rArg)) {
				if (!param) {
					tkerr("too many arguments in function call");
				}
				if (!convTo(&rArg.type, &param->type)) {
					tkerr("in call, cannot convert the argument type to the parameter type");
				}
				param=param->next;
				for (;;) {
					if (consume(COMMA)) {
						if (expr(&rArg)) {
							if (!param) {
								tkerr("too many arguments in function call");
							}
							if (!convTo(&rArg.type, &param->type)) {
								tkerr("in call, cannot convert the argument type to the parameter type");
							}
							param=param->next;
						} 
						else {
							tkerr("Lipseste expresia dupa ','");
						};
					} else break;
				}
			}
			if (consume(RPAR)) {
				if (param) {
					tkerr("too few arguments in function call");
				}
				*r = (Ret){s->type,false,true};
			} else {
				if (s->kind == SK_FN) {
					tkerr("a function can only be called");
				}
				*r = (Ret){s->type,true,s->type.n>=0};
				tkerr("Lipseste ')' in apelul functiei");
			}
		} else {
			if(s->kind==SK_FN) {
				tkerr("a function can only be called"); 
			}
			*r = (Ret){s->type,true,s->type.n>=0};
		}
		return true;
	}
	if (consume(INT)) {
		*r = (Ret){{TB_INT,NULL,-1},false,true};
		return true;
	}
	if (consume(DOUBLE)) {
		*r = (Ret){{TB_DOUBLE,NULL,-1},false,true};
		return true;
	}
	if (consume(CHAR)) {
		*r = (Ret){{TB_CHAR,NULL,-1},false,true};
		return true;
	}
	if (consume(STRING)) {
		*r = (Ret){{TB_CHAR,NULL,0},false,true};
		return true;
	}
	if (consume(LPAR)) {
		if (expr(r)) {
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