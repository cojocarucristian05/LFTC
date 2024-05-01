#include <stdio.h>
#include <stdlib.h>

#include "utils.h"
#include "lexer.h"
#include "parser.h"
#include "ad.h"
#include "vm.h"

int main(int argc, char *argv[]) {
    if (argc != 2) {
        printf("Eroare argumente!\n");
        return -1;
    }
    char *inbuf = loadFile(argv[1]);
    // puts(inbuf);
    Token *tokens = tokenize(inbuf);
    // showTokens(tokens);
    free(inbuf);

    pushDomain();
    vmInit();

    parse(tokens);
    
    // showDomain(symTable, "global");
    // Instr *testCode = genTestProgram(); // genereaza cod de test pentru masina virtuala
    Instr *testCode = temaMasinaVirtuala();
    run(testCode); // executie cod masina virtuala
    dropDomain();
    
    printf("\n");
    return 0;
}