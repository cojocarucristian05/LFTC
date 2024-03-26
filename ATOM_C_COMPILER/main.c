#include <stdio.h>
#include <stdlib.h>

#include "utils.h"
#include "lexer.h"
#include "parser.h"

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
    parse(tokens);
    return 0;
}