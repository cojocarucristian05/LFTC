#include <stdio.h>
#include <stdlib.h>

#include "utils.h"
#include "lexer.h"

int main(int argc, char *argv[]) {
    char *inbuf = loadFile("tests/testlex.c");
    puts(inbuf);
    Token *tokens = tokenize(inbuf);
    showTokens(tokens);
    free(inbuf);
    return 0;
}