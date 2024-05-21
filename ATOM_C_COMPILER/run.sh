#!/bin/bash
EXECUTABIL="program"
MAIN="main.c"
UTILS="./utils.c ./lexer.c ./ad.c ./at.c ./vm.c ./gc.c ./parser.c"
ARGS=$1

gcc -Wall -o $EXECUTABIL $UTILS $MAIN

./$EXECUTABIL $ARGS

rm $EXECUTABIL