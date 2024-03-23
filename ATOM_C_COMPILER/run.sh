#!/bin/bash
EXECUTABIL="program"
MAIN="main.c"
UTILS="./utils.c ./lexer.c ./parser.c"
ARGS=$1

gcc -Wall -o $EXECUTABIL $UTILS $MAIN

./$EXECUTABIL $ARGS

rm $EXECUTABIL