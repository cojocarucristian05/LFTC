#!/bin/bash
EXECUTABIL="program"
MAIN="main.c"
UTILS="./utils.c ./lexer.c"

gcc -Wall -o $EXECUTABIL $UTILS $MAIN

./$EXECUTABIL

rm $EXECUTABIL