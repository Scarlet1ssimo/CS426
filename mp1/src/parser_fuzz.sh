#!/bin/bash
diffone(){
    # echo "My result:"
    ./lexer $1 | ./parser> rmy
    # echo "Ref result:"
    ../reference-binaries/lexer $1 | ../reference-binaries/parser> rac
    # DIFF=$(diff myresult result)
    if ! diff rmy rac -I#; then
        >&2 echo "Fail $1"
        exit
    else
        echo "Pass $1"
    fi
}

make all
# diffone ../cases/qwq.cl
# difffolder ../cool-examples

while true; do
    python3.10 derive.py > qwq.cl
    diffone qwq.cl
done
