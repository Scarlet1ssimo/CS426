#!/bin/bash
diffone(){
    # echo "My result:"
    ./lexer $1 | ./parser> rmy
    # echo "Ref result:"
    ../reference-binaries/lexer $1 | ../reference-binaries/parser> rac
    # DIFF=$(diff myresult result)
    if ! diff rmy rac -I#; then
        >&2 echo "Fail $1"
        exit 1
    else
        echo "Pass $1"
    fi
}

difffolder(){
    FILES=$(find $1 -name "*.cl")
    for i in $FILES; do
        diffone $i
    done
}

make all
# diffone ../cases/qwq.cl
# difffolder ../cool-examples

if [[ -n $1 ]]; then
    diffone $1
else 
    difffolder ../cool-examples
fi