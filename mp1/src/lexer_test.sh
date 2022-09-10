diffone(){
    ./lexer $1> rmy
    ../reference-binaries/lexer $1 > rac
    # DIFF=$(diff myresult result -I\#)
    if ! diff rmy rac -I#; then
        >&2 echo "Fail $1"
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

make lexer 
difffolder ../cool-examples
