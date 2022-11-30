define i8 @PR30841(i64 %argc) {
entry:
    %or = or i64 %argc, -4294967296
    br label %end

end:
    %neg = sub nuw nsw i64 -4294967296, %argc
    %trunc = trunc i64 %neg to i8
    ret i8 %trunc
}

define i8 @main() {
    %a = call i8 @PR30841(i64 123)
    ret i8 %a
}
