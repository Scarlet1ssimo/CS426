define i32 @pr32690(i32) {
    %2 = zext i32 %0 to i64
    %3 = add nuw nsw i64 %2, 63
    %4 = lshr i64 %3, 6
    %5 = trunc i64 %4 to i32
    ret i32 %5
}

define i32 @main() {
    %a = call i32 @pr32690(i32 2)
    ret i32 %a
}
