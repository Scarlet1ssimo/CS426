define i32 @test2(i8 %A, i8 %B) {
    %C = zext i8 %A to i64
    %D = shl i64 %C, 4
    %E = and i64 %D, 48
    %F = zext i8 %B to i64
    %G = lshr i64 %F, 4
    %H = or i64 %G, %E
    %I = trunc i64 %H to i32
    ret i32 %I
}

define i32 @main() {
    %c = call i32 @test2(i8 2, i8 15)
    ret i32 %c
}
