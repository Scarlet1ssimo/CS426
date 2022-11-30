define i32 @test1(i32 %x) {
    %t0 = shl i32 %x, 5
    %t1 = or i32 %t0, 3
    ret i32 %t1
}

define i32 @main() {
    %c = call i32 @test1(i32 15)
    ret i32 %c
}
