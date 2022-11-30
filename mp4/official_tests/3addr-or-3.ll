define void @test3(i32 %x, i32* %P) {
    %t0 = shl i32 %x, 5
    %t1 = or i32 %t0, 3
    store i32 %t1, i32* %P
    ret void
}

define i32 @main() {
    %p = alloca i32
    call void @test3(i32 15, i32* %p)
    %a = load i32, i32* %p
    ret i32 %a
}
