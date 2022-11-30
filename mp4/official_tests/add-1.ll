define i32 @test1(i32 inreg %a) nounwind {
entry:
    %b = add i32 %a, 128
    ret i32 %b
}

define i32 @main() {
    %a = call i32 @test1(i32 123)
    ret i32 %a
}
