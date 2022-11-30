define i32 @test1(i32 %sum, i32 %x) {
    %add4 = add i32 %x, %sum
    %cmp = icmp ult i32 %add4, %x
    %inc = zext i1 %cmp to i32
    %z.0 = add i32 %add4, %inc
    ret i32 %z.0
}

define i32 @main() {
    %a = call i32 @test1(i32 2, i32 12831)
    ret i32 %a
}
