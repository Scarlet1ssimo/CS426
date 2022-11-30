define i32 @test2(i32 %x, i32 %y, i32 %res) {
    %cmp = icmp ugt i32 %x, %y
    %dec = sext i1 %cmp to i32
    %dec.res = add nsw i32 %dec, %res
    ret i32 %dec.res
}

define i32 @main() {
    %a = call i32 @test2(i32 2, i32 12831, i32 2)
    ret i32 %a
}
