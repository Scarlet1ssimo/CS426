define i32 @test1(i32 %a, i32 %b) nounwind ssp {
entry:
    %0 = and i32 %a, 16384
    %1 = icmp ne i32 %0, 0
    %2 = and i32 %b, 16384
    %3 = icmp ne i32 %2, 0
    %4 = xor i1 %1, %3
    br i1 %4, label %bb1, label %bb

bb:                                               ; preds = %entry
    ret i32 10

bb1:                                              ; preds = %entry
    ret i32 11
}

define i32 @main() {
    %a = call i32 @test1(i32 107, i32 87)
    ret i32 %a
}
