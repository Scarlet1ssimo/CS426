define i8 @t0(i32 %a1_wide_orig, i32 %a2_wide_orig, i32 %inc) {
    %a1_wide = add i32 %a1_wide_orig, %inc
    %a2_wide = add i32 %a2_wide_orig, %inc
    %a1 = trunc i32 %a1_wide to i8
    %a2 = trunc i32 %a2_wide to i8
    %t1 = icmp sgt i8 %a1, %a2
    %t2 = select i1 %t1, i8 %a1, i8 %a2
    ret i8 %t2
}

define i8 @main() {
    %a = call i8 @t0(i32 15, i32 8, i32 19)
    ret i8 %a
}
