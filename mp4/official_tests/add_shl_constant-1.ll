define i32 @add_shl_add_constant_1_i32(i32 %x, i32 %y) nounwind {
    %add.0 = add i32 %x, 123
    %shl = shl i32 %add.0, 3
    %add.1 = add i32 %shl, %y
    ret i32 %add.1
}

define i32 @main() {
    %a = call i32 @add_shl_add_constant_1_i32(i32 15, i32 7)
    ret i32 %a
}
