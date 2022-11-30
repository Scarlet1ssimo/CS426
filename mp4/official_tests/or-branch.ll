define i32 @foo(i32 %X, i32 %Y, i32 %Z) nounwind {
entry:
    %tmp1 = icmp eq i32 %X, 0
    %tmp3 = icmp slt i32 %Y, 5
    %tmp4 = or i1 %tmp3, %tmp1
    br i1 %tmp4, label %cond_true, label %UnifiedReturnBlock

cond_true:
    ret i32 1

UnifiedReturnBlock:
    ret i32 %Z
}

define i32 @main() {
    %a = call i32 @foo(i32 10, i32 29, i32 21)
    ret i32 %a
}
