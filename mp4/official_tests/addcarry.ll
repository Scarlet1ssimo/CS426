define i8 @e(i32* nocapture %a, i32 %b) nounwind {
    %1 = load i32, i32* %a, align 4
    %2 = add i32 %1, %b
    %3 = icmp ult i32 %2, %b
    %4 = zext i1 %3 to i8
    %5 = add i32 %2, %b
    store i32 %5, i32* %a, align 4
    %6 = icmp ult i32 %5, %b
    %7 = zext i1 %6 to i8
    %8 = add nuw nsw i8 %7, %4
    ret i8 %8
}

define i8 @main() {
    %p = alloca i32
    %a = call i8 @e(i32* %p, i32 21)
    ret i8 %a
}
