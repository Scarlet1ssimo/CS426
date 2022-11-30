define i32 @load_i32_by_i16_i8(i32* %arg) {
    %tmp = bitcast i32* %arg to i16*
    %tmp1 = bitcast i32* %arg to i8*
    %tmp2 = load i16, i16* %tmp, align 1
    %tmp3 = zext i16 %tmp2 to i32
    %tmp4 = getelementptr inbounds i8, i8* %tmp1, i32 2
    %tmp5 = load i8, i8* %tmp4, align 1
    %tmp6 = zext i8 %tmp5 to i32
    %tmp7 = shl nuw nsw i32 %tmp6, 16
    %tmp8 = getelementptr inbounds i8, i8* %tmp1, i32 3
    %tmp9 = load i8, i8* %tmp8, align 1
    %tmp10 = zext i8 %tmp9 to i32
    %tmp11 = shl nuw nsw i32 %tmp10, 24
    %tmp12 = or i32 %tmp7, %tmp11
    %tmp13 = or i32 %tmp12, %tmp3
    ret i32 %tmp13
}

define i1 @main() {
    %p = alloca i32
    store i32 16909060, i32* %p
    %a = call i32 @load_i32_by_i16_i8(i32* %p)
    %e = icmp eq i32 %a, 16909060
    ret i1 %e
}
