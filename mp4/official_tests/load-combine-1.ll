define i32 @load_i32_by_i8(i32* %arg) {
    %tmp = bitcast i32* %arg to i8*
    %tmp1 = load i8, i8* %tmp, align 1
    %tmp2 = zext i8 %tmp1 to i32
    %tmp3 = getelementptr inbounds i8, i8* %tmp, i32 1
    %tmp4 = load i8, i8* %tmp3, align 1
    %tmp5 = zext i8 %tmp4 to i32
    %tmp6 = shl nuw nsw i32 %tmp5, 8
    %tmp7 = or i32 %tmp6, %tmp2
    %tmp8 = getelementptr inbounds i8, i8* %tmp, i32 2
    %tmp9 = load i8, i8* %tmp8, align 1
    %tmp10 = zext i8 %tmp9 to i32
    %tmp11 = shl nuw nsw i32 %tmp10, 16
    %tmp12 = or i32 %tmp7, %tmp11
    %tmp13 = getelementptr inbounds i8, i8* %tmp, i32 3
    %tmp14 = load i8, i8* %tmp13, align 1
    %tmp15 = zext i8 %tmp14 to i32
    %tmp16 = shl nuw nsw i32 %tmp15, 24
    %tmp17 = or i32 %tmp12, %tmp16
    ret i32 %tmp17
}

define i1 @main() {
    %p = alloca i32
    store i32 16909060, i32* %p
    %a = call i32 @load_i32_by_i8(i32* %p)
    %e = icmp eq i32 %a, 16909060
    ret i1 %e
}
