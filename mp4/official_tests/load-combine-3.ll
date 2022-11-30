define i32 @load_i32_by_i16_by_i8(i32* %arg) {
    %tmp = bitcast i32* %arg to i8*
    %tmp1 = load i8, i8* %tmp, align 1
    %tmp2 = zext i8 %tmp1 to i16
    %tmp3 = getelementptr inbounds i8, i8* %tmp, i32 1
    %tmp4 = load i8, i8* %tmp3, align 1
    %tmp5 = zext i8 %tmp4 to i16
    %tmp6 = shl nuw nsw i16 %tmp5, 8
    %tmp7 = or i16 %tmp6, %tmp2
    %tmp8 = getelementptr inbounds i8, i8* %tmp, i32 2
    %tmp9 = load i8, i8* %tmp8, align 1
    %tmp10 = zext i8 %tmp9 to i16
    %tmp11 = getelementptr inbounds i8, i8* %tmp, i32 3
    %tmp12 = load i8, i8* %tmp11, align 1
    %tmp13 = zext i8 %tmp12 to i16
    %tmp14 = shl nuw nsw i16 %tmp13, 8
    %tmp15 = or i16 %tmp14, %tmp10
    %tmp16 = zext i16 %tmp7 to i32
    %tmp17 = zext i16 %tmp15 to i32
    %tmp18 = shl nuw nsw i32 %tmp17, 16
    %tmp19 = or i32 %tmp18, %tmp16
    ret i32 %tmp19
}

define i1 @main() {
    %p = alloca i32
    store i32 16909060, i32* %p
    %a = call i32 @load_i32_by_i16_by_i8(i32* %p)
    %e = icmp eq i32 %a, 16909060
    ret i1 %e
}
