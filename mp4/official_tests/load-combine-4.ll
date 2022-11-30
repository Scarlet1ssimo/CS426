define i64 @load_i64_by_i8_bswap(i64* %arg) {
    %tmp = bitcast i64* %arg to i8*
    %tmp1 = load i8, i8* %tmp, align 1
    %tmp2 = zext i8 %tmp1 to i64
    %tmp3 = shl nuw i64 %tmp2, 56
    %tmp4 = getelementptr inbounds i8, i8* %tmp, i64 1
    %tmp5 = load i8, i8* %tmp4, align 1
    %tmp6 = zext i8 %tmp5 to i64
    %tmp7 = shl nuw nsw i64 %tmp6, 48
    %tmp8 = or i64 %tmp7, %tmp3
    %tmp9 = getelementptr inbounds i8, i8* %tmp, i64 2
    %tmp10 = load i8, i8* %tmp9, align 1
    %tmp11 = zext i8 %tmp10 to i64
    %tmp12 = shl nuw nsw i64 %tmp11, 40
    %tmp13 = or i64 %tmp8, %tmp12
    %tmp14 = getelementptr inbounds i8, i8* %tmp, i64 3
    %tmp15 = load i8, i8* %tmp14, align 1
    %tmp16 = zext i8 %tmp15 to i64
    %tmp17 = shl nuw nsw i64 %tmp16, 32
    %tmp18 = or i64 %tmp13, %tmp17
    %tmp19 = getelementptr inbounds i8, i8* %tmp, i64 4
    %tmp20 = load i8, i8* %tmp19, align 1
    %tmp21 = zext i8 %tmp20 to i64
    %tmp22 = shl nuw nsw i64 %tmp21, 24
    %tmp23 = or i64 %tmp18, %tmp22
    %tmp24 = getelementptr inbounds i8, i8* %tmp, i64 5
    %tmp25 = load i8, i8* %tmp24, align 1
    %tmp26 = zext i8 %tmp25 to i64
    %tmp27 = shl nuw nsw i64 %tmp26, 16
    %tmp28 = or i64 %tmp23, %tmp27
    %tmp29 = getelementptr inbounds i8, i8* %tmp, i64 6
    %tmp30 = load i8, i8* %tmp29, align 1
    %tmp31 = zext i8 %tmp30 to i64
    %tmp32 = shl nuw nsw i64 %tmp31, 8
    %tmp33 = or i64 %tmp28, %tmp32
    %tmp34 = getelementptr inbounds i8, i8* %tmp, i64 7
    %tmp35 = load i8, i8* %tmp34, align 1
    %tmp36 = zext i8 %tmp35 to i64
    %tmp37 = or i64 %tmp33, %tmp36
    ret i64 %tmp37
}

define i1 @main() {
    %p = alloca i64
    store i64 72623859790382856, i64* %p
    %a = call i64 @load_i64_by_i8_bswap(i64* %p)
    %e = icmp eq i64 %a, 72623859790382856
    ret i1 %e
}
