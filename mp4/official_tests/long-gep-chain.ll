@g0 = global [64 x i32] zeroinitializer, align 32

define void @long_gep_chain(i32 %a) {
    %g1 = getelementptr inbounds [64 x i32], [64 x i32]* @g0, i64 0, i64 1
    %g2 = getelementptr inbounds i32, i32* %g1, i64 1
    %g3 = getelementptr inbounds i32, i32* %g2, i64 1
    %g4 = getelementptr inbounds i32, i32* %g3, i64 1
    %g5 = getelementptr inbounds i32, i32* %g4, i64 1
    %g6 = getelementptr inbounds i32, i32* %g5, i64 1
    %g7 = getelementptr inbounds i32, i32* %g6, i64 1
    %g8 = getelementptr inbounds i32, i32* %g7, i64 1
    %g9 = getelementptr inbounds i32, i32* %g8, i64 1
    %g10 = getelementptr inbounds i32, i32* %g9, i64 1
    %g11 = getelementptr inbounds i32, i32* %g10, i64 1
    %g12 = getelementptr inbounds i32, i32* %g11, i64 1
    %g13 = getelementptr inbounds i32, i32* %g12, i64 1
    %g14 = getelementptr inbounds i32, i32* %g13, i64 1
    %g15 = getelementptr inbounds i32, i32* %g14, i64 1
    %g16 = getelementptr inbounds i32, i32* %g15, i64 1
    %g17 = getelementptr inbounds i32, i32* %g16, i64 1
    %g18 = getelementptr inbounds i32, i32* %g17, i64 1
    %g19 = getelementptr inbounds i32, i32* %g18, i64 1
    %g20 = getelementptr inbounds i32, i32* %g19, i64 1
    %g21 = getelementptr inbounds i32, i32* %g20, i64 1
    %g22 = getelementptr inbounds i32, i32* %g21, i64 1
    %g23 = getelementptr inbounds i32, i32* %g22, i64 1
    %g24 = getelementptr inbounds i32, i32* %g23, i64 1
    %g25 = getelementptr inbounds i32, i32* %g24, i64 1
    %g26 = getelementptr inbounds i32, i32* %g25, i64 1
    %g27 = getelementptr inbounds i32, i32* %g26, i64 1
    %g28 = getelementptr inbounds i32, i32* %g27, i64 1
    %g29 = getelementptr inbounds i32, i32* %g28, i64 1
    %g30 = getelementptr inbounds i32, i32* %g29, i64 1
    %g31 = getelementptr inbounds i32, i32* %g30, i64 1
    %g32 = getelementptr inbounds i32, i32* %g31, i64 1
    store i32 %a, i32* %g32
    ret void
}

define i32 @main() {
    call void @long_gep_chain(i32 107)
    %a = getelementptr inbounds [64 x i32], [64 x i32]* @g0, i64 0, i64 32
    %b = load i32, i32* %a
    ret i32 %b
}
