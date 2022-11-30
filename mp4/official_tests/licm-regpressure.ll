%struct.A = type { i32, i32, i32, i32, i32, i32, i32 }

define i32 @assign(i32* %arg) {
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

define void @test(i1 %b, %struct.A* %a) {
entry:
  br label %loop-header

loop-header:
  br label %loop-body

loop-body:
  %0 = getelementptr inbounds %struct.A, %struct.A* %a, i64 0, i32 0
  %1 = getelementptr inbounds %struct.A, %struct.A* %a, i64 0, i32 1
  %2 = getelementptr inbounds %struct.A, %struct.A* %a, i64 0, i32 2
  %3 = getelementptr inbounds %struct.A, %struct.A* %a, i64 0, i32 3
  %4 = getelementptr inbounds %struct.A, %struct.A* %a, i64 0, i32 4
  %5 = getelementptr inbounds %struct.A, %struct.A* %a, i64 0, i32 5
  %6 = getelementptr inbounds %struct.A, %struct.A* %a, i64 0, i32 6
  call i32 @assign(i32* %0)
  call i32 @assign(i32* %1)
  call i32 @assign(i32* %2)
  call i32 @assign(i32* %3)
  call i32 @assign(i32* %4)
  call i32 @assign(i32* %5)
  call i32 @assign(i32* %6)
  br i1 %b, label %loop-body, label %loop-exit

loop-exit:
  ret void
}

define i32 @main() {
    %a = alloca %struct.A
    %1 = getelementptr inbounds %struct.A, %struct.A* %a, i64 0, i32 0
    %2 = getelementptr inbounds %struct.A, %struct.A* %a, i64 0, i32 1
    %3 = getelementptr inbounds %struct.A, %struct.A* %a, i64 0, i32 2
    %4 = getelementptr inbounds %struct.A, %struct.A* %a, i64 0, i32 3
    %5 = getelementptr inbounds %struct.A, %struct.A* %a, i64 0, i32 4
    %6 = getelementptr inbounds %struct.A, %struct.A* %a, i64 0, i32 5
    %7 = getelementptr inbounds %struct.A, %struct.A* %a, i64 0, i32 6
    store i32 105, i32* %1
    store i32 106, i32* %2
    store i32 107, i32* %3
    store i32 108, i32* %4
    store i32 109, i32* %5
    store i32 100, i32* %6
    store i32 101, i32* %7
    call void @test(i1 0, %struct.A* %a)
    ret i32 102
}
