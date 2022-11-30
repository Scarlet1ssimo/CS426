define void @adc_load_store_64_15(i64* inreg %x, i64* inreg %x2, i64 inreg %y) nounwind {
    %vx   = load i64, i64* %x
    %zvx  = zext i64 %vx to i192
    %szvx = shl i192 %zvx, 64
    %zy   = zext i64 %y to i192
    %op   = or i192 %szvx, %zy
    %zsum = add i192 %op, 276701161105643274241
    %ssum = lshr i192 %zsum, 64
    %val = trunc i192 %ssum to i64
    store i64 %val, i64* %x
    %ssum2 = lshr i192 %zsum, 128
    %val2 = trunc i192 %ssum2 to i64
    store i64 %val2, i64* %x2
    ret void
}

define i64 @main() {
    %p = alloca i64
    %x = alloca i64
    store i64 10, i64* %x
    call void @adc_load_store_64_15(i64* %x, i64* %p, i64 21)
    %a = load i64, i64* %p
    ret i64 %a
}
