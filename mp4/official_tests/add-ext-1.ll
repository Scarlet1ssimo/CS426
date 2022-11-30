define i64 @add_nsw_consts(i32 %i) {
    %add = add nsw i32 %i, 5
    %ext = sext i32 %add to i64
    %idx = add i64 %ext, 7
    ret i64 %idx
}

define i64 @main() {
    %a = call i64 @add_nsw_consts(i32 0)
    ret i64 %a
}
