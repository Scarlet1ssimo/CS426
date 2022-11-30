declare i32 @strcmp(i8*, i8*)
declare i32 @printf(i8*, ...)
declare void @abort()
declare i8* @malloc(i32)
declare %Object* @Object_new()
declare %Object* @Object_abort(%Object*)
declare %String* @Object_type_name(%Object*)
declare %Object* @Object_copy(%Object*)
declare %IO* @IO_new()
declare %IO* @IO_out_string(%IO*, %String*)
declare %IO* @IO_out_int(%IO*, i32)
declare %String* @IO_in_string(%IO*)
declare i32 @IO_in_int(%IO*)
declare %String* @String_new()
declare i32 @String_length(%String*)
declare %String* @String_concat(%String*, %String*)
declare %String* @String_substr(%String*, i32, i32)
declare %Int* @Int_new()
declare void @Int_init(%Int*, i32)
declare %Bool* @Bool_new()
declare void @Bool_init(%Bool*, i1)
@str.Object = internal constant [7 x i8] c"Object\00"
%Object = type {
	%_Object_vtable*
}

%_Object_vtable = type {
	i32,
	i32,
	i8*,
	%Object* () *,
	%Object* (%Object*) *,
	%String* (%Object*) *,
	%Object* (%Object*) *
}

@_Object_vtable_prototype = constant %_Object_vtable {
	i32 0,
	i32 ptrtoint (%Object* getelementptr (%Object, %Object* null, i32 1) to i32),
	i8* getelementptr ([7 x i8], [7 x i8]* @str.Object, i32 0, i32 0),
	%Object* () * @Object_new,
	%Object* (%Object*) * @Object_abort,
	%String* (%Object*) * @Object_type_name,
	%Object* (%Object*) * @Object_copy
}

@str.Int = internal constant [4 x i8] c"Int\00"
%Int = type {
	%_Int_vtable*,
	i32
}

%_Int_vtable = type {
	i32,
	i32,
	i8*,
	%Int* () *,
	%Object* (%Int*) *,
	%String* (%Int*) *,
	%Int* (%Int*) *
}

@_Int_vtable_prototype = constant %_Int_vtable {
	i32 1,
	i32 ptrtoint (%Int* getelementptr (%Int, %Int* null, i32 1) to i32),
	i8* getelementptr ([4 x i8], [4 x i8]* @str.Int, i32 0, i32 0),
	%Int* () * @Int_new,
	%Object* (%Int*) * bitcast (%Object* (%Object*) * @Object_abort to %Object* (%Int*) *),
	%String* (%Int*) * bitcast (%String* (%Object*) * @Object_type_name to %String* (%Int*) *),
	%Int* (%Int*) * bitcast (%Object* (%Object*) * @Object_copy to %Int* (%Int*) *)
}

@str.Bool = internal constant [5 x i8] c"Bool\00"
%Bool = type {
	%_Bool_vtable*,
	i1
}

%_Bool_vtable = type {
	i32,
	i32,
	i8*,
	%Bool* () *,
	%Object* (%Bool*) *,
	%String* (%Bool*) *,
	%Bool* (%Bool*) *
}

@_Bool_vtable_prototype = constant %_Bool_vtable {
	i32 2,
	i32 ptrtoint (%Bool* getelementptr (%Bool, %Bool* null, i32 1) to i32),
	i8* getelementptr ([5 x i8], [5 x i8]* @str.Bool, i32 0, i32 0),
	%Bool* () * @Bool_new,
	%Object* (%Bool*) * bitcast (%Object* (%Object*) * @Object_abort to %Object* (%Bool*) *),
	%String* (%Bool*) * bitcast (%String* (%Object*) * @Object_type_name to %String* (%Bool*) *),
	%Bool* (%Bool*) * bitcast (%Object* (%Object*) * @Object_copy to %Bool* (%Bool*) *)
}

@str.String = internal constant [7 x i8] c"String\00"
%String = type {
	%_String_vtable*,
	i8*
}

%_String_vtable = type {
	i32,
	i32,
	i8*,
	%String* () *,
	%Object* (%String*) *,
	%String* (%String*) *,
	%String* (%String*) *,
	i32 (%String*) *,
	%String* (%String*,%String*) *,
	%String* (%String*,i32,i32) *
}

@_String_vtable_prototype = constant %_String_vtable {
	i32 3,
	i32 ptrtoint (%String* getelementptr (%String, %String* null, i32 1) to i32),
	i8* getelementptr ([7 x i8], [7 x i8]* @str.String, i32 0, i32 0),
	%String* () * @String_new,
	%Object* (%String*) * bitcast (%Object* (%Object*) * @Object_abort to %Object* (%String*) *),
	%String* (%String*) * bitcast (%String* (%Object*) * @Object_type_name to %String* (%String*) *),
	%String* (%String*) * bitcast (%Object* (%Object*) * @Object_copy to %String* (%String*) *),
	i32 (%String*) * @String_length,
	%String* (%String*,%String*) * @String_concat,
	%String* (%String*,i32,i32) * @String_substr
}

@str.IO = internal constant [3 x i8] c"IO\00"
%IO = type {
	%_IO_vtable*
}

%_IO_vtable = type {
	i32,
	i32,
	i8*,
	%IO* () *,
	%Object* (%IO*) *,
	%String* (%IO*) *,
	%IO* (%IO*) *,
	%IO* (%IO*,%String*) *,
	%IO* (%IO*,i32) *,
	%String* (%IO*) *,
	i32 (%IO*) *
}

@_IO_vtable_prototype = constant %_IO_vtable {
	i32 4,
	i32 ptrtoint (%IO* getelementptr (%IO, %IO* null, i32 1) to i32),
	i8* getelementptr ([3 x i8], [3 x i8]* @str.IO, i32 0, i32 0),
	%IO* () * @IO_new,
	%Object* (%IO*) * bitcast (%Object* (%Object*) * @Object_abort to %Object* (%IO*) *),
	%String* (%IO*) * bitcast (%String* (%Object*) * @Object_type_name to %String* (%IO*) *),
	%IO* (%IO*) * bitcast (%Object* (%Object*) * @Object_copy to %IO* (%IO*) *),
	%IO* (%IO*,%String*) * @IO_out_string,
	%IO* (%IO*,i32) * @IO_out_int,
	%String* (%IO*) * @IO_in_string,
	i32 (%IO*) * @IO_in_int
}

@str.Main = internal constant [5 x i8] c"Main\00"
%Main = type {
	%_Main_vtable*
}

%_Main_vtable = type {
	i32,
	i32,
	i8*,
	%Main* () *,
	%Object* (%Main*) *,
	%String* (%Main*) *,
	%Main* (%Main*) *,
	%Main* (%Main*,%String*) *,
	%Main* (%Main*,i32) *,
	%String* (%Main*) *,
	i32 (%Main*) *,
	%Object* (%Main*) *
}

@_Main_vtable_prototype = constant %_Main_vtable {
	i32 5,
	i32 ptrtoint (%Main* getelementptr (%Main, %Main* null, i32 1) to i32),
	i8* getelementptr ([5 x i8], [5 x i8]* @str.Main, i32 0, i32 0),
	%Main* () * @Main_new,
	%Object* (%Main*) * bitcast (%Object* (%Object*) * @Object_abort to %Object* (%Main*) *),
	%String* (%Main*) * bitcast (%String* (%Object*) * @Object_type_name to %String* (%Main*) *),
	%Main* (%Main*) * bitcast (%Object* (%Object*) * @Object_copy to %Main* (%Main*) *),
	%Main* (%Main*,%String*) * bitcast (%IO* (%IO*,%String*) * @IO_out_string to %Main* (%Main*,%String*) *),
	%Main* (%Main*,i32) * bitcast (%IO* (%IO*,i32) * @IO_out_int to %Main* (%Main*,i32) *),
	%String* (%Main*) * bitcast (%String* (%IO*) * @IO_in_string to %String* (%Main*) *),
	i32 (%Main*) * bitcast (i32 (%IO*) * @IO_in_int to i32 (%Main*) *),
	%Object* (%Main*) * @Main.main
}

@str.A = internal constant [2 x i8] c"A\00"
%A = type {
	%_A_vtable*
}

%_A_vtable = type {
	i32,
	i32,
	i8*,
	%A* () *,
	%Object* (%A*) *,
	%String* (%A*) *,
	%A* (%A*) *
}

@_A_vtable_prototype = constant %_A_vtable {
	i32 6,
	i32 ptrtoint (%A* getelementptr (%A, %A* null, i32 1) to i32),
	i8* getelementptr ([2 x i8], [2 x i8]* @str.A, i32 0, i32 0),
	%A* () * @A_new,
	%Object* (%A*) * bitcast (%Object* (%Object*) * @Object_abort to %Object* (%A*) *),
	%String* (%A*) * bitcast (%String* (%Object*) * @Object_type_name to %String* (%A*) *),
	%A* (%A*) * bitcast (%Object* (%Object*) * @Object_copy to %A* (%A*) *)
}

@str.B = internal constant [2 x i8] c"B\00"
%B = type {
	%_B_vtable*
}

%_B_vtable = type {
	i32,
	i32,
	i8*,
	%B* () *,
	%Object* (%B*) *,
	%String* (%B*) *,
	%B* (%B*) *
}

@_B_vtable_prototype = constant %_B_vtable {
	i32 7,
	i32 ptrtoint (%B* getelementptr (%B, %B* null, i32 1) to i32),
	i8* getelementptr ([2 x i8], [2 x i8]* @str.B, i32 0, i32 0),
	%B* () * @B_new,
	%Object* (%B*) * bitcast (%Object* (%Object*) * @Object_abort to %Object* (%B*) *),
	%String* (%B*) * bitcast (%String* (%Object*) * @Object_type_name to %String* (%B*) *),
	%B* (%B*) * bitcast (%Object* (%Object*) * @Object_copy to %B* (%B*) *)
}

@str.B1 = internal constant [3 x i8] c"B1\00"
%B1 = type {
	%_B1_vtable*
}

%_B1_vtable = type {
	i32,
	i32,
	i8*,
	%B1* () *,
	%Object* (%B1*) *,
	%String* (%B1*) *,
	%B1* (%B1*) *
}

@_B1_vtable_prototype = constant %_B1_vtable {
	i32 8,
	i32 ptrtoint (%B1* getelementptr (%B1, %B1* null, i32 1) to i32),
	i8* getelementptr ([3 x i8], [3 x i8]* @str.B1, i32 0, i32 0),
	%B1* () * @B1_new,
	%Object* (%B1*) * bitcast (%Object* (%Object*) * @Object_abort to %Object* (%B1*) *),
	%String* (%B1*) * bitcast (%String* (%Object*) * @Object_type_name to %String* (%B1*) *),
	%B1* (%B1*) * bitcast (%Object* (%Object*) * @Object_copy to %B1* (%B1*) *)
}

@str.B2 = internal constant [3 x i8] c"B2\00"
%B2 = type {
	%_B2_vtable*
}

%_B2_vtable = type {
	i32,
	i32,
	i8*,
	%B2* () *,
	%Object* (%B2*) *,
	%String* (%B2*) *,
	%B2* (%B2*) *
}

@_B2_vtable_prototype = constant %_B2_vtable {
	i32 9,
	i32 ptrtoint (%B2* getelementptr (%B2, %B2* null, i32 1) to i32),
	i8* getelementptr ([3 x i8], [3 x i8]* @str.B2, i32 0, i32 0),
	%B2* () * @B2_new,
	%Object* (%B2*) * bitcast (%Object* (%Object*) * @Object_abort to %Object* (%B2*) *),
	%String* (%B2*) * bitcast (%String* (%Object*) * @Object_type_name to %String* (%B2*) *),
	%B2* (%B2*) * bitcast (%Object* (%Object*) * @Object_copy to %B2* (%B2*) *)
}

@str.E = internal constant [2 x i8] c"E\00"
%E = type {
	%_E_vtable*
}

%_E_vtable = type {
	i32,
	i32,
	i8*,
	%E* () *,
	%Object* (%E*) *,
	%String* (%E*) *,
	%E* (%E*) *
}

@_E_vtable_prototype = constant %_E_vtable {
	i32 10,
	i32 ptrtoint (%E* getelementptr (%E, %E* null, i32 1) to i32),
	i8* getelementptr ([2 x i8], [2 x i8]* @str.E, i32 0, i32 0),
	%E* () * @E_new,
	%Object* (%E*) * bitcast (%Object* (%Object*) * @Object_abort to %Object* (%E*) *),
	%String* (%E*) * bitcast (%String* (%Object*) * @Object_type_name to %String* (%E*) *),
	%E* (%E*) * bitcast (%Object* (%Object*) * @Object_copy to %E* (%E*) *)
}

@str.F = internal constant [2 x i8] c"F\00"
%F = type {
	%_F_vtable*
}

%_F_vtable = type {
	i32,
	i32,
	i8*,
	%F* () *,
	%Object* (%F*) *,
	%String* (%F*) *,
	%F* (%F*) *
}

@_F_vtable_prototype = constant %_F_vtable {
	i32 11,
	i32 ptrtoint (%F* getelementptr (%F, %F* null, i32 1) to i32),
	i8* getelementptr ([2 x i8], [2 x i8]* @str.F, i32 0, i32 0),
	%F* () * @F_new,
	%Object* (%F*) * bitcast (%Object* (%Object*) * @Object_abort to %Object* (%F*) *),
	%String* (%F*) * bitcast (%String* (%Object*) * @Object_type_name to %String* (%F*) *),
	%F* (%F*) * bitcast (%Object* (%Object*) * @Object_copy to %F* (%F*) *)
}

@str.1 = internal constant [14 x i8] c"<basic class>\00"
@String.1 = constant %String {
	%_String_vtable* @_String_vtable_prototype,
	i8* getelementptr ([14 x i8], [14 x i8]* @str.1, i32 0, i32 0)
}

@str.0 = internal constant [12 x i8] c"caseskip.cl\00"
@String.0 = constant %String {
	%_String_vtable* @_String_vtable_prototype,
	i8* getelementptr ([12 x i8], [12 x i8]* @str.0, i32 0, i32 0)
}

define i32 @main() {
entry:
	%main.obj = call %Main*() @Main_new( )
	%main.retval = call %Object*(%Main*) @Main.main( %Main* %main.obj )
	ret i32 0
}

define %Object* @Main.main(%Main* %self) {

entry:
	%tmp.1 = alloca %B*
	%tmp.2 = alloca %Object*
	%tmp.3 = alloca i32
	%tmp.4 = alloca %Int*
	%vtpm.0 = alloca %A*
	%vtpm.1 = alloca %B1*
	%vtpm.2 = alloca %E*
	%tmp.0 = alloca %Main*
	store %Main* %self, %Main** %tmp.0
	%tmp.5 = call %B* @B_new(  )
	store %B* %tmp.5, %B** %tmp.1
	store %Object* null, %Object** %tmp.2
	store i32 0, i32* %tmp.3
	%tmp.6 = load %B*, %B** %tmp.1
	%tmp.7 = icmp eq %B* %tmp.6, null
	br i1 %tmp.7, label %abort, label %ok.1

ok.1:
	%tmp.8 = getelementptr %B, %B* %tmp.6, i32 0, i32 0
	%tmp.9 = load %_B_vtable*, %_B_vtable** %tmp.8
	%tmp.10 = getelementptr %_B_vtable, %_B_vtable* %tmp.9, i32 0, i32 0
	%tmp.11 = load i32, i32* %tmp.10
	br label %case.hdr.0

case.hdr.0:
	br label %case.10.0

case.10.0:
	%vtpm.3 = icmp slt i32 %tmp.11, 10
	br i1 %vtpm.3, label %br_exit.10.0, label %src_gte_br.10.0

src_gte_br.10.0:
	%vtpm.4 = icmp sgt i32 %tmp.11, 10
	br i1 %vtpm.4, label %br_exit.10.0, label %src_lte_mc.10.0

src_lte_mc.10.0:
	%tmp.12 = bitcast %B* %tmp.6 to %E*
	store %E* %tmp.12, %E** %vtpm.2
	%tmp.14 = call %Int* @Int_new(  )
	call void(%Int*, i32 ) @Int_init( %Int* %tmp.14, i32 17 )
	store %Int* %tmp.14, %Int** %tmp.4
	br label %case.exit.0

br_exit.10.0:
	br label %case.8.0

case.8.0:
	%vtpm.5 = icmp slt i32 %tmp.11, 8
	br i1 %vtpm.5, label %br_exit.8.0, label %src_gte_br.8.0

src_gte_br.8.0:
	%vtpm.6 = icmp sgt i32 %tmp.11, 8
	br i1 %vtpm.6, label %br_exit.8.0, label %src_lte_mc.8.0

src_lte_mc.8.0:
	%tmp.15 = bitcast %B* %tmp.6 to %B1*
	store %B1* %tmp.15, %B1** %vtpm.1
	%tmp.17 = call %Int* @Int_new(  )
	call void(%Int*, i32 ) @Int_init( %Int* %tmp.17, i32 3 )
	store %Int* %tmp.17, %Int** %tmp.4
	br label %case.exit.0

br_exit.8.0:
	br label %case.6.0

case.6.0:
	%vtpm.7 = icmp slt i32 %tmp.11, 6
	br i1 %vtpm.7, label %br_exit.6.0, label %src_gte_br.6.0

src_gte_br.6.0:
	%vtpm.8 = icmp sgt i32 %tmp.11, 11
	br i1 %vtpm.8, label %br_exit.6.0, label %src_lte_mc.6.0

src_lte_mc.6.0:
	%tmp.18 = bitcast %B* %tmp.6 to %A*
	store %A* %tmp.18, %A** %vtpm.0
	%tmp.20 = call %Int* @Int_new(  )
	call void(%Int*, i32 ) @Int_init( %Int* %tmp.20, i32 2 )
	store %Int* %tmp.20, %Int** %tmp.4
	br label %case.exit.0

br_exit.6.0:
	br label %abort

case.exit.0:
	%tmp.21 = load %Int*, %Int** %tmp.4
	%tmp.22 = load %Main*, %Main** %tmp.0
	%tmp.23 = icmp eq %Main* %tmp.22, null
	br i1 %tmp.23, label %abort, label %ok.0

ok.0:
	%tmp.24 = getelementptr %Main, %Main* %tmp.22, i32 0, i32 0
	%tmp.25 = load %_Main_vtable*, %_Main_vtable** %tmp.24
	%tmp.26 = getelementptr %_Main_vtable, %_Main_vtable* %tmp.25, i32 0, i32 8
	%tmp.27 = load %Main* (%Main*,i32) *, %Main* (%Main*,i32) ** %tmp.26
	%tmp.29 = getelementptr %Int, %Int* %tmp.21, i32 0, i32 1
	%tmp.28 = load i32, i32* %tmp.29
	%tmp.30 = call %Main*(%Main*, i32 ) %tmp.27( %Main* %tmp.22, i32 %tmp.28 )
	%tmp.31 = bitcast %Main* %tmp.30 to %Object*
	ret %Object* %tmp.31

abort:
	call void @abort(  )
	unreachable
}

define %Main* @Main_new() {

entry:
	%tmp.32 = alloca %Main*
	%tmp.33 = getelementptr %_Main_vtable, %_Main_vtable* @_Main_vtable_prototype, i32 0, i32 1
	%tmp.34 = load i32, i32* %tmp.33
	%tmp.35 = call i8*(i32 ) @malloc( i32 %tmp.34 )
	%tmp.36 = bitcast i8* %tmp.35 to %Main*
	%malloc.null = icmp eq %Main* %tmp.36, null
	br i1 %malloc.null, label %abort, label %okay

okay:
	%tmp.37 = getelementptr %Main, %Main* %tmp.36, i32 0, i32 0
	store %_Main_vtable* @_Main_vtable_prototype, %_Main_vtable** %tmp.37
	store %Main* %tmp.36, %Main** %tmp.32
	ret %Main* %tmp.36

abort:
	call void @abort(  )
	unreachable
}

define %A* @A_new() {

entry:
	%tmp.0 = alloca %A*
	%tmp.1 = getelementptr %_A_vtable, %_A_vtable* @_A_vtable_prototype, i32 0, i32 1
	%tmp.2 = load i32, i32* %tmp.1
	%tmp.3 = call i8*(i32 ) @malloc( i32 %tmp.2 )
	%tmp.4 = bitcast i8* %tmp.3 to %A*
	%malloc.null = icmp eq %A* %tmp.4, null
	br i1 %malloc.null, label %abort, label %okay

okay:
	%tmp.5 = getelementptr %A, %A* %tmp.4, i32 0, i32 0
	store %_A_vtable* @_A_vtable_prototype, %_A_vtable** %tmp.5
	store %A* %tmp.4, %A** %tmp.0
	ret %A* %tmp.4

abort:
	call void @abort(  )
	unreachable
}

define %B* @B_new() {

entry:
	%tmp.0 = alloca %B*
	%tmp.1 = getelementptr %_B_vtable, %_B_vtable* @_B_vtable_prototype, i32 0, i32 1
	%tmp.2 = load i32, i32* %tmp.1
	%tmp.3 = call i8*(i32 ) @malloc( i32 %tmp.2 )
	%tmp.4 = bitcast i8* %tmp.3 to %B*
	%malloc.null = icmp eq %B* %tmp.4, null
	br i1 %malloc.null, label %abort, label %okay

okay:
	%tmp.5 = getelementptr %B, %B* %tmp.4, i32 0, i32 0
	store %_B_vtable* @_B_vtable_prototype, %_B_vtable** %tmp.5
	store %B* %tmp.4, %B** %tmp.0
	ret %B* %tmp.4

abort:
	call void @abort(  )
	unreachable
}

define %B1* @B1_new() {

entry:
	%tmp.0 = alloca %B1*
	%tmp.1 = getelementptr %_B1_vtable, %_B1_vtable* @_B1_vtable_prototype, i32 0, i32 1
	%tmp.2 = load i32, i32* %tmp.1
	%tmp.3 = call i8*(i32 ) @malloc( i32 %tmp.2 )
	%tmp.4 = bitcast i8* %tmp.3 to %B1*
	%malloc.null = icmp eq %B1* %tmp.4, null
	br i1 %malloc.null, label %abort, label %okay

okay:
	%tmp.5 = getelementptr %B1, %B1* %tmp.4, i32 0, i32 0
	store %_B1_vtable* @_B1_vtable_prototype, %_B1_vtable** %tmp.5
	store %B1* %tmp.4, %B1** %tmp.0
	ret %B1* %tmp.4

abort:
	call void @abort(  )
	unreachable
}

define %B2* @B2_new() {

entry:
	%tmp.0 = alloca %B2*
	%tmp.1 = getelementptr %_B2_vtable, %_B2_vtable* @_B2_vtable_prototype, i32 0, i32 1
	%tmp.2 = load i32, i32* %tmp.1
	%tmp.3 = call i8*(i32 ) @malloc( i32 %tmp.2 )
	%tmp.4 = bitcast i8* %tmp.3 to %B2*
	%malloc.null = icmp eq %B2* %tmp.4, null
	br i1 %malloc.null, label %abort, label %okay

okay:
	%tmp.5 = getelementptr %B2, %B2* %tmp.4, i32 0, i32 0
	store %_B2_vtable* @_B2_vtable_prototype, %_B2_vtable** %tmp.5
	store %B2* %tmp.4, %B2** %tmp.0
	ret %B2* %tmp.4

abort:
	call void @abort(  )
	unreachable
}

define %E* @E_new() {

entry:
	%tmp.0 = alloca %E*
	%tmp.1 = getelementptr %_E_vtable, %_E_vtable* @_E_vtable_prototype, i32 0, i32 1
	%tmp.2 = load i32, i32* %tmp.1
	%tmp.3 = call i8*(i32 ) @malloc( i32 %tmp.2 )
	%tmp.4 = bitcast i8* %tmp.3 to %E*
	%malloc.null = icmp eq %E* %tmp.4, null
	br i1 %malloc.null, label %abort, label %okay

okay:
	%tmp.5 = getelementptr %E, %E* %tmp.4, i32 0, i32 0
	store %_E_vtable* @_E_vtable_prototype, %_E_vtable** %tmp.5
	store %E* %tmp.4, %E** %tmp.0
	ret %E* %tmp.4

abort:
	call void @abort(  )
	unreachable
}

define %F* @F_new() {

entry:
	%tmp.0 = alloca %F*
	%tmp.1 = getelementptr %_F_vtable, %_F_vtable* @_F_vtable_prototype, i32 0, i32 1
	%tmp.2 = load i32, i32* %tmp.1
	%tmp.3 = call i8*(i32 ) @malloc( i32 %tmp.2 )
	%tmp.4 = bitcast i8* %tmp.3 to %F*
	%malloc.null = icmp eq %F* %tmp.4, null
	br i1 %malloc.null, label %abort, label %okay

okay:
	%tmp.5 = getelementptr %F, %F* %tmp.4, i32 0, i32 0
	store %_F_vtable* @_F_vtable_prototype, %_F_vtable** %tmp.5
	store %F* %tmp.4, %F** %tmp.0
	ret %F* %tmp.4

abort:
	call void @abort(  )
	unreachable
}

