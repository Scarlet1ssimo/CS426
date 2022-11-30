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

@str.A = internal constant [2 x i8] c"A\00"
%A = type {
	%_A_vtable*,
	%Object*,
	i32
}

%_A_vtable = type {
	i32,
	i32,
	i8*,
	%A* () *,
	%Object* (%A*) *,
	%String* (%A*) *,
	%A* (%A*) *,
	%A* (%A*,%String*) *,
	%A* (%A*,i32) *,
	%String* (%A*) *,
	i32 (%A*) *,
	%Object* (%A*) *
}

@_A_vtable_prototype = constant %_A_vtable {
	i32 5,
	i32 ptrtoint (%A* getelementptr (%A, %A* null, i32 1) to i32),
	i8* getelementptr ([2 x i8], [2 x i8]* @str.A, i32 0, i32 0),
	%A* () * @A_new,
	%Object* (%A*) * bitcast (%Object* (%Object*) * @Object_abort to %Object* (%A*) *),
	%String* (%A*) * bitcast (%String* (%Object*) * @Object_type_name to %String* (%A*) *),
	%A* (%A*) * bitcast (%Object* (%Object*) * @Object_copy to %A* (%A*) *),
	%A* (%A*,%String*) * bitcast (%IO* (%IO*,%String*) * @IO_out_string to %A* (%A*,%String*) *),
	%A* (%A*,i32) * bitcast (%IO* (%IO*,i32) * @IO_out_int to %A* (%A*,i32) *),
	%String* (%A*) * bitcast (%String* (%IO*) * @IO_in_string to %String* (%A*) *),
	i32 (%A*) * bitcast (i32 (%IO*) * @IO_in_int to i32 (%A*) *),
	%Object* (%A*) * @A.f
}

@str.B = internal constant [2 x i8] c"B\00"
%B = type {
	%_B_vtable*,
	%Object*,
	i32,
	i32
}

%_B_vtable = type {
	i32,
	i32,
	i8*,
	%B* () *,
	%Object* (%B*) *,
	%String* (%B*) *,
	%B* (%B*) *,
	%B* (%B*,%String*) *,
	%B* (%B*,i32) *,
	%String* (%B*) *,
	i32 (%B*) *,
	%Object* (%B*) *
}

@_B_vtable_prototype = constant %_B_vtable {
	i32 6,
	i32 ptrtoint (%B* getelementptr (%B, %B* null, i32 1) to i32),
	i8* getelementptr ([2 x i8], [2 x i8]* @str.B, i32 0, i32 0),
	%B* () * @B_new,
	%Object* (%B*) * bitcast (%Object* (%Object*) * @Object_abort to %Object* (%B*) *),
	%String* (%B*) * bitcast (%String* (%Object*) * @Object_type_name to %String* (%B*) *),
	%B* (%B*) * bitcast (%Object* (%Object*) * @Object_copy to %B* (%B*) *),
	%B* (%B*,%String*) * bitcast (%IO* (%IO*,%String*) * @IO_out_string to %B* (%B*,%String*) *),
	%B* (%B*,i32) * bitcast (%IO* (%IO*,i32) * @IO_out_int to %B* (%B*,i32) *),
	%String* (%B*) * bitcast (%String* (%IO*) * @IO_in_string to %String* (%B*) *),
	i32 (%B*) * bitcast (i32 (%IO*) * @IO_in_int to i32 (%B*) *),
	%Object* (%B*) * @B.f
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
	i32 7,
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

@str.7 = internal constant [14 x i8] c"<basic class>\00"
@String.7 = constant %String {
	%_String_vtable* @_String_vtable_prototype,
	i8* getelementptr ([14 x i8], [14 x i8]* @str.7, i32 0, i32 0)
}

@str.6 = internal constant [7 x i8] c"new B\0A\00"
@String.6 = constant %String {
	%_String_vtable* @_String_vtable_prototype,
	i8* getelementptr ([7 x i8], [7 x i8]* @str.6, i32 0, i32 0)
}

@str.5 = internal constant [7 x i8] c"new A\0A\00"
@String.5 = constant %String {
	%_String_vtable* @_String_vtable_prototype,
	i8* getelementptr ([7 x i8], [7 x i8]* @str.5, i32 0, i32 0)
}

@str.4 = internal constant [10 x i8] c"B.f: y = \00"
@String.4 = constant %String {
	%_String_vtable* @_String_vtable_prototype,
	i8* getelementptr ([10 x i8], [10 x i8]* @str.4, i32 0, i32 0)
}

@str.3 = internal constant [28 x i8] c"initializing B.y. here x = \00"
@String.3 = constant %String {
	%_String_vtable* @_String_vtable_prototype,
	i8* getelementptr ([28 x i8], [28 x i8]* @str.3, i32 0, i32 0)
}

@str.2 = internal constant [2 x i8] c"\0A\00"
@String.2 = constant %String {
	%_String_vtable* @_String_vtable_prototype,
	i8* getelementptr ([2 x i8], [2 x i8]* @str.2, i32 0, i32 0)
}

@str.1 = internal constant [10 x i8] c"A.f: x = \00"
@String.1 = constant %String {
	%_String_vtable* @_String_vtable_prototype,
	i8* getelementptr ([10 x i8], [10 x i8]* @str.1, i32 0, i32 0)
}

@str.0 = internal constant [19 x i8] c"initializer-inh.cl\00"
@String.0 = constant %String {
	%_String_vtable* @_String_vtable_prototype,
	i8* getelementptr ([19 x i8], [19 x i8]* @str.0, i32 0, i32 0)
}

define i32 @main() {
entry:
	%main.obj = call %Main*() @Main_new( )
	%main.retval = call %Object*(%Main*) @Main.main( %Main* %main.obj )
	ret i32 0
}

define %Object* @A.f(%A* %self) {

entry:
	%tmp.0 = alloca %A*
	store %A* %self, %A** %tmp.0
	%tmp.1 = load %A*, %A** %tmp.0
	%tmp.2 = icmp eq %A* %tmp.1, null
	br i1 %tmp.2, label %abort, label %ok.0

ok.0:
	%tmp.3 = getelementptr %A, %A* %tmp.1, i32 0, i32 0
	%tmp.4 = load %_A_vtable*, %_A_vtable** %tmp.3
	%tmp.5 = getelementptr %_A_vtable, %_A_vtable* %tmp.4, i32 0, i32 7
	%tmp.6 = load %A* (%A*,%String*) *, %A* (%A*,%String*) ** %tmp.5
	%tmp.7 = call %A*(%A*, %String* ) %tmp.6( %A* %tmp.1, %String* @String.1 )
	%tmp.8 = load %A*, %A** %tmp.0
	%tmp.9 = getelementptr %A, %A* %tmp.8, i32 0, i32 2
	%tmp.10 = load i32, i32* %tmp.9
	%tmp.11 = load %A*, %A** %tmp.0
	%tmp.12 = icmp eq %A* %tmp.11, null
	br i1 %tmp.12, label %abort, label %ok.1

ok.1:
	%tmp.13 = getelementptr %A, %A* %tmp.11, i32 0, i32 0
	%tmp.14 = load %_A_vtable*, %_A_vtable** %tmp.13
	%tmp.15 = getelementptr %_A_vtable, %_A_vtable* %tmp.14, i32 0, i32 8
	%tmp.16 = load %A* (%A*,i32) *, %A* (%A*,i32) ** %tmp.15
	%tmp.17 = call %A*(%A*, i32 ) %tmp.16( %A* %tmp.11, i32 %tmp.10 )
	%tmp.18 = load %A*, %A** %tmp.0
	%tmp.19 = icmp eq %A* %tmp.18, null
	br i1 %tmp.19, label %abort, label %ok.2

ok.2:
	%tmp.20 = getelementptr %A, %A* %tmp.18, i32 0, i32 0
	%tmp.21 = load %_A_vtable*, %_A_vtable** %tmp.20
	%tmp.22 = getelementptr %_A_vtable, %_A_vtable* %tmp.21, i32 0, i32 7
	%tmp.23 = load %A* (%A*,%String*) *, %A* (%A*,%String*) ** %tmp.22
	%tmp.24 = call %A*(%A*, %String* ) %tmp.23( %A* %tmp.18, %String* @String.2 )
	%tmp.25 = bitcast %A* %tmp.24 to %Object*
	ret %Object* %tmp.25

abort:
	call void @abort(  )
	unreachable
}

define %A* @A_new() {

entry:
	%tmp.26 = alloca %A*
	%tmp.27 = getelementptr %_A_vtable, %_A_vtable* @_A_vtable_prototype, i32 0, i32 1
	%tmp.28 = load i32, i32* %tmp.27
	%tmp.29 = call i8*(i32 ) @malloc( i32 %tmp.28 )
	%tmp.30 = bitcast i8* %tmp.29 to %A*
	%malloc.null = icmp eq %A* %tmp.30, null
	br i1 %malloc.null, label %abort, label %okay

okay:
	%tmp.31 = getelementptr %A, %A* %tmp.30, i32 0, i32 0
	store %_A_vtable* @_A_vtable_prototype, %_A_vtable** %tmp.31
	store %A* %tmp.30, %A** %tmp.26
	%tmp.32 = getelementptr %A, %A* %tmp.30, i32 0, i32 1
	store %Object* null, %Object** %tmp.32
	%tmp.33 = getelementptr %A, %A* %tmp.30, i32 0, i32 2
	store i32 0, i32* %tmp.33
	%tmp.34 = getelementptr %A, %A* %tmp.30, i32 0, i32 1
	%tmp.35 = load %A*, %A** %tmp.26
	%tmp.36 = icmp eq %A* %tmp.35, null
	br i1 %tmp.36, label %abort, label %ok.3

ok.3:
	%tmp.37 = getelementptr %A, %A* %tmp.35, i32 0, i32 0
	%tmp.38 = load %_A_vtable*, %_A_vtable** %tmp.37
	%tmp.39 = getelementptr %_A_vtable, %_A_vtable* %tmp.38, i32 0, i32 11
	%tmp.40 = load %Object* (%A*) *, %Object* (%A*) ** %tmp.39
	%tmp.41 = call %Object*(%A* ) %tmp.40( %A* %tmp.35 )
	store %Object* %tmp.41, %Object** %tmp.34
	%tmp.42 = getelementptr %A, %A* %tmp.30, i32 0, i32 2
	store i32 12, i32* %tmp.42
	ret %A* %tmp.30

abort:
	call void @abort(  )
	unreachable
}

define %Object* @B.f(%B* %self) {

entry:
	%tmp.0 = alloca %B*
	store %B* %self, %B** %tmp.0
	%tmp.1 = load %B*, %B** %tmp.0
	%tmp.2 = icmp eq %B* %tmp.1, null
	br i1 %tmp.2, label %abort, label %ok.0

ok.0:
	%tmp.3 = getelementptr %B, %B* %tmp.1, i32 0, i32 0
	%tmp.4 = load %_B_vtable*, %_B_vtable** %tmp.3
	%tmp.5 = getelementptr %_B_vtable, %_B_vtable* %tmp.4, i32 0, i32 7
	%tmp.6 = load %B* (%B*,%String*) *, %B* (%B*,%String*) ** %tmp.5
	%tmp.7 = call %B*(%B*, %String* ) %tmp.6( %B* %tmp.1, %String* @String.4 )
	%tmp.8 = load %B*, %B** %tmp.0
	%tmp.9 = getelementptr %B, %B* %tmp.8, i32 0, i32 3
	%tmp.10 = load i32, i32* %tmp.9
	%tmp.11 = load %B*, %B** %tmp.0
	%tmp.12 = icmp eq %B* %tmp.11, null
	br i1 %tmp.12, label %abort, label %ok.1

ok.1:
	%tmp.13 = getelementptr %B, %B* %tmp.11, i32 0, i32 0
	%tmp.14 = load %_B_vtable*, %_B_vtable** %tmp.13
	%tmp.15 = getelementptr %_B_vtable, %_B_vtable* %tmp.14, i32 0, i32 8
	%tmp.16 = load %B* (%B*,i32) *, %B* (%B*,i32) ** %tmp.15
	%tmp.17 = call %B*(%B*, i32 ) %tmp.16( %B* %tmp.11, i32 %tmp.10 )
	%tmp.18 = load %B*, %B** %tmp.0
	%tmp.19 = icmp eq %B* %tmp.18, null
	br i1 %tmp.19, label %abort, label %ok.2

ok.2:
	%tmp.20 = getelementptr %B, %B* %tmp.18, i32 0, i32 0
	%tmp.21 = load %_B_vtable*, %_B_vtable** %tmp.20
	%tmp.22 = getelementptr %_B_vtable, %_B_vtable* %tmp.21, i32 0, i32 7
	%tmp.23 = load %B* (%B*,%String*) *, %B* (%B*,%String*) ** %tmp.22
	%tmp.24 = call %B*(%B*, %String* ) %tmp.23( %B* %tmp.18, %String* @String.2 )
	%tmp.25 = bitcast %B* %tmp.24 to %Object*
	ret %Object* %tmp.25

abort:
	call void @abort(  )
	unreachable
}

define %B* @B_new() {

entry:
	%tmp.26 = alloca %B*
	%tmp.27 = getelementptr %_B_vtable, %_B_vtable* @_B_vtable_prototype, i32 0, i32 1
	%tmp.28 = load i32, i32* %tmp.27
	%tmp.29 = call i8*(i32 ) @malloc( i32 %tmp.28 )
	%tmp.30 = bitcast i8* %tmp.29 to %B*
	%malloc.null = icmp eq %B* %tmp.30, null
	br i1 %malloc.null, label %abort, label %okay

okay:
	%tmp.31 = getelementptr %B, %B* %tmp.30, i32 0, i32 0
	store %_B_vtable* @_B_vtable_prototype, %_B_vtable** %tmp.31
	store %B* %tmp.30, %B** %tmp.26
	%tmp.32 = getelementptr %B, %B* %tmp.30, i32 0, i32 1
	store %Object* null, %Object** %tmp.32
	%tmp.33 = getelementptr %B, %B* %tmp.30, i32 0, i32 2
	store i32 0, i32* %tmp.33
	%tmp.34 = getelementptr %B, %B* %tmp.30, i32 0, i32 3
	store i32 0, i32* %tmp.34
	%tmp.35 = getelementptr %B, %B* %tmp.30, i32 0, i32 1
	%tmp.36 = load %B*, %B** %tmp.26
	%tmp.37 = icmp eq %B* %tmp.36, null
	br i1 %tmp.37, label %abort, label %ok.3

ok.3:
	%tmp.38 = getelementptr %B, %B* %tmp.36, i32 0, i32 0
	%tmp.39 = load %_B_vtable*, %_B_vtable** %tmp.38
	%tmp.40 = getelementptr %_B_vtable, %_B_vtable* %tmp.39, i32 0, i32 11
	%tmp.41 = load %Object* (%B*) *, %Object* (%B*) ** %tmp.40
	%tmp.42 = call %Object*(%B* ) %tmp.41( %B* %tmp.36 )
	store %Object* %tmp.42, %Object** %tmp.35
	%tmp.43 = getelementptr %B, %B* %tmp.30, i32 0, i32 2
	store i32 12, i32* %tmp.43
	%tmp.44 = getelementptr %B, %B* %tmp.30, i32 0, i32 3
	%tmp.45 = load %B*, %B** %tmp.26
	%tmp.46 = icmp eq %B* %tmp.45, null
	br i1 %tmp.46, label %abort, label %ok.4

ok.4:
	%tmp.47 = getelementptr %B, %B* %tmp.45, i32 0, i32 0
	%tmp.48 = load %_B_vtable*, %_B_vtable** %tmp.47
	%tmp.49 = getelementptr %_B_vtable, %_B_vtable* %tmp.48, i32 0, i32 7
	%tmp.50 = load %B* (%B*,%String*) *, %B* (%B*,%String*) ** %tmp.49
	%tmp.51 = call %B*(%B*, %String* ) %tmp.50( %B* %tmp.45, %String* @String.3 )
	%tmp.52 = load %B*, %B** %tmp.26
	%tmp.53 = getelementptr %B, %B* %tmp.52, i32 0, i32 2
	%tmp.54 = load i32, i32* %tmp.53
	%tmp.55 = load %B*, %B** %tmp.26
	%tmp.56 = icmp eq %B* %tmp.55, null
	br i1 %tmp.56, label %abort, label %ok.5

ok.5:
	%tmp.57 = getelementptr %B, %B* %tmp.55, i32 0, i32 0
	%tmp.58 = load %_B_vtable*, %_B_vtable** %tmp.57
	%tmp.59 = getelementptr %_B_vtable, %_B_vtable* %tmp.58, i32 0, i32 8
	%tmp.60 = load %B* (%B*,i32) *, %B* (%B*,i32) ** %tmp.59
	%tmp.61 = call %B*(%B*, i32 ) %tmp.60( %B* %tmp.55, i32 %tmp.54 )
	%tmp.62 = load %B*, %B** %tmp.26
	%tmp.63 = icmp eq %B* %tmp.62, null
	br i1 %tmp.63, label %abort, label %ok.6

ok.6:
	%tmp.64 = getelementptr %B, %B* %tmp.62, i32 0, i32 0
	%tmp.65 = load %_B_vtable*, %_B_vtable** %tmp.64
	%tmp.66 = getelementptr %_B_vtable, %_B_vtable* %tmp.65, i32 0, i32 7
	%tmp.67 = load %B* (%B*,%String*) *, %B* (%B*,%String*) ** %tmp.66
	%tmp.68 = call %B*(%B*, %String* ) %tmp.67( %B* %tmp.62, %String* @String.2 )
	store i32 13, i32* %tmp.44
	ret %B* %tmp.30

abort:
	call void @abort(  )
	unreachable
}

define %Object* @Main.main(%Main* %self) {

entry:
	%tmp.0 = alloca %Main*
	store %Main* %self, %Main** %tmp.0
	%tmp.1 = load %Main*, %Main** %tmp.0
	%tmp.2 = icmp eq %Main* %tmp.1, null
	br i1 %tmp.2, label %abort, label %ok.0

ok.0:
	%tmp.3 = getelementptr %Main, %Main* %tmp.1, i32 0, i32 0
	%tmp.4 = load %_Main_vtable*, %_Main_vtable** %tmp.3
	%tmp.5 = getelementptr %_Main_vtable, %_Main_vtable* %tmp.4, i32 0, i32 7
	%tmp.6 = load %Main* (%Main*,%String*) *, %Main* (%Main*,%String*) ** %tmp.5
	%tmp.7 = call %Main*(%Main*, %String* ) %tmp.6( %Main* %tmp.1, %String* @String.5 )
	%tmp.8 = call %A* @A_new(  )
	%tmp.9 = load %Main*, %Main** %tmp.0
	%tmp.10 = icmp eq %Main* %tmp.9, null
	br i1 %tmp.10, label %abort, label %ok.1

ok.1:
	%tmp.11 = getelementptr %Main, %Main* %tmp.9, i32 0, i32 0
	%tmp.12 = load %_Main_vtable*, %_Main_vtable** %tmp.11
	%tmp.13 = getelementptr %_Main_vtable, %_Main_vtable* %tmp.12, i32 0, i32 7
	%tmp.14 = load %Main* (%Main*,%String*) *, %Main* (%Main*,%String*) ** %tmp.13
	%tmp.15 = call %Main*(%Main*, %String* ) %tmp.14( %Main* %tmp.9, %String* @String.6 )
	%tmp.16 = call %B* @B_new(  )
	%tmp.17 = bitcast %B* %tmp.16 to %Object*
	ret %Object* %tmp.17

abort:
	call void @abort(  )
	unreachable
}

define %Main* @Main_new() {

entry:
	%tmp.18 = alloca %Main*
	%tmp.19 = getelementptr %_Main_vtable, %_Main_vtable* @_Main_vtable_prototype, i32 0, i32 1
	%tmp.20 = load i32, i32* %tmp.19
	%tmp.21 = call i8*(i32 ) @malloc( i32 %tmp.20 )
	%tmp.22 = bitcast i8* %tmp.21 to %Main*
	%malloc.null = icmp eq %Main* %tmp.22, null
	br i1 %malloc.null, label %abort, label %okay

okay:
	%tmp.23 = getelementptr %Main, %Main* %tmp.22, i32 0, i32 0
	store %_Main_vtable* @_Main_vtable_prototype, %_Main_vtable** %tmp.23
	store %Main* %tmp.22, %Main** %tmp.18
	ret %Main* %tmp.22

abort:
	call void @abort(  )
	unreachable
}

