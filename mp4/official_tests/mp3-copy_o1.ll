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
	%_A_vtable*,
	%String*
}

%_A_vtable = type {
	i32,
	i32,
	i8*,
	%A* () *,
	%Object* (%A*) *,
	%String* (%A*) *,
	%A* (%A*) *,
	%String* (%A*) *
}

@_A_vtable_prototype = constant %_A_vtable {
	i32 6,
	i32 ptrtoint (%A* getelementptr (%A, %A* null, i32 1) to i32),
	i8* getelementptr ([2 x i8], [2 x i8]* @str.A, i32 0, i32 0),
	%A* () * @A_new,
	%Object* (%A*) * bitcast (%Object* (%Object*) * @Object_abort to %Object* (%A*) *),
	%String* (%A*) * bitcast (%String* (%Object*) * @Object_type_name to %String* (%A*) *),
	%A* (%A*) * bitcast (%Object* (%Object*) * @Object_copy to %A* (%A*) *),
	%String* (%A*) * @A.getGreetings
}

@str.2 = internal constant [14 x i8] c"<basic class>\00"
@String.2 = constant %String {
	%_String_vtable* @_String_vtable_prototype,
	i8* getelementptr ([14 x i8], [14 x i8]* @str.2, i32 0, i32 0)
}

@str.1 = internal constant [8 x i8] c"Hello!\0A\00"
@String.1 = constant %String {
	%_String_vtable* @_String_vtable_prototype,
	i8* getelementptr ([8 x i8], [8 x i8]* @str.1, i32 0, i32 0)
}

@str.0 = internal constant [11 x i8] c"copy_o1.cl\00"
@String.0 = constant %String {
	%_String_vtable* @_String_vtable_prototype,
	i8* getelementptr ([11 x i8], [11 x i8]* @str.0, i32 0, i32 0)
}

define i32 @main() {
entry:
	%main.obj = call %Main*() @Main_new( )
	%main.retval = call %Object*(%Main*) @Main.main( %Main* %main.obj )
	ret i32 0
}

define %Object* @Main.main(%Main* %self) {

entry:
	%tmp.1 = alloca %A*
	%tmp.2 = alloca %A*
	%tmp.0 = alloca %Main*
	store %Main* %self, %Main** %tmp.0
	%tmp.3 = call %A* @A_new(  )
	store %A* %tmp.3, %A** %tmp.1
	store %A* null, %A** %tmp.2
	%tmp.4 = load %A*, %A** %tmp.1
	%tmp.5 = icmp eq %A* %tmp.4, null
	br i1 %tmp.5, label %abort, label %ok.0

ok.0:
	%tmp.6 = getelementptr %A, %A* %tmp.4, i32 0, i32 0
	%tmp.7 = load %_A_vtable*, %_A_vtable** %tmp.6
	%tmp.8 = getelementptr %_A_vtable, %_A_vtable* %tmp.7, i32 0, i32 6
	%tmp.9 = load %A* (%A*) *, %A* (%A*) ** %tmp.8
	%tmp.10 = call %A*(%A* ) %tmp.9( %A* %tmp.4 )
	store %A* %tmp.10, %A** %tmp.2
	%tmp.11 = load %A*, %A** %tmp.2
	%tmp.12 = icmp eq %A* %tmp.11, null
	br i1 %tmp.12, label %abort, label %ok.2

ok.2:
	%tmp.13 = getelementptr %A, %A* %tmp.11, i32 0, i32 0
	%tmp.14 = load %_A_vtable*, %_A_vtable** %tmp.13
	%tmp.15 = getelementptr %_A_vtable, %_A_vtable* %tmp.14, i32 0, i32 7
	%tmp.16 = load %String* (%A*) *, %String* (%A*) ** %tmp.15
	%tmp.17 = call %String*(%A* ) %tmp.16( %A* %tmp.11 )
	%tmp.18 = load %Main*, %Main** %tmp.0
	%tmp.19 = icmp eq %Main* %tmp.18, null
	br i1 %tmp.19, label %abort, label %ok.1

ok.1:
	%tmp.20 = getelementptr %Main, %Main* %tmp.18, i32 0, i32 0
	%tmp.21 = load %_Main_vtable*, %_Main_vtable** %tmp.20
	%tmp.22 = getelementptr %_Main_vtable, %_Main_vtable* %tmp.21, i32 0, i32 7
	%tmp.23 = load %Main* (%Main*,%String*) *, %Main* (%Main*,%String*) ** %tmp.22
	%tmp.24 = call %Main*(%Main*, %String* ) %tmp.23( %Main* %tmp.18, %String* %tmp.17 )
	%tmp.25 = bitcast %Main* %tmp.24 to %Object*
	ret %Object* %tmp.25

abort:
	call void @abort(  )
	unreachable
}

define %Main* @Main_new() {

entry:
	%tmp.26 = alloca %Main*
	%tmp.27 = getelementptr %_Main_vtable, %_Main_vtable* @_Main_vtable_prototype, i32 0, i32 1
	%tmp.28 = load i32, i32* %tmp.27
	%tmp.29 = call i8*(i32 ) @malloc( i32 %tmp.28 )
	%tmp.30 = bitcast i8* %tmp.29 to %Main*
	%malloc.null = icmp eq %Main* %tmp.30, null
	br i1 %malloc.null, label %abort, label %okay

okay:
	%tmp.31 = getelementptr %Main, %Main* %tmp.30, i32 0, i32 0
	store %_Main_vtable* @_Main_vtable_prototype, %_Main_vtable** %tmp.31
	store %Main* %tmp.30, %Main** %tmp.26
	ret %Main* %tmp.30

abort:
	call void @abort(  )
	unreachable
}

define %String* @A.getGreetings(%A* %self) {

entry:
	%tmp.0 = alloca %A*
	store %A* %self, %A** %tmp.0
	%tmp.1 = load %A*, %A** %tmp.0
	%tmp.2 = getelementptr %A, %A* %tmp.1, i32 0, i32 1
	%tmp.3 = load %String*, %String** %tmp.2
	ret %String* %tmp.3

abort:
	call void @abort(  )
	unreachable
}

define %A* @A_new() {

entry:
	%tmp.4 = alloca %A*
	%tmp.5 = getelementptr %_A_vtable, %_A_vtable* @_A_vtable_prototype, i32 0, i32 1
	%tmp.6 = load i32, i32* %tmp.5
	%tmp.7 = call i8*(i32 ) @malloc( i32 %tmp.6 )
	%tmp.8 = bitcast i8* %tmp.7 to %A*
	%malloc.null = icmp eq %A* %tmp.8, null
	br i1 %malloc.null, label %abort, label %okay

okay:
	%tmp.9 = getelementptr %A, %A* %tmp.8, i32 0, i32 0
	store %_A_vtable* @_A_vtable_prototype, %_A_vtable** %tmp.9
	store %A* %tmp.8, %A** %tmp.4
	%tmp.10 = getelementptr %A, %A* %tmp.8, i32 0, i32 1
	%tmp.11 = call %String* @String_new(  )
	store %String* %tmp.11, %String** %tmp.10
	%tmp.12 = getelementptr %A, %A* %tmp.8, i32 0, i32 1
	store %String* @String.1, %String** %tmp.12
	ret %A* %tmp.8

abort:
	call void @abort(  )
	unreachable
}

