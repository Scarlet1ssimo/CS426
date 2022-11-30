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
	i32 (%Main*) * @Main.func,
	%Object* (%Main*) * @Main.main
}

@str.Beta = internal constant [5 x i8] c"Beta\00"
%Beta = type {
	%_Beta_vtable*
}

%_Beta_vtable = type {
	i32,
	i32,
	i8*,
	%Beta* () *,
	%Object* (%Beta*) *,
	%String* (%Beta*) *,
	%Beta* (%Beta*) *,
	i32 (%Beta*) *,
	i32 (%Beta*) *
}

@_Beta_vtable_prototype = constant %_Beta_vtable {
	i32 6,
	i32 ptrtoint (%Beta* getelementptr (%Beta, %Beta* null, i32 1) to i32),
	i8* getelementptr ([5 x i8], [5 x i8]* @str.Beta, i32 0, i32 0),
	%Beta* () * @Beta_new,
	%Object* (%Beta*) * bitcast (%Object* (%Object*) * @Object_abort to %Object* (%Beta*) *),
	%String* (%Beta*) * bitcast (%String* (%Object*) * @Object_type_name to %String* (%Beta*) *),
	%Beta* (%Beta*) * bitcast (%Object* (%Object*) * @Object_copy to %Beta* (%Beta*) *),
	i32 (%Beta*) * @Beta.niam,
	i32 (%Beta*) * @Beta.bugger
}

@str.Alpha = internal constant [6 x i8] c"Alpha\00"
%Alpha = type {
	%_Alpha_vtable*
}

%_Alpha_vtable = type {
	i32,
	i32,
	i8*,
	%Alpha* () *,
	%Object* (%Alpha*) *,
	%String* (%Alpha*) *,
	%Alpha* (%Alpha*) *,
	i32 (%Alpha*) *,
	i32 (%Alpha*) *
}

@_Alpha_vtable_prototype = constant %_Alpha_vtable {
	i32 7,
	i32 ptrtoint (%Alpha* getelementptr (%Alpha, %Alpha* null, i32 1) to i32),
	i8* getelementptr ([6 x i8], [6 x i8]* @str.Alpha, i32 0, i32 0),
	%Alpha* () * @Alpha_new,
	%Object* (%Alpha*) * bitcast (%Object* (%Object*) * @Object_abort to %Object* (%Alpha*) *),
	%String* (%Alpha*) * bitcast (%String* (%Object*) * @Object_type_name to %String* (%Alpha*) *),
	%Alpha* (%Alpha*) * bitcast (%Object* (%Object*) * @Object_copy to %Alpha* (%Alpha*) *),
	i32 (%Alpha*) * @Alpha.niam,
	i32 (%Alpha*) * bitcast (i32 (%Beta*) * @Beta.bugger to i32 (%Alpha*) *)
}

@str.1 = internal constant [14 x i8] c"<basic class>\00"
@String.1 = constant %String {
	%_String_vtable* @_String_vtable_prototype,
	i8* getelementptr ([14 x i8], [14 x i8]* @str.1, i32 0, i32 0)
}

@str.0 = internal constant [15 x i8] c"dispatch4_o.cl\00"
@String.0 = constant %String {
	%_String_vtable* @_String_vtable_prototype,
	i8* getelementptr ([15 x i8], [15 x i8]* @str.0, i32 0, i32 0)
}

define i32 @main() {
entry:
	%main.obj = call %Main*() @Main_new( )
	%main.retval = call %Object*(%Main*) @Main.main( %Main* %main.obj )
	ret i32 0
}

define i32 @Main.func(%Main* %self) {

entry:
	%tmp.0 = alloca %Main*
	store %Main* %self, %Main** %tmp.0
	%tmp.1 = call %Alpha* @Alpha_new(  )
	%tmp.2 = icmp eq %Alpha* %tmp.1, null
	br i1 %tmp.2, label %abort, label %ok.0

ok.0:
	%tmp.3 = getelementptr %_Beta_vtable, %_Beta_vtable* @_Beta_vtable_prototype, i32 0, i32 7
	%tmp.4 = load i32 (%Beta*) *, i32 (%Beta*) ** %tmp.3
	%tmp.5 = bitcast %Alpha* %tmp.1 to %Beta*
	%tmp.6 = call i32(%Beta* ) %tmp.4( %Beta* %tmp.5 )
	ret i32 %tmp.6

abort:
	call void @abort(  )
	unreachable
}

define %Object* @Main.main(%Main* %self) {

entry:
	%tmp.7 = alloca %Main*
	store %Main* %self, %Main** %tmp.7
	%tmp.8 = load %Main*, %Main** %tmp.7
	%tmp.9 = icmp eq %Main* %tmp.8, null
	br i1 %tmp.9, label %abort, label %ok.2

ok.2:
	%tmp.10 = getelementptr %Main, %Main* %tmp.8, i32 0, i32 0
	%tmp.11 = load %_Main_vtable*, %_Main_vtable** %tmp.10
	%tmp.12 = getelementptr %_Main_vtable, %_Main_vtable* %tmp.11, i32 0, i32 11
	%tmp.13 = load i32 (%Main*) *, i32 (%Main*) ** %tmp.12
	%tmp.14 = call i32(%Main* ) %tmp.13( %Main* %tmp.8 )
	%tmp.15 = load %Main*, %Main** %tmp.7
	%tmp.16 = icmp eq %Main* %tmp.15, null
	br i1 %tmp.16, label %abort, label %ok.1

ok.1:
	%tmp.17 = getelementptr %Main, %Main* %tmp.15, i32 0, i32 0
	%tmp.18 = load %_Main_vtable*, %_Main_vtable** %tmp.17
	%tmp.19 = getelementptr %_Main_vtable, %_Main_vtable* %tmp.18, i32 0, i32 8
	%tmp.20 = load %Main* (%Main*,i32) *, %Main* (%Main*,i32) ** %tmp.19
	%tmp.21 = call %Main*(%Main*, i32 ) %tmp.20( %Main* %tmp.15, i32 %tmp.14 )
	%tmp.22 = load %Main*, %Main** %tmp.7
	%tmp.23 = bitcast %Main* %tmp.22 to %Object*
	ret %Object* %tmp.23

abort:
	call void @abort(  )
	unreachable
}

define %Main* @Main_new() {

entry:
	%tmp.24 = alloca %Main*
	%tmp.25 = getelementptr %_Main_vtable, %_Main_vtable* @_Main_vtable_prototype, i32 0, i32 1
	%tmp.26 = load i32, i32* %tmp.25
	%tmp.27 = call i8*(i32 ) @malloc( i32 %tmp.26 )
	%tmp.28 = bitcast i8* %tmp.27 to %Main*
	%malloc.null = icmp eq %Main* %tmp.28, null
	br i1 %malloc.null, label %abort, label %okay

okay:
	%tmp.29 = getelementptr %Main, %Main* %tmp.28, i32 0, i32 0
	store %_Main_vtable* @_Main_vtable_prototype, %_Main_vtable** %tmp.29
	store %Main* %tmp.28, %Main** %tmp.24
	ret %Main* %tmp.28

abort:
	call void @abort(  )
	unreachable
}

define i32 @Beta.niam(%Beta* %self) {

entry:
	%tmp.0 = alloca %Beta*
	store %Beta* %self, %Beta** %tmp.0
	ret i32 42

abort:
	call void @abort(  )
	unreachable
}

define i32 @Beta.bugger(%Beta* %self) {

entry:
	%tmp.1 = alloca %Beta*
	store %Beta* %self, %Beta** %tmp.1
	ret i32 57

abort:
	call void @abort(  )
	unreachable
}

define %Beta* @Beta_new() {

entry:
	%tmp.2 = alloca %Beta*
	%tmp.3 = getelementptr %_Beta_vtable, %_Beta_vtable* @_Beta_vtable_prototype, i32 0, i32 1
	%tmp.4 = load i32, i32* %tmp.3
	%tmp.5 = call i8*(i32 ) @malloc( i32 %tmp.4 )
	%tmp.6 = bitcast i8* %tmp.5 to %Beta*
	%malloc.null = icmp eq %Beta* %tmp.6, null
	br i1 %malloc.null, label %abort, label %okay

okay:
	%tmp.7 = getelementptr %Beta, %Beta* %tmp.6, i32 0, i32 0
	store %_Beta_vtable* @_Beta_vtable_prototype, %_Beta_vtable** %tmp.7
	store %Beta* %tmp.6, %Beta** %tmp.2
	ret %Beta* %tmp.6

abort:
	call void @abort(  )
	unreachable
}

define i32 @Alpha.niam(%Alpha* %self) {

entry:
	%tmp.0 = alloca %Alpha*
	store %Alpha* %self, %Alpha** %tmp.0
	ret i32 90

abort:
	call void @abort(  )
	unreachable
}

define %Alpha* @Alpha_new() {

entry:
	%tmp.1 = alloca %Alpha*
	%tmp.2 = getelementptr %_Alpha_vtable, %_Alpha_vtable* @_Alpha_vtable_prototype, i32 0, i32 1
	%tmp.3 = load i32, i32* %tmp.2
	%tmp.4 = call i8*(i32 ) @malloc( i32 %tmp.3 )
	%tmp.5 = bitcast i8* %tmp.4 to %Alpha*
	%malloc.null = icmp eq %Alpha* %tmp.5, null
	br i1 %malloc.null, label %abort, label %okay

okay:
	%tmp.6 = getelementptr %Alpha, %Alpha* %tmp.5, i32 0, i32 0
	store %_Alpha_vtable* @_Alpha_vtable_prototype, %_Alpha_vtable** %tmp.6
	store %Alpha* %tmp.5, %Alpha** %tmp.1
	ret %Alpha* %tmp.5

abort:
	call void @abort(  )
	unreachable
}

