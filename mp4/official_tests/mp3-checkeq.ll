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
	%_Main_vtable*,
	%Object*
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

@str.11 = internal constant [14 x i8] c"<basic class>\00"
@String.11 = constant %String {
	%_String_vtable* @_String_vtable_prototype,
	i8* getelementptr ([14 x i8], [14 x i8]* @str.11, i32 0, i32 0)
}

@str.10 = internal constant [16 x i8] c"\22abc\22 != \22bbc\22\0A\00"
@String.10 = constant %String {
	%_String_vtable* @_String_vtable_prototype,
	i8* getelementptr ([16 x i8], [16 x i8]* @str.10, i32 0, i32 0)
}

@str.9 = internal constant [15 x i8] c"\22abc\22 = \22bbc\22\0A\00"
@String.9 = constant %String {
	%_String_vtable* @_String_vtable_prototype,
	i8* getelementptr ([15 x i8], [15 x i8]* @str.9, i32 0, i32 0)
}

@str.8 = internal constant [4 x i8] c"bbc\00"
@String.8 = constant %String {
	%_String_vtable* @_String_vtable_prototype,
	i8* getelementptr ([4 x i8], [4 x i8]* @str.8, i32 0, i32 0)
}

@str.7 = internal constant [4 x i8] c"abc\00"
@String.7 = constant %String {
	%_String_vtable* @_String_vtable_prototype,
	i8* getelementptr ([4 x i8], [4 x i8]* @str.7, i32 0, i32 0)
}

@str.6 = internal constant [15 x i8] c"true != false\0A\00"
@String.6 = constant %String {
	%_String_vtable* @_String_vtable_prototype,
	i8* getelementptr ([15 x i8], [15 x i8]* @str.6, i32 0, i32 0)
}

@str.5 = internal constant [14 x i8] c"true = false\0A\00"
@String.5 = constant %String {
	%_String_vtable* @_String_vtable_prototype,
	i8* getelementptr ([14 x i8], [14 x i8]* @str.5, i32 0, i32 0)
}

@str.4 = internal constant [8 x i8] c"1 != 0\0A\00"
@String.4 = constant %String {
	%_String_vtable* @_String_vtable_prototype,
	i8* getelementptr ([8 x i8], [8 x i8]* @str.4, i32 0, i32 0)
}

@str.3 = internal constant [7 x i8] c"1 = 0\0A\00"
@String.3 = constant %String {
	%_String_vtable* @_String_vtable_prototype,
	i8* getelementptr ([7 x i8], [7 x i8]* @str.3, i32 0, i32 0)
}

@str.2 = internal constant [13 x i8] c"self != obj\0A\00"
@String.2 = constant %String {
	%_String_vtable* @_String_vtable_prototype,
	i8* getelementptr ([13 x i8], [13 x i8]* @str.2, i32 0, i32 0)
}

@str.1 = internal constant [12 x i8] c"self = obj\0A\00"
@String.1 = constant %String {
	%_String_vtable* @_String_vtable_prototype,
	i8* getelementptr ([12 x i8], [12 x i8]* @str.1, i32 0, i32 0)
}

@str.0 = internal constant [11 x i8] c"checkeq.cl\00"
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
	%tmp.1 = alloca %Main*
	%tmp.2 = alloca %Main*
	%tmp.3 = alloca %Main*
	%tmp.4 = alloca %Main*
	%tmp.0 = alloca %Main*
	store %Main* %self, %Main** %tmp.0
	%tmp.5 = load %Main*, %Main** %tmp.0
	%tmp.6 = load %Main*, %Main** %tmp.0
	%tmp.7 = getelementptr %Main, %Main* %tmp.6, i32 0, i32 1
	%tmp.8 = load %Object*, %Object** %tmp.7
	%tmp.10 = bitcast %Main* %tmp.5 to i8*
	%tmp.11 = bitcast %Object* %tmp.8 to i8*
	%tmp.9 = icmp eq i8* %tmp.10, %tmp.11
	br i1 %tmp.9, label %true.0, label %false.0

true.0:
	%tmp.12 = load %Main*, %Main** %tmp.0
	%tmp.13 = icmp eq %Main* %tmp.12, null
	br i1 %tmp.13, label %abort, label %ok.0

ok.0:
	%tmp.14 = getelementptr %Main, %Main* %tmp.12, i32 0, i32 0
	%tmp.15 = load %_Main_vtable*, %_Main_vtable** %tmp.14
	%tmp.16 = getelementptr %_Main_vtable, %_Main_vtable* %tmp.15, i32 0, i32 7
	%tmp.17 = load %Main* (%Main*,%String*) *, %Main* (%Main*,%String*) ** %tmp.16
	%tmp.18 = call %Main*(%Main*, %String* ) %tmp.17( %Main* %tmp.12, %String* @String.1 )
	store %Main* %tmp.18, %Main** %tmp.1
	br label %end.0

false.0:
	%tmp.19 = load %Main*, %Main** %tmp.0
	%tmp.20 = icmp eq %Main* %tmp.19, null
	br i1 %tmp.20, label %abort, label %ok.1

ok.1:
	%tmp.21 = getelementptr %Main, %Main* %tmp.19, i32 0, i32 0
	%tmp.22 = load %_Main_vtable*, %_Main_vtable** %tmp.21
	%tmp.23 = getelementptr %_Main_vtable, %_Main_vtable* %tmp.22, i32 0, i32 7
	%tmp.24 = load %Main* (%Main*,%String*) *, %Main* (%Main*,%String*) ** %tmp.23
	%tmp.25 = call %Main*(%Main*, %String* ) %tmp.24( %Main* %tmp.19, %String* @String.2 )
	store %Main* %tmp.25, %Main** %tmp.1
	br label %end.0

end.0:
	%tmp.26 = load %Main*, %Main** %tmp.1
	%tmp.27 = icmp eq i32 1, 0
	br i1 %tmp.27, label %true.1, label %false.1

true.1:
	%tmp.28 = load %Main*, %Main** %tmp.0
	%tmp.29 = icmp eq %Main* %tmp.28, null
	br i1 %tmp.29, label %abort, label %ok.2

ok.2:
	%tmp.30 = getelementptr %Main, %Main* %tmp.28, i32 0, i32 0
	%tmp.31 = load %_Main_vtable*, %_Main_vtable** %tmp.30
	%tmp.32 = getelementptr %_Main_vtable, %_Main_vtable* %tmp.31, i32 0, i32 7
	%tmp.33 = load %Main* (%Main*,%String*) *, %Main* (%Main*,%String*) ** %tmp.32
	%tmp.34 = call %Main*(%Main*, %String* ) %tmp.33( %Main* %tmp.28, %String* @String.3 )
	store %Main* %tmp.34, %Main** %tmp.2
	br label %end.1

false.1:
	%tmp.35 = load %Main*, %Main** %tmp.0
	%tmp.36 = icmp eq %Main* %tmp.35, null
	br i1 %tmp.36, label %abort, label %ok.3

ok.3:
	%tmp.37 = getelementptr %Main, %Main* %tmp.35, i32 0, i32 0
	%tmp.38 = load %_Main_vtable*, %_Main_vtable** %tmp.37
	%tmp.39 = getelementptr %_Main_vtable, %_Main_vtable* %tmp.38, i32 0, i32 7
	%tmp.40 = load %Main* (%Main*,%String*) *, %Main* (%Main*,%String*) ** %tmp.39
	%tmp.41 = call %Main*(%Main*, %String* ) %tmp.40( %Main* %tmp.35, %String* @String.4 )
	store %Main* %tmp.41, %Main** %tmp.2
	br label %end.1

end.1:
	%tmp.42 = load %Main*, %Main** %tmp.2
	%tmp.43 = icmp eq i1 true, false
	br i1 %tmp.43, label %true.2, label %false.2

true.2:
	%tmp.44 = load %Main*, %Main** %tmp.0
	%tmp.45 = icmp eq %Main* %tmp.44, null
	br i1 %tmp.45, label %abort, label %ok.4

ok.4:
	%tmp.46 = getelementptr %Main, %Main* %tmp.44, i32 0, i32 0
	%tmp.47 = load %_Main_vtable*, %_Main_vtable** %tmp.46
	%tmp.48 = getelementptr %_Main_vtable, %_Main_vtable* %tmp.47, i32 0, i32 7
	%tmp.49 = load %Main* (%Main*,%String*) *, %Main* (%Main*,%String*) ** %tmp.48
	%tmp.50 = call %Main*(%Main*, %String* ) %tmp.49( %Main* %tmp.44, %String* @String.5 )
	store %Main* %tmp.50, %Main** %tmp.3
	br label %end.2

false.2:
	%tmp.51 = load %Main*, %Main** %tmp.0
	%tmp.52 = icmp eq %Main* %tmp.51, null
	br i1 %tmp.52, label %abort, label %ok.5

ok.5:
	%tmp.53 = getelementptr %Main, %Main* %tmp.51, i32 0, i32 0
	%tmp.54 = load %_Main_vtable*, %_Main_vtable** %tmp.53
	%tmp.55 = getelementptr %_Main_vtable, %_Main_vtable* %tmp.54, i32 0, i32 7
	%tmp.56 = load %Main* (%Main*,%String*) *, %Main* (%Main*,%String*) ** %tmp.55
	%tmp.57 = call %Main*(%Main*, %String* ) %tmp.56( %Main* %tmp.51, %String* @String.6 )
	store %Main* %tmp.57, %Main** %tmp.3
	br label %end.2

end.2:
	%tmp.58 = load %Main*, %Main** %tmp.3
	%tmp.62 = getelementptr %String, %String* @String.7, i32 0, i32 1
	%tmp.60 = load i8*, i8** %tmp.62
	%tmp.63 = getelementptr %String, %String* @String.8, i32 0, i32 1
	%tmp.61 = load i8*, i8** %tmp.63
	%tmp.64 = call i32(i8*, i8* ) @strcmp( i8* %tmp.60, i8* %tmp.61 )
	%tmp.59 = icmp eq i32 %tmp.64, 0
	br i1 %tmp.59, label %true.3, label %false.3

true.3:
	%tmp.65 = load %Main*, %Main** %tmp.0
	%tmp.66 = icmp eq %Main* %tmp.65, null
	br i1 %tmp.66, label %abort, label %ok.6

ok.6:
	%tmp.67 = getelementptr %Main, %Main* %tmp.65, i32 0, i32 0
	%tmp.68 = load %_Main_vtable*, %_Main_vtable** %tmp.67
	%tmp.69 = getelementptr %_Main_vtable, %_Main_vtable* %tmp.68, i32 0, i32 7
	%tmp.70 = load %Main* (%Main*,%String*) *, %Main* (%Main*,%String*) ** %tmp.69
	%tmp.71 = call %Main*(%Main*, %String* ) %tmp.70( %Main* %tmp.65, %String* @String.9 )
	store %Main* %tmp.71, %Main** %tmp.4
	br label %end.3

false.3:
	%tmp.72 = load %Main*, %Main** %tmp.0
	%tmp.73 = icmp eq %Main* %tmp.72, null
	br i1 %tmp.73, label %abort, label %ok.7

ok.7:
	%tmp.74 = getelementptr %Main, %Main* %tmp.72, i32 0, i32 0
	%tmp.75 = load %_Main_vtable*, %_Main_vtable** %tmp.74
	%tmp.76 = getelementptr %_Main_vtable, %_Main_vtable* %tmp.75, i32 0, i32 7
	%tmp.77 = load %Main* (%Main*,%String*) *, %Main* (%Main*,%String*) ** %tmp.76
	%tmp.78 = call %Main*(%Main*, %String* ) %tmp.77( %Main* %tmp.72, %String* @String.10 )
	store %Main* %tmp.78, %Main** %tmp.4
	br label %end.3

end.3:
	%tmp.79 = load %Main*, %Main** %tmp.4
	%tmp.80 = bitcast %Main* %tmp.79 to %Object*
	ret %Object* %tmp.80

abort:
	call void @abort(  )
	unreachable
}

define %Main* @Main_new() {

entry:
	%tmp.81 = alloca %Main*
	%tmp.82 = getelementptr %_Main_vtable, %_Main_vtable* @_Main_vtable_prototype, i32 0, i32 1
	%tmp.83 = load i32, i32* %tmp.82
	%tmp.84 = call i8*(i32 ) @malloc( i32 %tmp.83 )
	%tmp.85 = bitcast i8* %tmp.84 to %Main*
	%malloc.null = icmp eq %Main* %tmp.85, null
	br i1 %malloc.null, label %abort, label %okay

okay:
	%tmp.86 = getelementptr %Main, %Main* %tmp.85, i32 0, i32 0
	store %_Main_vtable* @_Main_vtable_prototype, %_Main_vtable** %tmp.86
	store %Main* %tmp.85, %Main** %tmp.81
	%tmp.87 = getelementptr %Main, %Main* %tmp.85, i32 0, i32 1
	store %Object* null, %Object** %tmp.87
	%tmp.88 = getelementptr %Main, %Main* %tmp.85, i32 0, i32 1
	store %Object* null, %Object** %tmp.88
	ret %Main* %tmp.85

abort:
	call void @abort(  )
	unreachable
}

