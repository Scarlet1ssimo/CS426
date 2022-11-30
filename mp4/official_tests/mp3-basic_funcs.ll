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

@str.2 = internal constant [14 x i8] c"<basic class>\00"
@String.2 = constant %String {
	%_String_vtable* @_String_vtable_prototype,
	i8* getelementptr ([14 x i8], [14 x i8]* @str.2, i32 0, i32 0)
}

@str.1 = internal constant [7 x i8] c"Frugel\00"
@String.1 = constant %String {
	%_String_vtable* @_String_vtable_prototype,
	i8* getelementptr ([7 x i8], [7 x i8]* @str.1, i32 0, i32 0)
}

@str.0 = internal constant [15 x i8] c"basic_funcs.cl\00"
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

define %Object* @Main.main(%Main* %self) {

entry:
	%tmp.0 = alloca %Main*
	store %Main* %self, %Main** %tmp.0
	%tmp.1 = load %Main*, %Main** %tmp.0
	%tmp.2 = icmp eq %Main* %tmp.1, null
	br i1 %tmp.2, label %abort, label %ok.1

ok.1:
	%tmp.3 = getelementptr %Main, %Main* %tmp.1, i32 0, i32 0
	%tmp.4 = load %_Main_vtable*, %_Main_vtable** %tmp.3
	%tmp.5 = getelementptr %_Main_vtable, %_Main_vtable* %tmp.4, i32 0, i32 5
	%tmp.6 = load %String* (%Main*) *, %String* (%Main*) ** %tmp.5
	%tmp.7 = call %String*(%Main* ) %tmp.6( %Main* %tmp.1 )
	%tmp.8 = load %Main*, %Main** %tmp.0
	%tmp.9 = icmp eq %Main* %tmp.8, null
	br i1 %tmp.9, label %abort, label %ok.0

ok.0:
	%tmp.10 = getelementptr %Main, %Main* %tmp.8, i32 0, i32 0
	%tmp.11 = load %_Main_vtable*, %_Main_vtable** %tmp.10
	%tmp.12 = getelementptr %_Main_vtable, %_Main_vtable* %tmp.11, i32 0, i32 7
	%tmp.13 = load %Main* (%Main*,%String*) *, %Main* (%Main*,%String*) ** %tmp.12
	%tmp.14 = call %Main*(%Main*, %String* ) %tmp.13( %Main* %tmp.8, %String* %tmp.7 )
	%tmp.15 = load %Main*, %Main** %tmp.0
	%tmp.16 = icmp eq %Main* %tmp.15, null
	br i1 %tmp.16, label %abort, label %ok.2

ok.2:
	%tmp.17 = getelementptr %Main, %Main* %tmp.15, i32 0, i32 0
	%tmp.18 = load %_Main_vtable*, %_Main_vtable** %tmp.17
	%tmp.19 = getelementptr %_Main_vtable, %_Main_vtable* %tmp.18, i32 0, i32 8
	%tmp.20 = load %Main* (%Main*,i32) *, %Main* (%Main*,i32) ** %tmp.19
	%tmp.21 = call %Main*(%Main*, i32 ) %tmp.20( %Main* %tmp.15, i32 879 )
	%tmp.22 = load %Main*, %Main** %tmp.0
	%tmp.23 = icmp eq %Main* %tmp.22, null
	br i1 %tmp.23, label %abort, label %ok.5

ok.5:
	%tmp.24 = getelementptr %Main, %Main* %tmp.22, i32 0, i32 0
	%tmp.25 = load %_Main_vtable*, %_Main_vtable** %tmp.24
	%tmp.26 = getelementptr %_Main_vtable, %_Main_vtable* %tmp.25, i32 0, i32 5
	%tmp.27 = load %String* (%Main*) *, %String* (%Main*) ** %tmp.26
	%tmp.28 = call %String*(%Main* ) %tmp.27( %Main* %tmp.22 )
	%tmp.29 = icmp eq %String* %tmp.28, null
	br i1 %tmp.29, label %abort, label %ok.4

ok.4:
	%tmp.30 = getelementptr %String, %String* %tmp.28, i32 0, i32 0
	%tmp.31 = load %_String_vtable*, %_String_vtable** %tmp.30
	%tmp.32 = getelementptr %_String_vtable, %_String_vtable* %tmp.31, i32 0, i32 8
	%tmp.33 = load %String* (%String*,%String*) *, %String* (%String*,%String*) ** %tmp.32
	%tmp.34 = call %String*(%String*, %String* ) %tmp.33( %String* %tmp.28, %String* @String.1 )
	%tmp.35 = load %Main*, %Main** %tmp.0
	%tmp.36 = icmp eq %Main* %tmp.35, null
	br i1 %tmp.36, label %abort, label %ok.3

ok.3:
	%tmp.37 = getelementptr %Main, %Main* %tmp.35, i32 0, i32 0
	%tmp.38 = load %_Main_vtable*, %_Main_vtable** %tmp.37
	%tmp.39 = getelementptr %_Main_vtable, %_Main_vtable* %tmp.38, i32 0, i32 7
	%tmp.40 = load %Main* (%Main*,%String*) *, %Main* (%Main*,%String*) ** %tmp.39
	%tmp.41 = call %Main*(%Main*, %String* ) %tmp.40( %Main* %tmp.35, %String* %tmp.34 )
	%tmp.42 = load %Main*, %Main** %tmp.0
	%tmp.43 = icmp eq %Main* %tmp.42, null
	br i1 %tmp.43, label %abort, label %ok.8

ok.8:
	%tmp.44 = getelementptr %Main, %Main* %tmp.42, i32 0, i32 0
	%tmp.45 = load %_Main_vtable*, %_Main_vtable** %tmp.44
	%tmp.46 = getelementptr %_Main_vtable, %_Main_vtable* %tmp.45, i32 0, i32 5
	%tmp.47 = load %String* (%Main*) *, %String* (%Main*) ** %tmp.46
	%tmp.48 = call %String*(%Main* ) %tmp.47( %Main* %tmp.42 )
	%tmp.49 = icmp eq %String* %tmp.48, null
	br i1 %tmp.49, label %abort, label %ok.7

ok.7:
	%tmp.50 = getelementptr %String, %String* %tmp.48, i32 0, i32 0
	%tmp.51 = load %_String_vtable*, %_String_vtable** %tmp.50
	%tmp.52 = getelementptr %_String_vtable, %_String_vtable* %tmp.51, i32 0, i32 7
	%tmp.53 = load i32 (%String*) *, i32 (%String*) ** %tmp.52
	%tmp.54 = call i32(%String* ) %tmp.53( %String* %tmp.48 )
	%tmp.55 = load %Main*, %Main** %tmp.0
	%tmp.56 = icmp eq %Main* %tmp.55, null
	br i1 %tmp.56, label %abort, label %ok.6

ok.6:
	%tmp.57 = getelementptr %Main, %Main* %tmp.55, i32 0, i32 0
	%tmp.58 = load %_Main_vtable*, %_Main_vtable** %tmp.57
	%tmp.59 = getelementptr %_Main_vtable, %_Main_vtable* %tmp.58, i32 0, i32 8
	%tmp.60 = load %Main* (%Main*,i32) *, %Main* (%Main*,i32) ** %tmp.59
	%tmp.61 = call %Main*(%Main*, i32 ) %tmp.60( %Main* %tmp.55, i32 %tmp.54 )
	%tmp.62 = load %Main*, %Main** %tmp.0
	%tmp.63 = icmp eq %Main* %tmp.62, null
	br i1 %tmp.63, label %abort, label %ok.11

ok.11:
	%tmp.64 = getelementptr %Main, %Main* %tmp.62, i32 0, i32 0
	%tmp.65 = load %_Main_vtable*, %_Main_vtable** %tmp.64
	%tmp.66 = getelementptr %_Main_vtable, %_Main_vtable* %tmp.65, i32 0, i32 5
	%tmp.67 = load %String* (%Main*) *, %String* (%Main*) ** %tmp.66
	%tmp.68 = call %String*(%Main* ) %tmp.67( %Main* %tmp.62 )
	%tmp.69 = icmp eq %String* %tmp.68, null
	br i1 %tmp.69, label %abort, label %ok.10

ok.10:
	%tmp.70 = getelementptr %String, %String* %tmp.68, i32 0, i32 0
	%tmp.71 = load %_String_vtable*, %_String_vtable** %tmp.70
	%tmp.72 = getelementptr %_String_vtable, %_String_vtable* %tmp.71, i32 0, i32 9
	%tmp.73 = load %String* (%String*,i32,i32) *, %String* (%String*,i32,i32) ** %tmp.72
	%tmp.74 = call %String*(%String*, i32, i32 ) %tmp.73( %String* %tmp.68, i32 2, i32 2 )
	%tmp.75 = load %Main*, %Main** %tmp.0
	%tmp.76 = icmp eq %Main* %tmp.75, null
	br i1 %tmp.76, label %abort, label %ok.9

ok.9:
	%tmp.77 = getelementptr %Main, %Main* %tmp.75, i32 0, i32 0
	%tmp.78 = load %_Main_vtable*, %_Main_vtable** %tmp.77
	%tmp.79 = getelementptr %_Main_vtable, %_Main_vtable* %tmp.78, i32 0, i32 7
	%tmp.80 = load %Main* (%Main*,%String*) *, %Main* (%Main*,%String*) ** %tmp.79
	%tmp.81 = call %Main*(%Main*, %String* ) %tmp.80( %Main* %tmp.75, %String* %tmp.74 )
	%tmp.82 = load %Main*, %Main** %tmp.0
	%tmp.83 = bitcast %Main* %tmp.82 to %Object*
	ret %Object* %tmp.83

abort:
	call void @abort(  )
	unreachable
}

define %Main* @Main_new() {

entry:
	%tmp.84 = alloca %Main*
	%tmp.85 = getelementptr %_Main_vtable, %_Main_vtable* @_Main_vtable_prototype, i32 0, i32 1
	%tmp.86 = load i32, i32* %tmp.85
	%tmp.87 = call i8*(i32 ) @malloc( i32 %tmp.86 )
	%tmp.88 = bitcast i8* %tmp.87 to %Main*
	%malloc.null = icmp eq %Main* %tmp.88, null
	br i1 %malloc.null, label %abort, label %okay

okay:
	%tmp.89 = getelementptr %Main, %Main* %tmp.88, i32 0, i32 0
	store %_Main_vtable* @_Main_vtable_prototype, %_Main_vtable** %tmp.89
	store %Main* %tmp.88, %Main** %tmp.84
	ret %Main* %tmp.88

abort:
	call void @abort(  )
	unreachable
}

