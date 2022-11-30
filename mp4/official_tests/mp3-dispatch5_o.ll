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
	%A*,
	%A*,
	%A*,
	%B*,
	%C*
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
	%Main* (%Main*) *
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
	%Main* (%Main*) * @Main.main
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
	%A* (%A*) *,
	i32 (%A*) *
}

@_A_vtable_prototype = constant %_A_vtable {
	i32 6,
	i32 ptrtoint (%A* getelementptr (%A, %A* null, i32 1) to i32),
	i8* getelementptr ([2 x i8], [2 x i8]* @str.A, i32 0, i32 0),
	%A* () * @A_new,
	%Object* (%A*) * bitcast (%Object* (%Object*) * @Object_abort to %Object* (%A*) *),
	%String* (%A*) * bitcast (%String* (%Object*) * @Object_type_name to %String* (%A*) *),
	%A* (%A*) * bitcast (%Object* (%Object*) * @Object_copy to %A* (%A*) *),
	i32 (%A*) * @A.f
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
	%B* (%B*) *,
	i32 (%B*) *
}

@_B_vtable_prototype = constant %_B_vtable {
	i32 7,
	i32 ptrtoint (%B* getelementptr (%B, %B* null, i32 1) to i32),
	i8* getelementptr ([2 x i8], [2 x i8]* @str.B, i32 0, i32 0),
	%B* () * @B_new,
	%Object* (%B*) * bitcast (%Object* (%Object*) * @Object_abort to %Object* (%B*) *),
	%String* (%B*) * bitcast (%String* (%Object*) * @Object_type_name to %String* (%B*) *),
	%B* (%B*) * bitcast (%Object* (%Object*) * @Object_copy to %B* (%B*) *),
	i32 (%B*) * bitcast (i32 (%A*) * @A.f to i32 (%B*) *)
}

@str.C = internal constant [2 x i8] c"C\00"
%C = type {
	%_C_vtable*
}

%_C_vtable = type {
	i32,
	i32,
	i8*,
	%C* () *,
	%Object* (%C*) *,
	%String* (%C*) *,
	%C* (%C*) *,
	i32 (%C*) *
}

@_C_vtable_prototype = constant %_C_vtable {
	i32 8,
	i32 ptrtoint (%C* getelementptr (%C, %C* null, i32 1) to i32),
	i8* getelementptr ([2 x i8], [2 x i8]* @str.C, i32 0, i32 0),
	%C* () * @C_new,
	%Object* (%C*) * bitcast (%Object* (%Object*) * @Object_abort to %Object* (%C*) *),
	%String* (%C*) * bitcast (%String* (%Object*) * @Object_type_name to %String* (%C*) *),
	%C* (%C*) * bitcast (%Object* (%Object*) * @Object_copy to %C* (%C*) *),
	i32 (%C*) * @C.f
}

@str.1 = internal constant [14 x i8] c"<basic class>\00"
@String.1 = constant %String {
	%_String_vtable* @_String_vtable_prototype,
	i8* getelementptr ([14 x i8], [14 x i8]* @str.1, i32 0, i32 0)
}

@str.0 = internal constant [15 x i8] c"dispatch5_o.cl\00"
@String.0 = constant %String {
	%_String_vtable* @_String_vtable_prototype,
	i8* getelementptr ([15 x i8], [15 x i8]* @str.0, i32 0, i32 0)
}

define i32 @main() {
entry:
	%main.obj = call %Main*() @Main_new( )
	%main.retval = call %Main*(%Main*) @Main.main( %Main* %main.obj )
	ret i32 0
}

define %Main* @Main.main(%Main* %self) {

entry:
	%tmp.0 = alloca %Main*
	store %Main* %self, %Main** %tmp.0
	%tmp.1 = load %Main*, %Main** %tmp.0
	%tmp.2 = getelementptr %Main, %Main* %tmp.1, i32 0, i32 1
	%tmp.3 = load %A*, %A** %tmp.2
	%tmp.4 = icmp eq %A* %tmp.3, null
	br i1 %tmp.4, label %abort, label %ok.1

ok.1:
	%tmp.5 = getelementptr %A, %A* %tmp.3, i32 0, i32 0
	%tmp.6 = load %_A_vtable*, %_A_vtable** %tmp.5
	%tmp.7 = getelementptr %_A_vtable, %_A_vtable* %tmp.6, i32 0, i32 7
	%tmp.8 = load i32 (%A*) *, i32 (%A*) ** %tmp.7
	%tmp.9 = call i32(%A* ) %tmp.8( %A* %tmp.3 )
	%tmp.10 = load %Main*, %Main** %tmp.0
	%tmp.11 = icmp eq %Main* %tmp.10, null
	br i1 %tmp.11, label %abort, label %ok.0

ok.0:
	%tmp.12 = getelementptr %Main, %Main* %tmp.10, i32 0, i32 0
	%tmp.13 = load %_Main_vtable*, %_Main_vtable** %tmp.12
	%tmp.14 = getelementptr %_Main_vtable, %_Main_vtable* %tmp.13, i32 0, i32 8
	%tmp.15 = load %Main* (%Main*,i32) *, %Main* (%Main*,i32) ** %tmp.14
	%tmp.16 = call %Main*(%Main*, i32 ) %tmp.15( %Main* %tmp.10, i32 %tmp.9 )
	%tmp.17 = load %Main*, %Main** %tmp.0
	%tmp.18 = getelementptr %Main, %Main* %tmp.17, i32 0, i32 2
	%tmp.19 = load %A*, %A** %tmp.18
	%tmp.20 = icmp eq %A* %tmp.19, null
	br i1 %tmp.20, label %abort, label %ok.3

ok.3:
	%tmp.21 = getelementptr %A, %A* %tmp.19, i32 0, i32 0
	%tmp.22 = load %_A_vtable*, %_A_vtable** %tmp.21
	%tmp.23 = getelementptr %_A_vtable, %_A_vtable* %tmp.22, i32 0, i32 7
	%tmp.24 = load i32 (%A*) *, i32 (%A*) ** %tmp.23
	%tmp.25 = call i32(%A* ) %tmp.24( %A* %tmp.19 )
	%tmp.26 = load %Main*, %Main** %tmp.0
	%tmp.27 = icmp eq %Main* %tmp.26, null
	br i1 %tmp.27, label %abort, label %ok.2

ok.2:
	%tmp.28 = getelementptr %Main, %Main* %tmp.26, i32 0, i32 0
	%tmp.29 = load %_Main_vtable*, %_Main_vtable** %tmp.28
	%tmp.30 = getelementptr %_Main_vtable, %_Main_vtable* %tmp.29, i32 0, i32 8
	%tmp.31 = load %Main* (%Main*,i32) *, %Main* (%Main*,i32) ** %tmp.30
	%tmp.32 = call %Main*(%Main*, i32 ) %tmp.31( %Main* %tmp.26, i32 %tmp.25 )
	%tmp.33 = load %Main*, %Main** %tmp.0
	%tmp.34 = getelementptr %Main, %Main* %tmp.33, i32 0, i32 3
	%tmp.35 = load %A*, %A** %tmp.34
	%tmp.36 = icmp eq %A* %tmp.35, null
	br i1 %tmp.36, label %abort, label %ok.5

ok.5:
	%tmp.37 = getelementptr %A, %A* %tmp.35, i32 0, i32 0
	%tmp.38 = load %_A_vtable*, %_A_vtable** %tmp.37
	%tmp.39 = getelementptr %_A_vtable, %_A_vtable* %tmp.38, i32 0, i32 7
	%tmp.40 = load i32 (%A*) *, i32 (%A*) ** %tmp.39
	%tmp.41 = call i32(%A* ) %tmp.40( %A* %tmp.35 )
	%tmp.42 = load %Main*, %Main** %tmp.0
	%tmp.43 = icmp eq %Main* %tmp.42, null
	br i1 %tmp.43, label %abort, label %ok.4

ok.4:
	%tmp.44 = getelementptr %Main, %Main* %tmp.42, i32 0, i32 0
	%tmp.45 = load %_Main_vtable*, %_Main_vtable** %tmp.44
	%tmp.46 = getelementptr %_Main_vtable, %_Main_vtable* %tmp.45, i32 0, i32 8
	%tmp.47 = load %Main* (%Main*,i32) *, %Main* (%Main*,i32) ** %tmp.46
	%tmp.48 = call %Main*(%Main*, i32 ) %tmp.47( %Main* %tmp.42, i32 %tmp.41 )
	%tmp.49 = load %Main*, %Main** %tmp.0
	%tmp.50 = getelementptr %Main, %Main* %tmp.49, i32 0, i32 5
	%tmp.51 = load %C*, %C** %tmp.50
	%tmp.52 = icmp eq %C* %tmp.51, null
	br i1 %tmp.52, label %abort, label %ok.7

ok.7:
	%tmp.53 = getelementptr %C, %C* %tmp.51, i32 0, i32 0
	%tmp.54 = load %_C_vtable*, %_C_vtable** %tmp.53
	%tmp.55 = getelementptr %_C_vtable, %_C_vtable* %tmp.54, i32 0, i32 7
	%tmp.56 = load i32 (%C*) *, i32 (%C*) ** %tmp.55
	%tmp.57 = call i32(%C* ) %tmp.56( %C* %tmp.51 )
	%tmp.58 = load %Main*, %Main** %tmp.0
	%tmp.59 = icmp eq %Main* %tmp.58, null
	br i1 %tmp.59, label %abort, label %ok.6

ok.6:
	%tmp.60 = getelementptr %Main, %Main* %tmp.58, i32 0, i32 0
	%tmp.61 = load %_Main_vtable*, %_Main_vtable** %tmp.60
	%tmp.62 = getelementptr %_Main_vtable, %_Main_vtable* %tmp.61, i32 0, i32 8
	%tmp.63 = load %Main* (%Main*,i32) *, %Main* (%Main*,i32) ** %tmp.62
	%tmp.64 = call %Main*(%Main*, i32 ) %tmp.63( %Main* %tmp.58, i32 %tmp.57 )
	%tmp.65 = load %Main*, %Main** %tmp.0
	%tmp.66 = getelementptr %Main, %Main* %tmp.65, i32 0, i32 5
	%tmp.67 = load %C*, %C** %tmp.66
	%tmp.68 = icmp eq %C* %tmp.67, null
	br i1 %tmp.68, label %abort, label %ok.9

ok.9:
	%tmp.69 = getelementptr %_C_vtable, %_C_vtable* @_C_vtable_prototype, i32 0, i32 7
	%tmp.70 = load i32 (%C*) *, i32 (%C*) ** %tmp.69
	%tmp.71 = call i32(%C* ) %tmp.70( %C* %tmp.67 )
	%tmp.72 = load %Main*, %Main** %tmp.0
	%tmp.73 = icmp eq %Main* %tmp.72, null
	br i1 %tmp.73, label %abort, label %ok.8

ok.8:
	%tmp.74 = getelementptr %Main, %Main* %tmp.72, i32 0, i32 0
	%tmp.75 = load %_Main_vtable*, %_Main_vtable** %tmp.74
	%tmp.76 = getelementptr %_Main_vtable, %_Main_vtable* %tmp.75, i32 0, i32 8
	%tmp.77 = load %Main* (%Main*,i32) *, %Main* (%Main*,i32) ** %tmp.76
	%tmp.78 = call %Main*(%Main*, i32 ) %tmp.77( %Main* %tmp.72, i32 %tmp.71 )
	%tmp.79 = load %Main*, %Main** %tmp.0
	%tmp.80 = getelementptr %Main, %Main* %tmp.79, i32 0, i32 5
	%tmp.81 = load %C*, %C** %tmp.80
	%tmp.82 = icmp eq %C* %tmp.81, null
	br i1 %tmp.82, label %abort, label %ok.11

ok.11:
	%tmp.83 = getelementptr %_B_vtable, %_B_vtable* @_B_vtable_prototype, i32 0, i32 7
	%tmp.84 = load i32 (%B*) *, i32 (%B*) ** %tmp.83
	%tmp.85 = bitcast %C* %tmp.81 to %B*
	%tmp.86 = call i32(%B* ) %tmp.84( %B* %tmp.85 )
	%tmp.87 = load %Main*, %Main** %tmp.0
	%tmp.88 = icmp eq %Main* %tmp.87, null
	br i1 %tmp.88, label %abort, label %ok.10

ok.10:
	%tmp.89 = getelementptr %Main, %Main* %tmp.87, i32 0, i32 0
	%tmp.90 = load %_Main_vtable*, %_Main_vtable** %tmp.89
	%tmp.91 = getelementptr %_Main_vtable, %_Main_vtable* %tmp.90, i32 0, i32 8
	%tmp.92 = load %Main* (%Main*,i32) *, %Main* (%Main*,i32) ** %tmp.91
	%tmp.93 = call %Main*(%Main*, i32 ) %tmp.92( %Main* %tmp.87, i32 %tmp.86 )
	%tmp.94 = load %Main*, %Main** %tmp.0
	%tmp.95 = getelementptr %Main, %Main* %tmp.94, i32 0, i32 5
	%tmp.96 = load %C*, %C** %tmp.95
	%tmp.97 = icmp eq %C* %tmp.96, null
	br i1 %tmp.97, label %abort, label %ok.13

ok.13:
	%tmp.98 = getelementptr %_A_vtable, %_A_vtable* @_A_vtable_prototype, i32 0, i32 7
	%tmp.99 = load i32 (%A*) *, i32 (%A*) ** %tmp.98
	%tmp.100 = bitcast %C* %tmp.96 to %A*
	%tmp.101 = call i32(%A* ) %tmp.99( %A* %tmp.100 )
	%tmp.102 = load %Main*, %Main** %tmp.0
	%tmp.103 = icmp eq %Main* %tmp.102, null
	br i1 %tmp.103, label %abort, label %ok.12

ok.12:
	%tmp.104 = getelementptr %Main, %Main* %tmp.102, i32 0, i32 0
	%tmp.105 = load %_Main_vtable*, %_Main_vtable** %tmp.104
	%tmp.106 = getelementptr %_Main_vtable, %_Main_vtable* %tmp.105, i32 0, i32 8
	%tmp.107 = load %Main* (%Main*,i32) *, %Main* (%Main*,i32) ** %tmp.106
	%tmp.108 = call %Main*(%Main*, i32 ) %tmp.107( %Main* %tmp.102, i32 %tmp.101 )
	%tmp.109 = load %Main*, %Main** %tmp.0
	%tmp.110 = getelementptr %Main, %Main* %tmp.109, i32 0, i32 4
	%tmp.111 = load %B*, %B** %tmp.110
	%tmp.112 = icmp eq %B* %tmp.111, null
	br i1 %tmp.112, label %abort, label %ok.15

ok.15:
	%tmp.113 = getelementptr %B, %B* %tmp.111, i32 0, i32 0
	%tmp.114 = load %_B_vtable*, %_B_vtable** %tmp.113
	%tmp.115 = getelementptr %_B_vtable, %_B_vtable* %tmp.114, i32 0, i32 7
	%tmp.116 = load i32 (%B*) *, i32 (%B*) ** %tmp.115
	%tmp.117 = call i32(%B* ) %tmp.116( %B* %tmp.111 )
	%tmp.118 = load %Main*, %Main** %tmp.0
	%tmp.119 = icmp eq %Main* %tmp.118, null
	br i1 %tmp.119, label %abort, label %ok.14

ok.14:
	%tmp.120 = getelementptr %Main, %Main* %tmp.118, i32 0, i32 0
	%tmp.121 = load %_Main_vtable*, %_Main_vtable** %tmp.120
	%tmp.122 = getelementptr %_Main_vtable, %_Main_vtable* %tmp.121, i32 0, i32 8
	%tmp.123 = load %Main* (%Main*,i32) *, %Main* (%Main*,i32) ** %tmp.122
	%tmp.124 = call %Main*(%Main*, i32 ) %tmp.123( %Main* %tmp.118, i32 %tmp.117 )
	%tmp.125 = load %Main*, %Main** %tmp.0
	%tmp.126 = getelementptr %Main, %Main* %tmp.125, i32 0, i32 4
	%tmp.127 = load %B*, %B** %tmp.126
	%tmp.128 = icmp eq %B* %tmp.127, null
	br i1 %tmp.128, label %abort, label %ok.17

ok.17:
	%tmp.129 = getelementptr %_B_vtable, %_B_vtable* @_B_vtable_prototype, i32 0, i32 7
	%tmp.130 = load i32 (%B*) *, i32 (%B*) ** %tmp.129
	%tmp.131 = call i32(%B* ) %tmp.130( %B* %tmp.127 )
	%tmp.132 = load %Main*, %Main** %tmp.0
	%tmp.133 = icmp eq %Main* %tmp.132, null
	br i1 %tmp.133, label %abort, label %ok.16

ok.16:
	%tmp.134 = getelementptr %Main, %Main* %tmp.132, i32 0, i32 0
	%tmp.135 = load %_Main_vtable*, %_Main_vtable** %tmp.134
	%tmp.136 = getelementptr %_Main_vtable, %_Main_vtable* %tmp.135, i32 0, i32 8
	%tmp.137 = load %Main* (%Main*,i32) *, %Main* (%Main*,i32) ** %tmp.136
	%tmp.138 = call %Main*(%Main*, i32 ) %tmp.137( %Main* %tmp.132, i32 %tmp.131 )
	%tmp.139 = load %Main*, %Main** %tmp.0
	%tmp.140 = getelementptr %Main, %Main* %tmp.139, i32 0, i32 4
	%tmp.141 = load %B*, %B** %tmp.140
	%tmp.142 = icmp eq %B* %tmp.141, null
	br i1 %tmp.142, label %abort, label %ok.19

ok.19:
	%tmp.143 = getelementptr %_A_vtable, %_A_vtable* @_A_vtable_prototype, i32 0, i32 7
	%tmp.144 = load i32 (%A*) *, i32 (%A*) ** %tmp.143
	%tmp.145 = bitcast %B* %tmp.141 to %A*
	%tmp.146 = call i32(%A* ) %tmp.144( %A* %tmp.145 )
	%tmp.147 = load %Main*, %Main** %tmp.0
	%tmp.148 = icmp eq %Main* %tmp.147, null
	br i1 %tmp.148, label %abort, label %ok.18

ok.18:
	%tmp.149 = getelementptr %Main, %Main* %tmp.147, i32 0, i32 0
	%tmp.150 = load %_Main_vtable*, %_Main_vtable** %tmp.149
	%tmp.151 = getelementptr %_Main_vtable, %_Main_vtable* %tmp.150, i32 0, i32 8
	%tmp.152 = load %Main* (%Main*,i32) *, %Main* (%Main*,i32) ** %tmp.151
	%tmp.153 = call %Main*(%Main*, i32 ) %tmp.152( %Main* %tmp.147, i32 %tmp.146 )
	%tmp.154 = load %Main*, %Main** %tmp.0
	%tmp.155 = getelementptr %Main, %Main* %tmp.154, i32 0, i32 1
	%tmp.156 = load %A*, %A** %tmp.155
	%tmp.157 = icmp eq %A* %tmp.156, null
	br i1 %tmp.157, label %abort, label %ok.21

ok.21:
	%tmp.158 = getelementptr %_A_vtable, %_A_vtable* @_A_vtable_prototype, i32 0, i32 7
	%tmp.159 = load i32 (%A*) *, i32 (%A*) ** %tmp.158
	%tmp.160 = call i32(%A* ) %tmp.159( %A* %tmp.156 )
	%tmp.161 = load %Main*, %Main** %tmp.0
	%tmp.162 = icmp eq %Main* %tmp.161, null
	br i1 %tmp.162, label %abort, label %ok.20

ok.20:
	%tmp.163 = getelementptr %Main, %Main* %tmp.161, i32 0, i32 0
	%tmp.164 = load %_Main_vtable*, %_Main_vtable** %tmp.163
	%tmp.165 = getelementptr %_Main_vtable, %_Main_vtable* %tmp.164, i32 0, i32 8
	%tmp.166 = load %Main* (%Main*,i32) *, %Main* (%Main*,i32) ** %tmp.165
	%tmp.167 = call %Main*(%Main*, i32 ) %tmp.166( %Main* %tmp.161, i32 %tmp.160 )
	ret %Main* %tmp.167

abort:
	call void @abort(  )
	unreachable
}

define %Main* @Main_new() {

entry:
	%tmp.168 = alloca %Main*
	%tmp.169 = getelementptr %_Main_vtable, %_Main_vtable* @_Main_vtable_prototype, i32 0, i32 1
	%tmp.170 = load i32, i32* %tmp.169
	%tmp.171 = call i8*(i32 ) @malloc( i32 %tmp.170 )
	%tmp.172 = bitcast i8* %tmp.171 to %Main*
	%malloc.null = icmp eq %Main* %tmp.172, null
	br i1 %malloc.null, label %abort, label %okay

okay:
	%tmp.173 = getelementptr %Main, %Main* %tmp.172, i32 0, i32 0
	store %_Main_vtable* @_Main_vtable_prototype, %_Main_vtable** %tmp.173
	store %Main* %tmp.172, %Main** %tmp.168
	%tmp.174 = getelementptr %Main, %Main* %tmp.172, i32 0, i32 1
	store %A* null, %A** %tmp.174
	%tmp.175 = getelementptr %Main, %Main* %tmp.172, i32 0, i32 2
	store %A* null, %A** %tmp.175
	%tmp.176 = getelementptr %Main, %Main* %tmp.172, i32 0, i32 3
	store %A* null, %A** %tmp.176
	%tmp.177 = getelementptr %Main, %Main* %tmp.172, i32 0, i32 4
	store %B* null, %B** %tmp.177
	%tmp.178 = getelementptr %Main, %Main* %tmp.172, i32 0, i32 5
	store %C* null, %C** %tmp.178
	%tmp.179 = getelementptr %Main, %Main* %tmp.172, i32 0, i32 1
	%tmp.180 = call %A* @A_new(  )
	store %A* %tmp.180, %A** %tmp.179
	%tmp.181 = getelementptr %Main, %Main* %tmp.172, i32 0, i32 2
	%tmp.182 = call %B* @B_new(  )
	%tmp.183 = bitcast %B* %tmp.182 to %A*
	store %A* %tmp.183, %A** %tmp.181
	%tmp.184 = getelementptr %Main, %Main* %tmp.172, i32 0, i32 3
	%tmp.185 = call %C* @C_new(  )
	%tmp.186 = bitcast %C* %tmp.185 to %A*
	store %A* %tmp.186, %A** %tmp.184
	%tmp.187 = getelementptr %Main, %Main* %tmp.172, i32 0, i32 4
	%tmp.188 = call %B* @B_new(  )
	store %B* %tmp.188, %B** %tmp.187
	%tmp.189 = getelementptr %Main, %Main* %tmp.172, i32 0, i32 5
	%tmp.190 = call %C* @C_new(  )
	store %C* %tmp.190, %C** %tmp.189
	ret %Main* %tmp.172

abort:
	call void @abort(  )
	unreachable
}

define i32 @A.f(%A* %self) {

entry:
	%tmp.0 = alloca %A*
	store %A* %self, %A** %tmp.0
	ret i32 5

abort:
	call void @abort(  )
	unreachable
}

define %A* @A_new() {

entry:
	%tmp.1 = alloca %A*
	%tmp.2 = getelementptr %_A_vtable, %_A_vtable* @_A_vtable_prototype, i32 0, i32 1
	%tmp.3 = load i32, i32* %tmp.2
	%tmp.4 = call i8*(i32 ) @malloc( i32 %tmp.3 )
	%tmp.5 = bitcast i8* %tmp.4 to %A*
	%malloc.null = icmp eq %A* %tmp.5, null
	br i1 %malloc.null, label %abort, label %okay

okay:
	%tmp.6 = getelementptr %A, %A* %tmp.5, i32 0, i32 0
	store %_A_vtable* @_A_vtable_prototype, %_A_vtable** %tmp.6
	store %A* %tmp.5, %A** %tmp.1
	ret %A* %tmp.5

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

define i32 @C.f(%C* %self) {

entry:
	%tmp.0 = alloca %C*
	store %C* %self, %C** %tmp.0
	ret i32 6

abort:
	call void @abort(  )
	unreachable
}

define %C* @C_new() {

entry:
	%tmp.1 = alloca %C*
	%tmp.2 = getelementptr %_C_vtable, %_C_vtable* @_C_vtable_prototype, i32 0, i32 1
	%tmp.3 = load i32, i32* %tmp.2
	%tmp.4 = call i8*(i32 ) @malloc( i32 %tmp.3 )
	%tmp.5 = bitcast i8* %tmp.4 to %C*
	%malloc.null = icmp eq %C* %tmp.5, null
	br i1 %malloc.null, label %abort, label %okay

okay:
	%tmp.6 = getelementptr %C, %C* %tmp.5, i32 0, i32 0
	store %_C_vtable* @_C_vtable_prototype, %_C_vtable** %tmp.6
	store %C* %tmp.5, %C** %tmp.1
	ret %C* %tmp.5

abort:
	call void @abort(  )
	unreachable
}

