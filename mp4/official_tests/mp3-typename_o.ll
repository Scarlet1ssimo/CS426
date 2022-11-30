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
	%String* (%Main*) *,
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
	%String* (%Main*) * @Main.func,
	%Object* (%Main*) * @Main.main
}

@str.Base = internal constant [5 x i8] c"Base\00"
%Base = type {
	%_Base_vtable*
}

%_Base_vtable = type {
	i32,
	i32,
	i8*,
	%Base* () *,
	%Object* (%Base*) *,
	%String* (%Base*) *,
	%Base* (%Base*) *,
	%String* (%Base*,%Object*,%String*) *,
	%String* (%Base*,%String*) *
}

@_Base_vtable_prototype = constant %_Base_vtable {
	i32 6,
	i32 ptrtoint (%Base* getelementptr (%Base, %Base* null, i32 1) to i32),
	i8* getelementptr ([5 x i8], [5 x i8]* @str.Base, i32 0, i32 0),
	%Base* () * @Base_new,
	%Object* (%Base*) * bitcast (%Object* (%Object*) * @Object_abort to %Object* (%Base*) *),
	%String* (%Base*) * bitcast (%String* (%Object*) * @Object_type_name to %String* (%Base*) *),
	%Base* (%Base*) * bitcast (%Object* (%Object*) * @Object_copy to %Base* (%Base*) *),
	%String* (%Base*,%Object*,%String*) * @Base.identify,
	%String* (%Base*,%String*) * @Base.test
}

@str.Derived = internal constant [8 x i8] c"Derived\00"
%Derived = type {
	%_Derived_vtable*
}

%_Derived_vtable = type {
	i32,
	i32,
	i8*,
	%Derived* () *,
	%Object* (%Derived*) *,
	%String* (%Derived*) *,
	%Derived* (%Derived*) *,
	%String* (%Derived*,%Object*,%String*) *,
	%String* (%Derived*,%String*) *
}

@_Derived_vtable_prototype = constant %_Derived_vtable {
	i32 7,
	i32 ptrtoint (%Derived* getelementptr (%Derived, %Derived* null, i32 1) to i32),
	i8* getelementptr ([8 x i8], [8 x i8]* @str.Derived, i32 0, i32 0),
	%Derived* () * @Derived_new,
	%Object* (%Derived*) * bitcast (%Object* (%Object*) * @Object_abort to %Object* (%Derived*) *),
	%String* (%Derived*) * bitcast (%String* (%Object*) * @Object_type_name to %String* (%Derived*) *),
	%Derived* (%Derived*) * bitcast (%Object* (%Object*) * @Object_copy to %Derived* (%Derived*) *),
	%String* (%Derived*,%Object*,%String*) * bitcast (%String* (%Base*,%Object*,%String*) * @Base.identify to %String* (%Derived*,%Object*,%String*) *),
	%String* (%Derived*,%String*) * bitcast (%String* (%Base*,%String*) * @Base.test to %String* (%Derived*,%String*) *)
}

@str.3 = internal constant [14 x i8] c"<basic class>\00"
@String.3 = constant %String {
	%_String_vtable* @_String_vtable_prototype,
	i8* getelementptr ([14 x i8], [14 x i8]* @str.3, i32 0, i32 0)
}

@str.2 = internal constant [1 x i8] c"\00"
@String.2 = constant %String {
	%_String_vtable* @_String_vtable_prototype,
	i8* getelementptr ([1 x i8], [1 x i8]* @str.2, i32 0, i32 0)
}

@str.1 = internal constant [2 x i8] c"\0A\00"
@String.1 = constant %String {
	%_String_vtable* @_String_vtable_prototype,
	i8* getelementptr ([2 x i8], [2 x i8]* @str.1, i32 0, i32 0)
}

@str.0 = internal constant [14 x i8] c"typename_o.cl\00"
@String.0 = constant %String {
	%_String_vtable* @_String_vtable_prototype,
	i8* getelementptr ([14 x i8], [14 x i8]* @str.0, i32 0, i32 0)
}

define i32 @main() {
entry:
	%main.obj = call %Main*() @Main_new( )
	%main.retval = call %Object*(%Main*) @Main.main( %Main* %main.obj )
	ret i32 0
}

define %String* @Main.func(%Main* %self) {

entry:
	%tmp.0 = alloca %Main*
	store %Main* %self, %Main** %tmp.0
	%tmp.1 = call %Derived* @Derived_new(  )
	%tmp.2 = icmp eq %Derived* %tmp.1, null
	br i1 %tmp.2, label %abort, label %ok.0

ok.0:
	%tmp.3 = getelementptr %Derived, %Derived* %tmp.1, i32 0, i32 0
	%tmp.4 = load %_Derived_vtable*, %_Derived_vtable** %tmp.3
	%tmp.5 = getelementptr %_Derived_vtable, %_Derived_vtable* %tmp.4, i32 0, i32 8
	%tmp.6 = load %String* (%Derived*,%String*) *, %String* (%Derived*,%String*) ** %tmp.5
	%tmp.7 = call %String*(%Derived*, %String* ) %tmp.6( %Derived* %tmp.1, %String* @String.2 )
	ret %String* %tmp.7

abort:
	call void @abort(  )
	unreachable
}

define %Object* @Main.main(%Main* %self) {

entry:
	%tmp.8 = alloca %Main*
	store %Main* %self, %Main** %tmp.8
	%tmp.9 = load %Main*, %Main** %tmp.8
	%tmp.10 = icmp eq %Main* %tmp.9, null
	br i1 %tmp.10, label %abort, label %ok.2

ok.2:
	%tmp.11 = getelementptr %Main, %Main* %tmp.9, i32 0, i32 0
	%tmp.12 = load %_Main_vtable*, %_Main_vtable** %tmp.11
	%tmp.13 = getelementptr %_Main_vtable, %_Main_vtable* %tmp.12, i32 0, i32 11
	%tmp.14 = load %String* (%Main*) *, %String* (%Main*) ** %tmp.13
	%tmp.15 = call %String*(%Main* ) %tmp.14( %Main* %tmp.9 )
	%tmp.16 = load %Main*, %Main** %tmp.8
	%tmp.17 = icmp eq %Main* %tmp.16, null
	br i1 %tmp.17, label %abort, label %ok.1

ok.1:
	%tmp.18 = getelementptr %Main, %Main* %tmp.16, i32 0, i32 0
	%tmp.19 = load %_Main_vtable*, %_Main_vtable** %tmp.18
	%tmp.20 = getelementptr %_Main_vtable, %_Main_vtable* %tmp.19, i32 0, i32 7
	%tmp.21 = load %Main* (%Main*,%String*) *, %Main* (%Main*,%String*) ** %tmp.20
	%tmp.22 = call %Main*(%Main*, %String* ) %tmp.21( %Main* %tmp.16, %String* %tmp.15 )
	%tmp.23 = load %Main*, %Main** %tmp.8
	%tmp.24 = bitcast %Main* %tmp.23 to %Object*
	ret %Object* %tmp.24

abort:
	call void @abort(  )
	unreachable
}

define %Main* @Main_new() {

entry:
	%tmp.25 = alloca %Main*
	%tmp.26 = getelementptr %_Main_vtable, %_Main_vtable* @_Main_vtable_prototype, i32 0, i32 1
	%tmp.27 = load i32, i32* %tmp.26
	%tmp.28 = call i8*(i32 ) @malloc( i32 %tmp.27 )
	%tmp.29 = bitcast i8* %tmp.28 to %Main*
	%malloc.null = icmp eq %Main* %tmp.29, null
	br i1 %malloc.null, label %abort, label %okay

okay:
	%tmp.30 = getelementptr %Main, %Main* %tmp.29, i32 0, i32 0
	store %_Main_vtable* @_Main_vtable_prototype, %_Main_vtable** %tmp.30
	store %Main* %tmp.29, %Main** %tmp.25
	ret %Main* %tmp.29

abort:
	call void @abort(  )
	unreachable
}

define %String* @Base.identify(%Base* %self, %Object* %thing, %String* %s) {

entry:
	%tmp.0 = alloca %Base*
	%tmp.1 = alloca %Object*
	%tmp.2 = alloca %String*
	store %Base* %self, %Base** %tmp.0
	store %Object* %thing, %Object** %tmp.1
	store %String* %s, %String** %tmp.2
	%tmp.3 = load %Object*, %Object** %tmp.1
	%tmp.4 = icmp eq %Object* %tmp.3, null
	br i1 %tmp.4, label %abort, label %ok.2

ok.2:
	%tmp.5 = getelementptr %Object, %Object* %tmp.3, i32 0, i32 0
	%tmp.6 = load %_Object_vtable*, %_Object_vtable** %tmp.5
	%tmp.7 = getelementptr %_Object_vtable, %_Object_vtable* %tmp.6, i32 0, i32 5
	%tmp.8 = load %String* (%Object*) *, %String* (%Object*) ** %tmp.7
	%tmp.9 = call %String*(%Object* ) %tmp.8( %Object* %tmp.3 )
	%tmp.10 = load %String*, %String** %tmp.2
	%tmp.11 = icmp eq %String* %tmp.10, null
	br i1 %tmp.11, label %abort, label %ok.1

ok.1:
	%tmp.12 = getelementptr %String, %String* %tmp.10, i32 0, i32 0
	%tmp.13 = load %_String_vtable*, %_String_vtable** %tmp.12
	%tmp.14 = getelementptr %_String_vtable, %_String_vtable* %tmp.13, i32 0, i32 8
	%tmp.15 = load %String* (%String*,%String*) *, %String* (%String*,%String*) ** %tmp.14
	%tmp.16 = call %String*(%String*, %String* ) %tmp.15( %String* %tmp.10, %String* %tmp.9 )
	%tmp.17 = icmp eq %String* %tmp.16, null
	br i1 %tmp.17, label %abort, label %ok.0

ok.0:
	%tmp.18 = getelementptr %String, %String* %tmp.16, i32 0, i32 0
	%tmp.19 = load %_String_vtable*, %_String_vtable** %tmp.18
	%tmp.20 = getelementptr %_String_vtable, %_String_vtable* %tmp.19, i32 0, i32 8
	%tmp.21 = load %String* (%String*,%String*) *, %String* (%String*,%String*) ** %tmp.20
	%tmp.22 = call %String*(%String*, %String* ) %tmp.21( %String* %tmp.16, %String* @String.1 )
	ret %String* %tmp.22

abort:
	call void @abort(  )
	unreachable
}

define %String* @Base.test(%Base* %self, %String* %s) {

entry:
	%tmp.25 = alloca %Base*
	%tmp.23 = alloca %Base*
	%tmp.24 = alloca %String*
	store %Base* %self, %Base** %tmp.23
	store %String* %s, %String** %tmp.24
	%tmp.26 = call %Main* @Main_new(  )
	%tmp.27 = load %String*, %String** %tmp.24
	%tmp.28 = load %Base*, %Base** %tmp.23
	%tmp.29 = icmp eq %Base* %tmp.28, null
	br i1 %tmp.29, label %abort, label %ok.4

ok.4:
	%tmp.30 = getelementptr %Base, %Base* %tmp.28, i32 0, i32 0
	%tmp.31 = load %_Base_vtable*, %_Base_vtable** %tmp.30
	%tmp.32 = getelementptr %_Base_vtable, %_Base_vtable* %tmp.31, i32 0, i32 7
	%tmp.33 = load %String* (%Base*,%Object*,%String*) *, %String* (%Base*,%Object*,%String*) ** %tmp.32
	%tmp.34 = bitcast %Main* %tmp.26 to %Object*
	%tmp.35 = call %String*(%Base*, %Object*, %String* ) %tmp.33( %Base* %tmp.28, %Object* %tmp.34, %String* %tmp.27 )
	%tmp.36 = call %Derived* @Derived_new(  )
	%tmp.37 = load %String*, %String** %tmp.24
	%tmp.38 = load %Base*, %Base** %tmp.23
	%tmp.39 = icmp eq %Base* %tmp.38, null
	br i1 %tmp.39, label %abort, label %ok.6

ok.6:
	%tmp.40 = getelementptr %Base, %Base* %tmp.38, i32 0, i32 0
	%tmp.41 = load %_Base_vtable*, %_Base_vtable** %tmp.40
	%tmp.42 = getelementptr %_Base_vtable, %_Base_vtable* %tmp.41, i32 0, i32 7
	%tmp.43 = load %String* (%Base*,%Object*,%String*) *, %String* (%Base*,%Object*,%String*) ** %tmp.42
	%tmp.44 = bitcast %Derived* %tmp.36 to %Object*
	%tmp.45 = call %String*(%Base*, %Object*, %String* ) %tmp.43( %Base* %tmp.38, %Object* %tmp.44, %String* %tmp.37 )
	%tmp.46 = call %Base* @Base_new(  )
	%tmp.47 = load %String*, %String** %tmp.24
	%tmp.48 = load %Base*, %Base** %tmp.23
	%tmp.49 = icmp eq %Base* %tmp.48, null
	br i1 %tmp.49, label %abort, label %ok.8

ok.8:
	%tmp.50 = getelementptr %Base, %Base* %tmp.48, i32 0, i32 0
	%tmp.51 = load %_Base_vtable*, %_Base_vtable** %tmp.50
	%tmp.52 = getelementptr %_Base_vtable, %_Base_vtable* %tmp.51, i32 0, i32 7
	%tmp.53 = load %String* (%Base*,%Object*,%String*) *, %String* (%Base*,%Object*,%String*) ** %tmp.52
	%tmp.54 = bitcast %Base* %tmp.46 to %Object*
	%tmp.55 = call %String*(%Base*, %Object*, %String* ) %tmp.53( %Base* %tmp.48, %Object* %tmp.54, %String* %tmp.47 )
	%tmp.56 = load %String*, %String** %tmp.24
	%tmp.57 = icmp eq %String* %tmp.56, null
	br i1 %tmp.57, label %abort, label %ok.7

ok.7:
	%tmp.58 = getelementptr %String, %String* %tmp.56, i32 0, i32 0
	%tmp.59 = load %_String_vtable*, %_String_vtable** %tmp.58
	%tmp.60 = getelementptr %_String_vtable, %_String_vtable* %tmp.59, i32 0, i32 8
	%tmp.61 = load %String* (%String*,%String*) *, %String* (%String*,%String*) ** %tmp.60
	%tmp.62 = call %String*(%String*, %String* ) %tmp.61( %String* %tmp.56, %String* %tmp.55 )
	%tmp.63 = icmp eq %String* %tmp.62, null
	br i1 %tmp.63, label %abort, label %ok.5

ok.5:
	%tmp.64 = getelementptr %String, %String* %tmp.62, i32 0, i32 0
	%tmp.65 = load %_String_vtable*, %_String_vtable** %tmp.64
	%tmp.66 = getelementptr %_String_vtable, %_String_vtable* %tmp.65, i32 0, i32 8
	%tmp.67 = load %String* (%String*,%String*) *, %String* (%String*,%String*) ** %tmp.66
	%tmp.68 = call %String*(%String*, %String* ) %tmp.67( %String* %tmp.62, %String* %tmp.45 )
	%tmp.69 = icmp eq %String* %tmp.68, null
	br i1 %tmp.69, label %abort, label %ok.3

ok.3:
	%tmp.70 = getelementptr %String, %String* %tmp.68, i32 0, i32 0
	%tmp.71 = load %_String_vtable*, %_String_vtable** %tmp.70
	%tmp.72 = getelementptr %_String_vtable, %_String_vtable* %tmp.71, i32 0, i32 8
	%tmp.73 = load %String* (%String*,%String*) *, %String* (%String*,%String*) ** %tmp.72
	%tmp.74 = call %String*(%String*, %String* ) %tmp.73( %String* %tmp.68, %String* %tmp.35 )
	store %String* %tmp.74, %String** %tmp.24
	%tmp.75 = call %Derived* @Derived_new(  )
	%tmp.76 = bitcast %Derived* %tmp.75 to %Base*
	store %Base* %tmp.76, %Base** %tmp.25
	%tmp.77 = load %Base*, %Base** %tmp.25
	%tmp.78 = load %String*, %String** %tmp.24
	%tmp.79 = load %Base*, %Base** %tmp.23
	%tmp.80 = icmp eq %Base* %tmp.79, null
	br i1 %tmp.80, label %abort, label %ok.9

ok.9:
	%tmp.81 = getelementptr %Base, %Base* %tmp.79, i32 0, i32 0
	%tmp.82 = load %_Base_vtable*, %_Base_vtable** %tmp.81
	%tmp.83 = getelementptr %_Base_vtable, %_Base_vtable* %tmp.82, i32 0, i32 7
	%tmp.84 = load %String* (%Base*,%Object*,%String*) *, %String* (%Base*,%Object*,%String*) ** %tmp.83
	%tmp.85 = bitcast %Base* %tmp.77 to %Object*
	%tmp.86 = call %String*(%Base*, %Object*, %String* ) %tmp.84( %Base* %tmp.79, %Object* %tmp.85, %String* %tmp.78 )
	store %String* %tmp.86, %String** %tmp.24
	%tmp.87 = load %Base*, %Base** %tmp.23
	%tmp.88 = load %String*, %String** %tmp.24
	%tmp.89 = load %Base*, %Base** %tmp.23
	%tmp.90 = icmp eq %Base* %tmp.89, null
	br i1 %tmp.90, label %abort, label %ok.10

ok.10:
	%tmp.91 = getelementptr %Base, %Base* %tmp.89, i32 0, i32 0
	%tmp.92 = load %_Base_vtable*, %_Base_vtable** %tmp.91
	%tmp.93 = getelementptr %_Base_vtable, %_Base_vtable* %tmp.92, i32 0, i32 7
	%tmp.94 = load %String* (%Base*,%Object*,%String*) *, %String* (%Base*,%Object*,%String*) ** %tmp.93
	%tmp.95 = bitcast %Base* %tmp.87 to %Object*
	%tmp.96 = call %String*(%Base*, %Object*, %String* ) %tmp.94( %Base* %tmp.89, %Object* %tmp.95, %String* %tmp.88 )
	ret %String* %tmp.96

abort:
	call void @abort(  )
	unreachable
}

define %Base* @Base_new() {

entry:
	%tmp.97 = alloca %Base*
	%tmp.98 = getelementptr %_Base_vtable, %_Base_vtable* @_Base_vtable_prototype, i32 0, i32 1
	%tmp.99 = load i32, i32* %tmp.98
	%tmp.100 = call i8*(i32 ) @malloc( i32 %tmp.99 )
	%tmp.101 = bitcast i8* %tmp.100 to %Base*
	%malloc.null = icmp eq %Base* %tmp.101, null
	br i1 %malloc.null, label %abort, label %okay

okay:
	%tmp.102 = getelementptr %Base, %Base* %tmp.101, i32 0, i32 0
	store %_Base_vtable* @_Base_vtable_prototype, %_Base_vtable** %tmp.102
	store %Base* %tmp.101, %Base** %tmp.97
	ret %Base* %tmp.101

abort:
	call void @abort(  )
	unreachable
}

define %Derived* @Derived_new() {

entry:
	%tmp.0 = alloca %Derived*
	%tmp.1 = getelementptr %_Derived_vtable, %_Derived_vtable* @_Derived_vtable_prototype, i32 0, i32 1
	%tmp.2 = load i32, i32* %tmp.1
	%tmp.3 = call i8*(i32 ) @malloc( i32 %tmp.2 )
	%tmp.4 = bitcast i8* %tmp.3 to %Derived*
	%malloc.null = icmp eq %Derived* %tmp.4, null
	br i1 %malloc.null, label %abort, label %okay

okay:
	%tmp.5 = getelementptr %Derived, %Derived* %tmp.4, i32 0, i32 0
	store %_Derived_vtable* @_Derived_vtable_prototype, %_Derived_vtable** %tmp.5
	store %Derived* %tmp.4, %Derived** %tmp.0
	ret %Derived* %tmp.4

abort:
	call void @abort(  )
	unreachable
}

