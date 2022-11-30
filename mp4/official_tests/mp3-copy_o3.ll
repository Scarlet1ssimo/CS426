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
	%_A_vtable*,
	i32,
	i1,
	%String*,
	i32
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

@str.AA = internal constant [3 x i8] c"AA\00"
%AA = type {
	%_AA_vtable*,
	i32,
	i1,
	%String*,
	i32,
	i1,
	i32
}

%_AA_vtable = type {
	i32,
	i32,
	i8*,
	%AA* () *,
	%Object* (%AA*) *,
	%String* (%AA*) *,
	%AA* (%AA*) *,
	i32 (%AA*,i32) *,
	i32 (%AA*) *
}

@_AA_vtable_prototype = constant %_AA_vtable {
	i32 7,
	i32 ptrtoint (%AA* getelementptr (%AA, %AA* null, i32 1) to i32),
	i8* getelementptr ([3 x i8], [3 x i8]* @str.AA, i32 0, i32 0),
	%AA* () * @AA_new,
	%Object* (%AA*) * bitcast (%Object* (%Object*) * @Object_abort to %Object* (%AA*) *),
	%String* (%AA*) * bitcast (%String* (%Object*) * @Object_type_name to %String* (%AA*) *),
	%AA* (%AA*) * bitcast (%Object* (%Object*) * @Object_copy to %AA* (%AA*) *),
	i32 (%AA*,i32) * @AA.set,
	i32 (%AA*) * @AA.get
}

@str.3 = internal constant [14 x i8] c"<basic class>\00"
@String.3 = constant %String {
	%_String_vtable* @_String_vtable_prototype,
	i8* getelementptr ([14 x i8], [14 x i8]* @str.3, i32 0, i32 0)
}

@str.2 = internal constant [8 x i8] c"Error2\0A\00"
@String.2 = constant %String {
	%_String_vtable* @_String_vtable_prototype,
	i8* getelementptr ([8 x i8], [8 x i8]* @str.2, i32 0, i32 0)
}

@str.1 = internal constant [8 x i8] c"Error1\0A\00"
@String.1 = constant %String {
	%_String_vtable* @_String_vtable_prototype,
	i8* getelementptr ([8 x i8], [8 x i8]* @str.1, i32 0, i32 0)
}

@str.0 = internal constant [11 x i8] c"copy_o3.cl\00"
@String.0 = constant %String {
	%_String_vtable* @_String_vtable_prototype,
	i8* getelementptr ([11 x i8], [11 x i8]* @str.0, i32 0, i32 0)
}

define i32 @main() {
entry:
	%main.obj = call %Main*() @Main_new( )
	%main.retval = call %Main*(%Main*) @Main.main( %Main* %main.obj )
	ret i32 0
}

define %Main* @Main.main(%Main* %self) {

entry:
	%tmp.1 = alloca %A*
	%tmp.2 = alloca %A*
	%tmp.3 = alloca %Int*
	%vtpm.0 = alloca %AA*
	%vtpm.1 = alloca %Object*
	%tmp.4 = alloca %Main*
	%vtpm.2 = alloca %AA*
	%vtpm.3 = alloca %Object*
	%tmp.0 = alloca %Main*
	store %Main* %self, %Main** %tmp.0
	store %A* null, %A** %tmp.1
	store %A* null, %A** %tmp.2
	%tmp.5 = call %AA* @AA_new(  )
	%tmp.6 = bitcast %AA* %tmp.5 to %A*
	store %A* %tmp.6, %A** %tmp.1
	%tmp.7 = load %A*, %A** %tmp.1
	%tmp.8 = icmp eq %A* %tmp.7, null
	br i1 %tmp.8, label %abort, label %ok.0

ok.0:
	%tmp.9 = getelementptr %A, %A* %tmp.7, i32 0, i32 0
	%tmp.10 = load %_A_vtable*, %_A_vtable** %tmp.9
	%tmp.11 = getelementptr %_A_vtable, %_A_vtable* %tmp.10, i32 0, i32 0
	%tmp.12 = load i32, i32* %tmp.11
	br label %case.hdr.0

case.hdr.0:
	br label %case.7.0

case.7.0:
	%vtpm.4 = icmp slt i32 %tmp.12, 7
	br i1 %vtpm.4, label %br_exit.7.0, label %src_gte_br.7.0

src_gte_br.7.0:
	%vtpm.5 = icmp sgt i32 %tmp.12, 7
	br i1 %vtpm.5, label %br_exit.7.0, label %src_lte_mc.7.0

src_lte_mc.7.0:
	%tmp.13 = bitcast %A* %tmp.7 to %AA*
	store %AA* %tmp.13, %AA** %vtpm.0
	%tmp.14 = load %AA*, %AA** %vtpm.0
	%tmp.15 = icmp eq %AA* %tmp.14, null
	br i1 %tmp.15, label %abort, label %ok.1

ok.1:
	%tmp.16 = getelementptr %AA, %AA* %tmp.14, i32 0, i32 0
	%tmp.17 = load %_AA_vtable*, %_AA_vtable** %tmp.16
	%tmp.18 = getelementptr %_AA_vtable, %_AA_vtable* %tmp.17, i32 0, i32 7
	%tmp.19 = load i32 (%AA*,i32) *, i32 (%AA*,i32) ** %tmp.18
	%tmp.20 = call i32(%AA*, i32 ) %tmp.19( %AA* %tmp.14, i32 42 )
	%tmp.22 = call %Int* @Int_new(  )
	call void(%Int*, i32 ) @Int_init( %Int* %tmp.22, i32 %tmp.20 )
	%tmp.23 = bitcast %Int* %tmp.22 to %Object*
	%tmp.24 = bitcast %Object* %tmp.23 to %Int*
	store %Int* %tmp.24, %Int** %tmp.3
	br label %case.exit.0

br_exit.7.0:
	br label %case.0.0

case.0.0:
	%vtpm.6 = icmp slt i32 %tmp.12, 0
	br i1 %vtpm.6, label %br_exit.0.0, label %src_gte_br.0.0

src_gte_br.0.0:
	%vtpm.7 = icmp sgt i32 %tmp.12, 7
	br i1 %vtpm.7, label %br_exit.0.0, label %src_lte_mc.0.0

src_lte_mc.0.0:
	%tmp.25 = bitcast %A* %tmp.7 to %Object*
	store %Object* %tmp.25, %Object** %vtpm.1
	%tmp.26 = load %Main*, %Main** %tmp.0
	%tmp.27 = icmp eq %Main* %tmp.26, null
	br i1 %tmp.27, label %abort, label %ok.2

ok.2:
	%tmp.28 = getelementptr %Main, %Main* %tmp.26, i32 0, i32 0
	%tmp.29 = load %_Main_vtable*, %_Main_vtable** %tmp.28
	%tmp.30 = getelementptr %_Main_vtable, %_Main_vtable* %tmp.29, i32 0, i32 7
	%tmp.31 = load %Main* (%Main*,%String*) *, %Main* (%Main*,%String*) ** %tmp.30
	%tmp.32 = call %Main*(%Main*, %String* ) %tmp.31( %Main* %tmp.26, %String* @String.1 )
	%tmp.33 = bitcast %Main* %tmp.32 to %Object*
	%tmp.34 = bitcast %Object* %tmp.33 to %Int*
	store %Int* %tmp.34, %Int** %tmp.3
	br label %case.exit.0

br_exit.0.0:
	br label %abort

case.exit.0:
	%tmp.35 = load %Int*, %Int** %tmp.3
	%tmp.36 = load %A*, %A** %tmp.1
	%tmp.37 = icmp eq %A* %tmp.36, null
	br i1 %tmp.37, label %abort, label %ok.3

ok.3:
	%tmp.38 = getelementptr %A, %A* %tmp.36, i32 0, i32 0
	%tmp.39 = load %_A_vtable*, %_A_vtable** %tmp.38
	%tmp.40 = getelementptr %_A_vtable, %_A_vtable* %tmp.39, i32 0, i32 6
	%tmp.41 = load %A* (%A*) *, %A* (%A*) ** %tmp.40
	%tmp.42 = call %A*(%A* ) %tmp.41( %A* %tmp.36 )
	store %A* %tmp.42, %A** %tmp.2
	%tmp.43 = load %A*, %A** %tmp.2
	%tmp.44 = icmp eq %A* %tmp.43, null
	br i1 %tmp.44, label %abort, label %ok.4

ok.4:
	%tmp.45 = getelementptr %A, %A* %tmp.43, i32 0, i32 0
	%tmp.46 = load %_A_vtable*, %_A_vtable** %tmp.45
	%tmp.47 = getelementptr %_A_vtable, %_A_vtable* %tmp.46, i32 0, i32 0
	%tmp.48 = load i32, i32* %tmp.47
	br label %case.hdr.1

case.hdr.1:
	br label %case.7.1

case.7.1:
	%vtpm.8 = icmp slt i32 %tmp.48, 7
	br i1 %vtpm.8, label %br_exit.7.1, label %src_gte_br.7.1

src_gte_br.7.1:
	%vtpm.9 = icmp sgt i32 %tmp.48, 7
	br i1 %vtpm.9, label %br_exit.7.1, label %src_lte_mc.7.1

src_lte_mc.7.1:
	%tmp.49 = bitcast %A* %tmp.43 to %AA*
	store %AA* %tmp.49, %AA** %vtpm.2
	%tmp.50 = load %AA*, %AA** %vtpm.2
	%tmp.51 = icmp eq %AA* %tmp.50, null
	br i1 %tmp.51, label %abort, label %ok.6

ok.6:
	%tmp.52 = getelementptr %AA, %AA* %tmp.50, i32 0, i32 0
	%tmp.53 = load %_AA_vtable*, %_AA_vtable** %tmp.52
	%tmp.54 = getelementptr %_AA_vtable, %_AA_vtable* %tmp.53, i32 0, i32 8
	%tmp.55 = load i32 (%AA*) *, i32 (%AA*) ** %tmp.54
	%tmp.56 = call i32(%AA* ) %tmp.55( %AA* %tmp.50 )
	%tmp.57 = load %Main*, %Main** %tmp.0
	%tmp.58 = icmp eq %Main* %tmp.57, null
	br i1 %tmp.58, label %abort, label %ok.5

ok.5:
	%tmp.59 = getelementptr %Main, %Main* %tmp.57, i32 0, i32 0
	%tmp.60 = load %_Main_vtable*, %_Main_vtable** %tmp.59
	%tmp.61 = getelementptr %_Main_vtable, %_Main_vtable* %tmp.60, i32 0, i32 8
	%tmp.62 = load %Main* (%Main*,i32) *, %Main* (%Main*,i32) ** %tmp.61
	%tmp.63 = call %Main*(%Main*, i32 ) %tmp.62( %Main* %tmp.57, i32 %tmp.56 )
	store %Main* %tmp.63, %Main** %tmp.4
	br label %case.exit.1

br_exit.7.1:
	br label %case.0.1

case.0.1:
	%vtpm.10 = icmp slt i32 %tmp.48, 0
	br i1 %vtpm.10, label %br_exit.0.1, label %src_gte_br.0.1

src_gte_br.0.1:
	%vtpm.11 = icmp sgt i32 %tmp.48, 7
	br i1 %vtpm.11, label %br_exit.0.1, label %src_lte_mc.0.1

src_lte_mc.0.1:
	%tmp.64 = bitcast %A* %tmp.43 to %Object*
	store %Object* %tmp.64, %Object** %vtpm.3
	%tmp.65 = load %Main*, %Main** %tmp.0
	%tmp.66 = icmp eq %Main* %tmp.65, null
	br i1 %tmp.66, label %abort, label %ok.7

ok.7:
	%tmp.67 = getelementptr %Main, %Main* %tmp.65, i32 0, i32 0
	%tmp.68 = load %_Main_vtable*, %_Main_vtable** %tmp.67
	%tmp.69 = getelementptr %_Main_vtable, %_Main_vtable* %tmp.68, i32 0, i32 7
	%tmp.70 = load %Main* (%Main*,%String*) *, %Main* (%Main*,%String*) ** %tmp.69
	%tmp.71 = call %Main*(%Main*, %String* ) %tmp.70( %Main* %tmp.65, %String* @String.2 )
	store %Main* %tmp.71, %Main** %tmp.4
	br label %case.exit.1

br_exit.0.1:
	br label %abort

case.exit.1:
	%tmp.72 = load %Main*, %Main** %tmp.4
	%tmp.73 = load %Main*, %Main** %tmp.0
	ret %Main* %tmp.73

abort:
	call void @abort(  )
	unreachable
}

define %Main* @Main_new() {

entry:
	%tmp.74 = alloca %Main*
	%tmp.75 = getelementptr %_Main_vtable, %_Main_vtable* @_Main_vtable_prototype, i32 0, i32 1
	%tmp.76 = load i32, i32* %tmp.75
	%tmp.77 = call i8*(i32 ) @malloc( i32 %tmp.76 )
	%tmp.78 = bitcast i8* %tmp.77 to %Main*
	%malloc.null = icmp eq %Main* %tmp.78, null
	br i1 %malloc.null, label %abort, label %okay

okay:
	%tmp.79 = getelementptr %Main, %Main* %tmp.78, i32 0, i32 0
	store %_Main_vtable* @_Main_vtable_prototype, %_Main_vtable** %tmp.79
	store %Main* %tmp.78, %Main** %tmp.74
	ret %Main* %tmp.78

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
	%tmp.6 = getelementptr %A, %A* %tmp.4, i32 0, i32 1
	store i32 0, i32* %tmp.6
	%tmp.7 = getelementptr %A, %A* %tmp.4, i32 0, i32 2
	store i1 false, i1* %tmp.7
	%tmp.8 = getelementptr %A, %A* %tmp.4, i32 0, i32 3
	%tmp.9 = call %String* @String_new(  )
	store %String* %tmp.9, %String** %tmp.8
	%tmp.10 = getelementptr %A, %A* %tmp.4, i32 0, i32 4
	store i32 0, i32* %tmp.10
	%tmp.11 = getelementptr %A, %A* %tmp.4, i32 0, i32 1
	store i32 0, i32* %tmp.11
	%tmp.12 = getelementptr %A, %A* %tmp.4, i32 0, i32 2
	store i1 false, i1* %tmp.12
	%tmp.13 = getelementptr %A, %A* %tmp.4, i32 0, i32 3
	%tmp.14 = call %String* @String_new(  )
	store %String* %tmp.14, %String** %tmp.13
	%tmp.15 = getelementptr %A, %A* %tmp.4, i32 0, i32 4
	store i32 0, i32* %tmp.15
	ret %A* %tmp.4

abort:
	call void @abort(  )
	unreachable
}

define i32 @AA.set(%AA* %self, i32 %x) {

entry:
	%tmp.0 = alloca %AA*
	%tmp.1 = alloca i32
	store %AA* %self, %AA** %tmp.0
	store i32 %x, i32* %tmp.1
	%tmp.2 = load i32, i32* %tmp.1
	%tmp.3 = load %AA*, %AA** %tmp.0
	%tmp.4 = getelementptr %AA, %AA* %tmp.3, i32 0, i32 6
	store i32 %tmp.2, i32* %tmp.4
	ret i32 %tmp.2

abort:
	call void @abort(  )
	unreachable
}

define i32 @AA.get(%AA* %self) {

entry:
	%tmp.5 = alloca %AA*
	store %AA* %self, %AA** %tmp.5
	%tmp.6 = load %AA*, %AA** %tmp.5
	%tmp.7 = getelementptr %AA, %AA* %tmp.6, i32 0, i32 6
	%tmp.8 = load i32, i32* %tmp.7
	ret i32 %tmp.8

abort:
	call void @abort(  )
	unreachable
}

define %AA* @AA_new() {

entry:
	%tmp.9 = alloca %AA*
	%tmp.10 = getelementptr %_AA_vtable, %_AA_vtable* @_AA_vtable_prototype, i32 0, i32 1
	%tmp.11 = load i32, i32* %tmp.10
	%tmp.12 = call i8*(i32 ) @malloc( i32 %tmp.11 )
	%tmp.13 = bitcast i8* %tmp.12 to %AA*
	%malloc.null = icmp eq %AA* %tmp.13, null
	br i1 %malloc.null, label %abort, label %okay

okay:
	%tmp.14 = getelementptr %AA, %AA* %tmp.13, i32 0, i32 0
	store %_AA_vtable* @_AA_vtable_prototype, %_AA_vtable** %tmp.14
	store %AA* %tmp.13, %AA** %tmp.9
	%tmp.15 = getelementptr %AA, %AA* %tmp.13, i32 0, i32 1
	store i32 0, i32* %tmp.15
	%tmp.16 = getelementptr %AA, %AA* %tmp.13, i32 0, i32 2
	store i1 false, i1* %tmp.16
	%tmp.17 = getelementptr %AA, %AA* %tmp.13, i32 0, i32 3
	%tmp.18 = call %String* @String_new(  )
	store %String* %tmp.18, %String** %tmp.17
	%tmp.19 = getelementptr %AA, %AA* %tmp.13, i32 0, i32 4
	store i32 0, i32* %tmp.19
	%tmp.20 = getelementptr %AA, %AA* %tmp.13, i32 0, i32 5
	store i1 false, i1* %tmp.20
	%tmp.21 = getelementptr %AA, %AA* %tmp.13, i32 0, i32 6
	store i32 0, i32* %tmp.21
	%tmp.22 = getelementptr %AA, %AA* %tmp.13, i32 0, i32 1
	store i32 0, i32* %tmp.22
	%tmp.23 = getelementptr %AA, %AA* %tmp.13, i32 0, i32 2
	store i1 false, i1* %tmp.23
	%tmp.24 = getelementptr %AA, %AA* %tmp.13, i32 0, i32 3
	%tmp.25 = call %String* @String_new(  )
	store %String* %tmp.25, %String** %tmp.24
	%tmp.26 = getelementptr %AA, %AA* %tmp.13, i32 0, i32 4
	store i32 0, i32* %tmp.26
	%tmp.27 = getelementptr %AA, %AA* %tmp.13, i32 0, i32 5
	store i1 false, i1* %tmp.27
	%tmp.28 = getelementptr %AA, %AA* %tmp.13, i32 0, i32 6
	store i32 0, i32* %tmp.28
	ret %AA* %tmp.13

abort:
	call void @abort(  )
	unreachable
}

