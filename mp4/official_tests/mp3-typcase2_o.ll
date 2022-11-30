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
	%Gamma*
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
	i32 (%Main*) *
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
	i32 (%Main*) * @Main.main
}

@str.Gamma = internal constant [6 x i8] c"Gamma\00"
%Gamma = type {
	%_Gamma_vtable*
}

%_Gamma_vtable = type {
	i32,
	i32,
	i8*,
	%Gamma* () *,
	%Object* (%Gamma*) *,
	%String* (%Gamma*) *,
	%Gamma* (%Gamma*) *
}

@_Gamma_vtable_prototype = constant %_Gamma_vtable {
	i32 6,
	i32 ptrtoint (%Gamma* getelementptr (%Gamma, %Gamma* null, i32 1) to i32),
	i8* getelementptr ([6 x i8], [6 x i8]* @str.Gamma, i32 0, i32 0),
	%Gamma* () * @Gamma_new,
	%Object* (%Gamma*) * bitcast (%Object* (%Object*) * @Object_abort to %Object* (%Gamma*) *),
	%String* (%Gamma*) * bitcast (%String* (%Object*) * @Object_type_name to %String* (%Gamma*) *),
	%Gamma* (%Gamma*) * bitcast (%Object* (%Object*) * @Object_copy to %Gamma* (%Gamma*) *)
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
	%Beta* (%Beta*) *
}

@_Beta_vtable_prototype = constant %_Beta_vtable {
	i32 7,
	i32 ptrtoint (%Beta* getelementptr (%Beta, %Beta* null, i32 1) to i32),
	i8* getelementptr ([5 x i8], [5 x i8]* @str.Beta, i32 0, i32 0),
	%Beta* () * @Beta_new,
	%Object* (%Beta*) * bitcast (%Object* (%Object*) * @Object_abort to %Object* (%Beta*) *),
	%String* (%Beta*) * bitcast (%String* (%Object*) * @Object_type_name to %String* (%Beta*) *),
	%Beta* (%Beta*) * bitcast (%Object* (%Object*) * @Object_copy to %Beta* (%Beta*) *)
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
	%Alpha* (%Alpha*) *
}

@_Alpha_vtable_prototype = constant %_Alpha_vtable {
	i32 8,
	i32 ptrtoint (%Alpha* getelementptr (%Alpha, %Alpha* null, i32 1) to i32),
	i8* getelementptr ([6 x i8], [6 x i8]* @str.Alpha, i32 0, i32 0),
	%Alpha* () * @Alpha_new,
	%Object* (%Alpha*) * bitcast (%Object* (%Object*) * @Object_abort to %Object* (%Alpha*) *),
	%String* (%Alpha*) * bitcast (%String* (%Object*) * @Object_type_name to %String* (%Alpha*) *),
	%Alpha* (%Alpha*) * bitcast (%Object* (%Object*) * @Object_copy to %Alpha* (%Alpha*) *)
}

@str.1 = internal constant [14 x i8] c"<basic class>\00"
@String.1 = constant %String {
	%_String_vtable* @_String_vtable_prototype,
	i8* getelementptr ([14 x i8], [14 x i8]* @str.1, i32 0, i32 0)
}

@str.0 = internal constant [14 x i8] c"typcase2_o.cl\00"
@String.0 = constant %String {
	%_String_vtable* @_String_vtable_prototype,
	i8* getelementptr ([14 x i8], [14 x i8]* @str.0, i32 0, i32 0)
}

define i32 @main() {
entry:
	%main.obj = call %Main*() @Main_new( )
	%main.retval = call i32(%Main*) @Main.main( %Main* %main.obj )
	ret i32 0
}

define i32 @Main.func(%Main* %self) {

entry:
	%tmp.1 = alloca %Int*
	%vtpm.0 = alloca %Beta*
	%vtpm.1 = alloca %Alpha*
	%vtpm.2 = alloca %Gamma*
	%vtpm.3 = alloca %Object*
	%tmp.0 = alloca %Main*
	store %Main* %self, %Main** %tmp.0
	%tmp.2 = load %Main*, %Main** %tmp.0
	%tmp.3 = getelementptr %Main, %Main* %tmp.2, i32 0, i32 1
	%tmp.4 = load %Gamma*, %Gamma** %tmp.3
	%tmp.5 = icmp eq %Gamma* %tmp.4, null
	br i1 %tmp.5, label %abort, label %ok.0

ok.0:
	%tmp.6 = getelementptr %Gamma, %Gamma* %tmp.4, i32 0, i32 0
	%tmp.7 = load %_Gamma_vtable*, %_Gamma_vtable** %tmp.6
	%tmp.8 = getelementptr %_Gamma_vtable, %_Gamma_vtable* %tmp.7, i32 0, i32 0
	%tmp.9 = load i32, i32* %tmp.8
	br label %case.hdr.0

case.hdr.0:
	br label %case.8.0

case.8.0:
	%vtpm.4 = icmp slt i32 %tmp.9, 8
	br i1 %vtpm.4, label %br_exit.8.0, label %src_gte_br.8.0

src_gte_br.8.0:
	%vtpm.5 = icmp sgt i32 %tmp.9, 8
	br i1 %vtpm.5, label %br_exit.8.0, label %src_lte_mc.8.0

src_lte_mc.8.0:
	%tmp.10 = bitcast %Gamma* %tmp.4 to %Alpha*
	store %Alpha* %tmp.10, %Alpha** %vtpm.1
	%tmp.12 = call %Int* @Int_new(  )
	call void(%Int*, i32 ) @Int_init( %Int* %tmp.12, i32 2 )
	store %Int* %tmp.12, %Int** %tmp.1
	br label %case.exit.0

br_exit.8.0:
	br label %case.7.0

case.7.0:
	%vtpm.6 = icmp slt i32 %tmp.9, 7
	br i1 %vtpm.6, label %br_exit.7.0, label %src_gte_br.7.0

src_gte_br.7.0:
	%vtpm.7 = icmp sgt i32 %tmp.9, 8
	br i1 %vtpm.7, label %br_exit.7.0, label %src_lte_mc.7.0

src_lte_mc.7.0:
	%tmp.13 = bitcast %Gamma* %tmp.4 to %Beta*
	store %Beta* %tmp.13, %Beta** %vtpm.0
	%tmp.15 = call %Int* @Int_new(  )
	call void(%Int*, i32 ) @Int_init( %Int* %tmp.15, i32 1 )
	store %Int* %tmp.15, %Int** %tmp.1
	br label %case.exit.0

br_exit.7.0:
	br label %case.6.0

case.6.0:
	%vtpm.8 = icmp slt i32 %tmp.9, 6
	br i1 %vtpm.8, label %br_exit.6.0, label %src_gte_br.6.0

src_gte_br.6.0:
	%vtpm.9 = icmp sgt i32 %tmp.9, 8
	br i1 %vtpm.9, label %br_exit.6.0, label %src_lte_mc.6.0

src_lte_mc.6.0:
	store %Gamma* %tmp.4, %Gamma** %vtpm.2
	%tmp.17 = call %Int* @Int_new(  )
	call void(%Int*, i32 ) @Int_init( %Int* %tmp.17, i32 3 )
	store %Int* %tmp.17, %Int** %tmp.1
	br label %case.exit.0

br_exit.6.0:
	br label %case.0.0

case.0.0:
	%vtpm.10 = icmp slt i32 %tmp.9, 0
	br i1 %vtpm.10, label %br_exit.0.0, label %src_gte_br.0.0

src_gte_br.0.0:
	%vtpm.11 = icmp sgt i32 %tmp.9, 8
	br i1 %vtpm.11, label %br_exit.0.0, label %src_lte_mc.0.0

src_lte_mc.0.0:
	%tmp.18 = bitcast %Gamma* %tmp.4 to %Object*
	store %Object* %tmp.18, %Object** %vtpm.3
	%tmp.20 = call %Int* @Int_new(  )
	call void(%Int*, i32 ) @Int_init( %Int* %tmp.20, i32 0 )
	store %Int* %tmp.20, %Int** %tmp.1
	br label %case.exit.0

br_exit.0.0:
	br label %abort

case.exit.0:
	%tmp.21 = load %Int*, %Int** %tmp.1
	%tmp.23 = getelementptr %Int, %Int* %tmp.21, i32 0, i32 1
	%tmp.22 = load i32, i32* %tmp.23
	ret i32 %tmp.22

abort:
	call void @abort(  )
	unreachable
}

define i32 @Main.main(%Main* %self) {

entry:
	%tmp.24 = alloca %Main*
	store %Main* %self, %Main** %tmp.24
	%tmp.25 = load %Main*, %Main** %tmp.24
	%tmp.26 = icmp eq %Main* %tmp.25, null
	br i1 %tmp.26, label %abort, label %ok.2

ok.2:
	%tmp.27 = getelementptr %Main, %Main* %tmp.25, i32 0, i32 0
	%tmp.28 = load %_Main_vtable*, %_Main_vtable** %tmp.27
	%tmp.29 = getelementptr %_Main_vtable, %_Main_vtable* %tmp.28, i32 0, i32 11
	%tmp.30 = load i32 (%Main*) *, i32 (%Main*) ** %tmp.29
	%tmp.31 = call i32(%Main* ) %tmp.30( %Main* %tmp.25 )
	%tmp.32 = load %Main*, %Main** %tmp.24
	%tmp.33 = icmp eq %Main* %tmp.32, null
	br i1 %tmp.33, label %abort, label %ok.1

ok.1:
	%tmp.34 = getelementptr %Main, %Main* %tmp.32, i32 0, i32 0
	%tmp.35 = load %_Main_vtable*, %_Main_vtable** %tmp.34
	%tmp.36 = getelementptr %_Main_vtable, %_Main_vtable* %tmp.35, i32 0, i32 8
	%tmp.37 = load %Main* (%Main*,i32) *, %Main* (%Main*,i32) ** %tmp.36
	%tmp.38 = call %Main*(%Main*, i32 ) %tmp.37( %Main* %tmp.32, i32 %tmp.31 )
	ret i32 0

abort:
	call void @abort(  )
	unreachable
}

define %Main* @Main_new() {

entry:
	%tmp.39 = alloca %Main*
	%tmp.40 = getelementptr %_Main_vtable, %_Main_vtable* @_Main_vtable_prototype, i32 0, i32 1
	%tmp.41 = load i32, i32* %tmp.40
	%tmp.42 = call i8*(i32 ) @malloc( i32 %tmp.41 )
	%tmp.43 = bitcast i8* %tmp.42 to %Main*
	%malloc.null = icmp eq %Main* %tmp.43, null
	br i1 %malloc.null, label %abort, label %okay

okay:
	%tmp.44 = getelementptr %Main, %Main* %tmp.43, i32 0, i32 0
	store %_Main_vtable* @_Main_vtable_prototype, %_Main_vtable** %tmp.44
	store %Main* %tmp.43, %Main** %tmp.39
	%tmp.45 = getelementptr %Main, %Main* %tmp.43, i32 0, i32 1
	store %Gamma* null, %Gamma** %tmp.45
	%tmp.46 = getelementptr %Main, %Main* %tmp.43, i32 0, i32 1
	%tmp.47 = call %Alpha* @Alpha_new(  )
	%tmp.48 = bitcast %Alpha* %tmp.47 to %Gamma*
	store %Gamma* %tmp.48, %Gamma** %tmp.46
	ret %Main* %tmp.43

abort:
	call void @abort(  )
	unreachable
}

define %Gamma* @Gamma_new() {

entry:
	%tmp.0 = alloca %Gamma*
	%tmp.1 = getelementptr %_Gamma_vtable, %_Gamma_vtable* @_Gamma_vtable_prototype, i32 0, i32 1
	%tmp.2 = load i32, i32* %tmp.1
	%tmp.3 = call i8*(i32 ) @malloc( i32 %tmp.2 )
	%tmp.4 = bitcast i8* %tmp.3 to %Gamma*
	%malloc.null = icmp eq %Gamma* %tmp.4, null
	br i1 %malloc.null, label %abort, label %okay

okay:
	%tmp.5 = getelementptr %Gamma, %Gamma* %tmp.4, i32 0, i32 0
	store %_Gamma_vtable* @_Gamma_vtable_prototype, %_Gamma_vtable** %tmp.5
	store %Gamma* %tmp.4, %Gamma** %tmp.0
	ret %Gamma* %tmp.4

abort:
	call void @abort(  )
	unreachable
}

define %Beta* @Beta_new() {

entry:
	%tmp.0 = alloca %Beta*
	%tmp.1 = getelementptr %_Beta_vtable, %_Beta_vtable* @_Beta_vtable_prototype, i32 0, i32 1
	%tmp.2 = load i32, i32* %tmp.1
	%tmp.3 = call i8*(i32 ) @malloc( i32 %tmp.2 )
	%tmp.4 = bitcast i8* %tmp.3 to %Beta*
	%malloc.null = icmp eq %Beta* %tmp.4, null
	br i1 %malloc.null, label %abort, label %okay

okay:
	%tmp.5 = getelementptr %Beta, %Beta* %tmp.4, i32 0, i32 0
	store %_Beta_vtable* @_Beta_vtable_prototype, %_Beta_vtable** %tmp.5
	store %Beta* %tmp.4, %Beta** %tmp.0
	ret %Beta* %tmp.4

abort:
	call void @abort(  )
	unreachable
}

define %Alpha* @Alpha_new() {

entry:
	%tmp.0 = alloca %Alpha*
	%tmp.1 = getelementptr %_Alpha_vtable, %_Alpha_vtable* @_Alpha_vtable_prototype, i32 0, i32 1
	%tmp.2 = load i32, i32* %tmp.1
	%tmp.3 = call i8*(i32 ) @malloc( i32 %tmp.2 )
	%tmp.4 = bitcast i8* %tmp.3 to %Alpha*
	%malloc.null = icmp eq %Alpha* %tmp.4, null
	br i1 %malloc.null, label %abort, label %okay

okay:
	%tmp.5 = getelementptr %Alpha, %Alpha* %tmp.4, i32 0, i32 0
	store %_Alpha_vtable* @_Alpha_vtable_prototype, %_Alpha_vtable** %tmp.5
	store %Alpha* %tmp.4, %Alpha** %tmp.0
	ret %Alpha* %tmp.4

abort:
	call void @abort(  )
	unreachable
}

