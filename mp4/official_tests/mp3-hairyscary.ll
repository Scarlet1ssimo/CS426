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

@str.Bazz = internal constant [5 x i8] c"Bazz\00"
%Bazz = type {
	%_Bazz_vtable*,
	i32,
	%Foo*,
	%Object*
}

%_Bazz_vtable = type {
	i32,
	i32,
	i8*,
	%Bazz* () *,
	%Object* (%Bazz*) *,
	%String* (%Bazz*) *,
	%Bazz* (%Bazz*) *,
	%Bazz* (%Bazz*,%String*) *,
	%Bazz* (%Bazz*,i32) *,
	%String* (%Bazz*) *,
	i32 (%Bazz*) *,
	i32 (%Bazz*) *,
	i32 (%Bazz*) *
}

@_Bazz_vtable_prototype = constant %_Bazz_vtable {
	i32 5,
	i32 ptrtoint (%Bazz* getelementptr (%Bazz, %Bazz* null, i32 1) to i32),
	i8* getelementptr ([5 x i8], [5 x i8]* @str.Bazz, i32 0, i32 0),
	%Bazz* () * @Bazz_new,
	%Object* (%Bazz*) * bitcast (%Object* (%Object*) * @Object_abort to %Object* (%Bazz*) *),
	%String* (%Bazz*) * bitcast (%String* (%Object*) * @Object_type_name to %String* (%Bazz*) *),
	%Bazz* (%Bazz*) * bitcast (%Object* (%Object*) * @Object_copy to %Bazz* (%Bazz*) *),
	%Bazz* (%Bazz*,%String*) * bitcast (%IO* (%IO*,%String*) * @IO_out_string to %Bazz* (%Bazz*,%String*) *),
	%Bazz* (%Bazz*,i32) * bitcast (%IO* (%IO*,i32) * @IO_out_int to %Bazz* (%Bazz*,i32) *),
	%String* (%Bazz*) * bitcast (%String* (%IO*) * @IO_in_string to %String* (%Bazz*) *),
	i32 (%Bazz*) * bitcast (i32 (%IO*) * @IO_in_int to i32 (%Bazz*) *),
	i32 (%Bazz*) * @Bazz.printh,
	i32 (%Bazz*) * @Bazz.doh
}

@str.Foo = internal constant [4 x i8] c"Foo\00"
%Foo = type {
	%_Foo_vtable*,
	i32,
	%Foo*,
	%Object*,
	%Razz*,
	i32
}

%_Foo_vtable = type {
	i32,
	i32,
	i8*,
	%Foo* () *,
	%Object* (%Foo*) *,
	%String* (%Foo*) *,
	%Foo* (%Foo*) *,
	%Foo* (%Foo*,%String*) *,
	%Foo* (%Foo*,i32) *,
	%String* (%Foo*) *,
	i32 (%Foo*) *,
	i32 (%Foo*) *,
	i32 (%Foo*) *
}

@_Foo_vtable_prototype = constant %_Foo_vtable {
	i32 6,
	i32 ptrtoint (%Foo* getelementptr (%Foo, %Foo* null, i32 1) to i32),
	i8* getelementptr ([4 x i8], [4 x i8]* @str.Foo, i32 0, i32 0),
	%Foo* () * @Foo_new,
	%Object* (%Foo*) * bitcast (%Object* (%Object*) * @Object_abort to %Object* (%Foo*) *),
	%String* (%Foo*) * bitcast (%String* (%Object*) * @Object_type_name to %String* (%Foo*) *),
	%Foo* (%Foo*) * bitcast (%Object* (%Object*) * @Object_copy to %Foo* (%Foo*) *),
	%Foo* (%Foo*,%String*) * bitcast (%IO* (%IO*,%String*) * @IO_out_string to %Foo* (%Foo*,%String*) *),
	%Foo* (%Foo*,i32) * bitcast (%IO* (%IO*,i32) * @IO_out_int to %Foo* (%Foo*,i32) *),
	%String* (%Foo*) * bitcast (%String* (%IO*) * @IO_in_string to %String* (%Foo*) *),
	i32 (%Foo*) * bitcast (i32 (%IO*) * @IO_in_int to i32 (%Foo*) *),
	i32 (%Foo*) * bitcast (i32 (%Bazz*) * @Bazz.printh to i32 (%Foo*) *),
	i32 (%Foo*) * @Foo.doh
}

@str.Razz = internal constant [5 x i8] c"Razz\00"
%Razz = type {
	%_Razz_vtable*,
	i32,
	%Foo*,
	%Object*,
	%Razz*,
	i32,
	%Bar*,
	i32
}

%_Razz_vtable = type {
	i32,
	i32,
	i8*,
	%Razz* () *,
	%Object* (%Razz*) *,
	%String* (%Razz*) *,
	%Razz* (%Razz*) *,
	%Razz* (%Razz*,%String*) *,
	%Razz* (%Razz*,i32) *,
	%String* (%Razz*) *,
	i32 (%Razz*) *,
	i32 (%Razz*) *,
	i32 (%Razz*) *
}

@_Razz_vtable_prototype = constant %_Razz_vtable {
	i32 7,
	i32 ptrtoint (%Razz* getelementptr (%Razz, %Razz* null, i32 1) to i32),
	i8* getelementptr ([5 x i8], [5 x i8]* @str.Razz, i32 0, i32 0),
	%Razz* () * @Razz_new,
	%Object* (%Razz*) * bitcast (%Object* (%Object*) * @Object_abort to %Object* (%Razz*) *),
	%String* (%Razz*) * bitcast (%String* (%Object*) * @Object_type_name to %String* (%Razz*) *),
	%Razz* (%Razz*) * bitcast (%Object* (%Object*) * @Object_copy to %Razz* (%Razz*) *),
	%Razz* (%Razz*,%String*) * bitcast (%IO* (%IO*,%String*) * @IO_out_string to %Razz* (%Razz*,%String*) *),
	%Razz* (%Razz*,i32) * bitcast (%IO* (%IO*,i32) * @IO_out_int to %Razz* (%Razz*,i32) *),
	%String* (%Razz*) * bitcast (%String* (%IO*) * @IO_in_string to %String* (%Razz*) *),
	i32 (%Razz*) * bitcast (i32 (%IO*) * @IO_in_int to i32 (%Razz*) *),
	i32 (%Razz*) * bitcast (i32 (%Bazz*) * @Bazz.printh to i32 (%Razz*) *),
	i32 (%Razz*) * bitcast (i32 (%Foo*) * @Foo.doh to i32 (%Razz*) *)
}

@str.Bar = internal constant [4 x i8] c"Bar\00"
%Bar = type {
	%_Bar_vtable*,
	i32,
	%Foo*,
	%Object*,
	%Razz*,
	i32,
	%Bar*,
	i32,
	i32,
	%Object*
}

%_Bar_vtable = type {
	i32,
	i32,
	i8*,
	%Bar* () *,
	%Object* (%Bar*) *,
	%String* (%Bar*) *,
	%Bar* (%Bar*) *,
	%Bar* (%Bar*,%String*) *,
	%Bar* (%Bar*,i32) *,
	%String* (%Bar*) *,
	i32 (%Bar*) *,
	i32 (%Bar*) *,
	i32 (%Bar*) *
}

@_Bar_vtable_prototype = constant %_Bar_vtable {
	i32 8,
	i32 ptrtoint (%Bar* getelementptr (%Bar, %Bar* null, i32 1) to i32),
	i8* getelementptr ([4 x i8], [4 x i8]* @str.Bar, i32 0, i32 0),
	%Bar* () * @Bar_new,
	%Object* (%Bar*) * bitcast (%Object* (%Object*) * @Object_abort to %Object* (%Bar*) *),
	%String* (%Bar*) * bitcast (%String* (%Object*) * @Object_type_name to %String* (%Bar*) *),
	%Bar* (%Bar*) * bitcast (%Object* (%Object*) * @Object_copy to %Bar* (%Bar*) *),
	%Bar* (%Bar*,%String*) * bitcast (%IO* (%IO*,%String*) * @IO_out_string to %Bar* (%Bar*,%String*) *),
	%Bar* (%Bar*,i32) * bitcast (%IO* (%IO*,i32) * @IO_out_int to %Bar* (%Bar*,i32) *),
	%String* (%Bar*) * bitcast (%String* (%IO*) * @IO_in_string to %String* (%Bar*) *),
	i32 (%Bar*) * bitcast (i32 (%IO*) * @IO_in_int to i32 (%Bar*) *),
	i32 (%Bar*) * bitcast (i32 (%Bazz*) * @Bazz.printh to i32 (%Bar*) *),
	i32 (%Bar*) * bitcast (i32 (%Foo*) * @Foo.doh to i32 (%Bar*) *)
}

@str.Main = internal constant [5 x i8] c"Main\00"
%Main = type {
	%_Main_vtable*,
	%Bazz*,
	%Foo*,
	%Razz*,
	%Bar*
}

%_Main_vtable = type {
	i32,
	i32,
	i8*,
	%Main* () *,
	%Object* (%Main*) *,
	%String* (%Main*) *,
	%Main* (%Main*) *,
	%String* (%Main*) *
}

@_Main_vtable_prototype = constant %_Main_vtable {
	i32 9,
	i32 ptrtoint (%Main* getelementptr (%Main, %Main* null, i32 1) to i32),
	i8* getelementptr ([5 x i8], [5 x i8]* @str.Main, i32 0, i32 0),
	%Main* () * @Main_new,
	%Object* (%Main*) * bitcast (%Object* (%Object*) * @Object_abort to %Object* (%Main*) *),
	%String* (%Main*) * bitcast (%String* (%Object*) * @Object_type_name to %String* (%Main*) *),
	%Main* (%Main*) * bitcast (%Object* (%Object*) * @Object_copy to %Main* (%Main*) *),
	%String* (%Main*) * @Main.main
}

@str.2 = internal constant [14 x i8] c"<basic class>\00"
@String.2 = constant %String {
	%_String_vtable* @_String_vtable_prototype,
	i8* getelementptr ([14 x i8], [14 x i8]* @str.2, i32 0, i32 0)
}

@str.1 = internal constant [11 x i8] c"do nothing\00"
@String.1 = constant %String {
	%_String_vtable* @_String_vtable_prototype,
	i8* getelementptr ([11 x i8], [11 x i8]* @str.1, i32 0, i32 0)
}

@str.0 = internal constant [14 x i8] c"hairyscary.cl\00"
@String.0 = constant %String {
	%_String_vtable* @_String_vtable_prototype,
	i8* getelementptr ([14 x i8], [14 x i8]* @str.0, i32 0, i32 0)
}

define i32 @main() {
entry:
	%main.obj = call %Main*() @Main_new( )
	%main.retval = call %String*(%Main*) @Main.main( %Main* %main.obj )
	ret i32 0
}

define i32 @Bazz.printh(%Bazz* %self) {

entry:
	%tmp.0 = alloca %Bazz*
	store %Bazz* %self, %Bazz** %tmp.0
	%tmp.1 = load %Bazz*, %Bazz** %tmp.0
	%tmp.2 = getelementptr %Bazz, %Bazz* %tmp.1, i32 0, i32 1
	%tmp.3 = load i32, i32* %tmp.2
	%tmp.4 = load %Bazz*, %Bazz** %tmp.0
	%tmp.5 = icmp eq %Bazz* %tmp.4, null
	br i1 %tmp.5, label %abort, label %ok.0

ok.0:
	%tmp.6 = getelementptr %Bazz, %Bazz* %tmp.4, i32 0, i32 0
	%tmp.7 = load %_Bazz_vtable*, %_Bazz_vtable** %tmp.6
	%tmp.8 = getelementptr %_Bazz_vtable, %_Bazz_vtable* %tmp.7, i32 0, i32 8
	%tmp.9 = load %Bazz* (%Bazz*,i32) *, %Bazz* (%Bazz*,i32) ** %tmp.8
	%tmp.10 = call %Bazz*(%Bazz*, i32 ) %tmp.9( %Bazz* %tmp.4, i32 %tmp.3 )
	ret i32 0

abort:
	call void @abort(  )
	unreachable
}

define i32 @Bazz.doh(%Bazz* %self) {

entry:
	%tmp.12 = alloca i32
	%tmp.11 = alloca %Bazz*
	store %Bazz* %self, %Bazz** %tmp.11
	%tmp.13 = load %Bazz*, %Bazz** %tmp.11
	%tmp.14 = getelementptr %Bazz, %Bazz* %tmp.13, i32 0, i32 1
	%tmp.15 = load i32, i32* %tmp.14
	store i32 %tmp.15, i32* %tmp.12
	%tmp.16 = load %Bazz*, %Bazz** %tmp.11
	%tmp.17 = getelementptr %Bazz, %Bazz* %tmp.16, i32 0, i32 1
	%tmp.18 = load i32, i32* %tmp.17
	%tmp.19 = add i32 %tmp.18, 1
	%tmp.20 = load %Bazz*, %Bazz** %tmp.11
	%tmp.21 = getelementptr %Bazz, %Bazz* %tmp.20, i32 0, i32 1
	store i32 %tmp.19, i32* %tmp.21
	%tmp.22 = load i32, i32* %tmp.12
	ret i32 %tmp.22

abort:
	call void @abort(  )
	unreachable
}

define %Bazz* @Bazz_new() {

entry:
	%tmp.23 = alloca %Bazz*
	%tmp.24 = alloca %Foo*
	%vtpm.0 = alloca %Bazz*
	%vtpm.1 = alloca %Razz*
	%vtpm.2 = alloca %Foo*
	%vtpm.3 = alloca %Bar*
	%tmp.25 = getelementptr %_Bazz_vtable, %_Bazz_vtable* @_Bazz_vtable_prototype, i32 0, i32 1
	%tmp.26 = load i32, i32* %tmp.25
	%tmp.27 = call i8*(i32 ) @malloc( i32 %tmp.26 )
	%tmp.28 = bitcast i8* %tmp.27 to %Bazz*
	%malloc.null = icmp eq %Bazz* %tmp.28, null
	br i1 %malloc.null, label %abort, label %okay

okay:
	%tmp.29 = getelementptr %Bazz, %Bazz* %tmp.28, i32 0, i32 0
	store %_Bazz_vtable* @_Bazz_vtable_prototype, %_Bazz_vtable** %tmp.29
	store %Bazz* %tmp.28, %Bazz** %tmp.23
	%tmp.30 = getelementptr %Bazz, %Bazz* %tmp.28, i32 0, i32 1
	store i32 0, i32* %tmp.30
	%tmp.31 = getelementptr %Bazz, %Bazz* %tmp.28, i32 0, i32 2
	store %Foo* null, %Foo** %tmp.31
	%tmp.32 = getelementptr %Bazz, %Bazz* %tmp.28, i32 0, i32 3
	store %Object* null, %Object** %tmp.32
	%tmp.33 = getelementptr %Bazz, %Bazz* %tmp.28, i32 0, i32 1
	store i32 1, i32* %tmp.33
	%tmp.34 = getelementptr %Bazz, %Bazz* %tmp.28, i32 0, i32 2
	%tmp.35 = load %Bazz*, %Bazz** %tmp.23
	%tmp.36 = icmp eq %Bazz* %tmp.35, null
	br i1 %tmp.36, label %abort, label %ok.1

ok.1:
	%tmp.37 = getelementptr %Bazz, %Bazz* %tmp.35, i32 0, i32 0
	%tmp.38 = load %_Bazz_vtable*, %_Bazz_vtable** %tmp.37
	%tmp.39 = getelementptr %_Bazz_vtable, %_Bazz_vtable* %tmp.38, i32 0, i32 0
	%tmp.40 = load i32, i32* %tmp.39
	br label %case.hdr.0

case.hdr.0:
	br label %case.8.0

case.8.0:
	%vtpm.4 = icmp slt i32 %tmp.40, 8
	br i1 %vtpm.4, label %br_exit.8.0, label %src_gte_br.8.0

src_gte_br.8.0:
	%vtpm.5 = icmp sgt i32 %tmp.40, 8
	br i1 %vtpm.5, label %br_exit.8.0, label %src_lte_mc.8.0

src_lte_mc.8.0:
	%tmp.41 = bitcast %Bazz* %tmp.35 to %Bar*
	store %Bar* %tmp.41, %Bar** %vtpm.3
	%tmp.42 = load %Bar*, %Bar** %vtpm.3
	%tmp.43 = bitcast %Bar* %tmp.42 to %Foo*
	store %Foo* %tmp.43, %Foo** %tmp.24
	br label %case.exit.0

br_exit.8.0:
	br label %case.7.0

case.7.0:
	%vtpm.6 = icmp slt i32 %tmp.40, 7
	br i1 %vtpm.6, label %br_exit.7.0, label %src_gte_br.7.0

src_gte_br.7.0:
	%vtpm.7 = icmp sgt i32 %tmp.40, 8
	br i1 %vtpm.7, label %br_exit.7.0, label %src_lte_mc.7.0

src_lte_mc.7.0:
	%tmp.44 = bitcast %Bazz* %tmp.35 to %Razz*
	store %Razz* %tmp.44, %Razz** %vtpm.1
	%tmp.45 = call %Bar* @Bar_new(  )
	%tmp.46 = bitcast %Bar* %tmp.45 to %Foo*
	store %Foo* %tmp.46, %Foo** %tmp.24
	br label %case.exit.0

br_exit.7.0:
	br label %case.6.0

case.6.0:
	%vtpm.8 = icmp slt i32 %tmp.40, 6
	br i1 %vtpm.8, label %br_exit.6.0, label %src_gte_br.6.0

src_gte_br.6.0:
	%vtpm.9 = icmp sgt i32 %tmp.40, 8
	br i1 %vtpm.9, label %br_exit.6.0, label %src_lte_mc.6.0

src_lte_mc.6.0:
	%tmp.47 = bitcast %Bazz* %tmp.35 to %Foo*
	store %Foo* %tmp.47, %Foo** %vtpm.2
	%tmp.48 = call %Razz* @Razz_new(  )
	%tmp.49 = bitcast %Razz* %tmp.48 to %Foo*
	store %Foo* %tmp.49, %Foo** %tmp.24
	br label %case.exit.0

br_exit.6.0:
	br label %case.5.0

case.5.0:
	%vtpm.10 = icmp slt i32 %tmp.40, 5
	br i1 %vtpm.10, label %br_exit.5.0, label %src_gte_br.5.0

src_gte_br.5.0:
	%vtpm.11 = icmp sgt i32 %tmp.40, 8
	br i1 %vtpm.11, label %br_exit.5.0, label %src_lte_mc.5.0

src_lte_mc.5.0:
	store %Bazz* %tmp.35, %Bazz** %vtpm.0
	%tmp.50 = call %Foo* @Foo_new(  )
	store %Foo* %tmp.50, %Foo** %tmp.24
	br label %case.exit.0

br_exit.5.0:
	br label %abort

case.exit.0:
	%tmp.51 = load %Foo*, %Foo** %tmp.24
	store %Foo* %tmp.51, %Foo** %tmp.34
	%tmp.52 = getelementptr %Bazz, %Bazz* %tmp.28, i32 0, i32 3
	%tmp.53 = load %Bazz*, %Bazz** %tmp.23
	%tmp.54 = icmp eq %Bazz* %tmp.53, null
	br i1 %tmp.54, label %abort, label %ok.2

ok.2:
	%tmp.55 = getelementptr %Bazz, %Bazz* %tmp.53, i32 0, i32 0
	%tmp.56 = load %_Bazz_vtable*, %_Bazz_vtable** %tmp.55
	%tmp.57 = getelementptr %_Bazz_vtable, %_Bazz_vtable* %tmp.56, i32 0, i32 11
	%tmp.58 = load i32 (%Bazz*) *, i32 (%Bazz*) ** %tmp.57
	%tmp.59 = call i32(%Bazz* ) %tmp.58( %Bazz* %tmp.53 )
	%tmp.61 = call %Int* @Int_new(  )
	call void(%Int*, i32 ) @Int_init( %Int* %tmp.61, i32 %tmp.59 )
	%tmp.62 = bitcast %Int* %tmp.61 to %Object*
	store %Object* %tmp.62, %Object** %tmp.52
	ret %Bazz* %tmp.28

abort:
	call void @abort(  )
	unreachable
}

define i32 @Foo.doh(%Foo* %self) {

entry:
	%tmp.1 = alloca i32
	%tmp.0 = alloca %Foo*
	store %Foo* %self, %Foo** %tmp.0
	%tmp.2 = load %Foo*, %Foo** %tmp.0
	%tmp.3 = getelementptr %Foo, %Foo* %tmp.2, i32 0, i32 1
	%tmp.4 = load i32, i32* %tmp.3
	store i32 %tmp.4, i32* %tmp.1
	%tmp.5 = load %Foo*, %Foo** %tmp.0
	%tmp.6 = getelementptr %Foo, %Foo* %tmp.5, i32 0, i32 1
	%tmp.7 = load i32, i32* %tmp.6
	%tmp.8 = add i32 %tmp.7, 2
	%tmp.9 = load %Foo*, %Foo** %tmp.0
	%tmp.10 = getelementptr %Foo, %Foo* %tmp.9, i32 0, i32 1
	store i32 %tmp.8, i32* %tmp.10
	%tmp.11 = load i32, i32* %tmp.1
	ret i32 %tmp.11

abort:
	call void @abort(  )
	unreachable
}

define %Foo* @Foo_new() {

entry:
	%tmp.12 = alloca %Foo*
	%tmp.13 = alloca %Bar*
	%vtpm.12 = alloca %Razz*
	%vtpm.13 = alloca %Foo*
	%vtpm.14 = alloca %Bar*
	%tmp.14 = alloca %Foo*
	%vtpm.15 = alloca %Bazz*
	%vtpm.16 = alloca %Razz*
	%vtpm.17 = alloca %Foo*
	%vtpm.18 = alloca %Bar*
	%tmp.15 = getelementptr %_Foo_vtable, %_Foo_vtable* @_Foo_vtable_prototype, i32 0, i32 1
	%tmp.16 = load i32, i32* %tmp.15
	%tmp.17 = call i8*(i32 ) @malloc( i32 %tmp.16 )
	%tmp.18 = bitcast i8* %tmp.17 to %Foo*
	%malloc.null = icmp eq %Foo* %tmp.18, null
	br i1 %malloc.null, label %abort, label %okay

okay:
	%tmp.19 = getelementptr %Foo, %Foo* %tmp.18, i32 0, i32 0
	store %_Foo_vtable* @_Foo_vtable_prototype, %_Foo_vtable** %tmp.19
	store %Foo* %tmp.18, %Foo** %tmp.12
	%tmp.20 = getelementptr %Foo, %Foo* %tmp.18, i32 0, i32 1
	store i32 0, i32* %tmp.20
	%tmp.21 = getelementptr %Foo, %Foo* %tmp.18, i32 0, i32 2
	store %Foo* null, %Foo** %tmp.21
	%tmp.22 = getelementptr %Foo, %Foo* %tmp.18, i32 0, i32 3
	store %Object* null, %Object** %tmp.22
	%tmp.23 = getelementptr %Foo, %Foo* %tmp.18, i32 0, i32 4
	store %Razz* null, %Razz** %tmp.23
	%tmp.24 = getelementptr %Foo, %Foo* %tmp.18, i32 0, i32 5
	store i32 0, i32* %tmp.24
	%tmp.25 = getelementptr %Foo, %Foo* %tmp.18, i32 0, i32 1
	store i32 1, i32* %tmp.25
	%tmp.26 = getelementptr %Foo, %Foo* %tmp.18, i32 0, i32 2
	%tmp.27 = load %Foo*, %Foo** %tmp.12
	%tmp.28 = icmp eq %Foo* %tmp.27, null
	br i1 %tmp.28, label %abort, label %ok.0

ok.0:
	%tmp.29 = getelementptr %Foo, %Foo* %tmp.27, i32 0, i32 0
	%tmp.30 = load %_Foo_vtable*, %_Foo_vtable** %tmp.29
	%tmp.31 = getelementptr %_Foo_vtable, %_Foo_vtable* %tmp.30, i32 0, i32 0
	%tmp.32 = load i32, i32* %tmp.31
	br label %case.hdr.0

case.hdr.0:
	br label %case.8.0

case.8.0:
	%vtpm.19 = icmp slt i32 %tmp.32, 8
	br i1 %vtpm.19, label %br_exit.8.0, label %src_gte_br.8.0

src_gte_br.8.0:
	%vtpm.20 = icmp sgt i32 %tmp.32, 8
	br i1 %vtpm.20, label %br_exit.8.0, label %src_lte_mc.8.0

src_lte_mc.8.0:
	%tmp.33 = bitcast %Foo* %tmp.27 to %Bar*
	store %Bar* %tmp.33, %Bar** %vtpm.18
	%tmp.34 = load %Bar*, %Bar** %vtpm.18
	%tmp.35 = bitcast %Bar* %tmp.34 to %Foo*
	store %Foo* %tmp.35, %Foo** %tmp.14
	br label %case.exit.0

br_exit.8.0:
	br label %case.7.0

case.7.0:
	%vtpm.21 = icmp slt i32 %tmp.32, 7
	br i1 %vtpm.21, label %br_exit.7.0, label %src_gte_br.7.0

src_gte_br.7.0:
	%vtpm.22 = icmp sgt i32 %tmp.32, 8
	br i1 %vtpm.22, label %br_exit.7.0, label %src_lte_mc.7.0

src_lte_mc.7.0:
	%tmp.36 = bitcast %Foo* %tmp.27 to %Razz*
	store %Razz* %tmp.36, %Razz** %vtpm.16
	%tmp.37 = call %Bar* @Bar_new(  )
	%tmp.38 = bitcast %Bar* %tmp.37 to %Foo*
	store %Foo* %tmp.38, %Foo** %tmp.14
	br label %case.exit.0

br_exit.7.0:
	br label %case.6.0

case.6.0:
	%vtpm.23 = icmp slt i32 %tmp.32, 6
	br i1 %vtpm.23, label %br_exit.6.0, label %src_gte_br.6.0

src_gte_br.6.0:
	%vtpm.24 = icmp sgt i32 %tmp.32, 8
	br i1 %vtpm.24, label %br_exit.6.0, label %src_lte_mc.6.0

src_lte_mc.6.0:
	store %Foo* %tmp.27, %Foo** %vtpm.17
	%tmp.39 = call %Razz* @Razz_new(  )
	%tmp.40 = bitcast %Razz* %tmp.39 to %Foo*
	store %Foo* %tmp.40, %Foo** %tmp.14
	br label %case.exit.0

br_exit.6.0:
	br label %case.5.0

case.5.0:
	%vtpm.25 = icmp slt i32 %tmp.32, 5
	br i1 %vtpm.25, label %br_exit.5.0, label %src_gte_br.5.0

src_gte_br.5.0:
	%vtpm.26 = icmp sgt i32 %tmp.32, 8
	br i1 %vtpm.26, label %br_exit.5.0, label %src_lte_mc.5.0

src_lte_mc.5.0:
	%tmp.41 = bitcast %Foo* %tmp.27 to %Bazz*
	store %Bazz* %tmp.41, %Bazz** %vtpm.15
	%tmp.42 = call %Foo* @Foo_new(  )
	store %Foo* %tmp.42, %Foo** %tmp.14
	br label %case.exit.0

br_exit.5.0:
	br label %abort

case.exit.0:
	%tmp.43 = load %Foo*, %Foo** %tmp.14
	store %Foo* %tmp.43, %Foo** %tmp.26
	%tmp.44 = getelementptr %Foo, %Foo* %tmp.18, i32 0, i32 3
	%tmp.45 = load %Foo*, %Foo** %tmp.12
	%tmp.46 = icmp eq %Foo* %tmp.45, null
	br i1 %tmp.46, label %abort, label %ok.1

ok.1:
	%tmp.47 = getelementptr %Foo, %Foo* %tmp.45, i32 0, i32 0
	%tmp.48 = load %_Foo_vtable*, %_Foo_vtable** %tmp.47
	%tmp.49 = getelementptr %_Foo_vtable, %_Foo_vtable* %tmp.48, i32 0, i32 11
	%tmp.50 = load i32 (%Foo*) *, i32 (%Foo*) ** %tmp.49
	%tmp.51 = call i32(%Foo* ) %tmp.50( %Foo* %tmp.45 )
	%tmp.53 = call %Int* @Int_new(  )
	call void(%Int*, i32 ) @Int_init( %Int* %tmp.53, i32 %tmp.51 )
	%tmp.54 = bitcast %Int* %tmp.53 to %Object*
	store %Object* %tmp.54, %Object** %tmp.44
	%tmp.55 = getelementptr %Foo, %Foo* %tmp.18, i32 0, i32 4
	%tmp.56 = load %Foo*, %Foo** %tmp.12
	%tmp.57 = icmp eq %Foo* %tmp.56, null
	br i1 %tmp.57, label %abort, label %ok.2

ok.2:
	%tmp.58 = getelementptr %Foo, %Foo* %tmp.56, i32 0, i32 0
	%tmp.59 = load %_Foo_vtable*, %_Foo_vtable** %tmp.58
	%tmp.60 = getelementptr %_Foo_vtable, %_Foo_vtable* %tmp.59, i32 0, i32 0
	%tmp.61 = load i32, i32* %tmp.60
	br label %case.hdr.1

case.hdr.1:
	br label %case.8.1

case.8.1:
	%vtpm.27 = icmp slt i32 %tmp.61, 8
	br i1 %vtpm.27, label %br_exit.8.1, label %src_gte_br.8.1

src_gte_br.8.1:
	%vtpm.28 = icmp sgt i32 %tmp.61, 8
	br i1 %vtpm.28, label %br_exit.8.1, label %src_lte_mc.8.1

src_lte_mc.8.1:
	%tmp.62 = bitcast %Foo* %tmp.56 to %Bar*
	store %Bar* %tmp.62, %Bar** %vtpm.14
	%tmp.63 = load %Bar*, %Bar** %vtpm.14
	%tmp.64 = bitcast %Bar* %tmp.63 to %Razz*
	%tmp.65 = bitcast %Razz* %tmp.64 to %Bar*
	store %Bar* %tmp.65, %Bar** %tmp.13
	br label %case.exit.1

br_exit.8.1:
	br label %case.7.1

case.7.1:
	%vtpm.29 = icmp slt i32 %tmp.61, 7
	br i1 %vtpm.29, label %br_exit.7.1, label %src_gte_br.7.1

src_gte_br.7.1:
	%vtpm.30 = icmp sgt i32 %tmp.61, 8
	br i1 %vtpm.30, label %br_exit.7.1, label %src_lte_mc.7.1

src_lte_mc.7.1:
	%tmp.66 = bitcast %Foo* %tmp.56 to %Razz*
	store %Razz* %tmp.66, %Razz** %vtpm.12
	%tmp.67 = call %Bar* @Bar_new(  )
	%tmp.68 = bitcast %Bar* %tmp.67 to %Razz*
	%tmp.69 = bitcast %Razz* %tmp.68 to %Bar*
	store %Bar* %tmp.69, %Bar** %tmp.13
	br label %case.exit.1

br_exit.7.1:
	br label %case.6.1

case.6.1:
	%vtpm.31 = icmp slt i32 %tmp.61, 6
	br i1 %vtpm.31, label %br_exit.6.1, label %src_gte_br.6.1

src_gte_br.6.1:
	%vtpm.32 = icmp sgt i32 %tmp.61, 8
	br i1 %vtpm.32, label %br_exit.6.1, label %src_lte_mc.6.1

src_lte_mc.6.1:
	store %Foo* %tmp.56, %Foo** %vtpm.13
	%tmp.70 = call %Razz* @Razz_new(  )
	%tmp.71 = bitcast %Razz* %tmp.70 to %Bar*
	store %Bar* %tmp.71, %Bar** %tmp.13
	br label %case.exit.1

br_exit.6.1:
	br label %abort

case.exit.1:
	%tmp.72 = load %Bar*, %Bar** %tmp.13
	%tmp.73 = bitcast %Bar* %tmp.72 to %Razz*
	store %Razz* %tmp.73, %Razz** %tmp.55
	%tmp.74 = getelementptr %Foo, %Foo* %tmp.18, i32 0, i32 5
	%tmp.75 = load %Foo*, %Foo** %tmp.12
	%tmp.76 = getelementptr %Foo, %Foo* %tmp.75, i32 0, i32 4
	%tmp.77 = load %Razz*, %Razz** %tmp.76
	%tmp.78 = icmp eq %Razz* %tmp.77, null
	br i1 %tmp.78, label %abort, label %ok.3

ok.3:
	%tmp.79 = getelementptr %Razz, %Razz* %tmp.77, i32 0, i32 0
	%tmp.80 = load %_Razz_vtable*, %_Razz_vtable** %tmp.79
	%tmp.81 = getelementptr %_Razz_vtable, %_Razz_vtable* %tmp.80, i32 0, i32 12
	%tmp.82 = load i32 (%Razz*) *, i32 (%Razz*) ** %tmp.81
	%tmp.83 = call i32(%Razz* ) %tmp.82( %Razz* %tmp.77 )
	%tmp.84 = load %Foo*, %Foo** %tmp.12
	%tmp.85 = getelementptr %Foo, %Foo* %tmp.84, i32 0, i32 2
	%tmp.86 = load %Foo*, %Foo** %tmp.85
	%tmp.87 = icmp eq %Foo* %tmp.86, null
	br i1 %tmp.87, label %abort, label %ok.4

ok.4:
	%tmp.88 = getelementptr %Foo, %Foo* %tmp.86, i32 0, i32 0
	%tmp.89 = load %_Foo_vtable*, %_Foo_vtable** %tmp.88
	%tmp.90 = getelementptr %_Foo_vtable, %_Foo_vtable* %tmp.89, i32 0, i32 12
	%tmp.91 = load i32 (%Foo*) *, i32 (%Foo*) ** %tmp.90
	%tmp.92 = call i32(%Foo* ) %tmp.91( %Foo* %tmp.86 )
	%tmp.93 = add i32 %tmp.83, %tmp.92
	%tmp.94 = load %Foo*, %Foo** %tmp.12
	%tmp.95 = icmp eq %Foo* %tmp.94, null
	br i1 %tmp.95, label %abort, label %ok.5

ok.5:
	%tmp.96 = getelementptr %Foo, %Foo* %tmp.94, i32 0, i32 0
	%tmp.97 = load %_Foo_vtable*, %_Foo_vtable** %tmp.96
	%tmp.98 = getelementptr %_Foo_vtable, %_Foo_vtable* %tmp.97, i32 0, i32 12
	%tmp.99 = load i32 (%Foo*) *, i32 (%Foo*) ** %tmp.98
	%tmp.100 = call i32(%Foo* ) %tmp.99( %Foo* %tmp.94 )
	%tmp.101 = add i32 %tmp.93, %tmp.100
	%tmp.102 = load %Foo*, %Foo** %tmp.12
	%tmp.103 = icmp eq %Foo* %tmp.102, null
	br i1 %tmp.103, label %abort, label %ok.6

ok.6:
	%tmp.104 = getelementptr %Foo, %Foo* %tmp.102, i32 0, i32 0
	%tmp.105 = load %_Foo_vtable*, %_Foo_vtable** %tmp.104
	%tmp.106 = getelementptr %_Foo_vtable, %_Foo_vtable* %tmp.105, i32 0, i32 11
	%tmp.107 = load i32 (%Foo*) *, i32 (%Foo*) ** %tmp.106
	%tmp.108 = call i32(%Foo* ) %tmp.107( %Foo* %tmp.102 )
	%tmp.109 = add i32 %tmp.101, %tmp.108
	store i32 %tmp.109, i32* %tmp.74
	ret %Foo* %tmp.18

abort:
	call void @abort(  )
	unreachable
}

define %Razz* @Razz_new() {

entry:
	%tmp.0 = alloca %Razz*
	%tmp.1 = alloca %Bar*
	%vtpm.33 = alloca %Razz*
	%vtpm.34 = alloca %Foo*
	%vtpm.35 = alloca %Bar*
	%tmp.2 = alloca %Foo*
	%vtpm.36 = alloca %Bazz*
	%vtpm.37 = alloca %Razz*
	%vtpm.38 = alloca %Foo*
	%vtpm.39 = alloca %Bar*
	%tmp.3 = alloca %Bar*
	%vtpm.40 = alloca %Razz*
	%vtpm.41 = alloca %Bar*
	%tmp.4 = getelementptr %_Razz_vtable, %_Razz_vtable* @_Razz_vtable_prototype, i32 0, i32 1
	%tmp.5 = load i32, i32* %tmp.4
	%tmp.6 = call i8*(i32 ) @malloc( i32 %tmp.5 )
	%tmp.7 = bitcast i8* %tmp.6 to %Razz*
	%malloc.null = icmp eq %Razz* %tmp.7, null
	br i1 %malloc.null, label %abort, label %okay

okay:
	%tmp.8 = getelementptr %Razz, %Razz* %tmp.7, i32 0, i32 0
	store %_Razz_vtable* @_Razz_vtable_prototype, %_Razz_vtable** %tmp.8
	store %Razz* %tmp.7, %Razz** %tmp.0
	%tmp.9 = getelementptr %Razz, %Razz* %tmp.7, i32 0, i32 1
	store i32 0, i32* %tmp.9
	%tmp.10 = getelementptr %Razz, %Razz* %tmp.7, i32 0, i32 2
	store %Foo* null, %Foo** %tmp.10
	%tmp.11 = getelementptr %Razz, %Razz* %tmp.7, i32 0, i32 3
	store %Object* null, %Object** %tmp.11
	%tmp.12 = getelementptr %Razz, %Razz* %tmp.7, i32 0, i32 4
	store %Razz* null, %Razz** %tmp.12
	%tmp.13 = getelementptr %Razz, %Razz* %tmp.7, i32 0, i32 5
	store i32 0, i32* %tmp.13
	%tmp.14 = getelementptr %Razz, %Razz* %tmp.7, i32 0, i32 6
	store %Bar* null, %Bar** %tmp.14
	%tmp.15 = getelementptr %Razz, %Razz* %tmp.7, i32 0, i32 7
	store i32 0, i32* %tmp.15
	%tmp.16 = getelementptr %Razz, %Razz* %tmp.7, i32 0, i32 1
	store i32 1, i32* %tmp.16
	%tmp.17 = getelementptr %Razz, %Razz* %tmp.7, i32 0, i32 2
	%tmp.18 = load %Razz*, %Razz** %tmp.0
	%tmp.19 = icmp eq %Razz* %tmp.18, null
	br i1 %tmp.19, label %abort, label %ok.0

ok.0:
	%tmp.20 = getelementptr %Razz, %Razz* %tmp.18, i32 0, i32 0
	%tmp.21 = load %_Razz_vtable*, %_Razz_vtable** %tmp.20
	%tmp.22 = getelementptr %_Razz_vtable, %_Razz_vtable* %tmp.21, i32 0, i32 0
	%tmp.23 = load i32, i32* %tmp.22
	br label %case.hdr.0

case.hdr.0:
	br label %case.8.0

case.8.0:
	%vtpm.42 = icmp slt i32 %tmp.23, 8
	br i1 %vtpm.42, label %br_exit.8.0, label %src_gte_br.8.0

src_gte_br.8.0:
	%vtpm.43 = icmp sgt i32 %tmp.23, 8
	br i1 %vtpm.43, label %br_exit.8.0, label %src_lte_mc.8.0

src_lte_mc.8.0:
	%tmp.24 = bitcast %Razz* %tmp.18 to %Bar*
	store %Bar* %tmp.24, %Bar** %vtpm.39
	%tmp.25 = load %Bar*, %Bar** %vtpm.39
	%tmp.26 = bitcast %Bar* %tmp.25 to %Foo*
	store %Foo* %tmp.26, %Foo** %tmp.2
	br label %case.exit.0

br_exit.8.0:
	br label %case.7.0

case.7.0:
	%vtpm.44 = icmp slt i32 %tmp.23, 7
	br i1 %vtpm.44, label %br_exit.7.0, label %src_gte_br.7.0

src_gte_br.7.0:
	%vtpm.45 = icmp sgt i32 %tmp.23, 8
	br i1 %vtpm.45, label %br_exit.7.0, label %src_lte_mc.7.0

src_lte_mc.7.0:
	store %Razz* %tmp.18, %Razz** %vtpm.37
	%tmp.27 = call %Bar* @Bar_new(  )
	%tmp.28 = bitcast %Bar* %tmp.27 to %Foo*
	store %Foo* %tmp.28, %Foo** %tmp.2
	br label %case.exit.0

br_exit.7.0:
	br label %case.6.0

case.6.0:
	%vtpm.46 = icmp slt i32 %tmp.23, 6
	br i1 %vtpm.46, label %br_exit.6.0, label %src_gte_br.6.0

src_gte_br.6.0:
	%vtpm.47 = icmp sgt i32 %tmp.23, 8
	br i1 %vtpm.47, label %br_exit.6.0, label %src_lte_mc.6.0

src_lte_mc.6.0:
	%tmp.29 = bitcast %Razz* %tmp.18 to %Foo*
	store %Foo* %tmp.29, %Foo** %vtpm.38
	%tmp.30 = call %Razz* @Razz_new(  )
	%tmp.31 = bitcast %Razz* %tmp.30 to %Foo*
	store %Foo* %tmp.31, %Foo** %tmp.2
	br label %case.exit.0

br_exit.6.0:
	br label %case.5.0

case.5.0:
	%vtpm.48 = icmp slt i32 %tmp.23, 5
	br i1 %vtpm.48, label %br_exit.5.0, label %src_gte_br.5.0

src_gte_br.5.0:
	%vtpm.49 = icmp sgt i32 %tmp.23, 8
	br i1 %vtpm.49, label %br_exit.5.0, label %src_lte_mc.5.0

src_lte_mc.5.0:
	%tmp.32 = bitcast %Razz* %tmp.18 to %Bazz*
	store %Bazz* %tmp.32, %Bazz** %vtpm.36
	%tmp.33 = call %Foo* @Foo_new(  )
	store %Foo* %tmp.33, %Foo** %tmp.2
	br label %case.exit.0

br_exit.5.0:
	br label %abort

case.exit.0:
	%tmp.34 = load %Foo*, %Foo** %tmp.2
	store %Foo* %tmp.34, %Foo** %tmp.17
	%tmp.35 = getelementptr %Razz, %Razz* %tmp.7, i32 0, i32 3
	%tmp.36 = load %Razz*, %Razz** %tmp.0
	%tmp.37 = icmp eq %Razz* %tmp.36, null
	br i1 %tmp.37, label %abort, label %ok.1

ok.1:
	%tmp.38 = getelementptr %Razz, %Razz* %tmp.36, i32 0, i32 0
	%tmp.39 = load %_Razz_vtable*, %_Razz_vtable** %tmp.38
	%tmp.40 = getelementptr %_Razz_vtable, %_Razz_vtable* %tmp.39, i32 0, i32 11
	%tmp.41 = load i32 (%Razz*) *, i32 (%Razz*) ** %tmp.40
	%tmp.42 = call i32(%Razz* ) %tmp.41( %Razz* %tmp.36 )
	%tmp.44 = call %Int* @Int_new(  )
	call void(%Int*, i32 ) @Int_init( %Int* %tmp.44, i32 %tmp.42 )
	%tmp.45 = bitcast %Int* %tmp.44 to %Object*
	store %Object* %tmp.45, %Object** %tmp.35
	%tmp.46 = getelementptr %Razz, %Razz* %tmp.7, i32 0, i32 4
	%tmp.47 = load %Razz*, %Razz** %tmp.0
	%tmp.48 = icmp eq %Razz* %tmp.47, null
	br i1 %tmp.48, label %abort, label %ok.2

ok.2:
	%tmp.49 = getelementptr %Razz, %Razz* %tmp.47, i32 0, i32 0
	%tmp.50 = load %_Razz_vtable*, %_Razz_vtable** %tmp.49
	%tmp.51 = getelementptr %_Razz_vtable, %_Razz_vtable* %tmp.50, i32 0, i32 0
	%tmp.52 = load i32, i32* %tmp.51
	br label %case.hdr.1

case.hdr.1:
	br label %case.8.1

case.8.1:
	%vtpm.50 = icmp slt i32 %tmp.52, 8
	br i1 %vtpm.50, label %br_exit.8.1, label %src_gte_br.8.1

src_gte_br.8.1:
	%vtpm.51 = icmp sgt i32 %tmp.52, 8
	br i1 %vtpm.51, label %br_exit.8.1, label %src_lte_mc.8.1

src_lte_mc.8.1:
	%tmp.53 = bitcast %Razz* %tmp.47 to %Bar*
	store %Bar* %tmp.53, %Bar** %vtpm.35
	%tmp.54 = load %Bar*, %Bar** %vtpm.35
	%tmp.55 = bitcast %Bar* %tmp.54 to %Razz*
	%tmp.56 = bitcast %Razz* %tmp.55 to %Bar*
	store %Bar* %tmp.56, %Bar** %tmp.1
	br label %case.exit.1

br_exit.8.1:
	br label %case.7.1

case.7.1:
	%vtpm.52 = icmp slt i32 %tmp.52, 7
	br i1 %vtpm.52, label %br_exit.7.1, label %src_gte_br.7.1

src_gte_br.7.1:
	%vtpm.53 = icmp sgt i32 %tmp.52, 8
	br i1 %vtpm.53, label %br_exit.7.1, label %src_lte_mc.7.1

src_lte_mc.7.1:
	store %Razz* %tmp.47, %Razz** %vtpm.33
	%tmp.57 = call %Bar* @Bar_new(  )
	%tmp.58 = bitcast %Bar* %tmp.57 to %Razz*
	%tmp.59 = bitcast %Razz* %tmp.58 to %Bar*
	store %Bar* %tmp.59, %Bar** %tmp.1
	br label %case.exit.1

br_exit.7.1:
	br label %case.6.1

case.6.1:
	%vtpm.54 = icmp slt i32 %tmp.52, 6
	br i1 %vtpm.54, label %br_exit.6.1, label %src_gte_br.6.1

src_gte_br.6.1:
	%vtpm.55 = icmp sgt i32 %tmp.52, 8
	br i1 %vtpm.55, label %br_exit.6.1, label %src_lte_mc.6.1

src_lte_mc.6.1:
	%tmp.60 = bitcast %Razz* %tmp.47 to %Foo*
	store %Foo* %tmp.60, %Foo** %vtpm.34
	%tmp.61 = call %Razz* @Razz_new(  )
	%tmp.62 = bitcast %Razz* %tmp.61 to %Bar*
	store %Bar* %tmp.62, %Bar** %tmp.1
	br label %case.exit.1

br_exit.6.1:
	br label %abort

case.exit.1:
	%tmp.63 = load %Bar*, %Bar** %tmp.1
	%tmp.64 = bitcast %Bar* %tmp.63 to %Razz*
	store %Razz* %tmp.64, %Razz** %tmp.46
	%tmp.65 = getelementptr %Razz, %Razz* %tmp.7, i32 0, i32 5
	%tmp.66 = load %Razz*, %Razz** %tmp.0
	%tmp.67 = getelementptr %Razz, %Razz* %tmp.66, i32 0, i32 4
	%tmp.68 = load %Razz*, %Razz** %tmp.67
	%tmp.69 = icmp eq %Razz* %tmp.68, null
	br i1 %tmp.69, label %abort, label %ok.3

ok.3:
	%tmp.70 = getelementptr %Razz, %Razz* %tmp.68, i32 0, i32 0
	%tmp.71 = load %_Razz_vtable*, %_Razz_vtable** %tmp.70
	%tmp.72 = getelementptr %_Razz_vtable, %_Razz_vtable* %tmp.71, i32 0, i32 12
	%tmp.73 = load i32 (%Razz*) *, i32 (%Razz*) ** %tmp.72
	%tmp.74 = call i32(%Razz* ) %tmp.73( %Razz* %tmp.68 )
	%tmp.75 = load %Razz*, %Razz** %tmp.0
	%tmp.76 = getelementptr %Razz, %Razz* %tmp.75, i32 0, i32 2
	%tmp.77 = load %Foo*, %Foo** %tmp.76
	%tmp.78 = icmp eq %Foo* %tmp.77, null
	br i1 %tmp.78, label %abort, label %ok.4

ok.4:
	%tmp.79 = getelementptr %Foo, %Foo* %tmp.77, i32 0, i32 0
	%tmp.80 = load %_Foo_vtable*, %_Foo_vtable** %tmp.79
	%tmp.81 = getelementptr %_Foo_vtable, %_Foo_vtable* %tmp.80, i32 0, i32 12
	%tmp.82 = load i32 (%Foo*) *, i32 (%Foo*) ** %tmp.81
	%tmp.83 = call i32(%Foo* ) %tmp.82( %Foo* %tmp.77 )
	%tmp.84 = add i32 %tmp.74, %tmp.83
	%tmp.85 = load %Razz*, %Razz** %tmp.0
	%tmp.86 = icmp eq %Razz* %tmp.85, null
	br i1 %tmp.86, label %abort, label %ok.5

ok.5:
	%tmp.87 = getelementptr %Razz, %Razz* %tmp.85, i32 0, i32 0
	%tmp.88 = load %_Razz_vtable*, %_Razz_vtable** %tmp.87
	%tmp.89 = getelementptr %_Razz_vtable, %_Razz_vtable* %tmp.88, i32 0, i32 12
	%tmp.90 = load i32 (%Razz*) *, i32 (%Razz*) ** %tmp.89
	%tmp.91 = call i32(%Razz* ) %tmp.90( %Razz* %tmp.85 )
	%tmp.92 = add i32 %tmp.84, %tmp.91
	%tmp.93 = load %Razz*, %Razz** %tmp.0
	%tmp.94 = icmp eq %Razz* %tmp.93, null
	br i1 %tmp.94, label %abort, label %ok.6

ok.6:
	%tmp.95 = getelementptr %Razz, %Razz* %tmp.93, i32 0, i32 0
	%tmp.96 = load %_Razz_vtable*, %_Razz_vtable** %tmp.95
	%tmp.97 = getelementptr %_Razz_vtable, %_Razz_vtable* %tmp.96, i32 0, i32 11
	%tmp.98 = load i32 (%Razz*) *, i32 (%Razz*) ** %tmp.97
	%tmp.99 = call i32(%Razz* ) %tmp.98( %Razz* %tmp.93 )
	%tmp.100 = add i32 %tmp.92, %tmp.99
	store i32 %tmp.100, i32* %tmp.65
	%tmp.101 = getelementptr %Razz, %Razz* %tmp.7, i32 0, i32 6
	%tmp.102 = load %Razz*, %Razz** %tmp.0
	%tmp.103 = icmp eq %Razz* %tmp.102, null
	br i1 %tmp.103, label %abort, label %ok.7

ok.7:
	%tmp.104 = getelementptr %Razz, %Razz* %tmp.102, i32 0, i32 0
	%tmp.105 = load %_Razz_vtable*, %_Razz_vtable** %tmp.104
	%tmp.106 = getelementptr %_Razz_vtable, %_Razz_vtable* %tmp.105, i32 0, i32 0
	%tmp.107 = load i32, i32* %tmp.106
	br label %case.hdr.2

case.hdr.2:
	br label %case.8.2

case.8.2:
	%vtpm.56 = icmp slt i32 %tmp.107, 8
	br i1 %vtpm.56, label %br_exit.8.2, label %src_gte_br.8.2

src_gte_br.8.2:
	%vtpm.57 = icmp sgt i32 %tmp.107, 8
	br i1 %vtpm.57, label %br_exit.8.2, label %src_lte_mc.8.2

src_lte_mc.8.2:
	%tmp.108 = bitcast %Razz* %tmp.102 to %Bar*
	store %Bar* %tmp.108, %Bar** %vtpm.41
	%tmp.109 = load %Bar*, %Bar** %vtpm.41
	store %Bar* %tmp.109, %Bar** %tmp.3
	br label %case.exit.2

br_exit.8.2:
	br label %case.7.2

case.7.2:
	%vtpm.58 = icmp slt i32 %tmp.107, 7
	br i1 %vtpm.58, label %br_exit.7.2, label %src_gte_br.7.2

src_gte_br.7.2:
	%vtpm.59 = icmp sgt i32 %tmp.107, 8
	br i1 %vtpm.59, label %br_exit.7.2, label %src_lte_mc.7.2

src_lte_mc.7.2:
	store %Razz* %tmp.102, %Razz** %vtpm.40
	%tmp.110 = call %Bar* @Bar_new(  )
	store %Bar* %tmp.110, %Bar** %tmp.3
	br label %case.exit.2

br_exit.7.2:
	br label %abort

case.exit.2:
	%tmp.111 = load %Bar*, %Bar** %tmp.3
	store %Bar* %tmp.111, %Bar** %tmp.101
	%tmp.112 = getelementptr %Razz, %Razz* %tmp.7, i32 0, i32 7
	%tmp.113 = load %Razz*, %Razz** %tmp.0
	%tmp.114 = getelementptr %Razz, %Razz* %tmp.113, i32 0, i32 4
	%tmp.115 = load %Razz*, %Razz** %tmp.114
	%tmp.116 = icmp eq %Razz* %tmp.115, null
	br i1 %tmp.116, label %abort, label %ok.8

ok.8:
	%tmp.117 = getelementptr %_Bazz_vtable, %_Bazz_vtable* @_Bazz_vtable_prototype, i32 0, i32 12
	%tmp.118 = load i32 (%Bazz*) *, i32 (%Bazz*) ** %tmp.117
	%tmp.119 = bitcast %Razz* %tmp.115 to %Bazz*
	%tmp.120 = call i32(%Bazz* ) %tmp.118( %Bazz* %tmp.119 )
	%tmp.121 = load %Razz*, %Razz** %tmp.0
	%tmp.122 = getelementptr %Razz, %Razz* %tmp.121, i32 0, i32 2
	%tmp.123 = load %Foo*, %Foo** %tmp.122
	%tmp.124 = icmp eq %Foo* %tmp.123, null
	br i1 %tmp.124, label %abort, label %ok.9

ok.9:
	%tmp.125 = getelementptr %Foo, %Foo* %tmp.123, i32 0, i32 0
	%tmp.126 = load %_Foo_vtable*, %_Foo_vtable** %tmp.125
	%tmp.127 = getelementptr %_Foo_vtable, %_Foo_vtable* %tmp.126, i32 0, i32 12
	%tmp.128 = load i32 (%Foo*) *, i32 (%Foo*) ** %tmp.127
	%tmp.129 = call i32(%Foo* ) %tmp.128( %Foo* %tmp.123 )
	%tmp.130 = add i32 %tmp.120, %tmp.129
	%tmp.131 = load %Razz*, %Razz** %tmp.0
	%tmp.132 = getelementptr %Razz, %Razz* %tmp.131, i32 0, i32 6
	%tmp.133 = load %Bar*, %Bar** %tmp.132
	%tmp.134 = icmp eq %Bar* %tmp.133, null
	br i1 %tmp.134, label %abort, label %ok.10

ok.10:
	%tmp.135 = getelementptr %Bar, %Bar* %tmp.133, i32 0, i32 0
	%tmp.136 = load %_Bar_vtable*, %_Bar_vtable** %tmp.135
	%tmp.137 = getelementptr %_Bar_vtable, %_Bar_vtable* %tmp.136, i32 0, i32 12
	%tmp.138 = load i32 (%Bar*) *, i32 (%Bar*) ** %tmp.137
	%tmp.139 = call i32(%Bar* ) %tmp.138( %Bar* %tmp.133 )
	%tmp.140 = add i32 %tmp.130, %tmp.139
	%tmp.141 = load %Razz*, %Razz** %tmp.0
	%tmp.142 = icmp eq %Razz* %tmp.141, null
	br i1 %tmp.142, label %abort, label %ok.11

ok.11:
	%tmp.143 = getelementptr %Razz, %Razz* %tmp.141, i32 0, i32 0
	%tmp.144 = load %_Razz_vtable*, %_Razz_vtable** %tmp.143
	%tmp.145 = getelementptr %_Razz_vtable, %_Razz_vtable* %tmp.144, i32 0, i32 12
	%tmp.146 = load i32 (%Razz*) *, i32 (%Razz*) ** %tmp.145
	%tmp.147 = call i32(%Razz* ) %tmp.146( %Razz* %tmp.141 )
	%tmp.148 = add i32 %tmp.140, %tmp.147
	%tmp.149 = load %Razz*, %Razz** %tmp.0
	%tmp.150 = icmp eq %Razz* %tmp.149, null
	br i1 %tmp.150, label %abort, label %ok.12

ok.12:
	%tmp.151 = getelementptr %Razz, %Razz* %tmp.149, i32 0, i32 0
	%tmp.152 = load %_Razz_vtable*, %_Razz_vtable** %tmp.151
	%tmp.153 = getelementptr %_Razz_vtable, %_Razz_vtable* %tmp.152, i32 0, i32 11
	%tmp.154 = load i32 (%Razz*) *, i32 (%Razz*) ** %tmp.153
	%tmp.155 = call i32(%Razz* ) %tmp.154( %Razz* %tmp.149 )
	%tmp.156 = add i32 %tmp.148, %tmp.155
	store i32 %tmp.156, i32* %tmp.112
	ret %Razz* %tmp.7

abort:
	call void @abort(  )
	unreachable
}

define %Bar* @Bar_new() {

entry:
	%tmp.0 = alloca %Bar*
	%tmp.1 = alloca %Bar*
	%vtpm.60 = alloca %Razz*
	%vtpm.61 = alloca %Foo*
	%vtpm.62 = alloca %Bar*
	%tmp.2 = alloca %Foo*
	%vtpm.63 = alloca %Bazz*
	%vtpm.64 = alloca %Razz*
	%vtpm.65 = alloca %Foo*
	%vtpm.66 = alloca %Bar*
	%tmp.3 = alloca %Bar*
	%vtpm.67 = alloca %Razz*
	%vtpm.68 = alloca %Bar*
	%tmp.4 = getelementptr %_Bar_vtable, %_Bar_vtable* @_Bar_vtable_prototype, i32 0, i32 1
	%tmp.5 = load i32, i32* %tmp.4
	%tmp.6 = call i8*(i32 ) @malloc( i32 %tmp.5 )
	%tmp.7 = bitcast i8* %tmp.6 to %Bar*
	%malloc.null = icmp eq %Bar* %tmp.7, null
	br i1 %malloc.null, label %abort, label %okay

okay:
	%tmp.8 = getelementptr %Bar, %Bar* %tmp.7, i32 0, i32 0
	store %_Bar_vtable* @_Bar_vtable_prototype, %_Bar_vtable** %tmp.8
	store %Bar* %tmp.7, %Bar** %tmp.0
	%tmp.9 = getelementptr %Bar, %Bar* %tmp.7, i32 0, i32 1
	store i32 0, i32* %tmp.9
	%tmp.10 = getelementptr %Bar, %Bar* %tmp.7, i32 0, i32 2
	store %Foo* null, %Foo** %tmp.10
	%tmp.11 = getelementptr %Bar, %Bar* %tmp.7, i32 0, i32 3
	store %Object* null, %Object** %tmp.11
	%tmp.12 = getelementptr %Bar, %Bar* %tmp.7, i32 0, i32 4
	store %Razz* null, %Razz** %tmp.12
	%tmp.13 = getelementptr %Bar, %Bar* %tmp.7, i32 0, i32 5
	store i32 0, i32* %tmp.13
	%tmp.14 = getelementptr %Bar, %Bar* %tmp.7, i32 0, i32 6
	store %Bar* null, %Bar** %tmp.14
	%tmp.15 = getelementptr %Bar, %Bar* %tmp.7, i32 0, i32 7
	store i32 0, i32* %tmp.15
	%tmp.16 = getelementptr %Bar, %Bar* %tmp.7, i32 0, i32 8
	store i32 0, i32* %tmp.16
	%tmp.17 = getelementptr %Bar, %Bar* %tmp.7, i32 0, i32 9
	store %Object* null, %Object** %tmp.17
	%tmp.18 = getelementptr %Bar, %Bar* %tmp.7, i32 0, i32 1
	store i32 1, i32* %tmp.18
	%tmp.19 = getelementptr %Bar, %Bar* %tmp.7, i32 0, i32 2
	%tmp.20 = load %Bar*, %Bar** %tmp.0
	%tmp.21 = icmp eq %Bar* %tmp.20, null
	br i1 %tmp.21, label %abort, label %ok.0

ok.0:
	%tmp.22 = getelementptr %Bar, %Bar* %tmp.20, i32 0, i32 0
	%tmp.23 = load %_Bar_vtable*, %_Bar_vtable** %tmp.22
	%tmp.24 = getelementptr %_Bar_vtable, %_Bar_vtable* %tmp.23, i32 0, i32 0
	%tmp.25 = load i32, i32* %tmp.24
	br label %case.hdr.0

case.hdr.0:
	br label %case.8.0

case.8.0:
	%vtpm.69 = icmp slt i32 %tmp.25, 8
	br i1 %vtpm.69, label %br_exit.8.0, label %src_gte_br.8.0

src_gte_br.8.0:
	%vtpm.70 = icmp sgt i32 %tmp.25, 8
	br i1 %vtpm.70, label %br_exit.8.0, label %src_lte_mc.8.0

src_lte_mc.8.0:
	store %Bar* %tmp.20, %Bar** %vtpm.66
	%tmp.26 = load %Bar*, %Bar** %vtpm.66
	%tmp.27 = bitcast %Bar* %tmp.26 to %Foo*
	store %Foo* %tmp.27, %Foo** %tmp.2
	br label %case.exit.0

br_exit.8.0:
	br label %case.7.0

case.7.0:
	%vtpm.71 = icmp slt i32 %tmp.25, 7
	br i1 %vtpm.71, label %br_exit.7.0, label %src_gte_br.7.0

src_gte_br.7.0:
	%vtpm.72 = icmp sgt i32 %tmp.25, 8
	br i1 %vtpm.72, label %br_exit.7.0, label %src_lte_mc.7.0

src_lte_mc.7.0:
	%tmp.28 = bitcast %Bar* %tmp.20 to %Razz*
	store %Razz* %tmp.28, %Razz** %vtpm.64
	%tmp.29 = call %Bar* @Bar_new(  )
	%tmp.30 = bitcast %Bar* %tmp.29 to %Foo*
	store %Foo* %tmp.30, %Foo** %tmp.2
	br label %case.exit.0

br_exit.7.0:
	br label %case.6.0

case.6.0:
	%vtpm.73 = icmp slt i32 %tmp.25, 6
	br i1 %vtpm.73, label %br_exit.6.0, label %src_gte_br.6.0

src_gte_br.6.0:
	%vtpm.74 = icmp sgt i32 %tmp.25, 8
	br i1 %vtpm.74, label %br_exit.6.0, label %src_lte_mc.6.0

src_lte_mc.6.0:
	%tmp.31 = bitcast %Bar* %tmp.20 to %Foo*
	store %Foo* %tmp.31, %Foo** %vtpm.65
	%tmp.32 = call %Razz* @Razz_new(  )
	%tmp.33 = bitcast %Razz* %tmp.32 to %Foo*
	store %Foo* %tmp.33, %Foo** %tmp.2
	br label %case.exit.0

br_exit.6.0:
	br label %case.5.0

case.5.0:
	%vtpm.75 = icmp slt i32 %tmp.25, 5
	br i1 %vtpm.75, label %br_exit.5.0, label %src_gte_br.5.0

src_gte_br.5.0:
	%vtpm.76 = icmp sgt i32 %tmp.25, 8
	br i1 %vtpm.76, label %br_exit.5.0, label %src_lte_mc.5.0

src_lte_mc.5.0:
	%tmp.34 = bitcast %Bar* %tmp.20 to %Bazz*
	store %Bazz* %tmp.34, %Bazz** %vtpm.63
	%tmp.35 = call %Foo* @Foo_new(  )
	store %Foo* %tmp.35, %Foo** %tmp.2
	br label %case.exit.0

br_exit.5.0:
	br label %abort

case.exit.0:
	%tmp.36 = load %Foo*, %Foo** %tmp.2
	store %Foo* %tmp.36, %Foo** %tmp.19
	%tmp.37 = getelementptr %Bar, %Bar* %tmp.7, i32 0, i32 3
	%tmp.38 = load %Bar*, %Bar** %tmp.0
	%tmp.39 = icmp eq %Bar* %tmp.38, null
	br i1 %tmp.39, label %abort, label %ok.1

ok.1:
	%tmp.40 = getelementptr %Bar, %Bar* %tmp.38, i32 0, i32 0
	%tmp.41 = load %_Bar_vtable*, %_Bar_vtable** %tmp.40
	%tmp.42 = getelementptr %_Bar_vtable, %_Bar_vtable* %tmp.41, i32 0, i32 11
	%tmp.43 = load i32 (%Bar*) *, i32 (%Bar*) ** %tmp.42
	%tmp.44 = call i32(%Bar* ) %tmp.43( %Bar* %tmp.38 )
	%tmp.46 = call %Int* @Int_new(  )
	call void(%Int*, i32 ) @Int_init( %Int* %tmp.46, i32 %tmp.44 )
	%tmp.47 = bitcast %Int* %tmp.46 to %Object*
	store %Object* %tmp.47, %Object** %tmp.37
	%tmp.48 = getelementptr %Bar, %Bar* %tmp.7, i32 0, i32 4
	%tmp.49 = load %Bar*, %Bar** %tmp.0
	%tmp.50 = icmp eq %Bar* %tmp.49, null
	br i1 %tmp.50, label %abort, label %ok.2

ok.2:
	%tmp.51 = getelementptr %Bar, %Bar* %tmp.49, i32 0, i32 0
	%tmp.52 = load %_Bar_vtable*, %_Bar_vtable** %tmp.51
	%tmp.53 = getelementptr %_Bar_vtable, %_Bar_vtable* %tmp.52, i32 0, i32 0
	%tmp.54 = load i32, i32* %tmp.53
	br label %case.hdr.1

case.hdr.1:
	br label %case.8.1

case.8.1:
	%vtpm.77 = icmp slt i32 %tmp.54, 8
	br i1 %vtpm.77, label %br_exit.8.1, label %src_gte_br.8.1

src_gte_br.8.1:
	%vtpm.78 = icmp sgt i32 %tmp.54, 8
	br i1 %vtpm.78, label %br_exit.8.1, label %src_lte_mc.8.1

src_lte_mc.8.1:
	store %Bar* %tmp.49, %Bar** %vtpm.62
	%tmp.55 = load %Bar*, %Bar** %vtpm.62
	%tmp.56 = bitcast %Bar* %tmp.55 to %Razz*
	%tmp.57 = bitcast %Razz* %tmp.56 to %Bar*
	store %Bar* %tmp.57, %Bar** %tmp.1
	br label %case.exit.1

br_exit.8.1:
	br label %case.7.1

case.7.1:
	%vtpm.79 = icmp slt i32 %tmp.54, 7
	br i1 %vtpm.79, label %br_exit.7.1, label %src_gte_br.7.1

src_gte_br.7.1:
	%vtpm.80 = icmp sgt i32 %tmp.54, 8
	br i1 %vtpm.80, label %br_exit.7.1, label %src_lte_mc.7.1

src_lte_mc.7.1:
	%tmp.58 = bitcast %Bar* %tmp.49 to %Razz*
	store %Razz* %tmp.58, %Razz** %vtpm.60
	%tmp.59 = call %Bar* @Bar_new(  )
	%tmp.60 = bitcast %Bar* %tmp.59 to %Razz*
	%tmp.61 = bitcast %Razz* %tmp.60 to %Bar*
	store %Bar* %tmp.61, %Bar** %tmp.1
	br label %case.exit.1

br_exit.7.1:
	br label %case.6.1

case.6.1:
	%vtpm.81 = icmp slt i32 %tmp.54, 6
	br i1 %vtpm.81, label %br_exit.6.1, label %src_gte_br.6.1

src_gte_br.6.1:
	%vtpm.82 = icmp sgt i32 %tmp.54, 8
	br i1 %vtpm.82, label %br_exit.6.1, label %src_lte_mc.6.1

src_lte_mc.6.1:
	%tmp.62 = bitcast %Bar* %tmp.49 to %Foo*
	store %Foo* %tmp.62, %Foo** %vtpm.61
	%tmp.63 = call %Razz* @Razz_new(  )
	%tmp.64 = bitcast %Razz* %tmp.63 to %Bar*
	store %Bar* %tmp.64, %Bar** %tmp.1
	br label %case.exit.1

br_exit.6.1:
	br label %abort

case.exit.1:
	%tmp.65 = load %Bar*, %Bar** %tmp.1
	%tmp.66 = bitcast %Bar* %tmp.65 to %Razz*
	store %Razz* %tmp.66, %Razz** %tmp.48
	%tmp.67 = getelementptr %Bar, %Bar* %tmp.7, i32 0, i32 5
	%tmp.68 = load %Bar*, %Bar** %tmp.0
	%tmp.69 = getelementptr %Bar, %Bar* %tmp.68, i32 0, i32 4
	%tmp.70 = load %Razz*, %Razz** %tmp.69
	%tmp.71 = icmp eq %Razz* %tmp.70, null
	br i1 %tmp.71, label %abort, label %ok.3

ok.3:
	%tmp.72 = getelementptr %Razz, %Razz* %tmp.70, i32 0, i32 0
	%tmp.73 = load %_Razz_vtable*, %_Razz_vtable** %tmp.72
	%tmp.74 = getelementptr %_Razz_vtable, %_Razz_vtable* %tmp.73, i32 0, i32 12
	%tmp.75 = load i32 (%Razz*) *, i32 (%Razz*) ** %tmp.74
	%tmp.76 = call i32(%Razz* ) %tmp.75( %Razz* %tmp.70 )
	%tmp.77 = load %Bar*, %Bar** %tmp.0
	%tmp.78 = getelementptr %Bar, %Bar* %tmp.77, i32 0, i32 2
	%tmp.79 = load %Foo*, %Foo** %tmp.78
	%tmp.80 = icmp eq %Foo* %tmp.79, null
	br i1 %tmp.80, label %abort, label %ok.4

ok.4:
	%tmp.81 = getelementptr %Foo, %Foo* %tmp.79, i32 0, i32 0
	%tmp.82 = load %_Foo_vtable*, %_Foo_vtable** %tmp.81
	%tmp.83 = getelementptr %_Foo_vtable, %_Foo_vtable* %tmp.82, i32 0, i32 12
	%tmp.84 = load i32 (%Foo*) *, i32 (%Foo*) ** %tmp.83
	%tmp.85 = call i32(%Foo* ) %tmp.84( %Foo* %tmp.79 )
	%tmp.86 = add i32 %tmp.76, %tmp.85
	%tmp.87 = load %Bar*, %Bar** %tmp.0
	%tmp.88 = icmp eq %Bar* %tmp.87, null
	br i1 %tmp.88, label %abort, label %ok.5

ok.5:
	%tmp.89 = getelementptr %Bar, %Bar* %tmp.87, i32 0, i32 0
	%tmp.90 = load %_Bar_vtable*, %_Bar_vtable** %tmp.89
	%tmp.91 = getelementptr %_Bar_vtable, %_Bar_vtable* %tmp.90, i32 0, i32 12
	%tmp.92 = load i32 (%Bar*) *, i32 (%Bar*) ** %tmp.91
	%tmp.93 = call i32(%Bar* ) %tmp.92( %Bar* %tmp.87 )
	%tmp.94 = add i32 %tmp.86, %tmp.93
	%tmp.95 = load %Bar*, %Bar** %tmp.0
	%tmp.96 = icmp eq %Bar* %tmp.95, null
	br i1 %tmp.96, label %abort, label %ok.6

ok.6:
	%tmp.97 = getelementptr %Bar, %Bar* %tmp.95, i32 0, i32 0
	%tmp.98 = load %_Bar_vtable*, %_Bar_vtable** %tmp.97
	%tmp.99 = getelementptr %_Bar_vtable, %_Bar_vtable* %tmp.98, i32 0, i32 11
	%tmp.100 = load i32 (%Bar*) *, i32 (%Bar*) ** %tmp.99
	%tmp.101 = call i32(%Bar* ) %tmp.100( %Bar* %tmp.95 )
	%tmp.102 = add i32 %tmp.94, %tmp.101
	store i32 %tmp.102, i32* %tmp.67
	%tmp.103 = getelementptr %Bar, %Bar* %tmp.7, i32 0, i32 6
	%tmp.104 = load %Bar*, %Bar** %tmp.0
	%tmp.105 = icmp eq %Bar* %tmp.104, null
	br i1 %tmp.105, label %abort, label %ok.7

ok.7:
	%tmp.106 = getelementptr %Bar, %Bar* %tmp.104, i32 0, i32 0
	%tmp.107 = load %_Bar_vtable*, %_Bar_vtable** %tmp.106
	%tmp.108 = getelementptr %_Bar_vtable, %_Bar_vtable* %tmp.107, i32 0, i32 0
	%tmp.109 = load i32, i32* %tmp.108
	br label %case.hdr.2

case.hdr.2:
	br label %case.8.2

case.8.2:
	%vtpm.83 = icmp slt i32 %tmp.109, 8
	br i1 %vtpm.83, label %br_exit.8.2, label %src_gte_br.8.2

src_gte_br.8.2:
	%vtpm.84 = icmp sgt i32 %tmp.109, 8
	br i1 %vtpm.84, label %br_exit.8.2, label %src_lte_mc.8.2

src_lte_mc.8.2:
	store %Bar* %tmp.104, %Bar** %vtpm.68
	%tmp.110 = load %Bar*, %Bar** %vtpm.68
	store %Bar* %tmp.110, %Bar** %tmp.3
	br label %case.exit.2

br_exit.8.2:
	br label %case.7.2

case.7.2:
	%vtpm.85 = icmp slt i32 %tmp.109, 7
	br i1 %vtpm.85, label %br_exit.7.2, label %src_gte_br.7.2

src_gte_br.7.2:
	%vtpm.86 = icmp sgt i32 %tmp.109, 8
	br i1 %vtpm.86, label %br_exit.7.2, label %src_lte_mc.7.2

src_lte_mc.7.2:
	%tmp.111 = bitcast %Bar* %tmp.104 to %Razz*
	store %Razz* %tmp.111, %Razz** %vtpm.67
	%tmp.112 = call %Bar* @Bar_new(  )
	store %Bar* %tmp.112, %Bar** %tmp.3
	br label %case.exit.2

br_exit.7.2:
	br label %abort

case.exit.2:
	%tmp.113 = load %Bar*, %Bar** %tmp.3
	store %Bar* %tmp.113, %Bar** %tmp.103
	%tmp.114 = getelementptr %Bar, %Bar* %tmp.7, i32 0, i32 7
	%tmp.115 = load %Bar*, %Bar** %tmp.0
	%tmp.116 = getelementptr %Bar, %Bar* %tmp.115, i32 0, i32 4
	%tmp.117 = load %Razz*, %Razz** %tmp.116
	%tmp.118 = icmp eq %Razz* %tmp.117, null
	br i1 %tmp.118, label %abort, label %ok.8

ok.8:
	%tmp.119 = getelementptr %_Bazz_vtable, %_Bazz_vtable* @_Bazz_vtable_prototype, i32 0, i32 12
	%tmp.120 = load i32 (%Bazz*) *, i32 (%Bazz*) ** %tmp.119
	%tmp.121 = bitcast %Razz* %tmp.117 to %Bazz*
	%tmp.122 = call i32(%Bazz* ) %tmp.120( %Bazz* %tmp.121 )
	%tmp.123 = load %Bar*, %Bar** %tmp.0
	%tmp.124 = getelementptr %Bar, %Bar* %tmp.123, i32 0, i32 2
	%tmp.125 = load %Foo*, %Foo** %tmp.124
	%tmp.126 = icmp eq %Foo* %tmp.125, null
	br i1 %tmp.126, label %abort, label %ok.9

ok.9:
	%tmp.127 = getelementptr %Foo, %Foo* %tmp.125, i32 0, i32 0
	%tmp.128 = load %_Foo_vtable*, %_Foo_vtable** %tmp.127
	%tmp.129 = getelementptr %_Foo_vtable, %_Foo_vtable* %tmp.128, i32 0, i32 12
	%tmp.130 = load i32 (%Foo*) *, i32 (%Foo*) ** %tmp.129
	%tmp.131 = call i32(%Foo* ) %tmp.130( %Foo* %tmp.125 )
	%tmp.132 = add i32 %tmp.122, %tmp.131
	%tmp.133 = load %Bar*, %Bar** %tmp.0
	%tmp.134 = getelementptr %Bar, %Bar* %tmp.133, i32 0, i32 6
	%tmp.135 = load %Bar*, %Bar** %tmp.134
	%tmp.136 = icmp eq %Bar* %tmp.135, null
	br i1 %tmp.136, label %abort, label %ok.10

ok.10:
	%tmp.137 = getelementptr %Bar, %Bar* %tmp.135, i32 0, i32 0
	%tmp.138 = load %_Bar_vtable*, %_Bar_vtable** %tmp.137
	%tmp.139 = getelementptr %_Bar_vtable, %_Bar_vtable* %tmp.138, i32 0, i32 12
	%tmp.140 = load i32 (%Bar*) *, i32 (%Bar*) ** %tmp.139
	%tmp.141 = call i32(%Bar* ) %tmp.140( %Bar* %tmp.135 )
	%tmp.142 = add i32 %tmp.132, %tmp.141
	%tmp.143 = load %Bar*, %Bar** %tmp.0
	%tmp.144 = icmp eq %Bar* %tmp.143, null
	br i1 %tmp.144, label %abort, label %ok.11

ok.11:
	%tmp.145 = getelementptr %Bar, %Bar* %tmp.143, i32 0, i32 0
	%tmp.146 = load %_Bar_vtable*, %_Bar_vtable** %tmp.145
	%tmp.147 = getelementptr %_Bar_vtable, %_Bar_vtable* %tmp.146, i32 0, i32 12
	%tmp.148 = load i32 (%Bar*) *, i32 (%Bar*) ** %tmp.147
	%tmp.149 = call i32(%Bar* ) %tmp.148( %Bar* %tmp.143 )
	%tmp.150 = add i32 %tmp.142, %tmp.149
	%tmp.151 = load %Bar*, %Bar** %tmp.0
	%tmp.152 = icmp eq %Bar* %tmp.151, null
	br i1 %tmp.152, label %abort, label %ok.12

ok.12:
	%tmp.153 = getelementptr %Bar, %Bar* %tmp.151, i32 0, i32 0
	%tmp.154 = load %_Bar_vtable*, %_Bar_vtable** %tmp.153
	%tmp.155 = getelementptr %_Bar_vtable, %_Bar_vtable* %tmp.154, i32 0, i32 11
	%tmp.156 = load i32 (%Bar*) *, i32 (%Bar*) ** %tmp.155
	%tmp.157 = call i32(%Bar* ) %tmp.156( %Bar* %tmp.151 )
	%tmp.158 = add i32 %tmp.150, %tmp.157
	store i32 %tmp.158, i32* %tmp.114
	%tmp.159 = getelementptr %Bar, %Bar* %tmp.7, i32 0, i32 8
	%tmp.160 = load %Bar*, %Bar** %tmp.0
	%tmp.161 = icmp eq %Bar* %tmp.160, null
	br i1 %tmp.161, label %abort, label %ok.13

ok.13:
	%tmp.162 = getelementptr %Bar, %Bar* %tmp.160, i32 0, i32 0
	%tmp.163 = load %_Bar_vtable*, %_Bar_vtable** %tmp.162
	%tmp.164 = getelementptr %_Bar_vtable, %_Bar_vtable* %tmp.163, i32 0, i32 12
	%tmp.165 = load i32 (%Bar*) *, i32 (%Bar*) ** %tmp.164
	%tmp.166 = call i32(%Bar* ) %tmp.165( %Bar* %tmp.160 )
	store i32 %tmp.166, i32* %tmp.159
	%tmp.167 = getelementptr %Bar, %Bar* %tmp.7, i32 0, i32 9
	%tmp.168 = load %Bar*, %Bar** %tmp.0
	%tmp.169 = icmp eq %Bar* %tmp.168, null
	br i1 %tmp.169, label %abort, label %ok.14

ok.14:
	%tmp.170 = getelementptr %Bar, %Bar* %tmp.168, i32 0, i32 0
	%tmp.171 = load %_Bar_vtable*, %_Bar_vtable** %tmp.170
	%tmp.172 = getelementptr %_Bar_vtable, %_Bar_vtable* %tmp.171, i32 0, i32 11
	%tmp.173 = load i32 (%Bar*) *, i32 (%Bar*) ** %tmp.172
	%tmp.174 = call i32(%Bar* ) %tmp.173( %Bar* %tmp.168 )
	%tmp.176 = call %Int* @Int_new(  )
	call void(%Int*, i32 ) @Int_init( %Int* %tmp.176, i32 %tmp.174 )
	%tmp.177 = bitcast %Int* %tmp.176 to %Object*
	store %Object* %tmp.177, %Object** %tmp.167
	ret %Bar* %tmp.7

abort:
	call void @abort(  )
	unreachable
}

define %String* @Main.main(%Main* %self) {

entry:
	%tmp.0 = alloca %Main*
	store %Main* %self, %Main** %tmp.0
	ret %String* @String.1

abort:
	call void @abort(  )
	unreachable
}

define %Main* @Main_new() {

entry:
	%tmp.1 = alloca %Main*
	%tmp.2 = getelementptr %_Main_vtable, %_Main_vtable* @_Main_vtable_prototype, i32 0, i32 1
	%tmp.3 = load i32, i32* %tmp.2
	%tmp.4 = call i8*(i32 ) @malloc( i32 %tmp.3 )
	%tmp.5 = bitcast i8* %tmp.4 to %Main*
	%malloc.null = icmp eq %Main* %tmp.5, null
	br i1 %malloc.null, label %abort, label %okay

okay:
	%tmp.6 = getelementptr %Main, %Main* %tmp.5, i32 0, i32 0
	store %_Main_vtable* @_Main_vtable_prototype, %_Main_vtable** %tmp.6
	store %Main* %tmp.5, %Main** %tmp.1
	%tmp.7 = getelementptr %Main, %Main* %tmp.5, i32 0, i32 1
	store %Bazz* null, %Bazz** %tmp.7
	%tmp.8 = getelementptr %Main, %Main* %tmp.5, i32 0, i32 2
	store %Foo* null, %Foo** %tmp.8
	%tmp.9 = getelementptr %Main, %Main* %tmp.5, i32 0, i32 3
	store %Razz* null, %Razz** %tmp.9
	%tmp.10 = getelementptr %Main, %Main* %tmp.5, i32 0, i32 4
	store %Bar* null, %Bar** %tmp.10
	%tmp.11 = getelementptr %Main, %Main* %tmp.5, i32 0, i32 1
	%tmp.12 = call %Bazz* @Bazz_new(  )
	store %Bazz* %tmp.12, %Bazz** %tmp.11
	%tmp.13 = getelementptr %Main, %Main* %tmp.5, i32 0, i32 2
	%tmp.14 = call %Foo* @Foo_new(  )
	store %Foo* %tmp.14, %Foo** %tmp.13
	%tmp.15 = getelementptr %Main, %Main* %tmp.5, i32 0, i32 3
	%tmp.16 = call %Razz* @Razz_new(  )
	store %Razz* %tmp.16, %Razz** %tmp.15
	%tmp.17 = getelementptr %Main, %Main* %tmp.5, i32 0, i32 4
	%tmp.18 = call %Bar* @Bar_new(  )
	store %Bar* %tmp.18, %Bar** %tmp.17
	ret %Main* %tmp.5

abort:
	call void @abort(  )
	unreachable
}

