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

@str.Book = internal constant [5 x i8] c"Book\00"
%Book = type {
	%_Book_vtable*,
	%String*,
	%String*
}

%_Book_vtable = type {
	i32,
	i32,
	i8*,
	%Book* () *,
	%Object* (%Book*) *,
	%String* (%Book*) *,
	%Book* (%Book*) *,
	%Book* (%Book*,%String*) *,
	%Book* (%Book*,i32) *,
	%String* (%Book*) *,
	i32 (%Book*) *,
	%Book* (%Book*,%String*,%String*) *,
	%Object* (%Book*) *,
	%String* (%Book*,%String*) *
}

@_Book_vtable_prototype = constant %_Book_vtable {
	i32 5,
	i32 ptrtoint (%Book* getelementptr (%Book, %Book* null, i32 1) to i32),
	i8* getelementptr ([5 x i8], [5 x i8]* @str.Book, i32 0, i32 0),
	%Book* () * @Book_new,
	%Object* (%Book*) * bitcast (%Object* (%Object*) * @Object_abort to %Object* (%Book*) *),
	%String* (%Book*) * bitcast (%String* (%Object*) * @Object_type_name to %String* (%Book*) *),
	%Book* (%Book*) * bitcast (%Object* (%Object*) * @Object_copy to %Book* (%Book*) *),
	%Book* (%Book*,%String*) * bitcast (%IO* (%IO*,%String*) * @IO_out_string to %Book* (%Book*,%String*) *),
	%Book* (%Book*,i32) * bitcast (%IO* (%IO*,i32) * @IO_out_int to %Book* (%Book*,i32) *),
	%String* (%Book*) * bitcast (%String* (%IO*) * @IO_in_string to %String* (%Book*) *),
	i32 (%Book*) * bitcast (i32 (%IO*) * @IO_in_int to i32 (%Book*) *),
	%Book* (%Book*,%String*,%String*) * @Book.initBook,
	%Object* (%Book*) * @Book.getSelf,
	%String* (%Book*,%String*) * @Book.print
}

@str.Article = internal constant [8 x i8] c"Article\00"
%Article = type {
	%_Article_vtable*,
	%String*,
	%String*,
	%String*
}

%_Article_vtable = type {
	i32,
	i32,
	i8*,
	%Article* () *,
	%Object* (%Article*) *,
	%String* (%Article*) *,
	%Article* (%Article*) *,
	%Article* (%Article*,%String*) *,
	%Article* (%Article*,i32) *,
	%String* (%Article*) *,
	i32 (%Article*) *,
	%Book* (%Article*,%String*,%String*) *,
	%Object* (%Article*) *,
	%String* (%Article*,%String*) *,
	%Article* (%Article*,%String*,%String*,%String*) *
}

@_Article_vtable_prototype = constant %_Article_vtable {
	i32 6,
	i32 ptrtoint (%Article* getelementptr (%Article, %Article* null, i32 1) to i32),
	i8* getelementptr ([8 x i8], [8 x i8]* @str.Article, i32 0, i32 0),
	%Article* () * @Article_new,
	%Object* (%Article*) * bitcast (%Object* (%Object*) * @Object_abort to %Object* (%Article*) *),
	%String* (%Article*) * bitcast (%String* (%Object*) * @Object_type_name to %String* (%Article*) *),
	%Article* (%Article*) * bitcast (%Object* (%Object*) * @Object_copy to %Article* (%Article*) *),
	%Article* (%Article*,%String*) * bitcast (%IO* (%IO*,%String*) * @IO_out_string to %Article* (%Article*,%String*) *),
	%Article* (%Article*,i32) * bitcast (%IO* (%IO*,i32) * @IO_out_int to %Article* (%Article*,i32) *),
	%String* (%Article*) * bitcast (%String* (%IO*) * @IO_in_string to %String* (%Article*) *),
	i32 (%Article*) * bitcast (i32 (%IO*) * @IO_in_int to i32 (%Article*) *),
	%Book* (%Article*,%String*,%String*) * bitcast (%Book* (%Book*,%String*,%String*) * @Book.initBook to %Book* (%Article*,%String*,%String*) *),
	%Object* (%Article*) * @Article.getSelf,
	%String* (%Article*,%String*) * @Article.print,
	%Article* (%Article*,%String*,%String*,%String*) * @Article.initArticle
}

@str.BookList = internal constant [9 x i8] c"BookList\00"
%BookList = type {
	%_BookList_vtable*
}

%_BookList_vtable = type {
	i32,
	i32,
	i8*,
	%BookList* () *,
	%Object* (%BookList*) *,
	%String* (%BookList*) *,
	%BookList* (%BookList*) *,
	%BookList* (%BookList*,%String*) *,
	%BookList* (%BookList*,i32) *,
	%String* (%BookList*) *,
	i32 (%BookList*) *,
	i1 (%BookList*) *,
	%Cons* (%BookList*,%Book*) *,
	%Book* (%BookList*) *,
	%BookList* (%BookList*) *,
	%String* (%BookList*,%String*) *
}

@_BookList_vtable_prototype = constant %_BookList_vtable {
	i32 7,
	i32 ptrtoint (%BookList* getelementptr (%BookList, %BookList* null, i32 1) to i32),
	i8* getelementptr ([9 x i8], [9 x i8]* @str.BookList, i32 0, i32 0),
	%BookList* () * @BookList_new,
	%Object* (%BookList*) * bitcast (%Object* (%Object*) * @Object_abort to %Object* (%BookList*) *),
	%String* (%BookList*) * bitcast (%String* (%Object*) * @Object_type_name to %String* (%BookList*) *),
	%BookList* (%BookList*) * bitcast (%Object* (%Object*) * @Object_copy to %BookList* (%BookList*) *),
	%BookList* (%BookList*,%String*) * bitcast (%IO* (%IO*,%String*) * @IO_out_string to %BookList* (%BookList*,%String*) *),
	%BookList* (%BookList*,i32) * bitcast (%IO* (%IO*,i32) * @IO_out_int to %BookList* (%BookList*,i32) *),
	%String* (%BookList*) * bitcast (%String* (%IO*) * @IO_in_string to %String* (%BookList*) *),
	i32 (%BookList*) * bitcast (i32 (%IO*) * @IO_in_int to i32 (%BookList*) *),
	i1 (%BookList*) * @BookList.isNil,
	%Cons* (%BookList*,%Book*) * @BookList.cons,
	%Book* (%BookList*) * @BookList.car,
	%BookList* (%BookList*) * @BookList.cdr,
	%String* (%BookList*,%String*) * @BookList.print_list
}

@str.Cons = internal constant [5 x i8] c"Cons\00"
%Cons = type {
	%_Cons_vtable*,
	%Book*,
	%BookList*
}

%_Cons_vtable = type {
	i32,
	i32,
	i8*,
	%Cons* () *,
	%Object* (%Cons*) *,
	%String* (%Cons*) *,
	%Cons* (%Cons*) *,
	%Cons* (%Cons*,%String*) *,
	%Cons* (%Cons*,i32) *,
	%String* (%Cons*) *,
	i32 (%Cons*) *,
	i1 (%Cons*) *,
	%Cons* (%Cons*,%Book*) *,
	%Book* (%Cons*) *,
	%BookList* (%Cons*) *,
	%String* (%Cons*,%String*) *,
	%Cons* (%Cons*,%Book*,%BookList*) *
}

@_Cons_vtable_prototype = constant %_Cons_vtable {
	i32 8,
	i32 ptrtoint (%Cons* getelementptr (%Cons, %Cons* null, i32 1) to i32),
	i8* getelementptr ([5 x i8], [5 x i8]* @str.Cons, i32 0, i32 0),
	%Cons* () * @Cons_new,
	%Object* (%Cons*) * bitcast (%Object* (%Object*) * @Object_abort to %Object* (%Cons*) *),
	%String* (%Cons*) * bitcast (%String* (%Object*) * @Object_type_name to %String* (%Cons*) *),
	%Cons* (%Cons*) * bitcast (%Object* (%Object*) * @Object_copy to %Cons* (%Cons*) *),
	%Cons* (%Cons*,%String*) * bitcast (%IO* (%IO*,%String*) * @IO_out_string to %Cons* (%Cons*,%String*) *),
	%Cons* (%Cons*,i32) * bitcast (%IO* (%IO*,i32) * @IO_out_int to %Cons* (%Cons*,i32) *),
	%String* (%Cons*) * bitcast (%String* (%IO*) * @IO_in_string to %String* (%Cons*) *),
	i32 (%Cons*) * bitcast (i32 (%IO*) * @IO_in_int to i32 (%Cons*) *),
	i1 (%Cons*) * @Cons.isNil,
	%Cons* (%Cons*,%Book*) * bitcast (%Cons* (%BookList*,%Book*) * @BookList.cons to %Cons* (%Cons*,%Book*) *),
	%Book* (%Cons*) * @Cons.car,
	%BookList* (%Cons*) * @Cons.cdr,
	%String* (%Cons*,%String*) * @Cons.print_list,
	%Cons* (%Cons*,%Book*,%BookList*) * @Cons.init
}

@str.Nil = internal constant [4 x i8] c"Nil\00"
%Nil = type {
	%_Nil_vtable*
}

%_Nil_vtable = type {
	i32,
	i32,
	i8*,
	%Nil* () *,
	%Object* (%Nil*) *,
	%String* (%Nil*) *,
	%Nil* (%Nil*) *,
	%Nil* (%Nil*,%String*) *,
	%Nil* (%Nil*,i32) *,
	%String* (%Nil*) *,
	i32 (%Nil*) *,
	i1 (%Nil*) *,
	%Cons* (%Nil*,%Book*) *,
	%Book* (%Nil*) *,
	%BookList* (%Nil*) *,
	%String* (%Nil*,%String*) *
}

@_Nil_vtable_prototype = constant %_Nil_vtable {
	i32 9,
	i32 ptrtoint (%Nil* getelementptr (%Nil, %Nil* null, i32 1) to i32),
	i8* getelementptr ([4 x i8], [4 x i8]* @str.Nil, i32 0, i32 0),
	%Nil* () * @Nil_new,
	%Object* (%Nil*) * bitcast (%Object* (%Object*) * @Object_abort to %Object* (%Nil*) *),
	%String* (%Nil*) * bitcast (%String* (%Object*) * @Object_type_name to %String* (%Nil*) *),
	%Nil* (%Nil*) * bitcast (%Object* (%Object*) * @Object_copy to %Nil* (%Nil*) *),
	%Nil* (%Nil*,%String*) * bitcast (%IO* (%IO*,%String*) * @IO_out_string to %Nil* (%Nil*,%String*) *),
	%Nil* (%Nil*,i32) * bitcast (%IO* (%IO*,i32) * @IO_out_int to %Nil* (%Nil*,i32) *),
	%String* (%Nil*) * bitcast (%String* (%IO*) * @IO_in_string to %String* (%Nil*) *),
	i32 (%Nil*) * bitcast (i32 (%IO*) * @IO_in_int to i32 (%Nil*) *),
	i1 (%Nil*) * @Nil.isNil,
	%Cons* (%Nil*,%Book*) * bitcast (%Cons* (%BookList*,%Book*) * @BookList.cons to %Cons* (%Nil*,%Book*) *),
	%Book* (%Nil*) * bitcast (%Book* (%BookList*) * @BookList.car to %Book* (%Nil*) *),
	%BookList* (%Nil*) * bitcast (%BookList* (%BookList*) * @BookList.cdr to %BookList* (%Nil*) *),
	%String* (%Nil*,%String*) * @Nil.print_list
}

@str.Main = internal constant [5 x i8] c"Main\00"
%Main = type {
	%_Main_vtable*,
	%BookList*
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
	i32 10,
	i32 ptrtoint (%Main* getelementptr (%Main, %Main* null, i32 1) to i32),
	i8* getelementptr ([5 x i8], [5 x i8]* @str.Main, i32 0, i32 0),
	%Main* () * @Main_new,
	%Object* (%Main*) * bitcast (%Object* (%Object*) * @Object_abort to %Object* (%Main*) *),
	%String* (%Main*) * bitcast (%String* (%Object*) * @Object_type_name to %String* (%Main*) *),
	%Main* (%Main*) * bitcast (%Object* (%Object*) * @Object_copy to %Main* (%Main*) *),
	%String* (%Main*) * @Main.main
}

@str.13 = internal constant [14 x i8] c"<basic class>\00"
@String.13 = constant %String {
	%_String_vtable* @_String_vtable_prototype,
	i8* getelementptr ([14 x i8], [14 x i8]* @str.13, i32 0, i32 0)
}

@str.12 = internal constant [12 x i8] c"PC Magazine\00"
@String.12 = constant %String {
	%_String_vtable* @_String_vtable_prototype,
	i8* getelementptr ([12 x i8], [12 x i8]* @str.12, i32 0, i32 0)
}

@str.11 = internal constant [8 x i8] c"Ulanoff\00"
@String.11 = constant %String {
	%_String_vtable* @_String_vtable_prototype,
	i8* getelementptr ([8 x i8], [8 x i8]* @str.11, i32 0, i32 0)
}

@str.10 = internal constant [20 x i8] c"The Top 100 CD_ROMs\00"
@String.10 = constant %String {
	%_String_vtable* @_String_vtable_prototype,
	i8* getelementptr ([20 x i8], [20 x i8]* @str.10, i32 0, i32 0)
}

@str.9 = internal constant [23 x i8] c"Aho, Sethi, and Ullman\00"
@String.9 = constant %String {
	%_String_vtable* @_String_vtable_prototype,
	i8* getelementptr ([23 x i8], [23 x i8]* @str.9, i32 0, i32 0)
}

@str.8 = internal constant [45 x i8] c"Compilers, Principles, Techniques, and Tools\00"
@String.8 = constant %String {
	%_String_vtable* @_String_vtable_prototype,
	i8* getelementptr ([45 x i8], [45 x i8]* @str.8, i32 0, i32 0)
}

@str.7 = internal constant [30 x i8] c"- dynamic type was Article -\0A\00"
@String.7 = constant %String {
	%_String_vtable* @_String_vtable_prototype,
	i8* getelementptr ([30 x i8], [30 x i8]* @str.7, i32 0, i32 0)
}

@str.6 = internal constant [27 x i8] c"- dynamic type was Book -\0A\00"
@String.6 = constant %String {
	%_String_vtable* @_String_vtable_prototype,
	i8* getelementptr ([27 x i8], [27 x i8]* @str.6, i32 0, i32 0)
}

@str.5 = internal constant [1 x i8] c"\00"
@String.5 = constant %String {
	%_String_vtable* @_String_vtable_prototype,
	i8* getelementptr ([1 x i8], [1 x i8]* @str.5, i32 0, i32 0)
}

@str.4 = internal constant [14 x i8] c"periodical:  \00"
@String.4 = constant %String {
	%_String_vtable* @_String_vtable_prototype,
	i8* getelementptr ([14 x i8], [14 x i8]* @str.4, i32 0, i32 0)
}

@str.3 = internal constant [13 x i8] c"author:     \00"
@String.3 = constant %String {
	%_String_vtable* @_String_vtable_prototype,
	i8* getelementptr ([13 x i8], [13 x i8]* @str.3, i32 0, i32 0)
}

@str.2 = internal constant [2 x i8] c"\0A\00"
@String.2 = constant %String {
	%_String_vtable* @_String_vtable_prototype,
	i8* getelementptr ([2 x i8], [2 x i8]* @str.2, i32 0, i32 0)
}

@str.1 = internal constant [13 x i8] c"title:      \00"
@String.1 = constant %String {
	%_String_vtable* @_String_vtable_prototype,
	i8* getelementptr ([13 x i8], [13 x i8]* @str.1, i32 0, i32 0)
}

@str.0 = internal constant [13 x i8] c"book_list.cl\00"
@String.0 = constant %String {
	%_String_vtable* @_String_vtable_prototype,
	i8* getelementptr ([13 x i8], [13 x i8]* @str.0, i32 0, i32 0)
}

define i32 @main() {
entry:
	%main.obj = call %Main*() @Main_new( )
	%main.retval = call %String*(%Main*) @Main.main( %Main* %main.obj )
	ret i32 0
}

define %Book* @Book.initBook(%Book* %self, %String* %title_p, %String* %author_p) {

entry:
	%tmp.0 = alloca %Book*
	%tmp.1 = alloca %String*
	%tmp.2 = alloca %String*
	store %Book* %self, %Book** %tmp.0
	store %String* %title_p, %String** %tmp.1
	store %String* %author_p, %String** %tmp.2
	%tmp.3 = load %String*, %String** %tmp.1
	%tmp.4 = load %Book*, %Book** %tmp.0
	%tmp.5 = getelementptr %Book, %Book* %tmp.4, i32 0, i32 1
	store %String* %tmp.3, %String** %tmp.5
	%tmp.6 = load %String*, %String** %tmp.2
	%tmp.7 = load %Book*, %Book** %tmp.0
	%tmp.8 = getelementptr %Book, %Book* %tmp.7, i32 0, i32 2
	store %String* %tmp.6, %String** %tmp.8
	%tmp.9 = load %Book*, %Book** %tmp.0
	ret %Book* %tmp.9

abort:
	call void @abort(  )
	unreachable
}

define %Object* @Book.getSelf(%Book* %self) {

entry:
	%tmp.10 = alloca %Book*
	store %Book* %self, %Book** %tmp.10
	%tmp.11 = load %Book*, %Book** %tmp.10
	%tmp.12 = bitcast %Book* %tmp.11 to %Object*
	ret %Object* %tmp.12

abort:
	call void @abort(  )
	unreachable
}

define %String* @Book.print(%Book* %self, %String* %s) {

entry:
	%tmp.13 = alloca %Book*
	%tmp.14 = alloca %String*
	store %Book* %self, %Book** %tmp.13
	store %String* %s, %String** %tmp.14
	%tmp.15 = load %Book*, %Book** %tmp.13
	%tmp.16 = getelementptr %Book, %Book* %tmp.15, i32 0, i32 1
	%tmp.17 = load %String*, %String** %tmp.16
	%tmp.18 = load %String*, %String** %tmp.14
	%tmp.19 = icmp eq %String* %tmp.18, null
	br i1 %tmp.19, label %abort, label %ok.2

ok.2:
	%tmp.20 = getelementptr %String, %String* %tmp.18, i32 0, i32 0
	%tmp.21 = load %_String_vtable*, %_String_vtable** %tmp.20
	%tmp.22 = getelementptr %_String_vtable, %_String_vtable* %tmp.21, i32 0, i32 8
	%tmp.23 = load %String* (%String*,%String*) *, %String* (%String*,%String*) ** %tmp.22
	%tmp.24 = call %String*(%String*, %String* ) %tmp.23( %String* %tmp.18, %String* @String.1 )
	%tmp.25 = icmp eq %String* %tmp.24, null
	br i1 %tmp.25, label %abort, label %ok.1

ok.1:
	%tmp.26 = getelementptr %String, %String* %tmp.24, i32 0, i32 0
	%tmp.27 = load %_String_vtable*, %_String_vtable** %tmp.26
	%tmp.28 = getelementptr %_String_vtable, %_String_vtable* %tmp.27, i32 0, i32 8
	%tmp.29 = load %String* (%String*,%String*) *, %String* (%String*,%String*) ** %tmp.28
	%tmp.30 = call %String*(%String*, %String* ) %tmp.29( %String* %tmp.24, %String* %tmp.17 )
	%tmp.31 = icmp eq %String* %tmp.30, null
	br i1 %tmp.31, label %abort, label %ok.0

ok.0:
	%tmp.32 = getelementptr %String, %String* %tmp.30, i32 0, i32 0
	%tmp.33 = load %_String_vtable*, %_String_vtable** %tmp.32
	%tmp.34 = getelementptr %_String_vtable, %_String_vtable* %tmp.33, i32 0, i32 8
	%tmp.35 = load %String* (%String*,%String*) *, %String* (%String*,%String*) ** %tmp.34
	%tmp.36 = call %String*(%String*, %String* ) %tmp.35( %String* %tmp.30, %String* @String.2 )
	store %String* %tmp.36, %String** %tmp.14
	%tmp.37 = load %Book*, %Book** %tmp.13
	%tmp.38 = getelementptr %Book, %Book* %tmp.37, i32 0, i32 2
	%tmp.39 = load %String*, %String** %tmp.38
	%tmp.40 = load %String*, %String** %tmp.14
	%tmp.41 = icmp eq %String* %tmp.40, null
	br i1 %tmp.41, label %abort, label %ok.5

ok.5:
	%tmp.42 = getelementptr %String, %String* %tmp.40, i32 0, i32 0
	%tmp.43 = load %_String_vtable*, %_String_vtable** %tmp.42
	%tmp.44 = getelementptr %_String_vtable, %_String_vtable* %tmp.43, i32 0, i32 8
	%tmp.45 = load %String* (%String*,%String*) *, %String* (%String*,%String*) ** %tmp.44
	%tmp.46 = call %String*(%String*, %String* ) %tmp.45( %String* %tmp.40, %String* @String.3 )
	%tmp.47 = icmp eq %String* %tmp.46, null
	br i1 %tmp.47, label %abort, label %ok.4

ok.4:
	%tmp.48 = getelementptr %String, %String* %tmp.46, i32 0, i32 0
	%tmp.49 = load %_String_vtable*, %_String_vtable** %tmp.48
	%tmp.50 = getelementptr %_String_vtable, %_String_vtable* %tmp.49, i32 0, i32 8
	%tmp.51 = load %String* (%String*,%String*) *, %String* (%String*,%String*) ** %tmp.50
	%tmp.52 = call %String*(%String*, %String* ) %tmp.51( %String* %tmp.46, %String* %tmp.39 )
	%tmp.53 = icmp eq %String* %tmp.52, null
	br i1 %tmp.53, label %abort, label %ok.3

ok.3:
	%tmp.54 = getelementptr %String, %String* %tmp.52, i32 0, i32 0
	%tmp.55 = load %_String_vtable*, %_String_vtable** %tmp.54
	%tmp.56 = getelementptr %_String_vtable, %_String_vtable* %tmp.55, i32 0, i32 8
	%tmp.57 = load %String* (%String*,%String*) *, %String* (%String*,%String*) ** %tmp.56
	%tmp.58 = call %String*(%String*, %String* ) %tmp.57( %String* %tmp.52, %String* @String.2 )
	ret %String* %tmp.58

abort:
	call void @abort(  )
	unreachable
}

define %Book* @Book_new() {

entry:
	%tmp.59 = alloca %Book*
	%tmp.60 = getelementptr %_Book_vtable, %_Book_vtable* @_Book_vtable_prototype, i32 0, i32 1
	%tmp.61 = load i32, i32* %tmp.60
	%tmp.62 = call i8*(i32 ) @malloc( i32 %tmp.61 )
	%tmp.63 = bitcast i8* %tmp.62 to %Book*
	%malloc.null = icmp eq %Book* %tmp.63, null
	br i1 %malloc.null, label %abort, label %okay

okay:
	%tmp.64 = getelementptr %Book, %Book* %tmp.63, i32 0, i32 0
	store %_Book_vtable* @_Book_vtable_prototype, %_Book_vtable** %tmp.64
	store %Book* %tmp.63, %Book** %tmp.59
	%tmp.65 = getelementptr %Book, %Book* %tmp.63, i32 0, i32 1
	%tmp.66 = call %String* @String_new(  )
	store %String* %tmp.66, %String** %tmp.65
	%tmp.67 = getelementptr %Book, %Book* %tmp.63, i32 0, i32 2
	%tmp.68 = call %String* @String_new(  )
	store %String* %tmp.68, %String** %tmp.67
	%tmp.69 = getelementptr %Book, %Book* %tmp.63, i32 0, i32 1
	%tmp.70 = call %String* @String_new(  )
	store %String* %tmp.70, %String** %tmp.69
	%tmp.71 = getelementptr %Book, %Book* %tmp.63, i32 0, i32 2
	%tmp.72 = call %String* @String_new(  )
	store %String* %tmp.72, %String** %tmp.71
	ret %Book* %tmp.63

abort:
	call void @abort(  )
	unreachable
}

define %Article* @Article.initArticle(%Article* %self, %String* %title_p, %String* %author_p, %String* %per_title_p) {

entry:
	%tmp.0 = alloca %Article*
	%tmp.1 = alloca %String*
	%tmp.2 = alloca %String*
	%tmp.3 = alloca %String*
	store %Article* %self, %Article** %tmp.0
	store %String* %title_p, %String** %tmp.1
	store %String* %author_p, %String** %tmp.2
	store %String* %per_title_p, %String** %tmp.3
	%tmp.4 = load %String*, %String** %tmp.1
	%tmp.5 = load %String*, %String** %tmp.2
	%tmp.6 = load %Article*, %Article** %tmp.0
	%tmp.7 = icmp eq %Article* %tmp.6, null
	br i1 %tmp.7, label %abort, label %ok.0

ok.0:
	%tmp.8 = getelementptr %Article, %Article* %tmp.6, i32 0, i32 0
	%tmp.9 = load %_Article_vtable*, %_Article_vtable** %tmp.8
	%tmp.10 = getelementptr %_Article_vtable, %_Article_vtable* %tmp.9, i32 0, i32 11
	%tmp.11 = load %Book* (%Article*,%String*,%String*) *, %Book* (%Article*,%String*,%String*) ** %tmp.10
	%tmp.12 = call %Book*(%Article*, %String*, %String* ) %tmp.11( %Article* %tmp.6, %String* %tmp.4, %String* %tmp.5 )
	%tmp.13 = load %String*, %String** %tmp.3
	%tmp.14 = load %Article*, %Article** %tmp.0
	%tmp.15 = getelementptr %Article, %Article* %tmp.14, i32 0, i32 3
	store %String* %tmp.13, %String** %tmp.15
	%tmp.16 = load %Article*, %Article** %tmp.0
	ret %Article* %tmp.16

abort:
	call void @abort(  )
	unreachable
}

define %Object* @Article.getSelf(%Article* %self) {

entry:
	%tmp.17 = alloca %Article*
	store %Article* %self, %Article** %tmp.17
	%tmp.18 = load %Article*, %Article** %tmp.17
	%tmp.19 = bitcast %Article* %tmp.18 to %Object*
	ret %Object* %tmp.19

abort:
	call void @abort(  )
	unreachable
}

define %String* @Article.print(%Article* %self, %String* %s) {

entry:
	%tmp.20 = alloca %Article*
	%tmp.21 = alloca %String*
	store %Article* %self, %Article** %tmp.20
	store %String* %s, %String** %tmp.21
	%tmp.22 = load %String*, %String** %tmp.21
	%tmp.23 = load %Article*, %Article** %tmp.20
	%tmp.24 = icmp eq %Article* %tmp.23, null
	br i1 %tmp.24, label %abort, label %ok.1

ok.1:
	%tmp.25 = getelementptr %_Book_vtable, %_Book_vtable* @_Book_vtable_prototype, i32 0, i32 13
	%tmp.26 = load %String* (%Book*,%String*) *, %String* (%Book*,%String*) ** %tmp.25
	%tmp.27 = bitcast %Article* %tmp.23 to %Book*
	%tmp.28 = call %String*(%Book*, %String* ) %tmp.26( %Book* %tmp.27, %String* %tmp.22 )
	store %String* %tmp.28, %String** %tmp.21
	%tmp.29 = load %Article*, %Article** %tmp.20
	%tmp.30 = getelementptr %Article, %Article* %tmp.29, i32 0, i32 3
	%tmp.31 = load %String*, %String** %tmp.30
	%tmp.32 = load %String*, %String** %tmp.21
	%tmp.33 = icmp eq %String* %tmp.32, null
	br i1 %tmp.33, label %abort, label %ok.4

ok.4:
	%tmp.34 = getelementptr %String, %String* %tmp.32, i32 0, i32 0
	%tmp.35 = load %_String_vtable*, %_String_vtable** %tmp.34
	%tmp.36 = getelementptr %_String_vtable, %_String_vtable* %tmp.35, i32 0, i32 8
	%tmp.37 = load %String* (%String*,%String*) *, %String* (%String*,%String*) ** %tmp.36
	%tmp.38 = call %String*(%String*, %String* ) %tmp.37( %String* %tmp.32, %String* @String.4 )
	%tmp.39 = icmp eq %String* %tmp.38, null
	br i1 %tmp.39, label %abort, label %ok.3

ok.3:
	%tmp.40 = getelementptr %String, %String* %tmp.38, i32 0, i32 0
	%tmp.41 = load %_String_vtable*, %_String_vtable** %tmp.40
	%tmp.42 = getelementptr %_String_vtable, %_String_vtable* %tmp.41, i32 0, i32 8
	%tmp.43 = load %String* (%String*,%String*) *, %String* (%String*,%String*) ** %tmp.42
	%tmp.44 = call %String*(%String*, %String* ) %tmp.43( %String* %tmp.38, %String* %tmp.31 )
	%tmp.45 = icmp eq %String* %tmp.44, null
	br i1 %tmp.45, label %abort, label %ok.2

ok.2:
	%tmp.46 = getelementptr %String, %String* %tmp.44, i32 0, i32 0
	%tmp.47 = load %_String_vtable*, %_String_vtable** %tmp.46
	%tmp.48 = getelementptr %_String_vtable, %_String_vtable* %tmp.47, i32 0, i32 8
	%tmp.49 = load %String* (%String*,%String*) *, %String* (%String*,%String*) ** %tmp.48
	%tmp.50 = call %String*(%String*, %String* ) %tmp.49( %String* %tmp.44, %String* @String.2 )
	ret %String* %tmp.50

abort:
	call void @abort(  )
	unreachable
}

define %Article* @Article_new() {

entry:
	%tmp.51 = alloca %Article*
	%tmp.52 = getelementptr %_Article_vtable, %_Article_vtable* @_Article_vtable_prototype, i32 0, i32 1
	%tmp.53 = load i32, i32* %tmp.52
	%tmp.54 = call i8*(i32 ) @malloc( i32 %tmp.53 )
	%tmp.55 = bitcast i8* %tmp.54 to %Article*
	%malloc.null = icmp eq %Article* %tmp.55, null
	br i1 %malloc.null, label %abort, label %okay

okay:
	%tmp.56 = getelementptr %Article, %Article* %tmp.55, i32 0, i32 0
	store %_Article_vtable* @_Article_vtable_prototype, %_Article_vtable** %tmp.56
	store %Article* %tmp.55, %Article** %tmp.51
	%tmp.57 = getelementptr %Article, %Article* %tmp.55, i32 0, i32 1
	%tmp.58 = call %String* @String_new(  )
	store %String* %tmp.58, %String** %tmp.57
	%tmp.59 = getelementptr %Article, %Article* %tmp.55, i32 0, i32 2
	%tmp.60 = call %String* @String_new(  )
	store %String* %tmp.60, %String** %tmp.59
	%tmp.61 = getelementptr %Article, %Article* %tmp.55, i32 0, i32 3
	%tmp.62 = call %String* @String_new(  )
	store %String* %tmp.62, %String** %tmp.61
	%tmp.63 = getelementptr %Article, %Article* %tmp.55, i32 0, i32 1
	%tmp.64 = call %String* @String_new(  )
	store %String* %tmp.64, %String** %tmp.63
	%tmp.65 = getelementptr %Article, %Article* %tmp.55, i32 0, i32 2
	%tmp.66 = call %String* @String_new(  )
	store %String* %tmp.66, %String** %tmp.65
	%tmp.67 = getelementptr %Article, %Article* %tmp.55, i32 0, i32 3
	%tmp.68 = call %String* @String_new(  )
	store %String* %tmp.68, %String** %tmp.67
	ret %Article* %tmp.55

abort:
	call void @abort(  )
	unreachable
}

define i1 @BookList.isNil(%BookList* %self) {

entry:
	%tmp.0 = alloca %BookList*
	store %BookList* %self, %BookList** %tmp.0
	%tmp.1 = load %BookList*, %BookList** %tmp.0
	%tmp.2 = icmp eq %BookList* %tmp.1, null
	br i1 %tmp.2, label %abort, label %ok.0

ok.0:
	%tmp.3 = getelementptr %BookList, %BookList* %tmp.1, i32 0, i32 0
	%tmp.4 = load %_BookList_vtable*, %_BookList_vtable** %tmp.3
	%tmp.5 = getelementptr %_BookList_vtable, %_BookList_vtable* %tmp.4, i32 0, i32 4
	%tmp.6 = load %Object* (%BookList*) *, %Object* (%BookList*) ** %tmp.5
	%tmp.7 = call %Object*(%BookList* ) %tmp.6( %BookList* %tmp.1 )
	ret i1 true

abort:
	call void @abort(  )
	unreachable
}

define %Cons* @BookList.cons(%BookList* %self, %Book* %hd) {

entry:
	%tmp.10 = alloca %Cons*
	%tmp.8 = alloca %BookList*
	%tmp.9 = alloca %Book*
	store %BookList* %self, %BookList** %tmp.8
	store %Book* %hd, %Book** %tmp.9
	%tmp.11 = call %Cons* @Cons_new(  )
	store %Cons* %tmp.11, %Cons** %tmp.10
	%tmp.12 = load %Book*, %Book** %tmp.9
	%tmp.13 = load %BookList*, %BookList** %tmp.8
	%tmp.14 = load %Cons*, %Cons** %tmp.10
	%tmp.15 = icmp eq %Cons* %tmp.14, null
	br i1 %tmp.15, label %abort, label %ok.1

ok.1:
	%tmp.16 = getelementptr %Cons, %Cons* %tmp.14, i32 0, i32 0
	%tmp.17 = load %_Cons_vtable*, %_Cons_vtable** %tmp.16
	%tmp.18 = getelementptr %_Cons_vtable, %_Cons_vtable* %tmp.17, i32 0, i32 16
	%tmp.19 = load %Cons* (%Cons*,%Book*,%BookList*) *, %Cons* (%Cons*,%Book*,%BookList*) ** %tmp.18
	%tmp.20 = call %Cons*(%Cons*, %Book*, %BookList* ) %tmp.19( %Cons* %tmp.14, %Book* %tmp.12, %BookList* %tmp.13 )
	ret %Cons* %tmp.20

abort:
	call void @abort(  )
	unreachable
}

define %Book* @BookList.car(%BookList* %self) {

entry:
	%tmp.21 = alloca %BookList*
	store %BookList* %self, %BookList** %tmp.21
	%tmp.22 = load %BookList*, %BookList** %tmp.21
	%tmp.23 = icmp eq %BookList* %tmp.22, null
	br i1 %tmp.23, label %abort, label %ok.2

ok.2:
	%tmp.24 = getelementptr %BookList, %BookList* %tmp.22, i32 0, i32 0
	%tmp.25 = load %_BookList_vtable*, %_BookList_vtable** %tmp.24
	%tmp.26 = getelementptr %_BookList_vtable, %_BookList_vtable* %tmp.25, i32 0, i32 4
	%tmp.27 = load %Object* (%BookList*) *, %Object* (%BookList*) ** %tmp.26
	%tmp.28 = call %Object*(%BookList* ) %tmp.27( %BookList* %tmp.22 )
	%tmp.29 = call %Book* @Book_new(  )
	ret %Book* %tmp.29

abort:
	call void @abort(  )
	unreachable
}

define %BookList* @BookList.cdr(%BookList* %self) {

entry:
	%tmp.30 = alloca %BookList*
	store %BookList* %self, %BookList** %tmp.30
	%tmp.31 = load %BookList*, %BookList** %tmp.30
	%tmp.32 = icmp eq %BookList* %tmp.31, null
	br i1 %tmp.32, label %abort, label %ok.3

ok.3:
	%tmp.33 = getelementptr %BookList, %BookList* %tmp.31, i32 0, i32 0
	%tmp.34 = load %_BookList_vtable*, %_BookList_vtable** %tmp.33
	%tmp.35 = getelementptr %_BookList_vtable, %_BookList_vtable* %tmp.34, i32 0, i32 4
	%tmp.36 = load %Object* (%BookList*) *, %Object* (%BookList*) ** %tmp.35
	%tmp.37 = call %Object*(%BookList* ) %tmp.36( %BookList* %tmp.31 )
	%tmp.38 = call %BookList* @BookList_new(  )
	ret %BookList* %tmp.38

abort:
	call void @abort(  )
	unreachable
}

define %String* @BookList.print_list(%BookList* %self, %String* %s) {

entry:
	%tmp.39 = alloca %BookList*
	%tmp.40 = alloca %String*
	store %BookList* %self, %BookList** %tmp.39
	store %String* %s, %String** %tmp.40
	ret %String* @String.5

abort:
	call void @abort(  )
	unreachable
}

define %BookList* @BookList_new() {

entry:
	%tmp.41 = alloca %BookList*
	%tmp.42 = getelementptr %_BookList_vtable, %_BookList_vtable* @_BookList_vtable_prototype, i32 0, i32 1
	%tmp.43 = load i32, i32* %tmp.42
	%tmp.44 = call i8*(i32 ) @malloc( i32 %tmp.43 )
	%tmp.45 = bitcast i8* %tmp.44 to %BookList*
	%malloc.null = icmp eq %BookList* %tmp.45, null
	br i1 %malloc.null, label %abort, label %okay

okay:
	%tmp.46 = getelementptr %BookList, %BookList* %tmp.45, i32 0, i32 0
	store %_BookList_vtable* @_BookList_vtable_prototype, %_BookList_vtable** %tmp.46
	store %BookList* %tmp.45, %BookList** %tmp.41
	ret %BookList* %tmp.45

abort:
	call void @abort(  )
	unreachable
}

define i1 @Cons.isNil(%Cons* %self) {

entry:
	%tmp.0 = alloca %Cons*
	store %Cons* %self, %Cons** %tmp.0
	ret i1 false

abort:
	call void @abort(  )
	unreachable
}

define %Cons* @Cons.init(%Cons* %self, %Book* %hd, %BookList* %tl) {

entry:
	%tmp.1 = alloca %Cons*
	%tmp.2 = alloca %Book*
	%tmp.3 = alloca %BookList*
	store %Cons* %self, %Cons** %tmp.1
	store %Book* %hd, %Book** %tmp.2
	store %BookList* %tl, %BookList** %tmp.3
	%tmp.4 = load %Book*, %Book** %tmp.2
	%tmp.5 = load %Cons*, %Cons** %tmp.1
	%tmp.6 = getelementptr %Cons, %Cons* %tmp.5, i32 0, i32 1
	store %Book* %tmp.4, %Book** %tmp.6
	%tmp.7 = load %BookList*, %BookList** %tmp.3
	%tmp.8 = load %Cons*, %Cons** %tmp.1
	%tmp.9 = getelementptr %Cons, %Cons* %tmp.8, i32 0, i32 2
	store %BookList* %tmp.7, %BookList** %tmp.9
	%tmp.10 = load %Cons*, %Cons** %tmp.1
	ret %Cons* %tmp.10

abort:
	call void @abort(  )
	unreachable
}

define %Book* @Cons.car(%Cons* %self) {

entry:
	%tmp.11 = alloca %Cons*
	store %Cons* %self, %Cons** %tmp.11
	%tmp.12 = load %Cons*, %Cons** %tmp.11
	%tmp.13 = getelementptr %Cons, %Cons* %tmp.12, i32 0, i32 1
	%tmp.14 = load %Book*, %Book** %tmp.13
	ret %Book* %tmp.14

abort:
	call void @abort(  )
	unreachable
}

define %BookList* @Cons.cdr(%Cons* %self) {

entry:
	%tmp.15 = alloca %Cons*
	store %Cons* %self, %Cons** %tmp.15
	%tmp.16 = load %Cons*, %Cons** %tmp.15
	%tmp.17 = getelementptr %Cons, %Cons* %tmp.16, i32 0, i32 2
	%tmp.18 = load %BookList*, %BookList** %tmp.17
	ret %BookList* %tmp.18

abort:
	call void @abort(  )
	unreachable
}

define %String* @Cons.print_list(%Cons* %self, %String* %s) {

entry:
	%tmp.21 = alloca %String*
	%vtpm.0 = alloca %Book*
	%vtpm.1 = alloca %Article*
	%tmp.19 = alloca %Cons*
	%tmp.20 = alloca %String*
	store %Cons* %self, %Cons** %tmp.19
	store %String* %s, %String** %tmp.20
	%tmp.22 = load %String*, %String** %tmp.20
	%tmp.23 = load %Cons*, %Cons** %tmp.19
	%tmp.24 = getelementptr %Cons, %Cons* %tmp.23, i32 0, i32 1
	%tmp.25 = load %Book*, %Book** %tmp.24
	%tmp.26 = icmp eq %Book* %tmp.25, null
	br i1 %tmp.26, label %abort, label %ok.0

ok.0:
	%tmp.27 = getelementptr %Book, %Book* %tmp.25, i32 0, i32 0
	%tmp.28 = load %_Book_vtable*, %_Book_vtable** %tmp.27
	%tmp.29 = getelementptr %_Book_vtable, %_Book_vtable* %tmp.28, i32 0, i32 13
	%tmp.30 = load %String* (%Book*,%String*) *, %String* (%Book*,%String*) ** %tmp.29
	%tmp.31 = call %String*(%Book*, %String* ) %tmp.30( %Book* %tmp.25, %String* %tmp.22 )
	store %String* %tmp.31, %String** %tmp.20
	%tmp.32 = load %Cons*, %Cons** %tmp.19
	%tmp.33 = getelementptr %Cons, %Cons* %tmp.32, i32 0, i32 1
	%tmp.34 = load %Book*, %Book** %tmp.33
	%tmp.35 = icmp eq %Book* %tmp.34, null
	br i1 %tmp.35, label %abort, label %ok.1

ok.1:
	%tmp.36 = getelementptr %Book, %Book* %tmp.34, i32 0, i32 0
	%tmp.37 = load %_Book_vtable*, %_Book_vtable** %tmp.36
	%tmp.38 = getelementptr %_Book_vtable, %_Book_vtable* %tmp.37, i32 0, i32 12
	%tmp.39 = load %Object* (%Book*) *, %Object* (%Book*) ** %tmp.38
	%tmp.40 = call %Object*(%Book* ) %tmp.39( %Book* %tmp.34 )
	%tmp.41 = icmp eq %Object* %tmp.40, null
	br i1 %tmp.41, label %abort, label %ok.2

ok.2:
	%tmp.42 = getelementptr %Object, %Object* %tmp.40, i32 0, i32 0
	%tmp.43 = load %_Object_vtable*, %_Object_vtable** %tmp.42
	%tmp.44 = getelementptr %_Object_vtable, %_Object_vtable* %tmp.43, i32 0, i32 0
	%tmp.45 = load i32, i32* %tmp.44
	br label %case.hdr.0

case.hdr.0:
	br label %case.6.0

case.6.0:
	%vtpm.2 = icmp slt i32 %tmp.45, 6
	br i1 %vtpm.2, label %br_exit.6.0, label %src_gte_br.6.0

src_gte_br.6.0:
	%vtpm.3 = icmp sgt i32 %tmp.45, 6
	br i1 %vtpm.3, label %br_exit.6.0, label %src_lte_mc.6.0

src_lte_mc.6.0:
	%tmp.46 = bitcast %Object* %tmp.40 to %Article*
	store %Article* %tmp.46, %Article** %vtpm.1
	%tmp.47 = load %String*, %String** %tmp.20
	%tmp.48 = icmp eq %String* %tmp.47, null
	br i1 %tmp.48, label %abort, label %ok.3

ok.3:
	%tmp.49 = getelementptr %String, %String* %tmp.47, i32 0, i32 0
	%tmp.50 = load %_String_vtable*, %_String_vtable** %tmp.49
	%tmp.51 = getelementptr %_String_vtable, %_String_vtable* %tmp.50, i32 0, i32 8
	%tmp.52 = load %String* (%String*,%String*) *, %String* (%String*,%String*) ** %tmp.51
	%tmp.53 = call %String*(%String*, %String* ) %tmp.52( %String* %tmp.47, %String* @String.7 )
	store %String* %tmp.53, %String** %tmp.21
	br label %case.exit.0

br_exit.6.0:
	br label %case.5.0

case.5.0:
	%vtpm.4 = icmp slt i32 %tmp.45, 5
	br i1 %vtpm.4, label %br_exit.5.0, label %src_gte_br.5.0

src_gte_br.5.0:
	%vtpm.5 = icmp sgt i32 %tmp.45, 6
	br i1 %vtpm.5, label %br_exit.5.0, label %src_lte_mc.5.0

src_lte_mc.5.0:
	%tmp.54 = bitcast %Object* %tmp.40 to %Book*
	store %Book* %tmp.54, %Book** %vtpm.0
	%tmp.55 = load %String*, %String** %tmp.20
	%tmp.56 = icmp eq %String* %tmp.55, null
	br i1 %tmp.56, label %abort, label %ok.4

ok.4:
	%tmp.57 = getelementptr %String, %String* %tmp.55, i32 0, i32 0
	%tmp.58 = load %_String_vtable*, %_String_vtable** %tmp.57
	%tmp.59 = getelementptr %_String_vtable, %_String_vtable* %tmp.58, i32 0, i32 8
	%tmp.60 = load %String* (%String*,%String*) *, %String* (%String*,%String*) ** %tmp.59
	%tmp.61 = call %String*(%String*, %String* ) %tmp.60( %String* %tmp.55, %String* @String.6 )
	store %String* %tmp.61, %String** %tmp.21
	br label %case.exit.0

br_exit.5.0:
	br label %abort

case.exit.0:
	%tmp.62 = load %String*, %String** %tmp.21
	store %String* %tmp.62, %String** %tmp.20
	%tmp.63 = load %String*, %String** %tmp.20
	%tmp.64 = load %Cons*, %Cons** %tmp.19
	%tmp.65 = getelementptr %Cons, %Cons* %tmp.64, i32 0, i32 2
	%tmp.66 = load %BookList*, %BookList** %tmp.65
	%tmp.67 = icmp eq %BookList* %tmp.66, null
	br i1 %tmp.67, label %abort, label %ok.5

ok.5:
	%tmp.68 = getelementptr %BookList, %BookList* %tmp.66, i32 0, i32 0
	%tmp.69 = load %_BookList_vtable*, %_BookList_vtable** %tmp.68
	%tmp.70 = getelementptr %_BookList_vtable, %_BookList_vtable* %tmp.69, i32 0, i32 15
	%tmp.71 = load %String* (%BookList*,%String*) *, %String* (%BookList*,%String*) ** %tmp.70
	%tmp.72 = call %String*(%BookList*, %String* ) %tmp.71( %BookList* %tmp.66, %String* %tmp.63 )
	ret %String* %tmp.72

abort:
	call void @abort(  )
	unreachable
}

define %Cons* @Cons_new() {

entry:
	%tmp.73 = alloca %Cons*
	%tmp.74 = getelementptr %_Cons_vtable, %_Cons_vtable* @_Cons_vtable_prototype, i32 0, i32 1
	%tmp.75 = load i32, i32* %tmp.74
	%tmp.76 = call i8*(i32 ) @malloc( i32 %tmp.75 )
	%tmp.77 = bitcast i8* %tmp.76 to %Cons*
	%malloc.null = icmp eq %Cons* %tmp.77, null
	br i1 %malloc.null, label %abort, label %okay

okay:
	%tmp.78 = getelementptr %Cons, %Cons* %tmp.77, i32 0, i32 0
	store %_Cons_vtable* @_Cons_vtable_prototype, %_Cons_vtable** %tmp.78
	store %Cons* %tmp.77, %Cons** %tmp.73
	%tmp.79 = getelementptr %Cons, %Cons* %tmp.77, i32 0, i32 1
	store %Book* null, %Book** %tmp.79
	%tmp.80 = getelementptr %Cons, %Cons* %tmp.77, i32 0, i32 2
	store %BookList* null, %BookList** %tmp.80
	%tmp.81 = getelementptr %Cons, %Cons* %tmp.77, i32 0, i32 1
	store %Book* null, %Book** %tmp.81
	%tmp.82 = getelementptr %Cons, %Cons* %tmp.77, i32 0, i32 2
	store %BookList* null, %BookList** %tmp.82
	ret %Cons* %tmp.77

abort:
	call void @abort(  )
	unreachable
}

define i1 @Nil.isNil(%Nil* %self) {

entry:
	%tmp.0 = alloca %Nil*
	store %Nil* %self, %Nil** %tmp.0
	ret i1 true

abort:
	call void @abort(  )
	unreachable
}

define %String* @Nil.print_list(%Nil* %self, %String* %s) {

entry:
	%tmp.1 = alloca %Nil*
	%tmp.2 = alloca %String*
	store %Nil* %self, %Nil** %tmp.1
	store %String* %s, %String** %tmp.2
	%tmp.3 = load %String*, %String** %tmp.2
	ret %String* %tmp.3

abort:
	call void @abort(  )
	unreachable
}

define %Nil* @Nil_new() {

entry:
	%tmp.4 = alloca %Nil*
	%tmp.5 = getelementptr %_Nil_vtable, %_Nil_vtable* @_Nil_vtable_prototype, i32 0, i32 1
	%tmp.6 = load i32, i32* %tmp.5
	%tmp.7 = call i8*(i32 ) @malloc( i32 %tmp.6 )
	%tmp.8 = bitcast i8* %tmp.7 to %Nil*
	%malloc.null = icmp eq %Nil* %tmp.8, null
	br i1 %malloc.null, label %abort, label %okay

okay:
	%tmp.9 = getelementptr %Nil, %Nil* %tmp.8, i32 0, i32 0
	store %_Nil_vtable* @_Nil_vtable_prototype, %_Nil_vtable** %tmp.9
	store %Nil* %tmp.8, %Nil** %tmp.4
	ret %Nil* %tmp.8

abort:
	call void @abort(  )
	unreachable
}

define %String* @Main.main(%Main* %self) {

entry:
	%tmp.1 = alloca %Book*
	%tmp.2 = alloca %Article*
	%tmp.0 = alloca %Main*
	store %Main* %self, %Main** %tmp.0
	%tmp.3 = call %Book* @Book_new(  )
	%tmp.4 = icmp eq %Book* %tmp.3, null
	br i1 %tmp.4, label %abort, label %ok.0

ok.0:
	%tmp.5 = getelementptr %Book, %Book* %tmp.3, i32 0, i32 0
	%tmp.6 = load %_Book_vtable*, %_Book_vtable** %tmp.5
	%tmp.7 = getelementptr %_Book_vtable, %_Book_vtable* %tmp.6, i32 0, i32 11
	%tmp.8 = load %Book* (%Book*,%String*,%String*) *, %Book* (%Book*,%String*,%String*) ** %tmp.7
	%tmp.9 = call %Book*(%Book*, %String*, %String* ) %tmp.8( %Book* %tmp.3, %String* @String.8, %String* @String.9 )
	store %Book* %tmp.9, %Book** %tmp.1
	%tmp.10 = call %Article* @Article_new(  )
	%tmp.11 = icmp eq %Article* %tmp.10, null
	br i1 %tmp.11, label %abort, label %ok.1

ok.1:
	%tmp.12 = getelementptr %Article, %Article* %tmp.10, i32 0, i32 0
	%tmp.13 = load %_Article_vtable*, %_Article_vtable** %tmp.12
	%tmp.14 = getelementptr %_Article_vtable, %_Article_vtable* %tmp.13, i32 0, i32 14
	%tmp.15 = load %Article* (%Article*,%String*,%String*,%String*) *, %Article* (%Article*,%String*,%String*,%String*) ** %tmp.14
	%tmp.16 = call %Article*(%Article*, %String*, %String*, %String* ) %tmp.15( %Article* %tmp.10, %String* @String.10, %String* @String.11, %String* @String.12 )
	store %Article* %tmp.16, %Article** %tmp.2
	%tmp.17 = load %Article*, %Article** %tmp.2
	%tmp.18 = load %Book*, %Book** %tmp.1
	%tmp.19 = call %Nil* @Nil_new(  )
	%tmp.20 = icmp eq %Nil* %tmp.19, null
	br i1 %tmp.20, label %abort, label %ok.3

ok.3:
	%tmp.21 = getelementptr %Nil, %Nil* %tmp.19, i32 0, i32 0
	%tmp.22 = load %_Nil_vtable*, %_Nil_vtable** %tmp.21
	%tmp.23 = getelementptr %_Nil_vtable, %_Nil_vtable* %tmp.22, i32 0, i32 12
	%tmp.24 = load %Cons* (%Nil*,%Book*) *, %Cons* (%Nil*,%Book*) ** %tmp.23
	%tmp.25 = call %Cons*(%Nil*, %Book* ) %tmp.24( %Nil* %tmp.19, %Book* %tmp.18 )
	%tmp.26 = icmp eq %Cons* %tmp.25, null
	br i1 %tmp.26, label %abort, label %ok.2

ok.2:
	%tmp.27 = getelementptr %Cons, %Cons* %tmp.25, i32 0, i32 0
	%tmp.28 = load %_Cons_vtable*, %_Cons_vtable** %tmp.27
	%tmp.29 = getelementptr %_Cons_vtable, %_Cons_vtable* %tmp.28, i32 0, i32 12
	%tmp.30 = load %Cons* (%Cons*,%Book*) *, %Cons* (%Cons*,%Book*) ** %tmp.29
	%tmp.31 = bitcast %Article* %tmp.17 to %Book*
	%tmp.32 = call %Cons*(%Cons*, %Book* ) %tmp.30( %Cons* %tmp.25, %Book* %tmp.31 )
	%tmp.33 = load %Main*, %Main** %tmp.0
	%tmp.34 = getelementptr %Main, %Main* %tmp.33, i32 0, i32 1
	%tmp.35 = bitcast %Cons* %tmp.32 to %BookList*
	store %BookList* %tmp.35, %BookList** %tmp.34
	%tmp.36 = load %Main*, %Main** %tmp.0
	%tmp.37 = getelementptr %Main, %Main* %tmp.36, i32 0, i32 1
	%tmp.38 = load %BookList*, %BookList** %tmp.37
	%tmp.39 = icmp eq %BookList* %tmp.38, null
	br i1 %tmp.39, label %abort, label %ok.4

ok.4:
	%tmp.40 = getelementptr %BookList, %BookList* %tmp.38, i32 0, i32 0
	%tmp.41 = load %_BookList_vtable*, %_BookList_vtable** %tmp.40
	%tmp.42 = getelementptr %_BookList_vtable, %_BookList_vtable* %tmp.41, i32 0, i32 15
	%tmp.43 = load %String* (%BookList*,%String*) *, %String* (%BookList*,%String*) ** %tmp.42
	%tmp.44 = call %String*(%BookList*, %String* ) %tmp.43( %BookList* %tmp.38, %String* @String.5 )
	ret %String* %tmp.44

abort:
	call void @abort(  )
	unreachable
}

define %Main* @Main_new() {

entry:
	%tmp.45 = alloca %Main*
	%tmp.46 = getelementptr %_Main_vtable, %_Main_vtable* @_Main_vtable_prototype, i32 0, i32 1
	%tmp.47 = load i32, i32* %tmp.46
	%tmp.48 = call i8*(i32 ) @malloc( i32 %tmp.47 )
	%tmp.49 = bitcast i8* %tmp.48 to %Main*
	%malloc.null = icmp eq %Main* %tmp.49, null
	br i1 %malloc.null, label %abort, label %okay

okay:
	%tmp.50 = getelementptr %Main, %Main* %tmp.49, i32 0, i32 0
	store %_Main_vtable* @_Main_vtable_prototype, %_Main_vtable** %tmp.50
	store %Main* %tmp.49, %Main** %tmp.45
	%tmp.51 = getelementptr %Main, %Main* %tmp.49, i32 0, i32 1
	store %BookList* null, %BookList** %tmp.51
	%tmp.52 = getelementptr %Main, %Main* %tmp.49, i32 0, i32 1
	store %BookList* null, %BookList** %tmp.52
	ret %Main* %tmp.49

abort:
	call void @abort(  )
	unreachable
}

