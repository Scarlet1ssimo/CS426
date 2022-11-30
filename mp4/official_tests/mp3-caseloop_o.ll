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
	%_A_vtable*
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
	%B* (%B*) *
}

@_B_vtable_prototype = constant %_B_vtable {
	i32 7,
	i32 ptrtoint (%B* getelementptr (%B, %B* null, i32 1) to i32),
	i8* getelementptr ([2 x i8], [2 x i8]* @str.B, i32 0, i32 0),
	%B* () * @B_new,
	%Object* (%B*) * bitcast (%Object* (%Object*) * @Object_abort to %Object* (%B*) *),
	%String* (%B*) * bitcast (%String* (%Object*) * @Object_type_name to %String* (%B*) *),
	%B* (%B*) * bitcast (%Object* (%Object*) * @Object_copy to %B* (%B*) *)
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
	%C* (%C*) *
}

@_C_vtable_prototype = constant %_C_vtable {
	i32 8,
	i32 ptrtoint (%C* getelementptr (%C, %C* null, i32 1) to i32),
	i8* getelementptr ([2 x i8], [2 x i8]* @str.C, i32 0, i32 0),
	%C* () * @C_new,
	%Object* (%C*) * bitcast (%Object* (%Object*) * @Object_abort to %Object* (%C*) *),
	%String* (%C*) * bitcast (%String* (%Object*) * @Object_type_name to %String* (%C*) *),
	%C* (%C*) * bitcast (%Object* (%Object*) * @Object_copy to %C* (%C*) *)
}

@str.D = internal constant [2 x i8] c"D\00"
%D = type {
	%_D_vtable*
}

%_D_vtable = type {
	i32,
	i32,
	i8*,
	%D* () *,
	%Object* (%D*) *,
	%String* (%D*) *,
	%D* (%D*) *
}

@_D_vtable_prototype = constant %_D_vtable {
	i32 9,
	i32 ptrtoint (%D* getelementptr (%D, %D* null, i32 1) to i32),
	i8* getelementptr ([2 x i8], [2 x i8]* @str.D, i32 0, i32 0),
	%D* () * @D_new,
	%Object* (%D*) * bitcast (%Object* (%Object*) * @Object_abort to %Object* (%D*) *),
	%String* (%D*) * bitcast (%String* (%Object*) * @Object_type_name to %String* (%D*) *),
	%D* (%D*) * bitcast (%Object* (%Object*) * @Object_copy to %D* (%D*) *)
}

@str.E = internal constant [2 x i8] c"E\00"
%E = type {
	%_E_vtable*
}

%_E_vtable = type {
	i32,
	i32,
	i8*,
	%E* () *,
	%Object* (%E*) *,
	%String* (%E*) *,
	%E* (%E*) *
}

@_E_vtable_prototype = constant %_E_vtable {
	i32 10,
	i32 ptrtoint (%E* getelementptr (%E, %E* null, i32 1) to i32),
	i8* getelementptr ([2 x i8], [2 x i8]* @str.E, i32 0, i32 0),
	%E* () * @E_new,
	%Object* (%E*) * bitcast (%Object* (%Object*) * @Object_abort to %Object* (%E*) *),
	%String* (%E*) * bitcast (%String* (%Object*) * @Object_type_name to %String* (%E*) *),
	%E* (%E*) * bitcast (%Object* (%Object*) * @Object_copy to %E* (%E*) *)
}

@str.F = internal constant [2 x i8] c"F\00"
%F = type {
	%_F_vtable*
}

%_F_vtable = type {
	i32,
	i32,
	i8*,
	%F* () *,
	%Object* (%F*) *,
	%String* (%F*) *,
	%F* (%F*) *
}

@_F_vtable_prototype = constant %_F_vtable {
	i32 11,
	i32 ptrtoint (%F* getelementptr (%F, %F* null, i32 1) to i32),
	i8* getelementptr ([2 x i8], [2 x i8]* @str.F, i32 0, i32 0),
	%F* () * @F_new,
	%Object* (%F*) * bitcast (%Object* (%Object*) * @Object_abort to %Object* (%F*) *),
	%String* (%F*) * bitcast (%String* (%Object*) * @Object_type_name to %String* (%F*) *),
	%F* (%F*) * bitcast (%Object* (%Object*) * @Object_copy to %F* (%F*) *)
}

@str.G = internal constant [2 x i8] c"G\00"
%G = type {
	%_G_vtable*
}

%_G_vtable = type {
	i32,
	i32,
	i8*,
	%G* () *,
	%Object* (%G*) *,
	%String* (%G*) *,
	%G* (%G*) *
}

@_G_vtable_prototype = constant %_G_vtable {
	i32 12,
	i32 ptrtoint (%G* getelementptr (%G, %G* null, i32 1) to i32),
	i8* getelementptr ([2 x i8], [2 x i8]* @str.G, i32 0, i32 0),
	%G* () * @G_new,
	%Object* (%G*) * bitcast (%Object* (%Object*) * @Object_abort to %Object* (%G*) *),
	%String* (%G*) * bitcast (%String* (%Object*) * @Object_type_name to %String* (%G*) *),
	%G* (%G*) * bitcast (%Object* (%Object*) * @Object_copy to %G* (%G*) *)
}

@str.H = internal constant [2 x i8] c"H\00"
%H = type {
	%_H_vtable*
}

%_H_vtable = type {
	i32,
	i32,
	i8*,
	%H* () *,
	%Object* (%H*) *,
	%String* (%H*) *,
	%H* (%H*) *
}

@_H_vtable_prototype = constant %_H_vtable {
	i32 13,
	i32 ptrtoint (%H* getelementptr (%H, %H* null, i32 1) to i32),
	i8* getelementptr ([2 x i8], [2 x i8]* @str.H, i32 0, i32 0),
	%H* () * @H_new,
	%Object* (%H*) * bitcast (%Object* (%Object*) * @Object_abort to %Object* (%H*) *),
	%String* (%H*) * bitcast (%String* (%Object*) * @Object_type_name to %String* (%H*) *),
	%H* (%H*) * bitcast (%Object* (%Object*) * @Object_copy to %H* (%H*) *)
}

@str.I = internal constant [2 x i8] c"I\00"
%I = type {
	%_I_vtable*
}

%_I_vtable = type {
	i32,
	i32,
	i8*,
	%I* () *,
	%Object* (%I*) *,
	%String* (%I*) *,
	%I* (%I*) *
}

@_I_vtable_prototype = constant %_I_vtable {
	i32 14,
	i32 ptrtoint (%I* getelementptr (%I, %I* null, i32 1) to i32),
	i8* getelementptr ([2 x i8], [2 x i8]* @str.I, i32 0, i32 0),
	%I* () * @I_new,
	%Object* (%I*) * bitcast (%Object* (%Object*) * @Object_abort to %Object* (%I*) *),
	%String* (%I*) * bitcast (%String* (%Object*) * @Object_type_name to %String* (%I*) *),
	%I* (%I*) * bitcast (%Object* (%Object*) * @Object_copy to %I* (%I*) *)
}

@str.J = internal constant [2 x i8] c"J\00"
%J = type {
	%_J_vtable*
}

%_J_vtable = type {
	i32,
	i32,
	i8*,
	%J* () *,
	%Object* (%J*) *,
	%String* (%J*) *,
	%J* (%J*) *
}

@_J_vtable_prototype = constant %_J_vtable {
	i32 15,
	i32 ptrtoint (%J* getelementptr (%J, %J* null, i32 1) to i32),
	i8* getelementptr ([2 x i8], [2 x i8]* @str.J, i32 0, i32 0),
	%J* () * @J_new,
	%Object* (%J*) * bitcast (%Object* (%Object*) * @Object_abort to %Object* (%J*) *),
	%String* (%J*) * bitcast (%String* (%Object*) * @Object_type_name to %String* (%J*) *),
	%J* (%J*) * bitcast (%Object* (%Object*) * @Object_copy to %J* (%J*) *)
}

@str.K = internal constant [2 x i8] c"K\00"
%K = type {
	%_K_vtable*
}

%_K_vtable = type {
	i32,
	i32,
	i8*,
	%K* () *,
	%Object* (%K*) *,
	%String* (%K*) *,
	%K* (%K*) *
}

@_K_vtable_prototype = constant %_K_vtable {
	i32 16,
	i32 ptrtoint (%K* getelementptr (%K, %K* null, i32 1) to i32),
	i8* getelementptr ([2 x i8], [2 x i8]* @str.K, i32 0, i32 0),
	%K* () * @K_new,
	%Object* (%K*) * bitcast (%Object* (%Object*) * @Object_abort to %Object* (%K*) *),
	%String* (%K*) * bitcast (%String* (%Object*) * @Object_type_name to %String* (%K*) *),
	%K* (%K*) * bitcast (%Object* (%Object*) * @Object_copy to %K* (%K*) *)
}

@str.L = internal constant [2 x i8] c"L\00"
%L = type {
	%_L_vtable*
}

%_L_vtable = type {
	i32,
	i32,
	i8*,
	%L* () *,
	%Object* (%L*) *,
	%String* (%L*) *,
	%L* (%L*) *
}

@_L_vtable_prototype = constant %_L_vtable {
	i32 17,
	i32 ptrtoint (%L* getelementptr (%L, %L* null, i32 1) to i32),
	i8* getelementptr ([2 x i8], [2 x i8]* @str.L, i32 0, i32 0),
	%L* () * @L_new,
	%Object* (%L*) * bitcast (%Object* (%Object*) * @Object_abort to %Object* (%L*) *),
	%String* (%L*) * bitcast (%String* (%Object*) * @Object_type_name to %String* (%L*) *),
	%L* (%L*) * bitcast (%Object* (%Object*) * @Object_copy to %L* (%L*) *)
}

@str.M = internal constant [2 x i8] c"M\00"
%M = type {
	%_M_vtable*
}

%_M_vtable = type {
	i32,
	i32,
	i8*,
	%M* () *,
	%Object* (%M*) *,
	%String* (%M*) *,
	%M* (%M*) *
}

@_M_vtable_prototype = constant %_M_vtable {
	i32 18,
	i32 ptrtoint (%M* getelementptr (%M, %M* null, i32 1) to i32),
	i8* getelementptr ([2 x i8], [2 x i8]* @str.M, i32 0, i32 0),
	%M* () * @M_new,
	%Object* (%M*) * bitcast (%Object* (%Object*) * @Object_abort to %Object* (%M*) *),
	%String* (%M*) * bitcast (%String* (%Object*) * @Object_type_name to %String* (%M*) *),
	%M* (%M*) * bitcast (%Object* (%Object*) * @Object_copy to %M* (%M*) *)
}

@str.N = internal constant [2 x i8] c"N\00"
%N = type {
	%_N_vtable*
}

%_N_vtable = type {
	i32,
	i32,
	i8*,
	%N* () *,
	%Object* (%N*) *,
	%String* (%N*) *,
	%N* (%N*) *
}

@_N_vtable_prototype = constant %_N_vtable {
	i32 19,
	i32 ptrtoint (%N* getelementptr (%N, %N* null, i32 1) to i32),
	i8* getelementptr ([2 x i8], [2 x i8]* @str.N, i32 0, i32 0),
	%N* () * @N_new,
	%Object* (%N*) * bitcast (%Object* (%Object*) * @Object_abort to %Object* (%N*) *),
	%String* (%N*) * bitcast (%String* (%Object*) * @Object_type_name to %String* (%N*) *),
	%N* (%N*) * bitcast (%Object* (%Object*) * @Object_copy to %N* (%N*) *)
}

@str.O = internal constant [2 x i8] c"O\00"
%O = type {
	%_O_vtable*
}

%_O_vtable = type {
	i32,
	i32,
	i8*,
	%O* () *,
	%Object* (%O*) *,
	%String* (%O*) *,
	%O* (%O*) *
}

@_O_vtable_prototype = constant %_O_vtable {
	i32 20,
	i32 ptrtoint (%O* getelementptr (%O, %O* null, i32 1) to i32),
	i8* getelementptr ([2 x i8], [2 x i8]* @str.O, i32 0, i32 0),
	%O* () * @O_new,
	%Object* (%O*) * bitcast (%Object* (%Object*) * @Object_abort to %Object* (%O*) *),
	%String* (%O*) * bitcast (%String* (%Object*) * @Object_type_name to %String* (%O*) *),
	%O* (%O*) * bitcast (%Object* (%Object*) * @Object_copy to %O* (%O*) *)
}

@str.P = internal constant [2 x i8] c"P\00"
%P = type {
	%_P_vtable*
}

%_P_vtable = type {
	i32,
	i32,
	i8*,
	%P* () *,
	%Object* (%P*) *,
	%String* (%P*) *,
	%P* (%P*) *
}

@_P_vtable_prototype = constant %_P_vtable {
	i32 21,
	i32 ptrtoint (%P* getelementptr (%P, %P* null, i32 1) to i32),
	i8* getelementptr ([2 x i8], [2 x i8]* @str.P, i32 0, i32 0),
	%P* () * @P_new,
	%Object* (%P*) * bitcast (%Object* (%Object*) * @Object_abort to %Object* (%P*) *),
	%String* (%P*) * bitcast (%String* (%Object*) * @Object_type_name to %String* (%P*) *),
	%P* (%P*) * bitcast (%Object* (%Object*) * @Object_copy to %P* (%P*) *)
}

@str.Q = internal constant [2 x i8] c"Q\00"
%Q = type {
	%_Q_vtable*
}

%_Q_vtable = type {
	i32,
	i32,
	i8*,
	%Q* () *,
	%Object* (%Q*) *,
	%String* (%Q*) *,
	%Q* (%Q*) *
}

@_Q_vtable_prototype = constant %_Q_vtable {
	i32 22,
	i32 ptrtoint (%Q* getelementptr (%Q, %Q* null, i32 1) to i32),
	i8* getelementptr ([2 x i8], [2 x i8]* @str.Q, i32 0, i32 0),
	%Q* () * @Q_new,
	%Object* (%Q*) * bitcast (%Object* (%Object*) * @Object_abort to %Object* (%Q*) *),
	%String* (%Q*) * bitcast (%String* (%Object*) * @Object_type_name to %String* (%Q*) *),
	%Q* (%Q*) * bitcast (%Object* (%Object*) * @Object_copy to %Q* (%Q*) *)
}

@str.1 = internal constant [14 x i8] c"<basic class>\00"
@String.1 = constant %String {
	%_String_vtable* @_String_vtable_prototype,
	i8* getelementptr ([14 x i8], [14 x i8]* @str.1, i32 0, i32 0)
}

@str.0 = internal constant [14 x i8] c"caseloop_o.cl\00"
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

define %Object* @Main.main(%Main* %self) {

entry:
	%tmp.1 = alloca i32
	%tmp.2 = alloca i32
	%tmp.3 = alloca %Object*
	%tmp.4 = alloca %Object*
	%tmp.5 = alloca i32
	%tmp.6 = alloca %Int*
	%vtpm.0 = alloca %A*
	%vtpm.1 = alloca %B*
	%vtpm.2 = alloca %C*
	%vtpm.3 = alloca %D*
	%vtpm.4 = alloca %E*
	%vtpm.5 = alloca %F*
	%vtpm.6 = alloca %G*
	%vtpm.7 = alloca %H*
	%vtpm.8 = alloca %I*
	%vtpm.9 = alloca %J*
	%vtpm.10 = alloca %K*
	%vtpm.11 = alloca %L*
	%vtpm.12 = alloca %M*
	%vtpm.13 = alloca %N*
	%vtpm.14 = alloca %O*
	%vtpm.15 = alloca %P*
	%vtpm.16 = alloca %Q*
	%tmp.7 = alloca i32
	%tmp.8 = alloca %Int*
	%vtpm.17 = alloca %A*
	%vtpm.18 = alloca %B*
	%vtpm.19 = alloca %C*
	%vtpm.20 = alloca %D*
	%vtpm.21 = alloca %E*
	%vtpm.22 = alloca %F*
	%vtpm.23 = alloca %G*
	%vtpm.24 = alloca %H*
	%vtpm.25 = alloca %I*
	%vtpm.26 = alloca %J*
	%vtpm.27 = alloca %K*
	%vtpm.28 = alloca %L*
	%vtpm.29 = alloca %M*
	%vtpm.30 = alloca %N*
	%vtpm.31 = alloca %O*
	%vtpm.32 = alloca %P*
	%vtpm.33 = alloca %Q*
	%tmp.9 = alloca i32
	%tmp.0 = alloca %Main*
	store %Main* %self, %Main** %tmp.0
	store i32 0, i32* %tmp.1
	store i32 0, i32* %tmp.2
	%tmp.10 = call %A* @A_new(  )
	%tmp.11 = bitcast %A* %tmp.10 to %Object*
	store %Object* %tmp.11, %Object** %tmp.3
	%tmp.12 = call %Q* @Q_new(  )
	%tmp.13 = bitcast %Q* %tmp.12 to %Object*
	store %Object* %tmp.13, %Object** %tmp.4
	store i32 0, i32* %tmp.5
	br label %loop.0

loop.0:
	%tmp.14 = load i32, i32* %tmp.1
	%tmp.15 = icmp slt i32 %tmp.14, 100
	br i1 %tmp.15, label %true.0, label %false.0

true.0:
	%tmp.16 = load %Object*, %Object** %tmp.4
	%tmp.17 = icmp eq %Object* %tmp.16, null
	br i1 %tmp.17, label %abort, label %ok.0

ok.0:
	%tmp.18 = getelementptr %Object, %Object* %tmp.16, i32 0, i32 0
	%tmp.19 = load %_Object_vtable*, %_Object_vtable** %tmp.18
	%tmp.20 = getelementptr %_Object_vtable, %_Object_vtable* %tmp.19, i32 0, i32 0
	%tmp.21 = load i32, i32* %tmp.20
	br label %case.hdr.1

case.hdr.1:
	br label %case.22.1

case.22.1:
	%vtpm.34 = icmp slt i32 %tmp.21, 22
	br i1 %vtpm.34, label %br_exit.22.1, label %src_gte_br.22.1

src_gte_br.22.1:
	%vtpm.35 = icmp sgt i32 %tmp.21, 22
	br i1 %vtpm.35, label %br_exit.22.1, label %src_lte_mc.22.1

src_lte_mc.22.1:
	%tmp.22 = bitcast %Object* %tmp.16 to %Q*
	store %Q* %tmp.22, %Q** %vtpm.16
	%tmp.24 = call %Int* @Int_new(  )
	call void(%Int*, i32 ) @Int_init( %Int* %tmp.24, i32 17 )
	store %Int* %tmp.24, %Int** %tmp.6
	br label %case.exit.1

br_exit.22.1:
	br label %case.21.1

case.21.1:
	%vtpm.36 = icmp slt i32 %tmp.21, 21
	br i1 %vtpm.36, label %br_exit.21.1, label %src_gte_br.21.1

src_gte_br.21.1:
	%vtpm.37 = icmp sgt i32 %tmp.21, 22
	br i1 %vtpm.37, label %br_exit.21.1, label %src_lte_mc.21.1

src_lte_mc.21.1:
	%tmp.25 = bitcast %Object* %tmp.16 to %P*
	store %P* %tmp.25, %P** %vtpm.15
	%tmp.27 = call %Int* @Int_new(  )
	call void(%Int*, i32 ) @Int_init( %Int* %tmp.27, i32 16 )
	store %Int* %tmp.27, %Int** %tmp.6
	br label %case.exit.1

br_exit.21.1:
	br label %case.20.1

case.20.1:
	%vtpm.38 = icmp slt i32 %tmp.21, 20
	br i1 %vtpm.38, label %br_exit.20.1, label %src_gte_br.20.1

src_gte_br.20.1:
	%vtpm.39 = icmp sgt i32 %tmp.21, 22
	br i1 %vtpm.39, label %br_exit.20.1, label %src_lte_mc.20.1

src_lte_mc.20.1:
	%tmp.28 = bitcast %Object* %tmp.16 to %O*
	store %O* %tmp.28, %O** %vtpm.14
	%tmp.30 = call %Int* @Int_new(  )
	call void(%Int*, i32 ) @Int_init( %Int* %tmp.30, i32 15 )
	store %Int* %tmp.30, %Int** %tmp.6
	br label %case.exit.1

br_exit.20.1:
	br label %case.19.1

case.19.1:
	%vtpm.40 = icmp slt i32 %tmp.21, 19
	br i1 %vtpm.40, label %br_exit.19.1, label %src_gte_br.19.1

src_gte_br.19.1:
	%vtpm.41 = icmp sgt i32 %tmp.21, 22
	br i1 %vtpm.41, label %br_exit.19.1, label %src_lte_mc.19.1

src_lte_mc.19.1:
	%tmp.31 = bitcast %Object* %tmp.16 to %N*
	store %N* %tmp.31, %N** %vtpm.13
	%tmp.33 = call %Int* @Int_new(  )
	call void(%Int*, i32 ) @Int_init( %Int* %tmp.33, i32 14 )
	store %Int* %tmp.33, %Int** %tmp.6
	br label %case.exit.1

br_exit.19.1:
	br label %case.18.1

case.18.1:
	%vtpm.42 = icmp slt i32 %tmp.21, 18
	br i1 %vtpm.42, label %br_exit.18.1, label %src_gte_br.18.1

src_gte_br.18.1:
	%vtpm.43 = icmp sgt i32 %tmp.21, 22
	br i1 %vtpm.43, label %br_exit.18.1, label %src_lte_mc.18.1

src_lte_mc.18.1:
	%tmp.34 = bitcast %Object* %tmp.16 to %M*
	store %M* %tmp.34, %M** %vtpm.12
	%tmp.36 = call %Int* @Int_new(  )
	call void(%Int*, i32 ) @Int_init( %Int* %tmp.36, i32 13 )
	store %Int* %tmp.36, %Int** %tmp.6
	br label %case.exit.1

br_exit.18.1:
	br label %case.17.1

case.17.1:
	%vtpm.44 = icmp slt i32 %tmp.21, 17
	br i1 %vtpm.44, label %br_exit.17.1, label %src_gte_br.17.1

src_gte_br.17.1:
	%vtpm.45 = icmp sgt i32 %tmp.21, 22
	br i1 %vtpm.45, label %br_exit.17.1, label %src_lte_mc.17.1

src_lte_mc.17.1:
	%tmp.37 = bitcast %Object* %tmp.16 to %L*
	store %L* %tmp.37, %L** %vtpm.11
	%tmp.39 = call %Int* @Int_new(  )
	call void(%Int*, i32 ) @Int_init( %Int* %tmp.39, i32 12 )
	store %Int* %tmp.39, %Int** %tmp.6
	br label %case.exit.1

br_exit.17.1:
	br label %case.16.1

case.16.1:
	%vtpm.46 = icmp slt i32 %tmp.21, 16
	br i1 %vtpm.46, label %br_exit.16.1, label %src_gte_br.16.1

src_gte_br.16.1:
	%vtpm.47 = icmp sgt i32 %tmp.21, 22
	br i1 %vtpm.47, label %br_exit.16.1, label %src_lte_mc.16.1

src_lte_mc.16.1:
	%tmp.40 = bitcast %Object* %tmp.16 to %K*
	store %K* %tmp.40, %K** %vtpm.10
	%tmp.42 = call %Int* @Int_new(  )
	call void(%Int*, i32 ) @Int_init( %Int* %tmp.42, i32 11 )
	store %Int* %tmp.42, %Int** %tmp.6
	br label %case.exit.1

br_exit.16.1:
	br label %case.15.1

case.15.1:
	%vtpm.48 = icmp slt i32 %tmp.21, 15
	br i1 %vtpm.48, label %br_exit.15.1, label %src_gte_br.15.1

src_gte_br.15.1:
	%vtpm.49 = icmp sgt i32 %tmp.21, 22
	br i1 %vtpm.49, label %br_exit.15.1, label %src_lte_mc.15.1

src_lte_mc.15.1:
	%tmp.43 = bitcast %Object* %tmp.16 to %J*
	store %J* %tmp.43, %J** %vtpm.9
	%tmp.45 = call %Int* @Int_new(  )
	call void(%Int*, i32 ) @Int_init( %Int* %tmp.45, i32 10 )
	store %Int* %tmp.45, %Int** %tmp.6
	br label %case.exit.1

br_exit.15.1:
	br label %case.14.1

case.14.1:
	%vtpm.50 = icmp slt i32 %tmp.21, 14
	br i1 %vtpm.50, label %br_exit.14.1, label %src_gte_br.14.1

src_gte_br.14.1:
	%vtpm.51 = icmp sgt i32 %tmp.21, 22
	br i1 %vtpm.51, label %br_exit.14.1, label %src_lte_mc.14.1

src_lte_mc.14.1:
	%tmp.46 = bitcast %Object* %tmp.16 to %I*
	store %I* %tmp.46, %I** %vtpm.8
	%tmp.48 = call %Int* @Int_new(  )
	call void(%Int*, i32 ) @Int_init( %Int* %tmp.48, i32 9 )
	store %Int* %tmp.48, %Int** %tmp.6
	br label %case.exit.1

br_exit.14.1:
	br label %case.13.1

case.13.1:
	%vtpm.52 = icmp slt i32 %tmp.21, 13
	br i1 %vtpm.52, label %br_exit.13.1, label %src_gte_br.13.1

src_gte_br.13.1:
	%vtpm.53 = icmp sgt i32 %tmp.21, 22
	br i1 %vtpm.53, label %br_exit.13.1, label %src_lte_mc.13.1

src_lte_mc.13.1:
	%tmp.49 = bitcast %Object* %tmp.16 to %H*
	store %H* %tmp.49, %H** %vtpm.7
	%tmp.51 = call %Int* @Int_new(  )
	call void(%Int*, i32 ) @Int_init( %Int* %tmp.51, i32 8 )
	store %Int* %tmp.51, %Int** %tmp.6
	br label %case.exit.1

br_exit.13.1:
	br label %case.12.1

case.12.1:
	%vtpm.54 = icmp slt i32 %tmp.21, 12
	br i1 %vtpm.54, label %br_exit.12.1, label %src_gte_br.12.1

src_gte_br.12.1:
	%vtpm.55 = icmp sgt i32 %tmp.21, 22
	br i1 %vtpm.55, label %br_exit.12.1, label %src_lte_mc.12.1

src_lte_mc.12.1:
	%tmp.52 = bitcast %Object* %tmp.16 to %G*
	store %G* %tmp.52, %G** %vtpm.6
	%tmp.54 = call %Int* @Int_new(  )
	call void(%Int*, i32 ) @Int_init( %Int* %tmp.54, i32 7 )
	store %Int* %tmp.54, %Int** %tmp.6
	br label %case.exit.1

br_exit.12.1:
	br label %case.11.1

case.11.1:
	%vtpm.56 = icmp slt i32 %tmp.21, 11
	br i1 %vtpm.56, label %br_exit.11.1, label %src_gte_br.11.1

src_gte_br.11.1:
	%vtpm.57 = icmp sgt i32 %tmp.21, 22
	br i1 %vtpm.57, label %br_exit.11.1, label %src_lte_mc.11.1

src_lte_mc.11.1:
	%tmp.55 = bitcast %Object* %tmp.16 to %F*
	store %F* %tmp.55, %F** %vtpm.5
	%tmp.57 = call %Int* @Int_new(  )
	call void(%Int*, i32 ) @Int_init( %Int* %tmp.57, i32 6 )
	store %Int* %tmp.57, %Int** %tmp.6
	br label %case.exit.1

br_exit.11.1:
	br label %case.10.1

case.10.1:
	%vtpm.58 = icmp slt i32 %tmp.21, 10
	br i1 %vtpm.58, label %br_exit.10.1, label %src_gte_br.10.1

src_gte_br.10.1:
	%vtpm.59 = icmp sgt i32 %tmp.21, 22
	br i1 %vtpm.59, label %br_exit.10.1, label %src_lte_mc.10.1

src_lte_mc.10.1:
	%tmp.58 = bitcast %Object* %tmp.16 to %E*
	store %E* %tmp.58, %E** %vtpm.4
	%tmp.60 = call %Int* @Int_new(  )
	call void(%Int*, i32 ) @Int_init( %Int* %tmp.60, i32 5 )
	store %Int* %tmp.60, %Int** %tmp.6
	br label %case.exit.1

br_exit.10.1:
	br label %case.9.1

case.9.1:
	%vtpm.60 = icmp slt i32 %tmp.21, 9
	br i1 %vtpm.60, label %br_exit.9.1, label %src_gte_br.9.1

src_gte_br.9.1:
	%vtpm.61 = icmp sgt i32 %tmp.21, 22
	br i1 %vtpm.61, label %br_exit.9.1, label %src_lte_mc.9.1

src_lte_mc.9.1:
	%tmp.61 = bitcast %Object* %tmp.16 to %D*
	store %D* %tmp.61, %D** %vtpm.3
	%tmp.63 = call %Int* @Int_new(  )
	call void(%Int*, i32 ) @Int_init( %Int* %tmp.63, i32 4 )
	store %Int* %tmp.63, %Int** %tmp.6
	br label %case.exit.1

br_exit.9.1:
	br label %case.8.1

case.8.1:
	%vtpm.62 = icmp slt i32 %tmp.21, 8
	br i1 %vtpm.62, label %br_exit.8.1, label %src_gte_br.8.1

src_gte_br.8.1:
	%vtpm.63 = icmp sgt i32 %tmp.21, 22
	br i1 %vtpm.63, label %br_exit.8.1, label %src_lte_mc.8.1

src_lte_mc.8.1:
	%tmp.64 = bitcast %Object* %tmp.16 to %C*
	store %C* %tmp.64, %C** %vtpm.2
	%tmp.66 = call %Int* @Int_new(  )
	call void(%Int*, i32 ) @Int_init( %Int* %tmp.66, i32 3 )
	store %Int* %tmp.66, %Int** %tmp.6
	br label %case.exit.1

br_exit.8.1:
	br label %case.7.1

case.7.1:
	%vtpm.64 = icmp slt i32 %tmp.21, 7
	br i1 %vtpm.64, label %br_exit.7.1, label %src_gte_br.7.1

src_gte_br.7.1:
	%vtpm.65 = icmp sgt i32 %tmp.21, 22
	br i1 %vtpm.65, label %br_exit.7.1, label %src_lte_mc.7.1

src_lte_mc.7.1:
	%tmp.67 = bitcast %Object* %tmp.16 to %B*
	store %B* %tmp.67, %B** %vtpm.1
	%tmp.69 = call %Int* @Int_new(  )
	call void(%Int*, i32 ) @Int_init( %Int* %tmp.69, i32 2 )
	store %Int* %tmp.69, %Int** %tmp.6
	br label %case.exit.1

br_exit.7.1:
	br label %case.6.1

case.6.1:
	%vtpm.66 = icmp slt i32 %tmp.21, 6
	br i1 %vtpm.66, label %br_exit.6.1, label %src_gte_br.6.1

src_gte_br.6.1:
	%vtpm.67 = icmp sgt i32 %tmp.21, 22
	br i1 %vtpm.67, label %br_exit.6.1, label %src_lte_mc.6.1

src_lte_mc.6.1:
	%tmp.70 = bitcast %Object* %tmp.16 to %A*
	store %A* %tmp.70, %A** %vtpm.0
	%tmp.72 = call %Int* @Int_new(  )
	call void(%Int*, i32 ) @Int_init( %Int* %tmp.72, i32 1 )
	store %Int* %tmp.72, %Int** %tmp.6
	br label %case.exit.1

br_exit.6.1:
	br label %abort

case.exit.1:
	%tmp.73 = load %Int*, %Int** %tmp.6
	%tmp.75 = getelementptr %Int, %Int* %tmp.73, i32 0, i32 1
	%tmp.74 = load i32, i32* %tmp.75
	store i32 %tmp.74, i32* %tmp.5
	%tmp.76 = load i32, i32* %tmp.1
	%tmp.77 = add i32 %tmp.76, 1
	store i32 %tmp.77, i32* %tmp.1
	br label %loop.0

false.0:
	%tmp.78 = load i32, i32* %tmp.5
	%tmp.79 = icmp eq i32 %tmp.78, 17
	%tmp.80 = xor i1 %tmp.79, true
	br i1 %tmp.80, label %true.2, label %false.2

true.2:
	%tmp.81 = load i32, i32* %tmp.2
	%tmp.82 = add i32 %tmp.81, 1
	store i32 %tmp.82, i32* %tmp.2
	store i32 %tmp.82, i32* %tmp.7
	br label %end.2

false.2:
	store i32 0, i32* %tmp.7
	br label %end.2

end.2:
	%tmp.83 = load i32, i32* %tmp.7
	store i32 0, i32* %tmp.1
	br label %loop.3

loop.3:
	%tmp.84 = load i32, i32* %tmp.1
	%tmp.85 = icmp slt i32 %tmp.84, 10
	br i1 %tmp.85, label %true.3, label %false.3

true.3:
	%tmp.86 = load %Object*, %Object** %tmp.3
	%tmp.87 = icmp eq %Object* %tmp.86, null
	br i1 %tmp.87, label %abort, label %ok.1

ok.1:
	%tmp.88 = getelementptr %Object, %Object* %tmp.86, i32 0, i32 0
	%tmp.89 = load %_Object_vtable*, %_Object_vtable** %tmp.88
	%tmp.90 = getelementptr %_Object_vtable, %_Object_vtable* %tmp.89, i32 0, i32 0
	%tmp.91 = load i32, i32* %tmp.90
	br label %case.hdr.4

case.hdr.4:
	br label %case.22.4

case.22.4:
	%vtpm.68 = icmp slt i32 %tmp.91, 22
	br i1 %vtpm.68, label %br_exit.22.4, label %src_gte_br.22.4

src_gte_br.22.4:
	%vtpm.69 = icmp sgt i32 %tmp.91, 22
	br i1 %vtpm.69, label %br_exit.22.4, label %src_lte_mc.22.4

src_lte_mc.22.4:
	%tmp.92 = bitcast %Object* %tmp.86 to %Q*
	store %Q* %tmp.92, %Q** %vtpm.33
	%tmp.94 = call %Int* @Int_new(  )
	call void(%Int*, i32 ) @Int_init( %Int* %tmp.94, i32 17 )
	store %Int* %tmp.94, %Int** %tmp.8
	br label %case.exit.4

br_exit.22.4:
	br label %case.21.4

case.21.4:
	%vtpm.70 = icmp slt i32 %tmp.91, 21
	br i1 %vtpm.70, label %br_exit.21.4, label %src_gte_br.21.4

src_gte_br.21.4:
	%vtpm.71 = icmp sgt i32 %tmp.91, 22
	br i1 %vtpm.71, label %br_exit.21.4, label %src_lte_mc.21.4

src_lte_mc.21.4:
	%tmp.95 = bitcast %Object* %tmp.86 to %P*
	store %P* %tmp.95, %P** %vtpm.32
	%tmp.97 = call %Int* @Int_new(  )
	call void(%Int*, i32 ) @Int_init( %Int* %tmp.97, i32 16 )
	store %Int* %tmp.97, %Int** %tmp.8
	br label %case.exit.4

br_exit.21.4:
	br label %case.20.4

case.20.4:
	%vtpm.72 = icmp slt i32 %tmp.91, 20
	br i1 %vtpm.72, label %br_exit.20.4, label %src_gte_br.20.4

src_gte_br.20.4:
	%vtpm.73 = icmp sgt i32 %tmp.91, 22
	br i1 %vtpm.73, label %br_exit.20.4, label %src_lte_mc.20.4

src_lte_mc.20.4:
	%tmp.98 = bitcast %Object* %tmp.86 to %O*
	store %O* %tmp.98, %O** %vtpm.31
	%tmp.100 = call %Int* @Int_new(  )
	call void(%Int*, i32 ) @Int_init( %Int* %tmp.100, i32 15 )
	store %Int* %tmp.100, %Int** %tmp.8
	br label %case.exit.4

br_exit.20.4:
	br label %case.19.4

case.19.4:
	%vtpm.74 = icmp slt i32 %tmp.91, 19
	br i1 %vtpm.74, label %br_exit.19.4, label %src_gte_br.19.4

src_gte_br.19.4:
	%vtpm.75 = icmp sgt i32 %tmp.91, 22
	br i1 %vtpm.75, label %br_exit.19.4, label %src_lte_mc.19.4

src_lte_mc.19.4:
	%tmp.101 = bitcast %Object* %tmp.86 to %N*
	store %N* %tmp.101, %N** %vtpm.30
	%tmp.103 = call %Int* @Int_new(  )
	call void(%Int*, i32 ) @Int_init( %Int* %tmp.103, i32 14 )
	store %Int* %tmp.103, %Int** %tmp.8
	br label %case.exit.4

br_exit.19.4:
	br label %case.18.4

case.18.4:
	%vtpm.76 = icmp slt i32 %tmp.91, 18
	br i1 %vtpm.76, label %br_exit.18.4, label %src_gte_br.18.4

src_gte_br.18.4:
	%vtpm.77 = icmp sgt i32 %tmp.91, 22
	br i1 %vtpm.77, label %br_exit.18.4, label %src_lte_mc.18.4

src_lte_mc.18.4:
	%tmp.104 = bitcast %Object* %tmp.86 to %M*
	store %M* %tmp.104, %M** %vtpm.29
	%tmp.106 = call %Int* @Int_new(  )
	call void(%Int*, i32 ) @Int_init( %Int* %tmp.106, i32 13 )
	store %Int* %tmp.106, %Int** %tmp.8
	br label %case.exit.4

br_exit.18.4:
	br label %case.17.4

case.17.4:
	%vtpm.78 = icmp slt i32 %tmp.91, 17
	br i1 %vtpm.78, label %br_exit.17.4, label %src_gte_br.17.4

src_gte_br.17.4:
	%vtpm.79 = icmp sgt i32 %tmp.91, 22
	br i1 %vtpm.79, label %br_exit.17.4, label %src_lte_mc.17.4

src_lte_mc.17.4:
	%tmp.107 = bitcast %Object* %tmp.86 to %L*
	store %L* %tmp.107, %L** %vtpm.28
	%tmp.109 = call %Int* @Int_new(  )
	call void(%Int*, i32 ) @Int_init( %Int* %tmp.109, i32 12 )
	store %Int* %tmp.109, %Int** %tmp.8
	br label %case.exit.4

br_exit.17.4:
	br label %case.16.4

case.16.4:
	%vtpm.80 = icmp slt i32 %tmp.91, 16
	br i1 %vtpm.80, label %br_exit.16.4, label %src_gte_br.16.4

src_gte_br.16.4:
	%vtpm.81 = icmp sgt i32 %tmp.91, 22
	br i1 %vtpm.81, label %br_exit.16.4, label %src_lte_mc.16.4

src_lte_mc.16.4:
	%tmp.110 = bitcast %Object* %tmp.86 to %K*
	store %K* %tmp.110, %K** %vtpm.27
	%tmp.112 = call %Int* @Int_new(  )
	call void(%Int*, i32 ) @Int_init( %Int* %tmp.112, i32 11 )
	store %Int* %tmp.112, %Int** %tmp.8
	br label %case.exit.4

br_exit.16.4:
	br label %case.15.4

case.15.4:
	%vtpm.82 = icmp slt i32 %tmp.91, 15
	br i1 %vtpm.82, label %br_exit.15.4, label %src_gte_br.15.4

src_gte_br.15.4:
	%vtpm.83 = icmp sgt i32 %tmp.91, 22
	br i1 %vtpm.83, label %br_exit.15.4, label %src_lte_mc.15.4

src_lte_mc.15.4:
	%tmp.113 = bitcast %Object* %tmp.86 to %J*
	store %J* %tmp.113, %J** %vtpm.26
	%tmp.115 = call %Int* @Int_new(  )
	call void(%Int*, i32 ) @Int_init( %Int* %tmp.115, i32 10 )
	store %Int* %tmp.115, %Int** %tmp.8
	br label %case.exit.4

br_exit.15.4:
	br label %case.14.4

case.14.4:
	%vtpm.84 = icmp slt i32 %tmp.91, 14
	br i1 %vtpm.84, label %br_exit.14.4, label %src_gte_br.14.4

src_gte_br.14.4:
	%vtpm.85 = icmp sgt i32 %tmp.91, 22
	br i1 %vtpm.85, label %br_exit.14.4, label %src_lte_mc.14.4

src_lte_mc.14.4:
	%tmp.116 = bitcast %Object* %tmp.86 to %I*
	store %I* %tmp.116, %I** %vtpm.25
	%tmp.118 = call %Int* @Int_new(  )
	call void(%Int*, i32 ) @Int_init( %Int* %tmp.118, i32 9 )
	store %Int* %tmp.118, %Int** %tmp.8
	br label %case.exit.4

br_exit.14.4:
	br label %case.13.4

case.13.4:
	%vtpm.86 = icmp slt i32 %tmp.91, 13
	br i1 %vtpm.86, label %br_exit.13.4, label %src_gte_br.13.4

src_gte_br.13.4:
	%vtpm.87 = icmp sgt i32 %tmp.91, 22
	br i1 %vtpm.87, label %br_exit.13.4, label %src_lte_mc.13.4

src_lte_mc.13.4:
	%tmp.119 = bitcast %Object* %tmp.86 to %H*
	store %H* %tmp.119, %H** %vtpm.24
	%tmp.121 = call %Int* @Int_new(  )
	call void(%Int*, i32 ) @Int_init( %Int* %tmp.121, i32 8 )
	store %Int* %tmp.121, %Int** %tmp.8
	br label %case.exit.4

br_exit.13.4:
	br label %case.12.4

case.12.4:
	%vtpm.88 = icmp slt i32 %tmp.91, 12
	br i1 %vtpm.88, label %br_exit.12.4, label %src_gte_br.12.4

src_gte_br.12.4:
	%vtpm.89 = icmp sgt i32 %tmp.91, 22
	br i1 %vtpm.89, label %br_exit.12.4, label %src_lte_mc.12.4

src_lte_mc.12.4:
	%tmp.122 = bitcast %Object* %tmp.86 to %G*
	store %G* %tmp.122, %G** %vtpm.23
	%tmp.124 = call %Int* @Int_new(  )
	call void(%Int*, i32 ) @Int_init( %Int* %tmp.124, i32 7 )
	store %Int* %tmp.124, %Int** %tmp.8
	br label %case.exit.4

br_exit.12.4:
	br label %case.11.4

case.11.4:
	%vtpm.90 = icmp slt i32 %tmp.91, 11
	br i1 %vtpm.90, label %br_exit.11.4, label %src_gte_br.11.4

src_gte_br.11.4:
	%vtpm.91 = icmp sgt i32 %tmp.91, 22
	br i1 %vtpm.91, label %br_exit.11.4, label %src_lte_mc.11.4

src_lte_mc.11.4:
	%tmp.125 = bitcast %Object* %tmp.86 to %F*
	store %F* %tmp.125, %F** %vtpm.22
	%tmp.127 = call %Int* @Int_new(  )
	call void(%Int*, i32 ) @Int_init( %Int* %tmp.127, i32 6 )
	store %Int* %tmp.127, %Int** %tmp.8
	br label %case.exit.4

br_exit.11.4:
	br label %case.10.4

case.10.4:
	%vtpm.92 = icmp slt i32 %tmp.91, 10
	br i1 %vtpm.92, label %br_exit.10.4, label %src_gte_br.10.4

src_gte_br.10.4:
	%vtpm.93 = icmp sgt i32 %tmp.91, 22
	br i1 %vtpm.93, label %br_exit.10.4, label %src_lte_mc.10.4

src_lte_mc.10.4:
	%tmp.128 = bitcast %Object* %tmp.86 to %E*
	store %E* %tmp.128, %E** %vtpm.21
	%tmp.130 = call %Int* @Int_new(  )
	call void(%Int*, i32 ) @Int_init( %Int* %tmp.130, i32 5 )
	store %Int* %tmp.130, %Int** %tmp.8
	br label %case.exit.4

br_exit.10.4:
	br label %case.9.4

case.9.4:
	%vtpm.94 = icmp slt i32 %tmp.91, 9
	br i1 %vtpm.94, label %br_exit.9.4, label %src_gte_br.9.4

src_gte_br.9.4:
	%vtpm.95 = icmp sgt i32 %tmp.91, 22
	br i1 %vtpm.95, label %br_exit.9.4, label %src_lte_mc.9.4

src_lte_mc.9.4:
	%tmp.131 = bitcast %Object* %tmp.86 to %D*
	store %D* %tmp.131, %D** %vtpm.20
	%tmp.133 = call %Int* @Int_new(  )
	call void(%Int*, i32 ) @Int_init( %Int* %tmp.133, i32 4 )
	store %Int* %tmp.133, %Int** %tmp.8
	br label %case.exit.4

br_exit.9.4:
	br label %case.8.4

case.8.4:
	%vtpm.96 = icmp slt i32 %tmp.91, 8
	br i1 %vtpm.96, label %br_exit.8.4, label %src_gte_br.8.4

src_gte_br.8.4:
	%vtpm.97 = icmp sgt i32 %tmp.91, 22
	br i1 %vtpm.97, label %br_exit.8.4, label %src_lte_mc.8.4

src_lte_mc.8.4:
	%tmp.134 = bitcast %Object* %tmp.86 to %C*
	store %C* %tmp.134, %C** %vtpm.19
	%tmp.136 = call %Int* @Int_new(  )
	call void(%Int*, i32 ) @Int_init( %Int* %tmp.136, i32 3 )
	store %Int* %tmp.136, %Int** %tmp.8
	br label %case.exit.4

br_exit.8.4:
	br label %case.7.4

case.7.4:
	%vtpm.98 = icmp slt i32 %tmp.91, 7
	br i1 %vtpm.98, label %br_exit.7.4, label %src_gte_br.7.4

src_gte_br.7.4:
	%vtpm.99 = icmp sgt i32 %tmp.91, 22
	br i1 %vtpm.99, label %br_exit.7.4, label %src_lte_mc.7.4

src_lte_mc.7.4:
	%tmp.137 = bitcast %Object* %tmp.86 to %B*
	store %B* %tmp.137, %B** %vtpm.18
	%tmp.139 = call %Int* @Int_new(  )
	call void(%Int*, i32 ) @Int_init( %Int* %tmp.139, i32 2 )
	store %Int* %tmp.139, %Int** %tmp.8
	br label %case.exit.4

br_exit.7.4:
	br label %case.6.4

case.6.4:
	%vtpm.100 = icmp slt i32 %tmp.91, 6
	br i1 %vtpm.100, label %br_exit.6.4, label %src_gte_br.6.4

src_gte_br.6.4:
	%vtpm.101 = icmp sgt i32 %tmp.91, 22
	br i1 %vtpm.101, label %br_exit.6.4, label %src_lte_mc.6.4

src_lte_mc.6.4:
	%tmp.140 = bitcast %Object* %tmp.86 to %A*
	store %A* %tmp.140, %A** %vtpm.17
	%tmp.142 = call %Int* @Int_new(  )
	call void(%Int*, i32 ) @Int_init( %Int* %tmp.142, i32 1 )
	store %Int* %tmp.142, %Int** %tmp.8
	br label %case.exit.4

br_exit.6.4:
	br label %abort

case.exit.4:
	%tmp.143 = load %Int*, %Int** %tmp.8
	%tmp.145 = getelementptr %Int, %Int* %tmp.143, i32 0, i32 1
	%tmp.144 = load i32, i32* %tmp.145
	store i32 %tmp.144, i32* %tmp.5
	%tmp.146 = load i32, i32* %tmp.1
	%tmp.147 = add i32 %tmp.146, 1
	store i32 %tmp.147, i32* %tmp.1
	br label %loop.3

false.3:
	%tmp.148 = load i32, i32* %tmp.5
	%tmp.149 = icmp eq i32 %tmp.148, 1
	%tmp.150 = xor i1 %tmp.149, true
	br i1 %tmp.150, label %true.5, label %false.5

true.5:
	%tmp.151 = load i32, i32* %tmp.2
	%tmp.152 = add i32 %tmp.151, 1
	store i32 %tmp.152, i32* %tmp.2
	store i32 %tmp.152, i32* %tmp.9
	br label %end.5

false.5:
	store i32 0, i32* %tmp.9
	br label %end.5

end.5:
	%tmp.153 = load i32, i32* %tmp.9
	%tmp.154 = load i32, i32* %tmp.2
	%tmp.155 = load %Main*, %Main** %tmp.0
	%tmp.156 = icmp eq %Main* %tmp.155, null
	br i1 %tmp.156, label %abort, label %ok.2

ok.2:
	%tmp.157 = getelementptr %Main, %Main* %tmp.155, i32 0, i32 0
	%tmp.158 = load %_Main_vtable*, %_Main_vtable** %tmp.157
	%tmp.159 = getelementptr %_Main_vtable, %_Main_vtable* %tmp.158, i32 0, i32 8
	%tmp.160 = load %Main* (%Main*,i32) *, %Main* (%Main*,i32) ** %tmp.159
	%tmp.161 = call %Main*(%Main*, i32 ) %tmp.160( %Main* %tmp.155, i32 %tmp.154 )
	%tmp.162 = load %Main*, %Main** %tmp.0
	%tmp.163 = bitcast %Main* %tmp.162 to %Object*
	ret %Object* %tmp.163

abort:
	call void @abort(  )
	unreachable
}

define %Main* @Main_new() {

entry:
	%tmp.164 = alloca %Main*
	%tmp.165 = getelementptr %_Main_vtable, %_Main_vtable* @_Main_vtable_prototype, i32 0, i32 1
	%tmp.166 = load i32, i32* %tmp.165
	%tmp.167 = call i8*(i32 ) @malloc( i32 %tmp.166 )
	%tmp.168 = bitcast i8* %tmp.167 to %Main*
	%malloc.null = icmp eq %Main* %tmp.168, null
	br i1 %malloc.null, label %abort, label %okay

okay:
	%tmp.169 = getelementptr %Main, %Main* %tmp.168, i32 0, i32 0
	store %_Main_vtable* @_Main_vtable_prototype, %_Main_vtable** %tmp.169
	store %Main* %tmp.168, %Main** %tmp.164
	ret %Main* %tmp.168

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
	ret %A* %tmp.4

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

define %C* @C_new() {

entry:
	%tmp.0 = alloca %C*
	%tmp.1 = getelementptr %_C_vtable, %_C_vtable* @_C_vtable_prototype, i32 0, i32 1
	%tmp.2 = load i32, i32* %tmp.1
	%tmp.3 = call i8*(i32 ) @malloc( i32 %tmp.2 )
	%tmp.4 = bitcast i8* %tmp.3 to %C*
	%malloc.null = icmp eq %C* %tmp.4, null
	br i1 %malloc.null, label %abort, label %okay

okay:
	%tmp.5 = getelementptr %C, %C* %tmp.4, i32 0, i32 0
	store %_C_vtable* @_C_vtable_prototype, %_C_vtable** %tmp.5
	store %C* %tmp.4, %C** %tmp.0
	ret %C* %tmp.4

abort:
	call void @abort(  )
	unreachable
}

define %D* @D_new() {

entry:
	%tmp.0 = alloca %D*
	%tmp.1 = getelementptr %_D_vtable, %_D_vtable* @_D_vtable_prototype, i32 0, i32 1
	%tmp.2 = load i32, i32* %tmp.1
	%tmp.3 = call i8*(i32 ) @malloc( i32 %tmp.2 )
	%tmp.4 = bitcast i8* %tmp.3 to %D*
	%malloc.null = icmp eq %D* %tmp.4, null
	br i1 %malloc.null, label %abort, label %okay

okay:
	%tmp.5 = getelementptr %D, %D* %tmp.4, i32 0, i32 0
	store %_D_vtable* @_D_vtable_prototype, %_D_vtable** %tmp.5
	store %D* %tmp.4, %D** %tmp.0
	ret %D* %tmp.4

abort:
	call void @abort(  )
	unreachable
}

define %E* @E_new() {

entry:
	%tmp.0 = alloca %E*
	%tmp.1 = getelementptr %_E_vtable, %_E_vtable* @_E_vtable_prototype, i32 0, i32 1
	%tmp.2 = load i32, i32* %tmp.1
	%tmp.3 = call i8*(i32 ) @malloc( i32 %tmp.2 )
	%tmp.4 = bitcast i8* %tmp.3 to %E*
	%malloc.null = icmp eq %E* %tmp.4, null
	br i1 %malloc.null, label %abort, label %okay

okay:
	%tmp.5 = getelementptr %E, %E* %tmp.4, i32 0, i32 0
	store %_E_vtable* @_E_vtable_prototype, %_E_vtable** %tmp.5
	store %E* %tmp.4, %E** %tmp.0
	ret %E* %tmp.4

abort:
	call void @abort(  )
	unreachable
}

define %F* @F_new() {

entry:
	%tmp.0 = alloca %F*
	%tmp.1 = getelementptr %_F_vtable, %_F_vtable* @_F_vtable_prototype, i32 0, i32 1
	%tmp.2 = load i32, i32* %tmp.1
	%tmp.3 = call i8*(i32 ) @malloc( i32 %tmp.2 )
	%tmp.4 = bitcast i8* %tmp.3 to %F*
	%malloc.null = icmp eq %F* %tmp.4, null
	br i1 %malloc.null, label %abort, label %okay

okay:
	%tmp.5 = getelementptr %F, %F* %tmp.4, i32 0, i32 0
	store %_F_vtable* @_F_vtable_prototype, %_F_vtable** %tmp.5
	store %F* %tmp.4, %F** %tmp.0
	ret %F* %tmp.4

abort:
	call void @abort(  )
	unreachable
}

define %G* @G_new() {

entry:
	%tmp.0 = alloca %G*
	%tmp.1 = getelementptr %_G_vtable, %_G_vtable* @_G_vtable_prototype, i32 0, i32 1
	%tmp.2 = load i32, i32* %tmp.1
	%tmp.3 = call i8*(i32 ) @malloc( i32 %tmp.2 )
	%tmp.4 = bitcast i8* %tmp.3 to %G*
	%malloc.null = icmp eq %G* %tmp.4, null
	br i1 %malloc.null, label %abort, label %okay

okay:
	%tmp.5 = getelementptr %G, %G* %tmp.4, i32 0, i32 0
	store %_G_vtable* @_G_vtable_prototype, %_G_vtable** %tmp.5
	store %G* %tmp.4, %G** %tmp.0
	ret %G* %tmp.4

abort:
	call void @abort(  )
	unreachable
}

define %H* @H_new() {

entry:
	%tmp.0 = alloca %H*
	%tmp.1 = getelementptr %_H_vtable, %_H_vtable* @_H_vtable_prototype, i32 0, i32 1
	%tmp.2 = load i32, i32* %tmp.1
	%tmp.3 = call i8*(i32 ) @malloc( i32 %tmp.2 )
	%tmp.4 = bitcast i8* %tmp.3 to %H*
	%malloc.null = icmp eq %H* %tmp.4, null
	br i1 %malloc.null, label %abort, label %okay

okay:
	%tmp.5 = getelementptr %H, %H* %tmp.4, i32 0, i32 0
	store %_H_vtable* @_H_vtable_prototype, %_H_vtable** %tmp.5
	store %H* %tmp.4, %H** %tmp.0
	ret %H* %tmp.4

abort:
	call void @abort(  )
	unreachable
}

define %I* @I_new() {

entry:
	%tmp.0 = alloca %I*
	%tmp.1 = getelementptr %_I_vtable, %_I_vtable* @_I_vtable_prototype, i32 0, i32 1
	%tmp.2 = load i32, i32* %tmp.1
	%tmp.3 = call i8*(i32 ) @malloc( i32 %tmp.2 )
	%tmp.4 = bitcast i8* %tmp.3 to %I*
	%malloc.null = icmp eq %I* %tmp.4, null
	br i1 %malloc.null, label %abort, label %okay

okay:
	%tmp.5 = getelementptr %I, %I* %tmp.4, i32 0, i32 0
	store %_I_vtable* @_I_vtable_prototype, %_I_vtable** %tmp.5
	store %I* %tmp.4, %I** %tmp.0
	ret %I* %tmp.4

abort:
	call void @abort(  )
	unreachable
}

define %J* @J_new() {

entry:
	%tmp.0 = alloca %J*
	%tmp.1 = getelementptr %_J_vtable, %_J_vtable* @_J_vtable_prototype, i32 0, i32 1
	%tmp.2 = load i32, i32* %tmp.1
	%tmp.3 = call i8*(i32 ) @malloc( i32 %tmp.2 )
	%tmp.4 = bitcast i8* %tmp.3 to %J*
	%malloc.null = icmp eq %J* %tmp.4, null
	br i1 %malloc.null, label %abort, label %okay

okay:
	%tmp.5 = getelementptr %J, %J* %tmp.4, i32 0, i32 0
	store %_J_vtable* @_J_vtable_prototype, %_J_vtable** %tmp.5
	store %J* %tmp.4, %J** %tmp.0
	ret %J* %tmp.4

abort:
	call void @abort(  )
	unreachable
}

define %K* @K_new() {

entry:
	%tmp.0 = alloca %K*
	%tmp.1 = getelementptr %_K_vtable, %_K_vtable* @_K_vtable_prototype, i32 0, i32 1
	%tmp.2 = load i32, i32* %tmp.1
	%tmp.3 = call i8*(i32 ) @malloc( i32 %tmp.2 )
	%tmp.4 = bitcast i8* %tmp.3 to %K*
	%malloc.null = icmp eq %K* %tmp.4, null
	br i1 %malloc.null, label %abort, label %okay

okay:
	%tmp.5 = getelementptr %K, %K* %tmp.4, i32 0, i32 0
	store %_K_vtable* @_K_vtable_prototype, %_K_vtable** %tmp.5
	store %K* %tmp.4, %K** %tmp.0
	ret %K* %tmp.4

abort:
	call void @abort(  )
	unreachable
}

define %L* @L_new() {

entry:
	%tmp.0 = alloca %L*
	%tmp.1 = getelementptr %_L_vtable, %_L_vtable* @_L_vtable_prototype, i32 0, i32 1
	%tmp.2 = load i32, i32* %tmp.1
	%tmp.3 = call i8*(i32 ) @malloc( i32 %tmp.2 )
	%tmp.4 = bitcast i8* %tmp.3 to %L*
	%malloc.null = icmp eq %L* %tmp.4, null
	br i1 %malloc.null, label %abort, label %okay

okay:
	%tmp.5 = getelementptr %L, %L* %tmp.4, i32 0, i32 0
	store %_L_vtable* @_L_vtable_prototype, %_L_vtable** %tmp.5
	store %L* %tmp.4, %L** %tmp.0
	ret %L* %tmp.4

abort:
	call void @abort(  )
	unreachable
}

define %M* @M_new() {

entry:
	%tmp.0 = alloca %M*
	%tmp.1 = getelementptr %_M_vtable, %_M_vtable* @_M_vtable_prototype, i32 0, i32 1
	%tmp.2 = load i32, i32* %tmp.1
	%tmp.3 = call i8*(i32 ) @malloc( i32 %tmp.2 )
	%tmp.4 = bitcast i8* %tmp.3 to %M*
	%malloc.null = icmp eq %M* %tmp.4, null
	br i1 %malloc.null, label %abort, label %okay

okay:
	%tmp.5 = getelementptr %M, %M* %tmp.4, i32 0, i32 0
	store %_M_vtable* @_M_vtable_prototype, %_M_vtable** %tmp.5
	store %M* %tmp.4, %M** %tmp.0
	ret %M* %tmp.4

abort:
	call void @abort(  )
	unreachable
}

define %N* @N_new() {

entry:
	%tmp.0 = alloca %N*
	%tmp.1 = getelementptr %_N_vtable, %_N_vtable* @_N_vtable_prototype, i32 0, i32 1
	%tmp.2 = load i32, i32* %tmp.1
	%tmp.3 = call i8*(i32 ) @malloc( i32 %tmp.2 )
	%tmp.4 = bitcast i8* %tmp.3 to %N*
	%malloc.null = icmp eq %N* %tmp.4, null
	br i1 %malloc.null, label %abort, label %okay

okay:
	%tmp.5 = getelementptr %N, %N* %tmp.4, i32 0, i32 0
	store %_N_vtable* @_N_vtable_prototype, %_N_vtable** %tmp.5
	store %N* %tmp.4, %N** %tmp.0
	ret %N* %tmp.4

abort:
	call void @abort(  )
	unreachable
}

define %O* @O_new() {

entry:
	%tmp.0 = alloca %O*
	%tmp.1 = getelementptr %_O_vtable, %_O_vtable* @_O_vtable_prototype, i32 0, i32 1
	%tmp.2 = load i32, i32* %tmp.1
	%tmp.3 = call i8*(i32 ) @malloc( i32 %tmp.2 )
	%tmp.4 = bitcast i8* %tmp.3 to %O*
	%malloc.null = icmp eq %O* %tmp.4, null
	br i1 %malloc.null, label %abort, label %okay

okay:
	%tmp.5 = getelementptr %O, %O* %tmp.4, i32 0, i32 0
	store %_O_vtable* @_O_vtable_prototype, %_O_vtable** %tmp.5
	store %O* %tmp.4, %O** %tmp.0
	ret %O* %tmp.4

abort:
	call void @abort(  )
	unreachable
}

define %P* @P_new() {

entry:
	%tmp.0 = alloca %P*
	%tmp.1 = getelementptr %_P_vtable, %_P_vtable* @_P_vtable_prototype, i32 0, i32 1
	%tmp.2 = load i32, i32* %tmp.1
	%tmp.3 = call i8*(i32 ) @malloc( i32 %tmp.2 )
	%tmp.4 = bitcast i8* %tmp.3 to %P*
	%malloc.null = icmp eq %P* %tmp.4, null
	br i1 %malloc.null, label %abort, label %okay

okay:
	%tmp.5 = getelementptr %P, %P* %tmp.4, i32 0, i32 0
	store %_P_vtable* @_P_vtable_prototype, %_P_vtable** %tmp.5
	store %P* %tmp.4, %P** %tmp.0
	ret %P* %tmp.4

abort:
	call void @abort(  )
	unreachable
}

define %Q* @Q_new() {

entry:
	%tmp.0 = alloca %Q*
	%tmp.1 = getelementptr %_Q_vtable, %_Q_vtable* @_Q_vtable_prototype, i32 0, i32 1
	%tmp.2 = load i32, i32* %tmp.1
	%tmp.3 = call i8*(i32 ) @malloc( i32 %tmp.2 )
	%tmp.4 = bitcast i8* %tmp.3 to %Q*
	%malloc.null = icmp eq %Q* %tmp.4, null
	br i1 %malloc.null, label %abort, label %okay

okay:
	%tmp.5 = getelementptr %Q, %Q* %tmp.4, i32 0, i32 0
	store %_Q_vtable* @_Q_vtable_prototype, %_Q_vtable** %tmp.5
	store %Q* %tmp.4, %Q** %tmp.0
	ret %Q* %tmp.4

abort:
	call void @abort(  )
	unreachable
}

