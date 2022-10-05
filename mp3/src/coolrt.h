/*
 * This file provides the runtime library for cool. It implements
 * the cool classes in C.  Feel free to change it to match the structure
 * of your code generator.
 */

#include <stdbool.h>

typedef struct Object Object;
typedef struct Int Int;
typedef struct Bool Bool;
typedef struct String String;
typedef struct IO IO;

typedef struct _Object_vtable Object_vtable;
typedef struct _Int_vtable Int_vtable;
typedef struct _Bool_vtable Bool_vtable;
typedef struct _String_vtable String_vtable;
typedef struct _IO_vtable IO_vtable;

/* class type definitions */
struct Object {
  const Object_vtable *vtblptr;
};

struct Int {
  const Int_vtable *vtblptr;
  int val;
};

struct Bool {
  const Bool_vtable *vtblptr;
  bool val;
};

struct String {
  const String_vtable *vtblptr;
  const char *val;
};

struct IO {
  const IO_vtable *vtblptr;
};

/* vtable type definitions */
struct _Object_vtable {
  int class_no;
  int class_size;
  const char *name;
  Object *(*new)();
  Object *(*abort)(Object *);
  const String *(*type_name)(Object *);
  Object *(*copy)(Object *);
};

struct _Int_vtable {
  int class_no;
  int class_size;
  const char *name;
  Int *(*new)();
  Object *(*abort)(Int *);
  const String *(*type_name)(Int *);
  Int *(*copy)(Int *);
};

struct _Bool_vtable {
  int class_no;
  int class_size;
  const char *name;
  Bool *(*new)();
  Object *(*abort)(Bool *);
  const String *(*type_name)(Bool *);
  Bool *(*copy)(Bool *);
};

struct _String_vtable {
  int class_no;
  int class_size;
  const char *name;
  String *(*new)();
  Object *(*abort)(String *);
  const String *(*type_name)(String *);
  String *(*copy)(String *);
  int (*length)(String *);
  String *(*concat)(String *, String *);
  String *(*substr)(String *, int, int);
};

struct _IO_vtable {
  int class_no;
  int class_size;
  const char *name;
  IO *(*new)();
  Object *(*abort)(IO *);
  const String *(*type_name)(IO *);
  IO *(*copy)(IO *);
  IO *(*out_string)(IO *, String *);
  IO *(*out_int)(IO *, int);
  String *(*in_string)(IO *);
  int (*in_int)(IO *);
};

/* methods in class Object */
Object *Object_new(void);
Object *Object_abort(Object *self);
const String *Object_type_name(Object *self);
Object *Object_copy();

/* methods in class Int */
Int *Int_new(void);
void Int_init(Int *self, int i);

/* methods in class Bool */
Bool *Bool_new(void);
void Bool_init(Bool *self, bool i);

/* methods in class String */
String *String_new(void);
int String_length(String *self);
String *String_concat(String *self, String *nxt);
String *String_substr(String *self, int i, int l);

/* methods in class IO */
IO *IO_new(void);
IO *IO_out_string(IO *self, String *s);
IO *IO_out_int(IO *self, int x);
String *IO_in_string(IO *self);
int IO_in_int(IO *self);
