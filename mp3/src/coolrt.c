#include "coolrt.h"

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>  //make my linter happy

/* This file provides the runtime library for cool. It implements
   the functions of the cool classes in C
   */

/* Class name strings */
const char Object_string[] = "Object";
const char String_string[] = "String";
const char Int_string[] = "Int";
const char Bool_string[] = "Bool";
const char IO_string[] = "IO";

const char default_string[] = "";

/* Class vtable prototypes */
// const Object_vtable _Object_vtable_prototype = {
//     0, sizeof(Object), Object_string, Object_new, Object_abort, Object_type_name, Object_copy};
// const Int_vtable _Int_vtable_prototype = {
//     1, sizeof(Int), Int_string, Int_new, Object_abort, Object_type_name, Object_copy};
// const Bool_vtable _Bool_vtable_prototype = {
//     2, sizeof(Bool), Bool_string, Bool_new, Object_abort, Object_type_name, Object_copy};
// const String_vtable _String_vtable_prototype = {
//     3, sizeof(String), String_string, String_new, Object_abort, Object_type_name, Object_copy, String_length, String_concat, String_substr};
// const IO_vtable _IO_vtable_prototype = {4, sizeof(IO), IO_string, IO_new, Object_abort, Object_type_name, Object_copy, IO_out_string, IO_out_int, IO_in_string, IO_in_int};
extern const Object_vtable _Object_vtable_prototype;
extern const Int_vtable _Int_vtable_prototype;
extern const Bool_vtable _Bool_vtable_prototype;
extern const String_vtable _String_vtable_prototype;
extern const IO_vtable _IO_vtable_prototype;

/* ADD CODE HERE FOR MORE VTABLE PROTOTYPES */

/*
// Methods in class object (only some are provided to you)
*/

Object *Object_abort(Object *self) {
  printf("Abort called from class %s\n",
         !self ? "Unknown" : self->vtblptr->name);
  exit(1);
  return self;
}

const String *Object_type_name(Object *self) {
  if (self == 0) {
    fprintf(stderr, "At %s(line %d): self is NULL\n", __FILE__, __LINE__);
    abort();
  }
  String *s = String_new();
  s->val = self->vtblptr->name;
  return s;
}

Object *Object_new() {
  Object *tmp = malloc(sizeof(Object));
  if (!tmp) {
    fprintf(stderr, "At %s(line %d): Out of memory\n", __FILE__, __LINE__);
    Object_abort(0LL);
  }
  tmp->vtblptr = &_Object_vtable_prototype;
  return tmp;
}

Object *Object_copy(Object *self) {
  if (self == 0) {
    fprintf(stderr, "At %s(line %d): self is NULL\n", __FILE__, __LINE__);
    abort();
  }
  int size=self->vtblptr->class_size;
  Object *ptr = malloc(size);
  if (!ptr) {
    fprintf(stderr, "At %s(line %d): Out of memory\n", __FILE__, __LINE__);
    Object_abort(0LL);
  }
  memcpy(ptr, self, size);
  return ptr;
}

/*
// Methods in class IO (only some are provided to you)
*/

IO *IO_out_string(IO *self, String *s) {
  if (self == 0 || s == 0) {
    fprintf(stderr, "At %s(line %d): NULL object\n", __FILE__, __LINE__);
    abort();
  }
  printf("%s", s->val);
  return self;
}

IO *IO_out_int(IO *self, int x) {
  if (self == 0) {
    fprintf(stderr, "At %s(line %d): NULL object\n", __FILE__, __LINE__);
    abort();
  }
  printf("%d", x);
  return self;
}

/*
 * Get one line from stream using get_line(), then discard newline character.
 * Allocate string *in_string_p and store result there.
 * Return number of chars read.
 */
static int get_one_line(char **in_string_p, FILE *stream) {
  /* Get one line worth of input */
  size_t len = 0;
  ssize_t num_chars_read;
  num_chars_read = getline(in_string_p, &len, stdin);
  if (*in_string_p == 0) {
    fprintf(stderr, "At %s(line %d): allocation failed in IO::in_string()\n",
            __FILE__, __LINE__);
    exit(1);
  }

  /* Discard the newline char, if any.  It may not exist if EOF reached. */
  if (num_chars_read > 0 && (*in_string_p)[num_chars_read - 1] == '\n') {
    (*in_string_p)[num_chars_read - 1] = '\0';
    --len;
  }

  return len;
}

/*
 * The method IO::in_string(): String reads a string from
 * the standard input, up to but not including a newline character.
 */
String *IO_in_string(IO *self) {
  if (self == 0) {
    fprintf(stderr, "At %s(line %d): self is NULL\n", __FILE__, __LINE__);
    abort();
  }

  /* Get one line worth of input with the newline, if any, discarded */
  char *in_string = 0;
  ssize_t len = get_one_line(&in_string, stdin);
  assert(in_string);

  /* We can take advantage of knowing the internal layout of String objects */
  String *str = String_new();
  str->val = in_string;
  return str;
}

/*
 * The method IO::in_int(): Int reads a single integer, which may be preceded
 * by whitespace.
 * Any characters following the integer, up to and including the next newline,
 * are discarded by in_int.
 */
int IO_in_int(IO *self) {
  if (self == 0) {
    fprintf(stderr, "At %s(line %d): self is NULL\n", __FILE__, __LINE__);
    abort();
  }

  /* Get one line worth of input with the newline, if any, discarded */
  char *in_string = 0;
  ssize_t len = get_one_line(&in_string, stdin);
  assert(in_string);

  /* Now extract initial int and ignore the rest of the line */
  int x;
  int num_ints = 0;
  if (len)
    /* Discards initial spaces*/
    num_ints = sscanf(in_string, " %d", &x);

  /* If no text found, abort. */
  if (num_ints == 0) {
    fprintf(stderr, "At %s(line %d): Invalid integer on input in IO::in_int()\n",
            __FILE__, __LINE__);
    Object_abort((Object *)self);
  }
  return x;
}

IO *IO_new() {
  IO *tmp = malloc(sizeof(IO));
  if (!tmp) {
    fprintf(stderr, "At %s(line %d): Out of memory\n", __FILE__, __LINE__);
    Object_abort(0LL);
  }
  tmp->vtblptr = &_IO_vtable_prototype;
  return tmp;
}

Int *Int_new() {
  Int *tmp = malloc(sizeof(Int));
  if (!tmp) {
    fprintf(stderr, "At %s(line %d): Out of memory\n", __FILE__, __LINE__);
    Object_abort(0LL);
  }
  tmp->vtblptr = &_Int_vtable_prototype;
  tmp->val = 0;
  return tmp;
}
void Int_init(Int *self, int i) {
  if (self == 0) {
    fprintf(stderr, "At %s(line %d): self is NULL\n", __FILE__, __LINE__);
    abort();
  }
  self->val = i;
}

Bool *Bool_new() {
  Bool *tmp = malloc(sizeof(Bool));
  if (!tmp) {
    fprintf(stderr, "At %s(line %d): Out of memory\n", __FILE__, __LINE__);
    Object_abort(0LL);
  }
  tmp->vtblptr = &_Bool_vtable_prototype;
  tmp->val = 0;
  return tmp;
}
void Bool_init(Bool *self, bool i) {
  if (self == 0) {
    fprintf(stderr, "At %s(line %d): self is NULL\n", __FILE__, __LINE__);
    abort();
  }
  self->val = i;
}

String *String_new() {
  String *tmp = malloc(sizeof(String));
  if (!tmp) {
    fprintf(stderr, "At %s(line %d): Out of memory\n", __FILE__, __LINE__);
    Object_abort(0LL);
  }
  tmp->vtblptr = &_String_vtable_prototype;
  tmp->val = "";
  return tmp;
}
int String_length(String *self) {
  if (self == 0) {
    fprintf(stderr, "At %s(line %d): self is NULL\n", __FILE__, __LINE__);
    abort();
  }
  return strlen(self->val);
}
String *String_concat(String *self, String *nxt) {
  if (self == 0 || nxt == 0) {
    fprintf(stderr, "At %s(line %d): NULL object\n", __FILE__, __LINE__);
    abort();
  }
  int n = String_length(self);
  int m = String_length(nxt);
  char *cptr = malloc(sizeof(char) * (n + m + 1));
  if (!cptr) {
    fprintf(stderr, "At %s(line %d): Out of memory\n", __FILE__, __LINE__);
    Object_abort(0LL);
  }
  strncpy(cptr, self->val, n);
  strncpy(cptr + n, nxt->val, m);
  cptr[n + m] = 0;
  String *s = String_new();
  s->val = cptr;
  return s;
}
String *String_substr(String *self, int i, int l) {
  if (self == 0) {
    fprintf(stderr, "At %s(line %d): NULL object\n", __FILE__, __LINE__);
    abort();
  }
  int n = String_length(self);
  if (i < 0 || i >= n || i + l < 0 || i + l > n) {
    fprintf(stderr, "At %s(line %d): Substring out of range\n",
            __FILE__, __LINE__);
    Object_abort((Object *)self);
  }
  char *cptr = malloc(sizeof(char) * (l + 1));
  if (!cptr) {
    fprintf(stderr, "At %s(line %d): Out of memory\n", __FILE__, __LINE__);
    Object_abort(0LL);
  }
  strncpy(cptr, self->val + i, l);
  cptr[l] = 0;
  String *s = String_new();
  s->val = cptr;
  return s;
}