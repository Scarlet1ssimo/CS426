//**************************************************************
//
// Code generator SKELETON
//
// Read the comments carefully and add code to build an LLVM program
//**************************************************************

#define EXTERN
#include "cgen.h"

#include <sstream>
#include <string>

//
extern int cgen_debug;

//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.  Feel free to add your
// own definitions as you see fit.
//
//////////////////////////////////////////////////////////////////////
EXTERN Symbol
    // required classes
    Object,
    IO, String, Int, Bool, Main,

    // class methods
    cool_abort, type_name, cool_copy, out_string, out_int, in_string, in_int,
    length, concat, substr,

    // class members
    val,

    // special symbols
    No_class,   // symbol that can't be the name of any user-defined class
    No_type,    // If e : No_type, then no code is generated for e.
    SELF_TYPE,  // Special code is generated for new SELF_TYPE.
    self,       // self generates code differently than other references

    // extras
    arg, arg2, prim_string, prim_int, prim_bool;

//********************************************************
//
// PREDEFINED FUNCTIONS:
//
// The following functions are already coded, you should
// not need to modify them, although you may if necessary.
//
//********************************************************

//
// Initializing the predefined symbols.
//
static void initialize_constants(void) {
  Object = idtable.add_string("Object");
  IO = idtable.add_string("IO");
  String = idtable.add_string("String");
  Int = idtable.add_string("Int");
  Bool = idtable.add_string("Bool");
  Main = idtable.add_string("Main");

  cool_abort = idtable.add_string("abort");
  type_name = idtable.add_string("type_name");
  cool_copy = idtable.add_string("copy");
  out_string = idtable.add_string("out_string");
  out_int = idtable.add_string("out_int");
  in_string = idtable.add_string("in_string");
  in_int = idtable.add_string("in_int");
  length = idtable.add_string("length");
  concat = idtable.add_string("concat");
  substr = idtable.add_string("substr");

  val = idtable.add_string("val");

  No_class = idtable.add_string("_no_class");
  No_type = idtable.add_string("_no_type");
  SELF_TYPE = idtable.add_string("SELF_TYPE");
  self = idtable.add_string("self");

  arg = idtable.add_string("arg");
  arg2 = idtable.add_string("arg2");
  prim_string = idtable.add_string("sbyte*");
  prim_int = idtable.add_string("int");
  prim_bool = idtable.add_string("bool");
}

//*********************************************************
//
// Define method for code generation
//
// This is the method called by the compiler driver
// `cgtest.cc'. cgen takes an `ostream' to which the assembly will be
// emitted, and it passes this and the class list of the
// code generator tree to the constructor for `CgenClassTable'.
// That constructor performs all of the work of the code
// generator.
//
//*********************************************************
void program_class::cgen(ostream &os) {
  initialize_constants();
  class_table = new CgenClassTable(classes, os);
}

// Create definitions for all String constants
void StrTable::code_string_table(ostream &s, CgenClassTable *ct) {
  for (auto entry : tbl) {
    entry->code_def(s, ct);
  }
}

// Create definitions for all Int constants
void IntTable::code_string_table(ostream &s, CgenClassTable *ct) {
  for (auto entry : tbl) {
    entry->code_def(s, ct);
  }
}

//
// Sets up declarations for extra functions needed for code generation
// You should not need to modify this code for MP2.1
//
void CgenClassTable::setup_external_functions() {
  ValuePrinter vp;
  op_type Obj("Object", 1), Str("String", 1), IO("IO", 1), Int("Int", 1), Bool("Bool", 1);
  // setup function: external int strcmp(sbyte*, sbyte*)
  // setup function: external int printf(sbyte*, ...)
  // setup function: external void abort(void)
  // setup function: external i8* malloc(i32)
  vp.declare(*ct_stream, INT32, "strcmp", {INT8_PTR, INT8_PTR});
  vp.declare(*ct_stream, INT32, "printf", {INT8_PTR, VAR_ARG});
  vp.declare(*ct_stream, VOID, "abort", {});
  vp.declare(*ct_stream, INT8_PTR, "malloc", {INT32});

#ifdef MP3
  // Setup external functions for built in object class functions

  vp.declare(*ct_stream, Obj, "Object_new", {});
  vp.declare(*ct_stream, Obj, "Object_abort", {Obj});
  vp.declare(*ct_stream, Str, "Object_type_name", {Obj});
  vp.declare(*ct_stream, Obj, "Object_copy", {Obj});
  vp.declare(*ct_stream, IO, "IO_new", {});
  vp.declare(*ct_stream, IO, "IO_out_string", {IO, Str});
  vp.declare(*ct_stream, IO, "IO_out_int", {IO, INT32});
  vp.declare(*ct_stream, Str, "IO_in_string", {IO});
  vp.declare(*ct_stream, INT32, "IO_in_int", {IO});
  vp.declare(*ct_stream, Str, "String_new", {});
  vp.declare(*ct_stream, INT32, "String_length", {Str});
  vp.declare(*ct_stream, Str, "String_concat", {Str, Str});
  vp.declare(*ct_stream, Str, "String_substr", {Str, INT32, INT32});
  vp.declare(*ct_stream, Int, "Int_new", {});
  vp.declare(*ct_stream, VOID, "Int_init", {Int, INT32});
  vp.declare(*ct_stream, Bool, "Bool_new", {});
  vp.declare(*ct_stream, VOID, "Bool_init", {Bool, INT1});

#endif
}

// Creates AST nodes for the basic classes and installs them in the class list
void CgenClassTable::install_basic_classes() {
  // The tree package uses these globals to annotate the classes built below.
  curr_lineno = 0;
  Symbol filename = stringtable.add_string("<basic class>");

  //
  // A few special class names are installed in the lookup table but not
  // the class list. Thus, these classes exist, but are not part of the
  // inheritance hierarchy.

  // No_class serves as the parent of Object and the other special classes.
  Class_ noclasscls = class_(No_class, No_class, nil_Features(), filename);
  install_special_class(new CgenNode(noclasscls, CgenNode::Basic, this));
  delete noclasscls;

#ifdef MP3
  // SELF_TYPE is the self class; it cannot be redefined or inherited.
  Class_ selftypecls = class_(SELF_TYPE, No_class, nil_Features(), filename);
  install_special_class(new CgenNode(selftypecls, CgenNode::Basic, this));
  delete selftypecls;
  //
  // Primitive types masquerading as classes. This is done so we can
  // get the necessary Symbols for the innards of String, Int, and Bool
  //
  Class_ primstringcls =
      class_(prim_string, No_class, nil_Features(), filename);
  install_special_class(new CgenNode(primstringcls, CgenNode::Basic, this));
  delete primstringcls;
#endif
  Class_ primintcls = class_(prim_int, No_class, nil_Features(), filename);
  install_special_class(new CgenNode(primintcls, CgenNode::Basic, this));
  delete primintcls;
  Class_ primboolcls = class_(prim_bool, No_class, nil_Features(), filename);
  install_special_class(new CgenNode(primboolcls, CgenNode::Basic, this));
  delete primboolcls;
  //
  // The Object class has no parent class. Its methods are
  //        cool_abort() : Object   aborts the program
  //        type_name() : Str       returns a string representation of class
  //        name copy() : SELF_TYPE      returns a copy of the object
  //
  // There is no need for method bodies in the basic classes---these
  // are already built in to the runtime system.
  //
  Class_ objcls = class_(
      Object, No_class,
      append_Features(
          append_Features(single_Features(method(cool_abort, nil_Formals(),
                                                 Object, no_expr())),
                          single_Features(method(type_name, nil_Formals(),
                                                 String, no_expr()))),
          single_Features(
              method(cool_copy, nil_Formals(), SELF_TYPE, no_expr()))),
      filename);
  install_class(new CgenNode(objcls, CgenNode::Basic, this));
  delete objcls;

  //
  // The Int class has no methods and only a single attribute, the
  // "val" for the integer.
  //
  Class_ intcls = class_(
      Int, Object, single_Features(attr(val, prim_int, no_expr())), filename);
  install_class(new CgenNode(intcls, CgenNode::Basic, this));
  delete intcls;

  //
  // Bool also has only the "val" slot.
  //
  Class_ boolcls = class_(
      Bool, Object, single_Features(attr(val, prim_bool, no_expr())), filename);
  install_class(new CgenNode(boolcls, CgenNode::Basic, this));
  delete boolcls;

#ifdef MP3
  //
  // The class String has a number of slots and operations:
  //       val                                  the string itself
  //       length() : Int                       length of the string
  //       concat(arg: Str) : Str               string concatenation
  //       substr(arg: Int, arg2: Int): Str     substring
  //
  Class_ stringcls =
      class_(String, Object,
             append_Features(
                 append_Features(
                     append_Features(
                         single_Features(attr(val, prim_string, no_expr())),
                         single_Features(
                             method(length, nil_Formals(), Int, no_expr()))),
                     single_Features(method(concat,
                                            single_Formals(formal(arg, String)),
                                            String, no_expr()))),
                 single_Features(
                     method(substr,
                            append_Formals(single_Formals(formal(arg, Int)),
                                           single_Formals(formal(arg2, Int))),
                            String, no_expr()))),
             filename);
  install_class(new CgenNode(stringcls, CgenNode::Basic, this));
  delete stringcls;
#endif

#ifdef MP3
  //
  // The IO class inherits from Object. Its methods are
  //        out_string(Str) : SELF_TYPE          writes a string to the output
  //        out_int(Int) : SELF_TYPE               "    an int    "  "     "
  //        in_string() : Str                    reads a string from the input
  //        in_int() : Int                         "   an int     "  "     "
  //
  Class_ iocls = class_(
      IO, Object,
      append_Features(
          append_Features(
              append_Features(
                  single_Features(method(out_string,
                                         single_Formals(formal(arg, String)),
                                         SELF_TYPE, no_expr())),
                  single_Features(method(out_int,
                                         single_Formals(formal(arg, Int)),
                                         SELF_TYPE, no_expr()))),
              single_Features(
                  method(in_string, nil_Formals(), String, no_expr()))),
          single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
      filename);
  install_class(new CgenNode(iocls, CgenNode::Basic, this));
  delete iocls;
#endif
}

//
// install_classes enters a list of classes in the symbol table.
//
void CgenClassTable::install_classes(Classes cs) {
  for (auto cls : cs) {
    install_class(new CgenNode(cls, CgenNode::NotBasic, this));
  }
}

//
// Add this CgenNode to the class list and the lookup table
//
void CgenClassTable::install_class(CgenNode *nd) {
  Symbol name = nd->get_name();

  if (probe(name))
    return;

  // The class name is legal, so add it to the list of classes
  // and the symbol table.
  nds = new List<CgenNode>(nd, nds);
  addid(name, nd);
}

//
// Add this CgenNode to the special class list and the lookup table
//
void CgenClassTable::install_special_class(CgenNode *nd) {
  Symbol name = nd->get_name();

  if (probe(name))
    return;

  // The class name is legal, so add it to the list of special classes
  // and the symbol table.
  special_nds = new List<CgenNode>(nd, special_nds);
  addid(name, nd);
}

//
// CgenClassTable::build_inheritance_tree
//
void CgenClassTable::build_inheritance_tree() {
  for (auto node : nds)
    set_relations(node);
}

//
// CgenClassTable::set_relations
//
// Takes a CgenNode and locates its, and its parent's, inheritance nodes
// via the class table.  Parent and child pointers are added as appropriate.
//
void CgenClassTable::set_relations(CgenNode *nd) {
  CgenNode *parent_node = probe(nd->get_parent());
  nd->set_parentnd(parent_node);
  parent_node->add_child(nd);
}

// Get the root of the class tree.
CgenNode *CgenClassTable::root() { return probe(Object); }

//////////////////////////////////////////////////////////////////////
//
// Special-case functions used for the method Int Main::main() for
// MP2 only.
//
//////////////////////////////////////////////////////////////////////

#ifndef MP3

CgenNode *CgenClassTable::getMainmain(CgenNode *c) {
  if (c && !c->basic())
    return c;  // Found it!

  for (auto child : c->get_children()) {
    if (CgenNode *foundMain = this->getMainmain(child))
      return foundMain;  // Propagate it up the recursive calls
  }

  return 0;  // Make the recursion continue
}

#endif

//-------------------------------------------------------------------
//
// END OF PREDEFINED FUNCTIONS
//
//-------------------------------------------------------------------

///////////////////////////////////////////////////////////////////////////////
//
// coding string, int, and boolean constants
//
// Cool has three kinds of constants: strings, ints, and booleans.
// This section defines code generation for each type.
//
// All string constants are listed in the global "stringtable" and have
// type stringEntry.  stringEntry methods are defined both for string
// constant definitions and references.
//
// All integer constants are listed in the global "inttable" and have
// type IntEntry.  IntEntry methods are defined for Int
// constant definitions and references.
//
// Since there are only two Bool values, there is no need for a table.
// The two booleans are represented by instances of the class BoolConst,
// which defines the definition and reference methods for Bools.
//
///////////////////////////////////////////////////////////////////////////////

//
// Create global definitions for constant Cool objects
//
void CgenClassTable::code_constants() {
#ifdef MP3

  stringtable.code_string_table(*ct_stream, this);

#endif
}

// generate code to define a global string constant
void StringEntry::code_def(ostream &s, CgenClassTable *ct) {
  static int str_ctr = 0;
#ifdef MP3
  ValuePrinter vp(s);
  string str = this->get_string();
  string llvm_name = "str." + std::to_string(index);
  string llvm_name_String = "String." + std::to_string(index);
  ct->const_str[this] = llvm_name_String;
  op_arr_type str_type{INT8, (int)str.length() + 1};
  vp.init_constant(llvm_name, {str_type, str, true});

  vector<op_type> types({op_type("_String_vtable", 1), INT8_PTR});
  vp.init_struct_constant(
      global_value(op_type("String"), llvm_name_String),
      types,
      {const_value(types[0], "@_String_vtable_prototype", false),
       const_value(types[1], "getelementptr (" + str_type.get_name() + ", " + str_type.get_ptr_type_name() + " @" + llvm_name + ", i32 0, i32 0)", false)});
#endif
}

// generate code to define a global int constant
void IntEntry::code_def(ostream &s, CgenClassTable *ct) {
  // Leave this method blank, since we are not going to use global
  // declarations for int constants.
}

//////////////////////////////////////////////////////////////////////////////
//
//  CgenClassTable methods
//
//////////////////////////////////////////////////////////////////////////////

//
// CgenClassTable constructor orchestrates all code generation
//
CgenClassTable::CgenClassTable(Classes classes, ostream &s) : nds(0) {
  if (cgen_debug)
    std::cerr << "Building CgenClassTable" << endl;
  ct_stream = &s;
  // Make sure we have a scope, both for classes and for constants
  enterscope();
  //  Create an inheritance tree with one CgenNode per class.
  install_basic_classes();
  install_classes(classes);
  build_inheritance_tree();

  // First pass
  setup();

  // Second pass
  code_module();
  // Done with code generation: exit scopes
  exitscope();
}

CgenClassTable::~CgenClassTable() {}

// The code generation first pass.  Define these two functions to traverse
// the tree and setup each CgenNode
void CgenClassTable::setup() {
  setup_external_functions();
  setup_classes(root(), 0);
}

void CgenClassTable::setup_classes(CgenNode *c, int depth) {
  // MAY ADD CODE HERE
  // if you want to give classes more setup information

  c->setup(current_tag++, depth);
  for (auto child : c->get_children())
    setup_classes(child, depth + 1);

  c->set_max_child(current_tag - 1);

  if (cgen_debug)
    std::cerr << "Class " << c->get_name() << " assigned tag "
              << c->get_tag() << ", max child " << c->get_max_child()
              << endl;
}

// The code generation second pass. Add code here to traverse the tree and
// emit code for each CgenNode
void CgenClassTable::code_module() {
  code_constants();

#ifndef MP3
  // This must be after code_module() since that emits constants
  // needed by the code() method for expressions
  CgenNode *mainNode = getMainmain(root());
  mainNode->codeGenMainmain();
#endif
  code_main();

#ifdef MP3
  code_classes(root());
#else
#endif
}

#ifdef MP3
void CgenClassTable::code_classes(CgenNode *c) {
  c->code_class();
  for (auto f : c->get_children())
    code_classes(f);
}
#endif

//
// Create LLVM entry point. This function will initiate our Cool program
// by generating the code to execute (new Main).main()
//
void CgenClassTable::code_main() {
  ValuePrinter vp(*ct_stream);
#ifndef MP3
  op_type i32_type(INT32), i8ptr_type(INT8_PTR), vararg_type(VAR_ARG), i8_type(INT8);

  op_arr_type str_const(op_type_id::INT8, 25);
  op_arr_ptr_type str_const_ptr(op_type_id::INT8, 25);
  vp.init_constant("main.printout.str", {str_const, "Main.main() returned %d\n", true});
  // Define a function main that has no parameters and returns an i32
  vector<operand> main_args;
  vector<op_type> main_args_types;
  vp.define(i32_type, "main", main_args);
  // Define an entry basic block
  vp.begin_block("entry");
  // Call Main_main(). This returns int* for phase 1, Object for phase 2
  auto ret1 = vp.call(main_args_types, i32_type, "Main.main", 1, main_args);
  // Get the address of the string "Main_main() returned %d\n" using
  // getelementptr

  vp.getelementptr(*ct_stream, str_const, global_value(str_const_ptr, "main.printout.str"), int_value{0}, int_value{0}, operand(i8ptr_type, "tpm"));
  // Call printf with the string address of "Main_main() returned %d\n"
  // and the return value of Main_main() as its arguments
  vector<op_type> printf_args{i8ptr_type, vararg_type};
  vp.call(printf_args, i32_type, "printf", true, {operand(i8ptr_type, "tpm"), ret1});
  // Insert return 0
  vp.ret(int_value{0});
  vp.end_define();
#else
  auto mn = lookup(Main);
  // CgenNode *mainNode = getMainmain(root());
  // auto ww = mn->get_method(stringtable.add_string("main")).ret_type;
  for (auto i : mn->method_layout) {
    if (!strcmp(i.first->get_string(), "main")) {
      *ct_stream << "define i32 @main() {\nentry:\n\t%main.obj = call %Main*() @Main_new( )\n\t%main.retval = call " + mn->sym2type(i.second.ret_type).get_name() + "(%Main*) @Main.main( %Main* %main.obj )\n\tret i32 0\n}\n\n";
    }
  }
#endif
}

///////////////////////////////////////////////////////////////////////
//
// CgenNode methods
//
///////////////////////////////////////////////////////////////////////

CgenNode::CgenNode(Class_ nd, Basicness bstatus, CgenClassTable *ct)
    : class__class((const class__class &)*nd), parentnd(0), children(0), basic_status(bstatus), class_table(ct), tag(-1), attr_ctr(0), method_ctr(0) {
  // ADD CODE HERE
}

void CgenNode::add_child(CgenNode *n) {
  children = new List<CgenNode>(n, children);
}

void CgenNode::set_parentnd(CgenNode *p) {
  assert(parentnd == NULL);
  assert(p != NULL);
  parentnd = p;
}

//
// Class setup.  You may need to add parameters to this function so that
// the classtable can provide setup information (such as the class tag
// that should be used by this class).
//
// Things that setup should do:
//  - layout the features of the class
//  - create the types for the class and its vtable
//  - create global definitions used by the class such as the class vtable
//

op_type sym2type(Symbol s, Symbol cur_class) {
  if (s == prim_int) return op_type(INT32);
  if (s == prim_bool) return op_type(INT1);
  if (s == prim_string) return op_type(INT8_PTR);
  if (s == Int && s != cur_class)
    return op_type(INT32);
  if (s == Bool && s != cur_class)
    return op_type(INT1);
  if (s == SELF_TYPE)
    return op_type(cur_class->get_string(), 1);
  else
    return op_type(s->get_string(), 1);
}
op_type CgenNode::sym2type(Symbol s) {
  return ::sym2type(s, name);
}

void CgenNode::setup(int tag, int depth) {
  this->tag = tag;
#ifdef MP3
  if (cgen_debug)
    std::cerr << "setting: " << name << std::endl;
  layout_features();
  ValuePrinter vp(*class_table->ct_stream);
  string name_(name->get_string());
  op_type self_type(name_, 1);
  op_arr_type str_type{INT8, (int)name_.length() + 1};
  // str
  vp.init_constant("str." + name_, {str_type, name_, true});

  // attr
  vector<attribute> attrs(attr_ctr);
  for (auto k : attr_layout)
    attrs[k.second.index] = k.second;

  vector<op_type> attr;
  attr.push_back(op_type("_" + name_ + "_vtable", 1));
  for (auto k : attrs) {
    attr.push_back(sym2type(k.ret_type));
  }
  vp.type_define(name_, attr);

  // vtable+init
  vector<signature> signs(method_ctr);
  for (auto k : method_layout)
    signs[k.second.index] = k.second;

  vector<op_type>
      method{INT32, INT32, INT8_PTR, op_func_type{self_type, {}}};
  vector<const_value>
      init_val{
          int_value(tag),
          const_value{method[1], "ptrtoint (%" + name_ + "* getelementptr (%" + name_ + ", %" + name_ + "* null, i32 1) to i32)", false},
          const_value{method[2], "getelementptr (" + str_type.get_name() + ", " + str_type.get_ptr_type_name() + " @str." + name_ + ", i32 0, i32 0)", false},
          const_value{method[3], "@" + name_ + "_new", false}};

  for (auto k : signs) {
    vector<op_type> args, from_arg;
    auto &item = k;
    args.push_back(self_type);
    from_arg.push_back(::sym2type(item.from, item.from));
    for (auto kk : item.args) {
      cerr << kk << endl;
      args.push_back(sym2type(kk));
      from_arg.push_back(::sym2type(kk, item.from));
    }
    op_func_type full_ret_type(sym2type(item.ret_type), args);
    op_func_type from_ret_type(::sym2type(item.ret_type, item.from), from_arg);
    method.push_back(full_ret_type);
    if (item.from == name)
      init_val.push_back(const_value{
          full_ret_type,
          item.get_signature(), false});
    else
      init_val.push_back(casted_value{full_ret_type,
                                      item.get_signature(), from_ret_type});
  }
  vp.type_define("_" + name_ + "_vtable", method);

  vp.init_struct_constant(
      global_value(op_type("_" + name_ + "_vtable"), "_" + name_ + "_vtable_prototype"), method, init_val);
  this->attr = attr;
  this->method = method;
  this->init_val = init_val;
#endif
}

#ifdef MP3
//
// Class codegen. This should performed after every class has been setup.
// Generate code for each method of the class.
//
void CgenNode::code_class() {
  // No code generation for basic classes. The runtime will handle that.
  if (basic())
    return;
  CgenEnvironment *env = new CgenEnvironment(*class_table->ct_stream, this);
  for (auto i : features) {
    i->code(env);
  }
  code_new_object(env);
}

operand type2initval(op_type op, CgenEnvironment *env) {
  // FIXME: type2initval: possible other case
  if (op.get_id() == INT32)
    return int_value(0);
  if (op.get_id() == INT1)
    return bool_value(false, 1);
  if (op.is_string_object()) {
    auto &o = *env->cur_stream;
    ValuePrinter vp(o);
    string name_(op.get_name().substr(1, op.get_name().length() - 2));
    operand new_obj(op, env->new_name());
    vp.call(o, {}, name_ + "_new", true, {}, new_obj);
    return new_obj;
  }
  assert(op.is_ptr());
  return null_value({op});
}
// operand type2nullval(op_type op) {
//   // FIXME: type2initval: possible other case
//   if (op.get_id() == INT32)
//     return int_value(0);
//   if (op.get_id() == INT1)
//     return bool_value(false, 1);
//   return null_value({op});
// }

void CgenNode::code_new_object(CgenEnvironment *env) {
  ValuePrinter vp(*env->cur_stream);
  Symbol cur_class = get_name();
  string name_ = string(cur_class->get_string());
  op_type obj_ptr = op_type("_" + name_ + "_vtable", 1);

  vp.define(sym2type(SELF_TYPE), name_ + "_new", {});
  vp.begin_block("entry");
  operand obj_local(sym2type(SELF_TYPE).get_ptr_type(), env->new_name());
  env->add_local(self, obj_local);
  vp.alloca_mem(*env->cur_stream, sym2type(SELF_TYPE), obj_local);

  vector<attribute> attrs(attr_ctr);
  for (auto a : attr_layout) {
    attrs[a.second.index] = a.second;
    env->control = a.second.name;
    auto w = env->type_to_class(a.second.from);
    for (auto i : w->features) {
      auto ptr = dynamic_cast<attr_class *>(i);
      if (ptr != nullptr) {
        ptr->make_alloca(env);
      }
    }
  }

  const_value vtb(obj_ptr, "@_" + name_ + "_vtable_prototype", true);
  operand obj_size_ptr(INT32_PTR, env->new_name());
  vp.getelementptr(*env->cur_stream, obj_ptr.get_deref_type(), vtb, int_value(0), int_value(1), obj_size_ptr);
  operand obj_size(INT32, env->new_name());
  *env->cur_stream << "\t" + obj_size.get_name() + " = load i32, i32* " << obj_size_ptr.get_name() << endl;
  operand malloc_res(INT32, env->new_name());
  *env->cur_stream << "\t" + malloc_res.get_name() + " = call i8*(i32) @malloc( i32 " + obj_size.get_name() + " )" << endl;
  operand obj_heap(sym2type(SELF_TYPE), env->new_name());
  *env->cur_stream << "\t" + obj_heap.get_name() + " = bitcast i8* " + malloc_res.get_name() + " to %" + name_ + "*" << endl;
  *env->cur_stream << "\t%malloc.null = icmp eq %" + name_ + "* " + obj_heap.get_name() + ", null" << endl;
  *env->cur_stream << "\tbr i1 %malloc.null, label %abort, label %okay" << endl;

  vp.begin_block("okay");
  operand vtable_ptr(obj_ptr.get_ptr_type(), env->new_name());
  *env->cur_stream << "\t" + vtable_ptr.get_name() + " = getelementptr %" + name_ + ", %" + name_ + "* " + obj_heap.get_name() + ", i32 0, i32 0" << endl;
  *env->cur_stream << "\tstore %_" + name_ + "_vtable* @_" + name_ + "_vtable_prototype, %_" + name_ + "_vtable** " + vtable_ptr.get_name() << endl;
  *env->cur_stream << "\tstore %" + name_ + "* " + obj_heap.get_name() + ", %" + name_ + "** " + obj_local.get_name() << endl;

  for (auto a : attrs) {
    operand tmp(sym2type(a.ret_type).get_ptr_type(), env->new_name());
    vp.getelementptr(*env->cur_stream, obj_heap.get_type().get_deref_type(), obj_heap, int_value(0), int_value(a.index + 1), tmp);
    vp.store(type2initval(sym2type(a.ret_type), env), tmp);
  }
  for (auto a : attrs) {
    operand tmp(sym2type(a.ret_type).get_ptr_type(), env->new_name());
    vp.getelementptr(*env->cur_stream, obj_heap.get_type().get_deref_type(), obj_heap, int_value(0), int_value(a.index + 1), tmp);
    env->control = a.name;
    auto w = env->type_to_class(a.from);
    for (auto i : w->features) {
      auto ptr = dynamic_cast<attr_class *>(i);
      if (ptr != nullptr) {
        ptr->code(env);
      }
    }
    
    vp.store(conform(env->res, tmp.get_type().get_deref_type(), env), tmp);
  }

  vp.ret(obj_heap);

  vp.begin_block("abort");
  vp.call(*env->cur_stream, {}, "abort", true, {}, {VOID, ""});
  vp.unreachable();
  vp.end_define();
}

// Laying out the features involves creating a Function for each method
// and assigning each attribute a slot in the class structure.
void CgenNode::layout_features() {
  if (parentnd) {
    method_layout = parentnd->method_layout;  // cast
    attr_layout = parentnd->attr_layout;      // cast
    method_ctr = parentnd->method_ctr;
    attr_ctr = parentnd->attr_ctr;
  }
  for (auto f : features) {
    f->layout_feature(this);
  }
}
#else

//
// code-gen function main() in class Main
//
void CgenNode::codeGenMainmain() {
  ValuePrinter vp;
  // In Phase 1, this can only be class Main. Get method_class for main().
  assert(std::string(this->name->get_string()) == std::string("Main"));
  method_class *mainMethod = (method_class *)features->nth(features->first());

  // ADD CODE HERE TO GENERATE THE FUNCTION int Mainmain().
  // Generally what you need to do are:
  // -- setup or create the environment, env, for translating this method
  // -- invoke mainMethod->code(env) to translate the method

  CgenEnvironment *env = new CgenEnvironment(*class_table->ct_stream, this);
  mainMethod->code(env);
}

#endif

//
// CgenEnvironment functions
//

//
// Class CgenEnvironment should be constructed by a class prior to code
// generation for each method.  You may need to add parameters to this
// constructor.
//
CgenEnvironment::CgenEnvironment(std::ostream &o, CgenNode *c) {
  cur_class = c;
  cur_stream = &o;
  var_table.enterscope();
  tmp_count = block_count = ok_count = 0;
  // ADD CODE HERE
}

// Look up a CgenNode given a symbol
CgenNode *CgenEnvironment::type_to_class(Symbol t) {
  return t == SELF_TYPE ? get_class()
                        : get_class()->get_classtable()->lookup(t);
}

// Provided CgenEnvironment methods
// Generate unique string names
std::string CgenEnvironment::new_name() {
  std::stringstream s;
  s << tmp_count++;
  return "tmp." + s.str();
}

std::string CgenEnvironment::new_ok_label() {
  std::stringstream s;
  s << ok_count++;
  return "ok." + s.str();
}
const std::string CgenEnvironment::new_label(const std::string &prefix,
                                             bool increment) {
  std::string suffix = itos(block_count);
  block_count += increment;
  return prefix + suffix;
}

void CgenEnvironment::add_local(Symbol name, operand &vb) {
  var_table.enterscope();
  var_table.addid(name, &vb);
}

void CgenEnvironment::kill_local() { var_table.exitscope(); }

////////////////////////////////////////////////////////////////////////////
//
// APS class methods
//
////////////////////////////////////////////////////////////////////////////

//******************************************************************
//
//   Fill in the following methods to produce code for the
//   appropriate expression.  You may add or remove parameters
//   as you wish, but if you do, remember to change the parameters
//   of the declarations in `cool-tree.handcode.h'.
//
//*****************************************************************

#ifdef MP3
// conform and get_class_tag are only needed for MP3

// conform - If necessary, emit a bitcast or boxing/unboxing operations
// to convert an object to a new type. This can assume the object
// is known to be (dynamically) compatible with the target type.
// It should only be called when this condition holds.
// (It's needed by the supplied code for typecase)
inline bool is_bool_int(op_type op) {
  // replace?
  return op.get_id() == INT32 || op.get_id() == INT1;
}
inline bool is_basic(op_type op) {
  return op.get_id() == INT32 || op.get_id() == INT1 || op.is_string_object();
}
inline string boxing(op_type op) {
  if (op.get_id() == INT32) return "Int";
  if (op.get_id() == INT1) return "Bool";
  assert(false);
}
operand conform(operand src, op_type type, CgenEnvironment *env) {
  // ADD CODE HERE (MP3 ONLY)
  auto &o = *env->cur_stream;
  ValuePrinter vp(o);
  if (src.get_type().get_id() == VOID) {
    cerr << "conform: void to init val" << endl;
    return type2initval(type, env);
  }
  // Symbol src_ = env->type2sym(src.get_type()), dst_ = env->type2sym(type);
  // src_:A*,dst_:Main*
  // cerr << ";conform:" << src_ << "(" + src.get_type().get_name() + ") -> " << dst_ << "(" + type.get_name() + ")"
  //      << ": ";
  if (src.get_type().is_same_with(type)) {
    cerr << "identical" << endl;
    return src;
  }
  if (is_bool_int(type)) {
    // FIXME: conform unboxing
    // %Bool/Int -> INT1/INT32
    cerr << "unboxing" << endl;
    operand ret(type, env->new_name());
    operand val_ptr(type.get_ptr_type(), env->new_name());
    vp.getelementptr(o, src.get_type().get_deref_type(), src, int_value(0), int_value(1), val_ptr);
    vp.load(o, type, val_ptr, ret);
    return ret;
  }
  // INT1/INT32 -> INT/BOOL/OBJ
  if (is_bool_int(src.get_type())) {
    // FIXME:
    cerr << "boxing" << endl;
    auto make_em_happy = env->new_name();
    auto name_ = boxing(src.get_type());
    op_type boxing_type(name_, 1);
    operand new_obj_ptr(boxing_type, env->new_name());
    vp.call(o, {}, name_ + "_new", true, {}, new_obj_ptr);
    vp.call(o, {new_obj_ptr.get_type(), src.get_type()}, name_ + "_init", true, {new_obj_ptr, src}, {VOID, ""});

    return conform(new_obj_ptr, type, env);
  }
  cerr << "unconditional bitcast" << endl;
  operand casted(type, env->new_name());
  vp.bitcast(o, src, type, casted);
  return casted;
}

// Retrieve the class tag from an object record.
// src is the object we need the tag from.
// src_class is the CgenNode for the *static* class of the expression.
// You need to look up and return the class tag for it's dynamic value
operand get_class_tag(operand src, CgenNode *src_cls, CgenEnvironment *env) {
  // ADD CODE HERE (MP3 ONLY)
  auto &o = *env->cur_stream;
  ValuePrinter vp(o);
  string name_(src_cls->get_name()->get_string());
  string vtable_("_" + name_ + "_vtable");
  cerr << "get_tag: " << src.get_name() << " by class " << src_cls->get_name() << endl;
  operand vtable_ptr_ptr({vtable_, 2}, env->new_name());
  vp.getelementptr(o, src.get_type().get_deref_type(), src, int_value(0), int_value{0}, vtable_ptr_ptr);
  operand vtable_ptr({vtable_, 1}, env->new_name());
  vp.load(o, vtable_ptr.get_type(), vtable_ptr_ptr, vtable_ptr);
  operand tag_ptr(INT32_PTR, env->new_name());
  vp.getelementptr(o, vtable_ptr.get_type().get_deref_type(), vtable_ptr, int_value(0), int_value{0}, tag_ptr);
  operand tag(INT32, env->new_name());
  vp.load(o, tag.get_type(), tag_ptr, tag);
  return tag;
}
#endif

//
// Create a method body
//
void method_class::code(CgenEnvironment *env) {
  if (cgen_debug)
    std::cerr << "method" << endl;
  ValuePrinter vp(*env->cur_stream);
  Symbol cur_class = env->get_class()->get_name();
  vector<op_type> method_args_type{sym2type(SELF_TYPE, cur_class)};
  vector<operand> method_args_op{operand(method_args_type[0].get_ptr_type(), env->new_name())};
  vector<operand> method_args_name{operand(method_args_type[0], "self")};
  vector<Symbol> method_args{self};
  for (auto i : formals) {
    auto type = sym2type(i->get_type_decl(), cur_class);
    auto op = operand(type.get_ptr_type(), env->new_name());
    auto op_name = operand(type, i->get_name()->get_string());
    method_args_type.push_back(type);
    method_args_op.push_back(op);
    method_args_name.push_back(op_name);
    method_args.push_back(i->get_name());
  }
  for (size_t i = 0; i != method_args.size(); i++) {
    env->add_local(method_args[i], method_args_op[i]);
  }
  vp.define(sym2type(return_type, cur_class), string(cur_class->get_string()) + "." + string(name->get_string()), method_args_name);
  vp.begin_block("entry");

  expr->make_alloca(env);
  for (size_t i = 0; i != method_args.size(); i++) {
    vp.alloca_mem(*env->cur_stream, method_args_type[i], method_args_op[i]);
  }
  for (size_t i = 0; i != method_args.size(); i++) {
    vp.store({method_args_type[i], method_args[i]->get_string()}, method_args_op[i]);
  }
  auto ret = expr->code(env);
  vp.ret(conform(ret, env->get_class()->sym2type(return_type), env));
  vp.begin_block("abort");
  vp.call(*env->cur_stream, {}, "abort", true, {}, {VOID, ""});
  vp.unreachable();
  vp.end_define();
  for (size_t i = 0; i != method_args.size(); i++) {
    env->kill_local();
  }
}

//
// Codegen for expressions.  Note that each expression has a value.
//

operand assign_class::code(CgenEnvironment *env) {
  if (cgen_debug)
    std::cerr << "assign" << endl;
  auto ret = expr->code(env);
  ValuePrinter vp(*env->cur_stream);
  if (env->lookup(name) == NULL) {  // Attr
    assert(env->is_attr(name));
    auto attr = env->get_attr(name);
    auto alloca_op = *env->lookup(self);
    operand tmp_op1(alloca_op.get_type().get_deref_type(), env->new_name()),
        tmp_op2(env->get_class()->sym2type(attr.ret_type).get_ptr_type(), env->new_name());
    // ret(env->get_class()->sym2type(attr.ret_type), env->new_name());
    vp.load(*env->cur_stream, alloca_op.get_type().get_deref_type(), alloca_op, tmp_op1);
    vp.getelementptr(*env->cur_stream, tmp_op1.get_type().get_deref_type(), tmp_op1, int_value{0}, int_value{attr.index + 1}, tmp_op2);
    // vp.load(*env->cur_stream, tmp_op2.get_type().get_deref_type(), tmp_op2, ret);
    vp.store(conform(ret, tmp_op2.get_type().get_deref_type(), env), tmp_op2);
    return ret;
  } else {  // Local
    auto lvalue = env->lookup(name);
    vp.store(conform(ret, lvalue->get_type().get_deref_type(), env), *lvalue);
    return ret;
  }
}

operand cond_class::code(CgenEnvironment *env) {
  if (cgen_debug)
    std::cerr << "cond" << endl;
  ValuePrinter vp(*env->cur_stream);
  auto label_true = env->new_label("true.", false),
       label_false = env->new_label("false.", false),
       label_end = env->new_label("end.", true);
  auto pred_ = pred->code(env);
  vp.branch_cond(*env->cur_stream, conform(pred_, INT1, env), label_true, label_false);
  vp.begin_block(label_true);
  auto ret_then = then_exp->code(env);
  vp.store(conform(ret_then, alloca_op.get_type().get_deref_type(), env), alloca_op);
  vp.branch_uncond(label_end);
  vp.begin_block(label_false);
  auto ret_else = else_exp->code(env);
  vp.store(conform(ret_else, alloca_op.get_type().get_deref_type(), env), alloca_op);
  vp.branch_uncond(label_end);
  vp.begin_block(label_end);

  operand ret(alloca_op.get_type().get_deref_type(), env->new_name());
  vp.load(*env->cur_stream, alloca_op.get_type().get_deref_type(), alloca_op, ret);
  return ret;
}

operand loop_class::code(CgenEnvironment *env) {
  if (cgen_debug)
    std::cerr << "loop" << endl;
  ValuePrinter vp(*env->cur_stream);
  auto label_true = env->new_label("true.", false),
       label_false = env->new_label("false.", false),
       label_loop = env->new_label("loop.", true);
  vp.branch_uncond(label_loop);
  vp.begin_block(label_loop);
  auto pred_ = pred->code(env);
  vp.branch_cond(*env->cur_stream, pred_, label_true, label_false);
  vp.begin_block(label_true);
  auto body_ = body->code(env);
  vp.branch_uncond(label_loop);
  vp.begin_block(label_false);
  return operand(VOID, "");
}

operand block_class::code(CgenEnvironment *env) {
  if (cgen_debug)
    std::cerr << "block" << endl;
  operand ret;
  for (auto p : body) {
    ret = p->code(env);
  }
  return ret;
}

operand let_class::code(CgenEnvironment *env) {
  if (cgen_debug)
    std::cerr << "let" << endl;
  ValuePrinter vp(*env->cur_stream);
  auto rvalue = init->code(env);
  if (rvalue.is_empty()) {  // no_expr
    auto type = env->get_class()->sym2type(type_decl);
    vp.store(type2initval(type, env), alloca_op);
  } else {
    vp.store(conform(rvalue, alloca_op.get_type().get_deref_type(), env), alloca_op);
  }
  env->add_local(identifier, this->alloca_op);
  auto ret = body->code(env);
  env->kill_local();
  return ret;
}

operand plus_class::code(CgenEnvironment *env) {
  if (cgen_debug)
    std::cerr << "plus" << endl;
  ValuePrinter vp(*env->cur_stream);
  auto e1_ = conform(e1->code(env), INT32, env), e2_ = conform(e2->code(env), INT32, env);
  operand ret(INT32, env->new_name());
  vp.add(*env->cur_stream, e1_, e2_, ret);
  return ret;
}

operand sub_class::code(CgenEnvironment *env) {
  if (cgen_debug)
    std::cerr << "sub" << endl;
  ValuePrinter vp(*env->cur_stream);
  auto e1_ = conform(e1->code(env), INT32, env), e2_ = conform(e2->code(env), INT32, env);
  operand ret(INT32, env->new_name());
  vp.sub(*env->cur_stream, e1_, e2_, ret);
  return ret;
}

operand mul_class::code(CgenEnvironment *env) {
  if (cgen_debug)
    std::cerr << "mul" << endl;
  ValuePrinter vp(*env->cur_stream);
  auto e1_ = conform(e1->code(env), INT32, env), e2_ = conform(e2->code(env), INT32, env);
  operand ret(INT32, env->new_name());
  vp.mul(*env->cur_stream, e1_, e2_, ret);
  return ret;
}

operand divide_class::code(CgenEnvironment *env) {
  if (cgen_debug)
    std::cerr << "div" << endl;
  ValuePrinter vp(*env->cur_stream);
  auto e1_ = conform(e1->code(env), INT32, env), e2_ = conform(e2->code(env), INT32, env);
  operand cmp(INT1, env->new_name());
  vp.icmp(*env->cur_stream, EQ, e2_, int_value(0), cmp);
  auto ok = env->new_ok_label();
  vp.branch_cond(cmp, "abort", ok);
  vp.begin_block(ok);
  operand ret(INT32, env->new_name());
  vp.div(*env->cur_stream, e1_, e2_, ret);
  return ret;
}

operand neg_class::code(CgenEnvironment *env) {
  if (cgen_debug)
    std::cerr << "neg" << endl;
  ValuePrinter vp(*env->cur_stream);
  auto e1_ = conform(e1->code(env), INT32, env);
  operand ret(INT32, env->new_name());
  vp.sub(*env->cur_stream, int_value(0), e1_, ret);
  return ret;
}

operand lt_class::code(CgenEnvironment *env) {
  if (cgen_debug)
    std::cerr << "lt" << endl;
  ValuePrinter vp(*env->cur_stream);
  auto e1_ = conform(e1->code(env), INT32, env), e2_ = conform(e2->code(env), INT32, env);
  operand ret(INT1, env->new_name());
  vp.icmp(*env->cur_stream, LT, e1_, e2_, ret);
  return ret;
}

operand eq_class::code(CgenEnvironment *env) {
  // FIXME: eq_compare no basic
  if (cgen_debug)
    std::cerr << "eq" << endl;
  auto &o = *env->cur_stream;
  ValuePrinter vp(o);
  auto e1_ = e1->code(env), e2_ = e2->code(env);
  operand ret(INT1, env->new_name());
  // cout << ";;;;" << e1_.get_type().get_name() << e2_.get_type().get_name();
  if (is_basic(e1_.get_type())) {
    e2_ = conform(e2_, e1_.get_type(), env);
    // cout<<";1";
  } else if (is_basic(e2_.get_type())) {
    e1_ = conform(e1_, e2_.get_type(), env);
    // cout<<";2";
  } else if (!e1_.get_type().is_string_object() && !e1_.get_type().is_same_with(e2_.get_type())) {
    assert(e1_.get_type().is_ptr());
    assert(e2_.get_type().is_ptr());
    e1_ = conform(e1_, INT8_PTR, env);
    e2_ = conform(e2_, INT8_PTR, env);
    // cout<<";3";
  }
  if (e1_.get_type().is_string_object()) {
    // cout<<";4";
    operand str1(INT8_PTR, env->new_name());
    operand str2(INT8_PTR, env->new_name());
    operand str_ptr1(INT8_PPTR, env->new_name());
    operand str_ptr2(INT8_PPTR, env->new_name());
    // auto p = env->get_class()->sym2type(String);
    vp.getelementptr(o, e1_.get_type().get_deref_type(), e1_, int_value(0), int_value(1), str_ptr1);
    vp.load(o, INT8_PTR, str_ptr1, str1);
    vp.getelementptr(o, e2_.get_type().get_deref_type(), e2_, int_value(0), int_value(1), str_ptr2);
    vp.load(o, INT8_PTR, str_ptr2, str2);
    operand cmp_ret(INT32, env->new_name());
    vp.call(o, {INT8_PTR, INT8_PTR}, "strcmp", true, {str1, str2}, cmp_ret);
    vp.icmp(o, EQ, cmp_ret, int_value(0), ret);
  } else {
    // cout<<";5";
    vp.icmp(o, EQ, e1_, e2_, ret);
  }
  return ret;
}

operand leq_class::code(CgenEnvironment *env) {
  if (cgen_debug)
    std::cerr << "leq" << endl;
  ValuePrinter vp(*env->cur_stream);
  auto e1_ = conform(e1->code(env), INT32, env), e2_ = conform(e2->code(env), INT32, env);
  operand ret(INT1, env->new_name());
  vp.icmp(*env->cur_stream, LE, e1_, e2_, ret);
  return ret;
}

operand comp_class::code(CgenEnvironment *env) {
  if (cgen_debug)
    std::cerr << "complement" << endl;
  ValuePrinter vp(*env->cur_stream);
  auto e1_ = conform(e1->code(env), INT32, env);
  operand ret(INT1, env->new_name());
  vp.xor_in(*env->cur_stream, e1_, bool_value(true, true), ret);
  return ret;
}

operand int_const_class::code(CgenEnvironment *env) {
  if (cgen_debug)
    std::cerr << "Integer Constant" << endl;
  ValuePrinter vp(*env->cur_stream);
  auto ret = int_value(atoi(token->get_string()));
  return ret;
}

operand bool_const_class::code(CgenEnvironment *env) {
  if (cgen_debug)
    std::cerr << "Boolean Constant" << endl;
  ValuePrinter vp(*env->cur_stream);
  auto ret = bool_value(val, true);
  return ret;
}

operand object_class::code(CgenEnvironment *env) {
  if (cgen_debug)
    std::cerr << "Object" << endl;
  ValuePrinter vp(*env->cur_stream);
  if (env->lookup(name) == NULL) {
    assert(env->is_attr(name));
    auto attr = env->get_attr(name);
    auto self_op = *env->lookup(self);
    operand tmp_op1(self_op.get_type().get_deref_type(), env->new_name()),
        alloca_op(env->get_class()->sym2type(attr.ret_type).get_ptr_type(), env->new_name()),
        ret(env->get_class()->sym2type(attr.ret_type), env->new_name());
    vp.load(*env->cur_stream, self_op.get_type().get_deref_type(), self_op, tmp_op1);
    vp.getelementptr(*env->cur_stream, tmp_op1.get_type().get_deref_type(), tmp_op1, int_value{0}, int_value{attr.index + 1}, alloca_op);
    vp.load(*env->cur_stream, alloca_op.get_type().get_deref_type(), alloca_op, ret);
    return ret;
  } else {
    operand alloca_op = *env->lookup(name);
    operand ret(alloca_op.get_type().get_deref_type(), env->new_name());
    vp.load(*env->cur_stream, alloca_op.get_type().get_deref_type(), alloca_op, ret);
    return ret;
  }
}

operand no_expr_class::code(CgenEnvironment *env) {
  if (cgen_debug)
    std::cerr << "No_expr" << endl;
  return operand(EMPTY, "");
}

//*****************************************************************
// The next few code() functions are for node types not supported
// in Phase 1 but these functions must be defined because they are
// declared as methods via the Expression_SHARED_EXTRAS hack.
//*****************************************************************

operand static_dispatch_class::code(CgenEnvironment *env) {
  if (cgen_debug)
    std::cerr << "static dispatch" << endl;
#ifndef MP3
  assert(0 && "Unsupported case for phase 1");
#else
  auto &o = *env->cur_stream;
  ValuePrinter vp(o);
  vector<operand> arg_op;
  vector<op_type> arg_type;
  auto ok = env->new_ok_label();
  for (auto e : actual)
    arg_op.push_back(e->code(env));
  auto obj_ptr_local = expr->code(env);
  arg_op.insert(arg_op.begin(), {obj_ptr_local});
  auto call_class = env->type_to_class(type_name);
  // obj_ptr_local = conform(obj_ptr_local, call_class->sym2type(expr->type), env);
  auto sign = call_class->get_method(name);
  arg_type.push_back(call_class->sym2type(SELF_TYPE));
  for (auto sym : sign.args)
    arg_type.push_back(call_class->sym2type(sym));

  auto offset = sign.index + 4;
  op_func_type full_ret_type(call_class->sym2type(sign.ret_type), arg_type);
  op_func_ptr_type full_ret_ptr_type(call_class->sym2type(sign.ret_type), arg_type);

  operand cmp_result(INT1, env->new_name());
  vp.icmp(o, EQ, obj_ptr_local, null_value(obj_ptr_local.get_type()), cmp_result);
  vp.branch_cond(o, cmp_result, "abort", ok);

  vp.begin_block(ok);
  string name_ = call_class->get_name()->get_string();
  global_value static_vtable_prototype({"_" + name_ + "_vtable", 1}, "_" + name_ + "_vtable_prototype");
  operand method_ptr_heap(full_ret_ptr_type, env->new_name());
  vp.getelementptr(o, static_vtable_prototype.get_type().get_deref_type(), static_vtable_prototype, int_value(0), int_value(offset), method_ptr_heap);
  auto fn_name = env->new_name();
  operand method_addr(full_ret_type, fn_name);
  vp.load(o, method_addr.get_type(), method_ptr_heap, method_addr);
  for (size_t i = 0; i < arg_op.size(); i++)
    arg_op[i] = conform(arg_op[i], arg_type[i], env);
  operand ret(call_class->sym2type(sign.ret_type), env->new_name());
  vp.call(o, arg_type, fn_name, false, arg_op, ret);
  return ret;
#endif
}

operand string_const_class::code(CgenEnvironment *env) {
  if (cgen_debug)
    std::cerr << "string_const" << endl;
#ifndef MP3
  assert(0 && "Unsupported case for phase 1");
#else

  return global_value(op_type("String", 1), env->get_class()->get_classtable()->const_str[token]);
#endif
}

operand dispatch_class::code(CgenEnvironment *env) {
  if (cgen_debug)
    std::cerr << "dispatch" << endl;
#ifndef MP3
  assert(0 && "Unsupported case for phase 1");
#else
  // FIXME: dispatch
  auto &o = *env->cur_stream;
  ValuePrinter vp(o);
  cerr << ";Dispatch:" << this->name << endl;
  vector<operand> arg_op;
  vector<op_type> arg_type;
  auto ok = env->new_ok_label();
  for (auto e : actual)
    arg_op.push_back(e->code(env));
  auto obj_ptr_local_ = expr->code(env);
  auto call_class = env->type_to_class(expr->type);
  auto obj_ptr_local = conform(obj_ptr_local_, call_class->sym2type(expr->type), env);
  arg_op.insert(arg_op.begin(), {obj_ptr_local});
  auto sign = call_class->get_method(name);
  arg_type.push_back(call_class->sym2type(SELF_TYPE));
  for (auto sym : sign.args)
    arg_type.push_back(call_class->sym2type(sym));

  auto offset = sign.index + 4;
  op_func_type full_ret_type(call_class->sym2type(sign.ret_type), arg_type);
  op_func_ptr_type full_ret_ptr_type(call_class->sym2type(sign.ret_type), arg_type);

  if (is_bool_int(obj_ptr_local_.get_type())) {
  } else {
    operand cmp_result(INT1, env->new_name());
    vp.icmp(o, EQ, obj_ptr_local, null_value(obj_ptr_local.get_type()), cmp_result);
    vp.branch_cond(o, cmp_result, "abort", ok);

    vp.begin_block(ok);
  }
  operand vtable_ptr_ptr_heap(call_class->attr[0].get_ptr_type(), env->new_name());
  vp.getelementptr(o, obj_ptr_local.get_type().get_deref_type(), obj_ptr_local, int_value{0}, int_value(0), vtable_ptr_ptr_heap);
  operand vtable_ptr_heap(call_class->attr[0], env->new_name());
  vp.load(o, vtable_ptr_heap.get_type(), vtable_ptr_ptr_heap, vtable_ptr_heap);

  operand method_ptr_heap(full_ret_ptr_type, env->new_name());
  vp.getelementptr(o, vtable_ptr_heap.get_type().get_deref_type(), vtable_ptr_heap, int_value(0), int_value(offset), method_ptr_heap);
  auto fn_name = env->new_name();
  operand method_addr(full_ret_type, fn_name);
  vp.load(o, method_addr.get_type(), method_ptr_heap, method_addr);
  // o << "qwq" << endl;
  for (size_t i = 0; i < arg_op.size(); i++)
    arg_op[i] = conform(arg_op[i], arg_type[i], env);
  operand ret(call_class->sym2type(sign.ret_type), env->new_name());
  vp.call(o, arg_type, fn_name, false, arg_op, ret);
  return ret;
#endif
}

operand typcase_class::code(CgenEnvironment *env) {
  if (cgen_debug)
    std::cerr << "typecase::code()" << endl;
#ifndef MP3
  assert(0 && "Unsupported case for phase 1");
#else
  ValuePrinter vp(*env->cur_stream);
  CgenClassTable *ct = env->get_class()->get_classtable();

  string header_label = env->new_label("case.hdr.", false);
  string exit_label = env->new_label("case.exit.", false);

  // Generate code for expression to select on, and get its static type
  operand code_val = expr->code(env);
  operand expr_val = code_val;
  string code_val_t = code_val.get_typename();
  op_type join_type = env->type_to_class(type)->get_type_name();
  CgenNode *cls = env->type_to_class(expr->get_type());

  // Check for case on void, which gives a runtime error
  if (code_val.get_type().get_id() != INT32 &&
      code_val.get_type().get_id() != INT1) {
    op_type bool_type(INT1), empty_type;
    null_value null_op(code_val.get_type());
    operand icmp_result(bool_type, env->new_name());
    vp.icmp(*env->cur_stream, EQ, code_val, null_op, icmp_result);
    string ok_label = env->new_ok_label();
    vp.branch_cond(icmp_result, "abort", ok_label);
    vp.begin_block(ok_label);
  }

  operand tag = get_class_tag(expr_val, cls, env);
  vp.branch_uncond(header_label);
  string prev_label = header_label;
  vp.begin_block(header_label);

  env->branch_operand = alloca_op;

  std::vector<operand> values;
  env->next_label = exit_label;

  // Generate code for the branches
  for (int i = ct->get_num_classes() - 1; i >= 0; --i) {
    for (auto case_branch : cases) {
      if (i == ct->lookup(case_branch->get_type_decl())->get_tag()) {
        string prefix = string("case.") + itos(i) + ".";
        string case_label = env->new_label(prefix, false);
        vp.branch_uncond(case_label);
        vp.begin_block(case_label);
        operand val = case_branch->code(expr_val, tag, join_type, env);
        values.push_back(val);
      }
    }
  }

  // Abort if there was not a branch covering the actual type
  vp.branch_uncond("abort");

  // Done with case expression: get final result
  env->new_label("", true);
  vp.begin_block(exit_label);
  operand final_result(alloca_type, env->new_name());
  alloca_op.set_type(alloca_op.get_type().get_ptr_type());
  vp.load(*env->cur_stream, alloca_op.get_type().get_deref_type(),
          alloca_op, final_result);
  alloca_op.set_type(alloca_op.get_type().get_deref_type());

  if (cgen_debug)
    cerr << "Done typcase::code()" << endl;
  return final_result;
#endif
}

operand new__class::code(CgenEnvironment *env) {
  if (cgen_debug)
    std::cerr << "newClass" << endl;
#ifndef MP3
  assert(0 && "Unsupported case for phase 1");
#else
  ValuePrinter vp(*env->cur_stream);
  auto selftype = sym2type(type_name, type_name);
  operand tmp(selftype, env->new_name());
  string name_(type_name->get_string());
  vp.call(*env->cur_stream, {}, name_ + "_new", true, {}, tmp);
  return tmp;
#endif
}

operand isvoid_class::code(CgenEnvironment *env) {
  if (cgen_debug)
    std::cerr << "isvoid(" << e1->get_type() << endl;
#ifndef MP3
  assert(0 && "Unsupported case for phase 1");
#else
  ValuePrinter vp(*env->cur_stream);
  auto ret = e1->code(env);
  std::cerr << "isvoid(((" << ret.get_type().get_name() << endl;
  operand qwq(INT1, env->new_name());
  vp.icmp(*env->cur_stream, EQ, ret, null_value(OBJ_PTR), qwq);
  return qwq;
  // if (ret.get_type().get_id() == VOID) {
  //   *env->cur_stream << "\t%" << env->new_name() << " = icmp eq %Object* null, null" << endl;
  //   return bool_value(true, 1);
  // } else {
  //   return bool_value(false, 1);
  // }
#endif
}

// Create the LLVM Function corresponding to this method.
void method_class::layout_feature(CgenNode *cls) {
#ifndef MP3
  assert(0 && "Unsupported case for phase 1");
#else
  // std::cerr << "Method feature: " << name << ' ' << return_type << "( ";
  vector<Symbol> v;
  for (auto p : formals) {
    v.push_back(p->get_type_decl());
  }
  cls->add_method(name, return_type, v);
#endif
}

// If the source tag is >= the branch tag and <= (max child of the branch class)
// tag, then the branch is a superclass of the source
// Handle one branch of a Cool case expression.
// If the source tag is >= the branch tag
// and <= (max child of the branch class) tag,
// then the branch is a superclass of the source.
// See the MP3 handout for more information about our use of class tags.
operand branch_class::code(operand expr_val, operand tag, op_type join_type,
                           CgenEnvironment *env) {
#ifndef MP3
  assert(0 && "Unsupported case for phase 1");
#else
  operand empty;
  ValuePrinter vp(*env->cur_stream);
  if (cgen_debug)
    cerr << "In branch_class::code()" << endl;

  CgenNode *cls = env->get_class()->get_classtable()->lookup(type_decl);
  int my_tag = cls->get_tag();
  int max_child = cls->get_max_child();

  // Generate unique labels for branching into >= branch tag and <= max child
  string sg_label =
      env->new_label(string("src_gte_br") + "." + itos(my_tag) + ".", false);
  string sl_label =
      env->new_label(string("src_lte_mc") + "." + itos(my_tag) + ".", false);
  string exit_label =
      env->new_label(string("br_exit") + "." + itos(my_tag) + ".", false);

  int_value my_tag_val(my_tag);
  op_type old_tag_t(tag.get_type()), i32_t(INT32);
  tag.set_type(i32_t);

  // Compare the source tag to the class tag
  operand icmp_result = vp.icmp(LT, tag, my_tag_val);
  vp.branch_cond(icmp_result, exit_label, sg_label);
  vp.begin_block(sg_label);
  int_value max_child_val(max_child);

  // Compare the source tag to max child
  operand icmp2_result = vp.icmp(GT, tag, max_child_val);
  vp.branch_cond(icmp2_result, exit_label, sl_label);
  vp.begin_block(sl_label);
  tag.set_type(old_tag_t);

  // Handle casts of *arbitrary* types to Int or Bool.  We need to:
  // (a) cast expr_val to boxed type (struct Int* or struct Bool*)
  // (b) unwrap value field from the boxed type
  // At run-time, if source object is not Int or Bool, this will never
  // be invoked (assuming no bugs in the type checker).
  if (cls->get_type_name() == "Int") {
    expr_val = conform(expr_val, op_type(cls->get_type_name(), 1), env);
  } else if (cls->get_type_name() == "Bool") {
    expr_val = conform(expr_val, op_type(cls->get_type_name(), 1), env);
  }

  // If the case expression is of the right type, make a new local
  // variable for the type-casted version of it, which can be used
  // within the expression to evaluate on this branch.
  operand conf_result = conform(expr_val, alloca_type, env);
  vp.store(conf_result, alloca_op);
  env->add_local(name, alloca_op);

  // Generate code for the expression to evaluate on this branch
  operand val = conform(expr->code(env), join_type.get_ptr_type(), env);
  operand conformed = conform(val, env->branch_operand.get_type(), env);
  env->branch_operand.set_type(env->branch_operand.get_type().get_ptr_type());
  // Store result of the expression evaluated
  vp.store(conformed, env->branch_operand);
  env->branch_operand.set_type(env->branch_operand.get_type().get_deref_type());
  env->kill_local();
  // Branch to case statement exit label
  vp.branch_uncond(env->next_label);
  vp.begin_block(exit_label);
  if (cgen_debug)
    cerr << "Done branch_class::code()" << endl;
  return conformed;
#endif
}

// Assign this attribute a slot in the class structure
void attr_class::layout_feature(CgenNode *cls) {
#ifndef MP3
  assert(0 && "Unsupported case for phase 1");
#else
  cls->add_attr(name, type_decl);
#endif
}

void attr_class::code(CgenEnvironment *env) {
#ifndef MP3
  assert(0 && "Unsupported case for phase 1");
#else
  if (env->control != name) return;
  ValuePrinter vp(*env->cur_stream);
  auto rvalue = init->code(env);
  if (rvalue.is_empty()) {  // no_expr
    // cout << env->get_class()->sym2type(type_decl).get_name() << endl;
    env->res = type2initval(env->get_class()->sym2type(type_decl), env);
  } else {
    env->res = rvalue;
  }
#endif
}

//*****************************************************************
// Implementations of make_alloca which create the necessary alloca
// for an expression
//*****************************************************************
void assign_class::make_alloca(CgenEnvironment *env) {
  if (cgen_debug)
    std::cerr << "assign" << endl;
  expr->make_alloca(env);
}

void cond_class::make_alloca(CgenEnvironment *env) {
  if (cgen_debug)
    std::cerr << "cond" << endl;
  ValuePrinter vp(*env->cur_stream);

  if (get_type() == Int) {
    alloca_op = {INT32_PTR, env->new_name()};
    vp.alloca_mem(*env->cur_stream, INT32, alloca_op);
  } else if (get_type() == Bool) {
    alloca_op = {INT1_PTR, env->new_name()};
    vp.alloca_mem(*env->cur_stream, INT1, alloca_op);
  } else {
    // FIXME: cond make_alloca type cases
    auto type = env->get_class()->sym2type(get_type());
    assert(type.is_ptr());
    alloca_op = {type.get_ptr_type(), env->new_name()};
    vp.alloca_mem(*env->cur_stream, type, alloca_op);
  }
  pred->make_alloca(env);
  then_exp->make_alloca(env);
  else_exp->make_alloca(env);
}

void loop_class::make_alloca(CgenEnvironment *env) {
  if (cgen_debug)
    std::cerr << "loop" << endl;

  pred->make_alloca(env);
  body->make_alloca(env);
}

void block_class::make_alloca(CgenEnvironment *env) {
  if (cgen_debug)
    std::cerr << "block" << endl;
  for (auto p : body)
    p->make_alloca(env);
}

void let_class::make_alloca(CgenEnvironment *env) {
  if (cgen_debug)
    std::cerr << "let" << endl;
  init->make_alloca(env);
  ValuePrinter vp(*env->cur_stream);
  if (type_decl == Int) {
    alloca_op = {INT32_PTR, env->new_name()};
    vp.alloca_mem(*env->cur_stream, INT32, alloca_op);
  } else if (type_decl == Bool) {
    alloca_op = {INT1_PTR, env->new_name()};
    vp.alloca_mem(*env->cur_stream, INT1, alloca_op);
  } else {
    // FIXME: let make_alloca type cases
    auto type = env->get_class()->sym2type(type_decl);
    assert(type.is_ptr());
    alloca_op = {type.get_ptr_type(), env->new_name()};
    vp.alloca_mem(*env->cur_stream, type, alloca_op);
  }
  body->make_alloca(env);
}

void plus_class::make_alloca(CgenEnvironment *env) {
  if (cgen_debug)
    std::cerr << "plus" << endl;
  e1->make_alloca(env);
  e2->make_alloca(env);
}

void sub_class::make_alloca(CgenEnvironment *env) {
  if (cgen_debug)
    std::cerr << "sub" << endl;
  e1->make_alloca(env);
  e2->make_alloca(env);
}

void mul_class::make_alloca(CgenEnvironment *env) {
  if (cgen_debug)
    std::cerr << "mul" << endl;
  e1->make_alloca(env);
  e2->make_alloca(env);
}

void divide_class::make_alloca(CgenEnvironment *env) {
  if (cgen_debug)
    std::cerr << "div" << endl;
  e1->make_alloca(env);
  e2->make_alloca(env);
}

void neg_class::make_alloca(CgenEnvironment *env) {
  if (cgen_debug)
    std::cerr << "neg" << endl;
  e1->make_alloca(env);
}

void lt_class::make_alloca(CgenEnvironment *env) {
  if (cgen_debug)
    std::cerr << "lt" << endl;
  e1->make_alloca(env);
  e2->make_alloca(env);
}

void eq_class::make_alloca(CgenEnvironment *env) {
  if (cgen_debug)
    std::cerr << "eq" << endl;
  e1->make_alloca(env);
  e2->make_alloca(env);
}

void leq_class::make_alloca(CgenEnvironment *env) {
  if (cgen_debug)
    std::cerr << "leq" << endl;
  e1->make_alloca(env);
  e2->make_alloca(env);
}

void comp_class::make_alloca(CgenEnvironment *env) {
  if (cgen_debug)
    std::cerr << "complement" << endl;
  e1->make_alloca(env);
}

void int_const_class::make_alloca(CgenEnvironment *env) {
  if (cgen_debug)
    std::cerr << "Integer Constant" << endl;
}

void bool_const_class::make_alloca(CgenEnvironment *env) {
  if (cgen_debug)
    std::cerr << "Boolean Constant" << endl;
}

void object_class::make_alloca(CgenEnvironment *env) {
  if (cgen_debug)
    std::cerr << "Object" << endl;
}

void no_expr_class::make_alloca(CgenEnvironment *env) {
  if (cgen_debug)
    std::cerr << "No_expr" << endl;
}

void static_dispatch_class::make_alloca(CgenEnvironment *env) {
  if (cgen_debug)
    std::cerr << "static dispatch" << endl;

#ifndef MP3
  assert(0 && "Unsupported case for phase 1");
#else
  for (auto a : actual) a->make_alloca(env);
  expr->make_alloca(env);
#endif
}

void string_const_class::make_alloca(CgenEnvironment *env) {
  if (cgen_debug)
    std::cerr << "string_const" << endl;

#ifndef MP3
  assert(0 && "Unsupported case for phase 1");
#else
    // Nothing TODO
#endif
}

void dispatch_class::make_alloca(CgenEnvironment *env) {
  if (cgen_debug)
    std::cerr << "dispatch" << endl;

#ifndef MP3
  assert(0 && "Unsupported case for phase 1");
#else
  for (auto a : actual) a->make_alloca(env);
  expr->make_alloca(env);
#endif
}

// Handle a Cool case expression (selecting based on the type of an object)
void typcase_class::make_alloca(CgenEnvironment *env) {
  if (cgen_debug)
    std::cerr << "typecase::make_alloca()" << endl;
#ifndef MP3
  assert(0 && "Unsupported case for phase 1");
#else
  ValuePrinter vp(*env->cur_stream);
  expr->make_alloca(env);

  // Get result type of case expression
  branch_class *b = (branch_class *)cases->nth(cases->first());
  string case_result_type = b->get_expr()->get_type()->get_string();
  if (case_result_type == "SELF_TYPE")
    case_result_type = env->get_class()->get_type_name();

  // Allocate space for result of case expression
  alloca_type = op_type(case_result_type, 1);
  alloca_op = operand(alloca_type, env->new_name());
  vp.alloca_mem(*env->cur_stream, alloca_type, alloca_op);

  for (auto case_branch : cases) {
    case_branch->make_alloca(env);
  }
#endif
}

void new__class::make_alloca(CgenEnvironment *env) {
  if (cgen_debug)
    std::cerr << "newClass" << endl;

#ifndef MP3
  assert(0 && "Unsupported case for phase 1");
#else
    // Nothing TODO
#endif
}

void isvoid_class::make_alloca(CgenEnvironment *env) {
  if (cgen_debug)
    std::cerr << "isvoid" << endl;

#ifndef MP3
  assert(0 && "Unsupported case for phase 1");
#else
  e1->make_alloca(env);
#endif
}

void branch_class::make_alloca(CgenEnvironment *env) {
#ifndef MP3
  assert(0 && "Unsupported case for phase 1");
#else
  ValuePrinter vp(*env->cur_stream);
  if (cgen_debug)
    cerr << "In branch_class::make_alloca()" << endl;

  CgenNode *cls = env->get_class()->get_classtable()->lookup(type_decl);
  alloca_type = op_type(cls->get_type_name(), 1);

  if (cls->get_type_name() == "Int") {
    alloca_type = op_type(INT32);
  } else if (cls->get_type_name() == "Bool") {
    alloca_type = op_type(INT1);
  }

  alloca_op = vp.alloca_mem(alloca_type);

  expr->make_alloca(env);

  if (cgen_debug)
    cerr << "Done branch_class::make_alloca()" << endl;
#endif
}

void method_class::make_alloca(CgenEnvironment *env) {
  ValuePrinter vp(*env->cur_stream);
  expr->make_alloca(env);
}

void attr_class::make_alloca(CgenEnvironment *env) {
#ifndef MP3
  assert(0 && "Unsupported case for phase 1");
#else
  if (env->control != name) return;
  init->make_alloca(env);
#endif
}
