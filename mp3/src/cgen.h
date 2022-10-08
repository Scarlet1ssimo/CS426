//
// This is the MP2 skeleton cgen header.  As given, it contains only basic
// functionality.  You will need to add members to each of the classes
// to get them to perform their desired functions.  Document your important
// design decisions below.  We should be able to read your documentation and
// get a general overview of how your compiler generates code.  For instance,
// how does your compiler generate structures for classes, how is inheritance
// modeled, how do you handle dynamic binding, etc.
//

// ------------------ INSERT DESIGN DOCUMENTATION HERE --------------------- //

// ----------------------------- END DESIGN DOCS --------------------------- //

#include <map>
#include <unordered_map>
#include <utility>

#include "cool-tree.h"
#include "symtab.h"
#include "value_printer.h"
//
// CgenClassTable represents the top level of a Cool program, which is
// basically a list of classes.  The class table is used to look up classes
// (CgenNodes) by name, and it also handles global code generation tasks.
// The CgenClassTable constructor is where you'll find the entry point for
// code generation for an entire Cool program.
//
class CgenClassTable : public cool::SymbolTable<Symbol, CgenNode> {
 private:
  // Class list
  List<CgenNode> *nds;
  List<CgenNode> *special_nds;
  int current_tag;

#ifndef MP3
  CgenNode *getMainmain(CgenNode *c);
#endif

 public:
  // The ostream where we are emitting code
  ostream *ct_stream;
  // CgenClassTable constructor begins and ends the code generation process
  CgenClassTable(Classes, ostream &str);
  ~CgenClassTable();

  // Get the root of the class Tree, i.e. Object
  CgenNode *root();
  int get_num_classes() const { return current_tag; }

  std::map<Symbol, string> const_str;

 private:
  // COMPLETE FUNCTIONS

  // Create declarations for C runtime functions we need to generate code
  void setup_external_functions();
  void setup_classes(CgenNode *c, int depth);

#ifdef MP3
  void code_classes(CgenNode *c);
#endif

  // The following creates an inheritance graph from a list of classes.
  // The graph is implemented as a tree of `CgenNode', and class names
  // are placed in the base class symbol table.
  void install_basic_classes();
  void install_class(CgenNode *nd);
  void install_special_class(CgenNode *nd);
  void install_classes(Classes cs);
  void build_inheritance_tree();
  void set_relations(CgenNode *nd);

  // INCOMPLETE FUNCTIONS

  // Setup each class in the table and prepare for code generation phase
  void setup();

  // Code generation functions. You need to write these functions.
  void code_module();
  void code_constants();
  void code_main();

  // ADD CODE HERE
};

struct attribute {
  Symbol name;
  Symbol ret_type;
  Symbol from;
  int from_basic;
  int index;
};
struct signature {
  Symbol name;
  Symbol ret_type;
  std::vector<Symbol> args;
  Symbol from;
  int from_basic;
  int index;
  string get_signature() {
    if (from_basic)
      return "@" + string(from->get_string()) + "_" + string(name->get_string());
    else
      return "@" + string(from->get_string()) + "." + string(name->get_string());
  }
};
//
// Each CgenNode corresponds to a Cool class.  As such, it is responsible for
// performing code generation on the class level.  This includes laying out
// the class attributes, creating the necessary Types for the class and
// generating code for each of its methods.
//
class CgenNode : public class__class {
 public:
  enum Basicness { Basic,
                   NotBasic };

#ifndef MP3
  void codeGenMainmain();
#endif

 private:
  CgenNode *parentnd;        // Parent of class
  List<CgenNode> *children;  // Children of class
  Basicness basic_status;    // `Basic' or 'NotBasic'
  CgenClassTable *class_table;

  // Class tag.  Should be unique for each class in the tree
  int tag;
  int max_child;

  // ADD CODE HERE
  int attr_ctr, method_ctr;

 public:
  // COMPLETE FUNCTIONS

  // Relationships with other nodes in the tree
  CgenNode *get_parentnd() { return parentnd; }
  void add_child(CgenNode *child);
  void set_parentnd(CgenNode *p);
  int basic() { return (basic_status == Basic); }
  List<CgenNode> *get_children() { return children; }

  // Accessors for other provided fields
  int get_tag() const { return tag; }
  CgenClassTable *get_classtable() { return class_table; }

  void set_max_child(int mc) { max_child = mc; }
  int get_max_child() const { return max_child; }

  // INCOMPLETE FUNCTIONS

  // Constructs a CgenNode from a Class
  CgenNode(Class_ c, Basicness bstatus, CgenClassTable *class_table);
  virtual ~CgenNode() {}

  // Class setup. You need to write the body of this function.
  void setup(int tag, int depth);

  // Class codegen. You need to write the body of this function.
  void code_class();
  void code_new_object(CgenEnvironment *);

  // ADD CODE HERE
  string get_type_name() { return string(name->get_string()); }

  void add_method(Symbol k, Symbol ret_type, vector<Symbol> &v) {
    if (method_layout.find(k) != method_layout.end()) {
      signature s{k, ret_type, v, get_name(), basic(), method_layout[k].index};
      assert(s.name == method_layout[k].name);
      assert(s.ret_type == method_layout[k].ret_type);
      assert(s.args == method_layout[k].args);
      method_layout[k] = s;
    } else {
      signature s{k, ret_type, v, get_name(), basic(), method_ctr++};
      method_layout[k] = s;
    }
  }
  signature get_method(Symbol k) {
    assert(is_method(k));
    return method_layout[k];
  }
  int is_method(Symbol k) {
    return method_layout.count(k);
  }

  void add_attr(Symbol k, Symbol type) {
    attribute a{k, type, get_name(), basic(), attr_ctr++};
    assert(attr_layout.find(k) == attr_layout.end());
    attr_layout[k] = a;
  }
  attribute get_attr(Symbol k) {
    assert(is_attr(k));
    return attr_layout[k];
  }
  int is_attr(Symbol k) {
    return attr_layout.count(k);
  }
  op_type sym2type(Symbol k);
  vector<op_type> attr;
  vector<op_type> method;
  vector<const_value> init_val;
  std::map<Symbol, signature> method_layout;
  std::map<Symbol, attribute> attr_layout;

  bool is_derive_of(Symbol base) {
    for (auto p = this; p; p = p->parentnd) {
      if (p->name == base) return true;
    }
    return false;
  }

 private:
  // Layout the methods and attributes for code generation
  // You need to write the body of this function.
  void layout_features();

  // vector<std::pair<string, signature>> method_layout, attr_layout;
  // std::map<string, signature> method_layout, attr_layout;
  // vector<string> method_order, attr_order;
};

//
// CgenEnvironment provides the environment for code generation of a method.
// Its main task is to provide a mapping from Cool names to LLVM Values.
// This mapping needs to be maintained as scopes are entered and exited, new
// variables are declared, and so on. CgenEnvironment is also a good place
// to put non-local information you will need during code generation.  Two
// examples are the current CgenNode and the current Function.
//
class CgenEnvironment {
 private:
  // mapping from variable names to memory locations
  cool::SymbolTable<Symbol, operand> var_table;

  // Keep counters for unique name generation in the current method
  int block_count;
  int tmp_count;
  int ok_count;

  // ADD CODE HERE
  CgenNode *cur_class;

 public:
  Symbol control;
  operand res;
  std::ostream *cur_stream;

  // fresh name generation functions
  string new_name();
  string new_ok_label();
  const string new_label(const std::string &prefix, bool increment);

  // Used in provided code for the (case..of) construct
  string next_label;
  operand branch_operand;
  void add_local(Symbol name, operand &vb);
  void kill_local();
  // end of helpers for provided code

  CgenEnvironment(ostream &strea, CgenNode *cur_class);

  operand *lookup(Symbol name) { return var_table.lookup(name); }

  CgenNode *get_class() { return cur_class; }
  void set_class(CgenNode *c) { cur_class = c; }

  // INCOMPLETE FUNCTIONS

  // Must return the CgenNode for a class given the symbol of its name
  CgenNode *type_to_class(Symbol t);
  // ADD CODE HERE
  int is_attr(Symbol name) {
    return cur_class->is_attr(name);
  }
  int is_method(Symbol name) {
    return cur_class->is_method(name);
  }
  attribute get_attr(Symbol name) {
    return cur_class->get_attr(name);
  }
  signature get_method(Symbol name) {
    return cur_class->get_method(name);
  }
  CgenNode *str2class(const char *str) {
    return get_class()->get_classtable()->lookup(
        idtable.add_string((char *)str));
  }
  Symbol type2sym(op_type op) {
    if (op.get_id() == INT32)
      return str2class("Int")->get_name();
    if (op.get_id() == INT1)
      return str2class("Bool")->get_name();
    auto ret = str2class((char *)op.get_name().substr(1, op.get_name().length() - 2).c_str());
    assert(ret != NULL);
    return ret->get_name();
  }
};

// Utitlity function
// Generate any code necessary to convert from given operand to
// dest_type, assuing it has already been checked to be compatible
operand conform(operand src, op_type dest_type, CgenEnvironment *env);
// Retrieve the class tag from operand src. Argument is the cgen node for
// the static class of src.
operand get_class_tag(operand src, CgenNode *src_cls, CgenEnvironment *env);
