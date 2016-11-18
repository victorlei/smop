/*

Copyright (C) 2012-2015 Max Brister

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

// Author: Max Brister <max@2bass.com>

#if !defined (octave_jit_typeinfo_h)
#define octave_jit_typeinfo_h 1

#ifdef HAVE_LLVM

#include <map>
#include <vector>

#include "Range.h"
#include "jit-util.h"

// Defines the type system used by jit and a singleton class, jit_typeinfo, to
// manage the types.
//
// FIXME:
// Operations are defined and implemented in jit_typeinfo.  Eventually they
// should be moved elsewhere. (just like with octave_typeinfo)

// jit_range is compatible with the llvm range structure
struct
jit_range
{
  jit_range (const Range& from) : base (from.base ()), limit (from.limit ()),
                                  inc (from.inc ()), nelem (from.nelem ())
  { }

  operator Range () const
  {
    return Range (base, limit, inc);
  }

  bool all_elements_are_ints () const;

  double base;
  double limit;
  double inc;
  octave_idx_type nelem;
};

std::ostream& operator << (std::ostream& os, const jit_range& rng);

// jit_array is compatible with the llvm array/matrix structures
template <typename T, typename U>
struct
jit_array
{
  jit_array () : array (0) { }

  jit_array (T& from) : array (new T (from))
  {
    update ();
  }

  void update (void)
  {
    ref_count = array->jit_ref_count ();
    slice_data = array->jit_slice_data () - 1;
    slice_len = array->capacity ();
    dimensions = array->jit_dimensions ();
  }

  void update (T *aarray)
  {
    array = aarray;
    update ();
  }

  operator T () const
  {
    return *array;
  }

  int *ref_count;

  U *slice_data;
  octave_idx_type slice_len;
  octave_idx_type *dimensions;

  T *array;
};

typedef jit_array<NDArray, double> jit_matrix;

std::ostream& operator << (std::ostream& os, const jit_matrix& mat);

// calling convention
namespace
jit_convention
{
  enum
  type
  {
    // internal to jit
    internal,

    // an external C call
    external,

    length
  };
}

// Used to keep track of estimated (infered) types during JIT. This is a
// hierarchical type system which includes both concrete and abstract types.
//
// The types form a lattice. Currently we only allow for one parent type, but
// eventually we may allow for multiple predecessors.
class
jit_type
{
public:
  typedef llvm::Value *(*convert_fn) (llvm::IRBuilderD&, llvm::Value *);

  jit_type (const std::string& aname, jit_type *aparent, llvm::Type *allvm_type,
            bool askip_paren, int aid);

  // a user readable type name
  const std::string& name (void) const { return mname; }

  // a unique id for the type
  int type_id (void) const { return mid; }

  // An abstract base type, may be null
  jit_type *parent (void) const { return mparent; }

  // convert to an llvm type
  llvm::Type *to_llvm (void) const { return llvm_type; }

  // how this type gets passed as a function argument
  llvm::Type *to_llvm_arg (void) const;

  size_t depth (void) const { return mdepth; }

  bool skip_paren (void) const { return mskip_paren; }

  // -------------------- Calling Convention information --------------------

  // A function declared like: mytype foo (int arg0, int arg1);
  // Will be converted to: void foo (mytype *retval, int arg0, int arg1)
  // if mytype is sret. The caller is responsible for allocating space for
  // retval. (on the stack)
  bool sret (jit_convention::type cc) const { return msret[cc]; }

  void mark_sret (jit_convention::type cc)
  { msret[cc] = true; }

  // A function like: void foo (mytype arg0)
  // Will be converted to: void foo (mytype *arg0)
  // Basically just pass by reference.
  bool pointer_arg (jit_convention::type cc) const { return mpointer_arg[cc]; }

  void mark_pointer_arg (jit_convention::type cc)
  { mpointer_arg[cc] = true; }

  // Convert into an equivalent form before calling. For example, complex is
  // represented as two values llvm vector, but we need to pass it as a two
  // valued llvm structure to C functions.
  convert_fn pack (jit_convention::type cc) { return mpack[cc]; }

  void set_pack (jit_convention::type cc, convert_fn fn) { mpack[cc] = fn; }

  // The inverse operation of pack.
  convert_fn unpack (jit_convention::type cc) { return munpack[cc]; }

  void set_unpack (jit_convention::type cc, convert_fn fn)
  { munpack[cc] = fn; }

  // The resulting type after pack is called.
  llvm::Type *packed_type (jit_convention::type cc)
  { return mpacked_type[cc]; }

  void set_packed_type (jit_convention::type cc, llvm::Type *ty)
  { mpacked_type[cc] = ty; }
private:
  std::string mname;
  jit_type *mparent;
  llvm::Type *llvm_type;
  int mid;
  size_t mdepth;
  bool mskip_paren;

  bool msret[jit_convention::length];
  bool mpointer_arg[jit_convention::length];

  convert_fn mpack[jit_convention::length];
  convert_fn munpack[jit_convention::length];

  llvm::Type *mpacked_type[jit_convention::length];
};

// seperate print function to allow easy printing if type is null
std::ostream& jit_print (std::ostream& os, jit_type *atype);

class jit_value;

// An abstraction for calling llvm functions with jit_values.  Deals with
// calling convention details.
class
jit_function
{
  friend std::ostream& operator << (std::ostream& os, const jit_function& fn);
public:
  // create a function in an invalid state
  jit_function ();

  jit_function (llvm::Module *amodule, jit_convention::type acall_conv,
                const llvm::Twine& aname, jit_type *aresult,
                const std::vector<jit_type *>& aargs);

  // Use an existing function, but change the argument types. The new argument
  // types must behave the same for the current calling convention.
  jit_function (const jit_function& fn, jit_type *aresult,
                const std::vector<jit_type *>& aargs);

  jit_function (const jit_function& fn);

  // erase the interal LLVM function (if it exists). Will become invalid.
  void erase (void);

  template <typename T>
  void add_mapping (llvm::ExecutionEngine *engine, T fn)
  {
    do_add_mapping (engine, reinterpret_cast<void *> (fn));
  }

  bool valid (void) const { return llvm_function; }

  std::string name (void) const;

  llvm::BasicBlock *new_block (const std::string& aname = "body",
                               llvm::BasicBlock *insert_before = 0);

  llvm::Value *call (llvm::IRBuilderD& builder,
                     const std::vector<jit_value *>& in_args) const;

  llvm::Value *call (llvm::IRBuilderD& builder,
                     const std::vector<llvm::Value *>& in_args
                     = std::vector<llvm::Value *> ()) const;

#define JIT_PARAM_ARGS llvm::IRBuilderD& builder,
#define JIT_PARAMS builder,
#define JIT_CALL(N) JIT_EXPAND (llvm::Value *, call, llvm::Value *, const, N)

  JIT_CALL (1)
  JIT_CALL (2)
  JIT_CALL (3)
  JIT_CALL (4)
  JIT_CALL (5)

#undef JIT_CALL

#define JIT_CALL(N) JIT_EXPAND (llvm::Value *, call, jit_value *, const, N)

  JIT_CALL (1);
  JIT_CALL (2);
  JIT_CALL (3);

#undef JIT_CALL
#undef JIT_PARAMS
#undef JIT_PARAM_ARGS

  llvm::Value *argument (llvm::IRBuilderD& builder, size_t idx) const;

  void do_return (llvm::IRBuilderD& builder, llvm::Value *rval = 0,
                  bool verify = true);

  llvm::Function *to_llvm (void) const { return llvm_function; }

  // If true, then the return value is passed as a pointer in the first argument
  bool sret (void) const { return mresult && mresult->sret (call_conv); }

  bool can_error (void) const { return mcan_error; }

  void mark_can_error (void) { mcan_error = true; }

  jit_type *result (void) const { return mresult; }

  jit_type *argument_type (size_t idx) const
  {
    assert (idx < args.size ());
    return args[idx];
  }

  const std::vector<jit_type *>& arguments (void) const { return args; }
private:
  void do_add_mapping (llvm::ExecutionEngine *engine, void *fn);

  llvm::Module *module;
  llvm::Function *llvm_function;
  jit_type *mresult;
  std::vector<jit_type *> args;
  jit_convention::type call_conv;
  bool mcan_error;
};

std::ostream& operator << (std::ostream& os, const jit_function& fn);


// Keeps track of information about how to implement operations (+, -, *, ect)
// and their resulting types.
class
jit_operation
{
public:
  // type signature vector
  typedef std::vector<jit_type *> signature_vec;

  virtual ~jit_operation (void);

  void add_overload (const jit_function& func)
  {
    add_overload (func, func.arguments ());
  }

  void add_overload (const jit_function& func,
                     const signature_vec& args);

  const jit_function& overload (const signature_vec& types) const;

  jit_type *result (const signature_vec& types) const
  {
    const jit_function& temp = overload (types);
    return temp.result ();
  }

#define JIT_PARAMS
#define JIT_PARAM_ARGS
#define JIT_OVERLOAD(N)                                              \
  JIT_EXPAND (const jit_function&, overload, jit_type *, const, N)   \
  JIT_EXPAND (jit_type *, result, jit_type *, const, N)

  JIT_OVERLOAD (1);
  JIT_OVERLOAD (2);
  JIT_OVERLOAD (3);

#undef JIT_PARAMS
#undef JIT_PARAM_ARGS

  const std::string& name (void) const { return mname; }

  void stash_name (const std::string& aname) { mname = aname; }
protected:
  virtual jit_function *generate (const signature_vec& types) const;
private:
  Array<octave_idx_type> to_idx (const signature_vec& types) const;

  const jit_function& do_generate (const signature_vec& types) const;

  struct signature_cmp
  {
    bool operator() (const signature_vec *lhs, const signature_vec *rhs) const;
  };

  typedef std::map<const signature_vec *, jit_function *, signature_cmp>
  generated_map;

  mutable generated_map generated;

  std::vector<Array<jit_function> > overloads;

  std::string mname;
};

class
jit_index_operation : public jit_operation
{
public:
  jit_index_operation (void) : module (0), engine (0) { }

  void initialize (llvm::Module *amodule, llvm::ExecutionEngine *aengine)
  {
    module = amodule;
    engine = aengine;
    do_initialize ();
  }
protected:
  virtual jit_function *generate (const signature_vec& types) const;

  virtual jit_function *generate_matrix (const signature_vec& types) const = 0;

  virtual void do_initialize (void) = 0;

  // helper functions
  // [start_idx, end_idx).
  llvm::Value *create_arg_array (llvm::IRBuilderD& builder,
                                 const jit_function &fn, size_t start_idx,
                                 size_t end_idx) const;

  llvm::Module *module;
  llvm::ExecutionEngine *engine;
};

class
jit_paren_subsref : public jit_index_operation
{
protected:
  virtual jit_function *generate_matrix (const signature_vec& types) const;

  virtual void do_initialize (void);
private:
  jit_function paren_scalar;
};

class
jit_paren_subsasgn : public jit_index_operation
{
protected:
  jit_function *generate_matrix (const signature_vec& types) const;

  virtual void do_initialize (void);
private:
  jit_function paren_scalar;
};

// A singleton class which handles the construction of jit_types and
// jit_operations.
class
jit_typeinfo
{
public:
  static void initialize (llvm::Module *m, llvm::ExecutionEngine *e);

  static jit_type *join (jit_type *lhs, jit_type *rhs)
  {
    return instance->do_join (lhs, rhs);
  }

  static jit_type *get_any (void) { return instance->any; }

  static jit_type *get_matrix (void) { return instance->matrix; }

  static jit_type *get_scalar (void) { return instance->scalar; }

  static llvm::Type *get_scalar_llvm (void)
  { return instance->scalar->to_llvm (); }

  static jit_type *get_scalar_ptr (void) { return instance->scalar_ptr; }

  static jit_type *get_any_ptr (void) { return instance->any_ptr; }

  static jit_type *get_range (void) { return instance->range; }

  static jit_type *get_string (void) { return instance->string; }

  static jit_type *get_bool (void) { return instance->boolean; }

  static jit_type *get_index (void) { return instance->index; }

  static llvm::Type *get_index_llvm (void)
  { return instance->index->to_llvm (); }

  static jit_type *get_complex (void) { return instance->complex; }

  // Get the jit_type of an octave_value
  static jit_type *type_of (const octave_value& ov)
  {
    return instance->do_type_of (ov);
  }

  static const jit_operation& binary_op (int op)
  {
    return instance->do_binary_op (op);
  }

  static const jit_operation& unary_op (int op)
  {
    return instance->do_unary_op (op);
  }

  static const jit_operation& grab (void) { return instance->grab_fn; }

  static const jit_function& get_grab (jit_type *type)
  {
    return instance->grab_fn.overload (type);
  }

  static const jit_operation& release (void)
  {
    return instance->release_fn;
  }

  static const jit_function& get_release (jit_type *type)
  {
    return instance->release_fn.overload (type);
  }

  static const jit_operation& destroy (void)
  {
    return instance->destroy_fn;
  }

  static const jit_operation& print_value (void)
  {
    return instance->print_fn;
  }

  static const jit_operation& for_init (void)
  {
    return instance->for_init_fn;
  }

  static const jit_operation& for_check (void)
  {
    return instance->for_check_fn;
  }

  static const jit_operation& for_index (void)
  {
    return instance->for_index_fn;
  }

  static const jit_operation& make_range (void)
  {
    return instance->make_range_fn;
  }

  static const jit_operation& paren_subsref (void)
  {
    return instance->paren_subsref_fn;
  }

  static const jit_operation& paren_subsasgn (void)
  {
    return instance->paren_subsasgn_fn;
  }

  static const jit_operation& logically_true (void)
  {
    return instance->logically_true_fn;
  }

  static const jit_operation& cast (jit_type *result)
  {
    return instance->do_cast (result);
  }

  static const jit_function& cast (jit_type *to, jit_type *from)
  {
    return instance->do_cast (to, from);
  }

  static llvm::Value *insert_error_check (llvm::IRBuilderD& bld)
  {
    return instance->do_insert_error_check (bld);
  }

  static llvm::Value *insert_interrupt_check (llvm::IRBuilderD& bld)
  {
    return instance->do_insert_interrupt_check (bld);
  }

  static const jit_operation& end (void)
  {
    return instance->end_fn;
  }

  static const jit_function& end (jit_value *value, jit_value *index,
                                  jit_value *count)
  {
    return instance->do_end (value, index, count);
  }

  static const jit_operation& create_undef (void)
  {
    return instance->create_undef_fn;
  }

  static llvm::Value *create_complex (llvm::Value *real, llvm::Value *imag)
  {
    return instance->complex_new (real, imag);
  }
private:
  jit_typeinfo (llvm::Module *m, llvm::ExecutionEngine *e);

  // FIXME: Do these methods really need to be in jit_typeinfo?
  jit_type *do_join (jit_type *lhs, jit_type *rhs)
  {
    // empty case
    if (! lhs)
      return rhs;

    if (! rhs)
      return lhs;

    // check for a shared parent
    while (lhs != rhs)
      {
        if (lhs->depth () > rhs->depth ())
          lhs = lhs->parent ();
        else if (lhs->depth () < rhs->depth ())
          rhs = rhs->parent ();
        else
          {
            // we MUST have depth > 0 as any is the base type of everything
            do
              {
                lhs = lhs->parent ();
                rhs = rhs->parent ();
              }
            while (lhs != rhs);
          }
      }

    return lhs;
  }

  jit_type *do_difference (jit_type *lhs, jit_type *)
  {
    // FIXME: Maybe we can do something smarter?
    return lhs;
  }

  jit_type *do_type_of (const octave_value &ov) const;

  const jit_operation& do_binary_op (int op) const
  {
    assert (static_cast<size_t>(op) < binary_ops.size ());
    return binary_ops[op];
  }

  const jit_operation& do_unary_op (int op) const
  {
    assert (static_cast<size_t> (op) < unary_ops.size ());
    return unary_ops[op];
  }

  const jit_operation& do_cast (jit_type *to)
  {
    static jit_operation null_function;
    if (! to)
      return null_function;

    size_t id = to->type_id ();
    if (id >= casts.size ())
      return null_function;
    return casts[id];
  }

  const jit_function& do_cast (jit_type *to, jit_type *from)
  {
    return do_cast (to).overload (from);
  }

  const jit_function& do_end (jit_value *value, jit_value *index,
                              jit_value *count);

  jit_type *new_type (const std::string& name, jit_type *parent,
                      llvm::Type *llvm_type, bool skip_paren = false);


  void add_print (jit_type *ty, void *fptr);

  void add_binary_op (jit_type *ty, int op, int llvm_op);

  void add_binary_icmp (jit_type *ty, int op, int llvm_op);

  void add_binary_fcmp (jit_type *ty, int op, int llvm_op);

  // create a function with an external calling convention
  // forces the function pointer to be specified
  template <typename T>
  jit_function create_external (llvm::ExecutionEngine *ee, T fn,
                                const llvm::Twine& name, jit_type *ret,
                                const std::vector<jit_type *>& args
                                = std::vector<jit_type *> ())
  {
    jit_function retval = create_function (jit_convention::external, name, ret,
                                           args);
    retval.add_mapping (ee, fn);
    return retval;
  }

#define JIT_PARAM_ARGS llvm::ExecutionEngine *ee, T fn,     \
    const llvm::Twine& name, jit_type *ret,
#define JIT_PARAMS ee, fn, name, ret,
#define CREATE_FUNCTION(N) JIT_EXPAND(template <typename T> jit_function, \
                                      create_external,                  \
                                      jit_type *, /* empty */, N)

  CREATE_FUNCTION(1);
  CREATE_FUNCTION(2);
  CREATE_FUNCTION(3);
  CREATE_FUNCTION(4);

#undef JIT_PARAM_ARGS
#undef JIT_PARAMS
#undef CREATE_FUNCTION

  // use create_external or create_internal directly
  jit_function create_function (jit_convention::type cc,
                                const llvm::Twine& name, jit_type *ret,
                                const std::vector<jit_type *>& args
                                = std::vector<jit_type *> ());

  // create an internal calling convention (a function defined in llvm)
  jit_function create_internal (const llvm::Twine& name, jit_type *ret,
                                const std::vector<jit_type *>& args
                                = std::vector<jit_type *> ())
  {
    return create_function (jit_convention::internal, name, ret, args);
  }

#define JIT_PARAM_ARGS const llvm::Twine& name, jit_type *ret,
#define JIT_PARAMS name, ret,
#define CREATE_FUNCTION(N) JIT_EXPAND(jit_function, create_internal,    \
                                      jit_type *, /* empty */, N)

  CREATE_FUNCTION(1);
  CREATE_FUNCTION(2);
  CREATE_FUNCTION(3);
  CREATE_FUNCTION(4);

#undef JIT_PARAM_ARGS
#undef JIT_PARAMS
#undef CREATE_FUNCTION

  jit_function create_identity (jit_type *type);

  llvm::Value *do_insert_error_check (llvm::IRBuilderD& bld);

  llvm::Value *do_insert_interrupt_check (llvm::IRBuilderD& bld);

  void add_builtin (const std::string& name);

  void register_intrinsic (const std::string& name, size_t id,
                           jit_type *result, jit_type *arg0)
  {
    std::vector<jit_type *> args (1, arg0);
    register_intrinsic (name, id, result, args);
  }

  void register_intrinsic (const std::string& name, size_t id, jit_type *result,
                           const std::vector<jit_type *>& args);

  void register_generic (const std::string& name, jit_type *result,
                         jit_type *arg0)
  {
    std::vector<jit_type *> args (1, arg0);
    register_generic (name, result, args);
  }

  void register_generic (const std::string& name, jit_type *result,
                         const std::vector<jit_type *>& args);

  octave_builtin *find_builtin (const std::string& name);

  jit_function mirror_binary (const jit_function& fn);

  llvm::Function *wrap_complex (llvm::Function *wrap);

  static llvm::Value *pack_complex (llvm::IRBuilderD& bld,
                                    llvm::Value *cplx);

  static llvm::Value *unpack_complex (llvm::IRBuilderD& bld,
                                      llvm::Value *result);

  llvm::Value *complex_real (llvm::Value *cx);

  llvm::Value *complex_real (llvm::Value *cx, llvm::Value *real);

  llvm::Value *complex_imag (llvm::Value *cx);

  llvm::Value *complex_imag (llvm::Value *cx, llvm::Value *imag);

  llvm::Value *complex_new (llvm::Value *real, llvm::Value *imag);

  void create_int (size_t nbits);

  jit_type *intN (size_t nbits) const;

  static jit_typeinfo *instance;

  llvm::Module *module;
  llvm::ExecutionEngine *engine;
  int next_id;

  llvm::GlobalVariable *lerror_state;
  llvm::GlobalVariable *loctave_interrupt_state;

  llvm::Type *sig_atomic_type;

  std::vector<jit_type*> id_to_type;
  jit_type *any;
  jit_type *matrix;
  jit_type *scalar;
  jit_type *scalar_ptr; // a fake type for interfacing with C++
  jit_type *any_ptr; // a fake type for interfacing with C++
  jit_type *range;
  jit_type *string;
  jit_type *boolean;
  jit_type *index;
  jit_type *complex;
  jit_type *unknown_function;
  std::map<size_t, jit_type *> ints;
  std::map<std::string, jit_type *> builtins;

  llvm::StructType *complex_ret;

  std::vector<jit_operation> binary_ops;
  std::vector<jit_operation> unary_ops;
  jit_operation grab_fn;
  jit_operation release_fn;
  jit_operation destroy_fn;
  jit_operation print_fn;
  jit_operation for_init_fn;
  jit_operation for_check_fn;
  jit_operation for_index_fn;
  jit_operation logically_true_fn;
  jit_operation make_range_fn;
  jit_paren_subsref paren_subsref_fn;
  jit_paren_subsasgn paren_subsasgn_fn;
  jit_operation end1_fn;
  jit_operation end_fn;
  jit_operation create_undef_fn;

  jit_function any_call;

  // type id -> cast function TO that type
  std::vector<jit_operation> casts;

  // type id -> identity function
  std::vector<jit_function> identities;

  llvm::IRBuilderD& builder;
};

#endif
#endif
