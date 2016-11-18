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

#if !defined (octave_pt_jit_h)
#define octave_pt_jit_h 1

#ifdef HAVE_LLVM

#include "jit-ir.h"
#include "pt-walk.h"
#include "symtab.h"

class octave_value_list;

// Convert from the parse tree (AST) to the low level Octave IR.
class
jit_convert : public tree_walker
{
public:
  typedef std::pair<jit_type *, std::string> type_bound;
  typedef std::vector<type_bound> type_bound_vector;
  typedef std::map<std::string, jit_variable *> variable_map;

  jit_convert (tree &tee, jit_type *for_bounds = 0);

  jit_convert (octave_user_function& fcn, const std::vector<jit_type *>& args);

#define DECL_ARG(n) const ARG ## n& arg ## n
#define JIT_CREATE_CHECKED(N)                                           \
  template <OCT_MAKE_DECL_LIST (typename, ARG, N)>                      \
  jit_call *create_checked (OCT_MAKE_LIST (DECL_ARG, N))                \
  {                                                                     \
    jit_call *ret = factory.create<jit_call> (OCT_MAKE_ARG_LIST (arg, N)); \
    return create_checked_impl (ret);                                   \
  }

  JIT_CREATE_CHECKED (1)
  JIT_CREATE_CHECKED (2)
  JIT_CREATE_CHECKED (3)
  JIT_CREATE_CHECKED (4)

#undef JIT_CREATE_CHECKED
#undef DECL_ARG

  jit_block_list& get_blocks (void) { return blocks; }

  const type_bound_vector& get_bounds (void) const { return bounds; }

  jit_factory& get_factory (void) { return factory; }

  llvm::Function *get_function (void) const { return function; }

  const variable_map &get_variable_map (void) const { return vmap; }

  void visit_anon_fcn_handle (tree_anon_fcn_handle&);

  void visit_argument_list (tree_argument_list&);

  void visit_binary_expression (tree_binary_expression&);

  void visit_break_command (tree_break_command&);

  void visit_colon_expression (tree_colon_expression&);

  void visit_continue_command (tree_continue_command&);

  void visit_global_command (tree_global_command&);

  void visit_persistent_command (tree_persistent_command&);

  void visit_decl_elt (tree_decl_elt&);

  void visit_decl_init_list (tree_decl_init_list&);

  void visit_simple_for_command (tree_simple_for_command&);

  void visit_complex_for_command (tree_complex_for_command&);

  void visit_octave_user_script (octave_user_script&);

  void visit_octave_user_function (octave_user_function&);

  void visit_octave_user_function_header (octave_user_function&);

  void visit_octave_user_function_trailer (octave_user_function&);

  void visit_function_def (tree_function_def&);

  void visit_identifier (tree_identifier&);

  void visit_if_clause (tree_if_clause&);

  void visit_if_command (tree_if_command&);

  void visit_if_command_list (tree_if_command_list&);

  void visit_index_expression (tree_index_expression&);

  void visit_matrix (tree_matrix&);

  void visit_cell (tree_cell&);

  void visit_multi_assignment (tree_multi_assignment&);

  void visit_no_op_command (tree_no_op_command&);

  void visit_constant (tree_constant&);

  void visit_fcn_handle (tree_fcn_handle&);

  void visit_funcall (tree_funcall&);

  void visit_parameter_list (tree_parameter_list&);

  void visit_postfix_expression (tree_postfix_expression&);

  void visit_prefix_expression (tree_prefix_expression&);

  void visit_return_command (tree_return_command&);

  void visit_return_list (tree_return_list&);

  void visit_simple_assignment (tree_simple_assignment&);

  void visit_statement (tree_statement&);

  void visit_statement_list (tree_statement_list&);

  void visit_switch_case (tree_switch_case&);

  void visit_switch_case_list (tree_switch_case_list&);

  void visit_switch_command (tree_switch_command&);

  void visit_try_catch_command (tree_try_catch_command&);

  void visit_unwind_protect_command (tree_unwind_protect_command&);

  void visit_while_command (tree_while_command&);

  void visit_do_until_command (tree_do_until_command&);
private:
  std::vector<std::pair<std::string, bool> > arguments;
  type_bound_vector bounds;

  bool converting_function;

  // the scope of the function we are converting, or the current scope
  symbol_table::scope_id scope;

  jit_factory factory;

  // used instead of return values from visit_* functions
  jit_value *result;

  jit_block *entry_block;

  jit_block *final_block;

  jit_block *block;

  llvm::Function *function;

  jit_block_list blocks;

  std::vector<jit_magic_end::context> end_context;

  size_t iterator_count;
  size_t for_bounds_count;
  size_t short_count;

  variable_map vmap;

  void initialize (symbol_table::scope_id s);

  jit_call *create_checked_impl (jit_call *ret);

  // get an existing vairable. If the variable does not exist, it will not be
  // created
  jit_variable *find_variable (const std::string& vname) const;

  // get a variable, create it if it does not exist. The type will default to
  // the variable's current type in the symbol table.
  jit_variable *get_variable (const std::string& vname);

  // create a variable of the given name and given type. Will also insert an
  // extract statement
  jit_variable *create_variable (const std::string& vname, jit_type *type,
                                 bool isarg = true);

  // The name of the next for loop iterator. If inc is false, then the iterator
  // counter will not be incremented.
  std::string next_iterator (bool inc = true)
  { return next_name ("#iter", iterator_count, inc); }

  std::string next_for_bounds (bool inc = true)
  { return next_name ("#for_bounds", for_bounds_count, inc); }

  std::string next_shortcircut_result (bool inc = true)
  { return next_name ("#shortcircut_result", short_count, inc); }

  std::string next_name (const char *prefix, size_t& count, bool inc);

  jit_instruction *resolve (tree_index_expression& exp,
                            jit_value *extra_arg = 0, bool lhs = false);

  jit_value *do_assign (tree_expression *exp, jit_value *rhs,
                        bool artificial = false);

  jit_value *do_assign (const std::string& lhs, jit_value *rhs, bool print,
                        bool artificial = false);

  jit_value *visit (tree *tee) { return visit (*tee); }

  jit_value *visit (tree& tee);

  typedef std::list<jit_block *> block_list;
  block_list breaks;
  block_list continues;

  void finish_breaks (jit_block *dest, const block_list& lst);
};

// Convert from the low level Octave IR to LLVM
class
jit_convert_llvm : public jit_ir_walker
{
public:
  llvm::Function *convert_loop (llvm::Module *module,
                                const jit_block_list& blocks,
                                const std::list<jit_value *>& constants);

  jit_function convert_function (llvm::Module *module,
                                 const jit_block_list& blocks,
                                 const std::list<jit_value *>& constants,
                                 octave_user_function& fcn,
                                 const std::vector<jit_type *>& args);

  // arguments to the llvm::Function for loops
  const std::vector<std::pair<std::string, bool> >& get_arguments(void) const
  { return argument_vec; }

#define JIT_METH(clname)                        \
  virtual void visit (jit_ ## clname&);

  JIT_VISIT_IR_CLASSES;

#undef JIT_METH
private:
  // name -> argument index (used for compiling functions)
  std::map<std::string, int> argument_index;

  std::vector<std::pair<std::string, bool> > argument_vec;

  // name -> llvm argument (used for compiling loops)
  std::map<std::string, llvm::Value *> arguments;

  bool converting_function;

  // only used if we are converting a function
  jit_function creating;

  llvm::Function *function;
  llvm::BasicBlock *prelude;

  void convert (const jit_block_list& blocks,
                const std::list<jit_value *>& constants);

  void finish_phi (jit_phi *phi);

  void visit (jit_value *jvalue)
  {
    return visit (*jvalue);
  }

  void visit (jit_value &jvalue)
  {
    jvalue.accept (*this);
  }
};

// type inference and SSA construction on the low level Octave IR
class
jit_infer
{
public:
  typedef jit_convert::variable_map variable_map;

  jit_infer (jit_factory& afactory, jit_block_list& ablocks,
             const variable_map& avmap);

  jit_block_list& get_blocks (void) const { return blocks; }

  jit_factory& get_factory (void) const { return factory; }

  void infer (void);
private:
  jit_block_list& blocks;
  jit_factory& factory;
  const variable_map& vmap;
  std::list<jit_instruction *> worklist;

  void append_users (jit_value *v);

  void append_users_term (jit_terminator *term);

  void construct_ssa (void);

  void do_construct_ssa (jit_block& block, size_t avisit_count);

  jit_block& entry_block (void) { return *blocks.front (); }

  jit_block& final_block (void) { return *blocks.back (); }

  void place_releases (void);

  void push_worklist (jit_instruction *instr);

  void remove_dead ();

  void release_dead_phi (jit_block& ablock);

  void release_temp (jit_block& ablock, std::set<jit_value *>& temp);

  void simplify_phi (void);

  void simplify_phi (jit_phi& phi);
};

class
tree_jit
{
public:
  ~tree_jit (void);

  static bool execute (tree_simple_for_command& cmd,
                       const octave_value& bounds);

  static bool execute (tree_while_command& cmd);

  static bool execute (octave_user_function& fcn, const octave_value_list& args,
                       octave_value_list& retval);

  llvm::ExecutionEngine *get_engine (void) const { return engine; }

  llvm::Module *get_module (void) const { return module; }

  void optimize (llvm::Function *fn);
private:
  tree_jit (void);

  static tree_jit& instance (void);

  bool initialize (void);

  bool do_execute (tree_simple_for_command& cmd, const octave_value& bounds);

  bool do_execute (tree_while_command& cmd);

  bool do_execute (octave_user_function& fcn, const octave_value_list& args,
                   octave_value_list& retval);

  bool enabled (void);

  size_t trip_count (const octave_value& bounds) const;

  llvm::Module *module;
#ifdef LEGACY_PASSMANAGER
  llvm::legacy::PassManager *module_pass_manager;
  llvm::legacy::FunctionPassManager *pass_manager;
#else
  llvm::PassManager *module_pass_manager;
  llvm::FunctionPassManager *pass_manager;
#endif
  llvm::ExecutionEngine *engine;
};

class
jit_function_info
{
public:
  jit_function_info (tree_jit& tjit, octave_user_function& fcn,
                     const octave_value_list& ov_args);

  bool execute (const octave_value_list& ov_args,
                octave_value_list& retval) const;

  bool match (const octave_value_list& ov_args) const;
private:
  typedef octave_base_value *(*jited_function)(octave_base_value**);

  std::vector<jit_type *> argument_types;
  jited_function function;
};

class
jit_info
{
public:
  // we use a pointer here so we don't have to include ov.h
  typedef std::map<std::string, const octave_value *> vmap;

  jit_info (tree_jit& tjit, tree& tee);

  jit_info (tree_jit& tjit, tree& tee, const octave_value& for_bounds);

  ~jit_info (void);

  bool execute (const vmap& extra_vars = vmap ()) const;

  bool match (const vmap& extra_vars = vmap ()) const;
private:
  typedef jit_convert::type_bound type_bound;
  typedef jit_convert::type_bound_vector type_bound_vector;
  typedef void (*jited_function)(octave_base_value**);

  void compile (tree_jit& tjit, tree& tee, jit_type *for_bounds = 0);

  octave_value find (const vmap& extra_vars, const std::string& vname) const;

  llvm::ExecutionEngine *engine;
  jited_function function;
  llvm::Function *llvm_function;

  std::vector<std::pair<std::string, bool> > arguments;
  type_bound_vector bounds;
};

#endif
#endif
