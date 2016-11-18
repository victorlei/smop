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

#define __STDC_LIMIT_MACROS
#define __STDC_CONSTANT_MACROS

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "debug.h"
#include "defun.h"
#include "ov.h"
#include "pt-all.h"
#include "pt-jit.h"
#include "sighandlers.h"
#include "symtab.h"
#include "variables.h"

#ifdef HAVE_LLVM

static bool Vdebug_jit = false;

static bool Vjit_enable = false;

static int Vjit_startcnt = 1000;

static int Vjit_failcnt = 0;

#include <llvm/Analysis/CallGraph.h>
#include <llvm/Analysis/Passes.h>

#ifdef HAVE_LLVM_IR_VERIFIER_H
#include <llvm/IR/Verifier.h>
#else
#include <llvm/Analysis/Verifier.h>
#endif

#include <llvm/Bitcode/ReaderWriter.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/ExecutionEngine/JIT.h>

#ifdef LEGACY_PASSMANAGER
#include <llvm/IR/LegacyPassManager.h>
#else
#include <llvm/PassManager.h>
#endif

#ifdef HAVE_LLVM_IR_FUNCTION_H
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#else
#include <llvm/LLVMContext.h>
#include <llvm/Module.h>
#endif

#ifdef HAVE_LLVM_SUPPORT_IRBUILDER_H
#include <llvm/Support/IRBuilder.h>
#elif defined(HAVE_LLVM_IR_IRBUILDER_H)
#include <llvm/IR/IRBuilder.h>
#else
#include <llvm/IRBuilder.h>
#endif

#include <llvm/Support/raw_os_ostream.h>
#include <llvm/Support/TargetSelect.h>

#ifdef HAVE_LLVM_IR_DATALAYOUT_H
#include <llvm/IR/DataLayout.h>
#elif defined(HAVE_LLVM_DATALAYOUT_H)
#include <llvm/DataLayout.h>
#else
#include <llvm/Target/TargetData.h>
#endif

#include <llvm/Transforms/IPO.h>
#include <llvm/Transforms/Scalar.h>

static llvm::IRBuilder<> builder (llvm::getGlobalContext ());

static llvm::LLVMContext& context = llvm::getGlobalContext ();

// -------------------- jit_break_exception --------------------

// jit_break is thrown whenever a branch we are converting has only breaks or
// continues. This is because all code that follows a break or continue is dead.
class jit_break_exception : public std::exception {};

// -------------------- jit_convert --------------------
jit_convert::jit_convert (tree &tee, jit_type *for_bounds)
  : converting_function (false)
{
  initialize (symbol_table::current_scope ());

  if (for_bounds)
    create_variable (next_for_bounds (false), for_bounds);

  try
    {
      visit (tee);
    }
  catch (const jit_break_exception&)
    { }

  // breaks must have been handled by the top level loop
  assert (breaks.empty ());
  assert (continues.empty ());

  block->append (factory.create<jit_branch> (final_block));
  blocks.push_back (final_block);

  for (variable_map::iterator iter = vmap.begin (); iter != vmap.end (); ++iter)
    {
      jit_variable *var = iter->second;
      const std::string& name = var->name ();
      if (name.size () && name[0] != '#')
        final_block->append (factory.create<jit_store_argument> (var));
    }

  final_block->append (factory.create<jit_return> ());
}

jit_convert::jit_convert (octave_user_function& fcn,
                          const std::vector<jit_type *>& args)
  : converting_function (true)
{
  initialize (fcn.scope ());

  tree_parameter_list *plist = fcn.parameter_list ();
  tree_parameter_list *rlist = fcn.return_list ();
  if (plist && plist->takes_varargs ())
    throw jit_fail_exception ("varags not supported");

  if (rlist && (rlist->size () > 1 || rlist->takes_varargs ()))
    throw jit_fail_exception ("multiple returns not supported");

  if (plist)
    {
      tree_parameter_list::iterator piter = plist->begin ();
      for (size_t i = 0; i < args.size (); ++i, ++piter)
        {
          if (piter == plist->end ())
            throw jit_fail_exception ("Too many parameter to function");

          tree_decl_elt *elt = *piter;
          std::string name = elt->name ();
          create_variable (name, args[i]);
        }
    }

  jit_value *return_value = 0;
  bool all_breaking = false;
  if (fcn.is_special_expr ())
    {
      tree_expression *expr = fcn.special_expr ();
      if (expr)
        {
          jit_variable *retvar = get_variable ("#return");
          jit_value *retval = 0;
          try
            {
              retval = visit (expr);
            }
          catch (const jit_break_exception&)
            { }

          if (breaks.size () || continues.size ())
            throw jit_fail_exception ("break/continue not supported in "
                                      "anonymous functions");

          block->append (factory.create<jit_assign> (retvar, retval));
          return_value = retvar;
        }
    }
  else
    {
      try
        {
          visit_statement_list (*fcn.body ());
        }
      catch (const jit_break_exception&)
        {
          all_breaking = true;
        }

      // the user may use break or continue to exit the function
      finish_breaks (final_block, continues);
      finish_breaks (final_block, breaks);
    }

  if (! all_breaking)
    block->append (factory.create<jit_branch> (final_block));

  blocks.push_back (final_block);
  block = final_block;

  if (! return_value && rlist && rlist->size () == 1)
    {
      tree_decl_elt *elt = rlist->front ();
      return_value = get_variable (elt->name ());
    }

  // FIXME: We should use live range analysis to delete variables where needed.
  // For now we just delete everything at the end of the function.
  for (variable_map::iterator iter = vmap.begin (); iter != vmap.end (); ++iter)
    {
      if (iter->second != return_value)
        {
          jit_call *call;
          call = factory.create<jit_call> (&jit_typeinfo::destroy,
                                           iter->second);
          final_block->append (call);
        }
    }

  if (return_value)
    final_block->append (factory.create<jit_return> (return_value));
  else
    final_block->append (factory.create<jit_return> ());
}

void
jit_convert::visit_anon_fcn_handle (tree_anon_fcn_handle&)
{
  throw jit_fail_exception ("No visit_anon_fcn_handle implementation");
}

void
jit_convert::visit_argument_list (tree_argument_list&)
{
  throw jit_fail_exception ("No visit_argument_list implementation");
}

void
jit_convert::visit_binary_expression (tree_binary_expression& be)
{
  if (be.op_type () >= octave_value::num_binary_ops)
    {
      tree_boolean_expression *boole;
      boole = dynamic_cast<tree_boolean_expression *> (&be);
      assert (boole);
      bool is_and = boole->op_type () == tree_boolean_expression::bool_and;

      std::string short_name = next_shortcircut_result ();
      jit_variable *short_result = factory.create<jit_variable> (short_name);
      vmap[short_name] = short_result;

      jit_block *done = factory.create<jit_block> (block->name ());
      tree_expression *lhs = be.lhs ();
      jit_value *lhsv = visit (lhs);
      lhsv = create_checked (&jit_typeinfo::logically_true, lhsv);

      jit_block *short_early = factory.create<jit_block> ("short_early");
      blocks.push_back (short_early);

      jit_block *short_cont = factory.create<jit_block> ("short_cont");

      if (is_and)
        block->append (factory.create<jit_cond_branch> (lhsv, short_cont,
                                                        short_early));
      else
        block->append (factory.create<jit_cond_branch> (lhsv, short_early,
                                                        short_cont));

      block = short_early;

      jit_value *early_result = factory.create<jit_const_bool> (! is_and);
      block->append (factory.create<jit_assign> (short_result, early_result));
      block->append (factory.create<jit_branch> (done));

      blocks.push_back (short_cont);
      block = short_cont;

      tree_expression *rhs = be.rhs ();
      jit_value *rhsv = visit (rhs);
      rhsv = create_checked (&jit_typeinfo::logically_true, rhsv);
      block->append (factory.create<jit_assign> (short_result, rhsv));
      block->append (factory.create<jit_branch> (done));

      blocks.push_back (done);
      block = done;
      result = short_result;
    }
  else
    {
      tree_expression *lhs = be.lhs ();
      jit_value *lhsv = visit (lhs);

      tree_expression *rhs = be.rhs ();
      jit_value *rhsv = visit (rhs);

      const jit_operation& fn = jit_typeinfo::binary_op (be.op_type ());
      result = create_checked (fn, lhsv, rhsv);
    }
}

void
jit_convert::visit_break_command (tree_break_command&)
{
  breaks.push_back (block);
  throw jit_break_exception ();
}

void
jit_convert::visit_colon_expression (tree_colon_expression& expr)
{
  // in the futher we need to add support for classes and deal with rvalues
  jit_value *base = visit (expr.base ());
  jit_value *limit = visit (expr.limit ());
  jit_value *increment;
  tree_expression *tinc = expr.increment ();

  if (tinc)
    increment = visit (tinc);
  else
    increment = factory.create<jit_const_scalar> (1);

  result = block->append (factory.create<jit_call> (jit_typeinfo::make_range,
                                                    base, limit, increment));
}

void
jit_convert::visit_continue_command (tree_continue_command&)
{
  continues.push_back (block);
  throw jit_break_exception ();
}

void
jit_convert::visit_global_command (tree_global_command&)
{
  throw jit_fail_exception ("No visit_global_command implemenation");
}

void
jit_convert::visit_persistent_command (tree_persistent_command&)
{
  throw jit_fail_exception ("No visit_persistent_command implementation");
}

void
jit_convert::visit_decl_elt (tree_decl_elt&)
{
  throw jit_fail_exception ("No visit_decl_elt implementation");
}

void
jit_convert::visit_decl_init_list (tree_decl_init_list&)
{
  throw jit_fail_exception ("No visit_decl_init_list implementation");
}

void
jit_convert::visit_simple_for_command (tree_simple_for_command& cmd)
{
  // Note we do an initial check to see if the loop will run atleast once.
  // This allows us to get better type inference bounds on variables defined
  // and used only inside the for loop (e.g. the index variable)

  // If we are a nested for loop we need to store the previous breaks
  unwind_protect prot;
  prot.protect_var (breaks);
  prot.protect_var (continues);
  breaks.clear ();
  continues.clear ();

  // we need a variable for our iterator, because it is used in multiple blocks
  std::string iter_name = next_iterator ();
  jit_variable *iterator = factory.create<jit_variable> (iter_name);
  factory.create<jit_variable> (iter_name);
  vmap[iter_name] = iterator;

  jit_block *body = factory.create<jit_block> ("for_body");
  jit_block *tail = factory.create<jit_block> ("for_tail");

  // do control expression, iter init, and condition check in prev_block (block)
  // if we are the top level for loop, the bounds is an input argument.
  jit_value *control = find_variable (next_for_bounds ());
  if (! control)
    control = visit (cmd.control_expr ());
  jit_call *init_iter = factory.create<jit_call> (jit_typeinfo::for_init,
                                                  control);
  block->append (init_iter);
  block->append (factory.create<jit_assign> (iterator, init_iter));

  jit_call *check = factory.create<jit_call> (jit_typeinfo::for_check, control,
                                              iterator);
  block->append (check);
  block->append (factory.create<jit_cond_branch> (check, body, tail));

  blocks.push_back (body);
  block = body;

  // compute the syntactical iterator
  jit_call *idx_rhs = factory.create<jit_call> (jit_typeinfo::for_index,
                                                control, iterator);
  block->append (idx_rhs);
  do_assign (cmd.left_hand_side (), idx_rhs);

  // do loop
  tree_statement_list *pt_body = cmd.body ();
  bool all_breaking = false;
  try
    {
      pt_body->accept (*this);
    }
  catch (const jit_break_exception&)
    {
      if (continues.empty ())
        {
          // WTF are you doing user? Every branch was a break, why did you have
          // a loop??? Users are silly people...
          finish_breaks (tail, breaks);
          blocks.push_back (tail);
          block = tail;
          return;
        }

      all_breaking = true;
    }

  // check our condition, continues jump to this block
  jit_block *check_block = factory.create<jit_block> ("for_check");
  blocks.push_back (check_block);

  jit_block *interrupt_check = factory.create<jit_block> ("for_interrupt");
  blocks.push_back (interrupt_check);

  if (! all_breaking)
    block->append (factory.create<jit_branch> (check_block));
  finish_breaks (check_block, continues);

  block = check_block;
  const jit_operation& add_fn = jit_typeinfo::binary_op (octave_value::op_add);
  jit_value *one = factory.create<jit_const_index> (1);
  jit_call *iter_inc = factory.create<jit_call> (add_fn, iterator, one);
  block->append (iter_inc);
  block->append (factory.create<jit_assign> (iterator, iter_inc));
  check = block->append (factory.create<jit_call> (jit_typeinfo::for_check,
                                                   control, iterator));
  block->append (factory.create<jit_cond_branch> (check, interrupt_check,
                                                  tail));

  block = interrupt_check;
  jit_error_check *ec
    = factory.create<jit_error_check> (jit_error_check::var_interrupt,
                                       body, final_block);
  block->append (ec);

  // breaks will go to our tail
  blocks.push_back (tail);
  finish_breaks (tail, breaks);
  block = tail;
}

void
jit_convert::visit_complex_for_command (tree_complex_for_command&)
{
  throw jit_fail_exception ("No visit_complex_for_command implementation");
}

void
jit_convert::visit_octave_user_script (octave_user_script&)
{
  throw jit_fail_exception ("No visit_octave_user_script implementation");
}

void
jit_convert::visit_octave_user_function (octave_user_function&)
{
  throw jit_fail_exception ("No visit_octave_user_function implementation");
}

void
jit_convert::visit_octave_user_function_header (octave_user_function&)
{
  throw jit_fail_exception ("No visit_octave_user_function_header implementation");
}

void
jit_convert::visit_octave_user_function_trailer (octave_user_function&)
{
  throw jit_fail_exception ("No visit_octave_user_function_trailer implementation");
}

void
jit_convert::visit_function_def (tree_function_def&)
{
  throw jit_fail_exception ("No visit_function_def implementation");
}

void
jit_convert::visit_identifier (tree_identifier& ti)
{
  if (ti.has_magic_end ())
    {
      if (!end_context.size ())
        throw jit_fail_exception ("Illegal end");
      result = block->append (factory.create<jit_magic_end> (end_context));
    }
  else
    {
      jit_variable *var = get_variable (ti.name ());
      jit_instruction *instr;
      instr = factory.create<jit_call> (&jit_typeinfo::grab, var);
      result = block->append (instr);
    }
}

void
jit_convert::visit_if_clause (tree_if_clause&)
{
  throw jit_fail_exception ("No visit_if_clause implementation");
}

void
jit_convert::visit_if_command (tree_if_command& cmd)
{
  tree_if_command_list *lst = cmd.cmd_list ();
  assert (lst); // jwe: Can this be null?
  lst->accept (*this);
}

void
jit_convert::visit_if_command_list (tree_if_command_list& lst)
{
  tree_if_clause *last = lst.back ();
  size_t last_else = static_cast<size_t> (last->is_else_clause ());

  // entry_blocks represents the block you need to enter in order to execute
  // the condition check for the ith clause. For the else, it is simple the
  // else body. If there is no else body, then it is padded with the tail
  std::vector<jit_block *> entry_blocks (lst.size () + 1 - last_else);
  entry_blocks[0] = block;

  // we need to construct blocks first, because they have jumps to eachother
  tree_if_command_list::iterator iter = lst.begin ();
  ++iter;
  for (size_t i = 1; iter != lst.end (); ++iter, ++i)
    {
      tree_if_clause *tic = *iter;
      if (tic->is_else_clause ())
        entry_blocks[i] = factory.create<jit_block> ("else");
      else
        entry_blocks[i] = factory.create<jit_block> ("ifelse_cond");
    }

  jit_block *tail = factory.create<jit_block> ("if_tail");
  if (! last_else)
    entry_blocks[entry_blocks.size () - 1] = tail;


  // each branch in the if statement will have different breaks/continues
  block_list current_breaks = breaks;
  block_list current_continues = continues;
  breaks.clear ();
  continues.clear ();

  size_t num_incomming = 0; // number of incomming blocks to our tail
  iter = lst.begin ();
  for (size_t i = 0; iter != lst.end (); ++iter, ++i)
    {
      tree_if_clause *tic = *iter;
      block = entry_blocks[i];
      assert (block);

      if (i) // the first block is prev_block, so it has already been added
        blocks.push_back (entry_blocks[i]);

      if (! tic->is_else_clause ())
        {
          tree_expression *expr = tic->condition ();
          jit_value *cond = visit (expr);
          jit_call *check = create_checked (&jit_typeinfo::logically_true,
                                            cond);
          jit_block *body = factory.create<jit_block> (i == 0 ? "if_body"
                                                              : "ifelse_body");
          blocks.push_back (body);

          jit_instruction *br = factory.create<jit_cond_branch> (check, body,
                                                        entry_blocks[i + 1]);
          block->append (br);
          block = body;
        }

      tree_statement_list *stmt_lst = tic->commands ();
      assert (stmt_lst); // jwe: Can this be null?

      try
        {
          stmt_lst->accept (*this);
          ++num_incomming;
          block->append (factory.create<jit_branch> (tail));
        }
      catch (const jit_break_exception&)
        { }

      current_breaks.splice (current_breaks.end (), breaks);
      current_continues.splice (current_continues.end (), continues);
    }

  breaks.splice (breaks.end (), current_breaks);
  continues.splice (continues.end (), current_continues);

  if (num_incomming || ! last_else)
    {
      blocks.push_back (tail);
      block = tail;
    }
  else
    // every branch broke, so we don't have a tail
    throw jit_break_exception ();
}

void
jit_convert::visit_index_expression (tree_index_expression& exp)
{
  result = resolve (exp);
}

void
jit_convert::visit_matrix (tree_matrix&)
{
  throw jit_fail_exception ("No visit_matrix implementation");
}

void
jit_convert::visit_cell (tree_cell&)
{
  throw jit_fail_exception ("No visit_cell implementation");
}

void
jit_convert::visit_multi_assignment (tree_multi_assignment&)
{
  throw jit_fail_exception ("No visit_multi_assignment implementation");
}

void
jit_convert::visit_no_op_command (tree_no_op_command&)
{
  throw jit_fail_exception ("No visit_no_op_command implementation");
}

void
jit_convert::visit_constant (tree_constant& tc)
{
  octave_value v = tc.rvalue1 ();
  jit_type *ty = jit_typeinfo::type_of (v);

  if (ty == jit_typeinfo::get_scalar ())
    {
      double dv = v.double_value ();
      result = factory.create<jit_const_scalar> (dv);
    }
  else if (ty == jit_typeinfo::get_range ())
    {
      Range rv = v.range_value ();
      result = factory.create<jit_const_range> (rv);
    }
  else if (ty == jit_typeinfo::get_complex ())
    {
      Complex cv = v.complex_value ();
      result = factory.create<jit_const_complex> (cv);
    }
  else
    throw jit_fail_exception ("Unknown constant");
}

void
jit_convert::visit_fcn_handle (tree_fcn_handle&)
{
  throw jit_fail_exception ("No visit_fcn_handle implementation");
}

void
jit_convert::visit_funcall (tree_funcall&)
{
  throw jit_fail_exception ();
}

void
jit_convert::visit_parameter_list (tree_parameter_list&)
{
  throw jit_fail_exception ("No visit_parameter_list implementation");
}

void
jit_convert::visit_postfix_expression (tree_postfix_expression& tpe)
{
  octave_value::unary_op etype = tpe.op_type ();
  tree_expression *operand = tpe.operand ();
  jit_value *operandv = visit (operand);

  const jit_operation& fn = jit_typeinfo::unary_op (etype);
  result = create_checked (fn, operandv);

  if (etype == octave_value::op_incr || etype == octave_value::op_decr)
    {
      jit_value *ret = create_checked (&jit_typeinfo::grab, operandv);
      do_assign (operand, result);
      result = ret;
    }
}

void
jit_convert::visit_prefix_expression (tree_prefix_expression& tpe)
{
  octave_value::unary_op etype = tpe.op_type ();
  tree_expression *operand = tpe.operand ();
  const jit_operation& fn = jit_typeinfo::unary_op (etype);
  result = create_checked (fn, visit (operand));

  if (etype == octave_value::op_incr || etype == octave_value::op_decr)
    do_assign (operand, result);
}

void
jit_convert::visit_return_command (tree_return_command&)
{
  throw jit_fail_exception ("No visit_return_command implementation");
}

void
jit_convert::visit_return_list (tree_return_list&)
{
  throw jit_fail_exception ("No visit_return_list implementation");
}

void
jit_convert::visit_simple_assignment (tree_simple_assignment& tsa)
{
  tree_expression *rhs = tsa.right_hand_side ();
  jit_value *rhsv = visit (rhs);
  octave_value::assign_op op = tsa.op_type ();

  if (op != octave_value::op_asn_eq)
    {
      // do the equivlent binary operation, then assign. This is always correct,
      // but isn't always optimal.
      tree_expression *lhs = tsa.left_hand_side ();
      jit_value *lhsv = visit (lhs);
      octave_value::binary_op bop = octave_value::assign_op_to_binary_op (op);
      const jit_operation& fn = jit_typeinfo::binary_op (bop);
      rhsv = create_checked (fn, lhsv, rhsv);
    }

  result = do_assign (tsa.left_hand_side (), rhsv);
}

void
jit_convert::visit_statement (tree_statement& stmt)
{
  tree_command *cmd = stmt.command ();
  tree_expression *expr = stmt.expression ();

  if (cmd)
    visit (cmd);
  else
    {
      // stolen from tree_evaluator::visit_statement
      bool do_bind_ans = false;

      if (expr->is_identifier ())
        {
          tree_identifier *id = dynamic_cast<tree_identifier *> (expr);

          do_bind_ans = (! id->is_variable ());
        }
      else
        do_bind_ans = (! expr->is_assignment_expression ());

      jit_value *expr_result = visit (expr);

      if (do_bind_ans)
        do_assign ("ans", expr_result, expr->print_result ());
      else if (expr->is_identifier () && expr->print_result ())
        {
          // FIXME: ugly hack, we need to come up with a way to pass
          // nargout to visit_identifier
          const jit_operation& fn = jit_typeinfo::print_value ();
          jit_const_string *name = factory.create<jit_const_string>
                                    (expr->name ());
          block->append (factory.create<jit_call> (fn, name, expr_result));
        }
    }
}

void
jit_convert::visit_statement_list (tree_statement_list& lst)
{
  for (tree_statement_list::iterator iter = lst.begin (); iter != lst.end();
       ++iter)
    {
      tree_statement *elt = *iter;
      // jwe: Can this ever be null?
      assert (elt);
      elt->accept (*this);
    }
}

void
jit_convert::visit_switch_case (tree_switch_case&)
{
  throw jit_fail_exception ("No visit_switch_case implementation");
}

void
jit_convert::visit_switch_case_list (tree_switch_case_list&)
{
  throw jit_fail_exception ("No visit_switch_case_list implementation");
}

void
jit_convert::visit_switch_command (tree_switch_command& cmd)
{
  tree_switch_case_list *lst = cmd.case_list ();

  // always visit switch expression
  tree_expression *expr = cmd.switch_value ();
  assert (expr && "Switch value can not be null");
  jit_value *value = visit (expr);
  assert (value);

  size_t case_blocks_num = lst->size ();

  if (! case_blocks_num)  // there's nothing to do
    return;

  // check for otherwise, it's interpreted as last 'else' condition
  size_t has_otherwise = 0;
  tree_switch_case *last = lst->back ();
  if (last->is_default_case ())
    has_otherwise = 1;

  std::vector<jit_block *> entry_blocks (case_blocks_num + 1 - has_otherwise);

  // the first entry point is always the actual block. afterward new blocks
  // are created for every case and the otherwise branch
  entry_blocks[0] = block;
  for (size_t i = 1; i < case_blocks_num; ++i)
    entry_blocks[i] = factory.create<jit_block> ("case_cond");

  jit_block *tail = factory.create<jit_block> ("switch_tail");

  // if there's no otherwise branch, the the 'else' of the last branch
  // has to point to the tail
  if (! has_otherwise)
    entry_blocks[entry_blocks.size()-1] = tail;

  // each branch in the case statement will have different breaks/continues
  block_list current_breaks = breaks;
  block_list current_continues = continues;
  breaks.clear ();
  continues.clear ();

  size_t num_incomming = 0; // number of incomming blocks to our tail

  tree_switch_case_list::iterator iter = lst->begin ();
  for (size_t i = 0; i < case_blocks_num; ++iter, ++i)
    {
      tree_switch_case *twc = *iter;
      block = entry_blocks[i]; // case_cond
      assert (block);

      if (i)
        blocks.push_back (entry_blocks[i]);  // first block already pushed

      if (! twc->is_default_case ())
        {
          // compare result of switch expression with actual case label
          tree_expression *te = twc->case_label ();
          jit_value *label = visit (te);
          assert(label);

          const jit_operation& fn = jit_typeinfo::binary_op (octave_value::op_eq);
          jit_value *cond = create_checked (fn, value, label);
          assert(cond);

          jit_call *check = create_checked (&jit_typeinfo::logically_true,
                                            cond);

          jit_block *body = factory.create<jit_block> ("case_body");
          blocks.push_back (body);

          block->append (factory.create<jit_cond_branch> (check, body,
                                                          entry_blocks[i+1]));
          block = body; // case_body
        }

      tree_statement_list *stmt_lst = twc->commands ();
      assert(stmt_lst);

      try
        {
          stmt_lst->accept (*this);
          num_incomming++;
          block->append (factory.create<jit_branch> (tail));
        }
      catch (const jit_break_exception&)
        { }

      // each branch in the case statement will have different breaks/continues
      current_breaks.splice (current_breaks.end (), breaks);
      current_continues.splice (current_continues.end (), continues);
    }

  // each branch in the case statement will have different breaks/continues
  breaks.splice (breaks.end (), current_breaks);
  continues.splice (continues.end (), current_continues);

  if (num_incomming || ! has_otherwise)
    {
      blocks.push_back (tail);
      block = tail; // switch_tail
    }
  else
    throw jit_break_exception ();   // every branch broke
}

void
jit_convert::visit_try_catch_command (tree_try_catch_command&)
{
  throw jit_fail_exception ("No visit_try_catch_command implementation");
}

void
jit_convert::visit_unwind_protect_command (tree_unwind_protect_command&)
{
  throw jit_fail_exception ("No visit_unwind_protect_command implementation");
}

void
jit_convert::visit_while_command (tree_while_command& wc)
{
  unwind_protect prot;
  prot.protect_var (breaks);
  prot.protect_var (continues);
  breaks.clear ();
  continues.clear ();

  jit_block *cond_check = factory.create<jit_block> ("while_cond_check");
  block->append (factory.create<jit_branch> (cond_check));
  blocks.push_back (cond_check);
  block = cond_check;

  tree_expression *expr = wc.condition ();
  assert (expr && "While expression can not be null");
  jit_value *check = visit (expr);
  check = create_checked (&jit_typeinfo::logically_true, check);

  jit_block *body = factory.create<jit_block> ("while_body");
  blocks.push_back (body);

  jit_block *tail = factory.create<jit_block> ("while_tail");
  block->append (factory.create<jit_cond_branch> (check, body, tail));
  block = body;

  tree_statement_list *loop_body = wc.body ();
  bool all_breaking = false;
  if (loop_body)
    {
      try
        {
          loop_body->accept (*this);
        }
      catch (const jit_break_exception&)
        {
          all_breaking = true;
        }
    }

  finish_breaks (tail, breaks);

  if (! all_breaking || continues.size ())
    {
      jit_block *interrupt_check
        = factory.create<jit_block> ("interrupt_check");
      blocks.push_back (interrupt_check);
      finish_breaks (interrupt_check, continues);
      if (! all_breaking)
        block->append (factory.create<jit_branch> (interrupt_check));

      block = interrupt_check;
      jit_error_check *ec
        = factory.create<jit_error_check> (jit_error_check::var_interrupt,
                                           cond_check, final_block);
      block->append (ec);
    }

  blocks.push_back (tail);
  block = tail;
}

void
jit_convert::visit_do_until_command (tree_do_until_command& duc)
{
  unwind_protect prot;
  prot.protect_var (breaks);
  prot.protect_var (continues);
  breaks.clear ();
  continues.clear ();

  jit_block *body = factory.create<jit_block> ("do_until_body");
  jit_block *cond_check = factory.create<jit_block> ("do_until_cond_check");
  jit_block *tail = factory.create<jit_block> ("do_until_tail");

  block->append (factory.create<jit_branch> (body));
  blocks.push_back (body);
  block = body;

  tree_statement_list *loop_body = duc.body ();
  bool all_breaking = false;
  if (loop_body)
    {
      try
        {
          loop_body->accept (*this);
        }
      catch (const jit_break_exception&)
        {
          all_breaking = true;
        }
    }

  finish_breaks (tail, breaks);

  if (! all_breaking || continues.size ())
    {
      jit_block *interrupt_check
        = factory.create<jit_block> ("interrupt_check");
      blocks.push_back (interrupt_check);
      finish_breaks (interrupt_check, continues);
      if (! all_breaking)
        block->append (factory.create<jit_branch> (interrupt_check));

      block = interrupt_check;
      jit_error_check *ec
        = factory.create<jit_error_check> (jit_error_check::var_interrupt,
                                           cond_check, final_block);
      block->append (ec);

      blocks.push_back (cond_check);
      block = cond_check;

      tree_expression *expr = duc.condition ();
      assert (expr && "Do-Until expression can not be null");
      jit_value *check = visit (expr);
      check = create_checked (&jit_typeinfo::logically_true, check);

      block->append (factory.create<jit_cond_branch> (check, tail, body));
    }

  blocks.push_back (tail);
  block = tail;
}

void
jit_convert::initialize (symbol_table::scope_id s)
{
  scope = s;
  iterator_count = 0;
  for_bounds_count = 0;
  short_count = 0;
  jit_instruction::reset_ids ();

  entry_block = factory.create<jit_block> ("body");
  final_block = factory.create<jit_block> ("final");
  blocks.push_back (entry_block);
  entry_block->mark_alive ();
  block = entry_block;
}

jit_call *
jit_convert::create_checked_impl (jit_call *ret)
{
  block->append (ret);

  jit_block *normal = factory.create<jit_block> (block->name ());
  jit_error_check *check
    = factory.create<jit_error_check> (jit_error_check::var_error_state, ret,
                                       normal, final_block);
  block->append (check);
  blocks.push_back (normal);
  block = normal;

  return ret;
}

jit_variable *
jit_convert::find_variable (const std::string& vname) const
{
  variable_map::const_iterator iter;
  iter = vmap.find (vname);
  return iter != vmap.end () ? iter->second : 0;
}

jit_variable *
jit_convert::get_variable (const std::string& vname)
{
  jit_variable *ret = find_variable (vname);
  if (ret)
    return ret;

  symbol_table::symbol_record record = symbol_table::find_symbol (vname, scope);
  if (record.is_persistent () || record.is_global ())
    throw jit_fail_exception ("Persistent and global not yet supported");

  if (converting_function)
    return create_variable (vname, jit_typeinfo::get_any (), false);
  else
    {
      octave_value val = record.varval ();
      if (val.is_undefined ())
        val = symbol_table::find_function (vname);

      jit_type *type = jit_typeinfo::type_of (val);
      bounds.push_back (type_bound (type, vname));

      return create_variable (vname, type);
    }
}

jit_variable *
jit_convert::create_variable (const std::string& vname, jit_type *type,
                              bool isarg)
{
  jit_variable *var = factory.create<jit_variable> (vname);

  if (isarg)
    {
      jit_extract_argument *extract;
      extract = factory.create<jit_extract_argument> (type, var);
      entry_block->prepend (extract);
    }
  else
    {
      jit_call *init = factory.create<jit_call> (&jit_typeinfo::create_undef);
      jit_assign *assign = factory.create<jit_assign> (var, init);
      entry_block->prepend (assign);
      entry_block->prepend (init);
    }

  return vmap[vname] = var;
}

std::string
jit_convert::next_name (const char *prefix, size_t& count, bool inc)
{
  std::stringstream ss;
  ss << prefix << count;
  if (inc)
    ++count;
  return ss.str ();
}

jit_instruction *
jit_convert::resolve (tree_index_expression& exp, jit_value *extra_arg,
                      bool lhs)
{
  std::string type = exp.type_tags ();
  if (! (type.size () == 1 && type[0] == '('))
    throw jit_fail_exception ("Unsupported index operation");

  std::list<tree_argument_list *> args = exp.arg_lists ();
  if (args.size () != 1)
    throw jit_fail_exception ("Bad number of arguments in "
                              "tree_index_expression");

  tree_argument_list *arg_list = args.front ();
  if (! arg_list)
    throw jit_fail_exception ("null argument list");

  if (arg_list->size () < 1)
    throw jit_fail_exception ("Empty arg_list");

  tree_expression *tree_object = exp.expression ();
  jit_value *object;
  if (lhs)
    {
      tree_identifier *id = dynamic_cast<tree_identifier *> (tree_object);
      if (! id)
        throw jit_fail_exception ("expected identifier");
      object = get_variable (id->name ());
    }
  else
    object = visit (tree_object);

  size_t narg = arg_list->size ();
  tree_argument_list::iterator iter = arg_list->begin ();
  bool have_extra = extra_arg;
  std::vector<jit_value *> call_args (narg + 1 + have_extra);
  call_args[0] = object;

  for (size_t idx = 0; iter != arg_list->end (); ++idx, ++iter)
    {
      unwind_protect prot;
      prot.add_method (&end_context,
                       &std::vector<jit_magic_end::context>::pop_back);

      jit_magic_end::context ctx (factory, object, idx, narg);
      end_context.push_back (ctx);
      call_args[idx + 1] = visit (*iter);
    }

  if (extra_arg)
    call_args[call_args.size () - 1] = extra_arg;

  const jit_operation& fres = lhs ? jit_typeinfo::paren_subsasgn ()
                                  : jit_typeinfo::paren_subsref ();

  return create_checked (fres, call_args);
}

jit_value *
jit_convert::do_assign (tree_expression *exp, jit_value *rhs, bool artificial)
{
  if (! exp)
    throw jit_fail_exception ("NULL lhs in assign");

  if (isa<tree_identifier> (exp))
    return do_assign (exp->name (), rhs, exp->print_result (), artificial);
  else if (tree_index_expression *idx
           = dynamic_cast<tree_index_expression *> (exp))
    {
      jit_value *new_object = resolve (*idx, rhs, true);
      do_assign (idx->expression (), new_object, true);

      // FIXME: Will not work for values that must be release/grabed
      return rhs;
    }
  else
    throw jit_fail_exception ("Unsupported assignment");
}

jit_value *
jit_convert::do_assign (const std::string& lhs, jit_value *rhs,
                        bool print, bool artificial)
{
  jit_variable *var = get_variable (lhs);
  jit_assign *assign = block->append (factory.create<jit_assign> (var, rhs));

  if (artificial)
    assign->mark_artificial ();

  if (print)
    {
      const jit_operation& print_fn = jit_typeinfo::print_value ();
      jit_const_string *name = factory.create<jit_const_string> (lhs);
      block->append (factory.create<jit_call> (print_fn, name, var));
    }

  return var;
}

jit_value *
jit_convert::visit (tree& tee)
{
  unwind_protect prot;
  prot.protect_var (result);

  tee.accept (*this);
  return result;
}

void
jit_convert::finish_breaks (jit_block *dest, const block_list& lst)
{
  for (block_list::const_iterator iter = lst.begin (); iter != lst.end ();
       ++iter)
    {
      jit_block *b = *iter;
      b->append (factory.create<jit_branch> (dest));
    }
}

// -------------------- jit_convert_llvm --------------------
llvm::Function *
jit_convert_llvm::convert_loop (llvm::Module *module,
                                const jit_block_list& blocks,
                                const std::list<jit_value *>& constants)
{
  converting_function = false;

  // for now just init arguments from entry, later we will have to do something
  // more interesting
  jit_block *entry_block = blocks.front ();
  for (jit_block::iterator iter = entry_block->begin ();
       iter != entry_block->end (); ++iter)
    if (jit_extract_argument *extract
        = dynamic_cast<jit_extract_argument *> (*iter))
      argument_vec.push_back (std::make_pair (extract->name (), true));


  jit_type *any = jit_typeinfo::get_any ();

  // argument is an array of octave_base_value*, or octave_base_value**
  llvm::Type *arg_type = any->to_llvm (); // this is octave_base_value*
  arg_type = arg_type->getPointerTo ();
  llvm::FunctionType *ft;
  ft = llvm::FunctionType::get (llvm::Type::getVoidTy (context), arg_type,
                                false);
  function = llvm::Function::Create (ft, llvm::Function::ExternalLinkage,
                                     "foobar", module);

  try
    {
      prelude = llvm::BasicBlock::Create (context, "prelude", function);
      builder.SetInsertPoint (prelude);

      llvm::Value *arg = function->arg_begin ();
      for (size_t i = 0; i < argument_vec.size (); ++i)
        {
          llvm::Value *loaded_arg = builder.CreateConstInBoundsGEP1_32 (arg, i);
          arguments[argument_vec[i].first] = loaded_arg;
        }

      convert (blocks, constants);
    }
  catch (const jit_fail_exception& e)
    {
      function->eraseFromParent ();
      throw;
    }

  return function;
}


jit_function
jit_convert_llvm::convert_function (llvm::Module *module,
                                    const jit_block_list& blocks,
                                    const std::list<jit_value *>& constants,
                                    octave_user_function& fcn,
                                    const std::vector<jit_type *>& args)
{
  converting_function = true;

  jit_block *final_block = blocks.back ();
  jit_return *ret = dynamic_cast<jit_return *> (final_block->back ());
  assert (ret);

  creating = jit_function (module, jit_convention::internal,
                           "foobar", ret->result_type (), args);
  function = creating.to_llvm ();

  try
    {
      prelude = creating.new_block ("prelude");
      builder.SetInsertPoint (prelude);

      tree_parameter_list *plist = fcn.parameter_list ();
      if (plist)
        {
          tree_parameter_list::iterator piter = plist->begin ();
          tree_parameter_list::iterator pend = plist->end ();
          for (size_t i = 0; i < args.size () && piter != pend; ++i, ++piter)
            {
              tree_decl_elt *elt = *piter;
              std::string arg_name = elt->name ();
              arguments[arg_name] = creating.argument (builder, i);
            }
        }

      convert (blocks, constants);
    }
  catch (const jit_fail_exception& e)
    {
      function->eraseFromParent ();
      throw;
    }

  return creating;
}

void
jit_convert_llvm::convert (const jit_block_list& blocks,
                           const std::list<jit_value *>& constants)
{
  std::list<jit_block *>::const_iterator biter;
  for (biter = blocks.begin (); biter != blocks.end (); ++biter)
    {
      jit_block *jblock = *biter;
      llvm::BasicBlock *block = llvm::BasicBlock::Create (context,
                                                          jblock->name (),
                                                          function);
      jblock->stash_llvm (block);
    }

  jit_block *first = *blocks.begin ();
  builder.CreateBr (first->to_llvm ());

  // constants aren't in the IR, we visit those first
  for (std::list<jit_value *>::const_iterator iter = constants.begin ();
       iter != constants.end (); ++iter)
    if (! isa<jit_instruction> (*iter))
      visit (*iter);

  // convert all instructions
  for (biter = blocks.begin (); biter != blocks.end (); ++biter)
    visit (*biter);

  // now finish phi nodes
  for (biter = blocks.begin (); biter != blocks.end (); ++biter)
    {
      jit_block& block = **biter;
      for (jit_block::iterator piter = block.begin ();
           piter != block.end () && isa<jit_phi> (*piter); ++piter)
        {
          jit_instruction *phi = *piter;
          finish_phi (static_cast<jit_phi *> (phi));
        }
    }
}

void
jit_convert_llvm::finish_phi (jit_phi *phi)
{
  llvm::PHINode *llvm_phi = phi->to_llvm ();
  for (size_t i = 0; i < phi->argument_count (); ++i)
    {
      llvm::BasicBlock *pred = phi->incomming_llvm (i);
      llvm_phi->addIncoming (phi->argument_llvm (i), pred);
    }
}

void
jit_convert_llvm::visit (jit_const_string& cs)
{
  cs.stash_llvm (builder.CreateGlobalStringPtr (cs.value ()));
}

void
jit_convert_llvm::visit (jit_const_bool& cb)
{
  cb.stash_llvm (llvm::ConstantInt::get (cb.type_llvm (), cb.value ()));
}

void
jit_convert_llvm::visit (jit_const_scalar& cs)
{
  cs.stash_llvm (llvm::ConstantFP::get (cs.type_llvm (), cs.value ()));
}

void
jit_convert_llvm::visit (jit_const_complex& cc)
{
  llvm::Type *scalar_t = jit_typeinfo::get_scalar_llvm ();
  Complex value = cc.value ();
  llvm::Value *real = llvm::ConstantFP::get (scalar_t, value.real ());
  llvm::Value *imag = llvm::ConstantFP::get (scalar_t, value.imag ());
  cc.stash_llvm (jit_typeinfo::create_complex (real, imag));
}

void jit_convert_llvm::visit (jit_const_index& ci)
{
  ci.stash_llvm (llvm::ConstantInt::get (ci.type_llvm (), ci.value ()));
}

void
jit_convert_llvm::visit (jit_const_range& cr)
{
  llvm::StructType *stype = llvm::cast<llvm::StructType>(cr.type_llvm ());
  llvm::Type *scalar_t = jit_typeinfo::get_scalar_llvm ();
  llvm::Type *idx = jit_typeinfo::get_index_llvm ();
  const jit_range& rng = cr.value ();

  llvm::Constant *constants[4];
  constants[0] = llvm::ConstantFP::get (scalar_t, rng.base);
  constants[1] = llvm::ConstantFP::get (scalar_t, rng.limit);
  constants[2] = llvm::ConstantFP::get (scalar_t, rng.inc);
  constants[3] = llvm::ConstantInt::get (idx, rng.nelem);

  llvm::Value *as_llvm;
  as_llvm = llvm::ConstantStruct::get (stype,
                                       llvm::makeArrayRef (constants, 4));
  cr.stash_llvm (as_llvm);
}

void
jit_convert_llvm::visit (jit_block& b)
{
  llvm::BasicBlock *block = b.to_llvm ();
  builder.SetInsertPoint (block);
  for (jit_block::iterator iter = b.begin (); iter != b.end (); ++iter)
    visit (*iter);
}

void
jit_convert_llvm::visit (jit_branch& b)
{
  b.stash_llvm (builder.CreateBr (b.successor_llvm ()));
}

void
jit_convert_llvm::visit (jit_cond_branch& cb)
{
  llvm::Value *cond = cb.cond_llvm ();
  llvm::Value *br;
  br = builder.CreateCondBr (cond, cb.successor_llvm (0),
                             cb.successor_llvm (1));
  cb.stash_llvm (br);
}

void
jit_convert_llvm::visit (jit_call& call)
{
  const jit_function& ol = call.overload ();

  std::vector<jit_value *> args (call.arguments ().size ());
  for (size_t i = 0; i < args.size (); ++i)
    args[i] = call.argument (i);

  llvm::Value *ret = ol.call (builder, args);
  call.stash_llvm (ret);
}

void
jit_convert_llvm::visit (jit_extract_argument& extract)
{
  llvm::Value *arg = arguments[extract.name ()];
  assert (arg);

  if (converting_function)
    extract.stash_llvm (arg);
  else
    {
      arg = builder.CreateLoad (arg);

      const jit_function& ol = extract.overload ();
      extract.stash_llvm (ol.call (builder, arg));
    }
}

void
jit_convert_llvm::visit (jit_store_argument& store)
{
  const jit_function& ol = store.overload ();
  llvm::Value *arg_value = ol.call (builder, store.result ());
  llvm::Value *arg = arguments[store.name ()];
  store.stash_llvm (builder.CreateStore (arg_value, arg));
}

void
jit_convert_llvm::visit (jit_return& ret)
{
  jit_value *res = ret.result ();

  if (converting_function)
    creating.do_return (builder, res->to_llvm (), false);
  else
    {
      if (res)
        builder.CreateRet (res->to_llvm ());
      else
        builder.CreateRetVoid ();
    }
}

void
jit_convert_llvm::visit (jit_phi& phi)
{
  // we might not have converted all incoming branches, so we don't
  // set incomming branches now
  llvm::PHINode *node = llvm::PHINode::Create (phi.type_llvm (),
                                               phi.argument_count ());
  builder.Insert (node);
  phi.stash_llvm (node);
}

void
jit_convert_llvm::visit (jit_variable&)
{
  throw jit_fail_exception ("ERROR: SSA construction should remove all variables");
}

void
jit_convert_llvm::visit (jit_error_check& check)
{
  llvm::Value *cond;

  switch (check.check_variable ())
    {
    case jit_error_check::var_error_state:
      cond = jit_typeinfo::insert_error_check (builder);
      break;
    case jit_error_check::var_interrupt:
      cond = jit_typeinfo::insert_interrupt_check (builder);
      break;
    default:
      panic_impossible ();
    }

  llvm::Value *br = builder.CreateCondBr (cond, check.successor_llvm (0),
                                          check.successor_llvm (1));
  check.stash_llvm (br);
}

void
jit_convert_llvm::visit (jit_assign& assign)
{
  jit_value *new_value = assign.src ();
  assign.stash_llvm (new_value->to_llvm ());

  if (assign.artificial ())
    return;

  jit_value *overwrite = assign.overwrite ();
  if (isa<jit_assign_base> (overwrite))
    {
      const jit_function& ol = jit_typeinfo::get_release (overwrite->type ());
      if (ol.valid ())
        ol.call (builder, overwrite);
    }
}

void
jit_convert_llvm::visit (jit_argument&)
{}

void
jit_convert_llvm::visit (jit_magic_end& me)
{
  const jit_function& ol = me.overload ();

  jit_magic_end::context ctx = me.resolve_context ();
  llvm::Value *ret = ol.call (builder, ctx.value, ctx.index, ctx.count);
  me.stash_llvm (ret);
}

// -------------------- jit_infer --------------------
jit_infer::jit_infer (jit_factory& afactory, jit_block_list& ablocks,
                      const variable_map& avmap)
  : blocks (ablocks), factory (afactory), vmap (avmap) { }

void
jit_infer::infer (void)
{
  construct_ssa ();

  // initialize the worklist to instructions derived from constants
  const std::list<jit_value *>& constants = factory.constants ();
  for (std::list<jit_value *>::const_iterator iter = constants.begin ();
       iter != constants.end (); ++iter)
    append_users (*iter);

  // the entry block terminator may be a regular branch statement
  if (entry_block ().terminator ())
    push_worklist (entry_block ().terminator ());

  // FIXME: Describe algorithm here
  while (worklist.size ())
    {
      jit_instruction *next = worklist.front ();
      worklist.pop_front ();
      next->stash_in_worklist (false);

      if (next->infer ())
        {
          // terminators need to be handles specially
          if (jit_terminator *term = dynamic_cast<jit_terminator *> (next))
            append_users_term (term);
          else
            append_users (next);
        }
    }

  remove_dead ();
  blocks.label ();
  place_releases ();
  simplify_phi ();
}

void
jit_infer::append_users (jit_value *v)
{
  for (jit_use *use = v->first_use (); use; use = use->next ())
    push_worklist (use->user ());
}

void
jit_infer::append_users_term (jit_terminator *term)
{
  for (size_t i = 0; i < term->successor_count (); ++i)
    {
      if (term->alive (i))
        {
          jit_block *succ = term->successor (i);
          for (jit_block::iterator iter = succ->begin ();
               iter != succ->end () && isa<jit_phi> (*iter); ++iter)
            push_worklist (*iter);

          jit_terminator *sterm = succ->terminator ();
          if (sterm)
            push_worklist (sterm);
        }
    }
}

void
jit_infer::construct_ssa (void)
{
  blocks.label ();
  final_block ().compute_idom (entry_block ());
  entry_block ().compute_df ();
  entry_block ().create_dom_tree ();

  // insert phi nodes where needed, this is done on a per variable basis
  for (variable_map::const_iterator iter = vmap.begin (); iter != vmap.end ();
       ++iter)
    {
      jit_block::df_set visited, added_phi;
      std::list<jit_block *> ssa_worklist;
      iter->second->use_blocks (visited);
      ssa_worklist.insert (ssa_worklist.begin (), visited.begin (),
                           visited.end ());

      while (ssa_worklist.size ())
        {
          jit_block *b = ssa_worklist.front ();
          ssa_worklist.pop_front ();

          for (jit_block::df_iterator diter = b->df_begin ();
               diter != b->df_end (); ++diter)
            {
              jit_block *dblock = *diter;
              if (! added_phi.count (dblock))
                {
                  jit_phi *phi = factory.create<jit_phi> (iter->second,
                                                          dblock->use_count ());
                  dblock->prepend (phi);
                  added_phi.insert (dblock);
                }

              if (! visited.count (dblock))
                {
                  ssa_worklist.push_back (dblock);
                  visited.insert (dblock);
                }
            }
        }
    }

  do_construct_ssa (entry_block (), entry_block ().visit_count ());
}

void
jit_infer::do_construct_ssa (jit_block& ablock, size_t avisit_count)
{
  if (ablock.visited (avisit_count))
    return;

  // replace variables with their current SSA value
  for (jit_block::iterator iter = ablock.begin (); iter != ablock.end ();
       ++iter)
    {
      jit_instruction *instr = *iter;
      instr->construct_ssa ();
      instr->push_variable ();
    }

  // finish phi nodes of successors
  for (size_t i = 0; i < ablock.successor_count (); ++i)
    {
      jit_block *finish = ablock.successor (i);

      for (jit_block::iterator iter = finish->begin ();
           iter != finish->end () && isa<jit_phi> (*iter);)
        {
          jit_phi *phi = static_cast<jit_phi *> (*iter);
          jit_variable *var = phi->dest ();
          ++iter;

          if (var->has_top ())
            phi->add_incomming (&ablock, var->top ());
          else
            {
              // temporaries may have extranious phi nodes which can be removed
              assert (! phi->use_count ());
              assert (var->name ().size () && var->name ()[0] == '#');
              phi->remove ();
            }
        }
    }

  for (size_t i = 0; i < ablock.dom_successor_count (); ++i)
    do_construct_ssa (*ablock.dom_successor (i), avisit_count);

  ablock.pop_all ();
}

void
jit_infer::place_releases (void)
{
  std::set<jit_value *> temporaries;
  for (jit_block_list::iterator iter = blocks.begin (); iter != blocks.end ();
       ++iter)
    {
      jit_block& ablock = **iter;
      if (ablock.id () != jit_block::NO_ID)
        {
          release_temp (ablock, temporaries);
          release_dead_phi (ablock);
        }
    }
}

void
jit_infer::push_worklist (jit_instruction *instr)
{
  if (! instr->in_worklist ())
    {
      instr->stash_in_worklist (true);
      worklist.push_back (instr);
    }
}

void
jit_infer::remove_dead ()
{
  jit_block_list::iterator biter;
  for (biter = blocks.begin (); biter != blocks.end (); ++biter)
    {
      jit_block *b = *biter;
      if (b->alive ())
        {
          for (jit_block::iterator iter = b->begin ();
               iter != b->end () && isa<jit_phi> (*iter);)
            {
              jit_phi *phi = static_cast<jit_phi *> (*iter);
              if (phi->prune ())
                iter = b->remove (iter);
              else
                ++iter;
            }
        }
    }

  for (biter = blocks.begin (); biter != blocks.end ();)
    {
      jit_block *b = *biter;
      if (b->alive ())
        {
          // FIXME: A special case for jit_error_check, if we generalize to
          // we will need to change!
          jit_terminator *term = b->terminator ();
          if (term && term->successor_count () == 2 && ! term->alive (0))
            {
              jit_block *succ = term->successor (1);
              term->remove ();
              jit_branch *abreak = factory.create<jit_branch> (succ);
              b->append (abreak);
              abreak->infer ();
            }

          ++biter;
        }
      else
        {
          jit_terminator *term = b->terminator ();
          if (term)
            term->remove ();
          biter = blocks.erase (biter);
        }
    }
}

void
jit_infer::release_dead_phi (jit_block& ablock)
{
  jit_block::iterator iter = ablock.begin ();
  while (iter != ablock.end () && isa<jit_phi> (*iter))
    {
      jit_phi *phi = static_cast<jit_phi *> (*iter);
      ++iter;

      jit_use *use = phi->first_use ();
      if (phi->use_count () == 1 && isa<jit_assign> (use->user ()))
        {
          // instead of releasing on assign, release on all incomming branches,
          // this can get rid of casts inside loops
          for (size_t i = 0; i < phi->argument_count (); ++i)
            {
              jit_value *arg = phi->argument (i);
              if (! arg->needs_release ())
                continue;

              jit_block *inc = phi->incomming (i);
              jit_block *split = inc->maybe_split (factory, blocks, ablock);
              jit_terminator *term = split->terminator ();
              jit_call *release
                = factory.create<jit_call> (jit_typeinfo::release, arg);
              release->infer ();
              split->insert_before (term, release);
            }

          phi->replace_with (0);
          phi->remove ();
        }
    }
}

void
jit_infer::release_temp (jit_block& ablock, std::set<jit_value *>& temp)
{
  for (jit_block::iterator iter = ablock.begin (); iter != ablock.end ();
       ++iter)
    {
      jit_instruction *instr = *iter;

      // check for temporaries that require release and live across
      // multiple blocks
      if (instr->needs_release ())
        {
          jit_block *fu_block = instr->first_use_block ();
          if (fu_block && fu_block != &ablock && instr->needs_release ())
            temp.insert (instr);
        }

      if (isa<jit_call> (instr))
        {
          // place releases for temporary arguments
          for (size_t i = 0; i < instr->argument_count (); ++i)
            {
              jit_value *arg = instr->argument (i);
              if (! arg->needs_release ())
                continue;

              jit_call *release
                = factory.create<jit_call> (&jit_typeinfo::release, arg);
              release->infer ();
              ablock.insert_after (iter, release);
              ++iter;
              temp.erase (arg);
            }
        }
    }

  if (! temp.size () || ! isa<jit_error_check> (ablock.terminator ()))
    return;

  // FIXME: If we support try/catch or unwind_protect final_block
  //        may not be the destination
  jit_block *split = ablock.maybe_split (factory, blocks, final_block ());
  jit_terminator *term = split->terminator ();
  for (std::set<jit_value *>::const_iterator iter = temp.begin ();
       iter != temp.end (); ++iter)
    {
      jit_value *value = *iter;
      jit_call *release
        = factory.create<jit_call> (&jit_typeinfo::release, value);
      split->insert_before (term, release);
      release->infer ();
    }
}

void
jit_infer::simplify_phi (void)
{
  for (jit_block_list::iterator biter = blocks.begin (); biter != blocks.end ();
       ++biter)
    {
      jit_block &ablock = **biter;
      for (jit_block::iterator iter = ablock.begin ();
           iter != ablock.end () && isa<jit_phi> (*iter); ++iter)
        simplify_phi (*static_cast<jit_phi *> (*iter));
    }
}

void
jit_infer::simplify_phi (jit_phi& phi)
{
  jit_block& pblock = *phi.parent ();
  const jit_operation& cast_fn = jit_typeinfo::cast (phi.type ());
  jit_variable *dest = phi.dest ();
  for (size_t i = 0; i < phi.argument_count (); ++i)
    {
      jit_value *arg = phi.argument (i);
      if (arg->type () != phi.type ())
        {
          jit_block *pred = phi.incomming (i);
          jit_block *split = pred->maybe_split (factory, blocks, pblock);
          jit_terminator *term = split->terminator ();
          jit_instruction *cast = factory.create<jit_call> (cast_fn, arg);
          jit_assign *assign = factory.create<jit_assign> (dest, cast);

          split->insert_before (term, cast);
          split->insert_before (term, assign);
          cast->infer ();
          assign->infer ();
          phi.stash_argument (i, assign);
        }
    }
}

// -------------------- tree_jit --------------------

tree_jit::tree_jit (void) : module (0), engine (0)
{
}

tree_jit::~tree_jit (void)
{}

bool
tree_jit::execute (tree_simple_for_command& cmd, const octave_value& bounds)
{
  return instance ().do_execute (cmd, bounds);
}

bool
tree_jit::execute (tree_while_command& cmd)
{
  return instance ().do_execute (cmd);
}

bool
tree_jit::execute (octave_user_function& fcn, const octave_value_list& args,
                   octave_value_list& retval)
{
  return instance ().do_execute (fcn, args, retval);
}

tree_jit&
tree_jit::instance (void)
{
  static tree_jit ret;
  return ret;
}

bool
tree_jit::initialize (void)
{
  if (engine)
    return true;

  if (! module)
    {
      llvm::InitializeNativeTarget ();
      module = new llvm::Module ("octave", context);
    }

  // sometimes this fails pre main
  engine = llvm::ExecutionEngine::createJIT (module);

  if (! engine)
    return false;

#ifdef LEGACY_PASSMANAGER
  module_pass_manager = new llvm::legacy::PassManager ();
  pass_manager = new llvm::legacy::FunctionPassManager (module);
#else
  module_pass_manager = new llvm::PassManager ();
  pass_manager = new llvm::FunctionPassManager (module);
#endif
  module_pass_manager->add (llvm::createAlwaysInlinerPass ());

#ifdef HAVE_LLVM_DATALAYOUT
  pass_manager->add (new llvm::DataLayout (*engine->getDataLayout ()));
#else
  pass_manager->add (new llvm::TargetData (*engine->getTargetData ()));
#endif
  pass_manager->add (llvm::createCFGSimplificationPass ());
  pass_manager->add (llvm::createBasicAliasAnalysisPass ());
  pass_manager->add (llvm::createPromoteMemoryToRegisterPass ());
  pass_manager->add (llvm::createInstructionCombiningPass ());
  pass_manager->add (llvm::createReassociatePass ());
  pass_manager->add (llvm::createGVNPass ());
  pass_manager->add (llvm::createCFGSimplificationPass ());
  pass_manager->doInitialization ();

  jit_typeinfo::initialize (module, engine);

  return true;
}

bool
tree_jit::do_execute (tree_simple_for_command& cmd, const octave_value& bounds)
{
  size_t tc = trip_count (bounds);
  if (! tc || ! initialize () || ! enabled ())
    return false;

  jit_info::vmap extra_vars;
  extra_vars["#for_bounds0"] = &bounds;

  jit_info *info = cmd.get_info ();
  if (! info || ! info->match (extra_vars))
    {
      if (tc < static_cast<size_t> (Vjit_startcnt))
        return false;

      delete info;
      info = new jit_info (*this, cmd, bounds);
      cmd.stash_info (info);
    }

  return info->execute (extra_vars);
}

bool
tree_jit::do_execute (tree_while_command& cmd)
{
  if (! initialize () || ! enabled ())
    return false;

  jit_info *info = cmd.get_info ();
  if (! info || ! info->match ())
    {
      delete info;
      info = new jit_info (*this, cmd);
      cmd.stash_info (info);
    }

  return info->execute ();
}

bool
tree_jit::do_execute (octave_user_function& fcn, const octave_value_list& args,
                      octave_value_list& retval)
{
  if (! initialize () || ! enabled ())
    return false;

  jit_function_info *info = fcn.get_info ();
  if (! info || ! info->match (args))
    {
      delete info;
      info = new jit_function_info (*this, fcn, args);
      fcn.stash_info (info);
    }

  return info->execute (args, retval);
}

bool
tree_jit::enabled (void)
{
  // Ideally, we should only disable JIT if there is a breakpoint in the code we
  // are about to run. However, we can't figure this out in O(1) time, so we
  // conservatively check for the existence of any breakpoints.
  return Vjit_enable && ! bp_table::have_breakpoints ()
         && ! Vdebug_on_interrupt && ! Vdebug_on_error;
}

size_t
tree_jit::trip_count (const octave_value& bounds) const
{
  if (bounds.is_range ())
    {
      Range rng = bounds.range_value ();
      return rng.nelem ();
    }

  // unsupported type
  return 0;
}


void
tree_jit::optimize (llvm::Function *fn)
{
  if (Vdebug_jit)
    llvm::verifyModule (*module);

  module_pass_manager->run (*module);
  pass_manager->run (*fn);

  if (Vdebug_jit)
    {
      std::string error;
#ifdef RAW_FD_OSTREAM_ARG_IS_LLVM_SYS_FS
      llvm::raw_fd_ostream fout ("test.bc", error,
                                 llvm::sys::fs::F_Binary);
#else
      llvm::raw_fd_ostream fout ("test.bc", error,
                                 llvm::raw_fd_ostream::F_Binary);
#endif
      llvm::WriteBitcodeToFile (module, fout);
    }
}

// -------------------- jit_function_info --------------------
jit_function_info::jit_function_info (tree_jit& tjit,
                                      octave_user_function& fcn,
                                      const octave_value_list& ov_args)
  : argument_types (ov_args.length ()), function (0)
{
  size_t nargs = ov_args.length ();
  for (size_t i = 0; i < nargs; ++i)
    argument_types[i] = jit_typeinfo::type_of (ov_args(i));

  jit_function raw_fn;
  jit_function wrapper;

  try
    {
      jit_convert conv (fcn, argument_types);
      jit_infer infer (conv.get_factory (), conv.get_blocks (),
                       conv.get_variable_map ());
      infer.infer ();

      if (Vdebug_jit)
        {
          jit_block_list& blocks = infer.get_blocks ();
          blocks.label ();
          std::cout << "-------------------- Compiling function ";
          std::cout << "--------------------\n";

          tree_print_code tpc (std::cout);
          tpc.visit_octave_user_function_header (fcn);
          tpc.visit_statement_list (*fcn.body ());
          tpc.visit_octave_user_function_trailer (fcn);
          blocks.print (std::cout, "octave jit ir");
        }

      jit_factory& factory = conv.get_factory ();
      llvm::Module *module = tjit.get_module ();
      jit_convert_llvm to_llvm;
      raw_fn = to_llvm.convert_function (module, infer.get_blocks (),
                                         factory.constants (), fcn,
                                         argument_types);

      if (Vdebug_jit)
        {
          std::cout << "-------------------- raw function ";
          std::cout << "--------------------\n";
          std::cout << *raw_fn.to_llvm () << std::endl;
          llvm::verifyFunction (*raw_fn.to_llvm ());
        }

      std::string wrapper_name = fcn.name () + "_wrapper";
      jit_type *any_t = jit_typeinfo::get_any ();
      std::vector<jit_type *> wrapper_args (1, jit_typeinfo::get_any_ptr ());
      wrapper = jit_function (module, jit_convention::internal, wrapper_name,
                              any_t, wrapper_args);

      llvm::BasicBlock *wrapper_body = wrapper.new_block ();
      builder.SetInsertPoint (wrapper_body);

      llvm::Value *wrapper_arg = wrapper.argument (builder, 0);
      std::vector<llvm::Value *> raw_args (nargs);
      for (size_t i = 0; i < nargs; ++i)
        {
          llvm::Value *arg;
          arg = builder.CreateConstInBoundsGEP1_32 (wrapper_arg, i);
          arg = builder.CreateLoad (arg);

          jit_type *arg_type = argument_types[i];
          const jit_function& cast = jit_typeinfo::cast (arg_type, any_t);
          raw_args[i] = cast.call (builder, arg);
        }

      llvm::Value *result = raw_fn.call (builder, raw_args);
      if (raw_fn.result ())
        {
          jit_type *raw_result_t = raw_fn.result ();
          const jit_function& cast = jit_typeinfo::cast (any_t, raw_result_t);
          result = cast.call (builder, result);
        }
      else
        {
          llvm::Value *zero = builder.getInt32 (0);
          result = builder.CreateBitCast (zero, any_t->to_llvm ());
        }

      wrapper.do_return (builder, result);

      llvm::Function *llvm_function = wrapper.to_llvm ();
      tjit.optimize (llvm_function);

      if (Vdebug_jit)
        {
          std::cout << "-------------------- optimized and wrapped ";
          std::cout << "--------------------\n";
          std::cout << *llvm_function << std::endl;
          llvm::verifyFunction (*llvm_function);
        }

      llvm::ExecutionEngine* engine = tjit.get_engine ();
      void *void_fn = engine->getPointerToFunction (llvm_function);
      function = reinterpret_cast<jited_function> (void_fn);
    }
  catch (const jit_fail_exception& e)
    {
      argument_types.clear ();

      if (Vdebug_jit)
        {
          if (e.known ())
            std::cout << "jit fail: " << e.what () << std::endl;
        }

      Vjit_failcnt++;

      wrapper.erase ();
      raw_fn.erase ();
    }
}

bool
jit_function_info::execute (const octave_value_list& ov_args,
                            octave_value_list& retval) const
{
  if (! function)
    return false;

  // TODO figure out a way to delete ov_args so we avoid duplicating refcount
  size_t nargs = ov_args.length ();
  std::vector<octave_base_value *> args (nargs);
  for (size_t i = 0; i < nargs; ++i)
    {
      octave_base_value *obv = ov_args(i).internal_rep ();
      obv->grab ();
      args[i] = obv;
    }

  octave_base_value *ret = function (&args[0]);
  if (ret)
    retval(0) = octave_value (ret);

  octave_quit ();

  return true;
}

bool
jit_function_info::match (const octave_value_list& ov_args) const
{
  if (! function)
    return true;

  size_t nargs = ov_args.length ();
  if (nargs != argument_types.size ())
    return false;

  for (size_t i = 0; i < nargs; ++i)
    if (jit_typeinfo::type_of (ov_args(i)) != argument_types[i])
      return false;

  return true;
}

// -------------------- jit_info --------------------
jit_info::jit_info (tree_jit& tjit, tree& tee)
  : engine (tjit.get_engine ()), function (0), llvm_function (0)
{
  compile (tjit, tee);
}

jit_info::jit_info (tree_jit& tjit, tree& tee, const octave_value& for_bounds)
  : engine (tjit.get_engine ()), function (0), llvm_function (0)
{
  compile (tjit, tee, jit_typeinfo::type_of (for_bounds));
}

jit_info::~jit_info (void)
{
  if (llvm_function)
    llvm_function->eraseFromParent ();
}

bool
jit_info::execute (const vmap& extra_vars) const
{
  if (! function)
    return false;

  std::vector<octave_base_value *> real_arguments (arguments.size ());
  for (size_t i = 0; i < arguments.size (); ++i)
    {
      if (arguments[i].second)
        {
          octave_value current = find (extra_vars, arguments[i].first);
          octave_base_value *obv = current.internal_rep ();
          obv->grab ();
          real_arguments[i] = obv;
        }
    }

  function (&real_arguments[0]);

  for (size_t i = 0; i < arguments.size (); ++i)
    {
      const std::string& name = arguments[i].first;

      // do not store for loop bounds temporary
      if (name.size () && name[0] != '#')
        symbol_table::assign (arguments[i].first, real_arguments[i]);
    }

  octave_quit ();

  return true;
}

bool
jit_info::match (const vmap& extra_vars) const
{
  if (! function)
    return true;

  for (size_t i = 0; i < bounds.size (); ++i)
    {
      const std::string& arg_name = bounds[i].second;
      octave_value value = find (extra_vars, arg_name);
      jit_type *type = jit_typeinfo::type_of (value);

      // FIXME: Check for a parent relationship
      if (type != bounds[i].first)
        return false;
    }

  return true;
}

void
jit_info::compile (tree_jit& tjit, tree& tee, jit_type *for_bounds)
{
  try
    {
      jit_convert conv (tee, for_bounds);
      jit_infer infer (conv.get_factory (), conv.get_blocks (),
                       conv.get_variable_map ());

      infer.infer ();

      if (Vdebug_jit)
        {
          jit_block_list& blocks = infer.get_blocks ();
          blocks.label ();
          std::cout << "-------------------- Compiling tree --------------------\n";
          std::cout << tee.str_print_code () << std::endl;
          blocks.print (std::cout, "octave jit ir");
        }

      jit_factory& factory = conv.get_factory ();
      jit_convert_llvm to_llvm;
      llvm_function = to_llvm.convert_loop (tjit.get_module (),
                                            infer.get_blocks (),
                                            factory.constants ());
      arguments = to_llvm.get_arguments ();
      bounds = conv.get_bounds ();
    }
  catch (const jit_fail_exception& e)
    {
      if (Vdebug_jit)
        {
          if (e.known ())
            std::cout << "jit fail: " << e.what () << std::endl;
        }

      Vjit_failcnt++;

    }

  if (llvm_function)
    {
      if (Vdebug_jit)
        {
          std::cout << "-------------------- llvm ir --------------------";
          std::cout << *llvm_function << std::endl;
          llvm::verifyFunction (*llvm_function);
        }

      tjit.optimize (llvm_function);

      if (Vdebug_jit)
        {
          std::cout << "-------------------- optimized llvm ir "
                    << "--------------------\n";
          std::cout << *llvm_function << std::endl;
        }

      void *void_fn = engine->getPointerToFunction (llvm_function);
      function = reinterpret_cast<jited_function> (void_fn);
    }
}

octave_value
jit_info::find (const vmap& extra_vars, const std::string& vname) const
{
  vmap::const_iterator iter = extra_vars.find (vname);
  return iter == extra_vars.end () ? symbol_table::varval (vname)
                                   : *iter->second;
}

#endif

#if defined (HAVE_LLVM)
#define UNUSED_WITHOUT_LLVM(x) x
#else
#define UNUSED_WITHOUT_LLVM(x) x GCC_ATTR_UNUSED
#endif

DEFUN (jit_failcnt, UNUSED_WITHOUT_LLVM (args),
       UNUSED_WITHOUT_LLVM (nargout),
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{val} =} jit_failcnt ()\n\
@deftypefnx {Built-in Function} {@var{old_val} =} jit_failcnt (@var{new_val})\n\
@deftypefnx {Built-in Function} {} jit_failcnt (@var{new_val}, \"local\")\n\
Query or set the internal variable that counts the number of JIT fail\n\
exceptions for Octave's JIT compiler.\n\
\n\
When called from inside a function with the @qcode{\"local\"} option, the\n\
variable is changed locally for the function and any subroutines it calls.\n\
The original variable value is restored when exiting the function.\n\
@seealso{jit_enable, jit_startcnt, debug_jit}\n\
@end deftypefn")
{
#if defined (HAVE_LLVM)
  return SET_INTERNAL_VARIABLE (jit_failcnt);
#else
  warning ("jit_failcnt: JIT compiling not available in this version of Octave");
  return octave_value ();
#endif
}

DEFUN (debug_jit, UNUSED_WITHOUT_LLVM (args),
       UNUSED_WITHOUT_LLVM (nargout),
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{val} =} debug_jit ()\n\
@deftypefnx {Built-in Function} {@var{old_val} =} debug_jit (@var{new_val})\n\
@deftypefnx {Built-in Function} {} debug_jit (@var{new_val}, \"local\")\n\
Query or set the internal variable that determines whether\n\
debugging/tracing is enabled for Octave's JIT compiler.\n\
\n\
When called from inside a function with the @qcode{\"local\"} option, the\n\
variable is changed locally for the function and any subroutines it calls.\n\
The original variable value is restored when exiting the function.\n\
@seealso{jit_enable, jit_startcnt}\n\
@end deftypefn")
{
#if defined (HAVE_LLVM)
  return SET_INTERNAL_VARIABLE (debug_jit);
#else
  warning ("debug_jit: JIT compiling not available in this version of Octave");
  return octave_value ();
#endif
}

DEFUN (jit_enable, UNUSED_WITHOUT_LLVM (args),
       UNUSED_WITHOUT_LLVM (nargout),
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{val} =} jit_enable ()\n\
@deftypefnx {Built-in Function} {@var{old_val} =} jit_enable (@var{new_val})\n\
@deftypefnx {Built-in Function} {} jit_enable (@var{new_val}, \"local\")\n\
Query or set the internal variable that enables Octave's JIT compiler.\n\
\n\
When called from inside a function with the @qcode{\"local\"} option, the\n\
variable is changed locally for the function and any subroutines it calls.\n\
The original variable value is restored when exiting the function.\n\
@seealso{jit_startcnt, debug_jit}\n\
@end deftypefn")
{
#if defined (HAVE_LLVM)
  return SET_INTERNAL_VARIABLE (jit_enable);
#else
  warning ("jit_enable: JIT compiling not available in this version of Octave");
  return octave_value ();
#endif
}

DEFUN (jit_startcnt, UNUSED_WITHOUT_LLVM (args),
       UNUSED_WITHOUT_LLVM (nargout),
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{val} =} jit_startcnt ()\n\
@deftypefnx {Built-in Function} {@var{old_val} =} jit_startcnt (@var{new_val})\n\
@deftypefnx {Built-in Function} {} jit_startcnt (@var{new_val}, \"local\")\n\
Query or set the internal variable that determines whether JIT compilation\n\
will take place for a specific loop.\n\
\n\
Because compilation is a costly operation it does not make sense to employ\n\
JIT when the loop count is low.  By default only loops with greater than\n\
1000 iterations will be accelerated.\n\
\n\
When called from inside a function with the @qcode{\"local\"} option, the\n\
variable is changed locally for the function and any subroutines it calls.\n\
The original variable value is restored when exiting the function.\n\
@seealso{jit_enable, jit_failcnt, debug_jit}\n\
@end deftypefn")
{
#if defined (HAVE_LLVM)
  return SET_INTERNAL_VARIABLE_WITH_LIMITS (jit_startcnt, 1,
                                            std::numeric_limits<int>::max ());
#else
  warning ("jit_enable: JIT compiling not available in this version of Octave");
  return octave_value ();
#endif
}
