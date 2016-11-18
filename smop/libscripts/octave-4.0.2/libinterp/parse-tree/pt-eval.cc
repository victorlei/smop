/*

Copyright (C) 2009-2015 John W. Eaton

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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cctype>

#include <iostream>

#include <fstream>
#include <typeinfo>

#include "debug.h"
#include "defun.h"
#include "error.h"
#include "gripes.h"
#include "input.h"
#include "ov-fcn-handle.h"
#include "ov-usr-fcn.h"
#include "variables.h"
#include "pt-all.h"
#include "pt-eval.h"
#include "symtab.h"
#include "unwind-prot.h"

//FIXME: This should be part of tree_evaluator
#include "pt-jit.h"

static tree_evaluator std_evaluator;

tree_evaluator *current_evaluator = &std_evaluator;

int tree_evaluator::dbstep_flag = 0;

size_t tree_evaluator::current_frame = 0;

bool tree_evaluator::debug_mode = false;

bool tree_evaluator::quiet_breakpoint_flag = false;

tree_evaluator::stmt_list_type tree_evaluator::statement_context
  = tree_evaluator::other;

bool tree_evaluator::in_loop_command = false;

// Maximum nesting level for functions, scripts, or sourced files called
// recursively.
int Vmax_recursion_depth = 256;

// If TRUE, turn off printing of results in functions (as if a
// semicolon has been appended to each statement).
static bool Vsilent_functions = false;

// Normal evaluator.

void
tree_evaluator::visit_anon_fcn_handle (tree_anon_fcn_handle&)
{
  panic_impossible ();
}

void
tree_evaluator::visit_argument_list (tree_argument_list&)
{
  panic_impossible ();
}

void
tree_evaluator::visit_binary_expression (tree_binary_expression&)
{
  panic_impossible ();
}

void
tree_evaluator::visit_break_command (tree_break_command& cmd)
{
  if (! error_state)
    {
      if (debug_mode)
        do_breakpoint (cmd.is_breakpoint ());

      if (statement_context == function || statement_context == script
          || in_loop_command)
        tree_break_command::breaking = 1;
    }
}

void
tree_evaluator::visit_colon_expression (tree_colon_expression&)
{
  panic_impossible ();
}

void
tree_evaluator::visit_continue_command (tree_continue_command& cmd)
{
  if (! error_state)
    {
      if (debug_mode)
        do_breakpoint (cmd.is_breakpoint ());

      if (statement_context == function || statement_context == script
          || in_loop_command)
        tree_continue_command::continuing = 1;
    }
}

void
tree_evaluator::reset_debug_state (void)
{
  debug_mode = bp_table::have_breakpoints () || Vdebugging;

  dbstep_flag = 0;
}

bool
tree_evaluator::statement_printing_enabled (void)
{
  return ! (Vsilent_functions && (statement_context == function
                                  || statement_context == script));
}

static inline void
do_global_init (tree_decl_elt& elt)
{
  tree_identifier *id = elt.ident ();

  if (id)
    {
      id->mark_global ();

      if (! error_state)
        {
          octave_lvalue ult = id->lvalue ();

          if (ult.is_undefined ())
            {
              tree_expression *expr = elt.expression ();

              octave_value init_val;

              if (expr)
                init_val = expr->rvalue1 ();
              else
                init_val = Matrix ();

              ult.assign (octave_value::op_asn_eq, init_val);
            }
        }
    }
}

static inline void
do_static_init (tree_decl_elt& elt)
{
  tree_identifier *id = elt.ident ();

  if (id)
    {
      id->mark_as_static ();

      octave_lvalue ult = id->lvalue ();

      if (ult.is_undefined ())
        {
          tree_expression *expr = elt.expression ();

          octave_value init_val;

          if (expr)
            init_val = expr->rvalue1 ();
          else
            init_val = Matrix ();

          ult.assign (octave_value::op_asn_eq, init_val);
        }
    }
}

void
tree_evaluator::do_decl_init_list (decl_elt_init_fcn fcn,
                                   tree_decl_init_list *init_list)
{
  if (init_list)
    {
      for (tree_decl_init_list::iterator p = init_list->begin ();
           p != init_list->end (); p++)
        {
          tree_decl_elt *elt = *p;

          fcn (*elt);

          if (error_state)
            break;
        }
    }
}

void
tree_evaluator::visit_global_command (tree_global_command& cmd)
{
  if (debug_mode)
    do_breakpoint (cmd.is_breakpoint ());

  do_decl_init_list (do_global_init, cmd.initializer_list ());
}

void
tree_evaluator::visit_persistent_command (tree_persistent_command& cmd)
{
  if (debug_mode)
    do_breakpoint (cmd.is_breakpoint ());

  do_decl_init_list (do_static_init, cmd.initializer_list ());
}

void
tree_evaluator::visit_decl_elt (tree_decl_elt&)
{
  panic_impossible ();
}

#if 0
bool
tree_decl_elt::eval (void)
{
  bool retval = false;

  if (id && expr)
    {
      octave_lvalue ult = id->lvalue ();

      octave_value init_val = expr->rvalue1 ();

      if (! error_state)
        {
          ult.assign (octave_value::op_asn_eq, init_val);

          retval = true;
        }
    }

  return retval;
}
#endif

void
tree_evaluator::visit_decl_init_list (tree_decl_init_list&)
{
  panic_impossible ();
}

// Decide if it's time to quit a for or while loop.
static inline bool
quit_loop_now (void)
{
  octave_quit ();

  // Maybe handle 'continue N' someday...

  if (tree_continue_command::continuing)
    tree_continue_command::continuing--;

  bool quit = (error_state
               || tree_return_command::returning
               || tree_break_command::breaking
               || tree_continue_command::continuing);

  if (tree_break_command::breaking)
    tree_break_command::breaking--;

  return quit;
}

void
tree_evaluator::visit_simple_for_command (tree_simple_for_command& cmd)
{
  if (error_state)
    return;

  if (debug_mode)
    do_breakpoint (cmd.is_breakpoint ());

  // FIXME: need to handle PARFOR loops here using cmd.in_parallel ()
  // and cmd.maxproc_expr ();

  unwind_protect frame;

  frame.protect_var (in_loop_command);

  in_loop_command = true;

  tree_expression *expr = cmd.control_expr ();

  octave_value rhs = expr->rvalue1 ();

#if HAVE_LLVM
  if (tree_jit::execute (cmd, rhs))
    return;
#endif

  if (error_state || rhs.is_undefined ())
    return;

  {
    tree_expression *lhs = cmd.left_hand_side ();

    octave_lvalue ult = lhs->lvalue ();

    if (error_state)
      return;

    tree_statement_list *loop_body = cmd.body ();

    if (rhs.is_range ())
      {
        Range rng = rhs.range_value ();

        octave_idx_type steps = rng.nelem ();

        for (octave_idx_type i = 0; i < steps; i++)
          {
            octave_value val (rng.elem (i));

            ult.assign (octave_value::op_asn_eq, val);

            if (! error_state && loop_body)
              loop_body->accept (*this);

            if (quit_loop_now ())
              break;
          }
      }
    else if (rhs.is_scalar_type ())
      {
        ult.assign (octave_value::op_asn_eq, rhs);

        if (! error_state && loop_body)
          loop_body->accept (*this);

        // Maybe decrement break and continue states.
        quit_loop_now ();
      }
    else if (rhs.is_matrix_type () || rhs.is_cell () || rhs.is_string ()
             || rhs.is_map ())
      {
        // A matrix or cell is reshaped to 2 dimensions and iterated by
        // columns.

        dim_vector dv = rhs.dims ().redim (2);

        octave_idx_type nrows = dv(0);
        octave_idx_type steps = dv(1);

        if (steps > 0)
          {
            octave_value arg = rhs;
            if (rhs.ndims () > 2)
              arg = arg.reshape (dv);

            // for row vectors, use single index to speed things up.
            octave_value_list idx;
            octave_idx_type iidx;
            if (nrows == 1)
              {
                idx.resize (1);
                iidx = 0;
              }
            else
              {
                idx.resize (2);
                idx(0) = octave_value::magic_colon_t;
                iidx = 1;
              }

            for (octave_idx_type i = 1; i <= steps; i++)
              {
                // do_index_op expects one-based indices.
                idx(iidx) = i;
                octave_value val = arg.do_index_op (idx);

                ult.assign (octave_value::op_asn_eq, val);

                if (! error_state && loop_body)
                  loop_body->accept (*this);

                if (quit_loop_now ())
                  break;
              }
          }
      }
    else
      {
        ::error ("invalid type in for loop expression near line %d, column %d",
                 cmd.line (), cmd.column ());
      }
  }
}

void
tree_evaluator::visit_complex_for_command (tree_complex_for_command& cmd)
{
  if (error_state)
    return;

  if (debug_mode)
    do_breakpoint (cmd.is_breakpoint ());

  unwind_protect frame;

  frame.protect_var (in_loop_command);

  in_loop_command = true;

  tree_expression *expr = cmd.control_expr ();

  octave_value rhs = expr->rvalue1 ();

  if (error_state || rhs.is_undefined ())
    return;

  if (rhs.is_map ())
    {
      // Cycle through structure elements.  First element of id_list
      // is set to value and the second is set to the name of the
      // structure element.

      tree_argument_list *lhs = cmd.left_hand_side ();

      tree_argument_list::iterator p = lhs->begin ();

      tree_expression *elt = *p++;

      octave_lvalue val_ref = elt->lvalue ();

      elt = *p;

      octave_lvalue key_ref = elt->lvalue ();

      const octave_map tmp_val = rhs.map_value ();

      tree_statement_list *loop_body = cmd.body ();

      string_vector keys = tmp_val.keys ();

      octave_idx_type nel = keys.numel ();

      for (octave_idx_type i = 0; i < nel; i++)
        {
          std::string key = keys[i];

          const Cell val_lst = tmp_val.contents (key);

          octave_idx_type n = val_lst.numel ();

          octave_value val = (n == 1) ? val_lst(0) : octave_value (val_lst);

          val_ref.assign (octave_value::op_asn_eq, val);
          key_ref.assign (octave_value::op_asn_eq, key);

          if (! error_state && loop_body)
            loop_body->accept (*this);

          if (quit_loop_now ())
            break;
        }
    }
  else
    error ("in statement 'for [X, Y] = VAL', VAL must be a structure");
}

void
tree_evaluator::visit_octave_user_script (octave_user_script&)
{
  panic_impossible ();
}

void
tree_evaluator::visit_octave_user_function (octave_user_function&)
{
  panic_impossible ();
}

void
tree_evaluator::visit_octave_user_function_header (octave_user_function&)
{
  panic_impossible ();
}

void
tree_evaluator::visit_octave_user_function_trailer (octave_user_function&)
{
  panic_impossible ();
}

void
tree_evaluator::visit_function_def (tree_function_def& cmd)
{
  octave_value fcn = cmd.function ();

  octave_function *f = fcn.function_value ();

  if (f)
    {
      std::string nm = f->name ();

      symbol_table::install_cmdline_function (nm, fcn);

      // Make sure that any variable with the same name as the new
      // function is cleared.

      symbol_table::assign (nm);
    }
}

void
tree_evaluator::visit_identifier (tree_identifier&)
{
  panic_impossible ();
}

void
tree_evaluator::visit_if_clause (tree_if_clause&)
{
  panic_impossible ();
}

void
tree_evaluator::visit_if_command (tree_if_command& cmd)
{
  tree_if_command_list *lst = cmd.cmd_list ();

  if (lst)
    lst->accept (*this);
}

void
tree_evaluator::visit_if_command_list (tree_if_command_list& lst)
{
  for (tree_if_command_list::iterator p = lst.begin (); p != lst.end (); p++)
    {
      tree_if_clause *tic = *p;

      tree_expression *expr = tic->condition ();

      if (statement_context == function || statement_context == script)
        octave_call_stack::set_location (tic->line (), tic->column ());

      if (debug_mode && ! tic->is_else_clause ())
        do_breakpoint (tic->is_breakpoint ());

      if (tic->is_else_clause () || expr->is_logically_true ("if"))
        {
          if (! error_state)
            {
              tree_statement_list *stmt_lst = tic->commands ();

              if (stmt_lst)
                stmt_lst->accept (*this);
            }

          break;
        }
    }
}

void
tree_evaluator::visit_index_expression (tree_index_expression&)
{
  panic_impossible ();
}

void
tree_evaluator::visit_matrix (tree_matrix&)
{
  panic_impossible ();
}

void
tree_evaluator::visit_cell (tree_cell&)
{
  panic_impossible ();
}

void
tree_evaluator::visit_multi_assignment (tree_multi_assignment&)
{
  panic_impossible ();
}

void
tree_evaluator::visit_no_op_command (tree_no_op_command& cmd)
{
  if (debug_mode && cmd.is_end_of_fcn_or_script ())
    do_breakpoint (cmd.is_breakpoint (), true);
}

void
tree_evaluator::visit_constant (tree_constant&)
{
  panic_impossible ();
}

void
tree_evaluator::visit_fcn_handle (tree_fcn_handle&)
{
  panic_impossible ();
}

void
tree_evaluator::visit_funcall (tree_funcall&)
{
  panic_impossible ();
}

void
tree_evaluator::visit_parameter_list (tree_parameter_list&)
{
  panic_impossible ();
}

void
tree_evaluator::visit_postfix_expression (tree_postfix_expression&)
{
  panic_impossible ();
}

void
tree_evaluator::visit_prefix_expression (tree_prefix_expression&)
{
  panic_impossible ();
}

void
tree_evaluator::visit_return_command (tree_return_command& cmd)
{
  if (! error_state)
    {
      if (debug_mode)
        do_breakpoint (cmd.is_breakpoint ());

      // Act like dbcont.

      if (Vdebugging
          && octave_call_stack::current_frame () == current_frame)
        {
          Vdebugging = false;

          reset_debug_state ();
        }
      else if (statement_context == function || statement_context == script
               || in_loop_command)
        tree_return_command::returning = 1;
    }
}

void
tree_evaluator::visit_return_list (tree_return_list&)
{
  panic_impossible ();
}

void
tree_evaluator::visit_simple_assignment (tree_simple_assignment&)
{
  panic_impossible ();
}

void
tree_evaluator::visit_statement (tree_statement& stmt)
{
  tree_command *cmd = stmt.command ();
  tree_expression *expr = stmt.expression ();

  if (cmd || expr)
    {
      if (statement_context == function || statement_context == script)
        {
          // Skip commands issued at a debug> prompt to avoid disturbing
          // the state of the program we are debugging.

          if (! Vdebugging)
            octave_call_stack::set_location (stmt.line (), stmt.column ());

          if ((statement_context == script
               && ((Vecho_executing_commands & ECHO_SCRIPTS
                    && octave_call_stack::all_scripts ())
                   || Vecho_executing_commands & ECHO_FUNCTIONS))
              || (statement_context == function
                  && Vecho_executing_commands & ECHO_FUNCTIONS))
            stmt.echo_code ();
        }

      try
        {
          if (cmd)
            cmd->accept (*this);
          else
            {
              if (debug_mode)
                do_breakpoint (expr->is_breakpoint ());

              // FIXME: maybe all of this should be packaged in
              // one virtual function that returns a flag saying whether
              // or not the expression will take care of binding ans and
              // printing the result.

              // FIXME: it seems that we should just have to
              // call expr->rvalue1 () and that should take care of
              // everything, binding ans as necessary?

              bool do_bind_ans = false;

              if (expr->is_identifier ())
                {
                  tree_identifier *id = dynamic_cast<tree_identifier *> (expr);

                  do_bind_ans = (! id->is_variable ());
                }
              else
                do_bind_ans = (! expr->is_assignment_expression ());

              octave_value tmp_result = expr->rvalue1 (0);

              if (do_bind_ans && ! (error_state || tmp_result.is_undefined ()))
                bind_ans (tmp_result, expr->print_result ()
                          && statement_printing_enabled ());

              //              if (tmp_result.is_defined ())
              //                result_values(0) = tmp_result;
            }
        }
      catch (octave_execution_exception)
        {
          gripe_library_execution_error ();
        }
      catch (std::bad_alloc)
        {
          // FIXME: We want to use error_with_id here so that we set
          // the error state, give users control over this error
          // message, and so that we set the error_state appropriately
          // so we'll get stack trace info when appropriate.  But
          // error_with_id will require some memory allocations.  Is
          // there anything we can do to make those more likely to
          // succeed?

          error_with_id ("Octave:bad-alloc",
                         "out of memory or dimension too large for Octave's index type");
        }
    }
}

void
tree_evaluator::visit_statement_list (tree_statement_list& lst)
{
  static octave_value_list empty_list;

  if (error_state)
    return;

  tree_statement_list::iterator p = lst.begin ();

  if (p != lst.end ())
    {
      while (true)
        {
          tree_statement *elt = *p++;

          if (elt)
            {
              octave_quit ();

              elt->accept (*this);

              if (error_state)
                break;

              if (tree_break_command::breaking
                  || tree_continue_command::continuing)
                break;

              if (tree_return_command::returning)
                break;

              if (p == lst.end ())
                break;
              else
                {
                  // Clear previous values before next statement is
                  // evaluated so that we aren't holding an extra
                  // reference to a value that may be used next.  For
                  // example, in code like this:
                  //
                  //   X = rand (N);  # refcount for X should be 1
                  //                  # after this statement
                  //
                  //   X(idx) = val;  # no extra copy of X should be
                  //                  # needed, but we will be faked
                  //                  # out if retval is not cleared
                  //                  # between statements here

                  //              result_values = empty_list;
                }
            }
          else
            error ("invalid statement found in statement list!");
        }
    }
}

void
tree_evaluator::visit_switch_case (tree_switch_case&)
{
  panic_impossible ();
}

void
tree_evaluator::visit_switch_case_list (tree_switch_case_list&)
{
  panic_impossible ();
}

void
tree_evaluator::visit_switch_command (tree_switch_command& cmd)
{
  if (debug_mode)
    do_breakpoint (cmd.is_breakpoint ());

  tree_expression *expr = cmd.switch_value ();

  if (expr)
    {
      octave_value val = expr->rvalue1 ();

      tree_switch_case_list *lst = cmd.case_list ();

      if (! error_state && lst)
        {
          for (tree_switch_case_list::iterator p = lst->begin ();
               p != lst->end (); p++)
            {
              tree_switch_case *t = *p;

              if (t->is_default_case () || t->label_matches (val))
                {
                  if (error_state)
                    break;

                  tree_statement_list *stmt_lst = t->commands ();

                  if (stmt_lst)
                    stmt_lst->accept (*this);

                  break;
                }
            }
        }
    }
  else
    ::error ("missing value in switch command near line %d, column %d",
             cmd.line (), cmd.column ());
}

void
tree_evaluator::visit_try_catch_command (tree_try_catch_command& cmd)
{
  unwind_protect frame;

  frame.protect_var (buffer_error_messages);
  frame.protect_var (Vdebug_on_error);
  frame.protect_var (Vdebug_on_warning);

  buffer_error_messages++;
  Vdebug_on_error = false;
  Vdebug_on_warning = false;

  tree_statement_list *catch_code = cmd.cleanup ();

  // The catch code is *not* added to unwind_protect stack; it doesn't need
  // to be run on interrupts.

  tree_statement_list *try_code = cmd.body ();

  if (try_code)
    {
      try_code->accept (*this);
    }

  if (error_state)
    {
      error_state = 0;

      if (catch_code)
        {
          // Set up for letting the user print any messages from errors that
          // occurred in the body of the try_catch statement.

          buffer_error_messages--;

          tree_identifier *expr_id = cmd.identifier ();
          octave_lvalue ult;

          if (expr_id)
            {

              octave_scalar_map err;

              ult = expr_id->lvalue ();

              if (error_state)
                return;

              err.assign ("message", last_error_message ());
              err.assign ("identifier", last_error_id ());
              err.assign ("stack", last_error_stack ());

              if (! error_state)
                ult.assign (octave_value::op_asn_eq, err);

            }

          if (catch_code)
            catch_code->accept (*this);
        }
    }
}

void
tree_evaluator::do_unwind_protect_cleanup_code (tree_statement_list *list)
{
  unwind_protect frame;

  frame.protect_var (octave_interrupt_state);
  octave_interrupt_state = 0;

  // We want to run the cleanup code without error_state being set,
  // but we need to restore its value, so that any errors encountered
  // in the first part of the unwind_protect are not completely
  // ignored.

  frame.protect_var (error_state);
  error_state = 0;

  // We want to preserve the last location info for possible
  // backtracking.
  frame.add_fcn (octave_call_stack::set_line,
                 octave_call_stack::current_line ());
  frame.add_fcn (octave_call_stack::set_column,
                 octave_call_stack::current_column ());

  // Similarly, if we have seen a return or break statement, allow all
  // the cleanup code to run before returning or handling the break.
  // We don't have to worry about continue statements because they can
  // only occur in loops.

  frame.protect_var (tree_return_command::returning);
  tree_return_command::returning = 0;

  frame.protect_var (tree_break_command::breaking);
  tree_break_command::breaking = 0;

  if (list)
    list->accept (*this);

  // The unwind_protects are popped off the stack in the reverse of
  // the order they are pushed on.

  // FIXME: these statements say that if we see a break or
  // return statement in the cleanup block, that we want to use the
  // new value of the breaking or returning flag instead of restoring
  // the previous value.  Is that the right thing to do?  I think so.
  // Consider the case of
  //
  //   function foo ()
  //     unwind_protect
  //       stderr << "1: this should always be executed\n";
  //       break;
  //       stderr << "1: this should never be executed\n";
  //     unwind_protect_cleanup
  //       stderr << "2: this should always be executed\n";
  //       return;
  //       stderr << "2: this should never be executed\n";
  //     end_unwind_protect
  //   endfunction
  //
  // If we reset the value of the breaking flag, both the returning
  // flag and the breaking flag will be set, and we shouldn't have
  // both.  So, use the most recent one.  If there is no return or
  // break in the cleanup block, the values should be reset to
  // whatever they were when the cleanup block was entered.

  if (tree_break_command::breaking || tree_return_command::returning)
    {
      frame.discard (2);
    }
  else
    {
      frame.run (2);
    }

  // We don't want to ignore errors that occur in the cleanup code, so
  // if an error is encountered there, leave error_state alone.
  // Otherwise, set it back to what it was before.

  if (error_state)
    frame.discard (2);
  else
    frame.run (2);

  frame.run ();
}

void
tree_evaluator::visit_unwind_protect_command (tree_unwind_protect_command& cmd)
{
  tree_statement_list *cleanup_code = cmd.cleanup ();

  tree_statement_list *unwind_protect_code = cmd.body ();

  if (unwind_protect_code)
    {
      try
        {
          unwind_protect_code->accept (*this);
        }
      catch (...)
        {
          // Run the cleanup code on exceptions, so that it is run even in case
          // of interrupt or out-of-memory.
          do_unwind_protect_cleanup_code (cleanup_code);
          // FIXME: should error_state be checked here?
          // We want to rethrow the exception, even if error_state is set, so
          // that interrupts continue.
          throw;
        }

      do_unwind_protect_cleanup_code (cleanup_code);
    }
}

void
tree_evaluator::visit_while_command (tree_while_command& cmd)
{
  if (error_state)
    return;

#if HAVE_LLVM
  if (tree_jit::execute (cmd))
    return;
#endif

  unwind_protect frame;

  frame.protect_var (in_loop_command);

  in_loop_command = true;

  tree_expression *expr = cmd.condition ();

  if (! expr)
    panic_impossible ();

  for (;;)
    {
      if (debug_mode)
        do_breakpoint (cmd.is_breakpoint ());

      if (expr->is_logically_true ("while"))
        {
          tree_statement_list *loop_body = cmd.body ();

          if (loop_body)
            {
              loop_body->accept (*this);

              if (error_state)
                return;
            }

          if (quit_loop_now ())
            break;
        }
      else
        break;
    }
}

void
tree_evaluator::visit_do_until_command (tree_do_until_command& cmd)
{
  if (error_state)
    return;

#if HAVE_LLVM
  if (tree_jit::execute (cmd))
    return;
#endif

  unwind_protect frame;

  frame.protect_var (in_loop_command);

  in_loop_command = true;

  tree_expression *expr = cmd.condition ();

  if (! expr)
    panic_impossible ();

  for (;;)
    {
      tree_statement_list *loop_body = cmd.body ();

      if (loop_body)
        {
          loop_body->accept (*this);

          if (error_state)
            return;
        }

      if (quit_loop_now ())
        break;

      if (debug_mode)
        do_breakpoint (cmd.is_breakpoint ());

      if (expr->is_logically_true ("do-until"))
        break;
    }
}

void
tree_evaluator::do_breakpoint (tree_statement& stmt) const
{
  do_breakpoint (stmt.is_breakpoint (), stmt.is_end_of_fcn_or_script ());
}

void
tree_evaluator::do_breakpoint (bool is_breakpoint,
                               bool is_end_of_fcn_or_script) const
{
  bool break_on_this_statement = false;

  if (octave_debug_on_interrupt_state)
    {
      break_on_this_statement = true;

      octave_debug_on_interrupt_state = false;

      current_frame = octave_call_stack::current_frame ();
    }
  else if (is_breakpoint)
    {
      break_on_this_statement = true;

      dbstep_flag = 0;

      current_frame = octave_call_stack::current_frame ();
    }
  else if (dbstep_flag > 0)
    {
      if (octave_call_stack::current_frame () == current_frame)
        {
          if (dbstep_flag == 1 || is_end_of_fcn_or_script)
            {
              // We get here if we are doing a "dbstep" or a "dbstep N" and the
              // count has reached 1 so that we must stop and return to debug
              // prompt.  Alternatively, "dbstep N" has been used but the end
              // of the frame has been reached so we stop at the last line and
              // return to prompt.

              break_on_this_statement = true;

              dbstep_flag = 0;
            }
          else
            {
              // Executing "dbstep N".  Decrease N by one and continue.

              dbstep_flag--;
            }

        }
      else if (dbstep_flag == 1
               && octave_call_stack::current_frame () < current_frame)
        {
          // We stepped out from the end of a function.

          current_frame = octave_call_stack::current_frame ();

          break_on_this_statement = true;

          dbstep_flag = 0;
        }
    }
  else if (dbstep_flag == -1)
    {
      // We get here if we are doing a "dbstep in".

      break_on_this_statement = true;

      dbstep_flag = 0;

      current_frame = octave_call_stack::current_frame ();
    }
  else if (dbstep_flag == -2)
    {
      // We get here if we are doing a "dbstep out".  Check for end of
      // function and whether the current frame is the same as the
      // cached value because we want to step out from the frame where
      // "dbstep out" was evaluated, not from any functions called from
      // that frame.

      if (is_end_of_fcn_or_script
          && octave_call_stack::current_frame () == current_frame)
        dbstep_flag = -1;
    }

  if (break_on_this_statement)
    do_keyboard ();

}

// ARGS is currently unused, but since the do_keyboard function in
// input.cc accepts an argument list, we preserve it here so that the
// interface won't have to change if we decide to use it in the future.

octave_value
tree_evaluator::do_keyboard (const octave_value_list& args) const
{
  return ::do_keyboard (args);
}

DEFUN (max_recursion_depth, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{val} =} max_recursion_depth ()\n\
@deftypefnx {Built-in Function} {@var{old_val} =} max_recursion_depth (@var{new_val})\n\
@deftypefnx {Built-in Function} {} max_recursion_depth (@var{new_val}, \"local\")\n\
Query or set the internal limit on the number of times a function may\n\
be called recursively.\n\
\n\
If the limit is exceeded, an error message is printed and control returns to\n\
the top level.\n\
\n\
When called from inside a function with the @qcode{\"local\"} option, the\n\
variable is changed locally for the function and any subroutines it calls.\n\
The original variable value is restored when exiting the function.\n\
@end deftypefn")
{
  return SET_INTERNAL_VARIABLE (max_recursion_depth);
}

/*
%!test
%! orig_val = max_recursion_depth ();
%! old_val = max_recursion_depth (2*orig_val);
%! assert (orig_val, old_val);
%! assert (max_recursion_depth (), 2*orig_val);
%! max_recursion_depth (orig_val);
%! assert (max_recursion_depth (), orig_val);

%!error (max_recursion_depth (1, 2))
*/

DEFUN (silent_functions, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{val} =} silent_functions ()\n\
@deftypefnx {Built-in Function} {@var{old_val} =} silent_functions (@var{new_val})\n\
@deftypefnx {Built-in Function} {} silent_functions (@var{new_val}, \"local\")\n\
Query or set the internal variable that controls whether internal\n\
output from a function is suppressed.\n\
\n\
If this option is disabled, Octave will display the results produced by\n\
evaluating expressions within a function body that are not terminated with\n\
a semicolon.\n\
\n\
When called from inside a function with the @qcode{\"local\"} option, the\n\
variable is changed locally for the function and any subroutines it calls.\n\
The original variable value is restored when exiting the function.\n\
@end deftypefn")
{
  return SET_INTERNAL_VARIABLE (silent_functions);
}

/*
%!test
%! orig_val = silent_functions ();
%! old_val = silent_functions (! orig_val);
%! assert (orig_val, old_val);
%! assert (silent_functions (), ! orig_val);
%! silent_functions (orig_val);
%! assert (silent_functions (), orig_val);

%!error (silent_functions (1, 2))
*/
