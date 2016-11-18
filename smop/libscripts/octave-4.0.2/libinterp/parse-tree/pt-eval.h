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

#if !defined (octave_pt_eval_h)
#define octave_pt_eval_h 1

#include <stack>
#include <string>

#include "comment-list.h"
#include "oct-obj.h"
#include "pt-walk.h"

class tree_expression;

// How to evaluate the code that the parse trees represent.

class
OCTINTERP_API
tree_evaluator : public tree_walker
{
public:

  typedef void (*decl_elt_init_fcn) (tree_decl_elt&);

  tree_evaluator (void) { }

  ~tree_evaluator (void) { }

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

  void do_unwind_protect_cleanup_code (tree_statement_list *list);

  void visit_unwind_protect_command (tree_unwind_protect_command&);

  void visit_while_command (tree_while_command&);

  void visit_do_until_command (tree_do_until_command&);

  static void reset_debug_state (void);

  static bool statement_printing_enabled (void);

  // If > 0, stop executing at the (N-1)th stopping point, counting
  //         from the the current execution point in the current frame.
  //
  // If < 0, stop executing at the next possible stopping point.
  static int dbstep_flag;

  // The number of the stack frame we are currently debugging.
  static size_t current_frame;

  static bool debug_mode;

  static bool quiet_breakpoint_flag;

  // Possible types of evaluation contexts.
  enum stmt_list_type
  {
    function,  // function body
    script,    // script file
    other      // command-line input or eval string
  };

  // The context for the current evaluation.
  static stmt_list_type statement_context;

  // TRUE means we are evaluating some kind of looping construct.
  static bool in_loop_command;

private:

  void do_decl_init_list (decl_elt_init_fcn fcn,
                          tree_decl_init_list *init_list);

  void do_breakpoint (tree_statement& stmt) const;

  void do_breakpoint (bool is_breakpoint,
                      bool is_end_of_fcn_or_script = false) const;

  virtual octave_value
  do_keyboard (const octave_value_list& args = octave_value_list ()) const;

  // No copying!

  tree_evaluator (const tree_evaluator&);

  tree_evaluator& operator = (const tree_evaluator&);
};

extern tree_evaluator *current_evaluator;

// Maximum nesting level for functions, scripts, or sourced files called
// recursively.
extern int Vmax_recursion_depth;

#endif
