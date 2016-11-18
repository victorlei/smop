/*

Copyright (C) 1996-2015 John W. Eaton

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

#include "error.h"
#include "input.h"
#include "ov-usr-fcn.h"
#include "pt-all.h"

void
tree_checker::visit_argument_list (tree_argument_list& lst)
{
  tree_argument_list::iterator p = lst.begin ();

  while (p != lst.end ())
    {
      tree_expression *elt = *p++;

      if (elt)
        {
          if (do_lvalue_check && ! elt->lvalue_ok ())
            gripe ("invalid lvalue in multiple assignment", elt->line ());
        }
    }
}

void
tree_checker::visit_binary_expression (tree_binary_expression& expr)
{
  tree_expression *op1 = expr.lhs ();

  if (op1)
    op1->accept (*this);

  tree_expression *op2 = expr.rhs ();

  if (op2)
    op2->accept (*this);
}

void
tree_checker::visit_break_command (tree_break_command&)
{
}

void
tree_checker::visit_colon_expression (tree_colon_expression& expr)
{
  tree_expression *op1 = expr.base ();

  if (op1)
    op1->accept (*this);

  tree_expression *op3 = expr.increment ();

  if (op3)
    op3->accept (*this);

  tree_expression *op2 = expr.limit ();

  if (op2)
    op2->accept (*this);
}

void
tree_checker::visit_continue_command (tree_continue_command&)
{
}

void
tree_checker::do_decl_command (tree_decl_command& cmd)
{
  tree_decl_init_list *init_list = cmd.initializer_list ();

  if (init_list)
    init_list->accept (*this);
}

void
tree_checker::visit_global_command (tree_global_command& cmd)
{
  do_decl_command (cmd);
}

void
tree_checker::visit_persistent_command (tree_persistent_command& cmd)
{
  do_decl_command (cmd);
}

void
tree_checker::visit_decl_elt (tree_decl_elt& cmd)
{
  tree_identifier *id = cmd.ident ();

  if (id)
    id->accept (*this);

  tree_expression *expr = cmd.expression ();

  if (expr)
    expr->accept (*this);
}

void
tree_checker::visit_decl_init_list (tree_decl_init_list& lst)
{
  tree_decl_init_list::iterator p = lst.begin ();

  while (p != lst.end ())
    {
      tree_decl_elt *elt = *p++;

      if (elt)
        elt->accept (*this);
    }
}

void
tree_checker::visit_simple_for_command (tree_simple_for_command& cmd)
{
  tree_expression *lhs = cmd.left_hand_side ();

  if (lhs)
    {
      if (! lhs->lvalue_ok ())
        gripe ("invalid lvalue in for command", cmd.line ());
    }

  tree_expression *expr = cmd.control_expr ();

  if (expr)
    expr->accept (*this);

  tree_expression *maxproc = cmd.maxproc_expr ();

  if (maxproc)
    maxproc->accept (*this);

  tree_statement_list *list = cmd.body ();

  if (list)
    list->accept (*this);
}

void
tree_checker::visit_complex_for_command (tree_complex_for_command& cmd)
{
  tree_argument_list *lhs = cmd.left_hand_side ();

  if (lhs)
    {
      int len = lhs->length ();

      if (len == 0 || len > 2)
        gripe ("invalid number of output arguments in for command",
               cmd.line ());

      do_lvalue_check = true;

      lhs->accept (*this);

      do_lvalue_check = false;
    }

  tree_expression *expr = cmd.control_expr ();

  if (expr)
    expr->accept (*this);

  tree_statement_list *list = cmd.body ();

  if (list)
    list->accept (*this);
}

void
tree_checker::visit_octave_user_script (octave_user_script& fcn)
{
  tree_statement_list *cmd_list = fcn.body ();

  if (cmd_list)
    cmd_list->accept (*this);
}

void
tree_checker::visit_octave_user_function (octave_user_function& fcn)
{
  tree_statement_list *cmd_list = fcn.body ();

  if (cmd_list)
    cmd_list->accept (*this);
}

void
tree_checker::visit_function_def (tree_function_def& fdef)
{
  octave_value fcn = fdef.function ();

  octave_function *f = fcn.function_value ();

  if (f)
    f->accept (*this);
}

void
tree_checker::visit_identifier (tree_identifier& /* id */)
{
}

void
tree_checker::visit_if_clause (tree_if_clause& cmd)
{
  tree_expression *expr = cmd.condition ();

  if (expr)
    expr->accept (*this);

  tree_statement_list *list = cmd.commands ();

  if (list)
    list->accept (*this);
}

void
tree_checker::visit_if_command (tree_if_command& cmd)
{
  tree_if_command_list *list = cmd.cmd_list ();

  if (list)
    list->accept (*this);
}

void
tree_checker::visit_if_command_list (tree_if_command_list& lst)
{
  tree_if_command_list::iterator p = lst.begin ();

  while (p != lst.end ())
    {
      tree_if_clause *elt = *p++;

      if (elt)
        elt->accept (*this);
    }
}

void
tree_checker::visit_index_expression (tree_index_expression& expr)
{
  tree_expression *e = expr.expression ();

  if (e)
    e->accept (*this);

  std::list<tree_argument_list *> lst = expr.arg_lists ();

  std::list<tree_argument_list *>::iterator p = lst.begin ();

  while (p != lst.end ())
    {
      tree_argument_list *elt = *p++;

      if (elt)
        elt->accept (*this);
    }
}

void
tree_checker::visit_matrix (tree_matrix& lst)
{
  tree_matrix::iterator p = lst.begin ();

  while (p != lst.end ())
    {
      tree_argument_list *elt = *p++;

      if (elt)
        elt->accept (*this);
    }
}

void
tree_checker::visit_cell (tree_cell& lst)
{
  tree_matrix::iterator p = lst.begin ();

  while (p != lst.end ())
    {
      tree_argument_list *elt = *p++;

      if (elt)
        elt->accept (*this);
    }
}

void
tree_checker::visit_multi_assignment (tree_multi_assignment& expr)
{
  tree_argument_list *lhs = expr.left_hand_side ();

  if (lhs)
    {
      do_lvalue_check = true;

      lhs->accept (*this);

      do_lvalue_check = false;
    }

  tree_expression *rhs = expr.right_hand_side ();

  if (rhs)
    rhs->accept (*this);
}

void
tree_checker::visit_no_op_command (tree_no_op_command& /* cmd */)
{
}

void
tree_checker::visit_anon_fcn_handle (tree_anon_fcn_handle& /* afh */)
{
}

void
tree_checker::visit_constant (tree_constant& /* val */)
{
}

void
tree_checker::visit_fcn_handle (tree_fcn_handle& /* fh */)
{
}

void
tree_checker::visit_funcall (tree_funcall& /* fc */)
{
}

void
tree_checker::visit_parameter_list (tree_parameter_list& lst)
{
  tree_parameter_list::iterator p = lst.begin ();

  while (p != lst.end ())
    {
      tree_decl_elt *elt = *p++;

      if (elt)
        elt->accept (*this);
    }
}

void
tree_checker::visit_postfix_expression (tree_postfix_expression& expr)
{
  tree_expression *e = expr.operand ();

  if (e)
    e->accept (*this);
}

void
tree_checker::visit_prefix_expression (tree_prefix_expression& expr)
{
  tree_expression *e = expr.operand ();

  if (e)
    e->accept (*this);
}

void
tree_checker::visit_return_command (tree_return_command&)
{
}

void
tree_checker::visit_return_list (tree_return_list& lst)
{
  tree_return_list::iterator p = lst.begin ();

  while (p != lst.end ())
    {
      tree_index_expression *elt = *p++;

      if (elt)
        elt->accept (*this);
    }
}

void
tree_checker::visit_simple_assignment (tree_simple_assignment& expr)
{
  tree_expression *lhs = expr.left_hand_side ();

  if (lhs)
    {
      if (! lhs->lvalue_ok ())
        gripe ("invalid lvalue in assignment", expr.line ());
    }

  tree_expression *rhs = expr.right_hand_side ();

  if (rhs)
    rhs->accept (*this);
}

void
tree_checker::visit_statement (tree_statement& stmt)
{
  tree_command *cmd = stmt.command ();

  if (cmd)
    cmd->accept (*this);
  else
    {
      tree_expression *expr = stmt.expression ();

      if (expr)
        expr->accept (*this);
    }
}

void
tree_checker::visit_statement_list (tree_statement_list& lst)
{
  for (tree_statement_list::iterator p = lst.begin (); p != lst.end (); p++)
    {
      tree_statement *elt = *p;

      if (elt)
        elt->accept (*this);
    }
}

void
tree_checker::visit_switch_case (tree_switch_case& cs)
{
  tree_expression *label = cs.case_label ();

  if (label)
    label->accept (*this);

  tree_statement_list *list = cs.commands ();

  if (list)
    list->accept (*this);
}

void
tree_checker::visit_switch_case_list (tree_switch_case_list& lst)
{
  tree_switch_case_list::iterator p = lst.begin ();

  while (p != lst.end ())
    {
      tree_switch_case *elt = *p++;

      if (elt)
        elt->accept (*this);
    }
}

void
tree_checker::visit_switch_command (tree_switch_command& cmd)
{
  tree_expression *expr = cmd.switch_value ();

  if (expr)
    expr->accept (*this);

  tree_switch_case_list *list = cmd.case_list ();

  if (list)
    list->accept (*this);
}

void
tree_checker::visit_try_catch_command (tree_try_catch_command& cmd)
{
  tree_statement_list *try_code = cmd.body ();

  tree_identifier *expr_id = cmd.identifier ();

  if (expr_id)
    {
      if (! expr_id->lvalue_ok ())
        gripe ("invalid lvalue used for identifier in try-catch command",
               cmd.line ());
    }

  if (try_code)
    try_code->accept (*this);

  tree_statement_list *catch_code = cmd.cleanup ();

  if (catch_code)
    catch_code->accept (*this);
}

void
tree_checker::visit_unwind_protect_command (tree_unwind_protect_command& cmd)
{
  tree_statement_list *unwind_protect_code = cmd.body ();

  if (unwind_protect_code)
    unwind_protect_code->accept (*this);

  tree_statement_list *cleanup_code = cmd.cleanup ();

  if (cleanup_code)
    cleanup_code->accept (*this);
}

void
tree_checker::visit_while_command (tree_while_command& cmd)
{
  tree_expression *expr = cmd.condition ();

  if (expr)
    expr->accept (*this);

  tree_statement_list *list = cmd.body ();

  if (list)
    list->accept (*this);
}

void
tree_checker::visit_do_until_command (tree_do_until_command& cmd)
{
  tree_statement_list *list = cmd.body ();

  if (list)
    list->accept (*this);

  tree_expression *expr = cmd.condition ();

  if (expr)
    expr->accept (*this);
}

void
tree_checker::gripe (const std::string& msg, int line)
{
  if (file_name.empty ())
    error ("%s", msg.c_str ());
  else
    error ("%s: %d: %s", file_name.c_str (), line, msg.c_str ());
}
