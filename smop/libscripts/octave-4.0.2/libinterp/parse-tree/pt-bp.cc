/*

Copyright (C) 2001-2015 Ben Sapp

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

#include "ov-usr-fcn.h"
#include "pager.h"
#include "pt-all.h"

// TRUE means SIGINT should put us in the debugger at the next
// available breakpoint.
bool octave_debug_on_interrupt_state = false;

void
tree_breakpoint::visit_while_command (tree_while_command& cmd)
{
  if (cmd.line () >= line)
    take_action (cmd);

  if (! found)
    {
      tree_statement_list *lst = cmd.body ();

      if (lst)
        lst->accept (*this);
    }
}

void
tree_breakpoint::visit_do_until_command (tree_do_until_command& cmd)
{
  if (! found)
    {
      tree_statement_list *lst = cmd.body ();

      if (lst)
        lst->accept (*this);

      if (! found)
        {
          if (cmd.line () >= line)
            take_action (cmd);
        }
    }
}

void
tree_breakpoint::visit_argument_list (tree_argument_list&)
{
  panic_impossible ();
}

void
tree_breakpoint::visit_binary_expression (tree_binary_expression&)
{
  panic_impossible ();
}

void
tree_breakpoint::visit_break_command (tree_break_command& cmd)
{
  if (cmd.line () >= line)
    take_action (cmd);
}

void
tree_breakpoint::visit_colon_expression (tree_colon_expression&)
{
  panic_impossible ();
}

void
tree_breakpoint::visit_continue_command (tree_continue_command& cmd)
{
  if (cmd.line () >= line)
    take_action (cmd);
}

void
tree_breakpoint::do_decl_command (tree_decl_command& cmd)
{
  if (cmd.line () >= line)
    take_action (cmd);
}

void
tree_breakpoint::visit_global_command (tree_global_command& cmd)
{
  do_decl_command (cmd);
}

void
tree_breakpoint::visit_persistent_command (tree_persistent_command& cmd)
{
  do_decl_command (cmd);
}

void
tree_breakpoint::visit_decl_elt (tree_decl_elt&)
{
  panic_impossible ();
}

void
tree_breakpoint::visit_decl_init_list (tree_decl_init_list&)
{
  panic_impossible ();
}

void
tree_breakpoint::visit_simple_for_command (tree_simple_for_command& cmd)
{
  if (cmd.line () >= line)
    take_action (cmd);

  if (! found)
    {
      tree_statement_list *lst = cmd.body ();

      if (lst)
        lst->accept (*this);
    }
}

void
tree_breakpoint::visit_complex_for_command (tree_complex_for_command& cmd)
{
  if (cmd.line () >= line)
    take_action (cmd);

  if (! found)
    {
      tree_statement_list *lst = cmd.body ();

      if (lst)
        lst->accept (*this);
    }
}

void
tree_breakpoint::visit_octave_user_script (octave_user_script& fcn)
{
  tree_statement_list *cmd_list = fcn.body ();

  if (cmd_list)
    cmd_list->accept (*this);
}

void
tree_breakpoint::visit_octave_user_function (octave_user_function& fcn)
{
  tree_statement_list *cmd_list = fcn.body ();

  if (cmd_list)
    cmd_list->accept (*this);
}

void
tree_breakpoint::visit_octave_user_function_header (octave_user_function&)
{
  panic_impossible ();
}

void
tree_breakpoint::visit_octave_user_function_trailer (octave_user_function&)
{
  panic_impossible ();
}

void
tree_breakpoint::visit_function_def (tree_function_def& fdef)
{
  octave_value fcn = fdef.function ();

  octave_function *f = fcn.function_value ();

  if (f)
    f->accept (*this);
}

void
tree_breakpoint::visit_identifier (tree_identifier&)
{
  panic_impossible ();
}

void
tree_breakpoint::visit_if_clause (tree_if_clause&)
{
  panic_impossible ();
}

void
tree_breakpoint::visit_if_command (tree_if_command& cmd)
{
  tree_if_command_list *lst = cmd.cmd_list ();

  if (lst)
    lst->accept (*this);
}

void
tree_breakpoint::visit_if_command_list (tree_if_command_list& lst)
{
  for (tree_if_command_list::iterator p = lst.begin (); p != lst.end (); p++)
    {
      tree_if_clause *t = *p;

      if (t->line () >= line)
        take_action (*t);

      if (! found)
        {
          tree_statement_list *stmt_lst = t->commands ();

          if (stmt_lst)
            stmt_lst->accept (*this);
        }

      if (found)
        break;
    }
}

void
tree_breakpoint::visit_index_expression (tree_index_expression&)
{
  panic_impossible ();
}

void
tree_breakpoint::visit_matrix (tree_matrix&)
{
  panic_impossible ();
}

void
tree_breakpoint::visit_cell (tree_cell&)
{
  panic_impossible ();
}

void
tree_breakpoint::visit_multi_assignment (tree_multi_assignment&)
{
  panic_impossible ();
}

void
tree_breakpoint::visit_no_op_command (tree_no_op_command& cmd)
{
  if (cmd.is_end_of_fcn_or_script () && cmd.line () >= line)
    take_action (cmd);
}

void
tree_breakpoint::visit_anon_fcn_handle (tree_anon_fcn_handle&)
{
  panic_impossible ();
}

void
tree_breakpoint::visit_constant (tree_constant&)
{
  panic_impossible ();
}

void
tree_breakpoint::visit_fcn_handle (tree_fcn_handle&)
{
  panic_impossible ();
}

void
tree_breakpoint::visit_funcall (tree_funcall&)
{
  panic_impossible ();
}

void
tree_breakpoint::visit_parameter_list (tree_parameter_list&)
{
  panic_impossible ();
}

void
tree_breakpoint::visit_postfix_expression (tree_postfix_expression&)
{
  panic_impossible ();
}

void
tree_breakpoint::visit_prefix_expression (tree_prefix_expression&)
{
  panic_impossible ();
}

void
tree_breakpoint::visit_return_command (tree_return_command& cmd)
{
  if (cmd.line () >= line)
    take_action (cmd);
}

void
tree_breakpoint::visit_return_list (tree_return_list&)
{
  panic_impossible ();
}

void
tree_breakpoint::visit_simple_assignment (tree_simple_assignment&)
{
  panic_impossible ();
}

void
tree_breakpoint::visit_statement (tree_statement& stmt)
{
  if (stmt.is_command ())
    {
      tree_command *cmd = stmt.command ();

      cmd->accept (*this);
    }
  else
    {
      if (stmt.line () >= line)
        take_action (stmt);
    }
}

void
tree_breakpoint::visit_statement_list (tree_statement_list& lst)
{
  for (tree_statement_list::iterator p = lst.begin (); p != lst.end (); p++)
    {
      tree_statement *elt = *p;

      if (elt)
        {
          elt->accept (*this);

          if (found)
            break;
        }
    }
}

void
tree_breakpoint::visit_switch_case (tree_switch_case&)
{
  panic_impossible ();
}

void
tree_breakpoint::visit_switch_case_list (tree_switch_case_list& lst)
{
  for (tree_switch_case_list::iterator p = lst.begin (); p != lst.end (); p++)
    {
      tree_switch_case *t = *p;

      if (t->line () >= line)
        take_action (*t);

      if (! found)
        {
          tree_statement_list *stmt_lst = t->commands ();

          if (stmt_lst)
            stmt_lst->accept (*this);
        }

      if (found)
        break;
    }
}

void
tree_breakpoint::visit_switch_command (tree_switch_command& cmd)
{
  if (cmd.line () >= line)
    take_action (cmd);

  if (! found)
    {
      tree_switch_case_list *lst = cmd.case_list ();

      if (lst)
        lst->accept (*this);
    }
}

void
tree_breakpoint::visit_try_catch_command (tree_try_catch_command& cmd)
{
  tree_statement_list *try_code = cmd.body ();

  if (try_code)
    try_code->accept (*this);

  if (! found)
    {
      tree_statement_list *catch_code = cmd.cleanup ();

      if (catch_code)
        catch_code->accept (*this);
    }
}

void
tree_breakpoint::visit_unwind_protect_command (tree_unwind_protect_command& cmd)
{
  tree_statement_list *body = cmd.body ();

  if (body)
    body->accept (*this);

  if (! found)
    {
      tree_statement_list *cleanup = cmd.cleanup ();

      if (cleanup)
        cleanup->accept (*this);
    }
}

void
tree_breakpoint::take_action (tree& tr)
{
  if (act == set)
    {
      tr.set_breakpoint ();
      line = tr.line ();
      found = true;
    }
  else if (act == clear)
    {
      if (tr.is_breakpoint ())
        {
          tr.delete_breakpoint ();
          found = true;
        }
    }
  else if (act == list)
    {
      if (tr.is_breakpoint ())
        bp_list.append (octave_value (tr.line ()));
    }
  else
    panic_impossible ();
}

void
tree_breakpoint::take_action (tree_statement& stmt)
{
  int lineno = stmt.line ();

  if (act == set)
    {
      stmt.set_breakpoint ();
      line = lineno;
      found = true;
    }
  else if (act == clear)
    {
      if (stmt.is_breakpoint ())
        {
          stmt.delete_breakpoint ();
          found = true;
        }
    }
  else if (act == list)
    {
      if (stmt.is_breakpoint ())
        bp_list.append (octave_value (lineno));
    }
  else
    panic_impossible ();
}
