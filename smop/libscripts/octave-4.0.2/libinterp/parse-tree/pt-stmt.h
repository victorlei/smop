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

#if !defined (octave_pt_stmt_h)
#define octave_pt_stmt_h 1

class octave_value_list;

class tree_command;
class tree_expression;

class tree_walker;

#include <deque>

#include "base-list.h"
#include "comment-list.h"
#include "debug.h"
#include "symtab.h"
#include "pt.h"

// A statement is either a command to execute or an expression to
// evaluate.

class
tree_statement : public tree
{
public:

  tree_statement (void)
    : cmd (0), expr (0), comm (0) { }

  tree_statement (tree_command *c, octave_comment_list *cl)
    : cmd (c), expr (0), comm (cl) { }

  tree_statement (tree_expression *e, octave_comment_list *cl)
    : cmd (0), expr (e), comm (cl) { }

  ~tree_statement (void);

  void set_print_flag (bool print_flag);

  bool print_result (void);

  bool is_command (void) const { return cmd != 0; }

  bool is_expression (void) const { return expr != 0; }

  void set_breakpoint (void);

  void delete_breakpoint (void);

  bool is_breakpoint (void) const;

  int line (void) const;
  int column (void) const;

  void set_location (int l, int c);

  void echo_code (void);

  tree_command *command (void) { return cmd; }

  tree_expression *expression (void) { return expr; }

  octave_comment_list *comment_text (void) { return comm; }

  bool is_null_statement (void) const { return ! (cmd || expr || comm); }

  bool is_end_of_fcn_or_script (void) const;

  bool is_end_of_file (void) const;

  // Allow modification of this statement.  Note that there is no
  // checking.  If you use these, are you sure you knwo what you are
  // doing?

  void set_command (tree_command *c) { cmd = c; }

  void set_expression (tree_expression *e) { expr = e; }

  tree_statement *dup (symbol_table::scope_id scope,
                       symbol_table::context_id context) const;

  void accept (tree_walker& tw);

private:

  // Only one of cmd or expr can be valid at once.

  // Command to execute.
  tree_command *cmd;

  // Expression to evaluate.
  tree_expression *expr;

  // Comment associated with this statement.
  octave_comment_list *comm;

  // No copying!
  tree_statement (const tree_statement&);

  tree_statement& operator = (const tree_statement&);
};

// A list of statements to evaluate.

class
tree_statement_list : public octave_base_list<tree_statement *>
{
public:

  tree_statement_list (void)
    : function_body (false), anon_function_body (false),
      script_body (false) { }

  tree_statement_list (tree_statement *s)
    : function_body (false), anon_function_body (false),
      script_body (false) { append (s); }

  ~tree_statement_list (void)
  {
    while (! empty ())
      {
        iterator p = begin ();
        delete *p;
        erase (p);
      }
  }

  void mark_as_function_body (void) { function_body = true; }

  void mark_as_anon_function_body (void) { anon_function_body = true; }

  void mark_as_script_body (void) { script_body = true; }

  bool is_function_body (void) const { return function_body; }

  bool is_anon_function_body (void) const { return anon_function_body; }

  bool is_script_body (void) const { return script_body; }

  int set_breakpoint (int line);

  void delete_breakpoint (int line);

  octave_value_list list_breakpoints (void);

  bp_table::intmap add_breakpoint (const std::string& file,
                                   const bp_table::intmap& line);

  bp_table::intmap remove_all_breakpoints (const std::string& file);

  tree_statement_list *dup (symbol_table::scope_id scope,
                            symbol_table::context_id context) const;

  void accept (tree_walker& tw);

private:

  // Does this list of statements make up the body of a function?
  bool function_body;

  // Does this list of statements make up the body of a function?
  bool anon_function_body;

  // Does this list of statements make up the body of a script?
  bool script_body;

  // No copying!

  tree_statement_list (const tree_statement_list&);

  tree_statement_list& operator = (const tree_statement_list&);
};

#endif
