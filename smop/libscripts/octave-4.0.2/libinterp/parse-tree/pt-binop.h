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

#if !defined (octave_pt_binop_h)
#define octave_pt_binop_h 1

#include <string>

class tree_walker;

class octave_value;
class octave_value_list;
class octave_lvalue;

#include "ov.h"
#include "pt-exp.h"
#include "symtab.h"

// Binary expressions.

class
tree_binary_expression : public tree_expression
{
public:

  tree_binary_expression (int l = -1, int c = -1,
                          octave_value::binary_op t
                            = octave_value::unknown_binary_op)
    : tree_expression (l, c), op_lhs (0), op_rhs (0), etype (t),
      eligible_for_braindead_shortcircuit (false),
      braindead_shortcircuit_warning_issued (false) { }

  tree_binary_expression (tree_expression *a, tree_expression *b,
                          int l = -1, int c = -1,
                          octave_value::binary_op t
                            = octave_value::unknown_binary_op)
    : tree_expression (l, c), op_lhs (a), op_rhs (b), etype (t),
      eligible_for_braindead_shortcircuit (false),
      braindead_shortcircuit_warning_issued (false) { }

  ~tree_binary_expression (void)
  {
    delete op_lhs;
    delete op_rhs;
  }

  void mark_braindead_shortcircuit (void)
  {
    if (etype == octave_value::op_el_and || etype == octave_value::op_el_or)
      {
        eligible_for_braindead_shortcircuit = true;

        op_lhs->mark_braindead_shortcircuit ();
        op_rhs->mark_braindead_shortcircuit ();
      }
  }

  bool has_magic_end (void) const
  {
    return ((op_lhs && op_lhs->has_magic_end ())
            || (op_rhs && op_rhs->has_magic_end ()));
  }

  bool is_binary_expression (void) const { return true; }

  bool rvalue_ok (void) const { return true; }

  octave_value rvalue1 (int nargout = 1);

  octave_value_list rvalue (int nargout);

  std::string oper (void) const;

  octave_value::binary_op op_type (void) const { return etype; }

  tree_expression *lhs (void) { return op_lhs; }
  tree_expression *rhs (void) { return op_rhs; }

  tree_expression *dup (symbol_table::scope_id scope,
                        symbol_table::context_id context) const;

  void accept (tree_walker& tw);

  std::string profiler_name (void) const { return "binary " + oper (); }


protected:

  // The operands for the expression.
  tree_expression *op_lhs;
  tree_expression *op_rhs;

private:

  // The type of the expression.
  octave_value::binary_op etype;

  // TRUE if this is an | or & expression in the condition of an IF
  // or WHILE statement.
  bool eligible_for_braindead_shortcircuit;

  // TRUE if we have already issued a warning about short circuiting
  // for this operator.
  bool braindead_shortcircuit_warning_issued;

  void matlab_style_short_circuit_warning (const char *op);

  // No copying!

  tree_binary_expression (const tree_binary_expression&);

  tree_binary_expression& operator = (const tree_binary_expression&);
};

// Boolean expressions.

class
tree_boolean_expression : public tree_binary_expression
{
public:

  enum type
  {
    unknown,
    bool_and,
    bool_or
  };

  tree_boolean_expression (int l = -1, int c = -1, type t = unknown)
    : tree_binary_expression (l, c), etype (t) { }

  tree_boolean_expression (tree_expression *a, tree_expression *b,
                           int l = -1, int c = -1, type t = unknown)
    : tree_binary_expression (a, b, l, c), etype (t) { }

  ~tree_boolean_expression (void) { }

  bool is_boolean_expression (void) const { return true; }

  bool rvalue_ok (void) const { return true; }

  octave_value rvalue1 (int nargout = 1);

  octave_value_list rvalue (int nargout);

  std::string oper (void) const;

  type op_type (void) const { return etype; }

  tree_expression *dup (symbol_table::scope_id scope,
                        symbol_table::context_id context) const;

private:

  // The type of the expression.
  type etype;

  // No copying!

  tree_boolean_expression (const tree_boolean_expression&);

  tree_boolean_expression& operator = (const tree_boolean_expression&);
};

#endif
