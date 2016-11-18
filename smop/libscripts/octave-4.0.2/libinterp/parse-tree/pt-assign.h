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

#if !defined (octave_pt_assign_h)
#define octave_pt_assign_h 1

#include <iosfwd>
#include <string>

class tree_argument_list;
class tree_walker;

class octave_value;
class octave_value_list;
class octave_lvalue;

#include "ov.h"
#include "pt-exp.h"
#include "symtab.h"

// Simple assignment expressions.

class
tree_simple_assignment : public tree_expression
{
public:

  tree_simple_assignment (bool plhs = false, int l = -1, int c = -1,
                          octave_value::assign_op t = octave_value::op_asn_eq)
    : tree_expression (l, c), lhs (0), rhs (0), preserve (plhs), ans_ass (),
      etype (t) { }

  tree_simple_assignment (tree_expression *le, tree_expression *re,
                          bool plhs = false, int l = -1, int c = -1,
                          octave_value::assign_op t = octave_value::op_asn_eq);

  ~tree_simple_assignment (void);

  bool has_magic_end (void) const { return (rhs && rhs->has_magic_end ()); }

  bool rvalue_ok (void) const { return true; }

  octave_value rvalue1 (int nargout = 1);

  octave_value_list rvalue (int nargout);

  bool is_assignment_expression (void) const { return true; }

  std::string oper (void) const;

  tree_expression *left_hand_side (void) { return lhs; }

  tree_expression *right_hand_side (void) { return rhs; }

  tree_expression *dup (symbol_table::scope_id scope,
                        symbol_table::context_id context) const;

  void accept (tree_walker& tw);

  octave_value::assign_op op_type (void) const { return etype; }

private:

  void do_assign (octave_lvalue& ult, const octave_value_list& args,
                  const octave_value& rhs_val);

  void do_assign (octave_lvalue& ult, const octave_value& rhs_val);

  // The left hand side of the assignment.
  tree_expression *lhs;

  // The right hand side of the assignment.
  tree_expression *rhs;

  // True if we should not delete the lhs.
  bool preserve;

  // True if this is an assignment to the automatic variable ans.
  bool ans_ass;

  // The type of the expression.
  octave_value::assign_op etype;

  // No copying!

  tree_simple_assignment (const tree_simple_assignment&);

  tree_simple_assignment& operator = (const tree_simple_assignment&);
};

// Multi-valued assignment expressions.

class
tree_multi_assignment : public tree_expression
{
public:

  tree_multi_assignment (bool plhs = false, int l = -1, int c = -1)
    : tree_expression (l, c), lhs (0), rhs (0), preserve (plhs) { }

  tree_multi_assignment (tree_argument_list *lst, tree_expression *r,
                         bool plhs = false, int l = -1, int c = -1);

  ~tree_multi_assignment (void);

  bool has_magic_end (void) const { return (rhs && rhs->has_magic_end ()); }

  bool is_assignment_expression (void) const { return true; }

  bool rvalue_ok (void) const { return true; }

  octave_value rvalue1 (int nargout = 1);

  octave_value_list rvalue (int nargout);

  std::string oper (void) const;

  tree_argument_list *left_hand_side (void) { return lhs; }

  tree_expression *right_hand_side (void) { return rhs; }

  tree_expression *dup (symbol_table::scope_id scope,
                        symbol_table::context_id context) const;

  void accept (tree_walker& tw);

  octave_value::assign_op op_type (void) const
  { return octave_value::op_asn_eq; }

private:

  // The left hand side of the assignment.
  tree_argument_list *lhs;

  // The right hand side of the assignment.
  tree_expression *rhs;

  // True if we should not delete the lhs.
  bool preserve;

  // No copying!

  tree_multi_assignment (const tree_multi_assignment&);

  tree_multi_assignment& operator = (const tree_multi_assignment&);
};

#endif
