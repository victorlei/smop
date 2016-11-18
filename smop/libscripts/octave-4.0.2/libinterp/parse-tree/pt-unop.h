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

#if !defined (octave_pt_unop_h)
#define octave_pt_unop_h 1

#include <string>

class tree_walker;

class octave_value;
class octave_value_list;
class octave_lvalue;

#include "pt-exp.h"
#include "symtab.h"

// Unary expressions.

class
tree_unary_expression : public tree_expression
{
public:

  tree_unary_expression (int l = -1, int c = -1,
                         octave_value::unary_op t
                           = octave_value::unknown_unary_op)
    : tree_expression (l, c), op (0), etype (t)  { }

  tree_unary_expression (tree_expression *e, int l = -1, int c = -1,
                         octave_value::unary_op t
                           = octave_value::unknown_unary_op)
    : tree_expression (l, c), op (e), etype (t) { }

  ~tree_unary_expression (void) { delete op; }

  bool is_unary_expression (void) const { return true; }

  bool has_magic_end (void) const { return (op && op->has_magic_end ()); }

  tree_expression *operand (void) { return op; }

  std::string oper (void) const;

  octave_value::unary_op op_type (void) const { return etype; }

protected:

  // The operand for the expression.
  tree_expression *op;

  // The type of the expression.
  octave_value::unary_op etype;

private:

  // No copying!

  tree_unary_expression (const tree_unary_expression&);

  tree_unary_expression& operator = (const tree_unary_expression&);
};

// Prefix expressions.

class
tree_prefix_expression : public tree_unary_expression
{
public:

  tree_prefix_expression (int l = -1, int c = -1)
    : tree_unary_expression (l, c, octave_value::unknown_unary_op) { }

  tree_prefix_expression (tree_expression *e, int l = -1, int c = -1,
                          octave_value::unary_op t
                            = octave_value::unknown_unary_op)
    : tree_unary_expression (e, l, c, t) { }

  ~tree_prefix_expression (void) { }

  bool rvalue_ok (void) const { return true; }

  octave_value rvalue1 (int nargout = 1);

  octave_value_list rvalue (int nargout);

  tree_expression *dup (symbol_table::scope_id scope,
                        symbol_table::context_id context) const;

  void accept (tree_walker& tw);

  std::string profiler_name (void) const { return "prefix " + oper (); }


private:

  // No copying!

  tree_prefix_expression (const tree_prefix_expression&);

  tree_prefix_expression& operator = (const tree_prefix_expression&);
};

// Postfix expressions.

class
tree_postfix_expression : public tree_unary_expression
{
public:

  tree_postfix_expression (int l = -1, int c = -1)
    : tree_unary_expression (l, c, octave_value::unknown_unary_op) { }

  tree_postfix_expression (tree_expression *e, int l = -1, int c = -1,
                           octave_value::unary_op t
                             = octave_value::unknown_unary_op)
    : tree_unary_expression (e, l, c, t) { }

  ~tree_postfix_expression (void) { }

  bool rvalue_ok (void) const { return true; }

  octave_value rvalue1 (int nargout = 1);

  octave_value_list rvalue (int nargout);

  tree_expression *dup (symbol_table::scope_id scope,
                        symbol_table::context_id context) const;

  void accept (tree_walker& tw);

  std::string profiler_name (void) const { return "postfix " + oper (); }

private:

  // No copying!

  tree_postfix_expression (const tree_postfix_expression&);

  tree_postfix_expression& operator = (const tree_postfix_expression&);
};

#endif
