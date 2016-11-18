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

#if !defined (octave_pt_colon_h)
#define octave_tree_colon 1

#include <string>

class tree_walker;

class octave_value;
class octave_value_list;
class octave_lvalue;

#include "pt-exp.h"
#include "symtab.h"

// Colon expressions.

class
tree_colon_expression : public tree_expression
{
public:

  tree_colon_expression (int l = -1, int c = -1)
    : tree_expression (l, c), op_base (0), op_limit (0),
      op_increment (0), save_base (false) { }

  tree_colon_expression (tree_expression *e, int l = -1, int c = -1)
    : tree_expression (l, c), op_base (e), op_limit (0),
      op_increment (0), save_base (false) { }

  tree_colon_expression (tree_expression *bas, tree_expression *lim,
                         tree_expression *inc, int l = -1, int c = -1)
    : tree_expression (l, c), op_base (bas), op_limit (lim),
      op_increment (inc), save_base (false) { }

  ~tree_colon_expression (void)
  {
    if (! save_base)
      delete op_base;

    delete op_limit;
    delete op_increment;
  }

  bool has_magic_end (void) const
  {
    return ((op_base && op_base->has_magic_end ())
            || (op_limit && op_limit->has_magic_end ())
            || (op_increment && op_increment->has_magic_end ()));
  }

  void preserve_base (void) { save_base = true; }

  tree_colon_expression *append (tree_expression *t);

  bool rvalue_ok (void) const { return true; }

  octave_value rvalue1 (int nargout = 1);

  octave_value_list rvalue (int nargout);

  void eval_error (const std::string& s) const;

  tree_expression *base (void) { return op_base; }

  tree_expression *limit (void) { return op_limit; }

  tree_expression *increment (void) { return op_increment; }

  int line (void) const;
  int column (void) const;

  tree_expression *dup (symbol_table::scope_id scope,
                        symbol_table::context_id context) const;

  void accept (tree_walker& tw);

private:

  // The components of the expression.
  tree_expression *op_base;
  tree_expression *op_limit;
  tree_expression *op_increment;

  bool save_base;

  // No copying!

  tree_colon_expression (const tree_colon_expression&);

  tree_colon_expression& operator = (const tree_colon_expression&);
};

#endif
