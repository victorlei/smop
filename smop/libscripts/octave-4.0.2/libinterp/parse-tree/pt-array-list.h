/*

Copyright (C) 2013-2015 John W. Eaton

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

#if !defined (octave_pt_array_list_h)
#define octave_pt_array_list_h 1

#include "base-list.h"
#include "pt-arg-list.h"
#include "pt-exp.h"
#include "symtab.h"

// Base class for cell arrays and matrices.

class
tree_array_list : public tree_expression,
                  public octave_base_list<tree_argument_list *>
{
public:

  typedef octave_base_list<tree_argument_list *>::iterator iterator;
  typedef octave_base_list<tree_argument_list *>::const_iterator const_iterator;

  tree_array_list (tree_argument_list *row = 0, int l = -1, int c = -1)
    : tree_expression (l, c), octave_base_list<tree_argument_list *> ()
  {
    if (row)
      append (row);
  }

  ~tree_array_list (void);

  bool all_elements_are_constant (void) const;

  bool has_magic_end (void) const;

  void copy_base (const tree_array_list& array_list);

  void copy_base (const tree_array_list& array_list,
                  symbol_table::scope_id scope,
                  symbol_table::context_id context);

  tree_expression *dup (symbol_table::scope_id scope,
                        symbol_table::context_id context) const;

  void accept (tree_walker& tw);

private:

  // No copying!

  tree_array_list (const tree_array_list&);

  tree_array_list& operator = (const tree_array_list&);
};

#endif
