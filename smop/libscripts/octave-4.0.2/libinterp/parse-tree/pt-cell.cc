/*

Copyright (C) 1999-2015 John W. Eaton

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

#include <iostream>

#include "Cell.h"
#include "oct-obj.h"
#include "pt-arg-list.h"
#include "pt-exp.h"
#include "pt-cell.h"
#include "pt-walk.h"
#include "ov.h"

octave_value
tree_cell::rvalue1 (int)
{
  octave_value retval;

  octave_idx_type nr = length ();
  octave_idx_type nc = -1;

  Cell val;

  octave_idx_type i = 0;

  for (iterator p = begin (); p != end (); p++)
    {
      tree_argument_list *elt = *p;

      octave_value_list row = elt->convert_to_const_vector ();

      if (nr == 1)
        // Optimize the single row case.
        val = row.cell_value ();
      else if (nc < 0)
        {
          nc = row.length ();

          val = Cell (nr, nc);
        }
      else
        {
          octave_idx_type this_nc = row.length ();

          if (this_nc != nc)
            {
              if (this_nc == 0)
                continue;  // blank line
              else
                {
                  ::error ("number of columns must match");
                  return retval;
                }
            }
        }

      for (octave_idx_type j = 0; j < nc; j++)
        val(i,j) = row(j);

      i++;
    }

  if (i < nr)
    val.resize (dim_vector (i, nc));  // there were blank rows
  retval = val;

  return retval;
}

octave_value_list
tree_cell::rvalue (int nargout)
{
  octave_value_list retval;

  if (nargout > 1)
    error ("invalid number of output arguments for cell array");
  else
    retval = rvalue1 (nargout);

  return retval;
}

tree_expression *
tree_cell::dup (symbol_table::scope_id scope,
                symbol_table::context_id context) const
{
  tree_cell *new_cell = new tree_cell (0, line (), column ());

  new_cell->copy_base (*this, scope, context);

  return new_cell;
}

void
tree_cell::accept (tree_walker& tw)
{
  tw.visit_cell (*this);
}
