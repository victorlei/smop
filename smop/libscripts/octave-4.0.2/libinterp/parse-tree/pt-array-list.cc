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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <iostream>

#include "error.h"
#include "pt-array-list.h"

tree_array_list::~tree_array_list (void)
{
  while (! empty ())
    {
      iterator p = begin ();
      delete *p;
      erase (p);
    }
}

bool
tree_array_list::all_elements_are_constant (void) const
{
  for (const_iterator p = begin (); p != end (); p++)
    {
      octave_quit ();

      tree_argument_list *elt = *p;

      if (! elt->all_elements_are_constant ())
        return false;
    }

  return true;
}

bool
tree_array_list::has_magic_end (void) const
{
  for (const_iterator p = begin (); p != end (); p++)
    {
      octave_quit ();

      tree_argument_list *elt = *p;

      if (elt && elt->has_magic_end ())
        return true;
    }

  return false;
}

void
tree_array_list::copy_base (const tree_array_list& array_list)
{
  tree_expression::copy_base (array_list);
}

void
tree_array_list::copy_base (const tree_array_list& array_list,
                            symbol_table::scope_id scope,
                            symbol_table::context_id context)
{
  for (const_iterator p = array_list.begin (); p != array_list.end (); p++)
    {
      const tree_argument_list *elt = *p;

      append (elt ? elt->dup (scope, context) : 0);
    }

  copy_base (*this);
}

tree_expression *
tree_array_list::dup (symbol_table::scope_id,
                      symbol_table::context_id) const
{
  panic_impossible ();
  return 0;
}

void
tree_array_list::accept (tree_walker&)
{
  panic_impossible ();
}

