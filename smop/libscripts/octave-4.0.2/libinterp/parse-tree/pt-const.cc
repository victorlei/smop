/*

Copyright (C) 1993-2015 John W. Eaton

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
#include "oct-obj.h"
#include "pager.h"
#include "pt-const.h"
#include "pt-walk.h"

// We are likely to have a lot of tree_constant objects to allocate,
// so make the grow_size large.

void
tree_constant::print (std::ostream& os, bool pr_as_read_syntax,
                      bool pr_orig_text)
{
  if (pr_orig_text && ! orig_text.empty ())
    os << orig_text;
  else
    val.print (os, pr_as_read_syntax);
}

void
tree_constant::print_raw (std::ostream& os, bool pr_as_read_syntax,
                          bool pr_orig_text)
{
  if (pr_orig_text && ! orig_text.empty ())
    os << orig_text;
  else
    val.print_raw (os, pr_as_read_syntax);
}

octave_value_list
tree_constant::rvalue (int nargout)
{
  octave_value_list retval;

  if (nargout > 1)
    error ("invalid number of output arguments for constant expression");
  else
    retval = rvalue1 (nargout);

  return retval;
}

tree_expression *
tree_constant::dup (symbol_table::scope_id,
                    symbol_table::context_id) const
{
  tree_constant *new_tc
    = new tree_constant (val, orig_text, line (), column ());

  new_tc->copy_base (*this);

  return new_tc;
}

void
tree_constant::accept (tree_walker& tw)
{
  tw.visit_constant (*this);
}
