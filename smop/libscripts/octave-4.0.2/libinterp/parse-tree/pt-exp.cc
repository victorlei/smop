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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <iostream>
#include <string>

#include "error.h"
#include "pager.h"
#include "oct-lvalue.h"
#include "ov.h"
#include "pt-exp.h"

// Expressions.

bool
tree_expression::is_logically_true (const char *warn_for)
{
  bool expr_value = false;

  octave_value t1 = rvalue1 ();

  if (! error_state)
    {
      if (t1.is_defined ())
        return t1.is_true ();
      else
        ::error ("%s: undefined value used in conditional expression",
                 warn_for);
    }

  return expr_value;
}

octave_value
tree_expression::rvalue1 (int)
{
  ::error ("invalid rvalue function called in expression");
  return octave_value ();
}

octave_value_list
tree_expression::rvalue (int)
{
  ::error ("invalid rvalue function called in expression");
  return octave_value_list ();
}

octave_value_list
tree_expression::rvalue (int nargout, const std::list<octave_lvalue> *)
{
  return rvalue (nargout);
}

octave_lvalue
tree_expression::lvalue (void)
{
  ::error ("invalid lvalue function called in expression");
  return octave_lvalue ();
}

std::string
tree_expression::original_text (void) const
{
  return std::string ();
}
