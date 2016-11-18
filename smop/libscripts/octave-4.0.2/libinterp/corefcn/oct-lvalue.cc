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

#include "error.h"
#include "oct-obj.h"
#include "oct-lvalue.h"
#include "ov.h"

void
octave_lvalue::assign (octave_value::assign_op op, const octave_value& rhs)
{
  if (! is_black_hole ())
    {
      if (idx.empty ())
        sym->assign (op, rhs);
      else
        sym->assign (op, type, idx, rhs);
    }
}

void
octave_lvalue::set_index (const std::string& t,
                          const std::list<octave_value_list>& i)
{
  if (idx.empty ())
    {
      type = t;
      idx = i;
    }
  else
    error ("invalid index expression in assignment");
}

void
octave_lvalue::do_unary_op (octave_value::unary_op op)
{
  if (! is_black_hole ())
    {
      if (idx.empty ())
        sym->do_non_const_unary_op (op);
      else
        sym->do_non_const_unary_op (op, type, idx);
    }
}

octave_value
octave_lvalue::value (void) const
{
  octave_value retval;

  if (! is_black_hole ())
    {
      octave_value val = sym->varval ();

      if (idx.empty ())
        retval = val;
      else
        {
          if (val.is_constant ())
            retval = val.subsref (type, idx);
          else
            {
              octave_value_list t = val.subsref (type, idx, 1);
              if (t.length () > 0)
                retval = t(0);
            }
        }
    }

  return retval;
}
