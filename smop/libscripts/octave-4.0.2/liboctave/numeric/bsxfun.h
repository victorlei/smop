/*

Copyright (C) 2012-2015 Jordi Gutiérrez Hermoso

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

// Author: Jordi Gutiérrez Hermoso <jordigh@octave.org>

#if !defined (octave_bsxfun_h)
#define octave_bsxfun_h 1

#include <algorithm>

#include "Array.h"
#include "dim-vector.h"
#include "lo-error.h"

inline
bool
is_valid_bsxfun (const std::string& name, const dim_vector& dx,
                 const dim_vector& dy)
{
  for (int i = 0; i < std::min (dx.length (), dy.length ()); i++)
    {
      octave_idx_type xk = dx(i);
      octave_idx_type yk = dy(i);
      // Check the three conditions for valid bsxfun dims
      if (! ((xk == yk) || (xk == 1 && yk > 1) || (xk > 1 && yk == 1)))
        return false;
    }

  (*current_liboctave_warning_with_id_handler)
    ("Octave:language-extension", "performing `%s' automatic broadcasting",
     name.c_str ());

  return true;
}

// since we can't change the size of the assigned-to matrix, we cannot
// apply singleton expansion to it, so the conditions to check are
// different here.
inline
bool
is_valid_inplace_bsxfun (const std::string& name, const dim_vector& dr,
                         const dim_vector& dx)
{
  octave_idx_type drl = dr.length ();
  octave_idx_type dxl = dx.length ();
  if (drl < dxl)
    return false;

  for (int i = 0; i < drl; i++)
    {
      octave_idx_type rk = dr(i);
      octave_idx_type xk = dx(i);

      // Only two valid canditions to check; can't stretch rk
      if (! ((rk == xk) || (rk > 1 && xk == 1)))
        return false;
    }

  (*current_liboctave_warning_with_id_handler)
    ("Octave:language-extension", "performing `%s' automatic broadcasting",
     name.c_str ());

  return true;
}

#include "bsxfun-defs.cc"

#endif
