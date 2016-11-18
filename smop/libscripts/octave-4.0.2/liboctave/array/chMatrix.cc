// Matrix manipulations.
/*

Copyright (C) 1995-2015 John W. Eaton
Copyright (C) 2010 VZLU Prague

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

#include <cstring>

#include <iostream>
#include <string>

#include "lo-error.h"
#include "str-vec.h"
#include "mx-base.h"
#include "mx-inlines.cc"
#include "mx-op-defs.h"

// charMatrix class.

bool
charMatrix::operator == (const charMatrix& a) const
{
  if (rows () != a.rows () || cols () != a.cols ())
    return 0;

  return mx_inline_equal (length (), data (), a.data ());
}

bool
charMatrix::operator != (const charMatrix& a) const
{
  return !(*this == a);
}

charMatrix&
charMatrix::insert (const char *s, octave_idx_type r, octave_idx_type c)
{
  if (s)
    {
      octave_idx_type s_len = strlen (s);

      if (r < 0 || r >= rows () || c < 0 || c + s_len - 1 > cols ())
        {
          (*current_liboctave_error_handler) ("range error for insert");
          return *this;
        }

      for (octave_idx_type i = 0; i < s_len; i++)
        elem (r, c+i) = s[i];
    }
  return *this;
}

charMatrix&
charMatrix::insert (const charMatrix& a, octave_idx_type r, octave_idx_type c)
{
  Array<char>::insert (a, r, c);
  return *this;
}

std::string
charMatrix::row_as_string (octave_idx_type r, bool strip_ws) const
{
  std::string retval;

  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  if (r == 0 && (nr == 0 || nc == 0))
    return retval;

  if (r < 0 || r >= nr)
    {
      (*current_liboctave_error_handler) ("range error for row_as_string");
      return retval;
    }

  retval.resize (nc, '\0');

  for (octave_idx_type i = 0; i < nc; i++)
    retval[i] = elem (r, i);

  if (strip_ws)
    {
      while (--nc >= 0)
        {
          char c = retval[nc];
          if (c && c != ' ')
            break;
        }

      retval.resize (nc+1);
    }

  return retval;
}

charMatrix
charMatrix::extract (octave_idx_type r1, octave_idx_type c1,
                     octave_idx_type r2, octave_idx_type c2) const
{
  if (r1 > r2) { std::swap (r1, r2); }
  if (c1 > c2) { std::swap (c1, c2); }

  octave_idx_type new_r = r2 - r1 + 1;
  octave_idx_type new_c = c2 - c1 + 1;

  charMatrix result (new_r, new_c);

  for (octave_idx_type j = 0; j < new_c; j++)
    for (octave_idx_type i = 0; i < new_r; i++)
      result.elem (i, j) = elem (r1+i, c1+j);

  return result;
}

MS_CMP_OPS (charMatrix, char)
MS_BOOL_OPS (charMatrix, char)

SM_CMP_OPS (char, charMatrix)
SM_BOOL_OPS (char, charMatrix)

MM_CMP_OPS (charMatrix, charMatrix)
MM_BOOL_OPS (charMatrix, charMatrix)
