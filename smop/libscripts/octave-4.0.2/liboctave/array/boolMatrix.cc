// Matrix manipulations.
/*

Copyright (C) 1996-2015 John W. Eaton
Copyright (C) 2009-2010 VZLU Prague, a.s.

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

#include "Array-util.h"
#include "boolMatrix.h"
#include "lo-error.h"
#include "str-vec.h"
#include "mx-inlines.cc"
#include "mx-op-defs.h"

// boolMatrix class.

bool
boolMatrix::operator == (const boolMatrix& a) const
{
  if (rows () != a.rows () || cols () != a.cols ())
    return 0;

  return mx_inline_equal (length (), data (), a.data ());
}

bool
boolMatrix::operator != (const boolMatrix& a) const
{
  return !(*this == a);
}

boolMatrix&
boolMatrix::insert (const boolMatrix& a, octave_idx_type r, octave_idx_type c)
{
  Array<bool>::insert (a, r, c);
  return *this;
}

// unary operations

boolMatrix
boolMatrix::operator ! (void) const
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  boolMatrix b (nr, nc);

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      b.elem (i, j) = ! elem (i, j);

  return b;
}

// other operations

boolMatrix
boolMatrix::diag (octave_idx_type k) const
{
  return Array<bool>::diag (k);
}

MM_BOOL_OPS (boolMatrix, boolMatrix)
MS_BOOL_OPS (boolMatrix, bool)
SM_BOOL_OPS (bool, boolMatrix)
MM_CMP_OPS (boolMatrix, boolMatrix)
