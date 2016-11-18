// N-D Array  manipulations.
/*

Copyright (C) 1996-2015 John W. Eaton
Copyright (C) 2009 VZLU Prague, a.s.

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

#include "Array-util.h"
#include "boolNDArray.h"
#include "CNDArray.h"
#include "mx-base.h"
#include "lo-ieee.h"
#include "mx-op-defs.h"
#include "MArray-defs.h"

#include "bsxfun-defs.cc"

// unary operations

boolNDArray
boolNDArray::operator ! (void) const
{
  return do_mx_unary_op<bool, bool> (*this, mx_inline_not);
}

boolNDArray&
boolNDArray::invert (void)
{
  if (is_shared ())
    *this = ! *this;
  else
    do_mx_inplace_op<bool> (*this, mx_inline_not2);

  return *this;
}

// FIXME: this is not quite the right thing.

boolNDArray
boolNDArray::all (int dim) const
{
  return do_mx_red_op<bool, bool> (*this, dim, mx_inline_all);
}

boolNDArray
boolNDArray::any (int dim) const
{
  return do_mx_red_op<bool, bool> (*this, dim, mx_inline_any);
}

NDArray
boolNDArray::sum (int dim) const
{
  // NOTE: going via octave_idx_type is typically faster even though it
  // requires a conversion.
  return do_mx_red_op<octave_idx_type, bool> (*this, dim, mx_inline_count);
}

NDArray
boolNDArray::cumsum (int dim) const
{
  // In this case, it's better to sum directly to doubles.
  return do_mx_cum_op<double , bool> (*this, dim, mx_inline_cumcount);
}

boolNDArray
boolNDArray::concat (const boolNDArray& rb,
                     const Array<octave_idx_type>& ra_idx)
{
  if (rb.numel () > 0)
    insert (rb, ra_idx);
  return *this;
}

boolNDArray&
boolNDArray::insert (const boolNDArray& a, octave_idx_type r, octave_idx_type c)
{
  Array<bool>::insert (a, r, c);
  return *this;
}

boolNDArray&
boolNDArray::insert (const boolNDArray& a, const Array<octave_idx_type>& ra_idx)
{
  Array<bool>::insert (a, ra_idx);
  return *this;
}

void
boolNDArray::increment_index (Array<octave_idx_type>& ra_idx,
                              const dim_vector& dimensions,
                              int start_dimension)
{
  ::increment_index (ra_idx, dimensions, start_dimension);
}

octave_idx_type
boolNDArray::compute_index (Array<octave_idx_type>& ra_idx,
                            const dim_vector& dimensions)
{
  return ::compute_index (ra_idx, dimensions);
}

boolNDArray
boolNDArray::diag (octave_idx_type k) const
{
  return Array<bool>::diag (k);
}

boolNDArray
boolNDArray::diag (octave_idx_type m, octave_idx_type n) const
{
  return Array<bool>::diag (m, n);
}

NDND_BOOL_OPS (boolNDArray, boolNDArray)
NDND_CMP_OPS (boolNDArray, boolNDArray)

NDS_BOOL_OPS (boolNDArray, bool)
NDS_CMP_OPS (boolNDArray, bool)

SND_BOOL_OPS (bool, boolNDArray)
SND_CMP_OPS (bool, boolNDArray)

boolNDArray&
mx_el_and_assign (boolNDArray& a, const boolNDArray& b)
{
  if (a.is_shared ())
    a = mx_el_and (a, b);
  else
    do_mm_inplace_op<bool, bool> (a, b, mx_inline_and2, mx_inline_and2,
                                  "operator &=");

  return a;
}

boolNDArray&
mx_el_or_assign (boolNDArray& a, const boolNDArray& b)
{
  if (a.is_shared ())
    a = mx_el_or (a, b);
  else
    do_mm_inplace_op<bool, bool> (a, b, mx_inline_or2, mx_inline_or2,
                                  "operator |=");

  return a;
}

BSXFUN_OP_DEF_MXLOOP (and, boolNDArray, mx_inline_and)
BSXFUN_OP_DEF_MXLOOP (or, boolNDArray, mx_inline_or)
