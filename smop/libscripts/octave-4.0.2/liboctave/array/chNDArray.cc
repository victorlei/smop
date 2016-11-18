// N-D Array  manipulations.
/*

Copyright (C) 2003-2015 John W. Eaton

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

#include <string>

#include "Array-util.h"
#include "chNDArray.h"
#include "mx-base.h"
#include "lo-ieee.h"
#include "lo-mappers.h"
#include "mx-op-defs.h"
#include "str-vec.h"

#include "bsxfun-defs.cc"

charNDArray::charNDArray (char c)
  : Array<char> ()
{
  octave_idx_type n = 1;

  resize1 (n);

  elem (0) = c;
}

charNDArray::charNDArray (const char *s)
  : Array<char> ()
{
  octave_idx_type n = s ? strlen (s) : 0;

  resize1 (n);

  for (octave_idx_type i = 0; i < n; i++)
    elem (i) = s[i];
}

charNDArray::charNDArray (const std::string& s)
  : Array<char> ()
{
  octave_idx_type n = s.length ();

  resize1 (n);

  for (octave_idx_type i = 0; i < n; i++)
    elem (i) = s[i];
}

charNDArray::charNDArray (const string_vector& s, char fill_value)
  : Array<char> (dim_vector (s.length (), s.max_length ()), fill_value)
{
  octave_idx_type nr = rows ();

  for (octave_idx_type i = 0; i < nr; i++)
    {
      const std::string si = s(i);
      octave_idx_type nc = si.length ();
      for (octave_idx_type j = 0; j < nc; j++)
        elem (i, j) = si[j];
    }
}

// FIXME: this is not quite the right thing.

boolNDArray
charNDArray::all (int dim) const
{
  return do_mx_red_op<bool, char> (*this, dim, mx_inline_all);
}

boolNDArray
charNDArray::any (int dim) const
{
  return do_mx_red_op<bool, char> (*this, dim, mx_inline_any);
}

charNDArray
charNDArray::concat (const charNDArray& rb,
                     const Array<octave_idx_type>& ra_idx)
{
  if (rb.numel () > 0)
    insert (rb, ra_idx);
  return *this;
}

charNDArray
charNDArray::concat (const NDArray& rb, const Array<octave_idx_type>& ra_idx)
{
  charNDArray tmp (rb.dims ());
  octave_idx_type nel = rb.numel ();

  if (rb.numel () == 0)
    return *this;

  for (octave_idx_type i = 0; i < nel; i++)
    {
      double d = rb.elem (i);

      if (xisnan (d))
        {
          (*current_liboctave_error_handler)
            ("invalid conversion from NaN to character");
          return *this;
        }
      else
        {
          octave_idx_type ival = NINTbig (d);

          if (ival < 0 || ival > std::numeric_limits<unsigned char>::max ())
            // FIXME: is there something better to do? Should we warn the user?
            ival = 0;

          tmp.elem (i) = static_cast<char>(ival);
        }
    }

  insert (tmp, ra_idx);
  return *this;
}

charNDArray
charNDArray::max (int dim) const
{
  return do_mx_minmax_op<char> (*this, dim, mx_inline_max);
}

charNDArray
charNDArray::max (Array<octave_idx_type>& idx_arg, int dim) const
{
  return do_mx_minmax_op<char> (*this, idx_arg, dim, mx_inline_max);
}

charNDArray
charNDArray::min (int dim) const
{
  return do_mx_minmax_op<char> (*this, dim, mx_inline_min);
}

charNDArray
charNDArray::min (Array<octave_idx_type>& idx_arg, int dim) const
{
  return do_mx_minmax_op<char> (*this, idx_arg, dim, mx_inline_min);
}

charNDArray&
charNDArray::insert (const charNDArray& a, octave_idx_type r, octave_idx_type c)
{
  Array<char>::insert (a, r, c);
  return *this;
}

charNDArray&
charNDArray::insert (const charNDArray& a, const Array<octave_idx_type>& ra_idx)
{
  Array<char>::insert (a, ra_idx);
  return *this;
}

void
charNDArray::increment_index (Array<octave_idx_type>& ra_idx,
                              const dim_vector& dimensions,
                              int start_dimension)
{
  ::increment_index (ra_idx, dimensions, start_dimension);
}

octave_idx_type
charNDArray::compute_index (Array<octave_idx_type>& ra_idx,
                            const dim_vector& dimensions)
{
  return ::compute_index (ra_idx, dimensions);
}

charNDArray
charNDArray::diag (octave_idx_type k) const
{
  return Array<char>::diag (k);
}

charNDArray
charNDArray::diag (octave_idx_type m, octave_idx_type n) const
{
  return Array<char>::diag (m, n);
}

charNDArray
min (char d, const charNDArray& m)
{
  return do_sm_binary_op<charNDArray::element_type, char,
                         charNDArray::element_type> (d, m, mx_inline_xmin);
}

charNDArray
min (const charNDArray& m, char d)
{
  return do_ms_binary_op<charNDArray::element_type, charNDArray::element_type,
                         char> (m, d, mx_inline_xmin);
}

charNDArray
min (const charNDArray& a, const charNDArray& b)
{
  return do_mm_binary_op<charNDArray::element_type, charNDArray::element_type,
                         charNDArray::element_type> (a, b, mx_inline_xmin,
                                                     mx_inline_xmin,
                                                     mx_inline_xmin, "min");
}

charNDArray
max (char d, const charNDArray& m)
{
  return do_sm_binary_op<charNDArray::element_type, char,
                         charNDArray::element_type> (d, m, mx_inline_xmax);
}

charNDArray
max (const charNDArray& m, char d)
{
  return do_ms_binary_op<charNDArray::element_type, charNDArray::element_type,
                         char> (m, d, mx_inline_xmax);
}

charNDArray
max (const charNDArray& a, const charNDArray& b)
{
  return do_mm_binary_op<charNDArray::element_type, charNDArray::element_type,
                         charNDArray::element_type> (a, b, mx_inline_xmax,
                                                     mx_inline_xmax,
                                                     mx_inline_xmax, "max");
}

NDS_CMP_OPS (charNDArray, char)
NDS_BOOL_OPS (charNDArray, char)

SND_CMP_OPS (char, charNDArray)
SND_BOOL_OPS (char, charNDArray)

NDND_CMP_OPS (charNDArray, charNDArray)
NDND_BOOL_OPS (charNDArray, charNDArray)

BSXFUN_STDREL_DEFS_MXLOOP (charNDArray)
