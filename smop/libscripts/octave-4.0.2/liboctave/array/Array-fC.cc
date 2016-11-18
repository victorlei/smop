/*

Copyright (C) 1994-2015 John W. Eaton
Copyright (C) 2009 VZLU Prague

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

// Instantiate Arrays of FloatComplex values.

#include "oct-cmplx.h"
#include "lo-mappers.h"

#include "Array.h"
#include "Array.cc"
#include "oct-sort.cc"

// Prevent implicit instantiations on some systems (Windows, others?)
// that can lead to duplicate definitions of static data members.

extern template class OCTAVE_API Array<idx_vector>;
extern template class OCTAVE_API Array<octave_idx_type>;

template <>
inline bool
sort_isnan<FloatComplex> (const FloatComplex& x)
{
  return xisnan (x);
}

// Sort Criteria: 1) isnan, 2) magnitude of z, 3) phase of z in range (-pi, pi]

static bool
nan_ascending_compare (const FloatComplex& x, const FloatComplex& y)
{
  return xisnan (y) ? ! xisnan (x) : x < y;
}

static bool
nan_descending_compare (const FloatComplex& x, const FloatComplex& y)
{
  return xisnan (x) ? ! xisnan (y) : x > y;
}

Array<FloatComplex>::compare_fcn_type
safe_comparator (sortmode mode, const Array<FloatComplex>& a, bool allow_chk)
{
  Array<FloatComplex>::compare_fcn_type result = 0;

  if (allow_chk)
    {
      octave_idx_type k = 0;
      for (; k < a.numel () && ! xisnan (a(k)); k++) ;
      if (k == a.numel ())
        {
          if (mode == ASCENDING)
            result = octave_sort<FloatComplex>::ascending_compare;
          else if (mode == DESCENDING)
            result = octave_sort<FloatComplex>::descending_compare;
        }
    }

  if (! result)
    {
      if (mode == ASCENDING)
        result = nan_ascending_compare;
      else if (mode == DESCENDING)
        result = nan_descending_compare;
    }

  return result;
}

template class OCTAVE_API octave_sort<FloatComplex>;

INSTANTIATE_ARRAY (FloatComplex, OCTAVE_API);

template OCTAVE_API std::ostream& operator << (std::ostream&,
                                               const Array<FloatComplex>&);

#include "DiagArray2.h"
#include "DiagArray2.cc"

template class OCTAVE_API DiagArray2<FloatComplex>;
