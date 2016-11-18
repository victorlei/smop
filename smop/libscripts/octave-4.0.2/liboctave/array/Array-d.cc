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

// Instantiate Arrays of double values.

#include "lo-mappers.h"
#include "Array.h"
#include "Array.cc"
#include "oct-locbuf.h"

#define INLINE_ASCENDING_SORT
#define INLINE_DESCENDING_SORT
#include "oct-sort.cc"

// Prevent implicit instantiations on some systems (Windows, others?)
// that can lead to duplicate definitions of static data members.

extern template class OCTAVE_API Array<idx_vector>;
extern template class OCTAVE_API Array<octave_idx_type>;

template <>
inline bool
sort_isnan<double> (double x)
{
  return xisnan (x);
}

static bool
nan_ascending_compare (double x, double y)
{
  return xisnan (y) ? ! xisnan (x) : x < y;
}

static bool
nan_descending_compare (double x, double y)
{
  return xisnan (x) ? ! xisnan (y) : x > y;
}

Array<double>::compare_fcn_type
safe_comparator (sortmode mode, const Array<double>& a , bool allow_chk)
{
  Array<double>::compare_fcn_type result = 0;

  if (allow_chk)
    {
      octave_idx_type k = 0;
      for (; k < a.numel () && ! xisnan (a(k)); k++) ;
      if (k == a.numel ())
        {
          if (mode == ASCENDING)
            result = octave_sort<double>::ascending_compare;
          else if (mode == DESCENDING)
            result = octave_sort<double>::descending_compare;
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

// The default solution using NaN-safe comparator is OK, but almost twice as
// slow than this code.
template <>
OCTAVE_API
sortmode
Array<double>::is_sorted (sortmode mode) const
{
  octave_idx_type n = numel ();

  const double *el = data ();

  if (n <= 1)
    return mode ? mode : ASCENDING;

  if (! mode)
    {
      // Auto-detect mode.
      if (el[n-1] < el[0] || xisnan (el[0]))
        mode = DESCENDING;
      else
        mode = ASCENDING;
    }

  if (mode == DESCENDING)
    {
      octave_idx_type j = 0;
      double r;
      // Sort out NaNs.
      do
        r = el[j++];
      while (xisnan (r) && j < n);

      // Orient the test so that NaN will not pass through.
      for (; j < n; j++)
        {
          if (r >= el[j])
            r = el[j];
          else
            {
              mode = UNSORTED;
              break;
            }
        }

    }
  else if (mode == ASCENDING)
    {
      // Sort out NaNs.
      while (n > 0 && xisnan (el[n-1]))
        n--;

      if (n > 0)
        {
          // Orient the test so that NaN will not pass through.
          double r = el[0];
          for (octave_idx_type j = 1; j < n; j++)
            {
              if (r <= el[j])
                r = el[j];
              else
                {
                  mode = UNSORTED;
                  break;
                }
            }
        }
    }

  return mode;
}

template class OCTAVE_API octave_sort<double>;

INSTANTIATE_ARRAY (double, OCTAVE_API);

template OCTAVE_API std::ostream& operator << (std::ostream&,
                                               const Array<double>&);

#include "DiagArray2.h"
#include "DiagArray2.cc"

template class OCTAVE_API DiagArray2<double>;
