/*

Copyright (C) 1994-2015 John W. Eaton

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

#include "oct-inttypes.h"

// Instantiate Arrays of integer values.

#include "Array.h"
#include "Array.cc"

#define INLINE_ASCENDING_SORT
#define INLINE_DESCENDING_SORT
#include "oct-sort.cc"

// Prevent implicit instantiations on some systems (Windows, others?)
// that can lead to duplicate definitions of static data members.

extern template class OCTAVE_API Array<idx_vector>;

template class OCTAVE_API octave_sort<int>;
template class OCTAVE_API octave_sort<long>;
#if defined (HAVE_LONG_LONG_INT)
template class OCTAVE_API octave_sort<long long>;
#endif

INSTANTIATE_ARRAY (int, OCTAVE_API);
INSTANTIATE_ARRAY (long, OCTAVE_API);
#if defined (HAVE_LONG_LONG_INT)
INSTANTIATE_ARRAY (long long, OCTAVE_API);
#endif

template class OCTAVE_API octave_sort<octave_int8>;
template class OCTAVE_API octave_sort<octave_int16>;
template class OCTAVE_API octave_sort<octave_int32>;
template class OCTAVE_API octave_sort<octave_int64>;

INSTANTIATE_ARRAY (octave_int8, OCTAVE_API);
INSTANTIATE_ARRAY (octave_int16, OCTAVE_API);
INSTANTIATE_ARRAY (octave_int32, OCTAVE_API);
INSTANTIATE_ARRAY (octave_int64, OCTAVE_API);

template class OCTAVE_API octave_sort<octave_uint8>;
template class OCTAVE_API octave_sort<octave_uint16>;
template class OCTAVE_API octave_sort<octave_uint32>;
template class OCTAVE_API octave_sort<octave_uint64>;

INSTANTIATE_ARRAY (octave_uint8, OCTAVE_API);
INSTANTIATE_ARRAY (octave_uint16, OCTAVE_API);
INSTANTIATE_ARRAY (octave_uint32, OCTAVE_API);
INSTANTIATE_ARRAY (octave_uint64, OCTAVE_API);

#include "DiagArray2.h"
#include "DiagArray2.cc"

template class OCTAVE_API DiagArray2<int>;
