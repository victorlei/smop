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

// Instantiate Arrays of octave_stream objects.

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "Array.h"
#include "Array.cc"

#include "oct-stream.h"

// Prevent implicit instantiations on some systems (Windows, others?)
// that can lead to duplicate definitions of static data members.

extern template class OCTAVE_API Array<bool>;
extern template class OCTAVE_API Array<idx_vector>;
extern template class OCTAVE_API Array<octave_idx_type>;

typedef scanf_format_elt* scanf_format_elt_ptr;
typedef printf_format_elt* printf_format_elt_ptr;

NO_INSTANTIATE_ARRAY_SORT (scanf_format_elt_ptr);
INSTANTIATE_ARRAY (scanf_format_elt_ptr, OCTINTERP_API);

NO_INSTANTIATE_ARRAY_SORT (printf_format_elt_ptr);
INSTANTIATE_ARRAY (printf_format_elt_ptr, OCTINTERP_API);

NO_INSTANTIATE_ARRAY_SORT (octave_stream);
INSTANTIATE_ARRAY (octave_stream, OCTINTERP_API);
