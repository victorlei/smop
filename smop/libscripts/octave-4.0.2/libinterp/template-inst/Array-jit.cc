/*

Copyright (C) 2012-2015 Max Brister

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

// Author: Max Brister <max@2bass.com>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef HAVE_LLVM

#include "Array.h"
#include "Array.cc"

#include "jit-ir.h"

// Prevent implicit instantiations on some systems (Windows, others?)
// that can lead to duplicate definitions of static data members.

extern template class OCTAVE_API Array<idx_vector>;
extern template class OCTAVE_API Array<octave_idx_type>;

NO_INSTANTIATE_ARRAY_SORT (jit_function);

INSTANTIATE_ARRAY (jit_function, OCTINTERP_API);

#ifdef Cell_h
#error Must not include Cell.h in Array-jit.h
#error This causes problems on MSVC
#endif

#endif
