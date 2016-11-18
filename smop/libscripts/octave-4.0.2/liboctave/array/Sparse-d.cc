/*

Copyright (C) 2004-2015 David Bateman
Copyright (C) 1998-2004 Andy Adler

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

// Instantiate Sparse matrix of double values.

#include "lo-mappers.h"
#include "Sparse.h"
#include "Sparse.cc"

template <>
bool
sparse_ascending_compare<double> (double a, double b)
{
  return (xisnan (b) || (a < b));
}

template <>
bool
sparse_descending_compare<double> (double a, double b)
{
  return (xisnan (a) || (a > b));
}

INSTANTIATE_SPARSE (double, OCTAVE_API);

#if 0
template std::ostream& operator << (std::ostream&, const Sparse<double>&);
#endif
