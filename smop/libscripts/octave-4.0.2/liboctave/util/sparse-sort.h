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

#if !defined (octave_sparse_sort_h)
#define octave_sparse_sort_h

#include "oct-sort.h"

class
octave_sparse_sort_idxl
{
public:
  octave_idx_type r;
  octave_idx_type c;
  octave_idx_type idx;
};

bool octave_sparse_sidxl_comp (octave_sparse_sort_idxl* i,
                               octave_sparse_sort_idxl* j);

class
octave_idx_vector_sort
{
public:
  octave_idx_type i;
  octave_idx_type idx;
};

bool octave_idx_vector_comp (octave_idx_vector_sort* i,
                             octave_idx_vector_sort* j);

#endif
