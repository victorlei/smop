/*

Copyright (C) 2008-2015 Jaroslav Hajek

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

#if !defined (octave_PermMatrix_h)
#define octave_PermMatrix_h 1

#include "Array.h"
#include "mx-defs.h"

// Array<T> is inherited privately so that some methods, like index, don't
// produce unexpected results.

class OCTAVE_API PermMatrix : protected Array<octave_idx_type>
{
public:

  PermMatrix (void) : Array<octave_idx_type> () { }

  PermMatrix (octave_idx_type n);

  PermMatrix (const Array<octave_idx_type>& p) GCC_ATTR_DEPRECATED;

  PermMatrix (const Array<octave_idx_type>& p, bool colp, bool check = true);

  PermMatrix (const PermMatrix& m) : Array<octave_idx_type> (m) { }

  PermMatrix (const idx_vector& idx) GCC_ATTR_DEPRECATED;

  PermMatrix (const idx_vector& idx, bool colp, octave_idx_type n = 0);

  octave_idx_type dim1 (void) const
  { return Array<octave_idx_type>::length (); }
  octave_idx_type dim2 (void) const
  { return Array<octave_idx_type>::length (); }

  octave_idx_type rows (void) const { return dim1 (); }
  octave_idx_type cols (void) const { return dim2 (); }
  octave_idx_type columns (void) const { return dim2 (); }

  octave_idx_type perm_length (void) const
  { return Array<octave_idx_type>::length (); }
  // FIXME: a dangerous ambiguity?
  octave_idx_type length (void) const
  { return perm_length (); }
  octave_idx_type nelem (void) const { return dim1 () * dim2 (); }
  octave_idx_type numel (void) const { return nelem (); }

  size_t byte_size (void) const
  { return Array<octave_idx_type>::byte_size (); }

  dim_vector dims (void) const { return dim_vector (dim1 (), dim2 ()); }

  const Array<octave_idx_type>& col_perm_vec (void) const
  { return *this; }

  octave_idx_type
  elem (octave_idx_type i, octave_idx_type j) const
  {
    return (Array<octave_idx_type>::elem (j) == i) ? 1 : 0;
  }

  octave_idx_type
  checkelem (octave_idx_type i, octave_idx_type j) const;

  octave_idx_type
  operator () (octave_idx_type i, octave_idx_type j) const
  {
#if defined (BOUNDS_CHECKING)
    return checkelem (i, j);
#else
    return elem (i, j);
#endif
  }

  // These are, in fact, super-fast.
  PermMatrix transpose (void) const;
  PermMatrix inverse (void) const;

  // Determinant, i.e. the sign of permutation.
  octave_idx_type determinant (void) const;

  // Efficient integer power of a permutation.
  PermMatrix power (octave_idx_type n) const;

  bool is_col_perm (void) const { return true; }
  bool is_row_perm (void) const { return false; }

  void print_info (std::ostream& os, const std::string& prefix) const
  { Array<octave_idx_type>::print_info (os, prefix); }

  static PermMatrix eye (octave_idx_type n);

private:

  PermMatrix pos_power (octave_idx_type m) const;

  void setup (const Array<octave_idx_type>& p, bool colp, bool check);

  void setup (const idx_vector& idx, bool colp, octave_idx_type n);
};

// Multiplying permutations together.
PermMatrix
OCTAVE_API
operator * (const PermMatrix& a, const PermMatrix& b);

#endif
