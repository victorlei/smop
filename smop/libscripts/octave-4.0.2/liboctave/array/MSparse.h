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

#if !defined (octave_MSparse_h)
#define octave_MSparse_h 1

#include "MArray.h"

#include "Sparse.h"

// Two dimensional sparse array with math ops.

// But first, some preprocessor abuse...

#include "MSparse-defs.h"

SPARSE_OPS_FORWARD_DECLS (MSparse, MArray, )

template <class T>
class
MSparse : public Sparse<T>
{
public:

  MSparse (void) : Sparse<T> () { }

  MSparse (octave_idx_type n, octave_idx_type m) : Sparse<T> (n, m) { }

  MSparse (const dim_vector& dv, octave_idx_type nz = 0)
    : Sparse<T> (dv, nz) { }

  MSparse (const MSparse<T>& a) : Sparse<T> (a) { }

  MSparse (const MSparse<T>& a, const dim_vector& dv) : Sparse<T> (a, dv) { }

  MSparse (const Sparse<T>& a) : Sparse<T> (a) { }

  template <class U>
  MSparse (const Sparse<U>& a) : Sparse<T> (a) { }

  MSparse (const Array<T>& a, const idx_vector& r, const idx_vector& c,
           octave_idx_type nr = -1, octave_idx_type nc = -1,
           bool sum_terms = true, octave_idx_type nzm = -1)
    : Sparse<T> (a, r, c, nr, nc, sum_terms, nzm) { }

  explicit MSparse (octave_idx_type r, octave_idx_type c, T val)
    : Sparse<T> (r, c, val) { }

  explicit MSparse (const PermMatrix& a) : Sparse<T>(a) { }

  MSparse (octave_idx_type r, octave_idx_type c, octave_idx_type num_nz)
    : Sparse<T> (r, c, num_nz) { }

  ~MSparse (void) { }

  MSparse<T>& operator = (const MSparse<T>& a)
  {
    Sparse<T>::operator = (a);
    return *this;
  }

  MSparse<T>& insert (const Sparse<T>& a, octave_idx_type r, octave_idx_type c)
  {
    Sparse<T>::insert (a, r, c);
    return *this;
  }

  MSparse<T>& insert (const Sparse<T>& a, const Array<octave_idx_type>& indx)
  {
    Sparse<T>::insert (a, indx);
    return *this;
  }

  MSparse<T> transpose (void) const { return Sparse<T>::transpose (); }

  MSparse<T> squeeze (void) const { return Sparse<T>::squeeze (); }

  MSparse<T> reshape (const dim_vector& new_dims) const
  { return Sparse<T>::reshape (new_dims); }

  MSparse<T> permute (const Array<octave_idx_type>& vec, bool inv = false) const
  { return Sparse<T>::permute (vec, inv); }

  MSparse<T> ipermute (const Array<octave_idx_type>& vec) const
  { return Sparse<T>::ipermute (vec); }

  MSparse<T> diag (octave_idx_type k = 0) const
  {
    return Sparse<T>::diag (k);
  }

  // FIXME: should go away.
  template <class U>
  MSparse<U>
  map (U (&fcn) (T)) const
  { return Sparse<T>::template map<U> (fcn); }

  template <class U>
  MSparse<U>
  map (U (&fcn) (const T&)) const
  { return Sparse<T>::template map<U> (fcn); }

  // Currently, the OPS functions don't need to be friends, but that
  // may change.

  // SPARSE_OPS_FRIEND_DECLS (MSparse, MArray)
};

#endif
