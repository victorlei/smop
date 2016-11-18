/*

Copyright (C) 2004-2015 John W. Eaton

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

#if !defined (octave_intNDArray_h)
#define octave_intNDArray_h 1

#include "MArray.h"
#include "boolNDArray.h"
class NDArray;

template <class T>
class
intNDArray : public MArray<T>
{
public:

  using typename MArray<T>::element_type;

  intNDArray (void) : MArray<T> () { }

  intNDArray (T val) : MArray<T> (dim_vector (1, 1), val) { }

  intNDArray (const dim_vector& dv) : MArray<T> (dv) { }

  intNDArray (const dim_vector& dv, T val)
    : MArray<T> (dv, val) { }

  template <class U>
  intNDArray (const Array<U>& a) : MArray<T> (a) { }

  template <class U>
  intNDArray (const MArray<U>& a) : MArray<T> (a) { }

  template <class U>
  intNDArray (const intNDArray<U>& a) : MArray<T> (a) { }

  intNDArray& operator = (const intNDArray<T>& a)
  {
    MArray<T>::operator = (a);
    return *this;
  }

  boolNDArray operator ! (void) const;

  bool any_element_is_nan (void) const { return false; }
  bool any_element_not_one_or_zero (void) const;

  intNDArray diag (octave_idx_type k = 0) const;

  intNDArray diag (octave_idx_type m, octave_idx_type n) const;

  intNDArray& changesign (void)
  {
    MArray<T>::changesign ();
    return *this;
  }

  // FIXME: this is not quite the right thing.

  boolNDArray all (int dim = -1) const;
  boolNDArray any (int dim = -1) const;

  intNDArray max (int dim = -1) const;
  intNDArray max (Array<octave_idx_type>& index, int dim = -1) const;
  intNDArray min (int dim = -1) const;
  intNDArray min (Array<octave_idx_type>& index, int dim = -1) const;

  intNDArray cummax (int dim = -1) const;
  intNDArray cummax (Array<octave_idx_type>& index, int dim = -1) const;
  intNDArray cummin (int dim = -1) const;
  intNDArray cummin (Array<octave_idx_type>& index, int dim = -1) const;

  intNDArray prod (int dim) const;
  intNDArray sum (int dim) const;
  NDArray dsum (int dim) const;
  intNDArray cumsum (int dim) const;

  intNDArray diff (octave_idx_type order = 1, int dim = -1) const;

  intNDArray abs (void) const;
  intNDArray signum (void) const;

  intNDArray squeeze (void) const
  { return intNDArray<T> (MArray<T>::squeeze ()); }

  intNDArray transpose (void) const
  { return intNDArray<T> (MArray<T>::transpose ()); }

  intNDArray concat (const intNDArray<T>& rb,
                     const Array<octave_idx_type>& ra_idx);

  intNDArray& insert (const intNDArray<T>& a,
                      octave_idx_type r, octave_idx_type c);
  intNDArray& insert (const intNDArray<T>& a,
                      const Array<octave_idx_type>& ra_idx);

  static void increment_index (Array<octave_idx_type>& ra_idx,
                               const dim_vector& dimensions,
                               int start_dimension = 0);

  static octave_idx_type compute_index (Array<octave_idx_type>& ra_idx,
                                        const dim_vector& dimensions);
};

// i/o

template <class T>
std::ostream& operator << (std::ostream& os, const intNDArray<T>& a);

template <class T>
std::istream& operator >> (std::istream& is, intNDArray<T>& a);

#endif
