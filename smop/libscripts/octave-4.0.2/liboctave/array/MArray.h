// Template array classes with like-type math ops
/*

Copyright (C) 1993-2015 John W. Eaton
Copyright (C) 2010 VZLU Prague

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

#if !defined (octave_MArray_h)
#define octave_MArray_h 1

#include "Array.h"

// N-dimensional array with math ops.

// But first, some preprocessor abuse...

#include "MArray-decl.h"

MARRAY_OPS_FORWARD_DECLS (MArray, )

template <class T>
class
MArray : public Array<T>
{
protected:

  // For jit support
  MArray (T *sdata, octave_idx_type slen, octave_idx_type *adims, void *arep)
    : Array<T> (sdata, slen, adims, arep) { }

public:

  MArray (void) : Array<T> () { }

  explicit MArray (const dim_vector& dv)
    : Array<T> (dv) { }

  explicit MArray (const dim_vector& dv, const T& val)
    : Array<T> (dv, val) { }

  MArray (const MArray<T>& a) : Array<T> (a) { }

  template <class U>
  MArray (const Array<U>& a) : Array<T> (a) { }

  ~MArray (void) { }

  MArray<T>& operator = (const MArray<T>& a)
  {
    Array<T>::operator = (a);
    return *this;
  }

  MArray<T> reshape (const dim_vector& new_dims) const
  { return Array<T>::reshape (new_dims); }

  MArray<T> permute (const Array<octave_idx_type>& vec,
                     bool inv = false) const
  { return Array<T>::permute (vec, inv); }

  MArray<T> ipermute (const Array<octave_idx_type>& vec) const
  { return Array<T>::ipermute (vec); }

  MArray squeeze (void) const { return Array<T>::squeeze (); }

  MArray<T> transpose (void) const
  { return Array<T>::transpose (); }

  MArray<T> hermitian (T (*fcn) (const T&) = 0) const
  { return Array<T>::hermitian (fcn); }

  // Performs indexed accumulative addition.

  void idx_add (const idx_vector& idx, T val);

  void idx_add (const idx_vector& idx, const MArray<T>& vals);

  void idx_min (const idx_vector& idx, const MArray<T>& vals);

  void idx_max (const idx_vector& idx, const MArray<T>& vals);

  void idx_add_nd (const idx_vector& idx, const MArray<T>& vals, int dim = -1);

  void changesign (void);
};

#endif
