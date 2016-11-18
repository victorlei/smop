// Template array classes with like-type math ops
/*

Copyright (C) 1996-2015 John W. Eaton
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

#if !defined (octave_MDiagArray2_h)
#define octave_MDiagArray2_h 1

#include "DiagArray2.h"
#include "MArray.h"

// Two dimensional diagonal array with math ops.

// But first, some preprocessor abuse...

#include "MArray-decl.h"

MDIAGARRAY2_OPS_FORWARD_DECLS (MDiagArray2, )

template <class T>
class
MDiagArray2 : public DiagArray2<T>
{
public:

  MDiagArray2 (void) : DiagArray2<T> () { }

  MDiagArray2 (octave_idx_type r, octave_idx_type c) : DiagArray2<T> (r, c) { }

  MDiagArray2 (octave_idx_type r, octave_idx_type c, const T& val)
    : DiagArray2<T> (r, c, val) { }

  MDiagArray2 (const MDiagArray2<T>& a) : DiagArray2<T> (a) { }

  MDiagArray2 (const DiagArray2<T>& a) : DiagArray2<T> (a) { }

  template <class U>
  MDiagArray2 (const DiagArray2<U>& a) : DiagArray2<T> (a) { }

  explicit MDiagArray2 (const Array<T>& a) : DiagArray2<T> (a) { }

  MDiagArray2 (const Array<T>& a, octave_idx_type r, octave_idx_type c)
    : DiagArray2<T> (a, r, c) { }

  ~MDiagArray2 (void) { }

  MDiagArray2<T>& operator = (const MDiagArray2<T>& a)
  {
    DiagArray2<T>::operator = (a);
    return *this;
  }

  MArray<T> array_value () const
  {
    return DiagArray2<T>::array_value ();
  }

  octave_idx_type nnz (void) const
  {
    octave_idx_type retval = 0;

    const T *d = this->data ();

    octave_idx_type nel = this->length ();

    for (octave_idx_type i = 0; i < nel; i++)
      {
        if (d[i] != T ())
          retval++;
      }

    return retval;
  }

  MArray<T> diag (octave_idx_type k = 0) const
  { return DiagArray2<T>::extract_diag (k); }

  MDiagArray2<T> transpose (void) const { return DiagArray2<T>::transpose (); }
  MDiagArray2<T> hermitian (T (*fcn) (const T&) = 0) const
  { return DiagArray2<T>::hermitian (fcn); }

  bool is_multiple_of_identity (T val) const;

  // Currently, the OPS functions don't need to be friends, but that
  // may change.

  MDIAGARRAY2_OPS_FRIEND_DECLS (MDiagArray2, )

};

#endif
