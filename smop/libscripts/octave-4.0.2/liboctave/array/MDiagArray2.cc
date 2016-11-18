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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "MDiagArray2.h"
#include "Array-util.h"
#include "lo-error.h"

#include "MArray-defs.h"

template <class T>
bool
MDiagArray2<T>::is_multiple_of_identity (T val) const
{
  bool retval = this->rows () == this->cols ();
  if (retval)
    {
      octave_idx_type len = this->length ();
      octave_idx_type i = 0;
      for (; i < len; i++)
        if (DiagArray2<T>::elem (i, i) != val) break;
      retval = i == len;
    }

  return retval;
}

// Two dimensional diagonal array with math ops.

// Element by element MDiagArray2 by MDiagArray2 ops.

// Element by element MDiagArray2 by scalar ops.

#define MARRAY_DAS_OP(OP, FN) \
  template <class T> \
  MDiagArray2<T> \
  operator OP (const MDiagArray2<T>& a, const T& s) \
  { \
    return MDiagArray2<T> (do_ms_binary_op<T, T, T> (a, s, FN), a.d1, a.d2); \
  }

MARRAY_DAS_OP (*, mx_inline_mul)
MARRAY_DAS_OP (/, mx_inline_div)

// Element by element scalar by MDiagArray2 ops.

template <class T>
MDiagArray2<T>
operator * (const T& s, const MDiagArray2<T>& a)
{
  return MDiagArray2<T> (do_sm_binary_op<T, T, T> (s, a, mx_inline_mul),
                                                   a.d1, a.d2);
}

// Element by element MDiagArray2 by MDiagArray2 ops.

#define MARRAY_DADA_OP(FCN, OP, FN) \
  template <class T> \
  MDiagArray2<T> \
  FCN (const MDiagArray2<T>& a, const MDiagArray2<T>& b) \
  { \
    if (a.d1 != b.d1 || a.d2 != b.d2) \
      gripe_nonconformant (#FCN, a.d1, a.d2, b.d1, b.d2); \
    return MDiagArray2<T> (do_mm_binary_op<T, T, T> (a, b, FN, FN, FN, #FCN), a.d1, a.d2); \
  }

MARRAY_DADA_OP (operator +, +, mx_inline_add)
MARRAY_DADA_OP (operator -, -, mx_inline_sub)
MARRAY_DADA_OP (product,    *, mx_inline_mul)

// Unary MDiagArray2 ops.

template <class T>
MDiagArray2<T>
operator + (const MDiagArray2<T>& a)
{
  return a;
}

template <class T>
MDiagArray2<T>
operator - (const MDiagArray2<T>& a)
{
  return MDiagArray2<T> (do_mx_unary_op<T, T> (a, mx_inline_uminus),
                         a.d1, a.d2);
}
