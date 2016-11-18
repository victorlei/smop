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

#if !defined (octave_fNDArray_h)
#define octave_fNDArray_h 1

#include "MArray.h"
#include "intNDArray.h"

#include "mx-defs.h"
#include "mx-op-decl.h"
#include "bsxfun-decl.h"

class
OCTAVE_API
FloatNDArray : public MArray<float>
{
public:

  FloatNDArray (void) : MArray<float> () { }

  FloatNDArray (const dim_vector& dv) : MArray<float> (dv) { }

  FloatNDArray (const dim_vector& dv, float val)
    : MArray<float> (dv, val) { }

  FloatNDArray (const FloatNDArray& a) : MArray<float> (a) { }

  template <class U>
  FloatNDArray (const MArray<U>& a) : MArray<float> (a) { }

  template <class U>
  FloatNDArray (const Array<U>& a) : MArray<float> (a) { }

  template <class U>
  explicit FloatNDArray (const intNDArray<U>& a) : MArray<float> (a) { }

  FloatNDArray (const charNDArray&);

  FloatNDArray& operator = (const FloatNDArray& a)
  {
    MArray<float>::operator = (a);
    return *this;
  }

  // unary operations

  boolNDArray operator ! (void) const;

  bool any_element_is_negative (bool = false) const;
  bool any_element_is_positive (bool = false) const;
  bool any_element_is_nan (void) const;
  bool any_element_is_inf_or_nan (void) const;
  bool any_element_not_one_or_zero (void) const;
  bool all_elements_are_zero (void) const;
  bool all_elements_are_int_or_inf_or_nan (void) const;
  bool all_integers (float& max_val, float& min_val) const;
  bool all_integers (void) const;
  bool too_large_for_float (void) const;

  // FIXME: this is not quite the right thing.

  boolNDArray all (int dim = -1) const;
  boolNDArray any (int dim = -1) const;

  FloatNDArray cumprod (int dim = -1) const;
  FloatNDArray cumsum (int dim = -1) const;
  FloatNDArray prod (int dim = -1) const;
  NDArray dprod (int dim = -1) const;
  FloatNDArray sum (int dim = -1) const;
  NDArray dsum (int dim = -1) const;
  FloatNDArray sumsq (int dim = -1) const;
  FloatNDArray concat (const FloatNDArray& rb,
                       const Array<octave_idx_type>& ra_idx);
  FloatComplexNDArray concat (const FloatComplexNDArray& rb,
                              const Array<octave_idx_type>& ra_idx);
  charNDArray concat (const charNDArray& rb,
                      const Array<octave_idx_type>& ra_idx);

  FloatNDArray max (int dim = -1) const;
  FloatNDArray max (Array<octave_idx_type>& index, int dim = -1) const;
  FloatNDArray min (int dim = -1) const;
  FloatNDArray min (Array<octave_idx_type>& index, int dim = -1) const;

  FloatNDArray cummax (int dim = -1) const;
  FloatNDArray cummax (Array<octave_idx_type>& index, int dim = -1) const;
  FloatNDArray cummin (int dim = -1) const;
  FloatNDArray cummin (Array<octave_idx_type>& index, int dim = -1) const;

  FloatNDArray diff (octave_idx_type order = 1, int dim = -1) const;

  FloatNDArray& insert (const FloatNDArray& a,
                        octave_idx_type r, octave_idx_type c);
  FloatNDArray& insert (const FloatNDArray& a,
                        const Array<octave_idx_type>& ra_idx);

  FloatNDArray abs (void) const;
  boolNDArray isnan (void) const;
  boolNDArray isinf (void) const;
  boolNDArray isfinite (void) const;

  FloatComplexNDArray fourier (int dim = 1) const;
  FloatComplexNDArray ifourier (int dim = 1) const;

  FloatComplexNDArray fourier2d (void) const;
  FloatComplexNDArray ifourier2d (void) const;

  FloatComplexNDArray fourierNd (void) const;
  FloatComplexNDArray ifourierNd (void) const;

  friend OCTAVE_API FloatNDArray real (const FloatComplexNDArray& a);
  friend OCTAVE_API FloatNDArray imag (const FloatComplexNDArray& a);

  friend class FloatComplexNDArray;

  FloatNDArray squeeze (void) const { return MArray<float>::squeeze (); }

  static void increment_index (Array<octave_idx_type>& ra_idx,
                               const dim_vector& dimensions,
                               int start_dimension = 0);

  static octave_idx_type compute_index (Array<octave_idx_type>& ra_idx,
                                        const dim_vector& dimensions);

  // i/o

  friend OCTAVE_API std::ostream& operator << (std::ostream& os,
                                               const FloatNDArray& a);
  friend OCTAVE_API std::istream& operator >> (std::istream& is, FloatNDArray& a);

  FloatNDArray diag (octave_idx_type k = 0) const;

  FloatNDArray diag (octave_idx_type m, octave_idx_type n) const;

  FloatNDArray& changesign (void)
  {
    MArray<float>::changesign ();
    return *this;
  }

};

// Publish externally used friend functions.

extern OCTAVE_API FloatNDArray real (const FloatComplexNDArray& a);
extern OCTAVE_API FloatNDArray imag (const FloatComplexNDArray& a);

MINMAX_DECLS (FloatNDArray, float, OCTAVE_API)

NDS_CMP_OP_DECLS (FloatNDArray, float, OCTAVE_API)
NDS_BOOL_OP_DECLS (FloatNDArray, float, OCTAVE_API)

SND_CMP_OP_DECLS (float, FloatNDArray, OCTAVE_API)
SND_BOOL_OP_DECLS (float, FloatNDArray, OCTAVE_API)

NDND_CMP_OP_DECLS (FloatNDArray, FloatNDArray, OCTAVE_API)
NDND_BOOL_OP_DECLS (FloatNDArray, FloatNDArray, OCTAVE_API)

MARRAY_FORWARD_DEFS (MArray, FloatNDArray, float)

BSXFUN_STDOP_DECLS (FloatNDArray, OCTAVE_API)
BSXFUN_STDREL_DECLS (FloatNDArray, OCTAVE_API)

BSXFUN_OP_DECL (pow, FloatNDArray, OCTAVE_API)
BSXFUN_OP2_DECL (pow, FloatComplexNDArray, FloatComplexNDArray,
                 FloatNDArray, OCTAVE_API)
BSXFUN_OP2_DECL (pow, FloatComplexNDArray, FloatNDArray,
                 FloatComplexNDArray, OCTAVE_API)

#endif
