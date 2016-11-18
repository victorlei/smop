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

#if !defined (octave_dNDArray_h)
#define octave_dNDArray_h 1

#include "MArray.h"
#include "intNDArray.h"

#include "mx-defs.h"
#include "mx-op-decl.h"
#include "bsxfun-decl.h"

class
OCTAVE_API
NDArray : public MArray<double>
{
public:

  NDArray (void) : MArray<double> () { }

  NDArray (const dim_vector& dv) : MArray<double> (dv) { }

  NDArray (const dim_vector& dv, double val)
    : MArray<double> (dv, val) { }

  NDArray (const NDArray& a) : MArray<double> (a) { }

  NDArray (const Array<octave_idx_type>& a, bool zero_based = false,
           bool negative_to_nan = false);

  template <class U>
  NDArray (const MArray<U>& a) : MArray<double> (a) { }

  template <class U>
  NDArray (const Array<U>& a) : MArray<double> (a) { }

  template <class U>
  explicit NDArray (const intNDArray<U>& a) : MArray<double> (a) { }

  NDArray (const charNDArray&);

  // For jit support only
  NDArray (double *sdata, octave_idx_type slen, octave_idx_type *adims,
           void *arep)
    : MArray<double> (sdata, slen, adims, arep) { }

  NDArray& operator = (const NDArray& a)
  {
    MArray<double>::operator = (a);
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
  bool all_integers (double& max_val, double& min_val) const;
  bool all_integers (void) const;
  bool too_large_for_float (void) const;

  // FIXME: this is not quite the right thing.

  boolNDArray all (int dim = -1) const;
  boolNDArray any (int dim = -1) const;

  NDArray cumprod (int dim = -1) const;
  NDArray cumsum (int dim = -1) const;
  NDArray prod (int dim = -1) const;
  NDArray sum (int dim = -1) const;
  NDArray xsum (int dim = -1) const;
  NDArray sumsq (int dim = -1) const;
  NDArray concat (const NDArray& rb, const Array<octave_idx_type>& ra_idx);
  ComplexNDArray concat (const ComplexNDArray& rb,
                         const Array<octave_idx_type>& ra_idx);
  charNDArray concat (const charNDArray& rb,
                      const Array<octave_idx_type>& ra_idx);

  NDArray max (int dim = -1) const;
  NDArray max (Array<octave_idx_type>& index, int dim = -1) const;
  NDArray min (int dim = -1) const;
  NDArray min (Array<octave_idx_type>& index, int dim = -1) const;

  NDArray cummax (int dim = -1) const;
  NDArray cummax (Array<octave_idx_type>& index, int dim = -1) const;
  NDArray cummin (int dim = -1) const;
  NDArray cummin (Array<octave_idx_type>& index, int dim = -1) const;

  NDArray diff (octave_idx_type order = 1, int dim = -1) const;

  NDArray& insert (const NDArray& a, octave_idx_type r, octave_idx_type c);
  NDArray& insert (const NDArray& a, const Array<octave_idx_type>& ra_idx);

  NDArray abs (void) const;
  boolNDArray isnan (void) const;
  boolNDArray isinf (void) const;
  boolNDArray isfinite (void) const;

  ComplexNDArray fourier (int dim = 1) const;
  ComplexNDArray ifourier (int dim = 1) const;

  ComplexNDArray fourier2d (void) const;
  ComplexNDArray ifourier2d (void) const;

  ComplexNDArray fourierNd (void) const;
  ComplexNDArray ifourierNd (void) const;

  friend OCTAVE_API NDArray real (const ComplexNDArray& a);
  friend OCTAVE_API NDArray imag (const ComplexNDArray& a);

  friend class ComplexNDArray;

  NDArray squeeze (void) const { return MArray<double>::squeeze (); }

  static void increment_index (Array<octave_idx_type>& ra_idx,
                               const dim_vector& dimensions,
                               int start_dimension = 0);

  static octave_idx_type compute_index (Array<octave_idx_type>& ra_idx,
                                        const dim_vector& dimensions);

  // i/o

  friend OCTAVE_API std::ostream& operator << (std::ostream& os,
                                               const NDArray& a);
  friend OCTAVE_API std::istream& operator >> (std::istream& is, NDArray& a);

  NDArray diag (octave_idx_type k = 0) const;

  NDArray diag (octave_idx_type m, octave_idx_type n) const;

  NDArray& changesign (void)
  {
    MArray<double>::changesign ();
    return *this;
  }

};

// Publish externally used friend functions.

extern OCTAVE_API NDArray real (const ComplexNDArray& a);
extern OCTAVE_API NDArray imag (const ComplexNDArray& a);

MINMAX_DECLS (NDArray, double, OCTAVE_API)

NDS_CMP_OP_DECLS (NDArray, double, OCTAVE_API)
NDS_BOOL_OP_DECLS (NDArray, double, OCTAVE_API)

SND_CMP_OP_DECLS (double, NDArray, OCTAVE_API)
SND_BOOL_OP_DECLS (double, NDArray, OCTAVE_API)

NDND_CMP_OP_DECLS (NDArray, NDArray, OCTAVE_API)
NDND_BOOL_OP_DECLS (NDArray, NDArray, OCTAVE_API)

MARRAY_FORWARD_DEFS (MArray, NDArray, double)

BSXFUN_STDOP_DECLS (NDArray, OCTAVE_API)
BSXFUN_STDREL_DECLS (NDArray, OCTAVE_API)

BSXFUN_OP_DECL (pow, NDArray, OCTAVE_API)
BSXFUN_OP2_DECL (pow, ComplexNDArray, ComplexNDArray,
                 NDArray, OCTAVE_API)
BSXFUN_OP2_DECL (pow, ComplexNDArray, NDArray,
                 ComplexNDArray, OCTAVE_API)

#endif
