/*

Copyright (C) 2003-2015 John W. Eaton

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

#if !defined (octave_CNDArray_h)
#define octave_CNDArray_h 1

#include "MArray.h"

#include "mx-defs.h"
#include "mx-op-decl.h"
#include "bsxfun-decl.h"

class
OCTAVE_API
ComplexNDArray : public MArray<Complex>
{
public:

  ComplexNDArray (void) : MArray<Complex> () { }

  ComplexNDArray (const dim_vector& dv) : MArray<Complex> (dv) { }

  ComplexNDArray (const dim_vector& dv, const Complex& val)
    : MArray<Complex> (dv, val) { }

  ComplexNDArray (const ComplexNDArray& a) : MArray<Complex> (a) { }

  template <class U>
  ComplexNDArray (const MArray<U>& a) : MArray<Complex> (a) { }

  template <class U>
  ComplexNDArray (const Array<U>& a) : MArray<Complex> (a) { }

  ComplexNDArray (const charNDArray&);

  ComplexNDArray& operator = (const ComplexNDArray& a)
  {
    MArray<Complex>::operator = (a);
    return *this;
  }

  // unary operations

  boolNDArray operator ! (void) const;

  // FIXME: this is not quite the right thing.

  bool any_element_is_nan (void) const;
  bool any_element_is_inf_or_nan (void) const;
  bool all_elements_are_real (void) const;
  bool all_integers (double& max_val, double& min_val) const;
  bool too_large_for_float (void) const;

  boolNDArray all (int dim = -1) const;
  boolNDArray any (int dim = -1) const;

  ComplexNDArray cumprod (int dim = -1) const;
  ComplexNDArray cumsum (int dim = -1) const;
  ComplexNDArray prod (int dim = -1) const;
  ComplexNDArray sum (int dim = -1) const;
  ComplexNDArray xsum (int dim = -1) const;
  ComplexNDArray sumsq (int dim = -1) const;
  ComplexNDArray concat (const ComplexNDArray& rb,
                         const Array<octave_idx_type>& ra_idx);
  ComplexNDArray concat (const NDArray& rb,
                         const Array<octave_idx_type>& ra_idx);

  ComplexNDArray max (int dim = -1) const;
  ComplexNDArray max (Array<octave_idx_type>& index, int dim = -1) const;
  ComplexNDArray min (int dim = -1) const;
  ComplexNDArray min (Array<octave_idx_type>& index, int dim = -1) const;

  ComplexNDArray cummax (int dim = -1) const;
  ComplexNDArray cummax (Array<octave_idx_type>& index, int dim = -1) const;
  ComplexNDArray cummin (int dim = -1) const;
  ComplexNDArray cummin (Array<octave_idx_type>& index, int dim = -1) const;

  ComplexNDArray diff (octave_idx_type order = 1, int dim = -1) const;

  ComplexNDArray& insert (const NDArray& a,
                          octave_idx_type r, octave_idx_type c);
  ComplexNDArray& insert (const ComplexNDArray& a,
                          octave_idx_type r, octave_idx_type c);
  ComplexNDArray& insert (const ComplexNDArray& a,
                          const Array<octave_idx_type>& ra_idx);

  NDArray abs (void) const;
  boolNDArray isnan (void) const;
  boolNDArray isinf (void) const;
  boolNDArray isfinite (void) const;

  friend OCTAVE_API ComplexNDArray conj (const ComplexNDArray& a);

  ComplexNDArray fourier (int dim = 1) const;
  ComplexNDArray ifourier (int dim = 1) const;

  ComplexNDArray fourier2d (void) const;
  ComplexNDArray ifourier2d (void) const;

  ComplexNDArray fourierNd (void) const;
  ComplexNDArray ifourierNd (void) const;

  ComplexNDArray squeeze (void) const { return MArray<Complex>::squeeze (); }

  static void increment_index (Array<octave_idx_type>& ra_idx,
                               const dim_vector& dimensions,
                               int start_dimension = 0);

  static octave_idx_type compute_index (Array<octave_idx_type>& ra_idx,
                                        const dim_vector& dimensions);

  // i/o

  friend OCTAVE_API std::ostream& operator << (std::ostream& os,
                                               const ComplexNDArray& a);
  friend OCTAVE_API std::istream& operator >> (std::istream& is,
                                               ComplexNDArray& a);

  //  bool all_elements_are_real (void) const;
  //  bool all_integers (double& max_val, double& min_val) const;

  ComplexNDArray diag (octave_idx_type k = 0) const;

  ComplexNDArray diag (octave_idx_type m, octave_idx_type n) const;

  ComplexNDArray& changesign (void)
  {
    MArray<Complex>::changesign ();
    return *this;
  }

};

extern OCTAVE_API ComplexNDArray conj (const ComplexNDArray& a);

MINMAX_DECLS (ComplexNDArray, Complex, OCTAVE_API)

NDS_CMP_OP_DECLS (ComplexNDArray, Complex, OCTAVE_API)
NDS_BOOL_OP_DECLS (ComplexNDArray, Complex, OCTAVE_API)

SND_CMP_OP_DECLS (Complex, ComplexNDArray, OCTAVE_API)
SND_BOOL_OP_DECLS (Complex, ComplexNDArray, OCTAVE_API)

NDND_CMP_OP_DECLS (ComplexNDArray, ComplexNDArray, OCTAVE_API)
NDND_BOOL_OP_DECLS (ComplexNDArray, ComplexNDArray, OCTAVE_API)

MARRAY_FORWARD_DEFS (MArray, ComplexNDArray, Complex)

extern OCTAVE_API ComplexNDArray& operator *= (ComplexNDArray& a, double s);
extern OCTAVE_API ComplexNDArray& operator /= (ComplexNDArray& a, double s);

BSXFUN_STDOP_DECLS (ComplexNDArray, OCTAVE_API)
BSXFUN_STDREL_DECLS (ComplexNDArray, OCTAVE_API)

BSXFUN_OP_DECL (pow, ComplexNDArray, OCTAVE_API)

#endif
