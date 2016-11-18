/*

Copyright (C) 2010-2015 VZLU Prague

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

#include <iostream>
#include <algorithm>

#include "f77-fcn.h"

#include "oct-convn.h"
#include "oct-locbuf.h"

// 2d convolution with a matrix kernel.
template <class T, class R>
static void
convolve_2d (const T *a, octave_idx_type ma, octave_idx_type na,
             const R *b, octave_idx_type mb, octave_idx_type nb,
             T *c, bool inner);

// Forward instances to our Fortran implementations.
#define FORWARD_IMPL(T,R,f,F) \
extern "C" \
F77_RET_T \
F77_FUNC (f##conv2o, F##CONV2O) (const octave_idx_type&, \
                                 const octave_idx_type&, \
                                 const T*, const octave_idx_type&, \
                                 const octave_idx_type&, const R*, T *); \
\
extern "C" \
F77_RET_T \
F77_FUNC (f##conv2i, F##CONV2I) (const octave_idx_type&, \
                                 const octave_idx_type&, \
                                 const T*, const octave_idx_type&, \
                                 const octave_idx_type&, const R*, T *); \
\
template <> void \
convolve_2d<T, R> (const T *a, octave_idx_type ma, octave_idx_type na, \
                   const R *b, octave_idx_type mb, octave_idx_type nb, \
                   T *c, bool inner) \
{ \
  if (inner) \
    F77_XFCN (f##conv2i, F##CONV2I, (ma, na, a, mb, nb, b, c)); \
  else \
    F77_XFCN (f##conv2o, F##CONV2O, (ma, na, a, mb, nb, b, c)); \
}

FORWARD_IMPL (double, double, d, D)
FORWARD_IMPL (float, float, s, S)
FORWARD_IMPL (Complex, Complex, z, Z)
FORWARD_IMPL (FloatComplex, FloatComplex, c, C)
FORWARD_IMPL (Complex, double, zd, ZD)
FORWARD_IMPL (FloatComplex, float, cs, CS)

template <class T, class R>
void convolve_nd (const T *a, const dim_vector& ad, const dim_vector& acd,
                  const R *b, const dim_vector& bd, const dim_vector& bcd,
                  T *c, const dim_vector& ccd, int nd, bool inner)
{
  if (nd == 2)
    convolve_2d<T, R> (a, ad(0), ad(1), b, bd(0), bd(1), c, inner);
  else
    {
      octave_idx_type ma = acd(nd-2);
      octave_idx_type na = ad(nd-1);
      octave_idx_type mb = bcd(nd-2);
      octave_idx_type nb = bd(nd-1);
      octave_idx_type ldc = ccd(nd-2);
      if (inner)
        {
          for (octave_idx_type ja = 0; ja < na - nb + 1; ja++)
            for (octave_idx_type jb = 0; jb < nb; jb++)
              convolve_nd<T, R> (a + ma*(ja+jb), ad, acd,
                                 b + mb*(nb-jb-1), bd, bcd,
                                 c + ldc*ja, ccd, nd-1, inner);
        }
      else
        {
          for (octave_idx_type ja = 0; ja < na; ja++)
            for (octave_idx_type jb = 0; jb < nb; jb++)
              convolve_nd<T, R> (a + ma*ja, ad, acd, b + mb*jb, bd, bcd,
                                 c + ldc*(ja+jb), ccd, nd-1, inner);
        }
    }
}

// Arbitrary convolutor.
// The 2nd array is assumed to be the smaller one.
template <class T, class R>
static MArray<T>
convolve (const MArray<T>& a, const MArray<R>& b,
          convn_type ct)
{
  if (a.is_empty () || b.is_empty ())
    return MArray<T> ();

  int nd = std::max (a.ndims (), b.ndims ());
  const dim_vector adims = a.dims ().redim (nd);
  const dim_vector bdims = b.dims ().redim (nd);
  dim_vector cdims = dim_vector::alloc (nd);

  for (int i = 0; i < nd; i++)
    {
      if (ct == convn_valid)
        cdims(i) = std::max (adims(i) - bdims(i) + 1,
                             static_cast<octave_idx_type> (0));
      else
        cdims(i) = std::max (adims(i) + bdims(i) - 1,
                             static_cast<octave_idx_type> (0));
    }

  MArray<T> c (cdims, T ());

  convolve_nd<T, R> (a.fortran_vec (), adims, adims.cumulative (),
                     b.fortran_vec (), bdims, bdims.cumulative (),
                     c.fortran_vec (), cdims.cumulative (),
                     nd, ct == convn_valid);

  if (ct == convn_same)
    {
      // Pick the relevant part.
      Array<idx_vector> sidx (dim_vector (nd, 1));

      for (int i = 0; i < nd; i++)
        sidx(i) = idx_vector::make_range (bdims(i)/2, 1, adims(i));
      c = c.index (sidx);
    }

  return c;
}

#define CONV_DEFS(TPREF, RPREF) \
TPREF ## NDArray \
convn (const TPREF ## NDArray& a, const RPREF ## NDArray& b, convn_type ct) \
{ \
  return convolve (a, b, ct); \
} \
TPREF ## Matrix \
convn (const TPREF ## Matrix& a, const RPREF ## Matrix& b, convn_type ct) \
{ \
  return convolve (a, b, ct); \
} \
TPREF ## Matrix \
convn (const TPREF ## Matrix& a, const RPREF ## ColumnVector& c, \
       const RPREF ## RowVector& r, convn_type ct) \
{ \
  return convolve (a, c * r, ct); \
}

CONV_DEFS ( , )
CONV_DEFS (Complex, )
CONV_DEFS (Complex, Complex)
CONV_DEFS (Float, Float)
CONV_DEFS (FloatComplex, Float)
CONV_DEFS (FloatComplex, FloatComplex)
