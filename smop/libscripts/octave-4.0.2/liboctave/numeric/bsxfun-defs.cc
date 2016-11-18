/*

Copyright (C) 2009-2015 Jaroslav Hajek
Copyright (C) 2009 VZLU Prague

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

#if !defined (octave_bsxfun_defs_h)
#define octave_bsxfun_defs_h 1

#include <algorithm>
#include <iostream>

#include "dim-vector.h"
#include "oct-locbuf.h"
#include "lo-error.h"

#include "mx-inlines.cc"

template <class R, class X, class Y>
Array<R>
do_bsxfun_op (const Array<X>& x, const Array<Y>& y,
              void (*op_vv) (size_t, R *, const X *, const Y *),
              void (*op_sv) (size_t, R *, X, const Y *),
              void (*op_vs) (size_t, R *, const X *, Y))
{
  int nd = std::max (x.ndims (), y.ndims ());
  dim_vector dvx = x.dims ().redim (nd);
  dim_vector dvy = y.dims ().redim (nd);

  // Construct the result dimensions.
  dim_vector dvr;
  dvr.resize (nd);
  for (int i = 0; i < nd; i++)
    {
      octave_idx_type xk = dvx(i);
      octave_idx_type yk = dvy(i);
      if (xk == 1)
        dvr(i) = yk;
      else if (yk == 1 || xk == yk)
        dvr(i) = xk;
      else
        {
          (*current_liboctave_error_handler)
            ("bsxfun: nonconformant dimensions: %s and %s",
             x.dims ().str ().c_str (), y.dims ().str ().c_str ());
          break;
        }
    }

  Array<R> retval (dvr);

  const X *xvec = x.fortran_vec ();
  const Y *yvec = y.fortran_vec ();
  R *rvec = retval.fortran_vec ();

  // Fold the common leading dimensions.
  octave_idx_type start, ldr = 1;
  for (start = 0; start < nd; start++)
    {
      if (dvx(start) != dvy(start))
        break;
      ldr *= dvr(start);
    }

  if (retval.is_empty ())
    ; // do nothing
  else if (start == nd)
    op_vv (retval.numel (), rvec, xvec, yvec);
  else
    {
      // Determine the type of the low-level loop.
      bool xsing = false;
      bool ysing = false;
      if (ldr == 1)
        {
          xsing = dvx(start) == 1;
          ysing = dvy(start) == 1;
          if (xsing || ysing)
            {
              ldr *= dvx(start) * dvy(start);
              start++;
            }
        }
      dim_vector cdvx = dvx.cumulative ();
      dim_vector cdvy = dvy.cumulative ();
      // Nullify singleton dims to achieve a spread effect.
      for (int i = std::max (start, octave_idx_type (1)); i < nd; i++)
        {
          if (dvx(i) == 1)
            cdvx(i-1) = 0;
          if (dvy(i) == 1)
            cdvy(i-1) = 0;
        }

      octave_idx_type niter = dvr.numel (start);
      // The index array.
      OCTAVE_LOCAL_BUFFER_INIT (octave_idx_type, idx, nd, 0);
      for (octave_idx_type iter = 0; iter < niter; iter++)
        {
          octave_quit ();

          // Compute indices.
          // FIXME: performance impact noticeable?
          octave_idx_type xidx = cdvx.cum_compute_index (idx);
          octave_idx_type yidx = cdvy.cum_compute_index (idx);
          octave_idx_type ridx = dvr.compute_index (idx);

          // Apply the low-level loop.
          if (xsing)
            op_sv (ldr, rvec + ridx, xvec[xidx], yvec + yidx);
          else if (ysing)
            op_vs (ldr, rvec + ridx, xvec + xidx, yvec[yidx]);
          else
            op_vv (ldr, rvec + ridx, xvec + xidx, yvec + yidx);

          dvr.increment_index (idx + start, start);
        }
    }

  return retval;
}

template <class R, class X>
void
do_inplace_bsxfun_op (Array<R>& r, const Array<X>& x,
                      void (*op_vv) (size_t, R *, const X *),
                      void (*op_vs) (size_t, R *, X))
{
  dim_vector dvr = r.dims ();
  dim_vector dvx = x.dims ();
  octave_idx_type nd = r.ndims ();
  dvx.redim (nd);

  const X* xvec = x.fortran_vec ();
  R* rvec = r.fortran_vec ();

  // Fold the common leading dimensions.
  octave_idx_type start, ldr = 1;
  for (start = 0; start < nd; start++)
    {
      if (dvr(start) != dvx(start))
        break;
      ldr *= dvr(start);
    }

  if (r.is_empty ())
    ; // do nothing
  else if (start == nd)
    op_vv (r.numel (), rvec, xvec);
  else
    {
      // Determine the type of the low-level loop.
      bool xsing = false;
      if (ldr == 1)
        {
          xsing = dvx(start) == 1;
          if (xsing)
            {
              ldr *= dvr(start) * dvx(start);
              start++;
            }
        }

      dim_vector cdvx = dvx.cumulative ();
      // Nullify singleton dims to achieve a spread effect.
      for (int i = std::max (start, octave_idx_type (1)); i < nd; i++)
        {
          if (dvx(i) == 1)
            cdvx(i-1) = 0;
        }

      octave_idx_type niter = dvr.numel (start);
      // The index array.
      OCTAVE_LOCAL_BUFFER_INIT (octave_idx_type, idx, nd, 0);
      for (octave_idx_type iter = 0; iter < niter; iter++)
        {
          octave_quit ();

          // Compute indices.
          // FIXME: performance impact noticeable?
          octave_idx_type xidx = cdvx.cum_compute_index (idx);
          octave_idx_type ridx = dvr.compute_index (idx);

          // Apply the low-level loop.
          if (xsing)
            op_vs (ldr, rvec + ridx, xvec[xidx]);
          else
            op_vv (ldr, rvec + ridx, xvec + xidx);

          dvr.increment_index (idx + start, start);
        }
    }
}

#define BSXFUN_OP_DEF(OP, ARRAY) \
ARRAY bsxfun_ ## OP (const ARRAY& x, const ARRAY& y)

#define BSXFUN_OP2_DEF(OP, ARRAY, ARRAY1, ARRAY2) \
ARRAY bsxfun_ ## OP (const ARRAY1& x, const ARRAY2& y)

#define BSXFUN_REL_DEF(OP, ARRAY) \
boolNDArray bsxfun_ ## OP (const ARRAY& x, const ARRAY& y)

#define BSXFUN_OP_DEF_MXLOOP(OP, ARRAY, LOOP) \
  BSXFUN_OP_DEF(OP, ARRAY) \
  { return do_bsxfun_op<ARRAY::element_type, ARRAY::element_type, ARRAY::element_type> \
    (x, y, LOOP, LOOP, LOOP); }

#define BSXFUN_OP2_DEF_MXLOOP(OP, ARRAY, ARRAY1, ARRAY2, LOOP) \
  BSXFUN_OP2_DEF(OP, ARRAY, ARRAY1, ARRAY2) \
  { return do_bsxfun_op<ARRAY::element_type, ARRAY1::element_type, ARRAY2::element_type> \
    (x, y, LOOP, LOOP, LOOP); }

#define BSXFUN_REL_DEF_MXLOOP(OP, ARRAY, LOOP) \
  BSXFUN_REL_DEF(OP, ARRAY) \
  { return do_bsxfun_op<bool, ARRAY::element_type, ARRAY::element_type> \
    (x, y, LOOP, LOOP, LOOP); }

#define BSXFUN_STDOP_DEFS_MXLOOP(ARRAY) \
  BSXFUN_OP_DEF_MXLOOP (add, ARRAY, mx_inline_add) \
  BSXFUN_OP_DEF_MXLOOP (sub, ARRAY, mx_inline_sub) \
  BSXFUN_OP_DEF_MXLOOP (mul, ARRAY, mx_inline_mul) \
  BSXFUN_OP_DEF_MXLOOP (div, ARRAY, mx_inline_div) \
  BSXFUN_OP_DEF_MXLOOP (min, ARRAY, mx_inline_xmin) \
  BSXFUN_OP_DEF_MXLOOP (max, ARRAY, mx_inline_xmax)

#define BSXFUN_STDREL_DEFS_MXLOOP(ARRAY) \
  BSXFUN_REL_DEF_MXLOOP (eq, ARRAY, mx_inline_eq) \
  BSXFUN_REL_DEF_MXLOOP (ne, ARRAY, mx_inline_ne) \
  BSXFUN_REL_DEF_MXLOOP (lt, ARRAY, mx_inline_lt) \
  BSXFUN_REL_DEF_MXLOOP (le, ARRAY, mx_inline_le) \
  BSXFUN_REL_DEF_MXLOOP (gt, ARRAY, mx_inline_gt) \
  BSXFUN_REL_DEF_MXLOOP (ge, ARRAY, mx_inline_ge)

//For bsxfun power with mixed integer/float types
#define BSXFUN_POW_MIXED_MXLOOP(INT_TYPE)                              \
  BSXFUN_OP2_DEF_MXLOOP (pow, INT_TYPE, INT_TYPE, NDArray, mx_inline_pow) \
  BSXFUN_OP2_DEF_MXLOOP (pow, INT_TYPE, INT_TYPE, FloatNDArray, mx_inline_pow)\
  BSXFUN_OP2_DEF_MXLOOP (pow, INT_TYPE, NDArray, INT_TYPE,  mx_inline_pow) \
  BSXFUN_OP2_DEF_MXLOOP (pow, INT_TYPE, FloatNDArray, INT_TYPE, mx_inline_pow)

#endif
