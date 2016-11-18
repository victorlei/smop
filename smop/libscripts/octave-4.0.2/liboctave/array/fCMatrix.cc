// Matrix manipulations.
/*

Copyright (C) 1994-2015 John W. Eaton
Copyright (C) 2008-2009 Jaroslav Hajek
Copyright (C) 2009 VZLU Prague, a.s.

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

#include <cfloat>

#include <iostream>
#include <vector>

// FIXME
#include <sys/types.h>

#include "Array-util.h"
#include "DET.h"
#include "f77-fcn.h"
#include "boolMatrix.h"
#include "chMatrix.h"
#include "fCMatrix.h"
#include "fCNDArray.h"
#include "fCDiagMatrix.h"
#include "fCColVector.h"
#include "fCRowVector.h"
#include "fCmplxCHOL.h"
#include "fCmplxSCHUR.h"
#include "fCmplxSVD.h"
#include "functor.h"
#include "lo-error.h"
#include "lo-ieee.h"
#include "lo-mappers.h"
#include "lo-utils.h"
#include "mx-fcm-fdm.h"
#include "mx-fcm-fs.h"
#include "mx-fdm-fcm.h"
#include "mx-inlines.cc"
#include "mx-op-defs.h"
#include "oct-cmplx.h"
#include "oct-fftw.h"
#include "oct-locbuf.h"
#include "oct-norm.h"

// Fortran functions we call.

extern "C"
{
  F77_RET_T
  F77_FUNC (xilaenv, XILAENV) (const octave_idx_type&,
                               F77_CONST_CHAR_ARG_DECL,
                               F77_CONST_CHAR_ARG_DECL,
                               const octave_idx_type&, const octave_idx_type&,
                               const octave_idx_type&, const octave_idx_type&,
                               octave_idx_type&
                               F77_CHAR_ARG_LEN_DECL
                               F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (cgebal, CGEBAL) (F77_CONST_CHAR_ARG_DECL,
                             const octave_idx_type&, FloatComplex*,
                             const octave_idx_type&, octave_idx_type&,
                             octave_idx_type&, float*, octave_idx_type&
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (sgebak, SGEBAK) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             const octave_idx_type&, const octave_idx_type&,
                             const octave_idx_type&, float*,
                             const octave_idx_type&, float*,
                             const octave_idx_type&, octave_idx_type&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (cgemm, CGEMM) (F77_CONST_CHAR_ARG_DECL,
                           F77_CONST_CHAR_ARG_DECL,
                           const octave_idx_type&, const octave_idx_type&,
                           const octave_idx_type&, const FloatComplex&,
                           const FloatComplex*, const octave_idx_type&,
                           const FloatComplex*, const octave_idx_type&,
                           const FloatComplex&, FloatComplex*,
                           const octave_idx_type&
                           F77_CHAR_ARG_LEN_DECL
                           F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (cgemv, CGEMV) (F77_CONST_CHAR_ARG_DECL,
                           const octave_idx_type&, const octave_idx_type&,
                           const FloatComplex&, const FloatComplex*,
                           const octave_idx_type&, const FloatComplex*,
                           const octave_idx_type&, const FloatComplex&,
                           FloatComplex*, const octave_idx_type&
                           F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (xcdotu, XCDOTU) (const octave_idx_type&, const FloatComplex*,
                             const octave_idx_type&, const FloatComplex*,
                             const octave_idx_type&, FloatComplex&);

  F77_RET_T
  F77_FUNC (xcdotc, XCDOTC) (const octave_idx_type&, const FloatComplex*,
                             const octave_idx_type&, const FloatComplex*,
                             const octave_idx_type&, FloatComplex&);

  F77_RET_T
  F77_FUNC (csyrk, CSYRK) (F77_CONST_CHAR_ARG_DECL,
                           F77_CONST_CHAR_ARG_DECL,
                           const octave_idx_type&, const octave_idx_type&,
                           const FloatComplex&, const FloatComplex*,
                           const octave_idx_type&, const FloatComplex&,
                           FloatComplex*, const octave_idx_type&
                           F77_CHAR_ARG_LEN_DECL
                           F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (cherk, CHERK) (F77_CONST_CHAR_ARG_DECL,
                           F77_CONST_CHAR_ARG_DECL,
                           const octave_idx_type&, const octave_idx_type&,
                           const float&, const FloatComplex*,
                           const octave_idx_type&, const float&,
                           FloatComplex*, const octave_idx_type&
                           F77_CHAR_ARG_LEN_DECL
                           F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (cgetrf, CGETRF) (const octave_idx_type&, const octave_idx_type&,
                             FloatComplex*, const octave_idx_type&,
                             octave_idx_type*, octave_idx_type&);

  F77_RET_T
  F77_FUNC (cgetrs, CGETRS) (F77_CONST_CHAR_ARG_DECL,
                             const octave_idx_type&, const octave_idx_type&,
                             FloatComplex*, const octave_idx_type&,
                             const octave_idx_type*, FloatComplex*,
                             const octave_idx_type&, octave_idx_type&
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (cgetri, CGETRI) (const octave_idx_type&, FloatComplex*,
                             const octave_idx_type&, const octave_idx_type*,
                             FloatComplex*, const octave_idx_type&,
                             octave_idx_type&);

  F77_RET_T
  F77_FUNC (cgecon, CGECON) (F77_CONST_CHAR_ARG_DECL,
                             const octave_idx_type&, FloatComplex*,
                             const octave_idx_type&, const float&, float&,
                             FloatComplex*, float*, octave_idx_type&
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (cgelsy, CGELSY) (const octave_idx_type&, const octave_idx_type&,
                             const octave_idx_type&, FloatComplex*,
                             const octave_idx_type&, FloatComplex*,
                             const octave_idx_type&, octave_idx_type*,
                             float&, octave_idx_type&, FloatComplex*,
                             const octave_idx_type&, float*, octave_idx_type&);

  F77_RET_T
  F77_FUNC (cgelsd, CGELSD) (const octave_idx_type&, const octave_idx_type&,
                             const octave_idx_type&, FloatComplex*,
                             const octave_idx_type&, FloatComplex*,
                             const octave_idx_type&, float*, float&,
                             octave_idx_type&, FloatComplex*,
                             const octave_idx_type&, float*,
                             octave_idx_type*, octave_idx_type&);

  F77_RET_T
  F77_FUNC (cpotrf, CPOTRF) (F77_CONST_CHAR_ARG_DECL,
                             const octave_idx_type&, FloatComplex*,
                             const octave_idx_type&, octave_idx_type&
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (cpocon, CPOCON) (F77_CONST_CHAR_ARG_DECL,
                             const octave_idx_type&, FloatComplex*,
                             const octave_idx_type&, const float&, float&,
                             FloatComplex*, float*, octave_idx_type&
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (cpotrs, CPOTRS) (F77_CONST_CHAR_ARG_DECL,
                             const octave_idx_type&,
                             const octave_idx_type&, const FloatComplex*,
                             const octave_idx_type&, FloatComplex*,
                             const octave_idx_type&, octave_idx_type&
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (ctrtri, CTRTRI) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             const octave_idx_type&, const FloatComplex*,
                             const octave_idx_type&, octave_idx_type&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (ctrcon, CTRCON) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             const octave_idx_type&, const FloatComplex*,
                             const octave_idx_type&, float&, FloatComplex*,
                             float*, octave_idx_type&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (ctrtrs, CTRTRS) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             const octave_idx_type&, const octave_idx_type&,
                             const FloatComplex*, const octave_idx_type&,
                             FloatComplex*, const octave_idx_type&,
                             octave_idx_type&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (clartg, CLARTG) (const FloatComplex&, const FloatComplex&,
                             float&, FloatComplex&, FloatComplex&);

  F77_RET_T
  F77_FUNC (ctrsyl, CTRSYL) (F77_CONST_CHAR_ARG_DECL,
                             F77_CONST_CHAR_ARG_DECL,
                             const octave_idx_type&, const octave_idx_type&,
                             const octave_idx_type&, const FloatComplex*,
                             const octave_idx_type&, const FloatComplex*,
                             const octave_idx_type&, const FloatComplex*,
                             const octave_idx_type&, float&, octave_idx_type&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (xclange, XCLANGE) (F77_CONST_CHAR_ARG_DECL,
                               const octave_idx_type&, const octave_idx_type&,
                               const FloatComplex*, const octave_idx_type&,
                               float*, float&
                               F77_CHAR_ARG_LEN_DECL);
}

static const FloatComplex FloatComplex_NaN_result (octave_Float_NaN,
                                                   octave_Float_NaN);

// FloatComplex Matrix class

FloatComplexMatrix::FloatComplexMatrix (const FloatMatrix& a)
  : FloatComplexNDArray (a)
{
}

FloatComplexMatrix::FloatComplexMatrix (const FloatRowVector& rv)
  : FloatComplexNDArray (rv)
{
}

FloatComplexMatrix::FloatComplexMatrix (const FloatColumnVector& cv)
  : FloatComplexNDArray (cv)
{
}

FloatComplexMatrix::FloatComplexMatrix (const FloatDiagMatrix& a)
  : FloatComplexNDArray (a.dims (), 0.0)
{
  for (octave_idx_type i = 0; i < a.length (); i++)
    elem (i, i) = a.elem (i, i);
}

FloatComplexMatrix::FloatComplexMatrix (const MDiagArray2<float>& a)
  : FloatComplexNDArray (a.dims (), 0.0)
{
  for (octave_idx_type i = 0; i < a.length (); i++)
    elem (i, i) = a.elem (i, i);
}

FloatComplexMatrix::FloatComplexMatrix (const DiagArray2<float>& a)
  : FloatComplexNDArray (a.dims (), 0.0)
{
  for (octave_idx_type i = 0; i < a.length (); i++)
    elem (i, i) = a.elem (i, i);
}

FloatComplexMatrix::FloatComplexMatrix (const FloatComplexRowVector& rv)
  : FloatComplexNDArray (rv)
{
}

FloatComplexMatrix::FloatComplexMatrix (const FloatComplexColumnVector& cv)
  : FloatComplexNDArray (cv)
{
}

FloatComplexMatrix::FloatComplexMatrix (const FloatComplexDiagMatrix& a)
  : FloatComplexNDArray (a.dims (), 0.0)
{
  for (octave_idx_type i = 0; i < a.length (); i++)
    elem (i, i) = a.elem (i, i);
}

FloatComplexMatrix::FloatComplexMatrix (const MDiagArray2<FloatComplex>& a)
  : FloatComplexNDArray (a.dims (), 0.0)
{
  for (octave_idx_type i = 0; i < a.length (); i++)
    elem (i, i) = a.elem (i, i);
}

FloatComplexMatrix::FloatComplexMatrix (const DiagArray2<FloatComplex>& a)
  : FloatComplexNDArray (a.dims (), 0.0)
{
  for (octave_idx_type i = 0; i < a.length (); i++)
    elem (i, i) = a.elem (i, i);
}

// FIXME: could we use a templated mixed-type copy function
// here?

FloatComplexMatrix::FloatComplexMatrix (const boolMatrix& a)
  : FloatComplexNDArray (a)
{
}

FloatComplexMatrix::FloatComplexMatrix (const charMatrix& a)
  : FloatComplexNDArray (a.dims (), 0.0)
{
  for (octave_idx_type i = 0; i < a.rows (); i++)
    for (octave_idx_type j = 0; j < a.cols (); j++)
      elem (i, j) = static_cast<unsigned char> (a.elem (i, j));
}

FloatComplexMatrix::FloatComplexMatrix (const FloatMatrix& re,
                                        const FloatMatrix& im)
  : FloatComplexNDArray (re.dims ())
{
  if (im.rows () != rows () || im.cols () != cols ())
    (*current_liboctave_error_handler) ("complex: internal error");

  octave_idx_type nel = numel ();
  for (octave_idx_type i = 0; i < nel; i++)
    xelem (i) = FloatComplex (re(i), im(i));
}

bool
FloatComplexMatrix::operator == (const FloatComplexMatrix& a) const
{
  if (rows () != a.rows () || cols () != a.cols ())
    return false;

  return mx_inline_equal (length (), data (), a.data ());
}

bool
FloatComplexMatrix::operator != (const FloatComplexMatrix& a) const
{
  return !(*this == a);
}

bool
FloatComplexMatrix::is_hermitian (void) const
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  if (is_square () && nr > 0)
    {
      for (octave_idx_type i = 0; i < nr; i++)
        for (octave_idx_type j = i; j < nc; j++)
          if (elem (i, j) != conj (elem (j, i)))
            return false;

      return true;
    }

  return false;
}

// destructive insert/delete/reorder operations

FloatComplexMatrix&
FloatComplexMatrix::insert (const FloatMatrix& a,
                            octave_idx_type r, octave_idx_type c)
{
  octave_idx_type a_nr = a.rows ();
  octave_idx_type a_nc = a.cols ();

  if (r < 0 || r + a_nr > rows () || c < 0 || c + a_nc > cols ())
    {
      (*current_liboctave_error_handler) ("range error for insert");
      return *this;
    }

  if (a_nr >0 && a_nc > 0)
    {
      make_unique ();

      for (octave_idx_type j = 0; j < a_nc; j++)
        for (octave_idx_type i = 0; i < a_nr; i++)
          xelem (r+i, c+j) = a.elem (i, j);
    }

  return *this;
}

FloatComplexMatrix&
FloatComplexMatrix::insert (const FloatRowVector& a,
                            octave_idx_type r, octave_idx_type c)
{
  octave_idx_type a_len = a.length ();

  if (r < 0 || r >= rows () || c < 0 || c + a_len > cols ())
    {
      (*current_liboctave_error_handler) ("range error for insert");
      return *this;
    }

  if (a_len > 0)
    {
      make_unique ();

      for (octave_idx_type i = 0; i < a_len; i++)
        xelem (r, c+i) = a.elem (i);
    }

  return *this;
}

FloatComplexMatrix&
FloatComplexMatrix::insert (const FloatColumnVector& a,
                            octave_idx_type r, octave_idx_type c)
{
  octave_idx_type a_len = a.length ();

  if (r < 0 || r + a_len > rows () || c < 0 || c >= cols ())
    {
      (*current_liboctave_error_handler) ("range error for insert");
      return *this;
    }

  if (a_len > 0)
    {
      make_unique ();

      for (octave_idx_type i = 0; i < a_len; i++)
        xelem (r+i, c) = a.elem (i);
    }

  return *this;
}

FloatComplexMatrix&
FloatComplexMatrix::insert (const FloatDiagMatrix& a,
                            octave_idx_type r, octave_idx_type c)
{
  octave_idx_type a_nr = a.rows ();
  octave_idx_type a_nc = a.cols ();

  if (r < 0 || r + a_nr > rows () || c < 0 || c + a_nc > cols ())
    {
      (*current_liboctave_error_handler) ("range error for insert");
      return *this;
    }

  fill (0.0, r, c, r + a_nr - 1, c + a_nc - 1);

  octave_idx_type a_len = a.length ();

  if (a_len > 0)
    {
      make_unique ();

      for (octave_idx_type i = 0; i < a_len; i++)
        xelem (r+i, c+i) = a.elem (i, i);
    }

  return *this;
}

FloatComplexMatrix&
FloatComplexMatrix::insert (const FloatComplexMatrix& a,
                            octave_idx_type r, octave_idx_type c)
{
  Array<FloatComplex>::insert (a, r, c);
  return *this;
}

FloatComplexMatrix&
FloatComplexMatrix::insert (const FloatComplexRowVector& a,
                            octave_idx_type r, octave_idx_type c)
{
  octave_idx_type a_len = a.length ();
  if (r < 0 || r >= rows () || c < 0 || c + a_len > cols ())
    {
      (*current_liboctave_error_handler) ("range error for insert");
      return *this;
    }

  for (octave_idx_type i = 0; i < a_len; i++)
    elem (r, c+i) = a.elem (i);

  return *this;
}

FloatComplexMatrix&
FloatComplexMatrix::insert (const FloatComplexColumnVector& a,
                            octave_idx_type r, octave_idx_type c)
{
  octave_idx_type a_len = a.length ();

  if (r < 0 || r + a_len > rows () || c < 0 || c >= cols ())
    {
      (*current_liboctave_error_handler) ("range error for insert");
      return *this;
    }

  if (a_len > 0)
    {
      make_unique ();

      for (octave_idx_type i = 0; i < a_len; i++)
        xelem (r+i, c) = a.elem (i);
    }

  return *this;
}

FloatComplexMatrix&
FloatComplexMatrix::insert (const FloatComplexDiagMatrix& a,
                            octave_idx_type r, octave_idx_type c)
{
  octave_idx_type a_nr = a.rows ();
  octave_idx_type a_nc = a.cols ();

  if (r < 0 || r + a_nr > rows () || c < 0 || c + a_nc > cols ())
    {
      (*current_liboctave_error_handler) ("range error for insert");
      return *this;
    }

  fill (0.0, r, c, r + a_nr - 1, c + a_nc - 1);

  octave_idx_type a_len = a.length ();

  if (a_len > 0)
    {
      make_unique ();

      for (octave_idx_type i = 0; i < a_len; i++)
        xelem (r+i, c+i) = a.elem (i, i);
    }

  return *this;
}

FloatComplexMatrix&
FloatComplexMatrix::fill (float val)
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  if (nr > 0 && nc > 0)
    {
      make_unique ();

      for (octave_idx_type j = 0; j < nc; j++)
        for (octave_idx_type i = 0; i < nr; i++)
          xelem (i, j) = val;
    }

  return *this;
}

FloatComplexMatrix&
FloatComplexMatrix::fill (const FloatComplex& val)
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  if (nr > 0 && nc > 0)
    {
      make_unique ();

      for (octave_idx_type j = 0; j < nc; j++)
        for (octave_idx_type i = 0; i < nr; i++)
          xelem (i, j) = val;
    }

  return *this;
}

FloatComplexMatrix&
FloatComplexMatrix::fill (float val, octave_idx_type r1, octave_idx_type c1,
                          octave_idx_type r2, octave_idx_type c2)
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  if (r1 < 0 || r2 < 0 || c1 < 0 || c2 < 0
      || r1 >= nr || r2 >= nr || c1 >= nc || c2 >= nc)
    {
      (*current_liboctave_error_handler) ("range error for fill");
      return *this;
    }

  if (r1 > r2) { std::swap (r1, r2); }
  if (c1 > c2) { std::swap (c1, c2); }

  if (r2 >= r1 && c2 >= c1)
    {
      make_unique ();

      for (octave_idx_type j = c1; j <= c2; j++)
        for (octave_idx_type i = r1; i <= r2; i++)
          xelem (i, j) = val;
    }

  return *this;
}

FloatComplexMatrix&
FloatComplexMatrix::fill (const FloatComplex& val,
                          octave_idx_type r1, octave_idx_type c1,
                          octave_idx_type r2, octave_idx_type c2)
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  if (r1 < 0 || r2 < 0 || c1 < 0 || c2 < 0
      || r1 >= nr || r2 >= nr || c1 >= nc || c2 >= nc)
    {
      (*current_liboctave_error_handler) ("range error for fill");
      return *this;
    }

  if (r1 > r2) { std::swap (r1, r2); }
  if (c1 > c2) { std::swap (c1, c2); }

  if (r2 >= r1 && c2 >=c1)
    {
      make_unique ();

      for (octave_idx_type j = c1; j <= c2; j++)
        for (octave_idx_type i = r1; i <= r2; i++)
          xelem (i, j) = val;
    }

  return *this;
}

FloatComplexMatrix
FloatComplexMatrix::append (const FloatMatrix& a) const
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  if (nr != a.rows ())
    {
      (*current_liboctave_error_handler) ("row dimension mismatch for append");
      return *this;
    }

  octave_idx_type nc_insert = nc;
  FloatComplexMatrix retval (nr, nc + a.cols ());
  retval.insert (*this, 0, 0);
  retval.insert (a, 0, nc_insert);
  return retval;
}

FloatComplexMatrix
FloatComplexMatrix::append (const FloatRowVector& a) const
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  if (nr != 1)
    {
      (*current_liboctave_error_handler) ("row dimension mismatch for append");
      return *this;
    }

  octave_idx_type nc_insert = nc;
  FloatComplexMatrix retval (nr, nc + a.length ());
  retval.insert (*this, 0, 0);
  retval.insert (a, 0, nc_insert);
  return retval;
}

FloatComplexMatrix
FloatComplexMatrix::append (const FloatColumnVector& a) const
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  if (nr != a.length ())
    {
      (*current_liboctave_error_handler) ("row dimension mismatch for append");
      return *this;
    }

  octave_idx_type nc_insert = nc;
  FloatComplexMatrix retval (nr, nc + 1);
  retval.insert (*this, 0, 0);
  retval.insert (a, 0, nc_insert);
  return retval;
}

FloatComplexMatrix
FloatComplexMatrix::append (const FloatDiagMatrix& a) const
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  if (nr != a.rows ())
    {
      (*current_liboctave_error_handler) ("row dimension mismatch for append");
      return *this;
    }

  octave_idx_type nc_insert = nc;
  FloatComplexMatrix retval (nr, nc + a.cols ());
  retval.insert (*this, 0, 0);
  retval.insert (a, 0, nc_insert);
  return retval;
}

FloatComplexMatrix
FloatComplexMatrix::append (const FloatComplexMatrix& a) const
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  if (nr != a.rows ())
    {
      (*current_liboctave_error_handler) ("row dimension mismatch for append");
      return *this;
    }

  octave_idx_type nc_insert = nc;
  FloatComplexMatrix retval (nr, nc + a.cols ());
  retval.insert (*this, 0, 0);
  retval.insert (a, 0, nc_insert);
  return retval;
}

FloatComplexMatrix
FloatComplexMatrix::append (const FloatComplexRowVector& a) const
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  if (nr != 1)
    {
      (*current_liboctave_error_handler) ("row dimension mismatch for append");
      return *this;
    }

  octave_idx_type nc_insert = nc;
  FloatComplexMatrix retval (nr, nc + a.length ());
  retval.insert (*this, 0, 0);
  retval.insert (a, 0, nc_insert);
  return retval;
}

FloatComplexMatrix
FloatComplexMatrix::append (const FloatComplexColumnVector& a) const
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  if (nr != a.length ())
    {
      (*current_liboctave_error_handler) ("row dimension mismatch for append");
      return *this;
    }

  octave_idx_type nc_insert = nc;
  FloatComplexMatrix retval (nr, nc + 1);
  retval.insert (*this, 0, 0);
  retval.insert (a, 0, nc_insert);
  return retval;
}

FloatComplexMatrix
FloatComplexMatrix::append (const FloatComplexDiagMatrix& a) const
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  if (nr != a.rows ())
    {
      (*current_liboctave_error_handler) ("row dimension mismatch for append");
      return *this;
    }

  octave_idx_type nc_insert = nc;
  FloatComplexMatrix retval (nr, nc + a.cols ());
  retval.insert (*this, 0, 0);
  retval.insert (a, 0, nc_insert);
  return retval;
}

FloatComplexMatrix
FloatComplexMatrix::stack (const FloatMatrix& a) const
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  if (nc != a.cols ())
    {
      (*current_liboctave_error_handler)
        ("column dimension mismatch for stack");
      return *this;
    }

  octave_idx_type nr_insert = nr;
  FloatComplexMatrix retval (nr + a.rows (), nc);
  retval.insert (*this, 0, 0);
  retval.insert (a, nr_insert, 0);
  return retval;
}

FloatComplexMatrix
FloatComplexMatrix::stack (const FloatRowVector& a) const
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  if (nc != a.length ())
    {
      (*current_liboctave_error_handler)
        ("column dimension mismatch for stack");
      return *this;
    }

  octave_idx_type nr_insert = nr;
  FloatComplexMatrix retval (nr + 1, nc);
  retval.insert (*this, 0, 0);
  retval.insert (a, nr_insert, 0);
  return retval;
}

FloatComplexMatrix
FloatComplexMatrix::stack (const FloatColumnVector& a) const
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  if (nc != 1)
    {
      (*current_liboctave_error_handler)
        ("column dimension mismatch for stack");
      return *this;
    }

  octave_idx_type nr_insert = nr;
  FloatComplexMatrix retval (nr + a.length (), nc);
  retval.insert (*this, 0, 0);
  retval.insert (a, nr_insert, 0);
  return retval;
}

FloatComplexMatrix
FloatComplexMatrix::stack (const FloatDiagMatrix& a) const
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  if (nc != a.cols ())
    {
      (*current_liboctave_error_handler)
        ("column dimension mismatch for stack");
      return *this;
    }

  octave_idx_type nr_insert = nr;
  FloatComplexMatrix retval (nr + a.rows (), nc);
  retval.insert (*this, 0, 0);
  retval.insert (a, nr_insert, 0);
  return retval;
}

FloatComplexMatrix
FloatComplexMatrix::stack (const FloatComplexMatrix& a) const
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  if (nc != a.cols ())
    {
      (*current_liboctave_error_handler)
        ("column dimension mismatch for stack");
      return *this;
    }

  octave_idx_type nr_insert = nr;
  FloatComplexMatrix retval (nr + a.rows (), nc);
  retval.insert (*this, 0, 0);
  retval.insert (a, nr_insert, 0);
  return retval;
}

FloatComplexMatrix
FloatComplexMatrix::stack (const FloatComplexRowVector& a) const
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  if (nc != a.length ())
    {
      (*current_liboctave_error_handler)
        ("column dimension mismatch for stack");
      return *this;
    }

  octave_idx_type nr_insert = nr;
  FloatComplexMatrix retval (nr + 1, nc);
  retval.insert (*this, 0, 0);
  retval.insert (a, nr_insert, 0);
  return retval;
}

FloatComplexMatrix
FloatComplexMatrix::stack (const FloatComplexColumnVector& a) const
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  if (nc != 1)
    {
      (*current_liboctave_error_handler)
        ("column dimension mismatch for stack");
      return *this;
    }

  octave_idx_type nr_insert = nr;
  FloatComplexMatrix retval (nr + a.length (), nc);
  retval.insert (*this, 0, 0);
  retval.insert (a, nr_insert, 0);
  return retval;
}

FloatComplexMatrix
FloatComplexMatrix::stack (const FloatComplexDiagMatrix& a) const
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();
  if (nc != a.cols ())
    {
      (*current_liboctave_error_handler)
        ("column dimension mismatch for stack");
      return *this;
    }

  octave_idx_type nr_insert = nr;
  FloatComplexMatrix retval (nr + a.rows (), nc);
  retval.insert (*this, 0, 0);
  retval.insert (a, nr_insert, 0);
  return retval;
}

FloatComplexMatrix
conj (const FloatComplexMatrix& a)
{
  return do_mx_unary_map<FloatComplex, FloatComplex, std::conj<float> > (a);
}

// resize is the destructive equivalent for this one

FloatComplexMatrix
FloatComplexMatrix::extract (octave_idx_type r1, octave_idx_type c1,
                             octave_idx_type r2, octave_idx_type c2) const
{
  if (r1 > r2) { std::swap (r1, r2); }
  if (c1 > c2) { std::swap (c1, c2); }

  return index (idx_vector (r1, r2+1), idx_vector (c1, c2+1));
}

FloatComplexMatrix
FloatComplexMatrix::extract_n (octave_idx_type r1, octave_idx_type c1,
                               octave_idx_type nr, octave_idx_type nc) const
{
  return index (idx_vector (r1, r1 + nr), idx_vector (c1, c1 + nc));
}

// extract row or column i.

FloatComplexRowVector
FloatComplexMatrix::row (octave_idx_type i) const
{
  return index (idx_vector (i), idx_vector::colon);
}

FloatComplexColumnVector
FloatComplexMatrix::column (octave_idx_type i) const
{
  return index (idx_vector::colon, idx_vector (i));
}

FloatComplexMatrix
FloatComplexMatrix::inverse (void) const
{
  octave_idx_type info;
  float rcon;
  MatrixType mattype (*this);
  return inverse (mattype, info, rcon, 0, 0);
}

FloatComplexMatrix
FloatComplexMatrix::inverse (octave_idx_type& info) const
{
  float rcon;
  MatrixType mattype (*this);
  return inverse (mattype, info, rcon, 0, 0);
}

FloatComplexMatrix
FloatComplexMatrix::inverse (octave_idx_type& info, float& rcon, int force,
                             int calc_cond) const
{
  MatrixType mattype (*this);
  return inverse (mattype, info, rcon, force, calc_cond);
}

FloatComplexMatrix
FloatComplexMatrix::inverse (MatrixType &mattype) const
{
  octave_idx_type info;
  float rcon;
  return inverse (mattype, info, rcon, 0, 0);
}

FloatComplexMatrix
FloatComplexMatrix::inverse (MatrixType &mattype, octave_idx_type& info) const
{
  float rcon;
  return inverse (mattype, info, rcon, 0, 0);
}

FloatComplexMatrix
FloatComplexMatrix::tinverse (MatrixType &mattype, octave_idx_type& info,
                              float& rcon, int force, int calc_cond) const
{
  FloatComplexMatrix retval;

  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  if (nr != nc || nr == 0 || nc == 0)
    (*current_liboctave_error_handler) ("inverse requires square matrix");
  else
    {
      int typ = mattype.type ();
      char uplo = (typ == MatrixType::Lower ? 'L' : 'U');
      char udiag = 'N';
      retval = *this;
      FloatComplex *tmp_data = retval.fortran_vec ();

      F77_XFCN (ctrtri, CTRTRI, (F77_CONST_CHAR_ARG2 (&uplo, 1),
                                 F77_CONST_CHAR_ARG2 (&udiag, 1),
                                 nr, tmp_data, nr, info
                                 F77_CHAR_ARG_LEN (1)
                                 F77_CHAR_ARG_LEN (1)));

      // Throw-away extra info LAPACK gives so as to not change output.
      rcon = 0.0;
      if (info != 0)
        info = -1;
      else if (calc_cond)
        {
          octave_idx_type ztrcon_info = 0;
          char job = '1';

          OCTAVE_LOCAL_BUFFER (FloatComplex, cwork, 2*nr);
          OCTAVE_LOCAL_BUFFER (float, rwork, nr);

          F77_XFCN (ctrcon, CTRCON, (F77_CONST_CHAR_ARG2 (&job, 1),
                                     F77_CONST_CHAR_ARG2 (&uplo, 1),
                                     F77_CONST_CHAR_ARG2 (&udiag, 1),
                                     nr, tmp_data, nr, rcon,
                                     cwork, rwork, ztrcon_info
                                     F77_CHAR_ARG_LEN (1)
                                     F77_CHAR_ARG_LEN (1)
                                     F77_CHAR_ARG_LEN (1)));

          if (ztrcon_info != 0)
            info = -1;
        }

      if (info == -1 && ! force)
        retval = *this; // Restore matrix contents.
    }

  return retval;
}

FloatComplexMatrix
FloatComplexMatrix::finverse (MatrixType &mattype, octave_idx_type& info,
                              float& rcon, int force, int calc_cond) const
{
  FloatComplexMatrix retval;

  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  if (nr != nc)
    (*current_liboctave_error_handler) ("inverse requires square matrix");
  else
    {
      Array<octave_idx_type> ipvt (dim_vector (nr, 1));
      octave_idx_type *pipvt = ipvt.fortran_vec ();

      retval = *this;
      FloatComplex *tmp_data = retval.fortran_vec ();

      Array<FloatComplex> z (dim_vector (1, 1));
      octave_idx_type lwork = -1;

      // Query the optimum work array size.

      F77_XFCN (cgetri, CGETRI, (nc, tmp_data, nr, pipvt,
                                 z.fortran_vec (), lwork, info));

      lwork = static_cast<octave_idx_type> (std::real (z(0)));
      lwork = (lwork <  2 *nc ? 2*nc : lwork);
      z.resize (dim_vector (lwork, 1));
      FloatComplex *pz = z.fortran_vec ();

      info = 0;

      // Calculate the norm of the matrix, for later use.
      float anorm;
      if (calc_cond)
        anorm = retval.abs ().sum ().row (static_cast<octave_idx_type>(0))
                .max ();

      F77_XFCN (cgetrf, CGETRF, (nc, nc, tmp_data, nr, pipvt, info));

      // Throw-away extra info LAPACK gives so as to not change output.
      rcon = 0.0;
      if (info != 0)
        info = -1;
      else if (calc_cond)
        {
          // Now calculate the condition number for non-singular matrix.
          octave_idx_type zgecon_info = 0;
          char job = '1';
          Array<float> rz (dim_vector (2 * nc, 1));
          float *prz = rz.fortran_vec ();
          F77_XFCN (cgecon, CGECON, (F77_CONST_CHAR_ARG2 (&job, 1),
                                     nc, tmp_data, nr, anorm,
                                     rcon, pz, prz, zgecon_info
                                     F77_CHAR_ARG_LEN (1)));

          if (zgecon_info != 0)
            info = -1;
        }

      if (info == -1 && ! force)
        retval = *this;  // Restore contents.
      else
        {
          octave_idx_type zgetri_info = 0;

          F77_XFCN (cgetri, CGETRI, (nc, tmp_data, nr, pipvt,
                                     pz, lwork, zgetri_info));

          if (zgetri_info != 0)
            info = -1;
        }

      if (info != 0)
        mattype.mark_as_rectangular ();
    }

  return retval;
}

FloatComplexMatrix
FloatComplexMatrix::inverse (MatrixType &mattype, octave_idx_type& info,
                             float& rcon, int force, int calc_cond) const
{
  int typ = mattype.type (false);
  FloatComplexMatrix ret;

  if (typ == MatrixType::Unknown)
    typ = mattype.type (*this);

  if (typ == MatrixType::Upper || typ == MatrixType::Lower)
    ret = tinverse (mattype, info, rcon, force, calc_cond);
  else
    {
      if (mattype.is_hermitian ())
        {
          FloatComplexCHOL chol (*this, info, calc_cond);
          if (info == 0)
            {
              if (calc_cond)
                rcon = chol.rcond ();
              else
                rcon = 1.0;
              ret = chol.inverse ();
            }
          else
            mattype.mark_as_unsymmetric ();
        }

      if (!mattype.is_hermitian ())
        ret = finverse (mattype, info, rcon, force, calc_cond);

      if ((mattype.is_hermitian () || calc_cond) && rcon == 0.)
        ret = FloatComplexMatrix (rows (), columns (),
                                  FloatComplex (octave_Float_Inf, 0.));
    }

  return ret;
}

FloatComplexMatrix
FloatComplexMatrix::pseudo_inverse (float tol) const
{
  FloatComplexMatrix retval;

  FloatComplexSVD result (*this, SVD::economy);

  FloatDiagMatrix S = result.singular_values ();
  FloatComplexMatrix U = result.left_singular_matrix ();
  FloatComplexMatrix V = result.right_singular_matrix ();

  FloatColumnVector sigma = S.extract_diag ();

  octave_idx_type r = sigma.length () - 1;
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  if (tol <= 0.0)
    {
      if (nr > nc)
        tol = nr * sigma.elem (0) * std::numeric_limits<double>::epsilon ();
      else
        tol = nc * sigma.elem (0) * std::numeric_limits<double>::epsilon ();
    }

  while (r >= 0 && sigma.elem (r) < tol)
    r--;

  if (r < 0)
    retval = FloatComplexMatrix (nc, nr, 0.0);
  else
    {
      FloatComplexMatrix Ur = U.extract (0, 0, nr-1, r);
      FloatDiagMatrix D = FloatDiagMatrix (sigma.extract (0, r)) . inverse ();
      FloatComplexMatrix Vr = V.extract (0, 0, nc-1, r);
      retval = Vr * D * Ur.hermitian ();
    }

  return retval;
}

#if defined (HAVE_FFTW)

FloatComplexMatrix
FloatComplexMatrix::fourier (void) const
{
  size_t nr = rows ();
  size_t nc = cols ();

  FloatComplexMatrix retval (nr, nc);

  size_t npts, nsamples;

  if (nr == 1 || nc == 1)
    {
      npts = nr > nc ? nr : nc;
      nsamples = 1;
    }
  else
    {
      npts = nr;
      nsamples = nc;
    }

  const FloatComplex *in (data ());
  FloatComplex *out (retval.fortran_vec ());

  octave_fftw::fft (in, out, npts, nsamples);

  return retval;
}

FloatComplexMatrix
FloatComplexMatrix::ifourier (void) const
{
  size_t nr = rows ();
  size_t nc = cols ();

  FloatComplexMatrix retval (nr, nc);

  size_t npts, nsamples;

  if (nr == 1 || nc == 1)
    {
      npts = nr > nc ? nr : nc;
      nsamples = 1;
    }
  else
    {
      npts = nr;
      nsamples = nc;
    }

  const FloatComplex *in (data ());
  FloatComplex *out (retval.fortran_vec ());

  octave_fftw::ifft (in, out, npts, nsamples);

  return retval;
}

FloatComplexMatrix
FloatComplexMatrix::fourier2d (void) const
{
  dim_vector dv(rows (), cols ());

  FloatComplexMatrix retval (rows (), cols ());
  const FloatComplex *in (data ());
  FloatComplex *out (retval.fortran_vec ());

  octave_fftw::fftNd (in, out, 2, dv);

  return retval;
}

FloatComplexMatrix
FloatComplexMatrix::ifourier2d (void) const
{
  dim_vector dv(rows (), cols ());

  FloatComplexMatrix retval (rows (), cols ());
  const FloatComplex *in (data ());
  FloatComplex *out (retval.fortran_vec ());

  octave_fftw::ifftNd (in, out, 2, dv);

  return retval;
}

#else

extern "C"
{
  F77_RET_T
  F77_FUNC (cffti, CFFTI) (const octave_idx_type&, FloatComplex*);

  F77_RET_T
  F77_FUNC (cfftf, CFFTF) (const octave_idx_type&, FloatComplex*,
                           FloatComplex*);

  F77_RET_T
  F77_FUNC (cfftb, CFFTB) (const octave_idx_type&, FloatComplex*,
                           FloatComplex*);
}

FloatComplexMatrix
FloatComplexMatrix::fourier (void) const
{
  FloatComplexMatrix retval;

  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  octave_idx_type npts, nsamples;

  if (nr == 1 || nc == 1)
    {
      npts = nr > nc ? nr : nc;
      nsamples = 1;
    }
  else
    {
      npts = nr;
      nsamples = nc;
    }

  octave_idx_type nn = 4*npts+15;

  Array<FloatComplex> wsave (dim_vector (nn, 1));
  FloatComplex *pwsave = wsave.fortran_vec ();

  retval = *this;
  FloatComplex *tmp_data = retval.fortran_vec ();

  F77_FUNC (cffti, CFFTI) (npts, pwsave);

  for (octave_idx_type j = 0; j < nsamples; j++)
    {
      octave_quit ();

      F77_FUNC (cfftf, CFFTF) (npts, &tmp_data[npts*j], pwsave);
    }

  return retval;
}

FloatComplexMatrix
FloatComplexMatrix::ifourier (void) const
{
  FloatComplexMatrix retval;

  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  octave_idx_type npts, nsamples;

  if (nr == 1 || nc == 1)
    {
      npts = nr > nc ? nr : nc;
      nsamples = 1;
    }
  else
    {
      npts = nr;
      nsamples = nc;
    }

  octave_idx_type nn = 4*npts+15;

  Array<FloatComplex> wsave (dim_vector (nn, 1));
  FloatComplex *pwsave = wsave.fortran_vec ();

  retval = *this;
  FloatComplex *tmp_data = retval.fortran_vec ();

  F77_FUNC (cffti, CFFTI) (npts, pwsave);

  for (octave_idx_type j = 0; j < nsamples; j++)
    {
      octave_quit ();

      F77_FUNC (cfftb, CFFTB) (npts, &tmp_data[npts*j], pwsave);
    }

  for (octave_idx_type j = 0; j < npts*nsamples; j++)
    tmp_data[j] = tmp_data[j] / static_cast<float> (npts);

  return retval;
}

FloatComplexMatrix
FloatComplexMatrix::fourier2d (void) const
{
  FloatComplexMatrix retval;

  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  octave_idx_type npts, nsamples;

  if (nr == 1 || nc == 1)
    {
      npts = nr > nc ? nr : nc;
      nsamples = 1;
    }
  else
    {
      npts = nr;
      nsamples = nc;
    }

  octave_idx_type nn = 4*npts+15;

  Array<FloatComplex> wsave (dim_vector (nn, 1));
  FloatComplex *pwsave = wsave.fortran_vec ();

  retval = *this;
  FloatComplex *tmp_data = retval.fortran_vec ();

  F77_FUNC (cffti, CFFTI) (npts, pwsave);

  for (octave_idx_type j = 0; j < nsamples; j++)
    {
      octave_quit ();

      F77_FUNC (cfftf, CFFTF) (npts, &tmp_data[npts*j], pwsave);
    }

  npts = nc;
  nsamples = nr;
  nn = 4*npts+15;

  wsave.resize (dim_vector (nn, 1));
  pwsave = wsave.fortran_vec ();

  Array<FloatComplex> tmp (dim_vector (npts, 1));
  FloatComplex *prow = tmp.fortran_vec ();

  F77_FUNC (cffti, CFFTI) (npts, pwsave);

  for (octave_idx_type j = 0; j < nsamples; j++)
    {
      octave_quit ();

      for (octave_idx_type i = 0; i < npts; i++)
        prow[i] = tmp_data[i*nr + j];

      F77_FUNC (cfftf, CFFTF) (npts, prow, pwsave);

      for (octave_idx_type i = 0; i < npts; i++)
        tmp_data[i*nr + j] = prow[i];
    }

  return retval;
}

FloatComplexMatrix
FloatComplexMatrix::ifourier2d (void) const
{
  FloatComplexMatrix retval;

  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  octave_idx_type npts, nsamples;

  if (nr == 1 || nc == 1)
    {
      npts = nr > nc ? nr : nc;
      nsamples = 1;
    }
  else
    {
      npts = nr;
      nsamples = nc;
    }

  octave_idx_type nn = 4*npts+15;

  Array<FloatComplex> wsave (dim_vector (nn, 1));
  FloatComplex *pwsave = wsave.fortran_vec ();

  retval = *this;
  FloatComplex *tmp_data = retval.fortran_vec ();

  F77_FUNC (cffti, CFFTI) (npts, pwsave);

  for (octave_idx_type j = 0; j < nsamples; j++)
    {
      octave_quit ();

      F77_FUNC (cfftb, CFFTB) (npts, &tmp_data[npts*j], pwsave);
    }

  for (octave_idx_type j = 0; j < npts*nsamples; j++)
    tmp_data[j] = tmp_data[j] / static_cast<float> (npts);

  npts = nc;
  nsamples = nr;
  nn = 4*npts+15;

  wsave.resize (dim_vector (nn, 1));
  pwsave = wsave.fortran_vec ();

  Array<FloatComplex> tmp (dim_vector (npts, 1));
  FloatComplex *prow = tmp.fortran_vec ();

  F77_FUNC (cffti, CFFTI) (npts, pwsave);

  for (octave_idx_type j = 0; j < nsamples; j++)
    {
      octave_quit ();

      for (octave_idx_type i = 0; i < npts; i++)
        prow[i] = tmp_data[i*nr + j];

      F77_FUNC (cfftb, CFFTB) (npts, prow, pwsave);

      for (octave_idx_type i = 0; i < npts; i++)
        tmp_data[i*nr + j] = prow[i] / static_cast<float> (npts);
    }

  return retval;
}

#endif

FloatComplexDET
FloatComplexMatrix::determinant (void) const
{
  octave_idx_type info;
  float rcon;
  return determinant (info, rcon, 0);
}

FloatComplexDET
FloatComplexMatrix::determinant (octave_idx_type& info) const
{
  float rcon;
  return determinant (info, rcon, 0);
}

FloatComplexDET
FloatComplexMatrix::determinant (octave_idx_type& info, float& rcon,
                                 int calc_cond) const
{
  MatrixType mattype (*this);
  return determinant (mattype, info, rcon, calc_cond);
}

FloatComplexDET
FloatComplexMatrix::determinant (MatrixType& mattype,
                                 octave_idx_type& info, float& rcon,
                                 int calc_cond) const
{
  FloatComplexDET retval (1.0);

  info = 0;
  rcon = 0.0;

  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  if (nr != nc)
    (*current_liboctave_error_handler) ("matrix must be square");
  else
    {
      volatile int typ = mattype.type ();

      // Even though the matrix is marked as singular (Rectangular), we may
      // still get a useful number from the LU factorization, because it always
      // completes.

      if (typ == MatrixType::Unknown)
        typ = mattype.type (*this);
      else if (typ == MatrixType::Rectangular)
        typ = MatrixType::Full;

      if (typ == MatrixType::Lower || typ == MatrixType::Upper)
        {
          for (octave_idx_type i = 0; i < nc; i++)
            retval *= elem (i,i);
        }
      else if (typ == MatrixType::Hermitian)
        {
          FloatComplexMatrix atmp = *this;
          FloatComplex *tmp_data = atmp.fortran_vec ();

          float anorm = 0;
          if (calc_cond) anorm = xnorm (*this, 1);


          char job = 'L';
          F77_XFCN (cpotrf, CPOTRF, (F77_CONST_CHAR_ARG2 (&job, 1), nr,
                                     tmp_data, nr, info
                                     F77_CHAR_ARG_LEN (1)));

          if (info != 0)
            {
              rcon = 0.0;
              mattype.mark_as_unsymmetric ();
              typ = MatrixType::Full;
            }
          else
            {
              Array<FloatComplex> z (dim_vector (2 * nc, 1));
              FloatComplex *pz = z.fortran_vec ();
              Array<float> rz (dim_vector (nc, 1));
              float *prz = rz.fortran_vec ();

              F77_XFCN (cpocon, CPOCON, (F77_CONST_CHAR_ARG2 (&job, 1),
                                         nr, tmp_data, nr, anorm,
                                         rcon, pz, prz, info
                                         F77_CHAR_ARG_LEN (1)));

              if (info != 0)
                rcon = 0.0;

              for (octave_idx_type i = 0; i < nc; i++)
                retval *= atmp (i,i);

              retval = retval.square ();
            }
        }
      else if (typ != MatrixType::Full)
        (*current_liboctave_error_handler) ("det: invalid dense matrix type");

      if (typ == MatrixType::Full)
        {
          Array<octave_idx_type> ipvt (dim_vector (nr, 1));
          octave_idx_type *pipvt = ipvt.fortran_vec ();

          FloatComplexMatrix atmp = *this;
          FloatComplex *tmp_data = atmp.fortran_vec ();

          info = 0;

          // Calculate the norm of the matrix, for later use.
          float anorm = 0;
          if (calc_cond) anorm = xnorm (*this, 1);

          F77_XFCN (cgetrf, CGETRF, (nr, nr, tmp_data, nr, pipvt, info));

          // Throw-away extra info LAPACK gives so as to not change output.
          rcon = 0.0;
          if (info != 0)
            {
              info = -1;
              retval = FloatComplexDET ();
            }
          else
            {
              if (calc_cond)
                {
                  // Now calc the condition number for non-singular matrix.
                  char job = '1';
                  Array<FloatComplex> z (dim_vector (2 * nc, 1));
                  FloatComplex *pz = z.fortran_vec ();
                  Array<float> rz (dim_vector (2 * nc, 1));
                  float *prz = rz.fortran_vec ();

                  F77_XFCN (cgecon, CGECON, (F77_CONST_CHAR_ARG2 (&job, 1),
                                             nc, tmp_data, nr, anorm,
                                             rcon, pz, prz, info
                                             F77_CHAR_ARG_LEN (1)));
                }

              if (info != 0)
                {
                  info = -1;
                  retval = FloatComplexDET ();
                }
              else
                {
                  for (octave_idx_type i = 0; i < nc; i++)
                    {
                      FloatComplex c = atmp(i,i);
                      retval *= (ipvt(i) != (i+1)) ? -c : c;
                    }
                }
            }
        }
    }

  return retval;
}

float
FloatComplexMatrix::rcond (void) const
{
  MatrixType mattype (*this);
  return rcond (mattype);
}

float
FloatComplexMatrix::rcond (MatrixType &mattype) const
{
  float rcon = octave_NaN;
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  if (nr != nc)
    (*current_liboctave_error_handler) ("matrix must be square");
  else if (nr == 0 || nc == 0)
    rcon = octave_Inf;
  else
    {
      volatile int typ = mattype.type ();

      if (typ == MatrixType::Unknown)
        typ = mattype.type (*this);

      // Only calculate the condition number for LU/Cholesky
      if (typ == MatrixType::Upper)
        {
          const FloatComplex *tmp_data = fortran_vec ();
          octave_idx_type info = 0;
          char norm = '1';
          char uplo = 'U';
          char dia = 'N';

          Array<FloatComplex> z (dim_vector (2 * nc, 1));
          FloatComplex *pz = z.fortran_vec ();
          Array<float> rz (dim_vector (nc, 1));
          float *prz = rz.fortran_vec ();

          F77_XFCN (ctrcon, CTRCON, (F77_CONST_CHAR_ARG2 (&norm, 1),
                                     F77_CONST_CHAR_ARG2 (&uplo, 1),
                                     F77_CONST_CHAR_ARG2 (&dia, 1),
                                     nr, tmp_data, nr, rcon,
                                     pz, prz, info
                                     F77_CHAR_ARG_LEN (1)
                                     F77_CHAR_ARG_LEN (1)
                                     F77_CHAR_ARG_LEN (1)));

          if (info != 0)
            rcon = 0;
        }
      else if  (typ == MatrixType::Permuted_Upper)
        (*current_liboctave_error_handler)
          ("permuted triangular matrix not implemented");
      else if (typ == MatrixType::Lower)
        {
          const FloatComplex *tmp_data = fortran_vec ();
          octave_idx_type info = 0;
          char norm = '1';
          char uplo = 'L';
          char dia = 'N';

          Array<FloatComplex> z (dim_vector (2 * nc, 1));
          FloatComplex *pz = z.fortran_vec ();
          Array<float> rz (dim_vector (nc, 1));
          float *prz = rz.fortran_vec ();

          F77_XFCN (ctrcon, CTRCON, (F77_CONST_CHAR_ARG2 (&norm, 1),
                                     F77_CONST_CHAR_ARG2 (&uplo, 1),
                                     F77_CONST_CHAR_ARG2 (&dia, 1),
                                     nr, tmp_data, nr, rcon,
                                     pz, prz, info
                                     F77_CHAR_ARG_LEN (1)
                                     F77_CHAR_ARG_LEN (1)
                                     F77_CHAR_ARG_LEN (1)));

          if (info != 0)
            rcon = 0.0;
        }
      else if (typ == MatrixType::Permuted_Lower)
        (*current_liboctave_error_handler)
          ("permuted triangular matrix not implemented");
      else if (typ == MatrixType::Full || typ == MatrixType::Hermitian)
        {
          float anorm = -1.0;

          if (typ == MatrixType::Hermitian)
            {
              octave_idx_type info = 0;
              char job = 'L';

              FloatComplexMatrix atmp = *this;
              FloatComplex *tmp_data = atmp.fortran_vec ();

              anorm = atmp.abs().sum().
                      row(static_cast<octave_idx_type>(0)).max();

              F77_XFCN (cpotrf, CPOTRF, (F77_CONST_CHAR_ARG2 (&job, 1), nr,
                                         tmp_data, nr, info
                                         F77_CHAR_ARG_LEN (1)));

              if (info != 0)
                {
                  rcon = 0.0;

                  mattype.mark_as_unsymmetric ();
                  typ = MatrixType::Full;
                }
              else
                {
                  Array<FloatComplex> z (dim_vector (2 * nc, 1));
                  FloatComplex *pz = z.fortran_vec ();
                  Array<float> rz (dim_vector (nc, 1));
                  float *prz = rz.fortran_vec ();

                  F77_XFCN (cpocon, CPOCON, (F77_CONST_CHAR_ARG2 (&job, 1),
                                             nr, tmp_data, nr, anorm,
                                             rcon, pz, prz, info
                                             F77_CHAR_ARG_LEN (1)));

                  if (info != 0)
                    rcon = 0.0;
                }
            }


          if (typ == MatrixType::Full)
            {
              octave_idx_type info = 0;

              FloatComplexMatrix atmp = *this;
              FloatComplex *tmp_data = atmp.fortran_vec ();

              Array<octave_idx_type> ipvt (dim_vector (nr, 1));
              octave_idx_type *pipvt = ipvt.fortran_vec ();

              if (anorm < 0.)
                anorm = atmp.abs ().sum ().
                        row(static_cast<octave_idx_type>(0)).max ();

              Array<FloatComplex> z (dim_vector (2 * nc, 1));
              FloatComplex *pz = z.fortran_vec ();
              Array<float> rz (dim_vector (2 * nc, 1));
              float *prz = rz.fortran_vec ();

              F77_XFCN (cgetrf, CGETRF, (nr, nr, tmp_data, nr, pipvt, info));

              if (info != 0)
                {
                  rcon = 0.0;
                  mattype.mark_as_rectangular ();
                }
              else
                {
                  char job = '1';
                  F77_XFCN (cgecon, CGECON, (F77_CONST_CHAR_ARG2 (&job, 1),
                                             nc, tmp_data, nr, anorm,
                                             rcon, pz, prz, info
                                             F77_CHAR_ARG_LEN (1)));

                  if (info != 0)
                    rcon = 0.0;
                }
            }
        }
      else
        rcon = 0.0;
    }

  return rcon;
}

FloatComplexMatrix
FloatComplexMatrix::utsolve (MatrixType &mattype, const FloatComplexMatrix& b,
                             octave_idx_type& info, float& rcon,
                             solve_singularity_handler sing_handler,
                             bool calc_cond, blas_trans_type transt) const
{
  FloatComplexMatrix retval;

  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  if (nr != b.rows ())
    (*current_liboctave_error_handler)
      ("matrix dimension mismatch solution of linear equations");
  else if (nr == 0 || nc == 0 || b.cols () == 0)
    retval = FloatComplexMatrix (nc, b.cols (), FloatComplex (0.0, 0.0));
  else
    {
      volatile int typ = mattype.type ();

      if (typ == MatrixType::Permuted_Upper || typ == MatrixType::Upper)
        {
          octave_idx_type b_nc = b.cols ();
          rcon = 1.;
          info = 0;

          if (typ == MatrixType::Permuted_Upper)
            {
              (*current_liboctave_error_handler)
                ("permuted triangular matrix not implemented");
            }
          else
            {
              const FloatComplex *tmp_data = fortran_vec ();

              retval = b;
              FloatComplex *result = retval.fortran_vec ();

              char uplo = 'U';
              char trans = get_blas_char (transt);
              char dia = 'N';

              F77_XFCN (ctrtrs, CTRTRS, (F77_CONST_CHAR_ARG2 (&uplo, 1),
                                         F77_CONST_CHAR_ARG2 (&trans, 1),
                                         F77_CONST_CHAR_ARG2 (&dia, 1),
                                         nr, b_nc, tmp_data, nr,
                                         result, nr, info
                                         F77_CHAR_ARG_LEN (1)
                                         F77_CHAR_ARG_LEN (1)
                                         F77_CHAR_ARG_LEN (1)));

              if (calc_cond)
                {
                  char norm = '1';
                  uplo = 'U';
                  dia = 'N';

                  Array<FloatComplex> z (dim_vector (2 * nc, 1));
                  FloatComplex *pz = z.fortran_vec ();
                  Array<float> rz (dim_vector (nc, 1));
                  float *prz = rz.fortran_vec ();

                  F77_XFCN (ctrcon, CTRCON, (F77_CONST_CHAR_ARG2 (&norm, 1),
                                             F77_CONST_CHAR_ARG2 (&uplo, 1),
                                             F77_CONST_CHAR_ARG2 (&dia, 1),
                                             nr, tmp_data, nr, rcon,
                                             pz, prz, info
                                             F77_CHAR_ARG_LEN (1)
                                             F77_CHAR_ARG_LEN (1)
                                             F77_CHAR_ARG_LEN (1)));

                  if (info != 0)
                    info = -2;

                  volatile float rcond_plus_one = rcon + 1.0;

                  if (rcond_plus_one == 1.0 || xisnan (rcon))
                    {
                      info = -2;

                      if (sing_handler)
                        sing_handler (rcon);
                      else
                        gripe_singular_matrix (rcon);
                    }
                }
            }
        }
      else
        (*current_liboctave_error_handler) ("incorrect matrix type");
    }

  return retval;
}

FloatComplexMatrix
FloatComplexMatrix::ltsolve (MatrixType &mattype, const FloatComplexMatrix& b,
                             octave_idx_type& info, float& rcon,
                             solve_singularity_handler sing_handler,
                             bool calc_cond, blas_trans_type transt) const
{
  FloatComplexMatrix retval;

  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  if (nr != b.rows ())
    (*current_liboctave_error_handler)
      ("matrix dimension mismatch solution of linear equations");
  else if (nr == 0 || nc == 0 || b.cols () == 0)
    retval = FloatComplexMatrix (nc, b.cols (), FloatComplex (0.0, 0.0));
  else
    {
      volatile int typ = mattype.type ();

      if (typ == MatrixType::Permuted_Lower || typ == MatrixType::Lower)
        {
          octave_idx_type b_nc = b.cols ();
          rcon = 1.;
          info = 0;

          if (typ == MatrixType::Permuted_Lower)
            {
              (*current_liboctave_error_handler)
                ("permuted triangular matrix not implemented");
            }
          else
            {
              const FloatComplex *tmp_data = fortran_vec ();

              retval = b;
              FloatComplex *result = retval.fortran_vec ();

              char uplo = 'L';
              char trans = get_blas_char (transt);
              char dia = 'N';

              F77_XFCN (ctrtrs, CTRTRS, (F77_CONST_CHAR_ARG2 (&uplo, 1),
                                         F77_CONST_CHAR_ARG2 (&trans, 1),
                                         F77_CONST_CHAR_ARG2 (&dia, 1),
                                         nr, b_nc, tmp_data, nr,
                                         result, nr, info
                                         F77_CHAR_ARG_LEN (1)
                                         F77_CHAR_ARG_LEN (1)
                                         F77_CHAR_ARG_LEN (1)));

              if (calc_cond)
                {
                  char norm = '1';
                  uplo = 'L';
                  dia = 'N';

                  Array<FloatComplex> z (dim_vector (2 * nc, 1));
                  FloatComplex *pz = z.fortran_vec ();
                  Array<float> rz (dim_vector (nc, 1));
                  float *prz = rz.fortran_vec ();

                  F77_XFCN (ctrcon, CTRCON, (F77_CONST_CHAR_ARG2 (&norm, 1),
                                             F77_CONST_CHAR_ARG2 (&uplo, 1),
                                             F77_CONST_CHAR_ARG2 (&dia, 1),
                                             nr, tmp_data, nr, rcon,
                                             pz, prz, info
                                             F77_CHAR_ARG_LEN (1)
                                             F77_CHAR_ARG_LEN (1)
                                             F77_CHAR_ARG_LEN (1)));

                  if (info != 0)
                    info = -2;

                  volatile float rcond_plus_one = rcon + 1.0;

                  if (rcond_plus_one == 1.0 || xisnan (rcon))
                    {
                      info = -2;

                      if (sing_handler)
                        sing_handler (rcon);
                      else
                        gripe_singular_matrix (rcon);
                    }
                }
            }
        }
      else
        (*current_liboctave_error_handler) ("incorrect matrix type");
    }

  return retval;
}

FloatComplexMatrix
FloatComplexMatrix::fsolve (MatrixType &mattype, const FloatComplexMatrix& b,
                            octave_idx_type& info, float& rcon,
                            solve_singularity_handler sing_handler,
                            bool calc_cond) const
{
  FloatComplexMatrix retval;

  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();


  if (nr != nc || nr != b.rows ())
    (*current_liboctave_error_handler)
      ("matrix dimension mismatch solution of linear equations");
  else if (nr == 0 || b.cols () == 0)
    retval = FloatComplexMatrix (nc, b.cols (), FloatComplex (0.0, 0.0));
  else
    {
      volatile int typ = mattype.type ();

      // Calculate the norm of the matrix, for later use.
      float anorm = -1.;

      if (typ == MatrixType::Hermitian)
        {
          info = 0;
          char job = 'L';

          FloatComplexMatrix atmp = *this;
          FloatComplex *tmp_data = atmp.fortran_vec ();

          anorm = atmp.abs().sum().row(static_cast<octave_idx_type>(0)).max();

          F77_XFCN (cpotrf, CPOTRF, (F77_CONST_CHAR_ARG2 (&job, 1), nr,
                                     tmp_data, nr, info
                                     F77_CHAR_ARG_LEN (1)));

          // Throw-away extra info LAPACK gives so as to not change output.
          rcon = 0.0;
          if (info != 0)
            {
              info = -2;

              mattype.mark_as_unsymmetric ();
              typ = MatrixType::Full;
            }
          else
            {
              if (calc_cond)
                {
                  Array<FloatComplex> z (dim_vector (2 * nc, 1));
                  FloatComplex *pz = z.fortran_vec ();
                  Array<float> rz (dim_vector (nc, 1));
                  float *prz = rz.fortran_vec ();

                  F77_XFCN (cpocon, CPOCON, (F77_CONST_CHAR_ARG2 (&job, 1),
                                             nr, tmp_data, nr, anorm,
                                             rcon, pz, prz, info
                                             F77_CHAR_ARG_LEN (1)));

                  if (info != 0)
                    info = -2;

                  volatile float rcond_plus_one = rcon + 1.0;

                  if (rcond_plus_one == 1.0 || xisnan (rcon))
                    {
                      info = -2;

                      if (sing_handler)
                        sing_handler (rcon);
                      else
                        gripe_singular_matrix (rcon);
                    }
                }

              if (info == 0)
                {
                  retval = b;
                  FloatComplex *result = retval.fortran_vec ();

                  octave_idx_type b_nc = b.cols ();

                  F77_XFCN (cpotrs, CPOTRS, (F77_CONST_CHAR_ARG2 (&job, 1),
                                             nr, b_nc, tmp_data, nr,
                                             result, b.rows (), info
                                             F77_CHAR_ARG_LEN (1)));
                }
              else
                {
                  mattype.mark_as_unsymmetric ();
                  typ = MatrixType::Full;
                }
            }
        }

      if (typ == MatrixType::Full)
        {
          info = 0;

          Array<octave_idx_type> ipvt (dim_vector (nr, 1));
          octave_idx_type *pipvt = ipvt.fortran_vec ();

          FloatComplexMatrix atmp = *this;
          FloatComplex *tmp_data = atmp.fortran_vec ();

          Array<FloatComplex> z (dim_vector (2 * nc, 1));
          FloatComplex *pz = z.fortran_vec ();
          Array<float> rz (dim_vector (2 * nc, 1));
          float *prz = rz.fortran_vec ();

          // Calculate the norm of the matrix, for later use.
          if (anorm < 0.)
            anorm = atmp.abs ().sum ().row (static_cast<octave_idx_type>(0))
                    .max ();

          F77_XFCN (cgetrf, CGETRF, (nr, nr, tmp_data, nr, pipvt, info));

          // Throw-away extra info LAPACK gives so as to not change output.
          rcon = 0.0;
          if (info != 0)
            {
              info = -2;

              if (sing_handler)
                sing_handler (rcon);
              else
                gripe_singular_matrix ();

              mattype.mark_as_rectangular ();
            }
          else
            {
              if (calc_cond)
                {
                  // Now calculate the condition number for
                  // non-singular matrix.
                  char job = '1';
                  F77_XFCN (cgecon, CGECON, (F77_CONST_CHAR_ARG2 (&job, 1),
                                             nc, tmp_data, nr, anorm,
                                             rcon, pz, prz, info
                                             F77_CHAR_ARG_LEN (1)));

                  if (info != 0)
                    info = -2;

                  volatile float rcond_plus_one = rcon + 1.0;

                  if (rcond_plus_one == 1.0 || xisnan (rcon))
                    {
                      info = -2;

                      if (sing_handler)
                        sing_handler (rcon);
                      else
                        gripe_singular_matrix (rcon);
                    }
                }

              if (info == 0)
                {
                  retval = b;
                  FloatComplex *result = retval.fortran_vec ();

                  octave_idx_type b_nc = b.cols ();

                  char job = 'N';
                  F77_XFCN (cgetrs, CGETRS, (F77_CONST_CHAR_ARG2 (&job, 1),
                                             nr, b_nc, tmp_data, nr,
                                             pipvt, result, b.rows (), info
                                             F77_CHAR_ARG_LEN (1)));
                }
              else
                mattype.mark_as_rectangular ();
            }
        }
    }

  return retval;
}

FloatComplexMatrix
FloatComplexMatrix::solve (MatrixType &typ, const FloatMatrix& b) const
{
  octave_idx_type info;
  float rcon;
  return solve (typ, b, info, rcon, 0);
}

FloatComplexMatrix
FloatComplexMatrix::solve (MatrixType &typ, const FloatMatrix& b,
                           octave_idx_type& info) const
{
  float rcon;
  return solve (typ, b, info, rcon, 0);
}

FloatComplexMatrix
FloatComplexMatrix::solve (MatrixType &typ, const FloatMatrix& b,
                           octave_idx_type& info,
                           float& rcon) const
{
  return solve (typ, b, info, rcon, 0);
}

FloatComplexMatrix
FloatComplexMatrix::solve (MatrixType &typ, const FloatMatrix& b,
                           octave_idx_type& info,
                           float& rcon, solve_singularity_handler sing_handler,
                           bool singular_fallback, blas_trans_type transt) const
{
  FloatComplexMatrix tmp (b);
  return solve (typ, tmp, info, rcon, sing_handler, singular_fallback, transt);
}

FloatComplexMatrix
FloatComplexMatrix::solve (MatrixType &typ, const FloatComplexMatrix& b) const
{
  octave_idx_type info;
  float rcon;
  return solve (typ, b, info, rcon, 0);
}

FloatComplexMatrix
FloatComplexMatrix::solve (MatrixType &typ, const FloatComplexMatrix& b,
                           octave_idx_type& info) const
{
  float rcon;
  return solve (typ, b, info, rcon, 0);
}

FloatComplexMatrix
FloatComplexMatrix::solve (MatrixType &typ, const FloatComplexMatrix& b,
                           octave_idx_type& info, float& rcon) const
{
  return solve (typ, b, info, rcon, 0);
}

FloatComplexMatrix
FloatComplexMatrix::solve (MatrixType &mattype, const FloatComplexMatrix& b,
                           octave_idx_type& info, float& rcon,
                           solve_singularity_handler sing_handler,
                           bool singular_fallback, blas_trans_type transt) const
{
  FloatComplexMatrix retval;
  int typ = mattype.type ();

  if (typ == MatrixType::Unknown)
    typ = mattype.type (*this);

  // Only calculate the condition number for LU/Cholesky
  if (typ == MatrixType::Upper || typ == MatrixType::Permuted_Upper)
    retval = utsolve (mattype, b, info, rcon, sing_handler, true, transt);
  else if (typ == MatrixType::Lower || typ == MatrixType::Permuted_Lower)
    retval = ltsolve (mattype, b, info, rcon, sing_handler, true, transt);
  else if (transt == blas_trans)
    return transpose ().solve (mattype, b, info, rcon, sing_handler,
                               singular_fallback);
  else if (transt == blas_conj_trans)
    retval = hermitian ().solve (mattype, b, info, rcon, sing_handler,
                                 singular_fallback);
  else if (typ == MatrixType::Full || typ == MatrixType::Hermitian)
    retval = fsolve (mattype, b, info, rcon, sing_handler, true);
  else if (typ != MatrixType::Rectangular)
    {
      (*current_liboctave_error_handler) ("unknown matrix type");
      return FloatComplexMatrix ();
    }

  // Rectangular or one of the above solvers flags a singular matrix
  if (singular_fallback && mattype.type () == MatrixType::Rectangular)
    {
      octave_idx_type rank;
      retval = lssolve (b, info, rank, rcon);
    }

  return retval;
}

FloatComplexColumnVector
FloatComplexMatrix::solve (MatrixType &typ, const FloatColumnVector& b) const
{
  octave_idx_type info;
  float rcon;
  return solve (typ, FloatComplexColumnVector (b), info, rcon, 0);
}

FloatComplexColumnVector
FloatComplexMatrix::solve (MatrixType &typ, const FloatColumnVector& b,
                           octave_idx_type& info) const
{
  float rcon;
  return solve (typ, FloatComplexColumnVector (b), info, rcon, 0);
}

FloatComplexColumnVector
FloatComplexMatrix::solve (MatrixType &typ, const FloatColumnVector& b,
                           octave_idx_type& info, float& rcon) const
{
  return solve (typ, FloatComplexColumnVector (b), info, rcon, 0);
}

FloatComplexColumnVector
FloatComplexMatrix::solve (MatrixType &typ, const FloatColumnVector& b,
                           octave_idx_type& info, float& rcon,
                           solve_singularity_handler sing_handler,
                           blas_trans_type transt) const
{
  return solve (typ, FloatComplexColumnVector (b), info, rcon, sing_handler,
                transt);
}

FloatComplexColumnVector
FloatComplexMatrix::solve (MatrixType &typ,
                           const FloatComplexColumnVector& b) const
{
  octave_idx_type info;
  float rcon;
  return solve (typ, b, info, rcon, 0);
}

FloatComplexColumnVector
FloatComplexMatrix::solve (MatrixType &typ, const FloatComplexColumnVector& b,
                           octave_idx_type& info) const
{
  float rcon;
  return solve (typ, b, info, rcon, 0);
}

FloatComplexColumnVector
FloatComplexMatrix::solve (MatrixType &typ, const FloatComplexColumnVector& b,
                           octave_idx_type& info, float& rcon) const
{
  return solve (typ, b, info, rcon, 0);
}

FloatComplexColumnVector
FloatComplexMatrix::solve (MatrixType &typ, const FloatComplexColumnVector& b,
                           octave_idx_type& info, float& rcon,
                           solve_singularity_handler sing_handler,
                           blas_trans_type transt) const
{

  FloatComplexMatrix tmp (b);
  tmp = solve (typ, tmp, info, rcon, sing_handler, true, transt);
  return tmp.column (static_cast<octave_idx_type> (0));
}

FloatComplexMatrix
FloatComplexMatrix::solve (const FloatMatrix& b) const
{
  octave_idx_type info;
  float rcon;
  return solve (b, info, rcon, 0);
}

FloatComplexMatrix
FloatComplexMatrix::solve (const FloatMatrix& b, octave_idx_type& info) const
{
  float rcon;
  return solve (b, info, rcon, 0);
}

FloatComplexMatrix
FloatComplexMatrix::solve (const FloatMatrix& b, octave_idx_type& info,
                           float& rcon) const
{
  return solve (b, info, rcon, 0);
}

FloatComplexMatrix
FloatComplexMatrix::solve (const FloatMatrix& b, octave_idx_type& info,
                           float& rcon,
                           solve_singularity_handler sing_handler,
                           blas_trans_type transt) const
{
  FloatComplexMatrix tmp (b);
  return solve (tmp, info, rcon, sing_handler, transt);
}

FloatComplexMatrix
FloatComplexMatrix::solve (const FloatComplexMatrix& b) const
{
  octave_idx_type info;
  float rcon;
  return solve (b, info, rcon, 0);
}

FloatComplexMatrix
FloatComplexMatrix::solve (const FloatComplexMatrix& b,
                           octave_idx_type& info) const
{
  float rcon;
  return solve (b, info, rcon, 0);
}

FloatComplexMatrix
FloatComplexMatrix::solve (const FloatComplexMatrix& b, octave_idx_type& info,
                           float& rcon) const
{
  return solve (b, info, rcon, 0);
}

FloatComplexMatrix
FloatComplexMatrix::solve (const FloatComplexMatrix& b, octave_idx_type& info,
                           float& rcon,
                           solve_singularity_handler sing_handler,
                           blas_trans_type transt) const
{
  MatrixType mattype (*this);
  return solve (mattype, b, info, rcon, sing_handler, true, transt);
}

FloatComplexColumnVector
FloatComplexMatrix::solve (const FloatColumnVector& b) const
{
  octave_idx_type info;
  float rcon;
  return solve (FloatComplexColumnVector (b), info, rcon, 0);
}

FloatComplexColumnVector
FloatComplexMatrix::solve (const FloatColumnVector& b,
                           octave_idx_type& info) const
{
  float rcon;
  return solve (FloatComplexColumnVector (b), info, rcon, 0);
}

FloatComplexColumnVector
FloatComplexMatrix::solve (const FloatColumnVector& b, octave_idx_type& info,
                           float& rcon) const
{
  return solve (FloatComplexColumnVector (b), info, rcon, 0);
}

FloatComplexColumnVector
FloatComplexMatrix::solve (const FloatColumnVector& b, octave_idx_type& info,
                           float& rcon,
                           solve_singularity_handler sing_handler,
                           blas_trans_type transt) const
{
  return solve (FloatComplexColumnVector (b), info, rcon, sing_handler, transt);
}

FloatComplexColumnVector
FloatComplexMatrix::solve (const FloatComplexColumnVector& b) const
{
  octave_idx_type info;
  float rcon;
  return solve (b, info, rcon, 0);
}

FloatComplexColumnVector
FloatComplexMatrix::solve (const FloatComplexColumnVector& b,
                           octave_idx_type& info) const
{
  float rcon;
  return solve (b, info, rcon, 0);
}

FloatComplexColumnVector
FloatComplexMatrix::solve (const FloatComplexColumnVector& b,
                           octave_idx_type& info,
                           float& rcon) const
{
  return solve (b, info, rcon, 0);
}

FloatComplexColumnVector
FloatComplexMatrix::solve (const FloatComplexColumnVector& b,
                           octave_idx_type& info,
                           float& rcon,
                           solve_singularity_handler sing_handler,
                           blas_trans_type transt) const
{
  MatrixType mattype (*this);
  return solve (mattype, b, info, rcon, sing_handler, transt);
}

FloatComplexMatrix
FloatComplexMatrix::lssolve (const FloatMatrix& b) const
{
  octave_idx_type info;
  octave_idx_type rank;
  float rcon;
  return lssolve (FloatComplexMatrix (b), info, rank, rcon);
}

FloatComplexMatrix
FloatComplexMatrix::lssolve (const FloatMatrix& b, octave_idx_type& info) const
{
  octave_idx_type rank;
  float rcon;
  return lssolve (FloatComplexMatrix (b), info, rank, rcon);
}

FloatComplexMatrix
FloatComplexMatrix::lssolve (const FloatMatrix& b, octave_idx_type& info,
                             octave_idx_type& rank) const
{
  float rcon;
  return lssolve (FloatComplexMatrix (b), info, rank, rcon);
}

FloatComplexMatrix
FloatComplexMatrix::lssolve (const FloatMatrix& b, octave_idx_type& info,
                             octave_idx_type& rank, float& rcon) const
{
  return lssolve (FloatComplexMatrix (b), info, rank, rcon);
}

FloatComplexMatrix
FloatComplexMatrix::lssolve (const FloatComplexMatrix& b) const
{
  octave_idx_type info;
  octave_idx_type rank;
  float rcon;
  return lssolve (b, info, rank, rcon);
}

FloatComplexMatrix
FloatComplexMatrix::lssolve (const FloatComplexMatrix& b,
                             octave_idx_type& info) const
{
  octave_idx_type rank;
  float rcon;
  return lssolve (b, info, rank, rcon);
}

FloatComplexMatrix
FloatComplexMatrix::lssolve (const FloatComplexMatrix& b, octave_idx_type& info,
                             octave_idx_type& rank) const
{
  float rcon;
  return lssolve (b, info, rank, rcon);
}

FloatComplexMatrix
FloatComplexMatrix::lssolve (const FloatComplexMatrix& b, octave_idx_type& info,
                             octave_idx_type& rank, float& rcon) const
{
  FloatComplexMatrix retval;

  octave_idx_type nrhs = b.cols ();

  octave_idx_type m = rows ();
  octave_idx_type n = cols ();

  if (m != b.rows ())
    (*current_liboctave_error_handler)
      ("matrix dimension mismatch solution of linear equations");
  else if (m== 0 || n == 0 || b.cols () == 0)
    retval = FloatComplexMatrix (n, b.cols (), FloatComplex (0.0, 0.0));
  else
    {
      volatile octave_idx_type minmn = (m < n ? m : n);
      octave_idx_type maxmn = m > n ? m : n;
      rcon = -1.0;

      if (m != n)
        {
          retval = FloatComplexMatrix (maxmn, nrhs);

          for (octave_idx_type j = 0; j < nrhs; j++)
            for (octave_idx_type i = 0; i < m; i++)
              retval.elem (i, j) = b.elem (i, j);
        }
      else
        retval = b;

      FloatComplexMatrix atmp = *this;
      FloatComplex *tmp_data = atmp.fortran_vec ();

      FloatComplex *pretval = retval.fortran_vec ();
      Array<float> s (dim_vector (minmn, 1));
      float *ps = s.fortran_vec ();

      // Ask ZGELSD what the dimension of WORK should be.
      octave_idx_type lwork = -1;

      Array<FloatComplex> work (dim_vector (1, 1));

      octave_idx_type smlsiz;
      F77_FUNC (xilaenv, XILAENV) (9, F77_CONST_CHAR_ARG2 ("CGELSD", 6),
                                   F77_CONST_CHAR_ARG2 (" ", 1),
                                   0, 0, 0, 0, smlsiz
                                   F77_CHAR_ARG_LEN (6)
                                   F77_CHAR_ARG_LEN (1));

      octave_idx_type mnthr;
      F77_FUNC (xilaenv, XILAENV) (6, F77_CONST_CHAR_ARG2 ("CGELSD", 6),
                                   F77_CONST_CHAR_ARG2 (" ", 1),
                                   m, n, nrhs, -1, mnthr
                                   F77_CHAR_ARG_LEN (6)
                                   F77_CHAR_ARG_LEN (1));

      // We compute the size of rwork and iwork because ZGELSD in
      // older versions of LAPACK does not return them on a query
      // call.
      float dminmn = static_cast<float> (minmn);
      float dsmlsizp1 = static_cast<float> (smlsiz+1);
      float tmp = xlog2 (dminmn / dsmlsizp1);

      octave_idx_type nlvl = static_cast<octave_idx_type> (tmp) + 1;
      if (nlvl < 0)
        nlvl = 0;

      octave_idx_type lrwork = minmn*(10 + 2*smlsiz + 8*nlvl)
                               + 3*smlsiz*nrhs
                               + std::max ((smlsiz+1)*(smlsiz+1),
                                           n*(1+nrhs) + 2*nrhs);
      if (lrwork < 1)
        lrwork = 1;
      Array<float> rwork (dim_vector (lrwork, 1));
      float *prwork = rwork.fortran_vec ();

      octave_idx_type liwork = 3 * minmn * nlvl + 11 * minmn;
      if (liwork < 1)
        liwork = 1;
      Array<octave_idx_type> iwork (dim_vector (liwork, 1));
      octave_idx_type* piwork = iwork.fortran_vec ();

      F77_XFCN (cgelsd, CGELSD, (m, n, nrhs, tmp_data, m, pretval, maxmn,
                                 ps, rcon, rank, work.fortran_vec (),
                                 lwork, prwork, piwork, info));

      // The workspace query is broken in at least LAPACK 3.0.0
      // through 3.1.1 when n >= mnthr.  The obtuse formula below
      // should provide sufficient workspace for ZGELSD to operate
      // efficiently.
      if (n > m && n >= mnthr)
        {
          octave_idx_type addend = m;

          if (2*m-4 > addend)
            addend = 2*m-4;

          if (nrhs > addend)
            addend = nrhs;

          if (n-3*m > addend)
            addend = n-3*m;

          const octave_idx_type lworkaround = 4*m + m*m + addend;

          if (std::real (work(0)) < lworkaround)
            work(0) = lworkaround;
        }
      else if (m >= n)
        {
          octave_idx_type lworkaround = 2*m + m*nrhs;

          if (std::real (work(0)) < lworkaround)
            work(0) = lworkaround;
        }

      lwork = static_cast<octave_idx_type> (std::real (work(0)));
      work.resize (dim_vector (lwork, 1));

      F77_XFCN (cgelsd, CGELSD, (m, n, nrhs, tmp_data, m, pretval,
                                 maxmn, ps, rcon, rank,
                                 work.fortran_vec (), lwork,
                                 prwork, piwork, info));

      if (s.elem (0) == 0.0)
        rcon = 0.0;
      else
        rcon = s.elem (minmn - 1) / s.elem (0);

      retval.resize (n, nrhs);
    }

  return retval;
}

FloatComplexColumnVector
FloatComplexMatrix::lssolve (const FloatColumnVector& b) const
{
  octave_idx_type info;
  octave_idx_type rank;
  float rcon;
  return lssolve (FloatComplexColumnVector (b), info, rank, rcon);
}

FloatComplexColumnVector
FloatComplexMatrix::lssolve (const FloatColumnVector& b,
                             octave_idx_type& info) const
{
  octave_idx_type rank;
  float rcon;
  return lssolve (FloatComplexColumnVector (b), info, rank, rcon);
}

FloatComplexColumnVector
FloatComplexMatrix::lssolve (const FloatColumnVector& b, octave_idx_type& info,
                             octave_idx_type& rank) const
{
  float rcon;
  return lssolve (FloatComplexColumnVector (b), info, rank, rcon);
}

FloatComplexColumnVector
FloatComplexMatrix::lssolve (const FloatColumnVector& b, octave_idx_type& info,
                             octave_idx_type& rank, float& rcon) const
{
  return lssolve (FloatComplexColumnVector (b), info, rank, rcon);
}

FloatComplexColumnVector
FloatComplexMatrix::lssolve (const FloatComplexColumnVector& b) const
{
  octave_idx_type info;
  octave_idx_type rank;
  float rcon;
  return lssolve (b, info, rank, rcon);
}

FloatComplexColumnVector
FloatComplexMatrix::lssolve (const FloatComplexColumnVector& b,
                             octave_idx_type& info) const
{
  octave_idx_type rank;
  float rcon;
  return lssolve (b, info, rank, rcon);
}

FloatComplexColumnVector
FloatComplexMatrix::lssolve (const FloatComplexColumnVector& b,
                             octave_idx_type& info,
                             octave_idx_type& rank) const
{
  float rcon;
  return lssolve (b, info, rank, rcon);

}

FloatComplexColumnVector
FloatComplexMatrix::lssolve (const FloatComplexColumnVector& b,
                             octave_idx_type& info,
                             octave_idx_type& rank, float& rcon) const
{
  FloatComplexColumnVector retval;

  octave_idx_type nrhs = 1;

  octave_idx_type m = rows ();
  octave_idx_type n = cols ();

  if (m != b.length ())
    (*current_liboctave_error_handler)
      ("matrix dimension mismatch solution of linear equations");
  else if (m == 0 || n == 0 || b.cols () == 0)
    retval = FloatComplexColumnVector (n, FloatComplex (0.0, 0.0));
  else
    {
      volatile octave_idx_type minmn = (m < n ? m : n);
      octave_idx_type maxmn = m > n ? m : n;
      rcon = -1.0;

      if (m != n)
        {
          retval = FloatComplexColumnVector (maxmn);

          for (octave_idx_type i = 0; i < m; i++)
            retval.elem (i) = b.elem (i);
        }
      else
        retval = b;

      FloatComplexMatrix atmp = *this;
      FloatComplex *tmp_data = atmp.fortran_vec ();

      FloatComplex *pretval = retval.fortran_vec ();
      Array<float> s (dim_vector (minmn, 1));
      float *ps = s.fortran_vec ();

      // Ask ZGELSD what the dimension of WORK should be.
      octave_idx_type lwork = -1;

      Array<FloatComplex> work (dim_vector (1, 1));

      octave_idx_type smlsiz;
      F77_FUNC (xilaenv, XILAENV) (9, F77_CONST_CHAR_ARG2 ("CGELSD", 6),
                                   F77_CONST_CHAR_ARG2 (" ", 1),
                                   0, 0, 0, 0, smlsiz
                                   F77_CHAR_ARG_LEN (6)
                                   F77_CHAR_ARG_LEN (1));

      // We compute the size of rwork and iwork because ZGELSD in
      // older versions of LAPACK does not return them on a query
      // call.
      float dminmn = static_cast<float> (minmn);
      float dsmlsizp1 = static_cast<float> (smlsiz+1);
      float tmp = xlog2 (dminmn / dsmlsizp1);

      octave_idx_type nlvl = static_cast<octave_idx_type> (tmp) + 1;
      if (nlvl < 0)
        nlvl = 0;

      octave_idx_type lrwork = minmn*(10 + 2*smlsiz + 8*nlvl)
                               + 3*smlsiz*nrhs + (smlsiz+1)*(smlsiz+1);
      if (lrwork < 1)
        lrwork = 1;
      Array<float> rwork (dim_vector (lrwork, 1));
      float *prwork = rwork.fortran_vec ();

      octave_idx_type liwork = 3 * minmn * nlvl + 11 * minmn;
      if (liwork < 1)
        liwork = 1;
      Array<octave_idx_type> iwork (dim_vector (liwork, 1));
      octave_idx_type* piwork = iwork.fortran_vec ();

      F77_XFCN (cgelsd, CGELSD, (m, n, nrhs, tmp_data, m, pretval, maxmn,
                                 ps, rcon, rank, work.fortran_vec (),
                                 lwork, prwork, piwork, info));

      lwork = static_cast<octave_idx_type> (std::real (work(0)));
      work.resize (dim_vector (lwork, 1));
      rwork.resize (dim_vector (static_cast<octave_idx_type> (rwork(0)), 1));
      iwork.resize (dim_vector (iwork(0), 1));

      F77_XFCN (cgelsd, CGELSD, (m, n, nrhs, tmp_data, m, pretval,
                                 maxmn, ps, rcon, rank,
                                 work.fortran_vec (), lwork,
                                 prwork, piwork, info));

      if (rank < minmn)
        {
          if (s.elem (0) == 0.0)
            rcon = 0.0;
          else
            rcon = s.elem (minmn - 1) / s.elem (0);

          retval.resize (n, nrhs);
        }
    }

  return retval;
}

// column vector by row vector -> matrix operations

FloatComplexMatrix
operator * (const FloatColumnVector& v, const FloatComplexRowVector& a)
{
  FloatComplexColumnVector tmp (v);
  return tmp * a;
}

FloatComplexMatrix
operator * (const FloatComplexColumnVector& a, const FloatRowVector& b)
{
  FloatComplexRowVector tmp (b);
  return a * tmp;
}

FloatComplexMatrix
operator * (const FloatComplexColumnVector& v, const FloatComplexRowVector& a)
{
  FloatComplexMatrix retval;

  octave_idx_type len = v.length ();

  if (len != 0)
    {
      octave_idx_type a_len = a.length ();

      retval = FloatComplexMatrix (len, a_len);
      FloatComplex *c = retval.fortran_vec ();

      F77_XFCN (cgemm, CGEMM, (F77_CONST_CHAR_ARG2 ("N", 1),
                               F77_CONST_CHAR_ARG2 ("N", 1),
                               len, a_len, 1, 1.0, v.data (), len,
                               a.data (), 1, 0.0, c, len
                               F77_CHAR_ARG_LEN (1)
                               F77_CHAR_ARG_LEN (1)));
    }

  return retval;
}

// matrix by diagonal matrix -> matrix operations

FloatComplexMatrix&
FloatComplexMatrix::operator += (const FloatDiagMatrix& a)
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  octave_idx_type a_nr = rows ();
  octave_idx_type a_nc = cols ();

  if (nr != a_nr || nc != a_nc)
    {
      gripe_nonconformant ("operator +=", nr, nc, a_nr, a_nc);
      return *this;
    }

  for (octave_idx_type i = 0; i < a.length (); i++)
    elem (i, i) += a.elem (i, i);

  return *this;
}

FloatComplexMatrix&
FloatComplexMatrix::operator -= (const FloatDiagMatrix& a)
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  octave_idx_type a_nr = rows ();
  octave_idx_type a_nc = cols ();

  if (nr != a_nr || nc != a_nc)
    {
      gripe_nonconformant ("operator -=", nr, nc, a_nr, a_nc);
      return *this;
    }

  for (octave_idx_type i = 0; i < a.length (); i++)
    elem (i, i) -= a.elem (i, i);

  return *this;
}

FloatComplexMatrix&
FloatComplexMatrix::operator += (const FloatComplexDiagMatrix& a)
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  octave_idx_type a_nr = rows ();
  octave_idx_type a_nc = cols ();

  if (nr != a_nr || nc != a_nc)
    {
      gripe_nonconformant ("operator +=", nr, nc, a_nr, a_nc);
      return *this;
    }

  for (octave_idx_type i = 0; i < a.length (); i++)
    elem (i, i) += a.elem (i, i);

  return *this;
}

FloatComplexMatrix&
FloatComplexMatrix::operator -= (const FloatComplexDiagMatrix& a)
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  octave_idx_type a_nr = rows ();
  octave_idx_type a_nc = cols ();

  if (nr != a_nr || nc != a_nc)
    {
      gripe_nonconformant ("operator -=", nr, nc, a_nr, a_nc);
      return *this;
    }

  for (octave_idx_type i = 0; i < a.length (); i++)
    elem (i, i) -= a.elem (i, i);

  return *this;
}

// matrix by matrix -> matrix operations

FloatComplexMatrix&
FloatComplexMatrix::operator += (const FloatMatrix& a)
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  octave_idx_type a_nr = a.rows ();
  octave_idx_type a_nc = a.cols ();

  if (nr != a_nr || nc != a_nc)
    {
      gripe_nonconformant ("operator +=", nr, nc, a_nr, a_nc);
      return *this;
    }

  if (nr == 0 || nc == 0)
    return *this;

  FloatComplex *d = fortran_vec (); // Ensures only 1 reference to my privates!

  mx_inline_add2 (length (), d, a.data ());
  return *this;
}

FloatComplexMatrix&
FloatComplexMatrix::operator -= (const FloatMatrix& a)
{
  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  octave_idx_type a_nr = a.rows ();
  octave_idx_type a_nc = a.cols ();

  if (nr != a_nr || nc != a_nc)
    {
      gripe_nonconformant ("operator -=", nr, nc, a_nr, a_nc);
      return *this;
    }

  if (nr == 0 || nc == 0)
    return *this;

  FloatComplex *d = fortran_vec (); // Ensures only 1 reference to my privates!

  mx_inline_sub2 (length (), d, a.data ());
  return *this;
}

// unary operations

boolMatrix
FloatComplexMatrix::all (int dim) const
{
  return FloatComplexNDArray::all (dim);
}

boolMatrix
FloatComplexMatrix::any (int dim) const
{
  return FloatComplexNDArray::any (dim);
}

FloatComplexMatrix
FloatComplexMatrix::cumprod (int dim) const
{
  return FloatComplexNDArray::cumprod (dim);
}

FloatComplexMatrix
FloatComplexMatrix::cumsum (int dim) const
{
  return FloatComplexNDArray::cumsum (dim);
}

FloatComplexMatrix
FloatComplexMatrix::prod (int dim) const
{
  return FloatComplexNDArray::prod (dim);
}

FloatComplexMatrix
FloatComplexMatrix::sum (int dim) const
{
  return FloatComplexNDArray::sum (dim);
}

FloatComplexMatrix
FloatComplexMatrix::sumsq (int dim) const
{
  return FloatComplexNDArray::sumsq (dim);
}

FloatMatrix FloatComplexMatrix::abs (void) const
{
  return FloatComplexNDArray::abs ();
}

FloatComplexMatrix
FloatComplexMatrix::diag (octave_idx_type k) const
{
  return FloatComplexNDArray::diag (k);
}

FloatComplexDiagMatrix
FloatComplexMatrix::diag (octave_idx_type m, octave_idx_type n) const
{
  FloatComplexDiagMatrix retval;

  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  if (nr == 1 || nc == 1)
    retval = FloatComplexDiagMatrix (*this, m, n);
  else
    (*current_liboctave_error_handler)
      ("diag: expecting vector argument");

  return retval;
}

bool
FloatComplexMatrix::row_is_real_only (octave_idx_type i) const
{
  bool retval = true;

  octave_idx_type nc = columns ();

  for (octave_idx_type j = 0; j < nc; j++)
    {
      if (std::imag (elem (i, j)) != 0.0)
        {
          retval = false;
          break;
        }
    }

  return retval;
}

bool
FloatComplexMatrix::column_is_real_only (octave_idx_type j) const
{
  bool retval = true;

  octave_idx_type nr = rows ();

  for (octave_idx_type i = 0; i < nr; i++)
    {
      if (std::imag (elem (i, j)) != 0.0)
        {
          retval = false;
          break;
        }
    }

  return retval;
}

FloatComplexColumnVector
FloatComplexMatrix::row_min (void) const
{
  Array<octave_idx_type> dummy_idx;
  return row_min (dummy_idx);
}

FloatComplexColumnVector
FloatComplexMatrix::row_min (Array<octave_idx_type>& idx_arg) const
{
  FloatComplexColumnVector result;

  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  if (nr > 0 && nc > 0)
    {
      result.resize (nr);
      idx_arg.resize (dim_vector (nr, 1));

      for (octave_idx_type i = 0; i < nr; i++)
        {
          bool real_only = row_is_real_only (i);

          octave_idx_type idx_j;

          FloatComplex tmp_min;

          float abs_min = octave_Float_NaN;

          for (idx_j = 0; idx_j < nc; idx_j++)
            {
              tmp_min = elem (i, idx_j);

              if (! xisnan (tmp_min))
                {
                  abs_min = real_only ? std::real (tmp_min)
                                      : std::abs (tmp_min);
                  break;
                }
            }

          for (octave_idx_type j = idx_j+1; j < nc; j++)
            {
              FloatComplex tmp = elem (i, j);

              if (xisnan (tmp))
                continue;

              float abs_tmp = real_only ? std::real (tmp) : std::abs (tmp);

              if (abs_tmp < abs_min)
                {
                  idx_j = j;
                  tmp_min = tmp;
                  abs_min = abs_tmp;
                }
            }

          if (xisnan (tmp_min))
            {
              result.elem (i) = FloatComplex_NaN_result;
              idx_arg.elem (i) = 0;
            }
          else
            {
              result.elem (i) = tmp_min;
              idx_arg.elem (i) = idx_j;
            }
        }
    }

  return result;
}

FloatComplexColumnVector
FloatComplexMatrix::row_max (void) const
{
  Array<octave_idx_type> dummy_idx;
  return row_max (dummy_idx);
}

FloatComplexColumnVector
FloatComplexMatrix::row_max (Array<octave_idx_type>& idx_arg) const
{
  FloatComplexColumnVector result;

  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  if (nr > 0 && nc > 0)
    {
      result.resize (nr);
      idx_arg.resize (dim_vector (nr, 1));

      for (octave_idx_type i = 0; i < nr; i++)
        {
          bool real_only = row_is_real_only (i);

          octave_idx_type idx_j;

          FloatComplex tmp_max;

          float abs_max = octave_Float_NaN;

          for (idx_j = 0; idx_j < nc; idx_j++)
            {
              tmp_max = elem (i, idx_j);

              if (! xisnan (tmp_max))
                {
                  abs_max = real_only ? std::real (tmp_max)
                                      : std::abs (tmp_max);
                  break;
                }
            }

          for (octave_idx_type j = idx_j+1; j < nc; j++)
            {
              FloatComplex tmp = elem (i, j);

              if (xisnan (tmp))
                continue;

              float abs_tmp = real_only ? std::real (tmp) : std::abs (tmp);

              if (abs_tmp > abs_max)
                {
                  idx_j = j;
                  tmp_max = tmp;
                  abs_max = abs_tmp;
                }
            }

          if (xisnan (tmp_max))
            {
              result.elem (i) = FloatComplex_NaN_result;
              idx_arg.elem (i) = 0;
            }
          else
            {
              result.elem (i) = tmp_max;
              idx_arg.elem (i) = idx_j;
            }
        }
    }

  return result;
}

FloatComplexRowVector
FloatComplexMatrix::column_min (void) const
{
  Array<octave_idx_type> dummy_idx;
  return column_min (dummy_idx);
}

FloatComplexRowVector
FloatComplexMatrix::column_min (Array<octave_idx_type>& idx_arg) const
{
  FloatComplexRowVector result;

  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  if (nr > 0 && nc > 0)
    {
      result.resize (nc);
      idx_arg.resize (dim_vector (1, nc));

      for (octave_idx_type j = 0; j < nc; j++)
        {
          bool real_only = column_is_real_only (j);

          octave_idx_type idx_i;

          FloatComplex tmp_min;

          float abs_min = octave_Float_NaN;

          for (idx_i = 0; idx_i < nr; idx_i++)
            {
              tmp_min = elem (idx_i, j);

              if (! xisnan (tmp_min))
                {
                  abs_min = real_only ? std::real (tmp_min)
                                      : std::abs (tmp_min);
                  break;
                }
            }

          for (octave_idx_type i = idx_i+1; i < nr; i++)
            {
              FloatComplex tmp = elem (i, j);

              if (xisnan (tmp))
                continue;

              float abs_tmp = real_only ? std::real (tmp) : std::abs (tmp);

              if (abs_tmp < abs_min)
                {
                  idx_i = i;
                  tmp_min = tmp;
                  abs_min = abs_tmp;
                }
            }

          if (xisnan (tmp_min))
            {
              result.elem (j) = FloatComplex_NaN_result;
              idx_arg.elem (j) = 0;
            }
          else
            {
              result.elem (j) = tmp_min;
              idx_arg.elem (j) = idx_i;
            }
        }
    }

  return result;
}

FloatComplexRowVector
FloatComplexMatrix::column_max (void) const
{
  Array<octave_idx_type> dummy_idx;
  return column_max (dummy_idx);
}

FloatComplexRowVector
FloatComplexMatrix::column_max (Array<octave_idx_type>& idx_arg) const
{
  FloatComplexRowVector result;

  octave_idx_type nr = rows ();
  octave_idx_type nc = cols ();

  if (nr > 0 && nc > 0)
    {
      result.resize (nc);
      idx_arg.resize (dim_vector (1, nc));

      for (octave_idx_type j = 0; j < nc; j++)
        {
          bool real_only = column_is_real_only (j);

          octave_idx_type idx_i;

          FloatComplex tmp_max;

          float abs_max = octave_Float_NaN;

          for (idx_i = 0; idx_i < nr; idx_i++)
            {
              tmp_max = elem (idx_i, j);

              if (! xisnan (tmp_max))
                {
                  abs_max = real_only ? std::real (tmp_max)
                                      : std::abs (tmp_max);
                  break;
                }
            }

          for (octave_idx_type i = idx_i+1; i < nr; i++)
            {
              FloatComplex tmp = elem (i, j);

              if (xisnan (tmp))
                continue;

              float abs_tmp = real_only ? std::real (tmp) : std::abs (tmp);

              if (abs_tmp > abs_max)
                {
                  idx_i = i;
                  tmp_max = tmp;
                  abs_max = abs_tmp;
                }
            }

          if (xisnan (tmp_max))
            {
              result.elem (j) = FloatComplex_NaN_result;
              idx_arg.elem (j) = 0;
            }
          else
            {
              result.elem (j) = tmp_max;
              idx_arg.elem (j) = idx_i;
            }
        }
    }

  return result;
}

// i/o

std::ostream&
operator << (std::ostream& os, const FloatComplexMatrix& a)
{
  for (octave_idx_type i = 0; i < a.rows (); i++)
    {
      for (octave_idx_type j = 0; j < a.cols (); j++)
        {
          os << " ";
          octave_write_complex (os, a.elem (i, j));
        }
      os << "\n";
    }
  return os;
}

std::istream&
operator >> (std::istream& is, FloatComplexMatrix& a)
{
  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.cols ();

  if (nr > 0 && nc > 0)
    {
      FloatComplex tmp;
      for (octave_idx_type i = 0; i < nr; i++)
        for (octave_idx_type j = 0; j < nc; j++)
          {
            tmp = octave_read_value<FloatComplex> (is);
            if (is)
              a.elem (i, j) = tmp;
            else
              goto done;
          }
    }

done:

  return is;
}

FloatComplexMatrix
Givens (const FloatComplex& x, const FloatComplex& y)
{
  float cc;
  FloatComplex cs, temp_r;

  F77_FUNC (clartg, CLARTG) (x, y, cc, cs, temp_r);

  FloatComplexMatrix g (2, 2);

  g.elem (0, 0) = cc;
  g.elem (1, 1) = cc;
  g.elem (0, 1) = cs;
  g.elem (1, 0) = -conj (cs);

  return g;
}

FloatComplexMatrix
Sylvester (const FloatComplexMatrix& a, const FloatComplexMatrix& b,
           const FloatComplexMatrix& c)
{
  FloatComplexMatrix retval;

  // FIXME: need to check that a, b, and c are all the same
  // size.

  // Compute Schur decompositions

  FloatComplexSCHUR as (a, "U");
  FloatComplexSCHUR bs (b, "U");

  // Transform c to new coordinates.

  FloatComplexMatrix ua = as.unitary_matrix ();
  FloatComplexMatrix sch_a = as.schur_matrix ();

  FloatComplexMatrix ub = bs.unitary_matrix ();
  FloatComplexMatrix sch_b = bs.schur_matrix ();

  FloatComplexMatrix cx = ua.hermitian () * c * ub;

  // Solve the sylvester equation, back-transform, and return the
  // solution.

  octave_idx_type a_nr = a.rows ();
  octave_idx_type b_nr = b.rows ();

  float scale;
  octave_idx_type info;

  FloatComplex *pa = sch_a.fortran_vec ();
  FloatComplex *pb = sch_b.fortran_vec ();
  FloatComplex *px = cx.fortran_vec ();

  F77_XFCN (ctrsyl, CTRSYL, (F77_CONST_CHAR_ARG2 ("N", 1),
                             F77_CONST_CHAR_ARG2 ("N", 1),
                             1, a_nr, b_nr, pa, a_nr, pb,
                             b_nr, px, a_nr, scale, info
                             F77_CHAR_ARG_LEN (1)
                             F77_CHAR_ARG_LEN (1)));

  // FIXME: check info?

  retval = ua * cx * ub.hermitian ();

  return retval;
}

FloatComplexMatrix
operator * (const FloatComplexMatrix& m, const FloatMatrix& a)
{
  if (m.columns () > std::min (m.rows (), a.columns ()) / 10)
    return FloatComplexMatrix (real (m) * a, imag (m) * a);
  else
    return m * FloatComplexMatrix (a);
}

FloatComplexMatrix
operator * (const FloatMatrix& m, const FloatComplexMatrix& a)
{
  if (a.rows () > std::min (m.rows (), a.columns ()) / 10)
    return FloatComplexMatrix (m * real (a), m * imag (a));
  else
    return FloatComplexMatrix (m) * a;
}

/*

## Simple Dot Product, Matrix-Vector, and Matrix-Matrix Unit tests
%!assert (single ([1+i 2+i 3+i]) * single ([ 4+i ; 5+i ; 6+i]), single (29+21i), 5e-7)
%!assert (single ([1+i 2+i ; 3+i 4+i]) * single ([5+i ; 6+i]), single ([15 + 14i ; 37 + 18i]), 5e-7)
%!assert (single ([1+i 2+i ; 3+i 4+i ]) * single ([5+i 6+i ; 7+i 8+i]), single ([17 + 15i 20 + 17i; 41 + 19i 48 + 21i]), 5e-7)
%!assert (single ([1 i])*single ([i 0])', single (-i))

## Test some simple identities
%!shared M, cv, rv
%! M = single (randn (10,10))+ i*single (rand (10,10));
%! cv = single (randn (10,1))+ i*single (rand (10,1));
%! rv = single (randn (1,10))+ i*single (rand (1,10));
%!assert ([M*cv,M*cv], M*[cv,cv], 5e-6)
%!assert ([M.'*cv,M.'*cv], M.'*[cv,cv], 5e-6)
%!assert ([M'*cv,M'*cv], M'*[cv,cv], 5e-6)
%!assert ([rv*M;rv*M], [rv;rv]*M, 5e-6)
%!assert ([rv*M.';rv*M.'], [rv;rv]*M.', 5e-6)
%!assert ([rv*M';rv*M'], [rv;rv]*M', 5e-6)
%!assert (2*rv*cv, [rv,rv]*[cv;cv], 5e-6)

*/

static char
get_blas_trans_arg (bool trans, bool conj)
{
  return trans ? (conj ? 'C' : 'T') : 'N';
}

// the general GEMM operation

FloatComplexMatrix
xgemm (const FloatComplexMatrix& a, const FloatComplexMatrix& b,
       blas_trans_type transa, blas_trans_type transb)
{
  FloatComplexMatrix retval;

  bool tra = transa != blas_no_trans;
  bool trb = transb != blas_no_trans;
  bool cja = transa == blas_conj_trans;
  bool cjb = transb == blas_conj_trans;

  octave_idx_type a_nr = tra ? a.cols () : a.rows ();
  octave_idx_type a_nc = tra ? a.rows () : a.cols ();

  octave_idx_type b_nr = trb ? b.cols () : b.rows ();
  octave_idx_type b_nc = trb ? b.rows () : b.cols ();

  if (a_nc != b_nr)
    gripe_nonconformant ("operator *", a_nr, a_nc, b_nr, b_nc);
  else
    {
      if (a_nr == 0 || a_nc == 0 || b_nc == 0)
        retval = FloatComplexMatrix (a_nr, b_nc, 0.0);
      else if (a.data () == b.data () && a_nr == b_nc && tra != trb)
        {
          octave_idx_type lda = a.rows ();

          // FIXME: looking at the reference BLAS, it appears that it
          // should not be necessary to initialize the output matrix if
          // BETA is 0 in the call to CHERK, but ATLAS appears to
          // use the result matrix before zeroing the elements.

          retval = FloatComplexMatrix (a_nr, b_nc, 0.0);
          FloatComplex *c = retval.fortran_vec ();

          const char ctra = get_blas_trans_arg (tra, cja);
          if (cja || cjb)
            {
              F77_XFCN (cherk, CHERK, (F77_CONST_CHAR_ARG2 ("U", 1),
                                       F77_CONST_CHAR_ARG2 (&ctra, 1),
                                       a_nr, a_nc, 1.0,
                                       a.data (), lda, 0.0, c, a_nr
                                       F77_CHAR_ARG_LEN (1)
                                       F77_CHAR_ARG_LEN (1)));
              for (octave_idx_type j = 0; j < a_nr; j++)
                for (octave_idx_type i = 0; i < j; i++)
                  retval.xelem (j,i) = std::conj (retval.xelem (i,j));
            }
          else
            {
              F77_XFCN (csyrk, CSYRK, (F77_CONST_CHAR_ARG2 ("U", 1),
                                       F77_CONST_CHAR_ARG2 (&ctra, 1),
                                       a_nr, a_nc, 1.0,
                                       a.data (), lda, 0.0, c, a_nr
                                       F77_CHAR_ARG_LEN (1)
                                       F77_CHAR_ARG_LEN (1)));
              for (octave_idx_type j = 0; j < a_nr; j++)
                for (octave_idx_type i = 0; i < j; i++)
                  retval.xelem (j,i) = retval.xelem (i,j);

            }

        }
      else
        {
          octave_idx_type lda = a.rows ();
          octave_idx_type tda = a.cols ();
          octave_idx_type ldb = b.rows ();
          octave_idx_type tdb = b.cols ();

          retval = FloatComplexMatrix (a_nr, b_nc, 0.0);
          FloatComplex *c = retval.fortran_vec ();

          if (b_nc == 1 && a_nr == 1)
            {
              if (cja == cjb)
                {
                  F77_FUNC (xcdotu, XCDOTU) (a_nc, a.data (), 1, b.data (), 1,
                                             *c);
                  if (cja) *c = std::conj (*c);
                }
              else if (cja)
                F77_FUNC (xcdotc, XCDOTC) (a_nc, a.data (), 1, b.data (), 1,
                                           *c);
              else
                F77_FUNC (xcdotc, XCDOTC) (a_nc, b.data (), 1, a.data (), 1,
                                           *c);
            }
          else if (b_nc == 1 && ! cjb)
            {
              const char ctra = get_blas_trans_arg (tra, cja);
              F77_XFCN (cgemv, CGEMV, (F77_CONST_CHAR_ARG2 (&ctra, 1),
                                       lda, tda, 1.0,  a.data (), lda,
                                       b.data (), 1, 0.0, c, 1
                                       F77_CHAR_ARG_LEN (1)));
            }
          else if (a_nr == 1 && ! cja && ! cjb)
            {
              const char crevtrb = get_blas_trans_arg (! trb, cjb);
              F77_XFCN (cgemv, CGEMV, (F77_CONST_CHAR_ARG2 (&crevtrb, 1),
                                       ldb, tdb, 1.0,  b.data (), ldb,
                                       a.data (), 1, 0.0, c, 1
                                       F77_CHAR_ARG_LEN (1)));
            }
          else
            {
              const char ctra = get_blas_trans_arg (tra, cja);
              const char ctrb = get_blas_trans_arg (trb, cjb);
              F77_XFCN (cgemm, CGEMM, (F77_CONST_CHAR_ARG2 (&ctra, 1),
                                       F77_CONST_CHAR_ARG2 (&ctrb, 1),
                                       a_nr, b_nc, a_nc, 1.0, a.data (),
                                       lda, b.data (), ldb, 0.0, c, a_nr
                                       F77_CHAR_ARG_LEN (1)
                                       F77_CHAR_ARG_LEN (1)));
            }
        }
    }

  return retval;
}

FloatComplexMatrix
operator * (const FloatComplexMatrix& a, const FloatComplexMatrix& b)
{
  return xgemm (a, b);
}

// FIXME: it would be nice to share code among the min/max
// functions below.

#define EMPTY_RETURN_CHECK(T) \
  if (nr == 0 || nc == 0) \
    return T (nr, nc);

FloatComplexMatrix
min (const FloatComplex& c, const FloatComplexMatrix& m)
{
  octave_idx_type nr = m.rows ();
  octave_idx_type nc = m.columns ();

  EMPTY_RETURN_CHECK (FloatComplexMatrix);

  FloatComplexMatrix result (nr, nc);

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      {
        octave_quit ();
        result(i, j) = xmin (c, m(i, j));
      }

  return result;
}

FloatComplexMatrix
min (const FloatComplexMatrix& m, const FloatComplex& c)
{
  octave_idx_type nr = m.rows ();
  octave_idx_type nc = m.columns ();

  EMPTY_RETURN_CHECK (FloatComplexMatrix);

  FloatComplexMatrix result (nr, nc);

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      {
        octave_quit ();
        result(i, j) = xmin (m(i, j), c);
      }

  return result;
}

FloatComplexMatrix
min (const FloatComplexMatrix& a, const FloatComplexMatrix& b)
{
  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.columns ();

  if (nr != b.rows () || nc != b.columns ())
    {
      (*current_liboctave_error_handler)
        ("two-arg min expecting args of same size");
      return FloatComplexMatrix ();
    }

  EMPTY_RETURN_CHECK (FloatComplexMatrix);

  FloatComplexMatrix result (nr, nc);

  for (octave_idx_type j = 0; j < nc; j++)
    {
      int columns_are_real_only = 1;
      for (octave_idx_type i = 0; i < nr; i++)
        {
          octave_quit ();
          if (std::imag (a(i, j)) != 0.0 || std::imag (b(i, j)) != 0.0)
            {
              columns_are_real_only = 0;
              break;
            }
        }

      if (columns_are_real_only)
        {
          for (octave_idx_type i = 0; i < nr; i++)
            result(i, j) = xmin (std::real (a(i, j)), std::real (b(i, j)));
        }
      else
        {
          for (octave_idx_type i = 0; i < nr; i++)
            {
              octave_quit ();
              result(i, j) = xmin (a(i, j), b(i, j));
            }
        }
    }

  return result;
}

FloatComplexMatrix
max (const FloatComplex& c, const FloatComplexMatrix& m)
{
  octave_idx_type nr = m.rows ();
  octave_idx_type nc = m.columns ();

  EMPTY_RETURN_CHECK (FloatComplexMatrix);

  FloatComplexMatrix result (nr, nc);

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      {
        octave_quit ();
        result(i, j) = xmax (c, m(i, j));
      }

  return result;
}

FloatComplexMatrix
max (const FloatComplexMatrix& m, const FloatComplex& c)
{
  octave_idx_type nr = m.rows ();
  octave_idx_type nc = m.columns ();

  EMPTY_RETURN_CHECK (FloatComplexMatrix);

  FloatComplexMatrix result (nr, nc);

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      {
        octave_quit ();
        result(i, j) = xmax (m(i, j), c);
      }

  return result;
}

FloatComplexMatrix
max (const FloatComplexMatrix& a, const FloatComplexMatrix& b)
{
  octave_idx_type nr = a.rows ();
  octave_idx_type nc = a.columns ();

  if (nr != b.rows () || nc != b.columns ())
    {
      (*current_liboctave_error_handler)
        ("two-arg max expecting args of same size");
      return FloatComplexMatrix ();
    }

  EMPTY_RETURN_CHECK (FloatComplexMatrix);

  FloatComplexMatrix result (nr, nc);

  for (octave_idx_type j = 0; j < nc; j++)
    {
      int columns_are_real_only = 1;
      for (octave_idx_type i = 0; i < nr; i++)
        {
          octave_quit ();
          if (std::imag (a(i, j)) != 0.0 || std::imag (b(i, j)) != 0.0)
            {
              columns_are_real_only = 0;
              break;
            }
        }

      if (columns_are_real_only)
        {
          for (octave_idx_type i = 0; i < nr; i++)
            {
              octave_quit ();
              result(i, j) = xmax (std::real (a(i, j)), std::real (b(i, j)));
            }
        }
      else
        {
          for (octave_idx_type i = 0; i < nr; i++)
            {
              octave_quit ();
              result(i, j) = xmax (a(i, j), b(i, j));
            }
        }
    }

  return result;
}

FloatComplexMatrix linspace (const FloatComplexColumnVector& x1,
                             const FloatComplexColumnVector& x2,
                             octave_idx_type n)

{
  if (n < 1) n = 1;

  octave_idx_type m = x1.length ();

  if (x2.length () != m)
    (*current_liboctave_error_handler)
      ("linspace: vectors must be of equal length");

  NoAlias<FloatComplexMatrix> retval;

  retval.clear (m, n);
  for (octave_idx_type i = 0; i < m; i++)
    retval(i, 0) = x1(i);

  // The last column is not needed while using delta.
  FloatComplex *delta = &retval(0, n-1);
  for (octave_idx_type i = 0; i < m; i++)
    delta[i] = (x2(i) - x1(i)) / (n - 1.0f);

  for (octave_idx_type j = 1; j < n-1; j++)
    for (octave_idx_type i = 0; i < m; i++)
      retval(i, j) = x1(i) + static_cast<float> (j)*delta[i];

  for (octave_idx_type i = 0; i < m; i++)
    retval(i, n-1) = x2(i);

  return retval;
}

MS_CMP_OPS (FloatComplexMatrix, FloatComplex)
MS_BOOL_OPS (FloatComplexMatrix, FloatComplex)

SM_CMP_OPS (FloatComplex, FloatComplexMatrix)
SM_BOOL_OPS (FloatComplex, FloatComplexMatrix)

MM_CMP_OPS (FloatComplexMatrix, FloatComplexMatrix)
MM_BOOL_OPS (FloatComplexMatrix, FloatComplexMatrix)
