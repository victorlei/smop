/*

Copyright (C) 2005-2015 David Bateman

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

#if !defined (octave_SparseQR_h)
#define octave_SparseQR_h 1

#include <iosfwd>

#include "dMatrix.h"
#include "CMatrix.h"
#include "dSparse.h"
#include "CSparse.h"
#include "oct-sparse.h"

#ifdef USE_64_BIT_IDX_T
#define CXSPARSE_DNAME(name) cs_dl ## name
#else
#define CXSPARSE_DNAME(name) cs_di ## name
#endif

class
OCTAVE_API
SparseQR
{
protected:
  class SparseQR_rep
  {
  public:
    SparseQR_rep (const SparseMatrix& a, int order);

    ~SparseQR_rep (void);
#ifdef HAVE_CXSPARSE
    bool ok (void) const { return (N && S); }
#else
    bool ok (void) const { return false; }
#endif
    SparseMatrix V (void) const;

    ColumnVector Pinv (void) const;

    ColumnVector P (void) const;

    SparseMatrix R (const bool econ) const;

    Matrix C (const Matrix &b) const;

    Matrix Q (void) const;

    octave_refcount<int> count;

    octave_idx_type nrows;
#ifdef HAVE_CXSPARSE
    CXSPARSE_DNAME (s) *S;

    CXSPARSE_DNAME (n) *N;
#endif

  private:

    // No copying!

    SparseQR_rep (const SparseQR_rep&);

    SparseQR_rep& operator = (const SparseQR_rep&);
  };

private:

  SparseQR_rep *rep;

public:

  SparseQR (void) : rep (new SparseQR_rep (SparseMatrix (), 0)) { }

  SparseQR (const SparseMatrix& a, int order = 0) :
    rep (new SparseQR_rep (a, order)) { }

  SparseQR (const SparseQR& a) : rep (a.rep) { rep->count++; }

  ~SparseQR (void)
  {
    if (--rep->count == 0)
      delete rep;
  }

  SparseQR& operator = (const SparseQR& a)
  {
    if (this != &a)
      {
        if (--rep->count == 0)
          delete rep;

        rep = a.rep;
        rep->count++;
      }
    return *this;
  }

  bool ok (void) const { return rep->ok (); }

  SparseMatrix V (void) const { return rep->V (); }

  ColumnVector Pinv (void) const { return rep->P (); }

  ColumnVector P (void) const { return rep->P (); }

  SparseMatrix R (const bool econ = false) const { return rep->R(econ); }

  Matrix C (const Matrix &b) const { return rep->C(b); }

  Matrix Q (void) const { return rep->Q (); }

  friend Matrix qrsolve (const SparseMatrix &a, const Matrix &b,
                         octave_idx_type &info);

  friend SparseMatrix qrsolve (const SparseMatrix &a, const SparseMatrix &b,
                               octave_idx_type &info);

  friend ComplexMatrix qrsolve (const SparseMatrix &a, const ComplexMatrix &b,
                                octave_idx_type &info);

  friend SparseComplexMatrix qrsolve (const SparseMatrix &a,
                                      const SparseComplexMatrix &b,
                                      octave_idx_type &info);

protected:
#ifdef HAVE_CXSPARSE
  CXSPARSE_DNAME (s) * S (void) { return rep->S; }

  CXSPARSE_DNAME (n) * N (void) { return rep->N; }
#endif
};


// Publish externally used friend functions.

extern Matrix qrsolve (const SparseMatrix &a, const Matrix &b,
                       octave_idx_type &info);

extern Matrix qrsolve (const SparseMatrix &a, const MArray<double> &b,
                       octave_idx_type &info);

extern SparseMatrix qrsolve (const SparseMatrix &a, const SparseMatrix &b,
                             octave_idx_type &info);

extern ComplexMatrix qrsolve (const SparseMatrix &a, const ComplexMatrix &b,
                              octave_idx_type &info);

extern ComplexMatrix qrsolve (const SparseMatrix &a, const MArray<Complex> &b,
                              octave_idx_type &info);

extern SparseComplexMatrix qrsolve (const SparseMatrix &a,
                                    const SparseComplexMatrix &b,
                                    octave_idx_type &info);

#endif
