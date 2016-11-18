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

#if !defined (octave_SparseCmplxQR_h)
#define octave_SparseCmplxQR_h 1

#include <iosfwd>

#include "dMatrix.h"
#include "CMatrix.h"
#include "dSparse.h"
#include "CSparse.h"
#include "oct-sparse.h"

#ifdef USE_64_BIT_IDX_T
#define CXSPARSE_ZNAME(name) cs_cl ## name
#else
#define CXSPARSE_ZNAME(name) cs_ci ## name
#endif

class
OCTAVE_API
SparseComplexQR
{
protected:
  class SparseComplexQR_rep
  {
  public:
    SparseComplexQR_rep (const SparseComplexMatrix& a, int order);

    ~SparseComplexQR_rep (void);
#ifdef HAVE_CXSPARSE
    bool ok (void) const { return (N && S); }
#else
    bool ok (void) const { return false; }
#endif
    SparseComplexMatrix V (void) const;

    ColumnVector Pinv (void) const;

    ColumnVector P (void) const;

    SparseComplexMatrix R (const bool econ) const;

    ComplexMatrix C (const ComplexMatrix &b) const;

    ComplexMatrix Q (void) const;

    octave_refcount<int> count;

    octave_idx_type nrows;
#ifdef HAVE_CXSPARSE
    CXSPARSE_ZNAME (s) *S;

    CXSPARSE_ZNAME (n) *N;
#endif
  private:

    // No copying!

    SparseComplexQR_rep (const SparseComplexQR_rep&);

    SparseComplexQR_rep operator = (const SparseComplexQR_rep&);

  };
private:
  SparseComplexQR_rep *rep;

public:
  SparseComplexQR (void) :
    rep (new SparseComplexQR_rep (SparseComplexMatrix (), 0)) { }

  SparseComplexQR (const SparseComplexMatrix& a, int order = 0) :
    rep (new SparseComplexQR_rep (a, order)) { }

  SparseComplexQR (const SparseComplexQR& a) : rep (a.rep) { rep->count++; }

  ~SparseComplexQR (void)
  {
    if (--rep->count == 0)
      delete rep;
  }

  SparseComplexQR& operator = (const SparseComplexQR& a)
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

  SparseComplexMatrix V (void) const { return rep->V (); }

  ColumnVector Pinv (void) const { return rep->P (); }

  ColumnVector P (void) const { return rep->P (); }

  SparseComplexMatrix R (const bool econ = false) const
  { return rep->R(econ); }

  ComplexMatrix C (const ComplexMatrix &b) const { return rep->C(b); }

  ComplexMatrix Q (void) const { return rep->Q (); }

  friend ComplexMatrix qrsolve (const SparseComplexMatrix &a, const Matrix &b,
                                octave_idx_type &info);

  friend SparseComplexMatrix qrsolve (const SparseComplexMatrix &a,
                                      const SparseMatrix &b,
                                      octave_idx_type &info);

  friend ComplexMatrix qrsolve (const SparseComplexMatrix &a,
                                const ComplexMatrix &b,
                                octave_idx_type &info);

  friend SparseComplexMatrix qrsolve (const SparseComplexMatrix &a,
                                      const SparseComplexMatrix &b,
                                      octave_idx_type &info);

protected:
#ifdef HAVE_CXSPARSE
  CXSPARSE_ZNAME (s) * S (void) { return rep->S; }

  CXSPARSE_ZNAME (n) * N (void) { return rep->N; }
#endif
};


// Publish externally used friend functions.

extern ComplexMatrix qrsolve (const SparseComplexMatrix &a, const Matrix &b,
                              octave_idx_type &info);

extern ComplexMatrix qrsolve (const SparseComplexMatrix &a,
                              const MArray<double> &b,
                              octave_idx_type &info);

extern SparseComplexMatrix qrsolve (const SparseComplexMatrix &a,
                                    const SparseMatrix &b,
                                    octave_idx_type &info);

extern ComplexMatrix qrsolve (const SparseComplexMatrix &a,
                              const ComplexMatrix &b,
                              octave_idx_type &info);

extern ComplexMatrix qrsolve (const SparseComplexMatrix &a,
                              const MArray<Complex> &b,
                              octave_idx_type &info);

extern SparseComplexMatrix qrsolve (const SparseComplexMatrix &a,
                                    const SparseComplexMatrix &b,
                                    octave_idx_type &info);
#endif
