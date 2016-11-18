/*

Copyright (C) 2004-2015 David Bateman
Copyright (C) 1998-2004 Andy Adler

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

#if !defined (octave_CSparse_h)
#define octave_CSparse_h 1

#include "dMatrix.h"
#include "dNDArray.h"
#include "CMatrix.h"
#include "CNDArray.h"
#include "dColVector.h"
#include "CColVector.h"
#include "oct-cmplx.h"

#include "DET.h"
#include "MSparse.h"
#include "MSparse-defs.h"

#include "Sparse-op-decls.h"

#include "MatrixType.h"

class PermMatrix;
class DiagMatrix;
class ComplexDiagMatrix;
class SparseMatrix;
class SparseBoolMatrix;

class
OCTAVE_API
SparseComplexMatrix : public MSparse<Complex>
{
public:

  typedef void (*solve_singularity_handler) (double rcond);

  SparseComplexMatrix (void) : MSparse<Complex> () { }

  SparseComplexMatrix (octave_idx_type r,
                       octave_idx_type c) : MSparse<Complex> (r, c) { }

  SparseComplexMatrix (const dim_vector& dv, octave_idx_type nz = 0)
    : MSparse<Complex> (dv, nz) { }

  explicit SparseComplexMatrix (octave_idx_type r, octave_idx_type c,
                                Complex val)
    : MSparse<Complex> (r, c, val) { }

  SparseComplexMatrix (octave_idx_type r, octave_idx_type c, double val)
    : MSparse<Complex> (r, c, Complex (val)) { }

  SparseComplexMatrix (const SparseComplexMatrix& a)
    : MSparse<Complex> (a) { }

  SparseComplexMatrix (const SparseComplexMatrix& a, const dim_vector& dv)
    : MSparse<Complex> (a, dv) { }

  SparseComplexMatrix (const MSparse<Complex>& a) : MSparse<Complex> (a) { }

  SparseComplexMatrix (const Sparse<Complex>& a) : MSparse<Complex> (a) { }

  explicit SparseComplexMatrix (const ComplexMatrix& a)
    : MSparse<Complex> (a) { }

  explicit SparseComplexMatrix (const ComplexNDArray& a)
    : MSparse<Complex> (a) { }

  SparseComplexMatrix (const Array<Complex>& a, const idx_vector& r,
                       const idx_vector& c, octave_idx_type nr = -1,
                       octave_idx_type nc = -1, bool sum_terms = true,
                       octave_idx_type nzm = -1)
    : MSparse<Complex> (a, r, c, nr, nc, sum_terms, nzm) { }

  explicit SparseComplexMatrix (const SparseMatrix& a);

  explicit SparseComplexMatrix (const SparseBoolMatrix& a);

  explicit SparseComplexMatrix (const ComplexDiagMatrix& a);

  SparseComplexMatrix (octave_idx_type r, octave_idx_type c,
                       octave_idx_type num_nz)
    : MSparse<Complex> (r, c, num_nz) { }

  SparseComplexMatrix& operator = (const SparseComplexMatrix& a)
  {
    MSparse<Complex>::operator = (a);
    return *this;
  }

  bool operator == (const SparseComplexMatrix& a) const;
  bool operator != (const SparseComplexMatrix& a) const;

  bool is_hermitian (void) const;

  SparseComplexMatrix max (int dim = -1) const;
  SparseComplexMatrix max (Array<octave_idx_type>& index, int dim = -1) const;
  SparseComplexMatrix min (int dim = -1) const;
  SparseComplexMatrix min (Array<octave_idx_type>& index, int dim = -1) const;

  SparseComplexMatrix& insert (const SparseComplexMatrix& a,
                               octave_idx_type r, octave_idx_type c);
  SparseComplexMatrix& insert (const SparseMatrix& a,
                               octave_idx_type r, octave_idx_type c);
  SparseComplexMatrix& insert (const SparseComplexMatrix& a,
                               const Array<octave_idx_type>& indx);
  SparseComplexMatrix& insert (const SparseMatrix& a,
                               const Array<octave_idx_type>& indx);

  SparseComplexMatrix concat (const SparseComplexMatrix& rb,
                              const Array<octave_idx_type>& ra_idx);
  SparseComplexMatrix concat (const SparseMatrix& rb,
                              const Array<octave_idx_type>& ra_idx);

  ComplexMatrix matrix_value (void) const;

  SparseComplexMatrix hermitian (void) const;  // complex conjugate transpose
  SparseComplexMatrix transpose (void) const
  { return MSparse<Complex>::transpose (); }

  friend SparseComplexMatrix conj (const SparseComplexMatrix& a);

  // extract row or column i.

  ComplexRowVector row (octave_idx_type i) const;

  ComplexColumnVector column (octave_idx_type i) const;

private:
  SparseComplexMatrix dinverse (MatrixType &mattyp, octave_idx_type& info,
                                double& rcond, const bool force = false,
                                const bool calccond = true) const;

  SparseComplexMatrix tinverse (MatrixType &mattyp, octave_idx_type& info,
                                double& rcond, const bool force = false,
                                const bool calccond = true) const;

public:
  SparseComplexMatrix inverse (void) const;
  SparseComplexMatrix inverse (MatrixType& mattype) const;
  SparseComplexMatrix inverse (MatrixType& mattype,
                               octave_idx_type& info) const;
  SparseComplexMatrix inverse (MatrixType& mattype, octave_idx_type& info,
                               double& rcond, int force = 0,
                               int calc_cond = 1) const;

  ComplexDET determinant (void) const;
  ComplexDET determinant (octave_idx_type& info) const;
  ComplexDET determinant (octave_idx_type& info, double& rcond,
                          int calc_cond = 1) const;

private:
  // Diagonal matrix solvers
  ComplexMatrix dsolve (MatrixType &typ, const Matrix& b, octave_idx_type& info,
                        double& rcond, solve_singularity_handler sing_handler,
                        bool calc_cond = false) const;

  ComplexMatrix dsolve (MatrixType &typ, const ComplexMatrix& b,
                        octave_idx_type& info, double& rcond,
                        solve_singularity_handler sing_handler,
                        bool calc_cond = false) const;

  SparseComplexMatrix dsolve (MatrixType &typ, const SparseMatrix& b,
                              octave_idx_type& info, double& rcond,
                              solve_singularity_handler sing_handler,
                              bool calc_cond = false) const;

  SparseComplexMatrix dsolve (MatrixType &typ, const SparseComplexMatrix& b,
                              octave_idx_type& info, double& rcond,
                              solve_singularity_handler sing_handler,
                              bool calc_cond = false) const;

  // Upper triangular matrix solvers
  ComplexMatrix utsolve (MatrixType &typ, const Matrix& b,
                         octave_idx_type& info, double& rcond,
                         solve_singularity_handler sing_handler,
                         bool calc_cond = false) const;

  ComplexMatrix utsolve (MatrixType &typ, const ComplexMatrix& b,
                         octave_idx_type& info, double& rcond,
                         solve_singularity_handler sing_handler,
                         bool calc_cond = false) const;

  SparseComplexMatrix utsolve (MatrixType &typ, const SparseMatrix& b,
                               octave_idx_type& info, double& rcond,
                               solve_singularity_handler sing_handler,
                               bool calc_cond = false) const;

  SparseComplexMatrix utsolve (MatrixType &typ, const SparseComplexMatrix& b,
                               octave_idx_type& info, double& rcond,
                               solve_singularity_handler sing_handler,
                               bool calc_cond = false) const;

  // Lower triangular matrix solvers
  ComplexMatrix ltsolve (MatrixType &typ, const Matrix& b,
                         octave_idx_type& info, double& rcond,
                         solve_singularity_handler sing_handler,
                         bool calc_cond = false) const;

  ComplexMatrix ltsolve (MatrixType &typ, const ComplexMatrix& b,
                         octave_idx_type& info, double& rcond,
                         solve_singularity_handler sing_handler,
                         bool calc_cond = false) const;

  SparseComplexMatrix ltsolve (MatrixType &typ, const SparseMatrix& b,
                               octave_idx_type& info, double& rcond,
                               solve_singularity_handler sing_handler,
                               bool calc_cond = false) const;

  SparseComplexMatrix ltsolve (MatrixType &typ, const SparseComplexMatrix& b,
                               octave_idx_type& info, double& rcond,
                               solve_singularity_handler sing_handler,
                               bool calc_cond = false) const;

  // Tridiagonal matrix solvers
  ComplexMatrix trisolve (MatrixType &typ, const Matrix& b,
                          octave_idx_type& info, double& rcond,
                          solve_singularity_handler sing_handler,
                          bool calc_cond = false) const;

  ComplexMatrix trisolve (MatrixType &typ, const ComplexMatrix& b,
                          octave_idx_type& info, double& rcond,
                          solve_singularity_handler sing_handler,
                          bool calc_cond = false) const;

  SparseComplexMatrix trisolve (MatrixType &typ, const SparseMatrix& b,
                                octave_idx_type& info, double& rcond,
                                solve_singularity_handler sing_handler,
                                bool calc_cond = false) const;

  SparseComplexMatrix trisolve (MatrixType &typ, const SparseComplexMatrix& b,
                                octave_idx_type& info, double& rcond,
                                solve_singularity_handler sing_handler,
                                bool calc_cond = false) const;

  // Banded matrix solvers (umfpack/cholesky)
  ComplexMatrix bsolve (MatrixType &typ, const Matrix& b,
                        octave_idx_type& info, double& rcond,
                        solve_singularity_handler sing_handler,
                        bool calc_cond = false) const;

  ComplexMatrix bsolve (MatrixType &typ, const ComplexMatrix& b,
                        octave_idx_type& info, double& rcond,
                        solve_singularity_handler sing_handler,
                        bool calc_cond = false) const;

  SparseComplexMatrix bsolve (MatrixType &typ, const SparseMatrix& b,
                              octave_idx_type& info, double& rcond,
                              solve_singularity_handler sing_handler,
                              bool calc_cond = false) const;

  SparseComplexMatrix bsolve (MatrixType &typ, const SparseComplexMatrix& b,
                              octave_idx_type& info, double& rcond,
                              solve_singularity_handler sing_handler,
                              bool calc_cond = false) const;

  // Full matrix solvers (umfpack/cholesky)
  void * factorize (octave_idx_type& err, double &rcond, Matrix &Control,
                    Matrix &Info, solve_singularity_handler sing_handler,
                    bool calc_cond) const;

  ComplexMatrix fsolve (MatrixType &typ, const Matrix& b,
                        octave_idx_type& info, double& rcond,
                        solve_singularity_handler sing_handler,
                        bool calc_cond = false) const;

  ComplexMatrix fsolve (MatrixType &typ, const ComplexMatrix& b,
                        octave_idx_type& info, double& rcond,
                        solve_singularity_handler sing_handler,
                        bool calc_cond = false) const;

  SparseComplexMatrix fsolve (MatrixType &typ, const SparseMatrix& b,
                              octave_idx_type& info, double& rcond,
                              solve_singularity_handler sing_handler,
                              bool calc_cond = false) const;

  SparseComplexMatrix fsolve (MatrixType &typ, const SparseComplexMatrix& b,
                              octave_idx_type& info, double& rcond,
                              solve_singularity_handler sing_handler,
                              bool calc_cond = false) const;

public:
  // Generic interface to solver with no probing of type
  ComplexMatrix solve (MatrixType &typ, const Matrix& b) const;
  ComplexMatrix solve (MatrixType &typ, const Matrix& b,
                       octave_idx_type& info) const;
  ComplexMatrix solve (MatrixType &typ, const Matrix& b, octave_idx_type& info,
                       double& rcond) const;
  ComplexMatrix solve (MatrixType &typ, const Matrix& b, octave_idx_type& info,
                       double& rcond, solve_singularity_handler sing_handler,
                       bool singular_fallback = true) const;

  ComplexMatrix solve (MatrixType &typ, const ComplexMatrix& b) const;
  ComplexMatrix solve (MatrixType &typ, const ComplexMatrix& b,
                       octave_idx_type& info) const;
  ComplexMatrix solve (MatrixType &typ, const ComplexMatrix& b,
                       octave_idx_type& info, double& rcond) const;
  ComplexMatrix solve (MatrixType &typ, const ComplexMatrix& b,
                       octave_idx_type& info, double& rcond,
                       solve_singularity_handler sing_handler,
                       bool singular_fallback = true) const;

  SparseComplexMatrix solve (MatrixType &typ, const SparseMatrix& b) const;
  SparseComplexMatrix solve (MatrixType &typ, const SparseMatrix& b,
                             octave_idx_type& info) const;
  SparseComplexMatrix solve (MatrixType &typ, const SparseMatrix& b,
                             octave_idx_type& info, double& rcond) const;
  SparseComplexMatrix solve (MatrixType &typ, const SparseMatrix& b,
                             octave_idx_type& info, double& rcond,
                             solve_singularity_handler sing_handler,
                             bool singular_fallback = true) const;

  SparseComplexMatrix solve (MatrixType &typ,
                             const SparseComplexMatrix& b) const;
  SparseComplexMatrix solve (MatrixType &typ, const SparseComplexMatrix& b,
                             octave_idx_type& info) const;
  SparseComplexMatrix solve (MatrixType &typ, const SparseComplexMatrix& b,
                             octave_idx_type& info, double& rcond) const;
  SparseComplexMatrix solve (MatrixType &typ, const SparseComplexMatrix& b,
                             octave_idx_type& info, double& rcond,
                             solve_singularity_handler sing_handler,
                             bool singular_fallback = true) const;

  ComplexColumnVector solve (MatrixType &typ, const ColumnVector& b) const;
  ComplexColumnVector solve (MatrixType &typ, const ColumnVector& b,
                             octave_idx_type& info) const;
  ComplexColumnVector solve (MatrixType &typ, const ColumnVector& b,
                             octave_idx_type& info, double& rcond) const;
  ComplexColumnVector solve (MatrixType &typ, const ColumnVector& b,
                             octave_idx_type& info, double& rcond,
                             solve_singularity_handler sing_handler) const;

  ComplexColumnVector solve (MatrixType &typ,
                             const ComplexColumnVector& b) const;
  ComplexColumnVector solve (MatrixType &typ, const ComplexColumnVector& b,
                             octave_idx_type& info) const;
  ComplexColumnVector solve (MatrixType &typ, const ComplexColumnVector& b,
                             octave_idx_type& info, double& rcond) const;
  ComplexColumnVector solve (MatrixType &typ, const ComplexColumnVector& b,
                             octave_idx_type& info, double& rcond,
                             solve_singularity_handler sing_handler) const;

  // Generic interface to solver with probing of type
  ComplexMatrix solve (const Matrix& b) const;
  ComplexMatrix solve (const Matrix& b, octave_idx_type& info) const;
  ComplexMatrix solve (const Matrix& b, octave_idx_type& info,
                       double& rcond) const;
  ComplexMatrix solve (const Matrix& b, octave_idx_type& info, double& rcond,
                       solve_singularity_handler sing_handler) const;

  ComplexMatrix solve (const ComplexMatrix& b) const;
  ComplexMatrix solve (const ComplexMatrix& b, octave_idx_type& info) const;
  ComplexMatrix solve (const ComplexMatrix& b, octave_idx_type& info,
                       double& rcond) const;
  ComplexMatrix solve (const ComplexMatrix& b, octave_idx_type& info,
                       double& rcond,
                       solve_singularity_handler sing_handler) const;

  SparseComplexMatrix solve (const SparseMatrix& b) const;
  SparseComplexMatrix solve (const SparseMatrix& b,
                             octave_idx_type& info) const;
  SparseComplexMatrix solve (const SparseMatrix& b, octave_idx_type& info,
                             double& rcond) const;
  SparseComplexMatrix solve (const SparseMatrix& b, octave_idx_type& info,
                             double& rcond,
                             solve_singularity_handler sing_handler) const;

  SparseComplexMatrix solve (const SparseComplexMatrix& b) const;
  SparseComplexMatrix solve (const SparseComplexMatrix& b,
                             octave_idx_type& info) const;
  SparseComplexMatrix solve (const SparseComplexMatrix& b,
                             octave_idx_type& info, double& rcond) const;
  SparseComplexMatrix solve (const SparseComplexMatrix& b,
                             octave_idx_type& info, double& rcond,
                             solve_singularity_handler sing_handler) const;

  ComplexColumnVector solve (const ColumnVector& b) const;
  ComplexColumnVector solve (const ColumnVector& b,
                             octave_idx_type& info) const;
  ComplexColumnVector solve (const ColumnVector& b, octave_idx_type& info,
                             double& rcond) const;
  ComplexColumnVector solve (const ColumnVector& b, octave_idx_type& info,
                             double& rcond,
                             solve_singularity_handler sing_handler) const;

  ComplexColumnVector solve (const ComplexColumnVector& b) const;
  ComplexColumnVector solve (const ComplexColumnVector& b,
                             octave_idx_type& info) const;
  ComplexColumnVector solve (const ComplexColumnVector& b,
                             octave_idx_type& info, double& rcond) const;
  ComplexColumnVector solve (const ComplexColumnVector& b,
                             octave_idx_type& info, double& rcond,
                             solve_singularity_handler sing_handler) const;

  SparseComplexMatrix squeeze (void) const;

  SparseComplexMatrix reshape (const dim_vector& new_dims) const;

  SparseComplexMatrix permute (const Array<octave_idx_type>& vec,
                               bool inv = false) const;

  SparseComplexMatrix ipermute (const Array<octave_idx_type>& vec) const;

  bool any_element_is_nan (void) const;
  bool any_element_is_inf_or_nan (void) const;
  bool all_elements_are_real (void) const;
  bool all_integers (double& max_val, double& min_val) const;
  bool too_large_for_float (void) const;

  SparseBoolMatrix operator ! (void) const;

  SparseBoolMatrix all (int dim = -1) const;
  SparseBoolMatrix any (int dim = -1) const;

  SparseComplexMatrix cumprod (int dim = -1) const;
  SparseComplexMatrix cumsum (int dim = -1) const;
  SparseComplexMatrix prod (int dim = -1) const;
  SparseComplexMatrix sum (int dim = -1) const;
  SparseComplexMatrix sumsq (int dim = -1) const;
  SparseMatrix abs (void) const;

  SparseComplexMatrix diag (octave_idx_type k = 0) const;

  // i/o
  friend OCTAVE_API std::ostream& operator << (std::ostream& os,
                                               const SparseComplexMatrix& a);
  friend OCTAVE_API std::istream& operator >> (std::istream& is,
                                               SparseComplexMatrix& a);
};

extern OCTAVE_API SparseComplexMatrix operator * (const SparseMatrix&,
                                                  const SparseComplexMatrix&);
extern OCTAVE_API SparseComplexMatrix operator * (const SparseComplexMatrix&,
                                                  const SparseMatrix&);
extern OCTAVE_API SparseComplexMatrix operator * (const SparseComplexMatrix&,
                                                  const SparseComplexMatrix&);

extern OCTAVE_API ComplexMatrix operator * (const Matrix&,
                                            const SparseComplexMatrix&);
extern OCTAVE_API ComplexMatrix operator * (const ComplexMatrix&,
                                            const SparseMatrix&);
extern OCTAVE_API ComplexMatrix operator * (const ComplexMatrix&,
                                            const SparseComplexMatrix&);
extern OCTAVE_API ComplexMatrix mul_trans (const ComplexMatrix&,
                                           const SparseComplexMatrix&);
extern OCTAVE_API ComplexMatrix mul_herm (const ComplexMatrix&,
                                          const SparseComplexMatrix&);

extern OCTAVE_API ComplexMatrix operator * (const SparseMatrix&,
                                            const ComplexMatrix&);
extern OCTAVE_API ComplexMatrix operator * (const SparseComplexMatrix&,
                                            const Matrix&);
extern OCTAVE_API ComplexMatrix operator * (const SparseComplexMatrix&,
                                            const ComplexMatrix&);
extern OCTAVE_API ComplexMatrix trans_mul (const SparseComplexMatrix&,
                                           const ComplexMatrix&);
extern OCTAVE_API ComplexMatrix herm_mul (const SparseComplexMatrix&,
                                          const ComplexMatrix&);

extern OCTAVE_API SparseComplexMatrix operator * (const DiagMatrix&,
                                                  const SparseComplexMatrix&);
extern OCTAVE_API SparseComplexMatrix operator * (const SparseComplexMatrix&,
                                                  const DiagMatrix&);

extern OCTAVE_API SparseComplexMatrix operator * (const ComplexDiagMatrix&,
                                                  const SparseMatrix&);
extern OCTAVE_API SparseComplexMatrix operator * (const SparseMatrix&,
                                                  const ComplexDiagMatrix&);

extern OCTAVE_API SparseComplexMatrix operator * (const ComplexDiagMatrix&,
                                                  const SparseComplexMatrix&);
extern OCTAVE_API SparseComplexMatrix operator * (const SparseComplexMatrix&,
                                                  const ComplexDiagMatrix&);

extern OCTAVE_API SparseComplexMatrix operator + (const ComplexDiagMatrix&,
                                                  const SparseMatrix&);
extern OCTAVE_API SparseComplexMatrix operator + (const DiagMatrix&,
                                                  const SparseComplexMatrix&);
extern OCTAVE_API SparseComplexMatrix operator + (const ComplexDiagMatrix&,
                                                  const SparseComplexMatrix&);
extern OCTAVE_API SparseComplexMatrix operator + (const SparseMatrix&,
                                                  const ComplexDiagMatrix&);
extern OCTAVE_API SparseComplexMatrix operator + (const SparseComplexMatrix&,
                                                  const DiagMatrix&);
extern OCTAVE_API SparseComplexMatrix operator + (const SparseComplexMatrix&,
                                                  const ComplexDiagMatrix&);

extern OCTAVE_API SparseComplexMatrix operator - (const ComplexDiagMatrix&,
                                                  const SparseMatrix&);
extern OCTAVE_API SparseComplexMatrix operator - (const DiagMatrix&,
                                                  const SparseComplexMatrix&);
extern OCTAVE_API SparseComplexMatrix operator - (const ComplexDiagMatrix&,
                                                  const SparseComplexMatrix&);
extern OCTAVE_API SparseComplexMatrix operator - (const SparseMatrix&,
                                                  const ComplexDiagMatrix&);
extern OCTAVE_API SparseComplexMatrix operator - (const SparseComplexMatrix&,
                                                  const DiagMatrix&);
extern OCTAVE_API SparseComplexMatrix operator - (const SparseComplexMatrix&,
                                                  const ComplexDiagMatrix&);

extern OCTAVE_API SparseComplexMatrix operator * (const PermMatrix&,
                                                  const SparseComplexMatrix&);
extern OCTAVE_API SparseComplexMatrix operator * (const SparseComplexMatrix&,
                                                  const PermMatrix&);

extern OCTAVE_API SparseComplexMatrix min (const Complex& c,
                                           const SparseComplexMatrix& m);
extern OCTAVE_API SparseComplexMatrix min (const SparseComplexMatrix& m,
                                           const Complex& c);
extern OCTAVE_API SparseComplexMatrix min (const SparseComplexMatrix& a,
                                           const SparseComplexMatrix& b);

extern OCTAVE_API SparseComplexMatrix max (const Complex& c,
                                           const SparseComplexMatrix& m);
extern OCTAVE_API SparseComplexMatrix max (const SparseComplexMatrix& m,
                                           const Complex& c);
extern OCTAVE_API SparseComplexMatrix max (const SparseComplexMatrix& a,
                                           const SparseComplexMatrix& b);

SPARSE_SMS_CMP_OP_DECLS (SparseComplexMatrix, Complex, OCTAVE_API)
SPARSE_SMS_BOOL_OP_DECLS (SparseComplexMatrix, Complex, OCTAVE_API)

SPARSE_SSM_CMP_OP_DECLS (Complex, SparseComplexMatrix, OCTAVE_API)
SPARSE_SSM_BOOL_OP_DECLS (Complex, SparseComplexMatrix, OCTAVE_API)

SPARSE_SMSM_CMP_OP_DECLS (SparseComplexMatrix, SparseComplexMatrix, OCTAVE_API)
SPARSE_SMSM_BOOL_OP_DECLS (SparseComplexMatrix, SparseComplexMatrix, OCTAVE_API)

SPARSE_FORWARD_DEFS (MSparse, SparseComplexMatrix, ComplexMatrix, Complex)

#ifdef USE_64_BIT_IDX_T
#define UMFPACK_ZNAME(name) umfpack_zl_ ## name
#else
#define UMFPACK_ZNAME(name) umfpack_zi_ ## name
#endif

#endif
