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

#if !defined (octave_dSparse_h)
#define octave_dSparse_h 1

#include "dMatrix.h"
#include "dNDArray.h"
#include "CMatrix.h"
#include "dColVector.h"
#include "CColVector.h"

#include "DET.h"
#include "MSparse.h"
#include "MSparse-defs.h"

#include "Sparse-op-decls.h"

#include "MatrixType.h"

class PermMatrix;
class DiagMatrix;
class SparseComplexMatrix;
class SparseBoolMatrix;

class
OCTAVE_API
SparseMatrix : public MSparse<double>
{
public:

  typedef void (*solve_singularity_handler) (double rcond);

  SparseMatrix (void) : MSparse<double> () { }

  SparseMatrix (octave_idx_type r, octave_idx_type c)
    : MSparse<double> (r, c) { }

  SparseMatrix (const dim_vector& dv, octave_idx_type nz = 0) :
    MSparse<double> (dv, nz) { }

  explicit SparseMatrix (octave_idx_type r, octave_idx_type c, double val)
    : MSparse<double> (r, c, val) { }

  SparseMatrix (const SparseMatrix& a) : MSparse<double> (a) { }

  SparseMatrix (const SparseMatrix& a, const dim_vector& dv)
    : MSparse<double> (a, dv) { }

  SparseMatrix (const MSparse<double>& a) : MSparse<double> (a) { }

  SparseMatrix (const Sparse<double>& a) : MSparse<double> (a) { }

  explicit SparseMatrix (const SparseBoolMatrix& a);

  explicit SparseMatrix (const Matrix& a) : MSparse<double> (a) { }

  explicit SparseMatrix (const NDArray& a) : MSparse<double> (a) { }

  SparseMatrix (const Array<double>& a, const idx_vector& r,
                const idx_vector& c, octave_idx_type nr = -1,
                octave_idx_type nc = -1, bool sum_terms = true,
                octave_idx_type nzm = -1)
    : MSparse<double> (a, r, c, nr, nc, sum_terms, nzm) { }

  explicit SparseMatrix (const DiagMatrix& a);

  explicit SparseMatrix (const PermMatrix& a) : MSparse<double>(a) { }

  SparseMatrix (octave_idx_type r, octave_idx_type c,
                octave_idx_type num_nz) : MSparse<double> (r, c, num_nz) { }

  SparseMatrix& operator = (const SparseMatrix& a)
  {
    MSparse<double>::operator = (a);
    return *this;
  }

  bool operator == (const SparseMatrix& a) const;
  bool operator != (const SparseMatrix& a) const;

  bool is_symmetric (void) const;

  SparseMatrix max (int dim = -1) const;
  SparseMatrix max (Array<octave_idx_type>& index, int dim = -1) const;
  SparseMatrix min (int dim = -1) const;
  SparseMatrix min (Array<octave_idx_type>& index, int dim = -1) const;

  // destructive insert/delete/reorder operations

  SparseMatrix& insert (const SparseMatrix& a, octave_idx_type r,
                        octave_idx_type c);

  SparseMatrix& insert (const SparseMatrix& a,
                        const Array<octave_idx_type>& indx);

  SparseMatrix concat (const SparseMatrix& rb,
                       const Array<octave_idx_type>& ra_idx);
  SparseComplexMatrix concat (const SparseComplexMatrix& rb,
                              const Array<octave_idx_type>& ra_idx);

  friend OCTAVE_API SparseMatrix real (const SparseComplexMatrix& a);
  friend OCTAVE_API SparseMatrix imag (const SparseComplexMatrix& a);

  friend OCTAVE_API SparseMatrix atan2 (const double& x, const SparseMatrix& y)
                                        GCC_ATTR_DEPRECATED ;
  friend OCTAVE_API SparseMatrix atan2 (const SparseMatrix& x, const double& y)
                                        GCC_ATTR_DEPRECATED ;
  friend OCTAVE_API SparseMatrix atan2 (const SparseMatrix& x,
                                        const SparseMatrix& y)
                                        GCC_ATTR_DEPRECATED ;

  SparseMatrix transpose (void) const
  {
    return MSparse<double>::transpose ();
  }
  SparseMatrix hermitian (void) const { return transpose (); }

  // extract row or column i.

  RowVector row (octave_idx_type i) const;

  ColumnVector column (octave_idx_type i) const;

private:
  SparseMatrix dinverse (MatrixType &mattyp, octave_idx_type& info,
                         double& rcond, const bool force = false,
                         const bool calccond = true) const;

  SparseMatrix tinverse (MatrixType &mattyp, octave_idx_type& info,
                         double& rcond, const bool force = false,
                         const bool calccond = true) const;

public:
  SparseMatrix inverse (void) const;
  SparseMatrix inverse (MatrixType& mattype) const;
  SparseMatrix inverse (MatrixType& mattype, octave_idx_type& info) const;
  SparseMatrix inverse (MatrixType& mattype, octave_idx_type& info,
                        double& rcond, int force = 0, int calc_cond = 1) const;

  DET determinant (void) const;
  DET determinant (octave_idx_type& info) const;
  DET determinant (octave_idx_type& info, double& rcond,
                   int calc_cond = 1) const;

private:
  // Diagonal matrix solvers
  Matrix dsolve (MatrixType &typ, const Matrix& b, octave_idx_type& info,
                 double& rcond, solve_singularity_handler sing_handler,
                 bool calc_cond = false) const;

  ComplexMatrix dsolve (MatrixType &typ, const ComplexMatrix& b,
                        octave_idx_type& info, double& rcond,
                        solve_singularity_handler sing_handler,
                        bool calc_cond = false) const;

  SparseMatrix dsolve (MatrixType &typ, const SparseMatrix& b,
                       octave_idx_type& info, double& rcond,
                       solve_singularity_handler sing_handler,
                       bool calc_cond = false) const;

  SparseComplexMatrix dsolve (MatrixType &typ, const SparseComplexMatrix& b,
                              octave_idx_type& info, double& rcond,
                              solve_singularity_handler sing_handler,
                              bool calc_cond = false) const;

  // Upper triangular matrix solvers
  Matrix utsolve (MatrixType &typ, const Matrix& b, octave_idx_type& info,
                  double& rcond, solve_singularity_handler sing_handler,
                  bool calc_cond = false) const;

  ComplexMatrix utsolve (MatrixType &typ, const ComplexMatrix& b,
                         octave_idx_type& info, double& rcond,
                         solve_singularity_handler sing_handler,
                         bool calc_cond = false) const;

  SparseMatrix utsolve (MatrixType &typ, const SparseMatrix& b,
                        octave_idx_type& info, double& rcond,
                        solve_singularity_handler sing_handler,
                        bool calc_cond = false) const;

  SparseComplexMatrix utsolve (MatrixType &typ, const SparseComplexMatrix& b,
                               octave_idx_type& info, double& rcond,
                               solve_singularity_handler sing_handler,
                               bool calc_cond = false) const;

  // Lower triangular matrix solvers
  Matrix ltsolve (MatrixType &typ, const Matrix& b, octave_idx_type& info,
                  double& rcond, solve_singularity_handler sing_handler,
                  bool calc_cond = false) const;

  ComplexMatrix ltsolve (MatrixType &typ, const ComplexMatrix& b,
                         octave_idx_type& info, double& rcond,
                         solve_singularity_handler sing_handler,
                         bool calc_cond = false) const;

  SparseMatrix ltsolve (MatrixType &typ, const SparseMatrix& b,
                        octave_idx_type& info, double& rcond,
                        solve_singularity_handler sing_handler,
                        bool calc_cond = false) const;

  SparseComplexMatrix ltsolve (MatrixType &typ, const SparseComplexMatrix& b,
                               octave_idx_type& info, double& rcond,
                               solve_singularity_handler sing_handler,
                               bool calc_cond = false) const;

  // Tridiagonal matrix solvers
  Matrix trisolve (MatrixType &typ, const Matrix& b, octave_idx_type& info,
                   double& rcond, solve_singularity_handler sing_handler,
                   bool calc_cond = false) const;

  ComplexMatrix trisolve (MatrixType &typ, const ComplexMatrix& b,
                          octave_idx_type& info, double& rcond,
                          solve_singularity_handler sing_handler,
                          bool calc_cond = false) const;

  SparseMatrix trisolve (MatrixType &typ, const SparseMatrix& b,
                         octave_idx_type& info, double& rcond,
                         solve_singularity_handler sing_handler,
                         bool calc_cond = false) const;

  SparseComplexMatrix trisolve (MatrixType &typ, const SparseComplexMatrix& b,
                                octave_idx_type& info, double& rcond,
                                solve_singularity_handler sing_handler,
                                bool calc_cond = false) const;

  // Banded matrix solvers (umfpack/cholesky)
  Matrix bsolve (MatrixType &typ, const Matrix& b, octave_idx_type& info,
                 double& rcond, solve_singularity_handler sing_handler,
                 bool calc_cond = false) const;

  ComplexMatrix bsolve (MatrixType &typ, const ComplexMatrix& b,
                        octave_idx_type& info, double& rcond,
                        solve_singularity_handler sing_handler,
                        bool calc_cond = false) const;

  SparseMatrix bsolve (MatrixType &typ, const SparseMatrix& b,
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
                    bool calc_cond = false) const;

  Matrix fsolve (MatrixType &typ, const Matrix& b, octave_idx_type& info,
                 double& rcond, solve_singularity_handler sing_handler,
                 bool calc_cond = false) const;

  ComplexMatrix fsolve (MatrixType &typ, const ComplexMatrix& b,
                        octave_idx_type& info, double& rcond,
                        solve_singularity_handler sing_handler,
                        bool calc_cond = false) const;

  SparseMatrix fsolve (MatrixType &typ, const SparseMatrix& b,
                       octave_idx_type& info, double& rcond,
                       solve_singularity_handler sing_handler,
                       bool calc_cond = false) const;

  SparseComplexMatrix fsolve (MatrixType &typ, const SparseComplexMatrix& b,
                              octave_idx_type& info, double& rcond,
                              solve_singularity_handler sing_handler,
                              bool calc_cond = false) const;

public:
  // Generic interface to solver with no probing of type
  Matrix solve (MatrixType &typ, const Matrix& b) const;
  Matrix solve (MatrixType &typ, const Matrix& b, octave_idx_type& info) const;
  Matrix solve (MatrixType &typ, const Matrix& b, octave_idx_type& info,
                double& rcond) const;
  Matrix solve (MatrixType &typ, const Matrix& b, octave_idx_type& info,
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

  SparseMatrix solve (MatrixType &typ, const SparseMatrix& b) const;
  SparseMatrix solve (MatrixType &typ, const SparseMatrix& b,
                      octave_idx_type& info) const;
  SparseMatrix solve (MatrixType &typ, const SparseMatrix& b,
                      octave_idx_type& info, double& rcond) const;
  SparseMatrix solve (MatrixType &typ, const SparseMatrix& b,
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
                             bool singular_fallabck = true) const;

  ColumnVector solve (MatrixType &typ, const ColumnVector& b) const;
  ColumnVector solve (MatrixType &typ, const ColumnVector& b,
                      octave_idx_type& info) const;
  ColumnVector solve (MatrixType &typ, const ColumnVector& b,
                      octave_idx_type& info, double& rcond) const;
  ColumnVector solve (MatrixType &typ, const ColumnVector& b,
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
  Matrix solve (const Matrix& b) const;
  Matrix solve (const Matrix& b, octave_idx_type& info) const;
  Matrix solve (const Matrix& b, octave_idx_type& info, double& rcond) const;
  Matrix solve (const Matrix& b, octave_idx_type& info, double& rcond,
                solve_singularity_handler sing_handler) const;

  ComplexMatrix solve (const ComplexMatrix& b) const;
  ComplexMatrix solve (const ComplexMatrix& b, octave_idx_type& info) const;
  ComplexMatrix solve (const ComplexMatrix& b, octave_idx_type& info,
                       double& rcond) const;
  ComplexMatrix solve (const ComplexMatrix& b, octave_idx_type& info,
                       double& rcond,
                       solve_singularity_handler sing_handler) const;

  SparseMatrix solve (const SparseMatrix& b) const;
  SparseMatrix solve (const SparseMatrix& b, octave_idx_type& info) const;
  SparseMatrix solve (const SparseMatrix& b, octave_idx_type& info,
                      double& rcond) const;
  SparseMatrix solve (const SparseMatrix& b, octave_idx_type& info,
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

  ColumnVector solve (const ColumnVector& b) const;
  ColumnVector solve (const ColumnVector& b, octave_idx_type& info) const;
  ColumnVector solve (const ColumnVector& b, octave_idx_type& info,
                      double& rcond) const;
  ColumnVector solve (const ColumnVector& b, octave_idx_type& info,
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

  // other operations

  bool any_element_is_negative (bool = false) const;
  bool any_element_is_nan (void) const;
  bool any_element_is_inf_or_nan (void) const;
  bool any_element_not_one_or_zero (void) const;
  bool all_elements_are_zero (void) const;
  bool all_elements_are_int_or_inf_or_nan (void) const;
  bool all_integers (double& max_val, double& min_val) const;
  bool too_large_for_float (void) const;

  SparseBoolMatrix operator ! (void) const;

  SparseBoolMatrix all (int dim = -1) const;
  SparseBoolMatrix any (int dim = -1) const;

  SparseMatrix cumprod (int dim = -1) const;
  SparseMatrix cumsum (int dim = -1) const;
  SparseMatrix prod (int dim = -1) const;
  SparseMatrix sum (int dim = -1) const;
  SparseMatrix sumsq (int dim = -1) const;
  SparseMatrix abs (void) const;

  SparseMatrix diag (octave_idx_type k = 0) const;

  Matrix matrix_value (void) const;

  SparseMatrix squeeze (void) const;

  SparseMatrix reshape (const dim_vector& new_dims) const;

  SparseMatrix permute (const Array<octave_idx_type>& vec,
                        bool inv = false) const;

  SparseMatrix ipermute (const Array<octave_idx_type>& vec) const;

  // i/o

  friend OCTAVE_API std::ostream& operator << (std::ostream& os,
                                               const SparseMatrix& a);
  friend OCTAVE_API std::istream& operator >> (std::istream& is,
                                               SparseMatrix& a);

};

// Publish externally used friend functions.

extern OCTAVE_API SparseMatrix real (const SparseComplexMatrix& a);
extern OCTAVE_API SparseMatrix imag (const SparseComplexMatrix& a);

// Other operators.

extern OCTAVE_API SparseMatrix operator * (const SparseMatrix& a,
                                           const SparseMatrix& b);
extern OCTAVE_API Matrix operator * (const Matrix& a,
                                     const SparseMatrix& b);
extern OCTAVE_API Matrix mul_trans (const Matrix& a,
                                    const SparseMatrix& b);
extern OCTAVE_API Matrix operator * (const SparseMatrix& a,
                                     const Matrix& b);
extern OCTAVE_API Matrix trans_mul (const SparseMatrix& a,
                                    const Matrix& b);

extern OCTAVE_API SparseMatrix operator * (const DiagMatrix&,
                                           const SparseMatrix&);
extern OCTAVE_API SparseMatrix operator * (const SparseMatrix&,
                                           const DiagMatrix&);

extern OCTAVE_API SparseMatrix operator + (const DiagMatrix&,
                                           const SparseMatrix&);
extern OCTAVE_API SparseMatrix operator + (const SparseMatrix&,
                                           const DiagMatrix&);
extern OCTAVE_API SparseMatrix operator - (const DiagMatrix&,
                                           const SparseMatrix&);
extern OCTAVE_API SparseMatrix operator - (const SparseMatrix&,
                                           const DiagMatrix&);

extern OCTAVE_API SparseMatrix operator * (const PermMatrix&,
                                           const SparseMatrix&);
extern OCTAVE_API SparseMatrix operator * (const SparseMatrix&,
                                           const PermMatrix&);

extern OCTAVE_API SparseMatrix min (double d, const SparseMatrix& m);
extern OCTAVE_API SparseMatrix min (const SparseMatrix& m, double d);
extern OCTAVE_API SparseMatrix min (const SparseMatrix& a,
                                    const SparseMatrix& b);

extern OCTAVE_API SparseMatrix max (double d, const SparseMatrix& m);
extern OCTAVE_API SparseMatrix max (const SparseMatrix& m, double d);
extern OCTAVE_API SparseMatrix max (const SparseMatrix& a,
                                    const SparseMatrix& b);

SPARSE_SMS_CMP_OP_DECLS (SparseMatrix, double, OCTAVE_API)
SPARSE_SMS_BOOL_OP_DECLS (SparseMatrix, double, OCTAVE_API)

SPARSE_SSM_CMP_OP_DECLS (double, SparseMatrix, OCTAVE_API)
SPARSE_SSM_BOOL_OP_DECLS (double, SparseMatrix, OCTAVE_API)

SPARSE_SMSM_CMP_OP_DECLS (SparseMatrix, SparseMatrix, OCTAVE_API)
SPARSE_SMSM_BOOL_OP_DECLS (SparseMatrix, SparseMatrix, OCTAVE_API)

SPARSE_FORWARD_DEFS (MSparse, SparseMatrix, Matrix, double)

#ifdef USE_64_BIT_IDX_T
#define UMFPACK_DNAME(name) umfpack_dl_ ## name
#else
#define UMFPACK_DNAME(name) umfpack_di_ ## name
#endif

#endif
