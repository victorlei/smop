/*

Copyright (C) 1994-2015 John W. Eaton

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

#if !defined (octave_CMatrix_h)
#define octave_CMatrix_h 1

#include "MArray.h"
#include "MDiagArray2.h"
#include "MatrixType.h"
#include "CNDArray.h"

#include "mx-defs.h"
#include "mx-op-decl.h"
#include "oct-cmplx.h"
#include "DET.h"

class
OCTAVE_API
ComplexMatrix : public ComplexNDArray
{
public:

  typedef ComplexColumnVector column_vector_type;
  typedef ComplexRowVector row_vector_type;

  typedef void (*solve_singularity_handler) (double rcon);

  ComplexMatrix (void) : ComplexNDArray () { }

  ComplexMatrix (octave_idx_type r, octave_idx_type c)
    : ComplexNDArray (dim_vector (r, c)) { }

  ComplexMatrix (octave_idx_type r, octave_idx_type c, const Complex& val)
    : ComplexNDArray (dim_vector (r, c), val) { }

  ComplexMatrix (const dim_vector& dv) : ComplexNDArray (dv.redim (2)) { }

  ComplexMatrix (const dim_vector& dv, const Complex& val)
    : ComplexNDArray (dv.redim (2), val) { }

  ComplexMatrix (const ComplexMatrix& a) : ComplexNDArray (a) { }

  template <class U>
  ComplexMatrix (const MArray<U>& a) : ComplexNDArray (a.as_matrix ()) { }

  template <class U>
  ComplexMatrix (const Array<U>& a) : ComplexNDArray (a.as_matrix ()) { }

  ComplexMatrix (const Matrix& re, const Matrix& im);

  explicit ComplexMatrix (const Matrix& a);

  explicit ComplexMatrix (const RowVector& rv);

  explicit ComplexMatrix (const ColumnVector& cv);

  explicit ComplexMatrix (const DiagMatrix& a);

  explicit ComplexMatrix (const MDiagArray2<double>& a);

  explicit ComplexMatrix (const DiagArray2<double>& a);

  explicit ComplexMatrix (const ComplexRowVector& rv);

  explicit ComplexMatrix (const ComplexColumnVector& cv);

  explicit ComplexMatrix (const ComplexDiagMatrix& a);

  explicit ComplexMatrix (const MDiagArray2<Complex>& a);

  explicit ComplexMatrix (const DiagArray2<Complex>& a);

  explicit ComplexMatrix (const boolMatrix& a);

  explicit ComplexMatrix (const charMatrix& a);

  bool operator == (const ComplexMatrix& a) const;
  bool operator != (const ComplexMatrix& a) const;

  bool is_hermitian (void) const;

  // destructive insert/delete/reorder operations

  ComplexMatrix& insert (const Matrix& a, octave_idx_type r, octave_idx_type c);
  ComplexMatrix& insert (const RowVector& a,
                         octave_idx_type r, octave_idx_type c);
  ComplexMatrix& insert (const ColumnVector& a,
                         octave_idx_type r, octave_idx_type c);
  ComplexMatrix& insert (const DiagMatrix& a,
                         octave_idx_type r, octave_idx_type c);

  ComplexMatrix& insert (const ComplexMatrix& a,
                         octave_idx_type r, octave_idx_type c);
  ComplexMatrix& insert (const ComplexRowVector& a,
                         octave_idx_type r, octave_idx_type c);
  ComplexMatrix& insert (const ComplexColumnVector& a,
                         octave_idx_type r, octave_idx_type c);
  ComplexMatrix& insert (const ComplexDiagMatrix& a,
                         octave_idx_type r, octave_idx_type c);

  ComplexMatrix& fill (double val);
  ComplexMatrix& fill (const Complex& val);
  ComplexMatrix& fill (double val,
                       octave_idx_type r1, octave_idx_type c1,
                       octave_idx_type r2, octave_idx_type c2);
  ComplexMatrix& fill (const Complex& val,
                       octave_idx_type r1, octave_idx_type c1,
                       octave_idx_type r2, octave_idx_type c2);

  ComplexMatrix append (const Matrix& a) const;
  ComplexMatrix append (const RowVector& a) const;
  ComplexMatrix append (const ColumnVector& a) const;
  ComplexMatrix append (const DiagMatrix& a) const;

  ComplexMatrix append (const ComplexMatrix& a) const;
  ComplexMatrix append (const ComplexRowVector& a) const;
  ComplexMatrix append (const ComplexColumnVector& a) const;
  ComplexMatrix append (const ComplexDiagMatrix& a) const;

  ComplexMatrix stack (const Matrix& a) const;
  ComplexMatrix stack (const RowVector& a) const;
  ComplexMatrix stack (const ColumnVector& a) const;
  ComplexMatrix stack (const DiagMatrix& a) const;

  ComplexMatrix stack (const ComplexMatrix& a) const;
  ComplexMatrix stack (const ComplexRowVector& a) const;
  ComplexMatrix stack (const ComplexColumnVector& a) const;
  ComplexMatrix stack (const ComplexDiagMatrix& a) const;

  ComplexMatrix hermitian (void) const
  { return MArray<Complex>::hermitian (std::conj); }
  ComplexMatrix transpose (void) const
  { return MArray<Complex>::transpose (); }

  friend OCTAVE_API ComplexMatrix conj (const ComplexMatrix& a);

  // resize is the destructive equivalent for this one

  ComplexMatrix extract (octave_idx_type r1, octave_idx_type c1,
                         octave_idx_type r2, octave_idx_type c2) const;

  ComplexMatrix extract_n (octave_idx_type r1, octave_idx_type c1,
                           octave_idx_type nr, octave_idx_type nc) const;

  // extract row or column i.

  ComplexRowVector row (octave_idx_type i) const;

  ComplexColumnVector column (octave_idx_type i) const;

  void resize (octave_idx_type nr, octave_idx_type nc,
               const Complex& rfv = Complex (0))
  {
    MArray<Complex>::resize (dim_vector (nr, nc), rfv);
  }

private:
  ComplexMatrix tinverse (MatrixType &mattype, octave_idx_type& info,
                          double& rcon, int force, int calc_cond) const;

  ComplexMatrix finverse (MatrixType &mattype, octave_idx_type& info,
                          double& rcon, int force, int calc_cond) const;

public:
  ComplexMatrix inverse (void) const;
  ComplexMatrix inverse (octave_idx_type& info) const;
  ComplexMatrix inverse (octave_idx_type& info, double& rcon, int force = 0,
                         int calc_cond = 1) const;

  ComplexMatrix inverse (MatrixType &mattype) const;
  ComplexMatrix inverse (MatrixType &mattype, octave_idx_type& info) const;
  ComplexMatrix inverse (MatrixType &mattype, octave_idx_type& info,
                         double& rcon, int force = 0,
                         int calc_cond = 1) const;

  ComplexMatrix pseudo_inverse (double tol = 0.0) const;

  ComplexMatrix fourier (void) const;
  ComplexMatrix ifourier (void) const;

  ComplexMatrix fourier2d (void) const;
  ComplexMatrix ifourier2d (void) const;

  ComplexDET determinant (void) const;
  ComplexDET determinant (octave_idx_type& info) const;
  ComplexDET determinant (octave_idx_type& info, double& rcon,
                          int calc_cond = 1) const;
  ComplexDET determinant (MatrixType &mattype, octave_idx_type& info,
                          double& rcon, int calc_cond = 1) const;

  double rcond (void) const;
  double rcond (MatrixType &mattype) const;

private:
  // Upper triangular matrix solvers
  ComplexMatrix utsolve (MatrixType &typ, const ComplexMatrix& b,
                         octave_idx_type& info, double& rcon,
                         solve_singularity_handler sing_handler,
                         bool calc_cond = false,
                         blas_trans_type transt = blas_no_trans) const;

  // Lower triangular matrix solvers
  ComplexMatrix ltsolve (MatrixType &typ, const ComplexMatrix& b,
                         octave_idx_type& info, double& rcon,
                         solve_singularity_handler sing_handler,
                         bool calc_cond = false,
                         blas_trans_type transt = blas_no_trans) const;

  // Full matrix solvers (umfpack/cholesky)
  ComplexMatrix fsolve (MatrixType &typ, const ComplexMatrix& b,
                        octave_idx_type& info, double& rcon,
                        solve_singularity_handler sing_handler,
                        bool calc_cond = false) const;

public:
  // Generic interface to solver with no probing of type
  ComplexMatrix solve (MatrixType &typ, const Matrix& b) const;
  ComplexMatrix solve (MatrixType &typ, const Matrix& b,
                       octave_idx_type& info) const;
  ComplexMatrix solve (MatrixType &typ, const Matrix& b,
                       octave_idx_type& info, double& rcon) const;
  ComplexMatrix solve (MatrixType &typ, const Matrix& b, octave_idx_type& info,
                       double& rcon, solve_singularity_handler sing_handler,
                       bool singular_fallback = true,
                       blas_trans_type transt = blas_no_trans) const;

  ComplexMatrix solve (MatrixType &typ, const ComplexMatrix& b) const;
  ComplexMatrix solve (MatrixType &typ, const ComplexMatrix& b,
                       octave_idx_type& info) const;
  ComplexMatrix solve (MatrixType &typ, const ComplexMatrix& b,
                       octave_idx_type& info, double& rcon) const;
  ComplexMatrix solve (MatrixType &typ, const ComplexMatrix& b,
                       octave_idx_type& info, double& rcon,
                       solve_singularity_handler sing_handler,
                       bool singular_fallback = true,
                       blas_trans_type transt = blas_no_trans) const;

  ComplexColumnVector solve (MatrixType &typ, const ColumnVector& b) const;
  ComplexColumnVector solve (MatrixType &typ, const ColumnVector& b,
                             octave_idx_type& info) const;
  ComplexColumnVector solve (MatrixType &typ, const ColumnVector& b,
                             octave_idx_type& info, double& rcon) const;
  ComplexColumnVector solve (MatrixType &typ, const ColumnVector& b,
                             octave_idx_type& info, double& rcon,
                             solve_singularity_handler sing_handler,
                             blas_trans_type transt = blas_no_trans) const;

  ComplexColumnVector solve (MatrixType &typ,
                             const ComplexColumnVector& b) const;
  ComplexColumnVector solve (MatrixType &typ, const ComplexColumnVector& b,
                             octave_idx_type& info) const;
  ComplexColumnVector solve (MatrixType &typ, const ComplexColumnVector& b,
                             octave_idx_type& info, double& rcon) const;
  ComplexColumnVector solve (MatrixType &typ, const ComplexColumnVector& b,
                             octave_idx_type& info, double& rcon,
                             solve_singularity_handler sing_handler,
                             blas_trans_type transt = blas_no_trans) const;

  // Generic interface to solver with probing of type
  ComplexMatrix solve (const Matrix& b) const;
  ComplexMatrix solve (const Matrix& b, octave_idx_type& info) const;
  ComplexMatrix solve (const Matrix& b, octave_idx_type& info,
                       double& rcon) const;
  ComplexMatrix solve (const Matrix& b, octave_idx_type& info, double& rcon,
                       solve_singularity_handler sing_handler,
                       blas_trans_type transt = blas_no_trans) const;

  ComplexMatrix solve (const ComplexMatrix& b) const;
  ComplexMatrix solve (const ComplexMatrix& b, octave_idx_type& info) const;
  ComplexMatrix solve (const ComplexMatrix& b, octave_idx_type& info,
                       double& rcon) const;
  ComplexMatrix solve (const ComplexMatrix& b, octave_idx_type& info,
                       double& rcon,
                       solve_singularity_handler sing_handler,
                       blas_trans_type transt = blas_no_trans) const;

  ComplexColumnVector solve (const ColumnVector& b) const;
  ComplexColumnVector solve (const ColumnVector& b, octave_idx_type& info) const;
  ComplexColumnVector solve (const ColumnVector& b, octave_idx_type& info,
                             double& rcon) const;
  ComplexColumnVector solve (const ColumnVector& b, octave_idx_type& info,
                             double& rcon,
                             solve_singularity_handler sing_handler,
                             blas_trans_type transt = blas_no_trans) const;

  ComplexColumnVector solve (const ComplexColumnVector& b) const;
  ComplexColumnVector solve (const ComplexColumnVector& b,
                             octave_idx_type& info) const;
  ComplexColumnVector solve (const ComplexColumnVector& b,
                             octave_idx_type& info,
                             double& rcon) const;
  ComplexColumnVector solve (const ComplexColumnVector& b,
                             octave_idx_type& info,
                             double& rcon,
                             solve_singularity_handler sing_handler,
                             blas_trans_type transt = blas_no_trans) const;

  ComplexMatrix lssolve (const Matrix& b) const;
  ComplexMatrix lssolve (const Matrix& b, octave_idx_type& info) const;
  ComplexMatrix lssolve (const Matrix& b, octave_idx_type& info,
                         octave_idx_type& rank) const;
  ComplexMatrix lssolve (const Matrix& b, octave_idx_type& info,
                         octave_idx_type& rank, double& rcon) const;

  ComplexMatrix lssolve (const ComplexMatrix& b) const;
  ComplexMatrix lssolve (const ComplexMatrix& b, octave_idx_type& info) const;
  ComplexMatrix lssolve (const ComplexMatrix& b, octave_idx_type& info,
                         octave_idx_type& rank) const;
  ComplexMatrix lssolve (const ComplexMatrix& b, octave_idx_type& info,
                         octave_idx_type& rank, double& rcon) const;

  ComplexColumnVector lssolve (const ColumnVector& b) const;
  ComplexColumnVector lssolve (const ColumnVector& b,
                               octave_idx_type& info) const;
  ComplexColumnVector lssolve (const ColumnVector& b, octave_idx_type& info,
                               octave_idx_type& rank) const;
  ComplexColumnVector lssolve (const ColumnVector& b, octave_idx_type& info,
                               octave_idx_type& rank, double& rcon) const;

  ComplexColumnVector lssolve (const ComplexColumnVector& b) const;
  ComplexColumnVector lssolve (const ComplexColumnVector& b,
                               octave_idx_type& info) const;
  ComplexColumnVector lssolve (const ComplexColumnVector& b,
                               octave_idx_type& info,
                               octave_idx_type& rank) const;
  ComplexColumnVector lssolve (const ComplexColumnVector& b,
                               octave_idx_type& info,
                               octave_idx_type& rank, double& rcon) const;

  // matrix by diagonal matrix -> matrix operations

  ComplexMatrix& operator += (const DiagMatrix& a);
  ComplexMatrix& operator -= (const DiagMatrix& a);

  ComplexMatrix& operator += (const ComplexDiagMatrix& a);
  ComplexMatrix& operator -= (const ComplexDiagMatrix& a);

  // matrix by matrix -> matrix operations

  ComplexMatrix& operator += (const Matrix& a);
  ComplexMatrix& operator -= (const Matrix& a);

  // other operations

  boolMatrix all (int dim = -1) const;
  boolMatrix any (int dim = -1) const;

  ComplexMatrix cumprod (int dim = -1) const;
  ComplexMatrix cumsum (int dim = -1) const;
  ComplexMatrix prod (int dim = -1) const;
  ComplexMatrix sum (int dim = -1) const;
  ComplexMatrix sumsq (int dim = -1) const;
  Matrix abs (void) const;

  ComplexMatrix diag (octave_idx_type k = 0) const;

  ComplexDiagMatrix diag (octave_idx_type m, octave_idx_type n) const;

  bool row_is_real_only (octave_idx_type) const;
  bool column_is_real_only (octave_idx_type) const;

  ComplexColumnVector row_min (void) const;
  ComplexColumnVector row_max (void) const;

  ComplexColumnVector row_min (Array<octave_idx_type>& index) const;
  ComplexColumnVector row_max (Array<octave_idx_type>& index) const;

  ComplexRowVector column_min (void) const;
  ComplexRowVector column_max (void) const;

  ComplexRowVector column_min (Array<octave_idx_type>& index) const;
  ComplexRowVector column_max (Array<octave_idx_type>& index) const;

  // i/o

  friend OCTAVE_API std::ostream& operator << (std::ostream& os,
                                               const ComplexMatrix& a);
  friend OCTAVE_API std::istream& operator >> (std::istream& is,
                                               ComplexMatrix& a);
};

extern OCTAVE_API ComplexMatrix conj (const ComplexMatrix& a);

// column vector by row vector -> matrix operations

extern OCTAVE_API ComplexMatrix
operator * (const ColumnVector& a, const ComplexRowVector& b);

extern OCTAVE_API ComplexMatrix
operator * (const ComplexColumnVector& a, const RowVector& b);

extern OCTAVE_API ComplexMatrix
operator * (const ComplexColumnVector& a, const ComplexRowVector& b);

extern OCTAVE_API ComplexMatrix
Givens (const Complex&, const Complex&);

extern OCTAVE_API ComplexMatrix
Sylvester (const ComplexMatrix&, const ComplexMatrix&, const ComplexMatrix&);

extern OCTAVE_API ComplexMatrix
xgemm (const ComplexMatrix& a, const ComplexMatrix& b,
       blas_trans_type transa = blas_no_trans,
       blas_trans_type transb = blas_no_trans);

extern OCTAVE_API ComplexMatrix operator * (const Matrix&,
                                            const ComplexMatrix&);
extern OCTAVE_API ComplexMatrix operator * (const ComplexMatrix&,
                                            const Matrix&);
extern OCTAVE_API ComplexMatrix operator * (const ComplexMatrix&,
                                            const ComplexMatrix&);

extern OCTAVE_API ComplexMatrix min (const Complex& c, const ComplexMatrix& m);
extern OCTAVE_API ComplexMatrix min (const ComplexMatrix& m, const Complex& c);
extern OCTAVE_API ComplexMatrix min (const ComplexMatrix& a,
                                     const ComplexMatrix& b);

extern OCTAVE_API ComplexMatrix max (const Complex& c, const ComplexMatrix& m);
extern OCTAVE_API ComplexMatrix max (const ComplexMatrix& m, const Complex& c);
extern OCTAVE_API ComplexMatrix max (const ComplexMatrix& a,
                                     const ComplexMatrix& b);

extern OCTAVE_API ComplexMatrix linspace (const ComplexColumnVector& x1,
                                          const ComplexColumnVector& x2,
                                          octave_idx_type n);


MS_CMP_OP_DECLS (ComplexMatrix, Complex, OCTAVE_API)
MS_BOOL_OP_DECLS (ComplexMatrix, Complex, OCTAVE_API)

SM_CMP_OP_DECLS (Complex, ComplexMatrix, OCTAVE_API)
SM_BOOL_OP_DECLS (Complex, ComplexMatrix, OCTAVE_API)

MM_CMP_OP_DECLS (ComplexMatrix, ComplexMatrix, OCTAVE_API)
MM_BOOL_OP_DECLS (ComplexMatrix, ComplexMatrix, OCTAVE_API)

MARRAY_FORWARD_DEFS (MArray, ComplexMatrix, Complex)

#endif
