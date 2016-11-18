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

#if !defined (octave_dMatrix_h)
#define octave_dMatrix_h 1

#include "dNDArray.h"
#include "MArray.h"
#include "MDiagArray2.h"
#include "MatrixType.h"

#include "mx-defs.h"
#include "mx-op-decl.h"
#include "DET.h"

class
OCTAVE_API
Matrix : public NDArray
{
public:

  typedef ColumnVector column_vector_type;
  typedef RowVector row_vector_type;

  typedef void (*solve_singularity_handler) (double rcon);

  Matrix (void) : NDArray () { }

  Matrix (octave_idx_type r, octave_idx_type c)
    : NDArray (dim_vector (r, c)) { }

  Matrix (octave_idx_type r, octave_idx_type c, double val)
    : NDArray (dim_vector (r, c), val) { }

  Matrix (const dim_vector& dv) : NDArray (dv.redim (2)) { }

  Matrix (const dim_vector& dv, double val)
    : NDArray (dv.redim (2), val) { }

  Matrix (const Matrix& a) : NDArray (a) { }

  template <class U>
  Matrix (const MArray<U>& a) : NDArray (a.as_matrix ()) { }

  template <class U>
  Matrix (const Array<U>& a) : NDArray (a.as_matrix ()) { }

  explicit Matrix (const RowVector& rv);

  explicit Matrix (const ColumnVector& cv);

  explicit Matrix (const DiagMatrix& a);

  explicit Matrix (const MDiagArray2<double>& a);

  explicit Matrix (const DiagArray2<double>& a);

  explicit Matrix (const PermMatrix& a);

  explicit Matrix (const boolMatrix& a);

  explicit Matrix (const charMatrix& a);

  bool operator == (const Matrix& a) const;
  bool operator != (const Matrix& a) const;

  bool is_symmetric (void) const;

  // destructive insert/delete/reorder operations

  Matrix& insert (const Matrix& a, octave_idx_type r, octave_idx_type c);
  Matrix& insert (const RowVector& a, octave_idx_type r, octave_idx_type c);
  Matrix& insert (const ColumnVector& a, octave_idx_type r, octave_idx_type c);
  Matrix& insert (const DiagMatrix& a, octave_idx_type r, octave_idx_type c);

  Matrix& fill (double val);
  Matrix& fill (double val, octave_idx_type r1, octave_idx_type c1,
                octave_idx_type r2, octave_idx_type c2);

  Matrix append (const Matrix& a) const;
  Matrix append (const RowVector& a) const;
  Matrix append (const ColumnVector& a) const;
  Matrix append (const DiagMatrix& a) const;

  Matrix stack (const Matrix& a) const;
  Matrix stack (const RowVector& a) const;
  Matrix stack (const ColumnVector& a) const;
  Matrix stack (const DiagMatrix& a) const;

  friend OCTAVE_API Matrix real (const ComplexMatrix& a);
  friend OCTAVE_API Matrix imag (const ComplexMatrix& a);

  friend class ComplexMatrix;

  Matrix transpose (void) const { return MArray<double>::transpose (); }

  // resize is the destructive equivalent for this one

  Matrix extract (octave_idx_type r1, octave_idx_type c1,
                  octave_idx_type r2, octave_idx_type c2) const;

  Matrix extract_n (octave_idx_type r1, octave_idx_type c1,
                    octave_idx_type nr, octave_idx_type nc) const;

  // extract row or column i.

  RowVector row (octave_idx_type i) const;

  ColumnVector column (octave_idx_type i) const;

  void resize (octave_idx_type nr, octave_idx_type nc, double rfv = 0)
  {
    MArray<double>::resize (dim_vector (nr, nc), rfv);
  }

private:
  Matrix tinverse (MatrixType &mattype, octave_idx_type& info, double& rcon,
                   int force, int calc_cond) const;

  Matrix finverse (MatrixType &mattype, octave_idx_type& info, double& rcon,
                   int force, int calc_cond) const;

public:
  Matrix inverse (void) const;
  Matrix inverse (octave_idx_type& info) const;
  Matrix inverse (octave_idx_type& info, double& rcon, int force = 0,
                  int calc_cond = 1) const;

  Matrix inverse (MatrixType &mattype) const;
  Matrix inverse (MatrixType &mattype, octave_idx_type& info) const;
  Matrix inverse (MatrixType &mattype, octave_idx_type& info, double& rcon,
                  int force = 0, int calc_cond = 1) const;

  Matrix pseudo_inverse (double tol = 0.0) const;

  ComplexMatrix fourier (void) const;
  ComplexMatrix ifourier (void) const;

  ComplexMatrix fourier2d (void) const;
  ComplexMatrix ifourier2d (void) const;

  DET determinant (void) const;
  DET determinant (octave_idx_type& info) const;
  DET determinant (octave_idx_type& info, double& rcon,
                   int calc_cond = 1) const;
  DET determinant (MatrixType &mattype, octave_idx_type& info,
                   double& rcon, int calc_cond = 1) const;

  double rcond (void) const;
  double rcond (MatrixType &mattype) const;

private:
  // Upper triangular matrix solvers
  Matrix utsolve (MatrixType &typ, const Matrix& b, octave_idx_type& info,
                  double& rcon, solve_singularity_handler sing_handler,
                  bool calc_cond = false,
                  blas_trans_type transt = blas_no_trans) const;

  // Lower triangular matrix solvers
  Matrix ltsolve (MatrixType &typ, const Matrix& b, octave_idx_type& info,
                  double& rcon, solve_singularity_handler sing_handler,
                  bool calc_cond = false,
                  blas_trans_type transt = blas_no_trans) const;

  // Full matrix solvers (lu/cholesky)
  Matrix fsolve (MatrixType &typ, const Matrix& b, octave_idx_type& info,
                 double& rcon, solve_singularity_handler sing_handler,
                 bool calc_cond = false) const;

public:
  // Generic interface to solver with no probing of type
  Matrix solve (MatrixType &typ, const Matrix& b) const;
  Matrix solve (MatrixType &typ, const Matrix& b, octave_idx_type& info) const;
  Matrix solve (MatrixType &typ, const Matrix& b, octave_idx_type& info,
                double& rcon) const;
  Matrix solve (MatrixType &typ, const Matrix& b, octave_idx_type& info,
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

  ColumnVector solve (MatrixType &typ, const ColumnVector& b) const;
  ColumnVector solve (MatrixType &typ, const ColumnVector& b,
                      octave_idx_type& info) const;
  ColumnVector solve (MatrixType &typ, const ColumnVector& b,
                      octave_idx_type& info, double& rcon) const;
  ColumnVector solve (MatrixType &typ, const ColumnVector& b,
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
  Matrix solve (const Matrix& b) const;
  Matrix solve (const Matrix& b, octave_idx_type& info) const;
  Matrix solve (const Matrix& b, octave_idx_type& info, double& rcon) const;
  Matrix solve (const Matrix& b, octave_idx_type& info, double& rcon,
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

  ColumnVector solve (const ColumnVector& b) const;
  ColumnVector solve (const ColumnVector& b, octave_idx_type& info) const;
  ColumnVector solve (const ColumnVector& b, octave_idx_type& info,
                      double& rcon) const;
  ColumnVector solve (const ColumnVector& b, octave_idx_type& info,
                      double& rcon,
                      solve_singularity_handler sing_handler,
                      blas_trans_type transt = blas_no_trans) const;

  ComplexColumnVector solve (const ComplexColumnVector& b) const;
  ComplexColumnVector solve (const ComplexColumnVector& b,
                             octave_idx_type& info) const;
  ComplexColumnVector solve (const ComplexColumnVector& b,
                             octave_idx_type& info, double& rcon) const;
  ComplexColumnVector solve (const ComplexColumnVector& b,
                             octave_idx_type& info, double& rcon,
                             solve_singularity_handler sing_handler,
                             blas_trans_type transt = blas_no_trans) const;

  // Singular solvers
  Matrix lssolve (const Matrix& b) const;
  Matrix lssolve (const Matrix& b, octave_idx_type& info) const;
  Matrix lssolve (const Matrix& b, octave_idx_type& info,
                  octave_idx_type& rank) const;
  Matrix lssolve (const Matrix& b, octave_idx_type& info,
                  octave_idx_type& rank, double& rcon) const;

  ComplexMatrix lssolve (const ComplexMatrix& b) const;
  ComplexMatrix lssolve (const ComplexMatrix& b, octave_idx_type& info) const;
  ComplexMatrix lssolve (const ComplexMatrix& b, octave_idx_type& info,
                         octave_idx_type& rank) const;
  ComplexMatrix lssolve (const ComplexMatrix& b, octave_idx_type& info,
                         octave_idx_type& rank, double &rcon) const;

  ColumnVector lssolve (const ColumnVector& b) const;
  ColumnVector lssolve (const ColumnVector& b, octave_idx_type& info) const;
  ColumnVector lssolve (const ColumnVector& b, octave_idx_type& info,
                        octave_idx_type& rank) const;
  ColumnVector lssolve (const ColumnVector& b, octave_idx_type& info,
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

  Matrix& operator += (const DiagMatrix& a);
  Matrix& operator -= (const DiagMatrix& a);

  // unary operations

  // other operations

  boolMatrix all (int dim = -1) const;
  boolMatrix any (int dim = -1) const;

  Matrix cumprod (int dim = -1) const;
  Matrix cumsum (int dim = -1) const;
  Matrix prod (int dim = -1) const;
  Matrix sum (int dim = -1) const;
  Matrix sumsq (int dim = -1) const;
  Matrix abs (void) const;

  Matrix diag (octave_idx_type k = 0) const;

  DiagMatrix diag (octave_idx_type m, octave_idx_type n) const;

  ColumnVector row_min (void) const;
  ColumnVector row_max (void) const;

  ColumnVector row_min (Array<octave_idx_type>& index) const;
  ColumnVector row_max (Array<octave_idx_type>& index) const;

  RowVector column_min (void) const;
  RowVector column_max (void) const;

  RowVector column_min (Array<octave_idx_type>& index) const;
  RowVector column_max (Array<octave_idx_type>& index) const;

  // i/o

  friend OCTAVE_API std::ostream& operator << (std::ostream& os,
                                               const Matrix& a);
  friend OCTAVE_API std::istream& operator >> (std::istream& is, Matrix& a);
};

// Publish externally used friend functions.

extern OCTAVE_API Matrix real (const ComplexMatrix& a);
extern OCTAVE_API Matrix imag (const ComplexMatrix& a);

// column vector by row vector -> matrix operations

extern OCTAVE_API Matrix operator * (const ColumnVector& a,
                                     const RowVector& b);

// Other functions.

extern OCTAVE_API Matrix Givens (double, double);

extern OCTAVE_API Matrix Sylvester (const Matrix&, const Matrix&,
                                    const Matrix&);

extern OCTAVE_API Matrix xgemm (const Matrix& a, const Matrix& b,
                                blas_trans_type transa = blas_no_trans,
                                blas_trans_type transb = blas_no_trans);

extern OCTAVE_API Matrix operator * (const Matrix& a, const Matrix& b);

extern OCTAVE_API Matrix min (double d, const Matrix& m);
extern OCTAVE_API Matrix min (const Matrix& m, double d);
extern OCTAVE_API Matrix min (const Matrix& a, const Matrix& b);

extern OCTAVE_API Matrix max (double d, const Matrix& m);
extern OCTAVE_API Matrix max (const Matrix& m, double d);
extern OCTAVE_API Matrix max (const Matrix& a, const Matrix& b);

extern OCTAVE_API Matrix linspace (const ColumnVector& x1,
                                   const ColumnVector& x2,
                                   octave_idx_type n);

MS_CMP_OP_DECLS (Matrix, double, OCTAVE_API)
MS_BOOL_OP_DECLS (Matrix, double, OCTAVE_API)

SM_CMP_OP_DECLS (double, Matrix, OCTAVE_API)
SM_BOOL_OP_DECLS (double, Matrix, OCTAVE_API)

MM_CMP_OP_DECLS (Matrix, Matrix, OCTAVE_API)
MM_BOOL_OP_DECLS (Matrix, Matrix, OCTAVE_API)

MARRAY_FORWARD_DEFS (MArray, Matrix, double)

template <class T>
void read_int (std::istream& is, bool swap_bytes, T& val);

#endif
