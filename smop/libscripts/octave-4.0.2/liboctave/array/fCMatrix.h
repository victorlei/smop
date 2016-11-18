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

#if !defined (octave_fCMatrix_h)
#define octave_fCMatrix_h 1

#include "fCNDArray.h"
#include "MArray.h"
#include "MDiagArray2.h"
#include "MatrixType.h"

#include "mx-defs.h"
#include "mx-op-decl.h"
#include "oct-cmplx.h"
#include "DET.h"

class
OCTAVE_API
FloatComplexMatrix : public FloatComplexNDArray
{
public:

  typedef FloatComplexColumnVector column_vector_type;
  typedef FloatComplexRowVector row_vector_type;

  typedef void (*solve_singularity_handler) (float rcon);

  FloatComplexMatrix (void) : FloatComplexNDArray () { }

  FloatComplexMatrix (octave_idx_type r, octave_idx_type c)
    : FloatComplexNDArray (dim_vector (r, c)) { }

  FloatComplexMatrix (octave_idx_type r, octave_idx_type c,
                      const FloatComplex& val)
    : FloatComplexNDArray (dim_vector (r, c), val) { }

  FloatComplexMatrix (const dim_vector& dv)
    : FloatComplexNDArray (dv.redim (2)) { }

  FloatComplexMatrix (const dim_vector& dv, const FloatComplex& val)
    : FloatComplexNDArray (dv.redim (2), val) { }

  FloatComplexMatrix (const FloatComplexMatrix& a)
    : FloatComplexNDArray (a) { }

  template <class U>
  FloatComplexMatrix (const MArray<U>& a)
    : FloatComplexNDArray (a.as_matrix ()) { }

  template <class U>
  FloatComplexMatrix (const Array<U>& a)
    : FloatComplexNDArray (a.as_matrix ()) { }

  explicit FloatComplexMatrix (const FloatMatrix& a);

  explicit FloatComplexMatrix (const FloatRowVector& rv);

  explicit FloatComplexMatrix (const FloatColumnVector& cv);

  explicit FloatComplexMatrix (const FloatDiagMatrix& a);

  explicit FloatComplexMatrix (const MDiagArray2<float>& a);

  explicit FloatComplexMatrix (const DiagArray2<float>& a);

  explicit FloatComplexMatrix (const FloatComplexRowVector& rv);

  explicit FloatComplexMatrix (const FloatComplexColumnVector& cv);

  explicit FloatComplexMatrix (const FloatComplexDiagMatrix& a);

  explicit FloatComplexMatrix (const MDiagArray2<FloatComplex>& a);

  explicit FloatComplexMatrix (const DiagArray2<FloatComplex>& a);

  explicit FloatComplexMatrix (const boolMatrix& a);

  explicit FloatComplexMatrix (const charMatrix& a);

  FloatComplexMatrix (const FloatMatrix& re, const FloatMatrix& im);

  bool operator == (const FloatComplexMatrix& a) const;
  bool operator != (const FloatComplexMatrix& a) const;

  bool is_hermitian (void) const;

  // destructive insert/delete/reorder operations

  FloatComplexMatrix& insert (const FloatMatrix& a,
                              octave_idx_type r, octave_idx_type c);
  FloatComplexMatrix& insert (const FloatRowVector& a,
                              octave_idx_type r, octave_idx_type c);
  FloatComplexMatrix& insert (const FloatColumnVector& a,
                              octave_idx_type r, octave_idx_type c);
  FloatComplexMatrix& insert (const FloatDiagMatrix& a,
                              octave_idx_type r, octave_idx_type c);

  FloatComplexMatrix& insert (const FloatComplexMatrix& a,
                              octave_idx_type r, octave_idx_type c);
  FloatComplexMatrix& insert (const FloatComplexRowVector& a,
                              octave_idx_type r, octave_idx_type c);
  FloatComplexMatrix& insert (const FloatComplexColumnVector& a,
                              octave_idx_type r, octave_idx_type c);
  FloatComplexMatrix& insert (const FloatComplexDiagMatrix& a,
                              octave_idx_type r, octave_idx_type c);

  FloatComplexMatrix& fill (float val);
  FloatComplexMatrix& fill (const FloatComplex& val);
  FloatComplexMatrix& fill (float val, octave_idx_type r1, octave_idx_type c1,
                            octave_idx_type r2, octave_idx_type c2);
  FloatComplexMatrix& fill (const FloatComplex& val,
                            octave_idx_type r1, octave_idx_type c1,
                            octave_idx_type r2, octave_idx_type c2);

  FloatComplexMatrix append (const FloatMatrix& a) const;
  FloatComplexMatrix append (const FloatRowVector& a) const;
  FloatComplexMatrix append (const FloatColumnVector& a) const;
  FloatComplexMatrix append (const FloatDiagMatrix& a) const;

  FloatComplexMatrix append (const FloatComplexMatrix& a) const;
  FloatComplexMatrix append (const FloatComplexRowVector& a) const;
  FloatComplexMatrix append (const FloatComplexColumnVector& a) const;
  FloatComplexMatrix append (const FloatComplexDiagMatrix& a) const;

  FloatComplexMatrix stack (const FloatMatrix& a) const;
  FloatComplexMatrix stack (const FloatRowVector& a) const;
  FloatComplexMatrix stack (const FloatColumnVector& a) const;
  FloatComplexMatrix stack (const FloatDiagMatrix& a) const;

  FloatComplexMatrix stack (const FloatComplexMatrix& a) const;
  FloatComplexMatrix stack (const FloatComplexRowVector& a) const;
  FloatComplexMatrix stack (const FloatComplexColumnVector& a) const;
  FloatComplexMatrix stack (const FloatComplexDiagMatrix& a) const;

  FloatComplexMatrix hermitian (void) const
  { return MArray<FloatComplex>::hermitian (std::conj); }
  FloatComplexMatrix transpose (void) const
  { return MArray<FloatComplex>::transpose (); }

  friend OCTAVE_API FloatComplexMatrix conj (const FloatComplexMatrix& a);

  // resize is the destructive equivalent for this one

  FloatComplexMatrix extract (octave_idx_type r1, octave_idx_type c1,
                              octave_idx_type r2, octave_idx_type c2) const;

  FloatComplexMatrix extract_n (octave_idx_type r1, octave_idx_type c1,
                                octave_idx_type nr, octave_idx_type nc) const;

  // extract row or column i.

  FloatComplexRowVector row (octave_idx_type i) const;

  FloatComplexColumnVector column (octave_idx_type i) const;

  void resize (octave_idx_type nr, octave_idx_type nc,
               const FloatComplex& rfv = FloatComplex (0))
  {
    MArray<FloatComplex>::resize (dim_vector (nr, nc), rfv);
  }

private:
  FloatComplexMatrix tinverse (MatrixType &mattype, octave_idx_type& info,
                               float& rcon, int force, int calc_cond) const;

  FloatComplexMatrix finverse (MatrixType &mattype, octave_idx_type& info,
                               float& rcon, int force, int calc_cond) const;

public:
  FloatComplexMatrix inverse (void) const;
  FloatComplexMatrix inverse (octave_idx_type& info) const;
  FloatComplexMatrix inverse (octave_idx_type& info, float& rcon,
                              int force = 0, int calc_cond = 1) const;

  FloatComplexMatrix inverse (MatrixType &mattype) const;
  FloatComplexMatrix inverse (MatrixType &mattype, octave_idx_type& info) const;
  FloatComplexMatrix inverse (MatrixType &mattype, octave_idx_type& info,
                              float& rcon, int force = 0,
                              int calc_cond = 1) const;

  FloatComplexMatrix pseudo_inverse (float tol = 0.0) const;

  FloatComplexMatrix fourier (void) const;
  FloatComplexMatrix ifourier (void) const;

  FloatComplexMatrix fourier2d (void) const;
  FloatComplexMatrix ifourier2d (void) const;

  FloatComplexDET determinant (void) const;
  FloatComplexDET determinant (octave_idx_type& info) const;
  FloatComplexDET determinant (octave_idx_type& info, float& rcon,
                               int calc_cond = 1) const;
  FloatComplexDET determinant (MatrixType &mattype, octave_idx_type& info,
                               float& rcon, int calc_cond = 1) const;

  float rcond (void) const;
  float rcond (MatrixType &mattype) const;

private:
  // Upper triangular matrix solvers
  FloatComplexMatrix utsolve (MatrixType &typ, const FloatComplexMatrix& b,
                              octave_idx_type& info, float& rcon,
                              solve_singularity_handler sing_handler,
                              bool calc_cond = false,
                              blas_trans_type transt = blas_no_trans) const;

  // Lower triangular matrix solvers
  FloatComplexMatrix ltsolve (MatrixType &typ, const FloatComplexMatrix& b,
                              octave_idx_type& info, float& rcon,
                              solve_singularity_handler sing_handler,
                              bool calc_cond = false,
                              blas_trans_type transt = blas_no_trans) const;

  // Full matrix solvers (umfpack/cholesky)
  FloatComplexMatrix fsolve (MatrixType &typ, const FloatComplexMatrix& b,
                             octave_idx_type& info, float& rcon,
                             solve_singularity_handler sing_handler,
                             bool calc_cond = false) const;

public:
  // Generic interface to solver with no probing of type
  FloatComplexMatrix solve (MatrixType &typ, const FloatMatrix& b) const;
  FloatComplexMatrix solve (MatrixType &typ, const FloatMatrix& b,
                            octave_idx_type& info) const;
  FloatComplexMatrix solve (MatrixType &typ, const FloatMatrix& b,
                            octave_idx_type& info, float& rcon) const;
  FloatComplexMatrix solve (MatrixType &typ, const FloatMatrix& b,
                            octave_idx_type& info,
                            float& rcon, solve_singularity_handler sing_handler,
                            bool singular_fallback = true,
                            blas_trans_type transt = blas_no_trans) const;

  FloatComplexMatrix solve (MatrixType &typ, const FloatComplexMatrix& b) const;
  FloatComplexMatrix solve (MatrixType &typ, const FloatComplexMatrix& b,
                            octave_idx_type& info) const;
  FloatComplexMatrix solve (MatrixType &typ, const FloatComplexMatrix& b,
                            octave_idx_type& info, float& rcon) const;
  FloatComplexMatrix solve (MatrixType &typ, const FloatComplexMatrix& b,
                            octave_idx_type& info, float& rcon,
                            solve_singularity_handler sing_handler,
                            bool singular_fallback = true,
                            blas_trans_type transt = blas_no_trans) const;

  FloatComplexColumnVector solve (MatrixType &typ,
                                  const FloatColumnVector& b) const;
  FloatComplexColumnVector solve (MatrixType &typ, const FloatColumnVector& b,
                                  octave_idx_type& info) const;
  FloatComplexColumnVector solve (MatrixType &typ, const FloatColumnVector& b,
                                  octave_idx_type& info, float& rcon) const;
  FloatComplexColumnVector solve (MatrixType &typ, const FloatColumnVector& b,
                                  octave_idx_type& info, float& rcon,
                                  solve_singularity_handler sing_handler,
                                  blas_trans_type transt = blas_no_trans) const;

  FloatComplexColumnVector solve (MatrixType &typ,
                                  const FloatComplexColumnVector& b) const;
  FloatComplexColumnVector solve (MatrixType &typ,
                                  const FloatComplexColumnVector& b,
                                  octave_idx_type& info) const;
  FloatComplexColumnVector solve (MatrixType &typ,
                                  const FloatComplexColumnVector& b,
                                  octave_idx_type& info, float& rcon) const;
  FloatComplexColumnVector solve (MatrixType &typ,
                                  const FloatComplexColumnVector& b,
                                  octave_idx_type& info, float& rcon,
                                  solve_singularity_handler sing_handler,
                                  blas_trans_type transt = blas_no_trans) const;

  // Generic interface to solver with probing of type
  FloatComplexMatrix solve (const FloatMatrix& b) const;
  FloatComplexMatrix solve (const FloatMatrix& b, octave_idx_type& info) const;
  FloatComplexMatrix solve (const FloatMatrix& b, octave_idx_type& info,
                            float& rcon) const;
  FloatComplexMatrix solve (const FloatMatrix& b, octave_idx_type& info,
                            float& rcon,
                            solve_singularity_handler sing_handler,
                            blas_trans_type transt = blas_no_trans) const;

  FloatComplexMatrix solve (const FloatComplexMatrix& b) const;
  FloatComplexMatrix solve (const FloatComplexMatrix& b,
                            octave_idx_type& info) const;
  FloatComplexMatrix solve (const FloatComplexMatrix& b, octave_idx_type& info,
                            float& rcon) const;
  FloatComplexMatrix solve (const FloatComplexMatrix& b, octave_idx_type& info,
                            float& rcon,
                            solve_singularity_handler sing_handler,
                            blas_trans_type transt = blas_no_trans) const;

  FloatComplexColumnVector solve (const FloatColumnVector& b) const;
  FloatComplexColumnVector solve (const FloatColumnVector& b,
                                  octave_idx_type& info) const;
  FloatComplexColumnVector solve (const FloatColumnVector& b,
                                  octave_idx_type& info,
                                  float& rcon) const;
  FloatComplexColumnVector solve (const FloatColumnVector& b,
                                  octave_idx_type& info, float& rcon,
                                  solve_singularity_handler sing_handler,
                                  blas_trans_type transt = blas_no_trans) const;

  FloatComplexColumnVector solve (const FloatComplexColumnVector& b) const;
  FloatComplexColumnVector solve (const FloatComplexColumnVector& b,
                                  octave_idx_type& info) const;
  FloatComplexColumnVector solve (const FloatComplexColumnVector& b,
                                  octave_idx_type& info,
                                  float& rcon) const;
  FloatComplexColumnVector solve (const FloatComplexColumnVector& b,
                                  octave_idx_type& info,
                                  float& rcon,
                                  solve_singularity_handler sing_handler,
                                  blas_trans_type transt = blas_no_trans) const;

  FloatComplexMatrix lssolve (const FloatMatrix& b) const;
  FloatComplexMatrix lssolve (const FloatMatrix& b,
                              octave_idx_type& info) const;
  FloatComplexMatrix lssolve (const FloatMatrix& b, octave_idx_type& info,
                              octave_idx_type& rank) const;
  FloatComplexMatrix lssolve (const FloatMatrix& b, octave_idx_type& info,
                              octave_idx_type& rank, float& rcon) const;

  FloatComplexMatrix lssolve (const FloatComplexMatrix& b) const;
  FloatComplexMatrix lssolve (const FloatComplexMatrix& b,
                              octave_idx_type& info) const;
  FloatComplexMatrix lssolve (const FloatComplexMatrix& b,
                              octave_idx_type& info, octave_idx_type& rank) const;
  FloatComplexMatrix lssolve (const FloatComplexMatrix& b,
                              octave_idx_type& info, octave_idx_type& rank, float& rcon) const;

  FloatComplexColumnVector lssolve (const FloatColumnVector& b) const;
  FloatComplexColumnVector lssolve (const FloatColumnVector& b,
                                    octave_idx_type& info) const;
  FloatComplexColumnVector lssolve (const FloatColumnVector& b,
                                    octave_idx_type& info,
                                    octave_idx_type& rank) const;
  FloatComplexColumnVector lssolve (const FloatColumnVector& b,
                                    octave_idx_type& info,
                                    octave_idx_type& rank, float& rcon) const;

  FloatComplexColumnVector lssolve (const FloatComplexColumnVector& b) const;
  FloatComplexColumnVector lssolve (const FloatComplexColumnVector& b,
                                    octave_idx_type& info) const;
  FloatComplexColumnVector lssolve (const FloatComplexColumnVector& b,
                                    octave_idx_type& info,
                                    octave_idx_type& rank) const;
  FloatComplexColumnVector lssolve (const FloatComplexColumnVector& b,
                                    octave_idx_type& info,
                                    octave_idx_type& rank, float& rcon) const;

  // matrix by diagonal matrix -> matrix operations

  FloatComplexMatrix& operator += (const FloatDiagMatrix& a);
  FloatComplexMatrix& operator -= (const FloatDiagMatrix& a);

  FloatComplexMatrix& operator += (const FloatComplexDiagMatrix& a);
  FloatComplexMatrix& operator -= (const FloatComplexDiagMatrix& a);

  // matrix by matrix -> matrix operations

  FloatComplexMatrix& operator += (const FloatMatrix& a);
  FloatComplexMatrix& operator -= (const FloatMatrix& a);

  // unary operations

  boolMatrix operator ! (void) const;

  // other operations

  boolMatrix all (int dim = -1) const;
  boolMatrix any (int dim = -1) const;

  FloatComplexMatrix cumprod (int dim = -1) const;
  FloatComplexMatrix cumsum (int dim = -1) const;
  FloatComplexMatrix prod (int dim = -1) const;
  FloatComplexMatrix sum (int dim = -1) const;
  FloatComplexMatrix sumsq (int dim = -1) const;
  FloatMatrix abs (void) const;

  FloatComplexMatrix diag (octave_idx_type k = 0) const;

  FloatComplexDiagMatrix diag (octave_idx_type m, octave_idx_type n) const;

  bool row_is_real_only (octave_idx_type) const;
  bool column_is_real_only (octave_idx_type) const;

  FloatComplexColumnVector row_min (void) const;
  FloatComplexColumnVector row_max (void) const;

  FloatComplexColumnVector row_min (Array<octave_idx_type>& index) const;
  FloatComplexColumnVector row_max (Array<octave_idx_type>& index) const;

  FloatComplexRowVector column_min (void) const;
  FloatComplexRowVector column_max (void) const;

  FloatComplexRowVector column_min (Array<octave_idx_type>& index) const;
  FloatComplexRowVector column_max (Array<octave_idx_type>& index) const;

  // i/o

  friend OCTAVE_API std::ostream& operator << (std::ostream& os,
                                               const FloatComplexMatrix& a);
  friend OCTAVE_API std::istream& operator >> (std::istream& is,
                                               FloatComplexMatrix& a);
};

extern OCTAVE_API FloatComplexMatrix conj (const FloatComplexMatrix& a);

// column vector by row vector -> matrix operations

extern OCTAVE_API FloatComplexMatrix
operator * (const FloatColumnVector& a, const FloatComplexRowVector& b);

extern OCTAVE_API FloatComplexMatrix
operator * (const FloatComplexColumnVector& a, const FloatRowVector& b);

extern OCTAVE_API FloatComplexMatrix
operator * (const FloatComplexColumnVector& a, const FloatComplexRowVector& b);

extern OCTAVE_API FloatComplexMatrix
Givens (const FloatComplex&, const FloatComplex&);

extern OCTAVE_API FloatComplexMatrix
Sylvester (const FloatComplexMatrix&, const FloatComplexMatrix&,
           const FloatComplexMatrix&);

extern OCTAVE_API FloatComplexMatrix
xgemm (const FloatComplexMatrix& a, const FloatComplexMatrix& b,
       blas_trans_type transa = blas_no_trans,
       blas_trans_type transb = blas_no_trans);

extern OCTAVE_API FloatComplexMatrix operator * (const FloatMatrix&,
                                                 const FloatComplexMatrix&);
extern OCTAVE_API FloatComplexMatrix operator * (const FloatComplexMatrix&,
                                                 const FloatMatrix&);
extern OCTAVE_API FloatComplexMatrix operator * (const FloatComplexMatrix&,
                                                 const FloatComplexMatrix&);

extern OCTAVE_API FloatComplexMatrix min (const FloatComplex& c,
                                          const FloatComplexMatrix& m);
extern OCTAVE_API FloatComplexMatrix min (const FloatComplexMatrix& m,
                                          const FloatComplex& c);
extern OCTAVE_API FloatComplexMatrix min (const FloatComplexMatrix& a,
                                          const FloatComplexMatrix& b);

extern OCTAVE_API FloatComplexMatrix max (const FloatComplex& c,
                                          const FloatComplexMatrix& m);
extern OCTAVE_API FloatComplexMatrix max (const FloatComplexMatrix& m,
                                          const FloatComplex& c);
extern OCTAVE_API FloatComplexMatrix max (const FloatComplexMatrix& a,
                                          const FloatComplexMatrix& b);

extern OCTAVE_API FloatComplexMatrix
linspace (const FloatComplexColumnVector& x1,
          const FloatComplexColumnVector& x2,
          octave_idx_type n);

MS_CMP_OP_DECLS (FloatComplexMatrix, FloatComplex, OCTAVE_API)
MS_BOOL_OP_DECLS (FloatComplexMatrix, FloatComplex, OCTAVE_API)

SM_CMP_OP_DECLS (FloatComplex, FloatComplexMatrix, OCTAVE_API)
SM_BOOL_OP_DECLS (FloatComplex, FloatComplexMatrix, OCTAVE_API)

MM_CMP_OP_DECLS (FloatComplexMatrix, FloatComplexMatrix, OCTAVE_API)
MM_BOOL_OP_DECLS (FloatComplexMatrix, FloatComplexMatrix, OCTAVE_API)

MARRAY_FORWARD_DEFS (MArray, FloatComplexMatrix, FloatComplex)

#endif
