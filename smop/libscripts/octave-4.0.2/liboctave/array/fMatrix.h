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

#if !defined (octave_fMatrix_h)
#define octave_fMatrix_h 1

#include "fNDArray.h"
#include "MArray.h"
#include "MDiagArray2.h"
#include "MatrixType.h"

#include "mx-defs.h"
#include "mx-op-decl.h"
#include "DET.h"

class
OCTAVE_API
FloatMatrix : public FloatNDArray
{
public:

  typedef FloatColumnVector column_vector_type;
  typedef FloatRowVector row_vector_type;

  typedef void (*solve_singularity_handler) (float rcon);

  FloatMatrix (void) : FloatNDArray () { }

  FloatMatrix (octave_idx_type r, octave_idx_type c)
    : FloatNDArray (dim_vector (r, c)) { }

  FloatMatrix (octave_idx_type r, octave_idx_type c, float val)
    : FloatNDArray (dim_vector (r, c), val) { }

  FloatMatrix (const dim_vector& dv) : FloatNDArray (dv.redim (2)) { }

  FloatMatrix (const dim_vector& dv, float val)
    : FloatNDArray (dv.redim (2), val) { }

  FloatMatrix (const FloatMatrix& a) : FloatNDArray (a) { }

  template <class U>
  FloatMatrix (const MArray<U>& a) : FloatNDArray (a.as_matrix ()) { }

  template <class U>
  FloatMatrix (const Array<U>& a) : FloatNDArray (a.as_matrix ()) { }

  explicit FloatMatrix (const FloatRowVector& rv);

  explicit FloatMatrix (const FloatColumnVector& cv);

  explicit FloatMatrix (const FloatDiagMatrix& a);

  explicit FloatMatrix (const MDiagArray2<float>& a);

  explicit FloatMatrix (const DiagArray2<float>& a);

  explicit FloatMatrix (const PermMatrix& a);

  explicit FloatMatrix (const boolMatrix& a);

  explicit FloatMatrix (const charMatrix& a);

  bool operator == (const FloatMatrix& a) const;
  bool operator != (const FloatMatrix& a) const;

  bool is_symmetric (void) const;

  // destructive insert/delete/reorder operations

  FloatMatrix& insert (const FloatMatrix& a,
                       octave_idx_type r, octave_idx_type c);
  FloatMatrix& insert (const FloatRowVector& a,
                       octave_idx_type r, octave_idx_type c);
  FloatMatrix& insert (const FloatColumnVector& a,
                       octave_idx_type r, octave_idx_type c);
  FloatMatrix& insert (const FloatDiagMatrix& a,
                       octave_idx_type r, octave_idx_type c);

  FloatMatrix& fill (float val);
  FloatMatrix& fill (float val, octave_idx_type r1, octave_idx_type c1,
                     octave_idx_type r2, octave_idx_type c2);

  FloatMatrix append (const FloatMatrix& a) const;
  FloatMatrix append (const FloatRowVector& a) const;
  FloatMatrix append (const FloatColumnVector& a) const;
  FloatMatrix append (const FloatDiagMatrix& a) const;

  FloatMatrix stack (const FloatMatrix& a) const;
  FloatMatrix stack (const FloatRowVector& a) const;
  FloatMatrix stack (const FloatColumnVector& a) const;
  FloatMatrix stack (const FloatDiagMatrix& a) const;

  friend OCTAVE_API FloatMatrix real (const FloatComplexMatrix& a);
  friend OCTAVE_API FloatMatrix imag (const FloatComplexMatrix& a);

  friend class FloatComplexMatrix;

  FloatMatrix transpose (void) const { return MArray<float>::transpose (); }

  // resize is the destructive equivalent for this one

  FloatMatrix extract (octave_idx_type r1, octave_idx_type c1,
                       octave_idx_type r2, octave_idx_type c2) const;

  FloatMatrix extract_n (octave_idx_type r1, octave_idx_type c1,
                         octave_idx_type nr, octave_idx_type nc) const;

  // extract row or column i.

  FloatRowVector row (octave_idx_type i) const;

  FloatColumnVector column (octave_idx_type i) const;

  void resize (octave_idx_type nr, octave_idx_type nc, float rfv = 0)
  {
    MArray<float>::resize (dim_vector (nr, nc), rfv);
  }

private:
  FloatMatrix tinverse (MatrixType &mattype, octave_idx_type& info,
                        float& rcon, int force, int calc_cond) const;

  FloatMatrix finverse (MatrixType &mattype, octave_idx_type& info,
                        float& rcon, int force, int calc_cond) const;

public:
  FloatMatrix inverse (void) const;
  FloatMatrix inverse (octave_idx_type& info) const;
  FloatMatrix inverse (octave_idx_type& info, float& rcon, int force = 0,
                       int calc_cond = 1) const;

  FloatMatrix inverse (MatrixType &mattype) const;
  FloatMatrix inverse (MatrixType &mattype, octave_idx_type& info) const;
  FloatMatrix inverse (MatrixType &mattype, octave_idx_type& info, float& rcon,
                       int force = 0, int calc_cond = 1) const;

  FloatMatrix pseudo_inverse (float tol = 0.0) const;

  FloatComplexMatrix fourier (void) const;
  FloatComplexMatrix ifourier (void) const;

  FloatComplexMatrix fourier2d (void) const;
  FloatComplexMatrix ifourier2d (void) const;

  FloatDET determinant (void) const;
  FloatDET determinant (octave_idx_type& info) const;
  FloatDET determinant (octave_idx_type& info, float& rcon,
                        int calc_cond = 1) const;
  FloatDET determinant (MatrixType &mattype, octave_idx_type& info,
                        float& rcon, int calc_cond = 1) const;

  float rcond (void) const;
  float rcond (MatrixType &mattype) const;

private:
  // Upper triangular matrix solvers
  FloatMatrix utsolve (MatrixType &typ, const FloatMatrix& b,
                       octave_idx_type& info,
                       float& rcon, solve_singularity_handler sing_handler,
                       bool calc_cond = false,
                       blas_trans_type transt = blas_no_trans) const;

  // Lower triangular matrix solvers
  FloatMatrix ltsolve (MatrixType &typ, const FloatMatrix& b,
                       octave_idx_type& info,
                       float& rcon, solve_singularity_handler sing_handler,
                       bool calc_cond = false,
                       blas_trans_type transt = blas_no_trans) const;

  // Full matrix solvers (lu/cholesky)
  FloatMatrix fsolve (MatrixType &typ, const FloatMatrix& b,
                      octave_idx_type& info,
                      float& rcon, solve_singularity_handler sing_handler,
                      bool calc_cond = false) const;

public:
  // Generic interface to solver with no probing of type
  FloatMatrix solve (MatrixType &typ, const FloatMatrix& b) const;
  FloatMatrix solve (MatrixType &typ, const FloatMatrix& b,
                     octave_idx_type& info) const;
  FloatMatrix solve (MatrixType &typ, const FloatMatrix& b,
                     octave_idx_type& info, float& rcon) const;
  FloatMatrix solve (MatrixType &typ, const FloatMatrix& b,
                     octave_idx_type& info, float& rcon,
                     solve_singularity_handler sing_handler,
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

  FloatColumnVector solve (MatrixType &typ, const FloatColumnVector& b) const;
  FloatColumnVector solve (MatrixType &typ, const FloatColumnVector& b,
                           octave_idx_type& info) const;
  FloatColumnVector solve (MatrixType &typ, const FloatColumnVector& b,
                           octave_idx_type& info, float& rcon) const;
  FloatColumnVector solve (MatrixType &typ, const FloatColumnVector& b,
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
  FloatMatrix solve (const FloatMatrix& b) const;
  FloatMatrix solve (const FloatMatrix& b, octave_idx_type& info) const;
  FloatMatrix solve (const FloatMatrix& b, octave_idx_type& info,
                     float& rcon) const;
  FloatMatrix solve (const FloatMatrix& b, octave_idx_type& info, float& rcon,
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

  FloatColumnVector solve (const FloatColumnVector& b) const;
  FloatColumnVector solve (const FloatColumnVector& b,
                           octave_idx_type& info) const;
  FloatColumnVector solve (const FloatColumnVector& b, octave_idx_type& info,
                           float& rcon) const;
  FloatColumnVector solve (const FloatColumnVector& b, octave_idx_type& info,
                           float& rcon,
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

  // Singular solvers
  FloatMatrix lssolve (const FloatMatrix& b) const;
  FloatMatrix lssolve (const FloatMatrix& b, octave_idx_type& info) const;
  FloatMatrix lssolve (const FloatMatrix& b, octave_idx_type& info,
                       octave_idx_type& rank) const;
  FloatMatrix lssolve (const FloatMatrix& b, octave_idx_type& info,
                       octave_idx_type& rank, float& rcon) const;

  FloatComplexMatrix lssolve (const FloatComplexMatrix& b) const;
  FloatComplexMatrix lssolve (const FloatComplexMatrix& b,
                              octave_idx_type& info) const;
  FloatComplexMatrix lssolve (const FloatComplexMatrix& b,
                              octave_idx_type& info,
                              octave_idx_type& rank) const;
  FloatComplexMatrix lssolve (const FloatComplexMatrix& b,
                              octave_idx_type& info, octave_idx_type& rank,
                              float &rcon) const;

  FloatColumnVector lssolve (const FloatColumnVector& b) const;
  FloatColumnVector lssolve (const FloatColumnVector& b,
                             octave_idx_type& info) const;
  FloatColumnVector lssolve (const FloatColumnVector& b, octave_idx_type& info,
                             octave_idx_type& rank) const;
  FloatColumnVector lssolve (const FloatColumnVector& b, octave_idx_type& info,
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

  FloatMatrix& operator += (const FloatDiagMatrix& a);
  FloatMatrix& operator -= (const FloatDiagMatrix& a);

  FloatMatrix cumprod (int dim = -1) const;
  FloatMatrix cumsum (int dim = -1) const;
  FloatMatrix prod (int dim = -1) const;
  FloatMatrix sum (int dim = -1) const;
  FloatMatrix sumsq (int dim = -1) const;
  FloatMatrix abs (void) const;

  FloatMatrix diag (octave_idx_type k = 0) const;

  FloatDiagMatrix diag (octave_idx_type m, octave_idx_type n) const;

  FloatColumnVector row_min (void) const;
  FloatColumnVector row_max (void) const;

  FloatColumnVector row_min (Array<octave_idx_type>& index) const;
  FloatColumnVector row_max (Array<octave_idx_type>& index) const;

  FloatRowVector column_min (void) const;
  FloatRowVector column_max (void) const;

  FloatRowVector column_min (Array<octave_idx_type>& index) const;
  FloatRowVector column_max (Array<octave_idx_type>& index) const;

  // i/o

  friend OCTAVE_API std::ostream& operator << (std::ostream& os,
                                               const FloatMatrix& a);
  friend OCTAVE_API std::istream& operator >> (std::istream& is,
                                               FloatMatrix& a);
};

// Publish externally used friend functions.

extern OCTAVE_API FloatMatrix real (const FloatComplexMatrix& a);
extern OCTAVE_API FloatMatrix imag (const FloatComplexMatrix& a);

// column vector by row vector -> matrix operations

extern OCTAVE_API FloatMatrix operator * (const FloatColumnVector& a,
                                          const FloatRowVector& b);

// Other functions.

extern OCTAVE_API FloatMatrix Givens (float, float);

extern OCTAVE_API FloatMatrix Sylvester (const FloatMatrix&, const FloatMatrix&,
                                         const FloatMatrix&);

extern OCTAVE_API FloatMatrix xgemm (const FloatMatrix& a, const FloatMatrix& b,
                                     blas_trans_type transa = blas_no_trans,
                                     blas_trans_type transb = blas_no_trans);

extern OCTAVE_API FloatMatrix operator * (const FloatMatrix& a,
                                          const FloatMatrix& b);

extern OCTAVE_API FloatMatrix min (float d, const FloatMatrix& m);
extern OCTAVE_API FloatMatrix min (const FloatMatrix& m, float d);
extern OCTAVE_API FloatMatrix min (const FloatMatrix& a, const FloatMatrix& b);

extern OCTAVE_API FloatMatrix max (float d, const FloatMatrix& m);
extern OCTAVE_API FloatMatrix max (const FloatMatrix& m, float d);
extern OCTAVE_API FloatMatrix max (const FloatMatrix& a, const FloatMatrix& b);

extern OCTAVE_API FloatMatrix linspace (const FloatColumnVector& x1,
                                        const FloatColumnVector& x2,
                                        octave_idx_type n);


MS_CMP_OP_DECLS (FloatMatrix, float, OCTAVE_API)
MS_BOOL_OP_DECLS (FloatMatrix, float, OCTAVE_API)

SM_CMP_OP_DECLS (float, FloatMatrix, OCTAVE_API)
SM_BOOL_OP_DECLS (float, FloatMatrix, OCTAVE_API)

MM_CMP_OP_DECLS (FloatMatrix, FloatMatrix, OCTAVE_API)
MM_BOOL_OP_DECLS (FloatMatrix, FloatMatrix, OCTAVE_API)

MARRAY_FORWARD_DEFS (MArray, FloatMatrix, float)

template <class T>
void read_int (std::istream& is, bool swap_bytes, T& val);

#endif
