/*

Copyright (C) 2006-2015 David Bateman
Copyright (C) 2006 Andy Adler

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

#if !defined (octave_MatrixType_h)
#define octave_MatrixType_h

class Matrix;
class ComplexMatrix;
class FloatMatrix;
class FloatComplexMatrix;
class SparseMatrix;
class SparseComplexMatrix;

class
OCTAVE_API
MatrixType
{
public:
  enum matrix_type
  {
    Unknown = 0,
    Full,
    Diagonal,
    Permuted_Diagonal,
    Upper,
    Lower,
    Permuted_Upper,
    Permuted_Lower,
    Banded,
    Hermitian,
    Banded_Hermitian,
    Tridiagonal,
    Tridiagonal_Hermitian,
    Rectangular
  };

  MatrixType (void);

  MatrixType (const MatrixType &a);

  MatrixType (const Matrix &a);

  MatrixType (const ComplexMatrix &a);

  MatrixType (const FloatMatrix &a);

  MatrixType (const FloatComplexMatrix &a);

  MatrixType (const SparseMatrix &a);

  MatrixType (const SparseComplexMatrix &a);

  MatrixType (const matrix_type t, bool _full = false);

  MatrixType (const matrix_type t, const octave_idx_type np,
              const octave_idx_type *p, bool _full = false);

  MatrixType (const matrix_type t, const octave_idx_type ku,
              const octave_idx_type kl, bool _full = false);

  ~MatrixType (void);

  MatrixType& operator = (const MatrixType& a);

  int type (bool quiet = true);

  int type (const Matrix &a);

  int type (const ComplexMatrix &a);

  int type (const FloatMatrix &a);

  int type (const FloatComplexMatrix &a);

  int type (const SparseMatrix &a);

  int type (const SparseComplexMatrix &a);

  double band_density (void) const { return bandden; }

  int nupper (void) const { return upper_band; }

  int nlower (void) const { return lower_band; }

  bool is_dense (void) const { return dense; }

  bool is_diagonal (void) const
  { return (typ == Diagonal || typ == Permuted_Diagonal); }

  bool is_upper_triangular (void) const
  { return (typ == Upper || typ == Permuted_Upper); }

  bool is_lower_triangular (void) const
  { return (typ == Lower || typ == Permuted_Lower); }

  bool is_banded (void)
  { return (typ == Banded || typ == Banded_Hermitian); }

  bool is_tridiagonal (void) const
  { return (typ == Tridiagonal || typ == Tridiagonal_Hermitian); }

  bool is_hermitian (void) const
  {
    return (typ == Banded_Hermitian || typ == Tridiagonal_Hermitian
            || typ == Hermitian);
  }

  bool is_rectangular (void) const { return (typ == Rectangular); }

  bool is_known (void) const { return (typ != Unknown); }

  bool is_unknown (void) const { return (typ == Unknown); }

  void info (void) const;

  octave_idx_type * triangular_perm (void) const { return perm; }

  void invalidate_type (void) { typ = Unknown; }

  void mark_as_diagonal (void) { typ = Diagonal; }

  void mark_as_permuted_diagonal (void) { typ = Permuted_Diagonal; }

  void mark_as_upper_triangular (void) { typ = Upper; }

  void mark_as_lower_triangular (void) { typ = Lower; }

  void mark_as_tridiagonal (void) {typ = Tridiagonal; }

  void mark_as_banded (const octave_idx_type ku, const octave_idx_type kl)
  { typ = Banded; upper_band = ku; lower_band = kl; }

  void mark_as_full (void) { typ = Full; }

  void mark_as_rectangular (void) { typ = Rectangular; }

  void mark_as_dense (void) { dense = true; }

  void mark_as_not_dense (void) { dense = false; }

  void mark_as_symmetric (void);

  void mark_as_unsymmetric (void);

  void mark_as_permuted (const octave_idx_type np, const octave_idx_type *p);

  void mark_as_unpermuted (void);

  MatrixType transpose (void) const;

private:
  void type (int new_typ) { typ = static_cast<matrix_type>(new_typ); }

  matrix_type typ;
  double sp_bandden;
  double bandden;
  octave_idx_type upper_band;
  octave_idx_type lower_band;
  bool dense;
  bool full;
  octave_idx_type nperm;
  octave_idx_type *perm;
};

#endif
