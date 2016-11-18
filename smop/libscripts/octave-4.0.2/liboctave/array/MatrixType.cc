/*

Copyright (C) 2006-2015 David Bateman
Copyright (C) 2006 Andy Adler
Copyright (C) 2009 VZLU Prague

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

#include <vector>

#include "MatrixType.h"
#include "dMatrix.h"
#include "fMatrix.h"
#include "CMatrix.h"
#include "fCMatrix.h"
#include "dSparse.h"
#include "CSparse.h"
#include "oct-spparms.h"
#include "oct-locbuf.h"

static void
warn_cached (void)
{
  (*current_liboctave_warning_with_id_handler)
    ("Octave:matrix-type-info", "using cached matrix type");
}

static void
warn_invalid (void)
{
  (*current_liboctave_warning_with_id_handler)
    ("Octave:matrix-type-info", "invalid matrix type");
}

static void
warn_calculating_sparse_type (void)
{
  (*current_liboctave_warning_with_id_handler)
    ("Octave:matrix-type-info", "calculating sparse matrix type");
}

// FIXME: There is a large code duplication here

MatrixType::MatrixType (void)
  : typ (MatrixType::Unknown),
    sp_bandden (octave_sparse_params::get_bandden ()),
    bandden (0), upper_band (0),
    lower_band (0), dense (false), full (false), nperm (0), perm (0) { }

MatrixType::MatrixType (const MatrixType &a)
  : typ (a.typ), sp_bandden (a.sp_bandden), bandden (a.bandden),
    upper_band (a.upper_band), lower_band (a.lower_band),
    dense (a.dense), full (a.full), nperm (a.nperm), perm (0)
{
  if (nperm != 0)
    {
      perm = new octave_idx_type [nperm];
      for (octave_idx_type i = 0; i < nperm; i++)
        perm[i] = a.perm[i];
    }
}

template<class T>
MatrixType::matrix_type
matrix_real_probe (const MArray<T>& a)
{
  MatrixType::matrix_type typ;
  octave_idx_type nrows = a.rows ();
  octave_idx_type ncols = a.cols ();

  const T zero = 0;

  if (ncols == nrows)
    {
      bool upper = true;
      bool lower = true;
      bool hermitian = true;

      // do the checks for lower/upper/hermitian all in one pass.
      OCTAVE_LOCAL_BUFFER (T, diag, ncols);

      for (octave_idx_type j = 0;
           j < ncols && upper; j++)
        {
          T d = a.elem (j,j);
          upper = upper && (d != zero);
          lower = lower && (d != zero);
          hermitian = hermitian && (d > zero);
          diag[j] = d;
        }

      for (octave_idx_type j = 0;
           j < ncols && (upper || lower || hermitian); j++)
        {
          for (octave_idx_type i = 0; i < j; i++)
            {
              double aij = a.elem (i,j);
              double aji = a.elem (j,i);
              lower = lower && (aij == zero);
              upper = upper && (aji == zero);
              hermitian = hermitian && (aij == aji
                                        && aij*aij < diag[i]*diag[j]);
            }
        }

      if (upper)
        typ = MatrixType::Upper;
      else if (lower)
        typ = MatrixType::Lower;
      else if (hermitian)
        typ = MatrixType::Hermitian;
      else
        typ = MatrixType::Full;
    }
  else
    typ = MatrixType::Rectangular;

  return typ;
}

template<class T>
MatrixType::matrix_type
matrix_complex_probe (const MArray<std::complex<T> >& a)
{
  MatrixType::matrix_type typ = MatrixType::Unknown;
  octave_idx_type nrows = a.rows ();
  octave_idx_type ncols = a.cols ();

  const T zero = 0;
  // get the real type

  if (ncols == nrows)
    {
      bool upper = true;
      bool lower = true;
      bool hermitian = true;

      // do the checks for lower/upper/hermitian all in one pass.
      OCTAVE_LOCAL_BUFFER (T, diag, ncols);

      for (octave_idx_type j = 0;
           j < ncols && upper; j++)
        {
          std::complex<T> d = a.elem (j,j);
          upper = upper && (d != zero);
          lower = lower && (d != zero);
          hermitian = hermitian && (d.real () > zero && d.imag () == zero);
          diag[j] = d.real ();
        }

      for (octave_idx_type j = 0;
           j < ncols && (upper || lower || hermitian); j++)
        {
          for (octave_idx_type i = 0; i < j; i++)
            {
              std::complex<T> aij = a.elem (i,j);
              std::complex<T> aji = a.elem (j,i);
              lower = lower && (aij == zero);
              upper = upper && (aji == zero);
              hermitian = hermitian && (aij == std::conj (aji)
                                        && std::norm (aij) < diag[i]*diag[j]);
            }
        }


      if (upper)
        typ = MatrixType::Upper;
      else if (lower)
        typ = MatrixType::Lower;
      else if (hermitian)
        typ = MatrixType::Hermitian;
      else if (ncols == nrows)
        typ = MatrixType::Full;
    }
  else
    typ = MatrixType::Rectangular;

  return typ;
}

MatrixType::MatrixType (const Matrix &a)
  : typ (MatrixType::Unknown),
    sp_bandden (0), bandden (0), upper_band (0), lower_band (0),
    dense (false), full (true), nperm (0), perm (0)
{
  typ = matrix_real_probe (a);
}

MatrixType::MatrixType (const ComplexMatrix &a)
  : typ (MatrixType::Unknown),
    sp_bandden (0), bandden (0), upper_band (0), lower_band (0),
    dense (false), full (true), nperm (0), perm (0)
{
  typ = matrix_complex_probe (a);
}


MatrixType::MatrixType (const FloatMatrix &a)
  : typ (MatrixType::Unknown),
    sp_bandden (0), bandden (0), upper_band (0), lower_band (0),
    dense (false), full (true), nperm (0), perm (0)
{
  typ = matrix_real_probe (a);
}

MatrixType::MatrixType (const FloatComplexMatrix &a)
  : typ (MatrixType::Unknown),
    sp_bandden (0), bandden (0), upper_band (0), lower_band (0),
    dense (false), full (true), nperm (0), perm (0)
{
  typ = matrix_complex_probe (a);
}

MatrixType::MatrixType (const SparseMatrix &a)
  : typ (MatrixType::Unknown),
    sp_bandden (0), bandden (0), upper_band (0), lower_band (0),
    dense (false), full (false), nperm (0), perm (0)
{
  octave_idx_type nrows = a.rows ();
  octave_idx_type ncols = a.cols ();
  octave_idx_type nm = (ncols < nrows ? ncols : nrows);
  octave_idx_type nnz = a.nnz ();

  if (octave_sparse_params::get_key ("spumoni") != 0.)
    warn_calculating_sparse_type ();

  sp_bandden = octave_sparse_params::get_bandden ();
  bool maybe_hermitian = false;
  typ = MatrixType::Full;

  if (nnz == nm)
    {
      matrix_type tmp_typ = MatrixType::Diagonal;
      octave_idx_type i;
      // Maybe the matrix is diagonal
      for (i = 0; i < nm; i++)
        {
          if (a.cidx (i+1) != a.cidx (i) + 1)
            {
              tmp_typ = MatrixType::Full;
              break;
            }
          if (a.ridx (i) != i)
            {
              tmp_typ = MatrixType::Permuted_Diagonal;
              break;
            }
        }

      if (tmp_typ == MatrixType::Permuted_Diagonal)
        {
          std::vector<bool> found (nrows);

          for (octave_idx_type j = 0; j < i; j++)
            found[j] = true;
          for (octave_idx_type j = i; j < nrows; j++)
            found[j] = false;

          for (octave_idx_type j = i; j < nm; j++)
            {
              if ((a.cidx (j+1) > a.cidx (j) + 1)
                  || ((a.cidx (j+1) == a.cidx (j) + 1) && found[a.ridx (j)]))
                {
                  tmp_typ = MatrixType::Full;
                  break;
                }
              found[a.ridx (j)] = true;
            }
        }
      typ = tmp_typ;
    }

  if (typ == MatrixType::Full)
    {
      // Search for banded, upper and lower triangular matrices
      bool singular = false;
      upper_band = 0;
      lower_band = 0;
      for (octave_idx_type j = 0; j < ncols; j++)
        {
          bool zero_on_diagonal = false;
          if (j < nrows)
            {
              zero_on_diagonal = true;
              for (octave_idx_type i = a.cidx (j); i < a.cidx (j+1); i++)
                if (a.ridx (i) == j)
                  {
                    zero_on_diagonal = false;
                    break;
                  }
            }

          if (zero_on_diagonal)
            {
              singular = true;
              break;
            }

          if (a.cidx (j+1) != a.cidx (j))
            {
              octave_idx_type ru = a.ridx (a.cidx (j));
              octave_idx_type rl = a.ridx (a.cidx (j+1)-1);

              if (j - ru > upper_band)
                upper_band = j - ru;

              if (rl - j > lower_band)
                lower_band = rl - j;
            }
        }

      if (!singular)
        {
          bandden = double (nnz) /
                    (double (ncols) * (double (lower_band) +
                                       double (upper_band)) -
                     0.5 * double (upper_band + 1) * double (upper_band) -
                     0.5 * double (lower_band + 1) * double (lower_band));

          if (nrows == ncols && sp_bandden != 1. && bandden > sp_bandden)
            {
              if (upper_band == 1 && lower_band == 1)
                typ = MatrixType::Tridiagonal;
              else
                typ = MatrixType::Banded;

              octave_idx_type nnz_in_band =
                (upper_band + lower_band + 1) * nrows -
                (1 + upper_band) * upper_band / 2 -
                (1 + lower_band) * lower_band / 2;
              if (nnz_in_band == nnz)
                dense = true;
              else
                dense = false;
            }
          else if (upper_band == 0)
            typ = MatrixType::Lower;
          else if (lower_band == 0)
            typ = MatrixType::Upper;

          if (upper_band == lower_band && nrows == ncols)
            maybe_hermitian = true;
        }

      if (typ == MatrixType::Full)
        {
          // Search for a permuted triangular matrix, and test if
          // permutation is singular

          // FIXME: Perhaps this should be based on a dmperm algorithm?
          bool found = false;

          nperm = ncols;
          perm = new octave_idx_type [ncols];

          for (octave_idx_type i = 0; i < ncols; i++)
            perm[i] = -1;

          for (octave_idx_type i = 0; i < nm; i++)
            {
              found = false;

              for (octave_idx_type j = 0; j < ncols; j++)
                {
                  if ((a.cidx (j+1) - a.cidx (j)) > 0
                      && (a.ridx (a.cidx (j+1)-1) == i))
                    {
                      perm[i] = j;
                      found = true;
                      break;
                    }
                }

              if (!found)
                break;
            }

          if (found)
            {
              typ = MatrixType::Permuted_Upper;
              if (ncols > nrows)
                {
                  octave_idx_type k = nrows;
                  for (octave_idx_type i = 0; i < ncols; i++)
                    if (perm[i] == -1)
                      perm[i] = k++;
                }
            }
          else if (a.cidx (nm) == a.cidx (ncols))
            {
              nperm = nrows;
              delete [] perm;
              perm = new octave_idx_type [nrows];
              OCTAVE_LOCAL_BUFFER (octave_idx_type, tmp, nrows);

              for (octave_idx_type i = 0; i < nrows; i++)
                {
                  perm[i] = -1;
                  tmp[i] = -1;
                }

              for (octave_idx_type j = 0; j < ncols; j++)
                for (octave_idx_type i = a.cidx (j); i < a.cidx (j+1); i++)
                  perm[a.ridx (i)] = j;

              found = true;
              for (octave_idx_type i = 0; i < nm; i++)
                if (perm[i] == -1)
                  {
                    found = false;
                    break;
                  }
                else
                  {
                    tmp[perm[i]] = 1;
                  }

              if (found)
                {
                  octave_idx_type k = ncols;
                  for (octave_idx_type i = 0; i < nrows; i++)
                    {
                      if (tmp[i] == -1)
                        {
                          if (k < nrows)
                            {
                              perm[k++] = i;
                            }
                          else
                            {
                              found = false;
                              break;
                            }
                        }
                    }
                }

              if (found)
                typ = MatrixType::Permuted_Lower;
              else
                {
                  delete [] perm;
                  nperm = 0;
                }
            }
          else
            {
              delete [] perm;
              nperm = 0;
            }
        }

      // FIXME: Disable lower under-determined and upper over-determined
      //        problems as being detected, and force to treat as singular
      //        as this seems to cause issues.
      if (((typ == MatrixType::Lower || typ == MatrixType::Permuted_Lower)
           && nrows > ncols)
          || ((typ == MatrixType::Upper || typ == MatrixType::Permuted_Upper)
              && nrows < ncols))
        {
          if (typ == MatrixType::Permuted_Upper
              || typ == MatrixType::Permuted_Lower)
            delete [] perm;
          nperm = 0;
          typ = MatrixType::Rectangular;
        }

      if (typ == MatrixType::Full && ncols != nrows)
        typ = MatrixType::Rectangular;

      if (maybe_hermitian && (typ == MatrixType::Full
                              || typ == MatrixType::Tridiagonal
                              || typ == MatrixType::Banded))
        {
          bool is_herm = true;

          // first, check whether the diagonal is positive & extract it
          ColumnVector diag (ncols);

          for (octave_idx_type j = 0; is_herm && j < ncols; j++)
            {
              is_herm = false;
              for (octave_idx_type i = a.cidx (j); i < a.cidx (j+1); i++)
                {
                  if (a.ridx (i) == j)
                    {
                      double d = a.data (i);
                      is_herm = d > 0.;
                      diag(j) = d;
                      break;
                    }
                }
            }


          // next, check symmetry and 2x2 positiveness

          for (octave_idx_type j = 0; is_herm && j < ncols; j++)
            for (octave_idx_type i = a.cidx (j); is_herm && i < a.cidx (j+1); i++)
              {
                octave_idx_type k = a.ridx (i);
                is_herm = k == j;
                if (is_herm)
                  continue;
                double d = a.data (i);
                if (d*d < diag(j)*diag(k))
                  {
                    for (octave_idx_type l = a.cidx (k); l < a.cidx (k+1); l++)
                      {
                        if (a.ridx (l) == j)
                          {
                            is_herm = a.data (l) == d;
                            break;
                          }
                      }
                  }
              }

          if (is_herm)
            {
              if (typ == MatrixType::Full)
                typ = MatrixType::Hermitian;
              else if (typ == MatrixType::Banded)
                typ = MatrixType::Banded_Hermitian;
              else
                typ = MatrixType::Tridiagonal_Hermitian;
            }
        }
    }
}

MatrixType::MatrixType (const SparseComplexMatrix &a)
  : typ (MatrixType::Unknown),
    sp_bandden (0), bandden (0), upper_band (0), lower_band (0),
    dense (false), full (false), nperm (0), perm (0)
{
  octave_idx_type nrows = a.rows ();
  octave_idx_type ncols = a.cols ();
  octave_idx_type nm = (ncols < nrows ? ncols : nrows);
  octave_idx_type nnz = a.nnz ();

  if (octave_sparse_params::get_key ("spumoni") != 0.)
    warn_calculating_sparse_type ();

  sp_bandden = octave_sparse_params::get_bandden ();
  bool maybe_hermitian = false;
  typ = MatrixType::Full;

  if (nnz == nm)
    {
      matrix_type tmp_typ = MatrixType::Diagonal;
      octave_idx_type i;
      // Maybe the matrix is diagonal
      for (i = 0; i < nm; i++)
        {
          if (a.cidx (i+1) != a.cidx (i) + 1)
            {
              tmp_typ = MatrixType::Full;
              break;
            }
          if (a.ridx (i) != i)
            {
              tmp_typ = MatrixType::Permuted_Diagonal;
              break;
            }
        }

      if (tmp_typ == MatrixType::Permuted_Diagonal)
        {
          std::vector<bool> found (nrows);

          for (octave_idx_type j = 0; j < i; j++)
            found[j] = true;
          for (octave_idx_type j = i; j < nrows; j++)
            found[j] = false;

          for (octave_idx_type j = i; j < nm; j++)
            {
              if ((a.cidx (j+1) > a.cidx (j) + 1)
                  || ((a.cidx (j+1) == a.cidx (j) + 1) && found[a.ridx (j)]))
                {
                  tmp_typ = MatrixType::Full;
                  break;
                }
              found[a.ridx (j)] = true;
            }
        }
      typ = tmp_typ;
    }

  if (typ == MatrixType::Full)
    {
      // Search for banded, upper and lower triangular matrices
      bool singular = false;
      upper_band = 0;
      lower_band = 0;
      for (octave_idx_type j = 0; j < ncols; j++)
        {
          bool zero_on_diagonal = false;
          if (j < nrows)
            {
              zero_on_diagonal = true;
              for (octave_idx_type i = a.cidx (j); i < a.cidx (j+1); i++)
                if (a.ridx (i) == j)
                  {
                    zero_on_diagonal = false;
                    break;
                  }
            }

          if (zero_on_diagonal)
            {
              singular = true;
              break;
            }

          if (a.cidx (j+1) != a.cidx (j))
            {
              octave_idx_type ru = a.ridx (a.cidx (j));
              octave_idx_type rl = a.ridx (a.cidx (j+1)-1);

              if (j - ru > upper_band)
                upper_band = j - ru;

              if (rl - j > lower_band)
                lower_band = rl - j;
            }
        }

      if (!singular)
        {
          bandden = double (nnz) /
                    (double (ncols) * (double (lower_band) +
                                       double (upper_band)) -
                     0.5 * double (upper_band + 1) * double (upper_band) -
                     0.5 * double (lower_band + 1) * double (lower_band));

          if (nrows == ncols && sp_bandden != 1. && bandden > sp_bandden)
            {
              if (upper_band == 1 && lower_band == 1)
                typ = MatrixType::Tridiagonal;
              else
                typ = MatrixType::Banded;

              octave_idx_type nnz_in_band =
                (upper_band + lower_band + 1) * nrows -
                (1 + upper_band) * upper_band / 2 -
                (1 + lower_band) * lower_band / 2;
              if (nnz_in_band == nnz)
                dense = true;
              else
                dense = false;
            }
          else if (upper_band == 0)
            typ = MatrixType::Lower;
          else if (lower_band == 0)
            typ = MatrixType::Upper;

          if (upper_band == lower_band && nrows == ncols)
            maybe_hermitian = true;
        }

      if (typ == MatrixType::Full)
        {
          // Search for a permuted triangular matrix, and test if
          // permutation is singular

          // FIXME: Perhaps this should be based on a dmperm algorithm?
          bool found = false;

          nperm = ncols;
          perm = new octave_idx_type [ncols];

          for (octave_idx_type i = 0; i < ncols; i++)
            perm[i] = -1;

          for (octave_idx_type i = 0; i < nm; i++)
            {
              found = false;

              for (octave_idx_type j = 0; j < ncols; j++)
                {
                  if ((a.cidx (j+1) - a.cidx (j)) > 0
                      && (a.ridx (a.cidx (j+1)-1) == i))
                    {
                      perm[i] = j;
                      found = true;
                      break;
                    }
                }

              if (!found)
                break;
            }

          if (found)
            {
              typ = MatrixType::Permuted_Upper;
              if (ncols > nrows)
                {
                  octave_idx_type k = nrows;
                  for (octave_idx_type i = 0; i < ncols; i++)
                    if (perm[i] == -1)
                      perm[i] = k++;
                }
            }
          else if (a.cidx (nm) == a.cidx (ncols))
            {
              nperm = nrows;
              delete [] perm;
              perm = new octave_idx_type [nrows];
              OCTAVE_LOCAL_BUFFER (octave_idx_type, tmp, nrows);

              for (octave_idx_type i = 0; i < nrows; i++)
                {
                  perm[i] = -1;
                  tmp[i] = -1;
                }

              for (octave_idx_type j = 0; j < ncols; j++)
                for (octave_idx_type i = a.cidx (j); i < a.cidx (j+1); i++)
                  perm[a.ridx (i)] = j;

              found = true;
              for (octave_idx_type i = 0; i < nm; i++)
                if (perm[i] == -1)
                  {
                    found = false;
                    break;
                  }
                else
                  {
                    tmp[perm[i]] = 1;
                  }

              if (found)
                {
                  octave_idx_type k = ncols;
                  for (octave_idx_type i = 0; i < nrows; i++)
                    {
                      if (tmp[i] == -1)
                        {
                          if (k < nrows)
                            {
                              perm[k++] = i;
                            }
                          else
                            {
                              found = false;
                              break;
                            }
                        }
                    }
                }

              if (found)
                typ = MatrixType::Permuted_Lower;
              else
                {
                  delete [] perm;
                  nperm = 0;
                }
            }
          else
            {
              delete [] perm;
              nperm = 0;
            }
        }

      // FIXME: Disable lower under-determined and upper over-determined
      //        problems as being detected, and force to treat as singular
      //        as this seems to cause issues.
      if (((typ == MatrixType::Lower || typ == MatrixType::Permuted_Lower)
           && nrows > ncols)
          || ((typ == MatrixType::Upper || typ == MatrixType::Permuted_Upper)
              && nrows < ncols))
        {
          if (typ == MatrixType::Permuted_Upper
              || typ == MatrixType::Permuted_Lower)
            delete [] perm;
          nperm = 0;
          typ = MatrixType::Rectangular;
        }

      if (typ == MatrixType::Full && ncols != nrows)
        typ = MatrixType::Rectangular;

      if (maybe_hermitian && (typ == MatrixType::Full
                              || typ == MatrixType::Tridiagonal
                              || typ == MatrixType::Banded))
        {
          bool is_herm = true;

          // first, check whether the diagonal is positive & extract it
          ColumnVector diag (ncols);

          for (octave_idx_type j = 0; is_herm && j < ncols; j++)
            {
              is_herm = false;
              for (octave_idx_type i = a.cidx (j); i < a.cidx (j+1); i++)
                {
                  if (a.ridx (i) == j)
                    {
                      Complex d = a.data (i);
                      is_herm = d.real () > 0. && d.imag () == 0.;
                      diag(j) = d.real ();
                      break;
                    }
                }
            }

          // next, check symmetry and 2x2 positiveness

          for (octave_idx_type j = 0; is_herm && j < ncols; j++)
            for (octave_idx_type i = a.cidx (j); is_herm && i < a.cidx (j+1); i++)
              {
                octave_idx_type k = a.ridx (i);
                is_herm = k == j;
                if (is_herm)
                  continue;
                Complex d = a.data (i);
                if (std::norm (d) < diag(j)*diag(k))
                  {
                    d = std::conj (d);
                    for (octave_idx_type l = a.cidx (k); l < a.cidx (k+1); l++)
                      {
                        if (a.ridx (l) == j)
                          {
                            is_herm = a.data (l) == d;
                            break;
                          }
                      }
                  }
              }


          if (is_herm)
            {
              if (typ == MatrixType::Full)
                typ = MatrixType::Hermitian;
              else if (typ == MatrixType::Banded)
                typ = MatrixType::Banded_Hermitian;
              else
                typ = MatrixType::Tridiagonal_Hermitian;
            }
        }
    }
}
MatrixType::MatrixType (const matrix_type t, bool _full)
  : typ (MatrixType::Unknown),
    sp_bandden (octave_sparse_params::get_bandden ()),
    bandden (0), upper_band (0), lower_band (0),
    dense (false), full (_full), nperm (0), perm (0)
{
  if (t == MatrixType::Unknown || t == MatrixType::Full
      || t == MatrixType::Diagonal || t == MatrixType::Permuted_Diagonal
      || t == MatrixType::Upper || t == MatrixType::Lower
      || t == MatrixType::Tridiagonal || t == MatrixType::Tridiagonal_Hermitian
      || t == MatrixType::Rectangular)
    typ = t;
  else
    warn_invalid ();
}

MatrixType::MatrixType (const matrix_type t, const octave_idx_type np,
                        const octave_idx_type *p, bool _full)
  : typ (MatrixType::Unknown),
    sp_bandden (octave_sparse_params::get_bandden ()),
    bandden (0), upper_band (0), lower_band (0),
    dense (false), full (_full), nperm (0), perm (0)
{
  if ((t == MatrixType::Permuted_Upper || t == MatrixType::Permuted_Lower)
      && np > 0 && p != 0)
    {
      typ = t;
      nperm = np;
      perm = new octave_idx_type [nperm];
      for (octave_idx_type i = 0; i < nperm; i++)
        perm[i] = p[i];
    }
  else
    warn_invalid ();
}

MatrixType::MatrixType (const matrix_type t, const octave_idx_type ku,
                        const octave_idx_type kl, bool _full)
  : typ (MatrixType::Unknown),
    sp_bandden (octave_sparse_params::get_bandden ()),
    bandden (0), upper_band (0), lower_band (0),
    dense (false), full (_full), nperm (0), perm (0)
{
  if (t == MatrixType::Banded || t == MatrixType::Banded_Hermitian)
    {
      typ = t;
      upper_band = ku;
      lower_band = kl;
    }
  else
    warn_invalid ();
}

MatrixType::~MatrixType (void)
{
  if (nperm != 0)
    {
      delete [] perm;
    }
}

MatrixType&
MatrixType::operator = (const MatrixType& a)
{
  if (this != &a)
    {
      typ = a.typ;
      sp_bandden = a.sp_bandden;
      bandden = a.bandden;
      upper_band = a.upper_band;
      lower_band = a.lower_band;
      dense = a.dense;
      full = a.full;

      if (nperm)
        {
          delete[] perm;
        }

      if (a.nperm != 0)
        {
          perm = new octave_idx_type [a.nperm];
          for (octave_idx_type i = 0; i < a.nperm; i++)
            perm[i] = a.perm[i];
        }

      nperm = a.nperm;
    }

  return *this;
}

int
MatrixType::type (bool quiet)
{
  if (typ != MatrixType::Unknown
      && (full || sp_bandden == octave_sparse_params::get_bandden ()))
    {
      if (!quiet && octave_sparse_params::get_key ("spumoni") != 0.)
        warn_cached ();

      return typ;
    }

  if (typ != MatrixType::Unknown
      && octave_sparse_params::get_key ("spumoni") != 0.)
    (*current_liboctave_warning_with_id_handler)
      ("Octave:matrix-type-info", "invalidating matrix type");

  typ = MatrixType::Unknown;

  return typ;
}

int
MatrixType::type (const SparseMatrix &a)
{
  if (typ != MatrixType::Unknown
      && (full || sp_bandden == octave_sparse_params::get_bandden ()))
    {
      if (octave_sparse_params::get_key ("spumoni") != 0.)
        warn_cached ();

      return typ;
    }

  MatrixType tmp_typ (a);
  typ = tmp_typ.typ;
  sp_bandden = tmp_typ.sp_bandden;
  bandden = tmp_typ.bandden;
  upper_band = tmp_typ.upper_band;
  lower_band = tmp_typ.lower_band;
  dense = tmp_typ.dense;
  full = tmp_typ.full;
  nperm = tmp_typ.nperm;

  if (nperm != 0)
    {
      perm = new octave_idx_type [nperm];
      for (octave_idx_type i = 0; i < nperm; i++)
        perm[i] = tmp_typ.perm[i];
    }

  return typ;
}

int
MatrixType::type (const SparseComplexMatrix &a)
{
  if (typ != MatrixType::Unknown
      && (full || sp_bandden == octave_sparse_params::get_bandden ()))
    {
      if (octave_sparse_params::get_key ("spumoni") != 0.)
        warn_cached ();

      return typ;
    }

  MatrixType tmp_typ (a);
  typ = tmp_typ.typ;
  sp_bandden = tmp_typ.sp_bandden;
  bandden = tmp_typ.bandden;
  upper_band = tmp_typ.upper_band;
  lower_band = tmp_typ.lower_band;
  dense = tmp_typ.dense;
  full = tmp_typ.full;
  nperm = tmp_typ.nperm;

  if (nperm != 0)
    {
      perm = new octave_idx_type [nperm];
      for (octave_idx_type i = 0; i < nperm; i++)
        perm[i] = tmp_typ.perm[i];
    }

  return typ;
}

int
MatrixType::type (const Matrix &a)
{
  if (typ != MatrixType::Unknown)
    {
      if (octave_sparse_params::get_key ("spumoni") != 0.)
        warn_cached ();

      return typ;
    }

  MatrixType tmp_typ (a);
  typ = tmp_typ.typ;
  full = tmp_typ.full;
  nperm = tmp_typ.nperm;

  if (nperm != 0)
    {
      perm = new octave_idx_type [nperm];
      for (octave_idx_type i = 0; i < nperm; i++)
        perm[i] = tmp_typ.perm[i];
    }

  return typ;
}

int
MatrixType::type (const ComplexMatrix &a)
{
  if (typ != MatrixType::Unknown)
    {
      if (octave_sparse_params::get_key ("spumoni") != 0.)
        warn_cached ();

      return typ;
    }

  MatrixType tmp_typ (a);
  typ = tmp_typ.typ;
  full = tmp_typ.full;
  nperm = tmp_typ.nperm;

  if (nperm != 0)
    {
      perm = new octave_idx_type [nperm];
      for (octave_idx_type i = 0; i < nperm; i++)
        perm[i] = tmp_typ.perm[i];
    }

  return typ;
}

int
MatrixType::type (const FloatMatrix &a)
{
  if (typ != MatrixType::Unknown)
    {
      if (octave_sparse_params::get_key ("spumoni") != 0.)
        warn_cached ();

      return typ;
    }

  MatrixType tmp_typ (a);
  typ = tmp_typ.typ;
  full = tmp_typ.full;
  nperm = tmp_typ.nperm;

  if (nperm != 0)
    {
      perm = new octave_idx_type [nperm];
      for (octave_idx_type i = 0; i < nperm; i++)
        perm[i] = tmp_typ.perm[i];
    }

  return typ;
}

int
MatrixType::type (const FloatComplexMatrix &a)
{
  if (typ != MatrixType::Unknown)
    {
      if (octave_sparse_params::get_key ("spumoni") != 0.)
        warn_cached ();

      return typ;
    }

  MatrixType tmp_typ (a);
  typ = tmp_typ.typ;
  full = tmp_typ.full;
  nperm = tmp_typ.nperm;

  if (nperm != 0)
    {
      perm = new octave_idx_type [nperm];
      for (octave_idx_type i = 0; i < nperm; i++)
        perm[i] = tmp_typ.perm[i];
    }

  return typ;
}

void
MatrixType::info () const
{
  if (octave_sparse_params::get_key ("spumoni") != 0.)
    {
      if (typ == MatrixType::Unknown)
        (*current_liboctave_warning_with_id_handler)
          ("Octave:matrix-type-info", "unknown matrix type");
      else if (typ == MatrixType::Diagonal)
        (*current_liboctave_warning_with_id_handler)
          ("Octave:matrix-type-info", "diagonal sparse matrix");
      else if (typ == MatrixType::Permuted_Diagonal)
        (*current_liboctave_warning_with_id_handler)
          ("Octave:matrix-type-info", "permuted diagonal sparse matrix");
      else if (typ == MatrixType::Upper)
        (*current_liboctave_warning_with_id_handler)
          ("Octave:matrix-type-info", "upper triangular matrix");
      else if (typ == MatrixType::Lower)
        (*current_liboctave_warning_with_id_handler)
          ("Octave:matrix-type-info", "lower triangular matrix");
      else if (typ == MatrixType::Permuted_Upper)
        (*current_liboctave_warning_with_id_handler)
          ("Octave:matrix-type-info", "permuted upper triangular matrix");
      else if (typ == MatrixType::Permuted_Lower)
        (*current_liboctave_warning_with_id_handler)
          ("Octave:matrix-type-info", "permuted lower triangular Matrix");
      else if (typ == MatrixType::Banded)
        (*current_liboctave_warning_with_id_handler)
          ("Octave:matrix-type-info",
           "banded sparse matrix %d-1-%d (density %f)",
           lower_band, upper_band, bandden);
      else if (typ == MatrixType::Banded_Hermitian)
        (*current_liboctave_warning_with_id_handler)
          ("Octave:matrix-type-info",
           "banded hermitian/symmetric sparse matrix %d-1-%d (density %f)",
           lower_band, upper_band, bandden);
      else if (typ == MatrixType::Hermitian)
        (*current_liboctave_warning_with_id_handler)
          ("Octave:matrix-type-info", "hermitian/symmetric matrix");
      else if (typ == MatrixType::Tridiagonal)
        (*current_liboctave_warning_with_id_handler)
          ("Octave:matrix-type-info", "tridiagonal sparse matrix");
      else if (typ == MatrixType::Tridiagonal_Hermitian)
        (*current_liboctave_warning_with_id_handler)
          ("Octave:matrix-type-info",
           "hermitian/symmetric tridiagonal sparse matrix");
      else if (typ == MatrixType::Rectangular)
        (*current_liboctave_warning_with_id_handler)
          ("Octave:matrix-type-info", "rectangular/singular matrix");
      else if (typ == MatrixType::Full)
        (*current_liboctave_warning_with_id_handler)
          ("Octave:matrix-type-info", "full matrix");
    }
}

void
MatrixType::mark_as_symmetric (void)
{
  if (typ == MatrixType::Tridiagonal
      || typ == MatrixType::Tridiagonal_Hermitian)
    typ = MatrixType::Tridiagonal_Hermitian;
  else if (typ == MatrixType::Banded || typ == MatrixType::Banded_Hermitian)
    typ = MatrixType::Banded_Hermitian;
  else if (typ == MatrixType::Full || typ == MatrixType::Hermitian
           || typ == MatrixType::Unknown)
    typ = MatrixType::Hermitian;
  else
    (*current_liboctave_error_handler)
      ("Can not mark current matrix type as symmetric");
}

void
MatrixType::mark_as_unsymmetric (void)
{
  if (typ == MatrixType::Tridiagonal
      || typ == MatrixType::Tridiagonal_Hermitian)
    typ = MatrixType::Tridiagonal;
  else if (typ == MatrixType::Banded || typ == MatrixType::Banded_Hermitian)
    typ = MatrixType::Banded;
  else if (typ == MatrixType::Full || typ == MatrixType::Hermitian
           || typ == MatrixType::Unknown)
    typ = MatrixType::Full;
}

void
MatrixType::mark_as_permuted (const octave_idx_type np,
                              const octave_idx_type *p)
{
  nperm = np;
  perm = new octave_idx_type [nperm];
  for (octave_idx_type i = 0; i < nperm; i++)
    perm[i] = p[i];

  if (typ == MatrixType::Diagonal || typ == MatrixType::Permuted_Diagonal)
    typ = MatrixType::Permuted_Diagonal;
  else if (typ == MatrixType::Upper || typ == MatrixType::Permuted_Upper)
    typ = MatrixType::Permuted_Upper;
  else if (typ == MatrixType::Lower || typ == MatrixType::Permuted_Lower)
    typ = MatrixType::Permuted_Lower;
  else
    (*current_liboctave_error_handler)
      ("Can not mark current matrix type as symmetric");
}

void
MatrixType::mark_as_unpermuted (void)
{
  if (nperm)
    {
      nperm = 0;
      delete [] perm;
    }

  if (typ == MatrixType::Diagonal || typ == MatrixType::Permuted_Diagonal)
    typ = MatrixType::Diagonal;
  else if (typ == MatrixType::Upper || typ == MatrixType::Permuted_Upper)
    typ = MatrixType::Upper;
  else if (typ == MatrixType::Lower || typ == MatrixType::Permuted_Lower)
    typ = MatrixType::Lower;
}

MatrixType
MatrixType::transpose (void) const
{
  MatrixType retval (*this);
  if (typ == MatrixType::Upper)
    retval.typ = MatrixType::Lower;
  else if (typ == MatrixType::Permuted_Upper)
    retval.typ = MatrixType::Permuted_Lower;
  else if (typ == MatrixType::Lower)
    retval.typ = MatrixType::Upper;
  else if (typ == MatrixType::Permuted_Lower)
    retval.typ = MatrixType::Permuted_Upper;
  else if (typ == MatrixType::Banded)
    {
      retval.upper_band = lower_band;
      retval.lower_band = upper_band;
    }

  return retval;
}
