/*

Copyright (C) 2014-2015 Eduardo Ramos Fernández <eduradical951@gmail.com>
Copyright (C) 2013-2015 Kai T. Ohlhus <k.ohlhus@gmail.com>

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

#include "oct-locbuf.h"

#include "defun.h"
#include "error.h"
#include "parse.h"

// That function implements the IKJ and JKI variants of Gaussian elimination to
// perform the ILUTP decomposition.  The behaviour is controlled by milu
// parameter.  If milu = ['off'|'col'] the JKI version is performed taking
// advantage of CCS format of the input matrix.  If milu = 'row' the input
// matrix has to be transposed to obtain the equivalent CRS structure so we can
// work efficiently with rows.  In this case IKJ version is used.
template <typename octave_matrix_t, typename T>
void ilu_0 (octave_matrix_t& sm, const std::string milu = "off")
{

  const octave_idx_type n = sm.cols ();
  OCTAVE_LOCAL_BUFFER (octave_idx_type, iw, n);
  OCTAVE_LOCAL_BUFFER (octave_idx_type, uptr, n);
  octave_idx_type j1, j2, jrow, jw, i, k, jj;
  T tl, r;

  enum {OFF, ROW, COL};
  char opt;
  if (milu == "row")
    {
      opt = ROW;
      sm = sm.transpose ();
    }
  else if (milu == "col")
    opt = COL;
  else
    opt = OFF;

  octave_idx_type* cidx = sm.cidx ();
  octave_idx_type* ridx = sm.ridx ();
  T* data = sm.data ();
  for (i = 0; i < n; i++)
    iw[i] = -1;
  for (k = 0; k < n; k++)
    {
      j1 = cidx[k];
      j2 = cidx[k+1] - 1;
      octave_idx_type j;
      for (j = j1; j <= j2; j++)
        {
          iw[ridx[j]] = j;
        }
      r = 0;
      j = j1;
      jrow = ridx[j];
      while ((jrow < k) && (j <= j2))
        {
          if (opt == ROW)
            {
              tl = data[j] / data[uptr[jrow]];
              data[j] = tl;
            }
          for (jj = uptr[jrow] + 1; jj < cidx[jrow+1]; jj++)
            {
              jw = iw[ridx[jj]];
              if (jw != -1)
                if (opt == ROW)
                  data[jw] -= tl * data[jj];
                else
                  data[jw] -= data[j] * data[jj];

              else
                // That is for the milu='row'
                if (opt == ROW)
                  r += tl * data[jj];
                else if (opt == COL)
                  r += data[j] * data[jj];
            }
          j++;
          jrow = ridx[j];
        }
      uptr[k] = j;
      if (opt != OFF)
        data[uptr[k]] -= r;
      if (opt != ROW)
        for (jj = uptr[k] + 1; jj < cidx[k+1]; jj++)
          data[jj] /=  data[uptr[k]];
      if (k != jrow)
        {
          error ("ilu: A has a zero on the diagonal");
          break;
        }

      if (data[j] == T(0))
        {
          error ("ilu: encountered a pivot equal to 0");
          break;
        }
      for (i = j1; i <= j2; i++)
        iw[ridx[i]] = -1;
    }
  if (opt == ROW)
    sm = sm.transpose ();
}

DEFUN (__ilu0__, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {[@var{L}, @var{U}] =} __ilu0__ (@var{A})\n\
@deftypefnx {Built-in Function} {[@var{L}, @var{U}] =} __ilu0__ (@var{A}, @var{milu})\n\
@deftypefnx {Built-in Function} {[@var{L}, @var{U}, @var{P}] =} __ilu0__ (@var{A}, @dots{})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();
  std::string milu;

  if (nargout > 2 || nargin < 1 || nargin > 2)
    {
      print_usage ();
      return retval;
    }

  // In ILU0 algorithm the zero-pattern of the input matrix is preserved so
  // it's structure does not change during the algorithm.  The same input
  // matrix is used to build the output matrix due to that fact.
  octave_value_list param_list;
  if (! args(0).is_complex_type ())
    {
      SparseMatrix sm = args(0).sparse_matrix_value ();
      ilu_0 <SparseMatrix, double> (sm, milu);
      if (!error_state)
        {
          param_list.append (sm);
          retval(1) = feval ("triu", param_list)(0).sparse_matrix_value ();
          SparseMatrix eye =
            feval ("speye", octave_value_list (
                     octave_value (sm.cols ())))(0).sparse_matrix_value ();
          param_list.append (-1);
          retval(0) = eye +
                      feval ("tril", param_list)(0).sparse_matrix_value ();
        }
    }
  else
    {
      SparseComplexMatrix sm = args(0).sparse_complex_matrix_value ();
      ilu_0 <SparseComplexMatrix, Complex> (sm, milu);
      if (! error_state)
        {
          param_list.append (sm);
          retval(1) =
            feval ("triu", param_list)(0).sparse_complex_matrix_value ();
          SparseComplexMatrix eye =
            feval ("speye", octave_value_list (
                     octave_value (sm.cols ())))(0).sparse_complex_matrix_value ();
          param_list.append (-1);
          retval(0) =
            eye + feval ("tril", param_list)(0).sparse_complex_matrix_value ();
        }
    }

  return retval;
}

template <typename octave_matrix_t, typename T>
void ilu_crout (octave_matrix_t& sm_l, octave_matrix_t& sm_u,
                octave_matrix_t& L, octave_matrix_t& U, T* cols_norm,
                T* rows_norm, const T droptol = 0,
                const std::string milu = "off")
{

  // Map the strings into chars for faster comparing inside loops
  char opt;
  enum {OFF, ROW, COL};
  if (milu == "row")
    opt = ROW;
  else if (milu == "col")
    opt = COL;
  else
    opt = OFF;

  octave_idx_type jrow, i, j, k, jj, total_len_l, total_len_u, max_len_u,
                  max_len_l, w_len_u, w_len_l, cols_list_len, rows_list_len;

  const octave_idx_type n = sm_u.cols ();
  sm_u = sm_u.transpose ();

  max_len_u = sm_u.nnz ();
  max_len_u += (0.1 * max_len_u) > n ? 0.1 * max_len_u : n;
  max_len_l = sm_l.nnz ();
  max_len_l += (0.1 * max_len_l) > n ? 0.1 * max_len_l : n;
  // Extract pointers to the arrays for faster access inside loops
  octave_idx_type* cidx_in_u = sm_u.cidx ();
  octave_idx_type* ridx_in_u = sm_u.ridx ();
  T* data_in_u = sm_u.data ();
  octave_idx_type* cidx_in_l = sm_l.cidx ();
  octave_idx_type* ridx_in_l = sm_l.ridx ();
  T* data_in_l = sm_l.data ();

  // L output arrays
  Array <octave_idx_type> ridx_out_l (dim_vector (max_len_l, 1));
  octave_idx_type* ridx_l = ridx_out_l.fortran_vec ();
  Array <T> data_out_l (dim_vector (max_len_l, 1));
  T* data_l = data_out_l.fortran_vec ();

  // U output arrays
  Array <octave_idx_type> ridx_out_u (dim_vector (max_len_u, 1));
  octave_idx_type* ridx_u = ridx_out_u.fortran_vec ();
  Array <T> data_out_u (dim_vector (max_len_u, 1));
  T* data_u = data_out_u.fortran_vec ();

  // Working arrays
  OCTAVE_LOCAL_BUFFER (octave_idx_type, cidx_l, n + 1);
  OCTAVE_LOCAL_BUFFER (octave_idx_type, cidx_u, n + 1);
  OCTAVE_LOCAL_BUFFER (octave_idx_type, cols_list, n);
  OCTAVE_LOCAL_BUFFER (octave_idx_type, rows_list, n);
  OCTAVE_LOCAL_BUFFER (T, w_data_l, n);
  OCTAVE_LOCAL_BUFFER (T, w_data_u, n);
  OCTAVE_LOCAL_BUFFER (octave_idx_type, Ufirst, n);
  OCTAVE_LOCAL_BUFFER (octave_idx_type, Lfirst, n);
  OCTAVE_LOCAL_BUFFER (T, cr_sum, n);

  T zero = T (0);

  cidx_u[0] = cidx_in_u[0];
  cidx_l[0] = cidx_in_l[0];
  for (i = 0; i < n; i++)
    {
      w_data_u[i] = zero;
      w_data_l[i] = zero;
      cr_sum[i] = zero;
    }

  total_len_u = 0;
  total_len_l = 0;
  cols_list_len = 0;
  rows_list_len = 0;

  for (k = 0; k < n; k++)
    {
      // Load the working column and working row
      for (i = cidx_in_l[k]; i < cidx_in_l[k+1]; i++)
        w_data_l[ridx_in_l[i]] = data_in_l[i];

      for (i = cidx_in_u[k]; i < cidx_in_u[k+1]; i++)
        w_data_u[ridx_in_u[i]] = data_in_u[i];

      // Update U working row
      for (j = 0; j < rows_list_len; j++)
        {
          if ((Ufirst[rows_list[j]] != -1))
            for (jj = Ufirst[rows_list[j]]; jj < cidx_u[rows_list[j]+1]; jj++)
              {
                jrow = ridx_u[jj];
                w_data_u[jrow] -= data_u[jj] * data_l[Lfirst[rows_list[j]]];
              }
        }
      // Update L working column
      for (j = 0; j < cols_list_len; j++)
        {
          if (Lfirst[cols_list[j]] != -1)
            for (jj = Lfirst[cols_list[j]]; jj < cidx_l[cols_list[j]+1]; jj++)
              {
                jrow = ridx_l[jj];
                w_data_l[jrow] -= data_l[jj] * data_u[Ufirst[cols_list[j]]];
              }
        }

      if ((max_len_u - total_len_u) < n)
        {
          max_len_u += (0.1 * max_len_u) > n ? 0.1 * max_len_u : n;
          data_out_u.resize (dim_vector (max_len_u, 1));
          data_u = data_out_u.fortran_vec ();
          ridx_out_u.resize (dim_vector (max_len_u, 1));
          ridx_u = ridx_out_u.fortran_vec ();
        }

      if ((max_len_l - total_len_l) < n)
        {
          max_len_l += (0.1 * max_len_l) > n ? 0.1 * max_len_l : n;
          data_out_l.resize (dim_vector (max_len_l, 1));
          data_l = data_out_l.fortran_vec ();
          ridx_out_l.resize (dim_vector (max_len_l, 1));
          ridx_l = ridx_out_l.fortran_vec ();
        }

      // Expand the working row into the U output data structures
      w_len_l = 0;
      data_u[total_len_u] = w_data_u[k];
      ridx_u[total_len_u] = k;
      w_len_u = 1;
      for (i = k + 1; i < n; i++)
        {
          if (w_data_u[i] != zero)
            {
              if (std::abs (w_data_u[i]) < (droptol * rows_norm[k]))
                {
                  if (opt == ROW)
                    cr_sum[k] += w_data_u[i];
                  else if (opt == COL)
                    cr_sum[i] += w_data_u[i];
                }
              else
                {
                  data_u[total_len_u + w_len_u] = w_data_u[i];
                  ridx_u[total_len_u + w_len_u] = i;
                  w_len_u++;
                }
            }

          // Expand the working column into the L output data structures
          if (w_data_l[i] != zero)
            {
              if (std::abs (w_data_l[i]) < (droptol * cols_norm[k]))
                {
                  if (opt == COL)
                    cr_sum[k] += w_data_l[i];
                  else if (opt == ROW)
                    cr_sum[i] += w_data_l[i];
                }
              else
                {
                  data_l[total_len_l + w_len_l] = w_data_l[i];
                  ridx_l[total_len_l + w_len_l] = i;
                  w_len_l++;
                }
            }
          w_data_u[i] = zero;
          w_data_l[i] = zero;
        }

      // Compensate row and column sums --> milu option
      if (opt == COL || opt == ROW)
        data_u[total_len_u] += cr_sum[k];

      // Check if the pivot is zero
      if (data_u[total_len_u] == zero)
        {
          error ("ilu: encountered a pivot equal to 0");
          break;
        }

      // Scale the elements in L by the pivot
      for (i = total_len_l ; i < (total_len_l + w_len_l); i++)
        data_l[i] /= data_u[total_len_u];


      total_len_u += w_len_u;
      total_len_l += w_len_l;
      // Check if there are too many elements to be indexed with
      // octave_idx_type type due to fill-in during the process.
      if (total_len_l < 0 || total_len_u < 0)
        {
          error ("ilu: integer overflow.  Too many fill-in elements in L or U");
          break;
        }
      cidx_u[k+1] = cidx_u[k] - cidx_u[0] + w_len_u;
      cidx_l[k+1] = cidx_l[k] - cidx_l[0] + w_len_l;

      // The tricky part of the algorithm.  The arrays pointing to the first
      // working element of each column in the next iteration (Lfirst) or
      // the first working element of each row (Ufirst) are updated.  Also the
      // arrays working as lists cols_list and rows_list are filled with
      // indices pointing to Ufirst and Lfirst respectively.
      // TODO: Maybe the -1 indicating in Ufirst and Lfirst, that no elements
      // have to be considered in a certain column or row in next iteration,
      // can be removed.  It feels safer to me using such an indicator.
      if (k < (n - 1))
        {
          if (w_len_u > 0)
            Ufirst[k] = cidx_u[k];
          else
            Ufirst[k] = -1;
          if (w_len_l > 0)
            Lfirst[k] = cidx_l[k];
          else
            Lfirst[k] = -1;
          cols_list_len = 0;
          rows_list_len = 0;
          for (i = 0; i <= k; i++)
            {
              if (Ufirst[i] != -1)
                {
                  jj = ridx_u[Ufirst[i]];
                  if (jj < (k + 1))
                    {
                      if (Ufirst[i] < (cidx_u[i+1]))
                        {
                          Ufirst[i]++;
                          if (Ufirst[i] == cidx_u[i+1])
                            Ufirst[i] = -1;
                          else
                            jj = ridx_u[Ufirst[i]];
                        }
                    }
                  if (jj == (k + 1))
                    {
                      cols_list[cols_list_len] = i;
                      cols_list_len++;
                    }
                }

              if (Lfirst[i] != -1)
                {
                  jj = ridx_l[Lfirst[i]];
                  if (jj < (k + 1))
                    if (Lfirst[i] < (cidx_l[i+1]))
                      {
                        Lfirst[i]++;
                        if (Lfirst[i] == cidx_l[i+1])
                          Lfirst[i] = -1;
                        else
                          jj = ridx_l[Lfirst[i]];
                      }
                  if (jj == (k + 1))
                    {
                      rows_list[rows_list_len] = i;
                      rows_list_len++;
                    }
                }
            }
        }
    }

  if (! error_state)
    {
      // Build the output matrices
      L = octave_matrix_t (n, n, total_len_l);
      U = octave_matrix_t (n, n, total_len_u);
      for (i = 0; i <= n; i++)
        L.cidx (i) = cidx_l[i];
      for (i = 0; i < total_len_l; i++)
        {
          L.ridx (i) = ridx_l[i];
          L.data (i) = data_l[i];
        }
      for (i = 0; i <= n; i++)
        U.cidx (i) = cidx_u[i];
      for (i = 0; i < total_len_u; i++)
        {
          U.ridx (i) = ridx_u[i];
          U.data (i) = data_u[i];
        }
      U = U.transpose ();
    }
}

DEFUN (__iluc__, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {[@var{L}, @var{U}] =} __iluc__ (@var{A})\n\
@deftypefnx {Built-in Function} {[@var{L}, @var{U}] =} __iluc__ (@var{A}, @var{droptol})\n\
@deftypefnx {Built-in Function} {[@var{L}, @var{U}] =} __iluc__ (@var{A}, @var{droptol}, @var{milu})\n\
@deftypefnx {Built-in Function} {[@var{L}, @var{U}, @var{P}] =} __iluc__ (@var{A}, @dots{})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value_list retval;
  int nargin = args.length ();
  std::string milu = "off";
  double droptol = 0;

  if (nargout != 2 || nargin < 1 || nargin > 3)
    {
      print_usage ();
      return retval;
    }

  // Don't repeat input validation of arguments done in ilu.m
  if (nargin >= 2)
    droptol = args(1).double_value ();

  if (nargin == 3)
    milu = args(2).string_value ();

  octave_value_list param_list;
  if (! args(0).is_complex_type ())
    {
      Array<double> cols_norm, rows_norm;
      param_list.append (args(0).sparse_matrix_value ());
      SparseMatrix sm_u = feval ("triu", param_list)(0).sparse_matrix_value ();
      param_list.append (-1);
      SparseMatrix sm_l = feval ("tril", param_list)(0).sparse_matrix_value ();
      param_list(1) = "rows";
      rows_norm = feval ("norm", param_list)(0).vector_value ();
      param_list(1) = "cols";
      cols_norm = feval ("norm", param_list)(0).vector_value ();
      param_list.clear ();
      SparseMatrix U;
      SparseMatrix L;
      ilu_crout <SparseMatrix, double> (sm_l, sm_u, L, U,
                                        cols_norm.fortran_vec (),
                                        rows_norm.fortran_vec (),
                                        droptol, milu);
      if (! error_state)
        {
          param_list.append (octave_value (L.cols ()));
          SparseMatrix eye =
            feval ("speye", param_list)(0).sparse_matrix_value ();
          retval(1) = U;
          retval(0) = L + eye;
        }
    }
  else
    {
      Array<Complex> cols_norm, rows_norm;
      param_list.append (args(0).sparse_complex_matrix_value ());
      SparseComplexMatrix sm_u =
        feval("triu", param_list)(0).sparse_complex_matrix_value ();
      param_list.append (-1);
      SparseComplexMatrix sm_l =
        feval("tril", param_list)(0).sparse_complex_matrix_value ();
      param_list(1) = "rows";
      rows_norm = feval ("norm", param_list)(0).complex_vector_value ();
      param_list(1) = "cols";
      cols_norm = feval ("norm", param_list)(0).complex_vector_value ();
      param_list.clear ();
      SparseComplexMatrix U;
      SparseComplexMatrix L;
      ilu_crout < SparseComplexMatrix, Complex >
                (sm_l, sm_u, L, U, cols_norm.fortran_vec () ,
                 rows_norm.fortran_vec (), Complex (droptol), milu);
      if (! error_state)
        {
          param_list.append (octave_value (L.cols ()));
          SparseComplexMatrix eye =
            feval ("speye", param_list)(0).sparse_complex_matrix_value ();
          retval(1) = U;
          retval(0) = L + eye;
        }
    }

  return retval;
}

// That function implements the IKJ and JKI variants of gaussian elimination
// to perform the ILUTP decomposition.  The behaviour is controlled by milu
// parameter.  If milu = ['off'|'col'] the JKI version is performed taking
// advantage of CCS format of the input matrix.  Row pivoting is performed.
// If milu = 'row' the input matrix has to be transposed to obtain the
// equivalent CRS structure so we can work efficiently with rows.  In that
// case IKJ version is used and column pivoting is performed.

template <typename octave_matrix_t, typename T>
void ilu_tp (octave_matrix_t& sm, octave_matrix_t& L, octave_matrix_t& U,
             octave_idx_type nnz_u, octave_idx_type nnz_l, T* cols_norm,
             Array <octave_idx_type>& perm_vec, const T droptol = T(0),
             const T thresh = T(0), const  std::string milu = "off",
             const double udiag = 0)
{
  char opt;
  enum {OFF, ROW, COL};
  if (milu == "row")
    opt = ROW;
  else if (milu == "col")
    opt = COL;
  else
    opt = OFF;

  const octave_idx_type n = sm.cols ();

  // That is necessary for the JKI (milu = "row") variant.
  if (opt == ROW)
    sm = sm.transpose();

  // Extract pointers to the arrays for faster access inside loops
  octave_idx_type* cidx_in = sm.cidx ();
  octave_idx_type* ridx_in = sm.ridx ();
  T* data_in = sm.data ();
  octave_idx_type jrow, i, j, k, jj, c, total_len_l, total_len_u, p_perm,
                  max_ind, max_len_l, max_len_u;
  T zero = T(0);

  T tl = zero, aux, maximum;

  max_len_u = nnz_u;
  max_len_u += (0.1 * max_len_u) > n ? 0.1 * max_len_u : n;
  max_len_l = nnz_l;
  max_len_l += (0.1 * max_len_l) > n ? 0.1 * max_len_l : n;

  Array <octave_idx_type> cidx_out_l (dim_vector (n + 1, 1));
  octave_idx_type* cidx_l = cidx_out_l.fortran_vec ();
  Array <octave_idx_type> ridx_out_l (dim_vector (max_len_l, 1));
  octave_idx_type* ridx_l = ridx_out_l.fortran_vec ();
  Array <T> data_out_l (dim_vector (max_len_l ,1));
  T* data_l = data_out_l.fortran_vec ();
  // Data for U
  Array <octave_idx_type> cidx_out_u (dim_vector (n + 1, 1));
  octave_idx_type* cidx_u = cidx_out_u.fortran_vec ();
  Array <octave_idx_type> ridx_out_u (dim_vector (max_len_u, 1));
  octave_idx_type* ridx_u = ridx_out_u.fortran_vec ();
  Array <T> data_out_u (dim_vector (max_len_u, 1));
  T* data_u = data_out_u.fortran_vec();

  // Working arrays and permutation arrays
  octave_idx_type w_len_u, w_len_l;
  T total_sum, partial_col_sum = zero, partial_row_sum = zero;
  std::set <octave_idx_type> iw_l;
  std::set <octave_idx_type> iw_u;
  std::set <octave_idx_type>::iterator it, it2;
  OCTAVE_LOCAL_BUFFER (T, w_data, n);
  OCTAVE_LOCAL_BUFFER (octave_idx_type, iperm, n);
  octave_idx_type* perm = perm_vec.fortran_vec ();
  OCTAVE_LOCAL_BUFFER (octave_idx_type, uptr, n);


  cidx_l[0] = cidx_in[0];
  cidx_u[0] = cidx_in[0];
  for (i = 0; i < n; i++)
    {
      w_data[i] = 0;
      perm[i] = i;
      iperm[i] = i;
    }
  total_len_u = 0;
  total_len_l = 0;

  for (k = 0; k < n; k++)
    {

      for (j = cidx_in[k]; j < cidx_in[k+1]; j++)
        {
          p_perm = iperm[ridx_in[j]];
          w_data[iperm[ridx_in[j]]] = data_in[j];
          if (p_perm > k)
            iw_l.insert (ridx_in[j]);
          else
            iw_u.insert (p_perm);
        }

      it = iw_u.begin ();
      jrow = *it;
      total_sum = zero;
      while ((jrow < k) && (it != iw_u.end ()))
        {
          if (opt == COL)
            partial_col_sum = w_data[jrow];
          if (w_data[jrow] != zero)
            {
              if (opt == ROW)
                {
                  partial_row_sum = w_data[jrow];
                  tl = w_data[jrow] / data_u[uptr[jrow]];
                }
              for (jj = cidx_l[jrow]; jj < cidx_l[jrow+1]; jj++)
                {
                  p_perm = iperm[ridx_l[jj]];
                  aux = w_data[p_perm];
                  if (opt == ROW)
                    {
                      w_data[p_perm] -= tl * data_l[jj];
                      partial_row_sum += tl * data_l[jj];
                    }
                  else
                    {
                      tl = data_l[jj] * w_data[jrow];
                      w_data[p_perm] -= tl;
                      if (opt == COL)
                        partial_col_sum += tl;
                    }

                  if ((aux == zero) && (w_data[p_perm] != zero))
                    {
                      if (p_perm > k)
                        iw_l.insert (ridx_l[jj]);
                      else
                        iw_u.insert (p_perm);
                    }
                }

              // Drop element from the U part in IKJ and L part in JKI
              // variant (milu = [col|off])
              if ((std::abs (w_data[jrow]) < (droptol * cols_norm[k]))
                  && (w_data[jrow] != zero))
                {
                  if (opt == COL)
                    total_sum += partial_col_sum;
                  else if (opt == ROW)
                    total_sum += partial_row_sum;
                  w_data[jrow] = zero;
                  it2 = it;
                  it++;
                  iw_u.erase (it2);
                  jrow = *it;
                  continue;
                }
              else
                // This is the element scaled by the pivot
                // in the actual iteration
                if (opt == ROW)
                  w_data[jrow] = tl;
            }
          jrow = *(++it);
        }

      // Search for the pivot and update iw_l and iw_u if the pivot is not the
      // diagonal element
      if ((thresh > zero) && (k < (n - 1)))
        {
          maximum = std::abs (w_data[k]) / thresh;
          max_ind = perm[k];
          for (it = iw_l.begin (); it != iw_l.end (); ++it)
            {
              p_perm = iperm[*it];
              if (std::abs (w_data[p_perm]) > maximum)
                {
                  maximum = std::abs (w_data[p_perm]);
                  max_ind = *it;
                  it2 = it;
                }
            }
          // If the pivot is not the diagonal element update all.
          p_perm = iperm[max_ind];
          if (max_ind != perm[k])
            {
              iw_l.erase (it2);
              if (w_data[k] != zero)
                iw_l.insert (perm[k]);
              else
                iw_u.insert (k);
              // Swap data and update permutation vectors
              aux = w_data[k];
              iperm[perm[p_perm]] = k;
              iperm[perm[k]] = p_perm;
              c = perm[k];
              perm[k] = perm[p_perm];
              perm[p_perm] = c;
              w_data[k] = w_data[p_perm];
              w_data[p_perm] = aux;
            }

        }

      // Drop elements in the L part in the IKJ and from the U part in the JKI
      // version.
      it = iw_l.begin ();
      while (it != iw_l.end ())
        {
          p_perm = iperm[*it];
          if (droptol > zero)
            if (std::abs (w_data[p_perm]) < (droptol * cols_norm[k]))
              {
                if (opt != OFF)
                  total_sum += w_data[p_perm];
                w_data[p_perm] = zero;
                it2 = it;
                it++;
                iw_l.erase (it2);
                continue;
              }

          it++;
        }

      // If milu == [row|col] summation is preserved.
      // Compensate diagonal element.
      if (opt != OFF)
        {
          if ((total_sum > zero) && (w_data[k] == zero))
            iw_u.insert (k);
          w_data[k] += total_sum;
        }



      // Check if the pivot is zero and if udiag is active.
      // NOTE: If the pivot == 0 and udiag is active, then if milu = [col|row]
      //       will not preserve the row sum for that column/row.
      if (w_data[k] == zero)
        {
          if (udiag == 1)
            {
              w_data[k] = droptol;
              iw_u.insert (k);
            }
          else
            {
              error ("ilu: encountered a pivot equal to 0");
              break;
            }
        }

      // Scale the elements on the L part for IKJ version (milu = [col|off])
      if (opt != ROW)
        for (it = iw_l.begin (); it != iw_l.end (); ++it)
          {
            p_perm = iperm[*it];
            w_data[p_perm] = w_data[p_perm] / w_data[k];
          }


      if ((max_len_u - total_len_u) < n)
        {
          max_len_u += (0.1 * max_len_u) > n ? 0.1 * max_len_u : n;
          data_out_u.resize (dim_vector (max_len_u, 1));
          data_u = data_out_u.fortran_vec ();
          ridx_out_u.resize (dim_vector (max_len_u, 1));
          ridx_u = ridx_out_u.fortran_vec ();
        }

      if ((max_len_l - total_len_l) < n)
        {
          max_len_l += (0.1 * max_len_l) > n ? 0.1 * max_len_l : n;
          data_out_l.resize (dim_vector (max_len_l, 1));
          data_l = data_out_l.fortran_vec ();
          ridx_out_l.resize (dim_vector (max_len_l, 1));
          ridx_l = ridx_out_l.fortran_vec ();
        }

      // Expand working vector into U.
      w_len_u = 0;
      for (it = iw_u.begin (); it != iw_u.end (); ++it)
        {
          if (w_data[*it] != zero)
            {
              data_u[total_len_u + w_len_u] = w_data[*it];
              ridx_u[total_len_u + w_len_u] = *it;
              w_len_u++;
            }
          w_data[*it] = 0;
        }
      // Expand working vector into L.
      w_len_l = 0;
      for (it = iw_l.begin (); it != iw_l.end (); ++it)
        {
          p_perm = iperm[*it];
          if (w_data[p_perm] != zero)
            {
              data_l[total_len_l + w_len_l] = w_data[p_perm];
              ridx_l[total_len_l + w_len_l] = *it;
              w_len_l++;
            }
          w_data[p_perm] = 0;
        }
      total_len_u += w_len_u;
      total_len_l += w_len_l;
      // Check if there are too many elements to be indexed with
      // octave_idx_type type due to fill-in during the process.
      if (total_len_l < 0 || total_len_u < 0)
        {
          error ("ilu: Integer overflow.  Too many fill-in elements in L or U");
          break;
        }
      if (opt == ROW)
        uptr[k] = total_len_u - 1;
      cidx_u[k+1] = cidx_u[k] - cidx_u[0] + w_len_u;
      cidx_l[k+1] = cidx_l[k] - cidx_l[0] + w_len_l;

      iw_l.clear ();
      iw_u.clear ();
    }

  if (! error_state)
    {
      octave_matrix_t *L_ptr;
      octave_matrix_t *U_ptr;
      octave_matrix_t diag (n, n, n);

      // L and U are interchanged if milu = 'row'.  It is a matter
      // of nomenclature to re-use code with both IKJ and JKI
      // versions of the algorithm.
      if (opt == ROW)
        {
          L_ptr = &U;
          U_ptr = &L;
          L = octave_matrix_t (n, n, total_len_u - n);
          U = octave_matrix_t (n, n, total_len_l);
        }
      else
        {
          L_ptr = &L;
          U_ptr = &U;
          L = octave_matrix_t (n, n, total_len_l);
          U = octave_matrix_t (n, n, total_len_u);
        }

      for (i = 0; i <= n; i++)
        {
          L_ptr->cidx (i) = cidx_l[i];
          U_ptr->cidx (i) = cidx_u[i];
          if (opt == ROW)
            U_ptr->cidx (i) -= i;
        }

      for (i = 0; i < n; i++)
        {
          if (opt == ROW)
            diag.elem (i,i) = data_u[uptr[i]];
          j = cidx_l[i];

          while (j < cidx_l[i+1])
            {
              L_ptr->ridx (j) = ridx_l[j];
              L_ptr->data (j) = data_l[j];
              j++;
            }
          j = cidx_u[i];

          while (j < cidx_u[i+1])
            {
              c = j;
              if (opt == ROW)
                {
                  // The diagonal is removed from L if milu = 'row'.
                  // That is because is convenient to have it inside
                  // the L part to carry out the process.
                  if (ridx_u[j] == i)
                    {
                      j++;
                      continue;
                    }
                  else
                    c -= i;
                }
              U_ptr->data (c) = data_u[j];
              U_ptr->ridx (c) = ridx_u[j];
              j++;
            }
        }

      if (opt == ROW)
        {
          U = U.transpose ();
          // The diagonal, conveniently permuted is added to U
          U += diag.index (idx_vector::colon, perm_vec);
          L = L.transpose ();
        }
    }
}

DEFUN (__ilutp__, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {[@var{L}, @var{U}] =} __ilutp__ (@var{A})\n\
@deftypefnx {Built-in Function} {[@var{L}, @var{U}] =} __ilutp__ (@var{A}, @var{droptol})\n\
@deftypefnx {Built-in Function} {[@var{L}, @var{U}] =} __ilutp__ (@var{A}, @var{droptol}, @var{thresh})\n\
@deftypefnx {Built-in Function} {[@var{L}, @var{U}] =} __ilutp__ (@var{A}, @var{droptol}, @var{thresh}, @var{milu})\n\
@deftypefnx {Built-in Function} {[@var{L}, @var{U}] =} __ilutp__ (@var{A}, @var{droptol}, @var{thresh}, @var{milu}, @var{udiag})\n\
@deftypefnx {Built-in Function} {[@var{L}, @var{U}, @var{P}] =} __ilutp__ (@var{A}, @dots{})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();
  std::string milu = "";
  double droptol = 0, thresh = 1;
  double udiag = 0;

  if (nargout < 2 || nargout > 3 || nargin < 1 || nargin > 5)
    {
      print_usage ();
      return retval;
    }

  // Don't repeat input validation of arguments done in ilu.m
  if (nargin >= 2)
    droptol = args(1).double_value ();

  if (nargin >= 3)
    thresh = args(2).double_value ();

  if (nargin >= 4)
    milu = args(3).string_value ();

  if (nargin == 5)
    udiag = args(4).double_value ();

  octave_value_list param_list;
  octave_idx_type nnz_u, nnz_l;
  if (! args(0).is_complex_type ())
    {
      Array <double> rc_norm;
      SparseMatrix sm = args(0).sparse_matrix_value ();
      param_list.append (sm);
      nnz_u =  (feval ("triu", param_list)(0).sparse_matrix_value ()).nnz ();
      param_list.append (-1);
      nnz_l =  (feval ("tril", param_list)(0).sparse_matrix_value ()).nnz ();
      if (milu == "row")
        param_list (1) = "rows";
      else
        param_list (1) = "cols";
      rc_norm = feval ("norm", param_list)(0).vector_value ();
      param_list.clear ();
      Array <octave_idx_type> perm (dim_vector (sm.cols (), 1));
      SparseMatrix U;
      SparseMatrix L;
      ilu_tp <SparseMatrix, double> (sm, L, U, nnz_u, nnz_l,
                                     rc_norm.fortran_vec (),
                                     perm, droptol, thresh, milu, udiag);
      if (! error_state)
        {
          param_list.append (octave_value (L.cols ()));
          SparseMatrix eye =
            feval ("speye", param_list)(0).sparse_matrix_value ();
          if (milu == "row")
            {
              if (nargout == 3)
                {
                  retval(2) = eye.index (idx_vector::colon, perm);
                  retval(1) = U.index (idx_vector::colon, perm);
                }
              else if (nargout == 2)
                retval(1) = U;
              retval(0) = L + eye;
            }
          else
            {
              if (nargout == 3)
                {
                  retval(2) = eye.index (perm, idx_vector::colon);
                  retval(1) = U;
                  retval(0) = L.index (perm, idx_vector::colon) + eye;
                }
              else
                {
                  retval(1) = U;
                  retval(0) = L + eye.index (perm, idx_vector::colon);
                }
            }
        }
    }
  else
    {
      Array <Complex> rc_norm;
      SparseComplexMatrix sm = args(0).sparse_complex_matrix_value ();
      param_list.append (sm);
      nnz_u =
        feval ("triu", param_list)(0).sparse_complex_matrix_value ().nnz ();
      param_list.append (-1);
      nnz_l =
        feval ("tril", param_list)(0).sparse_complex_matrix_value ().nnz ();
      if (milu == "row")
        param_list(1) = "rows";
      else
        param_list(1) = "cols";
      rc_norm = feval ("norm", param_list)(0).complex_vector_value ();
      Array <octave_idx_type> perm (dim_vector (sm.cols (), 1));
      param_list.clear ();
      SparseComplexMatrix U;
      SparseComplexMatrix L;
      ilu_tp < SparseComplexMatrix, Complex>
              (sm, L, U, nnz_u, nnz_l, rc_norm.fortran_vec (), perm,
               Complex (droptol), Complex (thresh), milu, udiag);

      if (! error_state)
        {
          param_list.append (octave_value (L.cols ()));
          SparseComplexMatrix eye =
            feval ("speye", param_list)(0).sparse_complex_matrix_value ();
          if (milu == "row")
            {
              if (nargout == 3)
                {
                  retval(2) = eye.index (idx_vector::colon, perm);
                  retval(1) = U.index (idx_vector::colon, perm);
                }
              else if (nargout == 2)
                retval(1) = U;
              retval(0) = L + eye;
            }
          else
            {
              if (nargout == 3)
                {
                  retval(2) = eye.index (perm, idx_vector::colon);
                  retval(1) = U;
                  retval(0) = L.index (perm, idx_vector::colon) + eye;
                }
              else
                {
                  retval(1) = U;
                  retval(0) = L + eye.index (perm, idx_vector::colon);
                }
            }
        }
    }

  return retval;
}

/*
## No test needed for internal helper function.
%!assert (1)
*/

