/*

Copyright (C) 2005-2015 David Bateman
Copyright (C) 1998-2005 Andy Adler

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

#include "SparseCmplxCHOL.h"
#include "SparsedbleCHOL.h"
#include "oct-spparms.h"
#include "sparse-util.h"
#include "oct-locbuf.h"

#include "ov-re-sparse.h"
#include "ov-cx-sparse.h"
#include "defun-dld.h"
#include "error.h"
#include "gripes.h"
#include "oct-obj.h"
#include "utils.h"

DEFUN_DLD (symbfact, args, nargout,
           "-*- texinfo -*-\n\
@deftypefn  {Loadable Function} {[@var{count}, @var{h}, @var{parent}, @var{post}, @var{r}] =} symbfact (@var{S})\n\
@deftypefnx {Loadable Function} {[@dots{}] =} symbfact (@var{S}, @var{typ})\n\
@deftypefnx {Loadable Function} {[@dots{}] =} symbfact (@var{S}, @var{typ}, @var{mode})\n\
\n\
Perform a symbolic factorization analysis on the sparse matrix @var{S}.\n\
\n\
The input variables are\n\
\n\
@table @var\n\
@item S\n\
@var{S} is a complex or real sparse matrix.\n\
\n\
@item typ\n\
Is the type of the factorization and can be one of\n\
\n\
@table @samp\n\
@item sym\n\
Factorize @var{S}.  This is the default.\n\
\n\
@item col\n\
Factorize @code{@var{S}' * @var{S}}.\n\
\n\
@item row\n\
Factorize @tcode{@var{S} * @var{S}'}.\n\
\n\
@item lo\n\
Factorize @tcode{@var{S}'}\n\
@end table\n\
\n\
@item mode\n\
The default is to return the Cholesky@tie{}factorization for @var{r}, and if\n\
@var{mode} is @qcode{'L'}, the conjugate transpose of the\n\
Cholesky@tie{}factorization is returned.  The conjugate transpose version is\n\
faster and uses less memory, but returns the same values for @var{count},\n\
@var{h}, @var{parent} and @var{post} outputs.\n\
@end table\n\
\n\
The output variables are\n\
\n\
@table @var\n\
@item count\n\
The row counts of the Cholesky@tie{}factorization as determined by @var{typ}.\n\
\n\
@item h\n\
The height of the elimination tree.\n\
\n\
@item parent\n\
The elimination tree itself.\n\
\n\
@item post\n\
A sparse boolean matrix whose structure is that of the Cholesky\n\
factorization as determined by @var{typ}.\n\
@end table\n\
@end deftypefn")
{
  octave_value_list retval;
  int nargin = args.length ();

  if (nargin < 1  || nargin > 3 || nargout > 5)
    {
      print_usage ();
      return retval;
    }

#ifdef HAVE_CHOLMOD

  cholmod_common Common;
  cholmod_common *cm = &Common;
  CHOLMOD_NAME(start) (cm);

  double spu = octave_sparse_params::get_key ("spumoni");
  if (spu == 0.)
    {
      cm->print = -1;
      SUITESPARSE_ASSIGN_FPTR (printf_func, cm->print_function, 0);
    }
  else
    {
      cm->print = static_cast<int> (spu) + 2;
      SUITESPARSE_ASSIGN_FPTR (printf_func, cm->print_function, &SparseCholPrint);
    }

  cm->error_handler = &SparseCholError;
  SUITESPARSE_ASSIGN_FPTR2 (divcomplex_func, cm->complex_divide, divcomplex);
  SUITESPARSE_ASSIGN_FPTR2 (hypot_func, cm->hypotenuse, hypot);

  double dummy;
  cholmod_sparse Astore;
  cholmod_sparse *A = &Astore;
  A->packed = true;
  A->sorted = true;
  A->nz = 0;
#ifdef USE_64_BIT_IDX_T
  A->itype = CHOLMOD_LONG;
#else
  A->itype = CHOLMOD_INT;
#endif
  A->dtype = CHOLMOD_DOUBLE;
  A->stype = 1;
  A->x = &dummy;

  if (args(0).is_real_type ())
    {
      const SparseMatrix a = args(0).sparse_matrix_value ();
      A->nrow = a.rows ();
      A->ncol = a.cols ();
      A->p = a.cidx ();
      A->i = a.ridx ();
      A->nzmax = a.nnz ();
      A->xtype = CHOLMOD_REAL;

      if (a.rows () > 0 && a.cols () > 0)
        A->x = a.data ();
    }
  else if (args(0).is_complex_type ())
    {
      const SparseComplexMatrix a = args(0).sparse_complex_matrix_value ();
      A->nrow = a.rows ();
      A->ncol = a.cols ();
      A->p = a.cidx ();
      A->i = a.ridx ();
      A->nzmax = a.nnz ();
      A->xtype = CHOLMOD_COMPLEX;

      if (a.rows () > 0 && a.cols () > 0)
        A->x = a.data ();
    }
  else
    gripe_wrong_type_arg ("symbfact", args(0));

  octave_idx_type coletree = false;
  octave_idx_type n = A->nrow;

  if (nargin > 1)
    {
      char ch;
      std::string str = args(1).string_value ();
      ch = tolower (str.c_str ()[0]);
      if (ch == 'r')
        A->stype = 0;
      else if (ch == 'c')
        {
          n = A->ncol;
          coletree = true;
          A->stype = 0;
        }
      else if (ch == 's')
        A->stype = 1;
      else if (ch == 's')
        A->stype = -1;
      else
        error ("symbfact: unrecognized TYP in symbolic factorization");
    }

  if (A->stype && A->nrow != A->ncol)
    error ("symbfact: S must be a square matrix");

  if (!error_state)
    {
      OCTAVE_LOCAL_BUFFER (octave_idx_type, Parent, n);
      OCTAVE_LOCAL_BUFFER (octave_idx_type, Post, n);
      OCTAVE_LOCAL_BUFFER (octave_idx_type, ColCount, n);
      OCTAVE_LOCAL_BUFFER (octave_idx_type, First, n);
      OCTAVE_LOCAL_BUFFER (octave_idx_type, Level, n);

      cholmod_sparse *F = CHOLMOD_NAME(transpose) (A, 0, cm);
      cholmod_sparse *Aup, *Alo;

      if (A->stype == 1 || coletree)
        {
          Aup = A ;
          Alo = F ;
        }
      else
        {
          Aup = F ;
          Alo = A ;
        }

      CHOLMOD_NAME(etree) (Aup, Parent, cm);

      if (cm->status < CHOLMOD_OK)
        {
          error ("matrix corrupted");
          goto symbfact_error;
        }

      if (CHOLMOD_NAME(postorder) (Parent, n, 0, Post, cm) != n)
        {
          error ("postorder failed");
          goto symbfact_error;
        }

      CHOLMOD_NAME(rowcolcounts) (Alo, 0, 0, Parent, Post, 0,
                                  ColCount, First, Level, cm);

      if (cm->status < CHOLMOD_OK)
        {
          error ("matrix corrupted");
          goto symbfact_error;
        }

      if (nargout > 4)
        {
          cholmod_sparse *A1, *A2;

          if (A->stype == 1)
            {
              A1 = A;
              A2 = 0;
            }
          else if (A->stype == -1)
            {
              A1 = F;
              A2 = 0;
            }
          else if (coletree)
            {
              A1 = F;
              A2 = A;
            }
          else
            {
              A1 = A;
              A2 = F;
            }

          // count the total number of entries in L
          octave_idx_type lnz = 0 ;
          for (octave_idx_type j = 0 ; j < n ; j++)
            lnz += ColCount[j];


          // allocate the output matrix L (pattern-only)
          SparseBoolMatrix L (n, n, lnz);

          // initialize column pointers
          lnz = 0;
          for (octave_idx_type j = 0 ; j < n ; j++)
            {
              L.xcidx(j) = lnz;
              lnz += ColCount[j];
            }
          L.xcidx(n) = lnz;


          /* create a copy of the column pointers */
          octave_idx_type *W = First;
          for (octave_idx_type j = 0 ; j < n ; j++)
            W[j] = L.xcidx (j);

          // get workspace for computing one row of L
          cholmod_sparse *R
            = CHOLMOD_NAME (allocate_sparse) (n, 1, n, false, true,
                                              0, CHOLMOD_PATTERN, cm);
          octave_idx_type *Rp = static_cast<octave_idx_type *>(R->p);
          octave_idx_type *Ri = static_cast<octave_idx_type *>(R->i);

          // compute L one row at a time
          for (octave_idx_type k = 0 ; k < n ; k++)
            {
              // get the kth row of L and store in the columns of L
              CHOLMOD_NAME (row_subtree) (A1, A2, k, Parent, R, cm) ;
              for (octave_idx_type p = 0 ; p < Rp[1] ; p++)
                L.xridx (W[Ri[p]]++) = k ;

              // add the diagonal entry
              L.xridx (W[k]++) = k ;
            }

          // free workspace
          CHOLMOD_NAME (free_sparse) (&R, cm) ;


          // transpose L to get R, or leave as is
          if (nargin < 3)
            L = L.transpose ();

          // fill numerical values of L with one's
          for (octave_idx_type p = 0 ; p < lnz ; p++)
            L.xdata(p) = true;

          retval(4) = L;
        }

      ColumnVector tmp (n);
      if (nargout > 3)
        {
          for (octave_idx_type i = 0; i < n; i++)
            tmp(i) = Post[i] + 1;
          retval(3) = tmp;
        }

      if (nargout > 2)
        {
          for (octave_idx_type i = 0; i < n; i++)
            tmp(i) = Parent[i] + 1;
          retval(2) = tmp;
        }

      if (nargout > 1)
        {
          /* compute the elimination tree height */
          octave_idx_type height = 0 ;
          for (int i = 0 ; i < n ; i++)
            height = (height > Level[i] ? height : Level[i]);
          height++ ;
          retval(1) = static_cast<double> (height);
        }

      for (octave_idx_type i = 0; i < n; i++)
        tmp(i) = ColCount[i];
      retval(0) = tmp;
    }

symbfact_error:
#else
  error ("symbfact: not available in this version of Octave");
#endif

  return retval;
}
