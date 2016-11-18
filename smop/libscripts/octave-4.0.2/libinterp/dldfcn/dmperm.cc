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

#include "defun-dld.h"
#include "error.h"
#include "gripes.h"
#include "oct-obj.h"
#include "utils.h"

#include "oct-sparse.h"
#include "ov-re-sparse.h"
#include "ov-cx-sparse.h"
#include "SparseQR.h"
#include "SparseCmplxQR.h"

#ifdef USE_64_BIT_IDX_T
#define CXSPARSE_NAME(name) cs_dl ## name
#else
#define CXSPARSE_NAME(name) cs_di ## name
#endif

static RowVector
put_int (octave_idx_type *p, octave_idx_type n)
{
  RowVector ret (n);
  for (octave_idx_type i = 0; i < n; i++)
    ret.xelem (i) = p[i] + 1;
  return ret;
}

#if HAVE_CXSPARSE
static octave_value_list
dmperm_internal (bool rank, const octave_value arg, int nargout)
{
  octave_value_list retval;
  octave_idx_type nr = arg.rows ();
  octave_idx_type nc = arg.columns ();
  SparseMatrix m;
  SparseComplexMatrix cm;
  CXSPARSE_NAME () csm;
  csm.m = nr;
  csm.n = nc;
  csm.x = 0;
  csm.nz = -1;

  if (arg.is_real_type ())
    {
      m = arg.sparse_matrix_value ();
      csm.nzmax = m.nnz ();
      csm.p = m.xcidx ();
      csm.i = m.xridx ();
    }
  else
    {
      cm = arg.sparse_complex_matrix_value ();
      csm.nzmax = cm.nnz ();
      csm.p = cm.xcidx ();
      csm.i = cm.xridx ();
    }

  if (!error_state)
    {
      if (nargout <= 1 || rank)
        {
#if defined(CS_VER) && (CS_VER >= 2)
          octave_idx_type *jmatch = CXSPARSE_NAME (_maxtrans) (&csm, 0);
#else
          octave_idx_type *jmatch = CXSPARSE_NAME (_maxtrans) (&csm);
#endif
          if (rank)
            {
              octave_idx_type r = 0;
              for (octave_idx_type i = 0; i < nc; i++)
                if (jmatch[nr+i] >= 0)
                  r++;
              retval(0) = static_cast<double>(r);
            }
          else
            retval(0) = put_int (jmatch + nr, nc);
          CXSPARSE_NAME (_free) (jmatch);
        }
      else
        {
#if defined(CS_VER) && (CS_VER >= 2)
          CXSPARSE_NAME (d) *dm = CXSPARSE_NAME(_dmperm) (&csm, 0);
#else
          CXSPARSE_NAME (d) *dm = CXSPARSE_NAME(_dmperm) (&csm);
#endif

          //retval(5) = put_int (dm->rr, 5);
          //retval(4) = put_int (dm->cc, 5);
#if defined(CS_VER) && (CS_VER >= 2)
          retval(3) = put_int (dm->s, dm->nb+1);
          retval(2) = put_int (dm->r, dm->nb+1);
          retval(1) = put_int (dm->q, nc);
          retval(0) = put_int (dm->p, nr);
#else
          retval(3) = put_int (dm->S, dm->nb+1);
          retval(2) = put_int (dm->R, dm->nb+1);
          retval(1) = put_int (dm->Q, nc);
          retval(0) = put_int (dm->P, nr);
#endif
          CXSPARSE_NAME (_dfree) (dm);
        }
    }
  return retval;
}
#endif

DEFUN_DLD (dmperm, args, nargout,
           "-*- texinfo -*-\n\
@deftypefn  {Loadable Function} {@var{p} =} dmperm (@var{S})\n\
@deftypefnx {Loadable Function} {[@var{p}, @var{q}, @var{r}, @var{S}] =} dmperm (@var{S})\n\
\n\
@cindex @nospell{Dulmage-Mendelsohn} decomposition\n\
Perform a @nospell{Dulmage-Mendelsohn} permutation of the sparse matrix\n\
@var{S}.\n\
\n\
With a single output argument @code{dmperm} performs the row permutations\n\
@var{p} such that @code{@var{S}(@var{p},:)} has no zero elements on the\n\
diagonal.\n\
\n\
Called with two or more output arguments, returns the row and column\n\
permutations, such that @code{@var{S}(@var{p}, @var{q})} is in block\n\
triangular form.  The values of @var{r} and @var{S} define the boundaries\n\
of the blocks.  If @var{S} is square then @code{@var{r} == @var{S}}.\n\
\n\
The method used is described in: @nospell{A. Pothen & C.-J. Fan.}\n\
@cite{Computing the Block Triangular Form of a Sparse Matrix}.\n\
ACM Trans. Math. Software, 16(4):303-324, 1990.\n\
@seealso{colamd, ccolamd}\n\
@end deftypefn")
{
  int nargin = args.length ();
  octave_value_list retval;

  if (nargin != 1)
    {
      print_usage ();
      return retval;
    }

#if HAVE_CXSPARSE
  retval = dmperm_internal (false, args(0), nargout);
#else
  error ("dmperm: not available in this version of Octave");
#endif

  return retval;
}

/*
%!testif HAVE_CXSPARSE
%! n = 20;
%! a = speye (n,n);
%! a = a(randperm (n),:);
%! assert (a(dmperm (a),:), speye (n));

%!testif HAVE_CXSPARSE
%! n = 20;
%! d = 0.2;
%! a = tril (sprandn (n,n,d), -1) + speye (n,n);
%! a = a(randperm (n), randperm (n));
%! [p,q,r,s] = dmperm (a);
%! assert (tril (a(p,q), -1), sparse (n, n));
*/

DEFUN_DLD (sprank, args, nargout,
           "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {@var{p} =} sprank (@var{S})\n\
@cindex structural rank\n\
\n\
Calculate the structural rank of the sparse matrix @var{S}.\n\
\n\
Note that only the structure of the matrix is used in this calculation based\n\
on a @nospell{Dulmage-Mendelsohn} permutation to block triangular form.  As\n\
such the numerical rank of the matrix @var{S} is bounded by\n\
@code{sprank (@var{S}) >= rank (@var{S})}.  Ignoring floating point errors\n\
@code{sprank (@var{S}) == rank (@var{S})}.\n\
@seealso{dmperm}\n\
@end deftypefn")
{
  int nargin = args.length ();
  octave_value_list retval;

  if (nargin != 1)
    {
      print_usage ();
      return retval;
    }

#if HAVE_CXSPARSE
  retval = dmperm_internal (true, args(0), nargout);
#else
  error ("sprank: not available in this version of Octave");
#endif

  return retval;
}

/*
%!testif HAVE_CXSPARSE
%! assert (sprank (speye (20)), 20)
%!testif HAVE_CXSPARSE
%! assert (sprank ([1,0,2,0;2,0,4,0]), 2)

%!error sprank (1,2)
*/
