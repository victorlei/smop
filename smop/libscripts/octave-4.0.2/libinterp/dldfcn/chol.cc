/*

Copyright (C) 1996-2015 John W. Eaton
Copyright (C) 2008-2009 Jaroslav Hajek
Copyright (C) 2008-2009 VZLU Prague

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

#include "CmplxCHOL.h"
#include "dbleCHOL.h"
#include "fCmplxCHOL.h"
#include "floatCHOL.h"
#include "SparseCmplxCHOL.h"
#include "SparsedbleCHOL.h"
#include "oct-spparms.h"
#include "sparse-util.h"

#include "ov-re-sparse.h"
#include "ov-cx-sparse.h"
#include "defun-dld.h"
#include "error.h"
#include "gripes.h"
#include "oct-obj.h"
#include "utils.h"

template <class CHOLT>
static octave_value
get_chol_r (const CHOLT& fact)
{
  return octave_value (fact.chol_matrix (),
                       MatrixType (MatrixType::Upper));
}

template <class CHOLT>
static octave_value
get_chol_l (const CHOLT& fact)
{
  return octave_value (fact.chol_matrix ().transpose (),
                       MatrixType (MatrixType::Lower));
}

DEFUN_DLD (chol, args, nargout,
           "-*- texinfo -*-\n\
@deftypefn  {Loadable Function} {@var{R} =} chol (@var{A})\n\
@deftypefnx {Loadable Function} {[@var{R}, @var{p}] =} chol (@var{A})\n\
@deftypefnx {Loadable Function} {[@var{R}, @var{p}, @var{Q}] =} chol (@var{S})\n\
@deftypefnx {Loadable Function} {[@var{R}, @var{p}, @var{Q}] =} chol (@var{S}, \"vector\")\n\
@deftypefnx {Loadable Function} {[@var{L}, @dots{}] =} chol (@dots{}, \"lower\")\n\
@deftypefnx {Loadable Function} {[@var{L}, @dots{}] =} chol (@dots{}, \"upper\")\n\
@cindex Cholesky factorization\n\
Compute the Cholesky@tie{}factor, @var{R}, of the symmetric positive definite\n\
matrix @var{A}.\n\
\n\
The Cholesky@tie{}factor is defined by\n\
@tex\n\
$ R^T R = A $.\n\
@end tex\n\
@ifnottex\n\
\n\
@example\n\
@var{R}' * @var{R} = @var{A}.\n\
@end example\n\
\n\
@end ifnottex\n\
\n\
Called with one output argument @code{chol} fails if @var{A} or @var{S} is\n\
not positive definite.  With two or more output arguments @var{p} flags\n\
whether the matrix was positive definite and @code{chol} does not fail.  A\n\
zero value indicated that the matrix was positive definite and the @var{R}\n\
gives the factorization, and @var{p} will have a positive value otherwise.\n\
\n\
If called with 3 outputs then a sparsity preserving row/column permutation\n\
is applied to @var{A} prior to the factorization.  That is @var{R} is the\n\
factorization of @code{@var{A}(@var{Q},@var{Q})} such that\n\
@tex\n\
$ R^T R = Q^T A Q$.\n\
@end tex\n\
@ifnottex\n\
\n\
@example\n\
@var{R}' * @var{R} = @var{Q}' * @var{A} * @var{Q}.\n\
@end example\n\
\n\
@end ifnottex\n\
\n\
The sparsity preserving permutation is generally returned as a matrix.\n\
However, given the flag @qcode{\"vector\"}, @var{Q} will be returned as a\n\
vector such that\n\
@tex\n\
$ R^T R = A (Q, Q)$.\n\
@end tex\n\
@ifnottex\n\
\n\
@example\n\
@var{R}' * @var{R} = @var{A}(@var{Q}, @var{Q}).\n\
@end example\n\
\n\
@end ifnottex\n\
\n\
Called with either a sparse or full matrix and using the @qcode{\"lower\"}\n\
flag, @code{chol} returns the lower triangular factorization such that\n\
@tex\n\
$ L L^T = A $.\n\
@end tex\n\
@ifnottex\n\
\n\
@example\n\
@var{L} * @var{L}' = @var{A}.\n\
@end example\n\
\n\
@end ifnottex\n\
\n\
For full matrices, if the @qcode{\"lower\"} flag is set only the lower\n\
triangular part of the matrix is used for the factorization, otherwise the\n\
upper triangular part is used.\n\
\n\
In general the lower triangular factorization is significantly faster for\n\
sparse matrices.\n\
@seealso{hess, lu, qr, qz, schur, svd, ichol, cholinv, chol2inv, cholupdate, cholinsert, choldelete, cholshift}\n\
@end deftypefn")
{
  octave_value_list retval;
  int nargin = args.length ();
  bool LLt = false;
  bool vecout = false;

  if (nargin < 1 || nargin > 3 || nargout > 3
      || (! args(0).is_sparse_type () && nargout > 2))
    {
      print_usage ();
      return retval;
    }

  int n = 1;
  while (n < nargin && ! error_state)
    {
      std::string tmp = args(n++).string_value ();

      if (! error_state)
        {
          if (tmp.compare ("vector") == 0)
            vecout = true;
          else if (tmp.compare ("lower") == 0)
            // FIXME: currently the option "lower" is handled by transposing
            //  the matrix, factorizing it with the lapack function
            //  DPOTRF ('U', ...) and finally transposing the factor.  It would
            //  be more efficient to use DPOTRF ('L', ...) in this case.
            LLt = true;
          else if (tmp.compare ("upper") == 0)
            LLt = false;
          else
            error ("chol: unexpected second or third input");
        }
      else
        error ("chol: expecting trailing string arguments");
    }

  if (! error_state)
    {
      octave_value arg = args(0);

      octave_idx_type nr = arg.rows ();
      octave_idx_type nc = arg.columns ();

      int arg_is_empty = empty_arg ("chol", nr, nc);

      if (arg_is_empty < 0)
        return retval;
      if (arg_is_empty > 0)
        return octave_value (Matrix ());

      if (arg.is_sparse_type ())
        {
          octave_idx_type info;
          bool natural = (nargout != 3);
          bool force = nargout > 1;

          if (arg.is_real_type ())
            {
              SparseMatrix m = arg.sparse_matrix_value ();

              if (! error_state)
                {
                  SparseCHOL fact (m, info, natural, force);

                  if (nargout == 3)
                    {
                      if (vecout)
                        retval(2) = fact.perm ();
                      else
                        retval(2) = fact.Q ();
                    }

                  if (nargout > 1 || info == 0)
                    {
                      retval(1) = info;
                      if (LLt)
                        retval(0) = fact.L ();
                      else
                        retval(0) = fact.R ();
                    }
                  else
                    error ("chol: input matrix must be positive definite");
                }
            }
          else if (arg.is_complex_type ())
            {
              SparseComplexMatrix m = arg.sparse_complex_matrix_value ();

              if (! error_state)
                {
                  SparseComplexCHOL fact (m, info, natural, force);

                  if (nargout == 3)
                    {
                      if (vecout)
                        retval(2) = fact.perm ();
                      else
                        retval(2) = fact.Q ();
                    }

                  if (nargout > 1 || info == 0)
                    {
                      retval(1) = info;
                      if (LLt)
                        retval(0) = fact.L ();
                      else
                        retval(0) = fact.R ();
                    }
                  else
                    error ("chol: input matrix must be positive definite");
                }
            }
          else
            gripe_wrong_type_arg ("chol", arg);
        }
      else if (arg.is_single_type ())
        {
          if (arg.is_real_type ())
            {
              FloatMatrix m = arg.float_matrix_value ();

              if (! error_state)
                {
                  octave_idx_type info;

                  FloatCHOL fact;
                  if (LLt)
                    fact = FloatCHOL (m.transpose (), info);
                  else
                    fact = FloatCHOL (m, info);

                  if (nargout == 2 || info == 0)
                    {
                      retval(1) = info;
                      if (LLt)
                        retval(0) = get_chol_l (fact);
                      else
                        retval(0) = get_chol_r (fact);
                    }
                  else
                    error ("chol: input matrix must be positive definite");
                }
            }
          else if (arg.is_complex_type ())
            {
              FloatComplexMatrix m = arg.float_complex_matrix_value ();

              if (! error_state)
                {
                  octave_idx_type info;

                  FloatComplexCHOL fact;
                  if (LLt)
                    fact = FloatComplexCHOL (m.transpose (), info);
                  else
                    fact = FloatComplexCHOL (m, info);

                  if (nargout == 2 || info == 0)
                    {
                      retval(1) = info;
                      if (LLt)
                        retval(0) = get_chol_l (fact);
                      else
                        retval(0) = get_chol_r (fact);
                    }
                  else
                    error ("chol: input matrix must be positive definite");
                }
            }
          else
            gripe_wrong_type_arg ("chol", arg);
        }
      else
        {
          if (arg.is_real_type ())
            {
              Matrix m = arg.matrix_value ();

              if (! error_state)
                {
                  octave_idx_type info;

                  CHOL fact;
                  if (LLt)
                    fact = CHOL (m.transpose (), info);
                  else
                    fact = CHOL (m, info);

                  if (nargout == 2 || info == 0)
                    {
                      retval(1) = info;
                      if (LLt)
                        retval(0) = get_chol_l (fact);
                      else
                        retval(0) = get_chol_r (fact);
                    }
                  else
                    error ("chol: input matrix must be positive definite");
                }
            }
          else if (arg.is_complex_type ())
            {
              ComplexMatrix m = arg.complex_matrix_value ();

              if (! error_state)
                {
                  octave_idx_type info;

                  ComplexCHOL fact;
                  if (LLt)
                    fact = ComplexCHOL (m.transpose (), info);
                  else
                    fact = ComplexCHOL (m, info);

                  if (nargout == 2 || info == 0)
                    {
                      retval(1) = info;
                      if (LLt)
                        retval(0) = get_chol_l (fact);
                      else
                        retval(0) = get_chol_r (fact);
                    }
                  else
                    error ("chol: input matrix must be positive definite");
                }
            }
          else
            gripe_wrong_type_arg ("chol", arg);
        }
    }

  return retval;
}

/*
%!assert (chol ([2, 1; 1, 1]), [sqrt(2), 1/sqrt(2); 0, 1/sqrt(2)], sqrt (eps))
%!assert (chol (single ([2, 1; 1, 1])), single ([sqrt(2), 1/sqrt(2); 0, 1/sqrt(2)]), sqrt (eps ("single")))
%!testif HAVE_CHOLMOD
%! ## Bug #42587
%! A = sparse ([1 0 8;0 1 8;8 8 1]);
%! [Q, p] = chol (A);
%! assert (p != 0);

%!error chol ()
%!error <matrix must be positive definite> chol ([1, 2; 3, 4])
%!error <requires square matrix> chol ([1, 2; 3, 4; 5, 6])
%!error <unexpected second or third input> chol (1, 2)
*/

DEFUN_DLD (cholinv, args, ,
           "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} cholinv (@var{A})\n\
Compute the inverse of the symmetric positive definite matrix @var{A} using\n\
the Cholesky@tie{}factorization.\n\
@seealso{chol, chol2inv, inv}\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 1)
    {
      octave_value arg = args(0);

      octave_idx_type nr = arg.rows ();
      octave_idx_type nc = arg.columns ();

      if (nr == 0 || nc == 0)
        retval = Matrix ();
      else
        {
          if (arg.is_sparse_type ())
            {
              octave_idx_type info;

              if (arg.is_real_type ())
                {
                  SparseMatrix m = arg.sparse_matrix_value ();

                  if (! error_state)
                    {
                      SparseCHOL chol (m, info);

                      if (info == 0)
                        retval = chol.inverse ();
                      else
                        error ("cholinv: A must be positive definite");
                    }
                }
              else if (arg.is_complex_type ())
                {
                  SparseComplexMatrix m = arg.sparse_complex_matrix_value ();

                  if (! error_state)
                    {
                      SparseComplexCHOL chol (m, info);

                      if (info == 0)
                        retval = chol.inverse ();
                      else
                        error ("cholinv: A must be positive definite");
                    }
                }
              else
                gripe_wrong_type_arg ("cholinv", arg);
            }
          else if (arg.is_single_type ())
            {
              if (arg.is_real_type ())
                {
                  FloatMatrix m = arg.float_matrix_value ();

                  if (! error_state)
                    {
                      octave_idx_type info;
                      FloatCHOL chol (m, info);
                      if (info == 0)
                        retval = chol.inverse ();
                      else
                        error ("cholinv: A must be positive definite");
                    }
                }
              else if (arg.is_complex_type ())
                {
                  FloatComplexMatrix m = arg.float_complex_matrix_value ();

                  if (! error_state)
                    {
                      octave_idx_type info;
                      FloatComplexCHOL chol (m, info);
                      if (info == 0)
                        retval = chol.inverse ();
                      else
                        error ("cholinv: A must be positive definite");
                    }
                }
              else
                gripe_wrong_type_arg ("chol", arg);
            }
          else
            {
              if (arg.is_real_type ())
                {
                  Matrix m = arg.matrix_value ();

                  if (! error_state)
                    {
                      octave_idx_type info;
                      CHOL chol (m, info);
                      if (info == 0)
                        retval = chol.inverse ();
                      else
                        error ("cholinv: A must be positive definite");
                    }
                }
              else if (arg.is_complex_type ())
                {
                  ComplexMatrix m = arg.complex_matrix_value ();

                  if (! error_state)
                    {
                      octave_idx_type info;
                      ComplexCHOL chol (m, info);
                      if (info == 0)
                        retval = chol.inverse ();
                      else
                        error ("cholinv: A must be positive definite");
                    }
                }
              else
                gripe_wrong_type_arg ("chol", arg);
            }
        }
    }
  else
    print_usage ();

  return retval;
}

/*
%!shared A, Ainv
%! A = [2,0.2;0.2,1];
%! Ainv = inv (A);
%!test
%! Ainv1 = cholinv (A);
%! assert (norm (Ainv-Ainv1), 0, 1e-10);
%!testif HAVE_CHOLMOD
%! Ainv2 = inv (sparse (A));
%! assert (norm (Ainv-Ainv2), 0, 1e-10);
%!testif HAVE_CHOLMOD
%! Ainv3 = cholinv (sparse (A));
%! assert (norm (Ainv-Ainv3), 0, 1e-10);
*/

DEFUN_DLD (chol2inv, args, ,
           "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} chol2inv (@var{U})\n\
Invert a symmetric, positive definite square matrix from its Cholesky\n\
decomposition, @var{U}.\n\
\n\
Note that @var{U} should be an upper-triangular matrix with positive\n\
diagonal elements.  @code{chol2inv (@var{U})} provides\n\
@code{inv (@var{U}'*@var{U})} but it is much faster than using @code{inv}.\n\
@seealso{chol, cholinv, inv}\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 1)
    {
      octave_value arg = args(0);

      octave_idx_type nr = arg.rows ();
      octave_idx_type nc = arg.columns ();

      if (nr == 0 || nc == 0)
        retval = Matrix ();
      else
        {
          if (arg.is_sparse_type ())
            {
              if (arg.is_real_type ())
                {
                  SparseMatrix r = arg.sparse_matrix_value ();

                  if (! error_state)
                    retval = chol2inv (r);
                }
              else if (arg.is_complex_type ())
                {
                  SparseComplexMatrix r = arg.sparse_complex_matrix_value ();

                  if (! error_state)
                    retval = chol2inv (r);
                }
              else
                gripe_wrong_type_arg ("chol2inv", arg);
            }
          else if (arg.is_single_type ())
            {
              if (arg.is_real_type ())
                {
                  FloatMatrix r = arg.float_matrix_value ();

                  if (! error_state)
                    retval = chol2inv (r);
                }
              else if (arg.is_complex_type ())
                {
                  FloatComplexMatrix r = arg.float_complex_matrix_value ();

                  if (! error_state)
                    retval = chol2inv (r);
                }
              else
                gripe_wrong_type_arg ("chol2inv", arg);

            }
          else
            {
              if (arg.is_real_type ())
                {
                  Matrix r = arg.matrix_value ();

                  if (! error_state)
                    retval = chol2inv (r);
                }
              else if (arg.is_complex_type ())
                {
                  ComplexMatrix r = arg.complex_matrix_value ();

                  if (! error_state)
                    retval = chol2inv (r);
                }
              else
                gripe_wrong_type_arg ("chol2inv", arg);
            }
        }
    }
  else
    print_usage ();

  return retval;
}

DEFUN_DLD (cholupdate, args, nargout,
           "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {[@var{R1}, @var{info}] =} cholupdate (@var{R}, @var{u}, @var{op})\n\
Update or downdate a Cholesky@tie{}factorization.\n\
\n\
Given an upper triangular matrix @var{R} and a column vector @var{u},\n\
attempt to determine another upper triangular matrix @var{R1} such that\n\
\n\
@itemize @bullet\n\
@item\n\
@var{R1}'*@var{R1} = @var{R}'*@var{R} + @var{u}*@var{u}'\n\
if @var{op} is @qcode{\"+\"}\n\
\n\
@item\n\
@var{R1}'*@var{R1} = @var{R}'*@var{R} - @var{u}*@var{u}'\n\
if @var{op} is @qcode{\"-\"}\n\
@end itemize\n\
\n\
If @var{op} is @qcode{\"-\"}, @var{info} is set to\n\
\n\
@itemize\n\
@item 0 if the downdate was successful,\n\
\n\
@item 1 if @var{R}'*@var{R} - @var{u}*@var{u}' is not positive definite,\n\
\n\
@item 2 if @var{R} is singular.\n\
@end itemize\n\
\n\
If @var{info} is not present, an error message is printed in cases 1 and 2.\n\
@seealso{chol, cholinsert, choldelete, cholshift}\n\
@end deftypefn")
{
  octave_idx_type nargin = args.length ();

  octave_value_list retval;

  if (nargin > 3 || nargin < 2)
    {
      print_usage ();
      return retval;
    }

  octave_value argr = args(0);
  octave_value argu = args(1);

  if (argr.is_numeric_type () && argu.is_numeric_type ()
      && (nargin < 3 || args(2).is_string ()))
    {
      octave_idx_type n = argr.rows ();

      std::string op = (nargin < 3) ? "+" : args(2).string_value ();

      bool down = op == "-";

      if (down || op == "+")
        if (argr.columns () == n && argu.rows () == n && argu.columns () == 1)
          {
            int err = 0;
            if (argr.is_single_type () || argu.is_single_type ())
              {
                if (argr.is_real_type () && argu.is_real_type ())
                  {
                    // real case
                    FloatMatrix R = argr.float_matrix_value ();
                    FloatColumnVector u = argu.float_column_vector_value ();

                    FloatCHOL fact;
                    fact.set (R);

                    if (down)
                      err = fact.downdate (u);
                    else
                      fact.update (u);

                    retval(0) = get_chol_r (fact);
                  }
                else
                  {
                    // complex case
                    FloatComplexMatrix R = argr.float_complex_matrix_value ();
                    FloatComplexColumnVector u =
                      argu.float_complex_column_vector_value ();

                    FloatComplexCHOL fact;
                    fact.set (R);

                    if (down)
                      err = fact.downdate (u);
                    else
                      fact.update (u);

                    retval(0) = get_chol_r (fact);
                  }
              }
            else
              {
                if (argr.is_real_type () && argu.is_real_type ())
                  {
                    // real case
                    Matrix R = argr.matrix_value ();
                    ColumnVector u = argu.column_vector_value ();

                    CHOL fact;
                    fact.set (R);

                    if (down)
                      err = fact.downdate (u);
                    else
                      fact.update (u);

                    retval(0) = get_chol_r (fact);
                  }
                else
                  {
                    // complex case
                    ComplexMatrix R = argr.complex_matrix_value ();
                    ComplexColumnVector u = argu.complex_column_vector_value ();

                    ComplexCHOL fact;
                    fact.set (R);

                    if (down)
                      err = fact.downdate (u);
                    else
                      fact.update (u);

                    retval(0) = get_chol_r (fact);
                  }
              }

            if (nargout > 1)
              retval(1) = err;
            else if (err == 1)
              error ("cholupdate: downdate violates positiveness");
            else if (err == 2)
              error ("cholupdate: singular matrix");
          }
        else
          error ("cholupdate: dimension mismatch between R and U");
      else
        error ("cholupdate: OP must be \"+\" or \"-\"");
    }
  else
    print_usage ();

  return retval;
}

/*
%!shared A, u, Ac, uc
%! A = [  0.436997  -0.131721   0.124120  -0.061673 ;
%!       -0.131721   0.738529   0.019851  -0.140295 ;
%!        0.124120   0.019851   0.354879  -0.059472 ;
%!       -0.061673  -0.140295  -0.059472   0.600939 ];
%!
%! u = [  0.98950 ;
%!        0.39844 ;
%!        0.63484 ;
%!        0.13351 ];
%! Ac = [  0.5585528 + 0.0000000i  -0.1662088 - 0.0315341i   0.0107873 + 0.0236411i  -0.0276775 - 0.0186073i ;
%!        -0.1662088 + 0.0315341i   0.6760061 + 0.0000000i   0.0011452 - 0.0475528i   0.0145967 + 0.0247641i ;
%!         0.0107873 - 0.0236411i   0.0011452 + 0.0475528i   0.6263149 - 0.0000000i  -0.1585837 - 0.0719763i ;
%!        -0.0276775 + 0.0186073i   0.0145967 - 0.0247641i  -0.1585837 + 0.0719763i   0.6034234 - 0.0000000i ];
%!
%! uc = [ 0.54267 + 0.91519i ;
%!        0.99647 + 0.43141i ;
%!        0.83760 + 0.68977i ;
%!        0.39160 + 0.90378i ];

%!test
%! R = chol (A);
%! R1 = cholupdate (R, u);
%! assert (norm (triu (R1)-R1, Inf), 0);
%! assert (norm (R1'*R1 - R'*R - u*u', Inf) < 1e1*eps);
%!
%! R1 = cholupdate (R1, u, "-");
%! assert (norm (triu (R1)-R1, Inf), 0);
%! assert (norm (R1 - R, Inf) < 1e1*eps);

%!test
%! R = chol (Ac);
%! R1 = cholupdate (R, uc);
%! assert (norm (triu (R1)-R1, Inf), 0);
%! assert (norm (R1'*R1 - R'*R - uc*uc', Inf) < 1e1*eps);
%!
%! R1 = cholupdate (R1, uc, "-");
%! assert (norm (triu (R1)-R1, Inf), 0);
%! assert (norm (R1 - R, Inf) < 1e1*eps);

%!test
%! R = chol (single (A));
%! R1 = cholupdate (R, single (u));
%! assert (norm (triu (R1)-R1, Inf), single (0));
%! assert (norm (R1'*R1 - R'*R - single (u*u'), Inf) < 1e1*eps ("single"));
%!
%! R1 = cholupdate (R1, single (u), "-");
%! assert (norm (triu (R1)-R1, Inf), single (0));
%! assert (norm (R1 - R, Inf) < 2e1*eps ("single"));

%!test
%! R = chol (single (Ac));
%! R1 = cholupdate (R, single (uc));
%! assert (norm (triu (R1)-R1, Inf), single (0));
%! assert (norm (R1'*R1 - R'*R - single (uc*uc'), Inf) < 1e1*eps ("single"));
%!
%! R1 = cholupdate (R1, single (uc), "-");
%! assert (norm (triu (R1)-R1, Inf), single (0));
%! assert (norm (R1 - R, Inf) < 2e1*eps ("single"));
*/

DEFUN_DLD (cholinsert, args, nargout,
           "-*- texinfo -*-\n\
@deftypefn  {Loadable Function} {@var{R1} =} cholinsert (@var{R}, @var{j}, @var{u})\n\
@deftypefnx {Loadable Function} {[@var{R1}, @var{info}] =} cholinsert (@var{R}, @var{j}, @var{u})\n\
Given a Cholesky@tie{}factorization of a real symmetric or complex Hermitian\n\
positive definite matrix @w{@var{A} = @var{R}'*@var{R}}, @var{R}@tie{}upper\n\
triangular, return the Cholesky@tie{}factorization of\n\
@var{A1}, where @w{A1(p,p) = A}, @w{A1(:,j) = A1(j,:)' = u} and\n\
@w{p = [1:j-1,j+1:n+1]}.  @w{u(j)} should be positive.\n\
\n\
On return, @var{info} is set to\n\
\n\
@itemize\n\
@item 0 if the insertion was successful,\n\
\n\
@item 1 if @var{A1} is not positive definite,\n\
\n\
@item 2 if @var{R} is singular.\n\
@end itemize\n\
\n\
If @var{info} is not present, an error message is printed in cases 1 and 2.\n\
@seealso{chol, cholupdate, choldelete, cholshift}\n\
@end deftypefn")
{
  octave_idx_type nargin = args.length ();

  octave_value_list retval;

  if (nargin != 3)
    {
      print_usage ();
      return retval;
    }

  octave_value argr = args(0);
  octave_value argj = args(1);
  octave_value argu = args(2);

  if (argr.is_numeric_type () && argu.is_numeric_type ()
      && argj.is_real_scalar ())
    {
      octave_idx_type n = argr.rows ();
      octave_idx_type j = argj.scalar_value ();

      if (argr.columns () == n && argu.rows () == n+1 && argu.columns () == 1)
        {
          if (j > 0 && j <= n+1)
            {
              int err = 0;
              if (argr.is_single_type () || argu.is_single_type ())
                {
                  if (argr.is_real_type () && argu.is_real_type ())
                    {
                      // real case
                      FloatMatrix R = argr.float_matrix_value ();
                      FloatColumnVector u = argu.float_column_vector_value ();

                      FloatCHOL fact;
                      fact.set (R);
                      err = fact.insert_sym (u, j-1);

                      retval(0) = get_chol_r (fact);
                    }
                  else
                    {
                      // complex case
                      FloatComplexMatrix R = argr.float_complex_matrix_value ();
                      FloatComplexColumnVector u =
                        argu.float_complex_column_vector_value ();

                      FloatComplexCHOL fact;
                      fact.set (R);
                      err = fact.insert_sym (u, j-1);

                      retval(0) = get_chol_r (fact);
                    }
                }
              else
                {
                  if (argr.is_real_type () && argu.is_real_type ())
                    {
                      // real case
                      Matrix R = argr.matrix_value ();
                      ColumnVector u = argu.column_vector_value ();

                      CHOL fact;
                      fact.set (R);
                      err = fact.insert_sym (u, j-1);

                      retval(0) = get_chol_r (fact);
                    }
                  else
                    {
                      // complex case
                      ComplexMatrix R = argr.complex_matrix_value ();
                      ComplexColumnVector u =
                        argu.complex_column_vector_value ();

                      ComplexCHOL fact;
                      fact.set (R);
                      err = fact.insert_sym (u, j-1);

                      retval(0) = get_chol_r (fact);
                    }
                }

              if (nargout > 1)
                retval(1) = err;
              else if (err == 1)
                error ("cholinsert: insertion violates positiveness");
              else if (err == 2)
                error ("cholinsert: singular matrix");
              else if (err == 3)
                error ("cholinsert: diagonal element must be real");
            }
          else
            error ("cholinsert: index J out of range");
        }
      else
        error ("cholinsert: dimension mismatch between R and U");
    }
  else
    print_usage ();

  return retval;
}

/*
%!test
%! u2 = [  0.35080 ;
%!         0.63930 ;
%!         3.31057 ;
%!        -0.13825 ;
%!         0.45266 ];
%!
%! R = chol (A);
%!
%! j = 3;  p = [1:j-1, j+1:5];
%! R1 = cholinsert (R, j, u2);
%! A1 = R1'*R1;
%!
%! assert (norm (triu (R1)-R1, Inf), 0);
%! assert (norm (A1(p,p) - A, Inf) < 1e1*eps);

%!test
%! u2 = [  0.35080  + 0.04298i;
%!         0.63930  + 0.23778i;
%!         3.31057  + 0.00000i;
%!        -0.13825  + 0.19879i;
%!         0.45266  + 0.50020i];
%!
%! R = chol (Ac);
%!
%! j = 3;  p = [1:j-1, j+1:5];
%! R1 = cholinsert (R, j, u2);
%! A1 = R1'*R1;
%!
%! assert (norm (triu (R1)-R1, Inf), 0);
%! assert (norm (A1(p,p) - Ac, Inf) < 1e1*eps);

%!test
%! u2 = single ([  0.35080 ;
%!                 0.63930 ;
%!                 3.31057 ;
%!                -0.13825 ;
%!                 0.45266 ]);
%!
%! R = chol (single (A));
%!
%! j = 3;  p = [1:j-1, j+1:5];
%! R1 = cholinsert (R, j, u2);
%! A1 = R1'*R1;
%!
%! assert (norm (triu (R1)-R1, Inf), single (0));
%! assert (norm (A1(p,p) - A, Inf) < 1e1*eps ("single"));

%!test
%! u2 = single ([  0.35080  + 0.04298i;
%!                 0.63930  + 0.23778i;
%!                 3.31057  + 0.00000i;
%!                -0.13825  + 0.19879i;
%!                 0.45266  + 0.50020i]);
%!
%! R = chol (single (Ac));
%!
%! j = 3;  p = [1:j-1, j+1:5];
%! R1 = cholinsert (R, j, u2);
%! A1 = R1'*R1;
%!
%! assert (norm (triu (R1)-R1, Inf), single (0));
%! assert (norm (A1(p,p) - single (Ac), Inf) < 2e1*eps ("single"));

%!test
%! cu = chol (triu (A), "upper");
%! cl = chol (tril (A), "lower");
%! assert (cu, cl', eps);

%!test
%! cca  = chol (Ac);
%!
%! ccal  = chol (Ac, "lower");
%! ccal2 = chol (tril (Ac), "lower");
%!
%! ccau  = chol (Ac, "upper");
%! ccau2 = chol (triu (Ac), "upper");
%!
%! assert (cca'*cca,     Ac, eps);
%! assert (ccau'*ccau,   Ac, eps);
%! assert (ccau2'*ccau2, Ac, eps);
%!
%! assert (cca, ccal',  eps);
%! assert (cca, ccau,   eps);
%! assert (cca, ccal2', eps);
%! assert (cca, ccau2,  eps);

%!test
%! cca  = chol (single (Ac));
%!
%! ccal  = chol (single (Ac), "lower");
%! ccal2 = chol (tril (single (Ac)), "lower");
%!
%! ccau  = chol (single (Ac), "upper");
%! ccau2 = chol (triu (single (Ac)), "upper");
%!
%! assert (cca'*cca,     single (Ac), eps ("single"));
%! assert (ccau'*ccau,   single (Ac), eps ("single"));
%! assert (ccau2'*ccau2, single (Ac), eps ("single"));
%!
%! assert (cca, ccal',  eps ("single"));
%! assert (cca, ccau,   eps ("single"));
%! assert (cca, ccal2', eps ("single"));
%! assert (cca, ccau2,  eps ("single"));

%!test
%! a = [12,  2,  3,  4;
%!       2, 14,  5,  3;
%!       3,  5, 16,  6;
%!       4,  3,  6, 16];
%!
%! b = [0,  1,  2,  3;
%!     -1,  0,  1,  2;
%!     -2, -1,  0,  1;
%!     -3, -2, -1,  0];
%!
%! ca = a + i*b;
%!
%! cca  = chol (ca);
%!
%! ccal  = chol (ca, "lower");
%! ccal2 = chol (tril (ca), "lower");
%!
%! ccau  = chol (ca, "upper");
%! ccau2 = chol (triu (ca), "upper");
%!
%! assert (cca'*cca,     ca, 16*eps);
%! assert (ccau'*ccau,   ca, 16*eps);
%! assert (ccau2'*ccau2, ca, 16*eps);
%!
%! assert (cca, ccal',  16*eps);
%! assert (cca, ccau,   16*eps);
%! assert (cca, ccal2', 16*eps);
%! assert (cca, ccau2,  16*eps);
*/

DEFUN_DLD (choldelete, args, ,
           "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {@var{R1} =} choldelete (@var{R}, @var{j})\n\
Given a Cholesky@tie{}factorization of a real symmetric or complex Hermitian\n\
positive definite matrix @w{@var{A} = @var{R}'*@var{R}}, @var{R}@tie{}upper\n\
triangular, return the Cholesky@tie{}factorization of @w{A(p,p)}, where\n\
@w{p = [1:j-1,j+1:n+1]}.\n\
@seealso{chol, cholupdate, cholinsert, cholshift}\n\
@end deftypefn")
{
  octave_idx_type nargin = args.length ();

  octave_value_list retval;

  if (nargin != 2)
    {
      print_usage ();
      return retval;
    }

  octave_value argr = args(0);
  octave_value argj = args(1);

  if (argr.is_numeric_type () && argj.is_real_scalar ())
    {
      octave_idx_type n = argr.rows ();
      octave_idx_type j = argj.scalar_value ();

      if (argr.columns () == n)
        {
          if (j > 0 && j <= n)
            {
              if (argr.is_single_type ())
                {
                  if (argr.is_real_type ())
                    {
                      // real case
                      FloatMatrix R = argr.float_matrix_value ();

                      FloatCHOL fact;
                      fact.set (R);
                      fact.delete_sym (j-1);

                      retval(0) = get_chol_r (fact);
                    }
                  else
                    {
                      // complex case
                      FloatComplexMatrix R = argr.float_complex_matrix_value ();

                      FloatComplexCHOL fact;
                      fact.set (R);
                      fact.delete_sym (j-1);

                      retval(0) = get_chol_r (fact);
                    }
                }
              else
                {
                  if (argr.is_real_type ())
                    {
                      // real case
                      Matrix R = argr.matrix_value ();

                      CHOL fact;
                      fact.set (R);
                      fact.delete_sym (j-1);

                      retval(0) = get_chol_r (fact);
                    }
                  else
                    {
                      // complex case
                      ComplexMatrix R = argr.complex_matrix_value ();

                      ComplexCHOL fact;
                      fact.set (R);
                      fact.delete_sym (j-1);

                      retval(0) = get_chol_r (fact);
                    }
                }
            }
          else
            error ("choldelete: index J out of range");
        }
      else
        error ("choldelete: matrix R must be square");
    }
  else
    print_usage ();

  return retval;
}

/*
%!test
%! R = chol (A);
%!
%! j = 3;  p = [1:j-1,j+1:4];
%! R1 = choldelete (R, j);
%!
%! assert (norm (triu (R1)-R1, Inf), 0);
%! assert (norm (R1'*R1 - A(p,p), Inf) < 1e1*eps);

%!test
%! R = chol (Ac);
%!
%! j = 3;  p = [1:j-1,j+1:4];
%! R1 = choldelete (R, j);
%!
%! assert (norm (triu (R1)-R1, Inf), 0);
%! assert (norm (R1'*R1 - Ac(p,p), Inf) < 1e1*eps);

%!test
%! R = chol (single (A));
%!
%! j = 3;  p = [1:j-1,j+1:4];
%! R1 = choldelete (R, j);
%!
%! assert (norm (triu (R1)-R1, Inf), single (0));
%! assert (norm (R1'*R1 - single (A(p,p)), Inf) < 1e1*eps ("single"));

%!test
%! R = chol (single (Ac));
%!
%! j = 3;  p = [1:j-1,j+1:4];
%! R1 = choldelete (R,j);
%!
%! assert (norm (triu (R1)-R1, Inf), single (0));
%! assert (norm (R1'*R1 - single (Ac(p,p)), Inf) < 1e1*eps ("single"));
*/

DEFUN_DLD (cholshift, args, ,
           "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {@var{R1} =} cholshift (@var{R}, @var{i}, @var{j})\n\
Given a Cholesky@tie{}factorization of a real symmetric or complex Hermitian\n\
positive definite matrix @w{@var{A} = @var{R}'*@var{R}}, @var{R}@tie{}upper\n\
triangular, return the Cholesky@tie{}factorization of\n\
@w{@var{A}(p,p)}, where @w{p} is the permutation @*\n\
@code{p = [1:i-1, shift(i:j, 1), j+1:n]} if @w{@var{i} < @var{j}} @*\n\
 or @*\n\
@code{p = [1:j-1, shift(j:i,-1), i+1:n]} if @w{@var{j} < @var{i}}.  @*\n\
\n\
@seealso{chol, cholupdate, cholinsert, choldelete}\n\
@end deftypefn")
{
  octave_idx_type nargin = args.length ();

  octave_value_list retval;

  if (nargin != 3)
    {
      print_usage ();
      return retval;
    }

  octave_value argr = args(0);
  octave_value argi = args(1);
  octave_value argj = args(2);

  if (argr.is_numeric_type ()
      && argi.is_real_scalar () && argj.is_real_scalar ())
    {
      octave_idx_type n = argr.rows ();
      octave_idx_type i = argi.scalar_value ();
      octave_idx_type j = argj.scalar_value ();

      if (argr.columns () == n)
        {
          if (j > 0 && j <= n+1 && i > 0 && i <= n+1)
            {

              if (argr.is_single_type () && argi.is_single_type ()
                  && argj.is_single_type ())
                {
                  if (argr.is_real_type ())
                    {
                      // real case
                      FloatMatrix R = argr.float_matrix_value ();

                      FloatCHOL fact;
                      fact.set (R);
                      fact.shift_sym (i-1, j-1);

                      retval(0) = get_chol_r (fact);
                    }
                  else
                    {
                      // complex case
                      FloatComplexMatrix R = argr.float_complex_matrix_value ();

                      FloatComplexCHOL fact;
                      fact.set (R);
                      fact.shift_sym (i-1, j-1);

                      retval(0) = get_chol_r (fact);
                    }
                }
              else
                {
                  if (argr.is_real_type ())
                    {
                      // real case
                      Matrix R = argr.matrix_value ();

                      CHOL fact;
                      fact.set (R);
                      fact.shift_sym (i-1, j-1);

                      retval(0) = get_chol_r (fact);
                    }
                  else
                    {
                      // complex case
                      ComplexMatrix R = argr.complex_matrix_value ();

                      ComplexCHOL fact;
                      fact.set (R);
                      fact.shift_sym (i-1, j-1);

                      retval(0) = get_chol_r (fact);
                    }
                }
            }
          else
            error ("cholshift: index I or J is out of range");
        }
      else
        error ("cholshift: R must be a square matrix");
    }
  else
    print_usage ();

  return retval;
}

/*
%!test
%! R = chol (A);
%!
%! i = 1;  j = 3;  p = [1:i-1, shift(i:j,-1), j+1:4];
%! R1 = cholshift (R, i, j);
%!
%! assert (norm (triu (R1)-R1, Inf), 0);
%! assert (norm (R1'*R1 - A(p,p), Inf) < 1e1*eps);
%!
%! j = 1;  i = 3;  p = [1:j-1, shift(j:i,+1), i+1:4];
%! R1 = cholshift (R, i, j);
%!
%! assert (norm (triu (R1) - R1, Inf), 0);
%! assert (norm (R1'*R1 - A(p,p), Inf) < 1e1*eps);

%!test
%! R = chol (Ac);
%!
%! i = 1;  j = 3;  p = [1:i-1, shift(i:j,-1), j+1:4];
%! R1 = cholshift (R, i, j);
%!
%! assert (norm (triu (R1)-R1, Inf), 0);
%! assert (norm (R1'*R1 - Ac(p,p), Inf) < 1e1*eps);
%!
%! j = 1;  i = 3;  p = [1:j-1, shift(j:i,+1), i+1:4];
%! R1 = cholshift (R, i, j);
%!
%! assert (norm (triu (R1)-R1, Inf), 0);
%! assert (norm (R1'*R1 - Ac(p,p), Inf) < 1e1*eps);

%!test
%! R = chol (single (A));
%!
%! i = 1;  j = 3;  p = [1:i-1, shift(i:j,-1), j+1:4];
%! R1 = cholshift (R, i, j);
%!
%! assert (norm (triu (R1)-R1, Inf), 0);
%! assert (norm (R1'*R1 - single (A(p,p)), Inf) < 1e1*eps ("single"));
%!
%! j = 1;  i = 3;  p = [1:j-1, shift(j:i,+1), i+1:4];
%! R1 = cholshift (R, i, j);
%!
%! assert (norm (triu (R1)-R1, Inf), 0);
%! assert (norm (R1'*R1 - single (A(p,p)), Inf) < 1e1*eps ("single"));

%!test
%! R = chol (single (Ac));
%!
%! i = 1;  j = 3;  p = [1:i-1, shift(i:j,-1), j+1:4];
%! R1 = cholshift (R, i, j);
%!
%! assert (norm (triu (R1)-R1, Inf), 0);
%! assert (norm (R1'*R1 - single (Ac(p,p)), Inf) < 1e1*eps ("single"));
%!
%! j = 1; i = 3; p = [1:j-1, shift(j:i,+1), i+1:4];
%! R1 = cholshift (R, i, j);
%!
%! assert (norm (triu (R1)-R1, Inf), 0);
%! assert (norm (R1'*R1 - single (Ac(p,p)), Inf) < 1e1*eps ("single"));
*/
