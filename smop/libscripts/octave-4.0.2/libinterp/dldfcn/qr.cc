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

#include "CmplxQR.h"
#include "CmplxQRP.h"
#include "dbleQR.h"
#include "dbleQRP.h"
#include "fCmplxQR.h"
#include "fCmplxQRP.h"
#include "floatQR.h"
#include "floatQRP.h"
#include "SparseQR.h"
#include "SparseCmplxQR.h"


#include "defun-dld.h"
#include "error.h"
#include "gripes.h"
#include "oct-obj.h"
#include "utils.h"

template <class MT>
static octave_value
get_qr_r (const base_qr<MT>& fact)
{
  MT R = fact.R ();
  if (R.is_square () && fact.regular ())
    return octave_value (R, MatrixType (MatrixType::Upper));
  else
    return R;
}

// [Q, R] = qr (X):      form Q unitary and R upper triangular such
//                        that Q * R = X
//
// [Q, R] = qr (X, 0):    form the economy decomposition such that if X is
//                        m by n then only the first n columns of Q are
//                        computed.
//
// [Q, R, P] = qr (X):    form QRP factorization of X where
//                        P is a permutation matrix such that
//                        A * P = Q * R
//
// [Q, R, P] = qr (X, 0): form the economy decomposition with
//                        permutation vector P such that Q * R = X (:, P)
//
// qr (X) alone returns the output of the LAPACK routine dgeqrf, such
// that R = triu (qr (X))

DEFUN_DLD (qr, args, nargout,
           "-*- texinfo -*-\n\
@deftypefn  {Loadable Function} {[@var{Q}, @var{R}, @var{P}] =} qr (@var{A})\n\
@deftypefnx {Loadable Function} {[@var{Q}, @var{R}, @var{P}] =} qr (@var{A}, '0')\n\
@deftypefnx {Loadable Function} {[@var{C}, @var{R}] =} qr (@var{A}, @var{B})\n\
@deftypefnx {Loadable Function} {[@var{C}, @var{R}] =} qr (@var{A}, @var{B}, '0')\n\
@cindex QR factorization\n\
Compute the QR@tie{}factorization of @var{A}, using standard @sc{lapack}\n\
subroutines.\n\
\n\
For example, given the matrix @code{@var{A} = [1, 2; 3, 4]},\n\
\n\
@example\n\
[@var{Q}, @var{R}] = qr (@var{A})\n\
@end example\n\
\n\
@noindent\n\
returns\n\
\n\
@example\n\
@group\n\
@var{Q} =\n\
\n\
  -0.31623  -0.94868\n\
  -0.94868   0.31623\n\
\n\
@var{R} =\n\
\n\
  -3.16228  -4.42719\n\
   0.00000  -0.63246\n\
@end group\n\
@end example\n\
\n\
The @code{qr} factorization has applications in the solution of least\n\
squares problems\n\
@tex\n\
$$\n\
\\min_x \\left\\Vert A x - b \\right\\Vert_2\n\
$$\n\
@end tex\n\
@ifnottex\n\
\n\
@example\n\
min norm(A x - b)\n\
@end example\n\
\n\
@end ifnottex\n\
for overdetermined systems of equations (i.e.,\n\
@tex\n\
$A$\n\
@end tex\n\
@ifnottex\n\
@var{A}\n\
@end ifnottex\n\
is a tall, thin matrix).  The QR@tie{}factorization is\n\
@tex\n\
$QR = A$ where $Q$ is an orthogonal matrix and $R$ is upper triangular.\n\
@end tex\n\
@ifnottex\n\
@code{@var{Q} * @var{R} = @var{A}} where @var{Q} is an orthogonal matrix and\n\
@var{R} is upper triangular.\n\
@end ifnottex\n\
\n\
If given a second argument of @qcode{'0'}, @code{qr} returns an economy-sized\n\
QR@tie{}factorization, omitting zero rows of @var{R} and the corresponding\n\
columns of @var{Q}.\n\
\n\
If the matrix @var{A} is full, the permuted QR@tie{}factorization\n\
@code{[@var{Q}, @var{R}, @var{P}] = qr (@var{A})} forms the\n\
QR@tie{}factorization such that the diagonal entries of @var{R} are\n\
decreasing in magnitude order.  For example, given the matrix\n\
@code{a = [1, 2; 3, 4]},\n\
\n\
@example\n\
[@var{Q}, @var{R}, @var{P}] = qr (@var{A})\n\
@end example\n\
\n\
@noindent\n\
returns\n\
\n\
@example\n\
@group\n\
@var{Q} =\n\
\n\
  -0.44721  -0.89443\n\
  -0.89443   0.44721\n\
\n\
@var{R} =\n\
\n\
  -4.47214  -3.13050\n\
   0.00000   0.44721\n\
\n\
@var{P} =\n\
\n\
   0  1\n\
   1  0\n\
@end group\n\
@end example\n\
\n\
The permuted @code{qr} factorization\n\
@code{[@var{Q}, @var{R}, @var{P}] = qr (@var{A})} factorization allows the\n\
construction of an orthogonal basis of @code{span (A)}.\n\
\n\
If the matrix @var{A} is sparse, then compute the sparse\n\
QR@tie{}factorization of @var{A}, using @sc{CSparse}.  As the matrix @var{Q}\n\
is in general a full matrix, this function returns the @var{Q}-less\n\
factorization @var{R} of @var{A}, such that\n\
@code{@var{R} = chol (@var{A}' * @var{A})}.\n\
\n\
If the final argument is the scalar @code{0} and the number of rows is\n\
larger than the number of columns, then an economy factorization is\n\
returned.  That is @var{R} will have only @code{size (@var{A},1)} rows.\n\
\n\
If an additional matrix @var{B} is supplied, then @code{qr} returns\n\
@var{C}, where @code{@var{C} = @var{Q}' * @var{B}}.  This allows the\n\
least squares approximation of @code{@var{A} \\ @var{B}} to be calculated\n\
as\n\
\n\
@example\n\
@group\n\
[@var{C}, @var{R}] = qr (@var{A}, @var{B})\n\
x = @var{R} \\ @var{C}\n\
@end group\n\
@end example\n\
@seealso{chol, hess, lu, qz, schur, svd, qrupdate, qrinsert, qrdelete, qrshift}\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin < 1 || nargin > (args(0).is_sparse_type () ? 3 : 2))
    {
      print_usage ();
      return retval;
    }

  octave_value arg = args(0);

  int arg_is_empty = empty_arg ("qr", arg.rows (), arg.columns ());

  if (arg_is_empty < 0)
    return retval;

  if (arg.is_sparse_type ())
    {
      bool economy = false;
      bool is_cmplx = false;
      int have_b = 0;

      if (arg.is_complex_type ())
        is_cmplx = true;
      if (nargin > 1)
        {
          have_b = 1;
          if (args(nargin-1).is_scalar_type ())
            {
              int val = args(nargin-1).int_value ();
              if (val == 0)
                {
                  economy = true;
                  have_b = (nargin > 2 ? 2 : 0);
                }
            }
          if (have_b > 0 && args(have_b).is_complex_type ())
            is_cmplx = true;
        }

      if (!error_state)
        {
          if (have_b && nargout < 2)
            error ("qr: incorrect number of output arguments");
          else if (is_cmplx)
            {
              SparseComplexQR q (arg.sparse_complex_matrix_value ());
              if (!error_state)
                {
                  if (have_b > 0)
                    {
                      retval(1) = q.R (economy);
                      retval(0) = q.C (args(have_b).complex_matrix_value ());
                      if (arg.rows () < arg.columns ())
                        warning ("qr: non minimum norm solution for under-determined problem");
                    }
                  else if (nargout > 1)
                    {
                      retval(1) = q.R (economy);
                      retval(0) = q.Q ();
                    }
                  else
                    retval(0) = q.R (economy);
                }
            }
          else
            {
              SparseQR q (arg.sparse_matrix_value ());
              if (!error_state)
                {
                  if (have_b > 0)
                    {
                      retval(1) = q.R (economy);
                      retval(0) = q.C (args(have_b).matrix_value ());
                      if (args(0).rows () < args(0).columns ())
                        warning ("qr: non minimum norm solution for under-determined problem");
                    }
                  else if (nargout > 1)
                    {
                      retval(1) = q.R (economy);
                      retval(0) = q.Q ();
                    }
                  else
                    retval(0) = q.R (economy);
                }
            }
        }
    }
  else
    {
      QR::type type = (nargout == 0 || nargout == 1) ? QR::raw
                                                     : nargin == 2
                                                       ? QR::economy : QR::std;

      if (arg.is_single_type ())
        {
          if (arg.is_real_type ())
            {
              FloatMatrix m = arg.float_matrix_value ();

              if (! error_state)
                {
                  switch (nargout)
                    {
                    case 0:
                    case 1:
                      {
                        FloatQR fact (m, type);
                        retval(0) = fact.R ();
                      }
                      break;

                    case 2:
                      {
                        FloatQR fact (m, type);
                        retval(1) = get_qr_r (fact);
                        retval(0) = fact.Q ();
                      }
                      break;

                    default:
                      {
                        FloatQRP fact (m, type);
                        if (type == QR::economy)
                          retval(2) = fact.Pvec ();
                        else
                          retval(2) = fact.P ();
                        retval(1) = get_qr_r (fact);
                        retval(0) = fact.Q ();
                      }
                      break;
                    }
                }
            }
          else if (arg.is_complex_type ())
            {
              FloatComplexMatrix m = arg.float_complex_matrix_value ();

              if (! error_state)
                {
                  switch (nargout)
                    {
                    case 0:
                    case 1:
                      {
                        FloatComplexQR fact (m, type);
                        retval(0) = fact.R ();
                      }
                      break;

                    case 2:
                      {
                        FloatComplexQR fact (m, type);
                        retval(1) = get_qr_r (fact);
                        retval(0) = fact.Q ();
                      }
                      break;

                    default:
                      {
                        FloatComplexQRP fact (m, type);
                        if (type == QR::economy)
                          retval(2) = fact.Pvec ();
                        else
                          retval(2) = fact.P ();
                        retval(1) = get_qr_r (fact);
                        retval(0) = fact.Q ();
                      }
                      break;
                    }
                }
            }
        }
      else
        {
          if (arg.is_real_type ())
            {
              Matrix m = arg.matrix_value ();

              if (! error_state)
                {
                  switch (nargout)
                    {
                    case 0:
                    case 1:
                      {
                        QR fact (m, type);
                        retval(0) = fact.R ();
                      }
                      break;

                    case 2:
                      {
                        QR fact (m, type);
                        retval(1) = get_qr_r (fact);
                        retval(0) = fact.Q ();
                      }
                      break;

                    default:
                      {
                        QRP fact (m, type);
                        if (type == QR::economy)
                          retval(2) = fact.Pvec ();
                        else
                          retval(2) = fact.P ();
                        retval(1) = get_qr_r (fact);
                        retval(0) = fact.Q ();
                      }
                      break;
                    }
                }
            }
          else if (arg.is_complex_type ())
            {
              ComplexMatrix m = arg.complex_matrix_value ();

              if (! error_state)
                {
                  switch (nargout)
                    {
                    case 0:
                    case 1:
                      {
                        ComplexQR fact (m, type);
                        retval(0) = fact.R ();
                      }
                      break;

                    case 2:
                      {
                        ComplexQR fact (m, type);
                        retval(1) = get_qr_r (fact);
                        retval(0) = fact.Q ();
                      }
                      break;

                    default:
                      {
                        ComplexQRP fact (m, type);
                        if (type == QR::economy)
                          retval(2) = fact.Pvec ();
                        else
                          retval(2) = fact.P ();
                        retval(1) = get_qr_r (fact);
                        retval(0) = fact.Q ();
                      }
                      break;
                    }
                }
            }
          else
            gripe_wrong_type_arg ("qr", arg);
        }
    }

  return retval;
}

/*
%!test
%! a = [0, 2, 1; 2, 1, 2];
%!
%! [q, r] = qr (a);
%! [qe, re] = qr (a, 0);
%!
%! assert (q * r, a, sqrt (eps));
%! assert (qe * re, a, sqrt (eps));

%!test
%! a = [0, 2, 1; 2, 1, 2];
%!
%! [q, r, p] = qr (a);  # FIXME: not giving right dimensions.
%! [qe, re, pe] = qr (a, 0);
%!
%! assert (q * r, a * p, sqrt (eps));
%! assert (qe * re, a(:, pe), sqrt (eps));

%!test
%! a = [0, 2; 2, 1; 1, 2];
%!
%! [q, r] = qr (a);
%! [qe, re] = qr (a, 0);
%!
%! assert (q * r, a, sqrt (eps));
%! assert (qe * re, a, sqrt (eps));

%!test
%! a = [0, 2; 2, 1; 1, 2];
%!
%! [q, r, p] = qr (a);
%! [qe, re, pe] = qr (a, 0);
%!
%! assert (q * r, a * p, sqrt (eps));
%! assert (qe * re, a(:, pe), sqrt (eps));

%!error qr ()
%!error qr ([1, 2; 3, 4], 0, 2)

%!function retval = __testqr (q, r, a, p)
%!  tol = 100*eps (class (q));
%!  retval = 0;
%!  if (nargin == 3)
%!    n1 = norm (q*r - a);
%!    n2 = norm (q'*q - eye (columns (q)));
%!    retval = (n1 < tol && n2 < tol);
%!  else
%!    n1 = norm (q'*q - eye (columns (q)));
%!    retval = (n1 < tol);
%!    if (isvector (p))
%!      n2 = norm (q*r - a(:,p));
%!      retval = (retval && n2 < tol);
%!    else
%!      n2 = norm (q*r - a*p);
%!      retval = (retval && n2 < tol);
%!    endif
%!  endif
%!endfunction

%!test
%! t = ones (24, 1);
%! j = 1;
%!
%! if (false)  # eliminate big matrix tests
%!   a = rand (5000, 20);
%!   [q,r]   = qr (a, 0);  t(j++) = __testqr (q, r, a);
%!   [q,r]   = qr (a',0);  t(j++) = __testqr (q, r, a');
%!   [q,r,p] = qr (a, 0);  t(j++) = __testqr (q, r, a, p);
%!   [q,r,p] = qr (a',0);  t(j++) = __testqr (q, r, a', p);
%!
%!   a = a+1i*eps;
%!   [q,r]   = qr (a, 0);  t(j++) = __testqr (q, r, a);
%!   [q,r]   = qr (a',0);  t(j++) = __testqr (q, r, a');
%!   [q,r,p] = qr (a, 0);  t(j++) = __testqr (q, r, a, p);
%!   [q,r,p] = qr (a',0);  t(j++) = __testqr (q, r, a', p);
%! endif
%!
%! a = [ ones(1,15); sqrt(eps)*eye(15) ];
%! [q,r]   = qr (a);   t(j++) = __testqr (q, r, a);
%! [q,r]   = qr (a');  t(j++) = __testqr (q, r, a');
%! [q,r,p] = qr (a);   t(j++) = __testqr (q, r, a, p);
%! [q,r,p] = qr (a');  t(j++) = __testqr (q, r, a', p);
%!
%! a = a+1i*eps;
%! [q,r]   = qr (a);   t(j++) = __testqr (q, r, a);
%! [q,r]   = qr (a');  t(j++) = __testqr (q, r, a');
%! [q,r,p] = qr (a);   t(j++) = __testqr (q, r, a, p);
%! [q,r,p] = qr (a');  t(j++) = __testqr (q, r, a', p);
%!
%! a = [ ones(1,15); sqrt(eps)*eye(15) ];
%! [q,r]   = qr (a, 0);  t(j++) = __testqr (q, r, a);
%! [q,r]   = qr (a',0);  t(j++) = __testqr (q, r, a');
%! [q,r,p] = qr (a, 0);  t(j++) = __testqr (q, r, a, p);
%! [q,r,p] = qr (a',0);  t(j++) = __testqr (q, r, a', p);
%!
%! a = a+1i*eps;
%! [q,r]   = qr (a, 0);  t(j++) = __testqr (q, r, a);
%! [q,r]   = qr (a',0);  t(j++) = __testqr (q, r, a');
%! [q,r,p] = qr (a, 0);  t(j++) = __testqr (q, r, a, p);
%! [q,r,p] = qr (a',0);  t(j++) = __testqr (q, r, a', p);
%!
%! a = [ 611   196  -192   407    -8   -52   -49    29
%!       196   899   113  -192   -71   -43    -8   -44
%!      -192   113   899   196    61    49     8    52
%!       407  -192   196   611     8    44    59   -23
%!        -8   -71    61     8   411  -599   208   208
%!       -52   -43    49    44  -599   411   208   208
%!       -49    -8     8    59   208   208    99  -911
%!        29   -44    52   -23   208   208  -911    99 ];
%! [q,r] = qr (a);
%!
%! assert (all (t) && norm (q*r - a) < 5000*eps);

%!test
%! a = single ([0, 2, 1; 2, 1, 2]);
%!
%! [q, r] = qr (a);
%! [qe, re] = qr (a, 0);
%!
%! assert (q * r, a, sqrt (eps ("single")));
%! assert (qe * re, a, sqrt (eps ("single")));

%!test
%! a = single ([0, 2, 1; 2, 1, 2]);
%!
%! [q, r, p] = qr (a);  # FIXME: not giving right dimensions.
%! [qe, re, pe] = qr (a, 0);
%!
%! assert (q * r, a * p, sqrt (eps ("single")));
%! assert (qe * re, a(:, pe), sqrt (eps ("single")));

%!test
%! a = single ([0, 2; 2, 1; 1, 2]);
%!
%! [q, r] = qr (a);
%! [qe, re] = qr (a, 0);
%!
%! assert (q * r, a, sqrt (eps ("single")));
%! assert (qe * re, a, sqrt (eps ("single")));

%!test
%! a = single ([0, 2; 2, 1; 1, 2]);
%!
%! [q, r, p] = qr (a);
%! [qe, re, pe] = qr (a, 0);
%!
%! assert (q * r, a * p, sqrt (eps ("single")));
%! assert (qe * re, a(:, pe), sqrt (eps ("single")));

%!error qr ()
%!error qr ([1, 2; 3, 4], 0, 2)

%!test
%! t = ones (24, 1);
%! j = 1;
%!
%! if (false)  # eliminate big matrix tests
%!   a = rand (5000,20);
%!   [q,r]   = qr (a, 0);  t(j++) = __testqr (q, r, a);
%!   [q,r]   = qr (a',0);  t(j++) = __testqr (q, r, a');
%!   [q,r,p] = qr (a, 0);  t(j++) = __testqr (q, r, a, p);
%!   [q,r,p] = qr (a',0);  t(j++) = __testqr (q, r, a', p);
%!
%!   a = a+1i*eps ("single");
%!   [q,r]   = qr (a, 0);  t(j++) = __testqr (q, r, a);
%!   [q,r]   = qr (a',0);  t(j++) = __testqr (q, r, a');
%!   [q,r,p] = qr (a, 0);  t(j++) = __testqr (q, r, a, p);
%!   [q,r,p] = qr (a',0);  t(j++) = __testqr (q, r, a', p);
%! endif
%!
%! a = [ ones(1,15); sqrt(eps("single"))*eye(15) ];
%! [q,r]   = qr (a);   t(j++) = __testqr (q, r, a);
%! [q,r]   = qr (a');  t(j++) = __testqr (q, r, a');
%! [q,r,p] = qr (a);   t(j++) = __testqr (q, r, a, p);
%! [q,r,p] = qr (a');  t(j++) = __testqr (q, r, a', p);
%!
%! a = a+1i*eps ("single");
%! [q,r]   = qr (a);   t(j++) = __testqr (q, r, a);
%! [q,r]   = qr (a');  t(j++) = __testqr (q, r, a');
%! [q,r,p] = qr (a);   t(j++) = __testqr (q, r, a, p);
%! [q,r,p] = qr (a');  t(j++) = __testqr (q, r, a', p);
%!
%! a = [ ones(1,15); sqrt(eps("single"))*eye(15) ];
%! [q,r]   = qr (a, 0);  t(j++) = __testqr (q, r, a);
%! [q,r]   = qr (a',0);  t(j++) = __testqr (q, r, a');
%! [q,r,p] = qr (a, 0);  t(j++) = __testqr (q, r, a, p);
%! [q,r,p] = qr (a',0);  t(j++) = __testqr (q, r, a', p);
%!
%! a = a+1i*eps ("single");
%! [q,r]   = qr (a, 0);  t(j++) = __testqr (q, r, a);
%! [q,r]   = qr (a',0);  t(j++) = __testqr (q, r, a');
%! [q,r,p] = qr (a, 0);  t(j++) = __testqr (q, r, a, p);
%! [q,r,p] = qr (a',0);  t(j++) = __testqr (q, r, a',p);
%!
%! a = [ 611   196  -192   407    -8   -52   -49    29
%!       196   899   113  -192   -71   -43    -8   -44
%!      -192   113   899   196    61    49     8    52
%!       407  -192   196   611     8    44    59   -23
%!        -8   -71    61     8   411  -599   208   208
%!       -52   -43    49    44  -599   411   208   208
%!       -49    -8     8    59   208   208    99  -911
%!        29   -44    52   -23   208   208  -911    99 ];
%! [q,r] = qr (a);
%!
%! assert (all (t) && norm (q*r-a) < 5000*eps ("single"));

## The deactivated tests below can't be tested till rectangular back-subs is
## implemented for sparse matrices.

%!testif HAVE_CXSPARSE
%! n = 20;  d = 0.2;
%! a = sprandn (n,n,d) + speye (n,n);
%! r = qr (a);
%! assert (r'*r, a'*a, 1e-10)

%!testif HAVE_COLAMD
%! n = 20;  d = 0.2;
%! a = sprandn (n,n,d) + speye (n,n);
%! q = symamd (a);
%! a = a(q,q);
%! r = qr (a);
%! assert (r'*r, a'*a, 1e-10)

%!testif HAVE_CXSPARSE
%! n = 20;  d = 0.2;
%! a = sprandn (n,n,d) + speye (n,n);
%! [c,r] = qr (a, ones (n,1));
%! assert (r\c, full (a)\ones (n,1), 10e-10)

%!testif HAVE_CXSPARSE
%! n = 20;  d = 0.2;
%! a = sprandn (n,n,d) + speye (n,n);
%! b = randn (n,2);
%! [c,r] = qr (a, b);
%! assert (r\c, full (a)\b, 10e-10)

%% Test under-determined systems!!
%!#testif HAVE_CXSPARSE
%! n = 20;  d = 0.2;
%! a = sprandn (n,n+1,d) + speye (n,n+1);
%! b = randn (n,2);
%! [c,r] = qr (a, b);
%! assert (r\c, full (a)\b, 10e-10)

%!testif HAVE_CXSPARSE
%! n = 20;  d = 0.2;
%! a = 1i*sprandn (n,n,d) + speye (n,n);
%! r = qr (a);
%! assert (r'*r,a'*a,1e-10)

%!testif HAVE_COLAMD
%! n = 20;  d = 0.2;
%! a = 1i*sprandn (n,n,d) + speye (n,n);
%! q = symamd (a);
%! a = a(q,q);
%! r = qr (a);
%! assert (r'*r, a'*a, 1e-10)

%!testif HAVE_CXSPARSE
%! n = 20;  d = 0.2;
%! a = 1i*sprandn (n,n,d) + speye (n,n);
%! [c,r] = qr (a, ones (n,1));
%! assert (r\c, full (a)\ones (n,1), 10e-10)

%!testif HAVE_CXSPARSE
%! n = 20;  d = 0.2;
%! a = 1i*sprandn (n,n,d) + speye (n,n);
%! b = randn (n,2);
%! [c,r] = qr (a, b);
%! assert (r\c, full (a)\b, 10e-10)

%% Test under-determined systems!!
%!#testif HAVE_CXSPARSE
%! n = 20;  d = 0.2;
%! a = 1i*sprandn (n,n+1,d) + speye (n,n+1);
%! b = randn (n,2);
%! [c,r] = qr (a, b);
%! assert (r\c, full (a)\b, 10e-10)

%!error qr (sprandn (10,10,0.2), ones (10,1))
*/

static
bool check_qr_dims (const octave_value& q, const octave_value& r,
                    bool allow_ecf = false)
{
  octave_idx_type m = q.rows ();
  octave_idx_type k = r.rows ();
  octave_idx_type n = r.columns ();
  return ((q.ndims () == 2 && r.ndims () == 2 && k == q.columns ())
          && (m == k || (allow_ecf && k == n && k < m)));
}

static
bool check_index (const octave_value& i, bool vector_allowed = false)
{
  return ((i.is_real_type () || i.is_integer_type ())
          && (i.is_scalar_type () || vector_allowed));
}

DEFUN_DLD (qrupdate, args, ,
           "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {[@var{Q1}, @var{R1}] =} qrupdate (@var{Q}, @var{R}, @var{u}, @var{v})\n\
Given a QR@tie{}factorization of a real or complex matrix\n\
@w{@var{A} = @var{Q}*@var{R}}, @var{Q}@tie{}unitary and\n\
@var{R}@tie{}upper trapezoidal, return the QR@tie{}factorization of\n\
@w{@var{A} + @var{u}*@var{v}'}, where @var{u} and @var{v} are column vectors\n\
(rank-1 update) or matrices with equal number of columns\n\
(rank-k update).  Notice that the latter case is done as a sequence of rank-1\n\
updates; thus, for k large enough, it will be both faster and more accurate\n\
to recompute the factorization from scratch.\n\
\n\
The QR@tie{}factorization supplied may be either full (Q is square) or\n\
economized (R is square).\n\
\n\
@seealso{qr, qrinsert, qrdelete, qrshift}\n\
@end deftypefn")
{
  octave_idx_type nargin = args.length ();
  octave_value_list retval;

  if (nargin != 4)
    {
      print_usage ();
      return retval;
    }

  octave_value argq = args(0);
  octave_value argr = args(1);
  octave_value argu = args(2);
  octave_value argv = args(3);

  if (argq.is_numeric_type () && argr.is_numeric_type ()
      && argu.is_numeric_type () && argv.is_numeric_type ())
    {
      if (check_qr_dims (argq, argr, true))
        {
          if (argq.is_real_type ()
              && argr.is_real_type ()
              && argu.is_real_type ()
              && argv.is_real_type ())
            {
              // all real case
              if (argq.is_single_type ()
                  || argr.is_single_type ()
                  || argu.is_single_type ()
                  || argv.is_single_type ())
                {
                  FloatMatrix Q = argq.float_matrix_value ();
                  FloatMatrix R = argr.float_matrix_value ();
                  FloatMatrix u = argu.float_matrix_value ();
                  FloatMatrix v = argv.float_matrix_value ();

                  FloatQR fact (Q, R);
                  fact.update (u, v);

                  retval(1) = get_qr_r (fact);
                  retval(0) = fact.Q ();
                }
              else
                {
                  Matrix Q = argq.matrix_value ();
                  Matrix R = argr.matrix_value ();
                  Matrix u = argu.matrix_value ();
                  Matrix v = argv.matrix_value ();

                  QR fact (Q, R);
                  fact.update (u, v);

                  retval(1) = get_qr_r (fact);
                  retval(0) = fact.Q ();
                }
            }
          else
            {
              // complex case
              if (argq.is_single_type ()
                  || argr.is_single_type ()
                  || argu.is_single_type ()
                  || argv.is_single_type ())
                {
                  FloatComplexMatrix Q = argq.float_complex_matrix_value ();
                  FloatComplexMatrix R = argr.float_complex_matrix_value ();
                  FloatComplexMatrix u = argu.float_complex_matrix_value ();
                  FloatComplexMatrix v = argv.float_complex_matrix_value ();

                  FloatComplexQR fact (Q, R);
                  fact.update (u, v);

                  retval(1) = get_qr_r (fact);
                  retval(0) = fact.Q ();
                }
              else
                {
                  ComplexMatrix Q = argq.complex_matrix_value ();
                  ComplexMatrix R = argr.complex_matrix_value ();
                  ComplexMatrix u = argu.complex_matrix_value ();
                  ComplexMatrix v = argv.complex_matrix_value ();

                  ComplexQR fact (Q, R);
                  fact.update (u, v);

                  retval(1) = get_qr_r (fact);
                  retval(0) = fact.Q ();
                }
            }
        }
      else
        error ("qrupdate: Q and R dimensions don't match");
    }
  else
    error ("qrupdate: Q, R, U, and V must be numeric");

  return retval;
}

/*
%!shared A, u, v, Ac, uc, vc
%! A = [0.091364  0.613038  0.999083;
%!      0.594638  0.425302  0.603537;
%!      0.383594  0.291238  0.085574;
%!      0.265712  0.268003  0.238409;
%!      0.669966  0.743851  0.445057 ];
%!
%! u = [0.85082;
%!      0.76426;
%!      0.42883;
%!      0.53010;
%!      0.80683 ];
%!
%! v = [0.98810;
%!      0.24295;
%!      0.43167 ];
%!
%! Ac = [0.620405 + 0.956953i  0.480013 + 0.048806i  0.402627 + 0.338171i;
%!      0.589077 + 0.658457i  0.013205 + 0.279323i  0.229284 + 0.721929i;
%!      0.092758 + 0.345687i  0.928679 + 0.241052i  0.764536 + 0.832406i;
%!      0.912098 + 0.721024i  0.049018 + 0.269452i  0.730029 + 0.796517i;
%!      0.112849 + 0.603871i  0.486352 + 0.142337i  0.355646 + 0.151496i ];
%!
%! uc = [0.20351 + 0.05401i;
%!      0.13141 + 0.43708i;
%!      0.29808 + 0.08789i;
%!      0.69821 + 0.38844i;
%!      0.74871 + 0.25821i ];
%!
%! vc = [0.85839 + 0.29468i;
%!      0.20820 + 0.93090i;
%!      0.86184 + 0.34689i ];
%!

%!test
%! [Q,R] = qr (A);
%! [Q,R] = qrupdate (Q, R, u, v);
%! assert (norm (vec (Q'*Q - eye (5)), Inf) < 1e1*eps);
%! assert (norm (vec (triu (R)-R), Inf) == 0);
%! assert (norm (vec (Q*R - A - u*v'), Inf) < norm (A)*1e1*eps);
%!
%!test
%! [Q,R] = qr (Ac);
%! [Q,R] = qrupdate (Q, R, uc, vc);
%! assert (norm (vec (Q'*Q - eye (5)), Inf) < 1e1*eps);
%! assert (norm (vec (triu (R)-R), Inf) == 0);
%! assert (norm (vec (Q*R - Ac - uc*vc'), Inf) < norm (Ac)*1e1*eps);

%!test
%! [Q,R] = qr (single (A));
%! [Q,R] = qrupdate (Q, R, single (u), single (v));
%! assert (norm (vec (Q'*Q - eye (5,"single")), Inf) < 1e1*eps ("single"));
%! assert (norm (vec (triu (R)-R), Inf) == 0);
%! assert (norm (vec (Q*R - single (A) - single (u)*single (v)'), Inf) < norm (single (A))*1e1*eps ("single"));
%!
%!test
%! [Q,R] = qr (single (Ac));
%! [Q,R] = qrupdate (Q, R, single (uc), single (vc));
%! assert (norm (vec (Q'*Q - eye (5,"single")), Inf) < 1e1*eps ("single"));
%! assert (norm (vec (triu (R)-R), Inf) == 0);
%! assert (norm (vec (Q*R - single (Ac) - single (uc)*single (vc)'), Inf) < norm (single (Ac))*1e1*eps ("single"));
*/

DEFUN_DLD (qrinsert, args, ,
           "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {[@var{Q1}, @var{R1}] =} qrinsert (@var{Q}, @var{R}, @var{j}, @var{x}, @var{orient})\n\
Given a QR@tie{}factorization of a real or complex matrix\n\
@w{@var{A} = @var{Q}*@var{R}}, @var{Q}@tie{}unitary and\n\
@var{R}@tie{}upper trapezoidal, return the QR@tie{}factorization of\n\
@w{[A(:,1:j-1) x A(:,j:n)]}, where @var{u} is a column vector to be inserted\n\
into @var{A} (if @var{orient} is @qcode{\"col\"}), or the\n\
QR@tie{}factorization of @w{[A(1:j-1,:);x;A(:,j:n)]}, where @var{x} is a row\n\
vector to be inserted into @var{A} (if @var{orient} is @qcode{\"row\"}).\n\
\n\
The default value of @var{orient} is @qcode{\"col\"}.  If @var{orient} is\n\
@qcode{\"col\"}, @var{u} may be a matrix and @var{j} an index vector\n\
resulting in the QR@tie{}factorization of a matrix @var{B} such that\n\
@w{B(:,@var{j})} gives @var{u} and @w{B(:,@var{j}) = []} gives @var{A}.\n\
Notice that the latter case is done as a sequence of k insertions;\n\
thus, for k large enough, it will be both faster and more accurate to\n\
recompute the factorization from scratch.\n\
\n\
If @var{orient} is @qcode{\"col\"}, the QR@tie{}factorization supplied may\n\
be either full (Q is square) or economized (R is square).\n\
\n\
If @var{orient} is @qcode{\"row\"}, full factorization is needed.\n\
@seealso{qr, qrupdate, qrdelete, qrshift}\n\
@end deftypefn")
{
  octave_idx_type nargin = args.length ();
  octave_value_list retval;

  if (nargin < 4 || nargin > 5)
    {
      print_usage ();
      return retval;
    }

  octave_value argq = args(0);
  octave_value argr = args(1);
  octave_value argj = args(2);
  octave_value argx = args(3);

  if (argq.is_numeric_type () && argr.is_numeric_type ()
      && argx.is_numeric_type ()
      && (nargin < 5 || args(4).is_string ()))
    {
      std::string orient = (nargin < 5) ? "col" : args(4).string_value ();

      bool col = orient == "col";

      if (col || orient == "row")
        if (check_qr_dims (argq, argr, col)
            && (col || argx.rows () == 1))
          {
            if (check_index (argj, col))
              {
                MArray<octave_idx_type> j
                  = argj.octave_idx_type_vector_value ();

                octave_idx_type one = 1;

                if (argq.is_real_type ()
                    && argr.is_real_type ()
                    && argx.is_real_type ())
                  {
                    // real case
                    if (argq.is_single_type ()
                        || argr.is_single_type ()
                        || argx.is_single_type ())
                      {
                        FloatMatrix Q = argq.float_matrix_value ();
                        FloatMatrix R = argr.float_matrix_value ();
                        FloatMatrix x = argx.float_matrix_value ();

                        FloatQR fact (Q, R);

                        if (col)
                          fact.insert_col (x, j-one);
                        else
                          fact.insert_row (x.row (0), j(0)-one);

                        retval(1) = get_qr_r (fact);
                        retval(0) = fact.Q ();

                      }
                    else
                      {
                        Matrix Q = argq.matrix_value ();
                        Matrix R = argr.matrix_value ();
                        Matrix x = argx.matrix_value ();

                        QR fact (Q, R);

                        if (col)
                          fact.insert_col (x, j-one);
                        else
                          fact.insert_row (x.row (0), j(0)-one);

                        retval(1) = get_qr_r (fact);
                        retval(0) = fact.Q ();

                      }
                  }
                else
                  {
                    // complex case
                    if (argq.is_single_type ()
                        || argr.is_single_type ()
                        || argx.is_single_type ())
                      {
                        FloatComplexMatrix Q =
                          argq.float_complex_matrix_value ();
                        FloatComplexMatrix R =
                          argr.float_complex_matrix_value ();
                        FloatComplexMatrix x =
                          argx.float_complex_matrix_value ();

                        FloatComplexQR fact (Q, R);

                        if (col)
                          fact.insert_col (x, j-one);
                        else
                          fact.insert_row (x.row (0), j(0)-one);

                        retval(1) = get_qr_r (fact);
                        retval(0) = fact.Q ();
                      }
                    else
                      {
                        ComplexMatrix Q = argq.complex_matrix_value ();
                        ComplexMatrix R = argr.complex_matrix_value ();
                        ComplexMatrix x = argx.complex_matrix_value ();

                        ComplexQR fact (Q, R);

                        if (col)
                          fact.insert_col (x, j-one);
                        else
                          fact.insert_row (x.row (0), j(0)-one);

                        retval(1) = get_qr_r (fact);
                        retval(0) = fact.Q ();
                      }
                  }

              }
            else
              error ("qrinsert: invalid index J");
          }
        else
          error ("qrinsert: dimension mismatch");

      else
        error ("qrinsert: ORIENT must be \"col\" or \"row\"");
    }
  else
    print_usage ();

  return retval;
}

/*
%!test
%! [Q,R] = qr (A);
%! [Q,R] = qrinsert (Q, R, 3, u);
%! assert (norm (vec (Q'*Q - eye (5)), Inf) < 1e1*eps);
%! assert (norm (vec (triu (R) - R), Inf) == 0);
%! assert (norm (vec (Q*R - [A(:,1:2) u A(:,3)]), Inf) < norm (A)*1e1*eps);
%!test
%! [Q,R] = qr (Ac);
%! [Q,R] = qrinsert (Q, R, 3, uc);
%! assert (norm (vec (Q'*Q - eye (5)), Inf) < 1e1*eps);
%! assert (norm (vec (triu (R) - R), Inf) == 0);
%! assert (norm (vec (Q*R - [Ac(:,1:2) uc Ac(:,3)]), Inf) < norm (Ac)*1e1*eps);
%!test
%! x = [0.85082  0.76426  0.42883 ];
%!
%! [Q,R] = qr (A);
%! [Q,R] = qrinsert (Q, R, 3, x, "row");
%! assert (norm (vec (Q'*Q - eye (6)), Inf) < 1e1*eps);
%! assert (norm (vec (triu (R) - R), Inf) == 0);
%! assert (norm (vec (Q*R - [A(1:2,:);x;A(3:5,:)]), Inf) < norm (A)*1e1*eps);
%!test
%! x = [0.20351 + 0.05401i  0.13141 + 0.43708i  0.29808 + 0.08789i ];
%!
%! [Q,R] = qr (Ac);
%! [Q,R] = qrinsert (Q, R, 3, x, "row");
%! assert (norm (vec (Q'*Q - eye (6)), Inf) < 1e1*eps);
%! assert (norm (vec (triu (R) - R), Inf) == 0);
%! assert (norm (vec (Q*R - [Ac(1:2,:);x;Ac(3:5,:)]), Inf) < norm (Ac)*1e1*eps);

%!test
%! [Q,R] = qr (single (A));
%! [Q,R] = qrinsert (Q, R, 3, single (u));
%! assert (norm (vec (Q'*Q - eye (5,"single")), Inf) < 1e1*eps ("single"));
%! assert (norm (vec (triu (R) - R), Inf) == 0);
%! assert (norm (vec (Q*R - single ([A(:,1:2) u A(:,3)])), Inf) < norm (single (A))*1e1*eps ("single"));
%!test
%! [Q,R] = qr (single (Ac));
%! [Q,R] = qrinsert (Q, R, 3, single (uc));
%! assert (norm (vec (Q'*Q - eye (5,"single")), Inf) < 1e1*eps ("single"));
%! assert (norm (vec (triu (R) - R), Inf) == 0);
%! assert (norm (vec (Q*R - single ([Ac(:,1:2) uc Ac(:,3)])), Inf) < norm (single (Ac))*1e1*eps ("single"));
%!test
%! x = single ([0.85082  0.76426  0.42883 ]);
%!
%! [Q,R] = qr (single (A));
%! [Q,R] = qrinsert (Q, R, 3, x, "row");
%! assert (norm (vec (Q'*Q - eye (6,"single")), Inf) < 1e1*eps ("single"));
%! assert (norm (vec (triu (R) - R), Inf) == 0);
%! assert (norm (vec (Q*R - single ([A(1:2,:);x;A(3:5,:)])), Inf) < norm (single (A))*1e1*eps ("single"));
%!test
%! x = single ([0.20351 + 0.05401i  0.13141 + 0.43708i  0.29808 + 0.08789i ]);
%!
%! [Q,R] = qr (single (Ac));
%! [Q,R] = qrinsert (Q, R, 3, x, "row");
%! assert (norm (vec (Q'*Q - eye (6,"single")), Inf) < 1e1*eps ("single"));
%! assert (norm (vec (triu (R) - R), Inf) == 0);
%! assert (norm (vec (Q*R - single ([Ac(1:2,:);x;Ac(3:5,:)])), Inf) < norm (single (Ac))*1e1*eps ("single"));
*/

DEFUN_DLD (qrdelete, args, ,
           "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {[@var{Q1}, @var{R1}] =} qrdelete (@var{Q}, @var{R}, @var{j}, @var{orient})\n\
Given a QR@tie{}factorization of a real or complex matrix\n\
@w{@var{A} = @var{Q}*@var{R}}, @var{Q}@tie{}unitary and\n\
@var{R}@tie{}upper trapezoidal, return the QR@tie{}factorization of\n\
@w{[A(:,1:j-1) A(:,j+1:n)]}, i.e., @var{A} with one column deleted\n\
(if @var{orient} is @qcode{\"col\"}), or the QR@tie{}factorization of\n\
@w{[A(1:j-1,:);A(j+1:n,:)]}, i.e., @var{A} with one row deleted (if\n\
@var{orient} is @qcode{\"row\"}).\n\
\n\
The default value of @var{orient} is @qcode{\"col\"}.\n\
\n\
If @var{orient} is @qcode{\"col\"}, @var{j} may be an index vector\n\
resulting in the QR@tie{}factorization of a matrix @var{B} such that\n\
@w{A(:,@var{j}) = []} gives @var{B}.  Notice that the latter case is done as\n\
a sequence of k deletions; thus, for k large enough, it will be both faster\n\
and more accurate to recompute the factorization from scratch.\n\
\n\
If @var{orient} is @qcode{\"col\"}, the QR@tie{}factorization supplied may\n\
be either full (Q is square) or economized (R is square).\n\
\n\
If @var{orient} is @qcode{\"row\"}, full factorization is needed.\n\
@seealso{qr, qrupdate, qrinsert, qrshift}\n\
@end deftypefn")
{
  octave_idx_type nargin = args.length ();
  octave_value_list retval;

  if (nargin < 3 || nargin > 4)
    {
      print_usage ();
      return retval;
    }

  octave_value argq = args(0);
  octave_value argr = args(1);
  octave_value argj = args(2);

  if (argq.is_numeric_type () && argr.is_numeric_type ()
      && (nargin < 4 || args(3).is_string ()))
    {
      std::string orient = (nargin < 4) ? "col" : args(3).string_value ();

      bool col = orient == "col";

      if (col || orient == "row")
        if (check_qr_dims (argq, argr, col))
          {
            if (check_index (argj, col))
              {
                MArray<octave_idx_type> j
                  = argj.octave_idx_type_vector_value ();

                octave_idx_type one = 1;

                if (argq.is_real_type ()
                    && argr.is_real_type ())
                  {
                    // real case
                    if (argq.is_single_type ()
                        || argr.is_single_type ())
                      {
                        FloatMatrix Q = argq.float_matrix_value ();
                        FloatMatrix R = argr.float_matrix_value ();

                        FloatQR fact (Q, R);

                        if (col)
                          fact.delete_col (j-one);
                        else
                          fact.delete_row (j(0)-one);

                        retval(1) = get_qr_r (fact);
                        retval(0) = fact.Q ();
                      }
                    else
                      {
                        Matrix Q = argq.matrix_value ();
                        Matrix R = argr.matrix_value ();

                        QR fact (Q, R);

                        if (col)
                          fact.delete_col (j-one);
                        else
                          fact.delete_row (j(0)-one);

                        retval(1) = get_qr_r (fact);
                        retval(0) = fact.Q ();
                      }
                  }
                else
                  {
                    // complex case
                    if (argq.is_single_type ()
                        || argr.is_single_type ())
                      {
                        FloatComplexMatrix Q =
                          argq.float_complex_matrix_value ();
                        FloatComplexMatrix R =
                          argr.float_complex_matrix_value ();

                        FloatComplexQR fact (Q, R);

                        if (col)
                          fact.delete_col (j-one);
                        else
                          fact.delete_row (j(0)-one);

                        retval(1) = get_qr_r (fact);
                        retval(0) = fact.Q ();
                      }
                    else
                      {
                        ComplexMatrix Q = argq.complex_matrix_value ();
                        ComplexMatrix R = argr.complex_matrix_value ();

                        ComplexQR fact (Q, R);

                        if (col)
                          fact.delete_col (j-one);
                        else
                          fact.delete_row (j(0)-one);

                        retval(1) = get_qr_r (fact);
                        retval(0) = fact.Q ();
                      }
                  }
              }
            else
              error ("qrdelete: invalid index J");
          }
        else
          error ("qrdelete: dimension mismatch");

      else
        error ("qrdelete: ORIENT must be \"col\" or \"row\"");
    }
  else
    print_usage ();

  return retval;
}

/*
%!test
%! AA = [0.091364  0.613038  0.027504  0.999083;
%!       0.594638  0.425302  0.562834  0.603537;
%!       0.383594  0.291238  0.742073  0.085574;
%!       0.265712  0.268003  0.783553  0.238409;
%!       0.669966  0.743851  0.457255  0.445057 ];
%!
%! [Q,R] = qr (AA);
%! [Q,R] = qrdelete (Q, R, 3);
%! assert (norm (vec (Q'*Q - eye (5)), Inf) < 16*eps);
%! assert (norm (vec (triu (R) - R), Inf) == 0);
%! assert (norm (vec (Q*R - [AA(:,1:2) AA(:,4)]), Inf) < norm (AA)*1e1*eps);
%!
%!test
%! AA = [0.364554 + 0.993117i  0.669818 + 0.510234i  0.426568 + 0.041337i  0.847051 + 0.233291i;
%!       0.049600 + 0.242783i  0.448946 + 0.484022i  0.141155 + 0.074420i  0.446746 + 0.392706i;
%!       0.581922 + 0.657416i  0.581460 + 0.030016i  0.219909 + 0.447288i  0.201144 + 0.069132i;
%!       0.694986 + 0.000571i  0.682327 + 0.841712i  0.807537 + 0.166086i  0.192767 + 0.358098i;
%!       0.945002 + 0.066788i  0.350492 + 0.642638i  0.579629 + 0.048102i  0.600170 + 0.636938i ] * I;
%!
%! [Q,R] = qr (AA);
%! [Q,R] = qrdelete (Q, R, 3);
%! assert (norm (vec (Q'*Q - eye (5)), Inf) < 16*eps);
%! assert (norm (vec (triu (R) - R), Inf) == 0);
%! assert (norm (vec (Q*R - [AA(:,1:2) AA(:,4)]), Inf) < norm (AA)*1e1*eps);
%!
%!test
%! AA = [0.091364  0.613038  0.027504  0.999083;
%!       0.594638  0.425302  0.562834  0.603537;
%!       0.383594  0.291238  0.742073  0.085574;
%!       0.265712  0.268003  0.783553  0.238409;
%!       0.669966  0.743851  0.457255  0.445057 ];
%!
%! [Q,R] = qr (AA);
%! [Q,R] = qrdelete (Q, R, 3, "row");
%! assert (norm (vec (Q'*Q - eye (4)), Inf) < 1e1*eps);
%! assert (norm (vec (triu (R) - R), Inf) == 0);
%! assert (norm (vec (Q*R - [AA(1:2,:);AA(4:5,:)]), Inf) < norm (AA)*1e1*eps);
%!
%!test
%! AA = [0.364554 + 0.993117i  0.669818 + 0.510234i  0.426568 + 0.041337i  0.847051 + 0.233291i;
%!       0.049600 + 0.242783i  0.448946 + 0.484022i  0.141155 + 0.074420i  0.446746 + 0.392706i;
%!       0.581922 + 0.657416i  0.581460 + 0.030016i  0.219909 + 0.447288i  0.201144 + 0.069132i;
%!       0.694986 + 0.000571i  0.682327 + 0.841712i  0.807537 + 0.166086i  0.192767 + 0.358098i;
%!       0.945002 + 0.066788i  0.350492 + 0.642638i  0.579629 + 0.048102i  0.600170 + 0.636938i ] * I;
%!
%! [Q,R] = qr (AA);
%! [Q,R] = qrdelete (Q, R, 3, "row");
%! assert (norm (vec (Q'*Q - eye (4)), Inf) < 1e1*eps);
%! assert (norm (vec (triu (R) - R), Inf) == 0);
%! assert (norm (vec (Q*R - [AA(1:2,:);AA(4:5,:)]), Inf) < norm (AA)*1e1*eps);

%!test
%! AA = single ([0.091364  0.613038  0.027504  0.999083;
%!               0.594638  0.425302  0.562834  0.603537;
%!               0.383594  0.291238  0.742073  0.085574;
%!               0.265712  0.268003  0.783553  0.238409;
%!               0.669966  0.743851  0.457255  0.445057 ]);
%!
%! [Q,R] = qr (AA);
%! [Q,R] = qrdelete (Q, R, 3);
%! assert (norm (vec (Q'*Q - eye (5,"single")), Inf) < 1e1*eps ("single"));
%! assert (norm (vec (triu (R) - R), Inf) == 0);
%! assert (norm (vec (Q*R - [AA(:,1:2) AA(:,4)]), Inf) < norm (AA)*1e1*eps ("single"));
%!
%!test
%! AA = single ([0.364554 + 0.993117i  0.669818 + 0.510234i  0.426568 + 0.041337i  0.847051 + 0.233291i;
%!               0.049600 + 0.242783i  0.448946 + 0.484022i  0.141155 + 0.074420i  0.446746 + 0.392706i;
%!               0.581922 + 0.657416i  0.581460 + 0.030016i  0.219909 + 0.447288i  0.201144 + 0.069132i;
%!               0.694986 + 0.000571i  0.682327 + 0.841712i  0.807537 + 0.166086i  0.192767 + 0.358098i;
%!               0.945002 + 0.066788i  0.350492 + 0.642638i  0.579629 + 0.048102i  0.600170 + 0.636938i ]) * I;
%!
%! [Q,R] = qr (AA);
%! [Q,R] = qrdelete (Q, R, 3);
%! assert (norm (vec (Q'*Q - eye (5,"single")), Inf) < 1e1*eps ("single"));
%! assert (norm (vec (triu (R) - R), Inf) == 0);
%! assert (norm (vec (Q*R - [AA(:,1:2) AA(:,4)]), Inf) < norm (AA)*1e1*eps ("single"));
%!
%!test
%! AA = single ([0.091364  0.613038  0.027504  0.999083;
%!               0.594638  0.425302  0.562834  0.603537;
%!               0.383594  0.291238  0.742073  0.085574;
%!               0.265712  0.268003  0.783553  0.238409;
%!               0.669966  0.743851  0.457255  0.445057 ]);
%!
%! [Q,R] = qr (AA);
%! [Q,R] = qrdelete (Q, R, 3, "row");
%! assert (norm (vec (Q'*Q - eye (4,"single")), Inf) < 1.5e1*eps ("single"));
%! assert (norm (vec (triu (R) - R), Inf) == 0);
%! assert (norm (vec (Q*R - [AA(1:2,:);AA(4:5,:)]), Inf) < norm (AA)*1e1*eps ("single"));
%!testif HAVE_QRUPDATE
%! ## Same test as above but with more precicision
%! AA = single ([0.091364  0.613038  0.027504  0.999083;
%!               0.594638  0.425302  0.562834  0.603537;
%!               0.383594  0.291238  0.742073  0.085574;
%!               0.265712  0.268003  0.783553  0.238409;
%!               0.669966  0.743851  0.457255  0.445057 ]);
%!
%! [Q,R] = qr (AA);
%! [Q,R] = qrdelete (Q, R, 3, "row");
%! assert (norm (vec (Q'*Q - eye (4,"single")), Inf) < 1e1*eps ("single"));
%! assert (norm (vec (triu (R) - R), Inf) == 0);
%! assert (norm (vec (Q*R - [AA(1:2,:);AA(4:5,:)]), Inf) < norm (AA)*1e1*eps ("single"));
%!
%!test
%! AA = single ([0.364554 + 0.993117i  0.669818 + 0.510234i  0.426568 + 0.041337i  0.847051 + 0.233291i;
%!              0.049600 + 0.242783i  0.448946 + 0.484022i  0.141155 + 0.074420i  0.446746 + 0.392706i;
%!              0.581922 + 0.657416i  0.581460 + 0.030016i  0.219909 + 0.447288i  0.201144 + 0.069132i;
%!              0.694986 + 0.000571i  0.682327 + 0.841712i  0.807537 + 0.166086i  0.192767 + 0.358098i;
%!              0.945002 + 0.066788i  0.350492 + 0.642638i  0.579629 + 0.048102i  0.600170 + 0.636938i ]) * I;
%!
%! [Q,R] = qr (AA);
%! [Q,R] = qrdelete (Q, R, 3, "row");
%! assert (norm (vec (Q'*Q - eye (4,"single")), Inf) < 1e1*eps ("single"));
%! assert (norm (vec (triu (R) - R), Inf) == 0);
%! assert (norm (vec (Q*R - [AA(1:2,:);AA(4:5,:)]), Inf) < norm (AA)*1e1*eps ("single"));
*/

DEFUN_DLD (qrshift, args, ,
           "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {[@var{Q1}, @var{R1}] =} qrshift (@var{Q}, @var{R}, @var{i}, @var{j})\n\
Given a QR@tie{}factorization of a real or complex matrix\n\
@w{@var{A} = @var{Q}*@var{R}}, @var{Q}@tie{}unitary and\n\
@var{R}@tie{}upper trapezoidal, return the QR@tie{}factorization\n\
of @w{@var{A}(:,p)}, where @w{p} is the permutation @*\n\
@code{p = [1:i-1, shift(i:j, 1), j+1:n]} if @w{@var{i} < @var{j}} @*\n\
 or @*\n\
@code{p = [1:j-1, shift(j:i,-1), i+1:n]} if @w{@var{j} < @var{i}}.  @*\n\
\n\
@seealso{qr, qrupdate, qrinsert, qrdelete}\n\
@end deftypefn")
{
  octave_idx_type nargin = args.length ();
  octave_value_list retval;

  if (nargin != 4)
    {
      print_usage ();
      return retval;
    }

  octave_value argq = args(0);
  octave_value argr = args(1);
  octave_value argi = args(2);
  octave_value argj = args(3);

  if (argq.is_numeric_type () && argr.is_numeric_type ())
    {
      if (check_qr_dims (argq, argr, true))
        {
          if (check_index (argi) && check_index (argj))
            {
              octave_idx_type i = argi.int_value ();
              octave_idx_type j = argj.int_value ();

              if (argq.is_real_type ()
                  && argr.is_real_type ())
                {
                  // all real case
                  if (argq.is_single_type ()
                      && argr.is_single_type ())
                    {
                      FloatMatrix Q = argq.float_matrix_value ();
                      FloatMatrix R = argr.float_matrix_value ();

                      FloatQR fact (Q, R);
                      fact.shift_cols (i-1, j-1);

                      retval(1) = get_qr_r (fact);
                      retval(0) = fact.Q ();
                    }
                  else
                    {
                      Matrix Q = argq.matrix_value ();
                      Matrix R = argr.matrix_value ();

                      QR fact (Q, R);
                      fact.shift_cols (i-1, j-1);

                      retval(1) = get_qr_r (fact);
                      retval(0) = fact.Q ();
                    }
                }
              else
                {
                  // complex case
                  if (argq.is_single_type ()
                      && argr.is_single_type ())
                    {
                      FloatComplexMatrix Q = argq.float_complex_matrix_value ();
                      FloatComplexMatrix R = argr.float_complex_matrix_value ();

                      FloatComplexQR fact (Q, R);
                      fact.shift_cols (i-1, j-1);

                      retval(1) = get_qr_r (fact);
                      retval(0) = fact.Q ();
                    }
                  else
                    {
                      ComplexMatrix Q = argq.complex_matrix_value ();
                      ComplexMatrix R = argr.complex_matrix_value ();

                      ComplexQR fact (Q, R);
                      fact.shift_cols (i-1, j-1);

                      retval(1) = get_qr_r (fact);
                      retval(0) = fact.Q ();
                    }
                }
            }
          else
            error ("qrshift: invalid index I or J");
        }
      else
        error ("qrshift: dimensions mismatch");
    }
  else
    error ("qrshift: Q and R must be numeric");

  return retval;
}

/*
%!test
%! AA = A.';
%! i = 2;  j = 4;  p = [1:i-1, shift(i:j,-1), j+1:5];
%!
%! [Q,R] = qr (AA);
%! [Q,R] = qrshift (Q, R, i, j);
%! assert (norm (vec (Q'*Q - eye (3)), Inf) < 1e1*eps);
%! assert (norm (vec (triu (R) - R), Inf) == 0);
%! assert (norm (vec (Q*R - AA(:,p)), Inf) < norm (AA)*1e1*eps);
%!
%! j = 2;  i = 4;  p = [1:j-1, shift(j:i,+1), i+1:5];
%!
%! [Q,R] = qr (AA);
%! [Q,R] = qrshift (Q, R, i, j);
%! assert (norm (vec (Q'*Q - eye (3)), Inf) < 1e1*eps);
%! assert (norm (vec (triu (R) - R), Inf) == 0);
%! assert (norm (vec (Q*R - AA(:,p)), Inf) < norm (AA)*1e1*eps);
%!
%!test
%! AA = Ac.';
%! i = 2;  j = 4;  p = [1:i-1, shift(i:j,-1), j+1:5];
%!
%! [Q,R] = qr (AA);
%! [Q,R] = qrshift (Q, R, i, j);
%! assert (norm (vec (Q'*Q - eye (3)), Inf) < 1e1*eps);
%! assert (norm (vec (triu (R) - R), Inf) == 0);
%! assert (norm (vec (Q*R - AA(:,p)), Inf) < norm (AA)*1e1*eps);
%!
%! j = 2;  i = 4;  p = [1:j-1, shift(j:i,+1), i+1:5];
%!
%! [Q,R] = qr (AA);
%! [Q,R] = qrshift (Q, R, i, j);
%! assert (norm (vec (Q'*Q - eye (3)), Inf) < 1e1*eps);
%! assert (norm (vec (triu (R) - R), Inf) == 0);
%! assert (norm (vec (Q*R - AA(:,p)), Inf) < norm (AA)*1e1*eps);

%!test
%! AA = single (A).';
%! i = 2;  j = 4;  p = [1:i-1, shift(i:j,-1), j+1:5];
%!
%! [Q,R] = qr (AA);
%! [Q,R] = qrshift (Q, R, i, j);
%! assert (norm (vec (Q'*Q - eye (3,"single")), Inf) < 1e1*eps ("single"));
%! assert (norm (vec (triu (R) - R), Inf) == 0);
%! assert (norm (vec (Q*R - AA(:,p)), Inf) < norm (AA)*1e1*eps ("single"));
%!
%! j = 2;  i = 4;  p = [1:j-1, shift(j:i,+1), i+1:5];
%!
%! [Q,R] = qr (AA);
%! [Q,R] = qrshift (Q, R, i, j);
%! assert (norm (vec (Q'*Q - eye (3,"single")), Inf) < 1e1*eps ("single"));
%! assert (norm (vec (triu (R) - R), Inf) == 0);
%! assert (norm (vec (Q*R - AA(:,p)), Inf) < norm (AA)*1e1*eps ("single"));
%!
%!test
%! AA = single (Ac).';
%! i = 2;  j = 4;  p = [1:i-1, shift(i:j,-1), j+1:5];
%!
%! [Q,R] = qr (AA);
%! [Q,R] = qrshift (Q, R, i, j);
%! assert (norm (vec (Q'*Q - eye (3,"single")), Inf) < 1e1*eps ("single"));
%! assert (norm (vec (triu (R) - R), Inf) == 0);
%! assert (norm (vec (Q*R - AA(:,p)), Inf) < norm (AA)*1e1*eps ("single"));
%!
%! j = 2;  i = 4;  p = [1:j-1, shift(j:i,+1), i+1:5];
%!
%! [Q,R] = qr (AA);
%! [Q,R] = qrshift (Q, R, i, j);
%! assert (norm (vec (Q'*Q - eye (3,"single")), Inf) < 1e1*eps ("single"));
%! assert (norm (vec (triu (R) - R), Inf) == 0);
%! assert (norm (vec (Q*R - AA(:,p)), Inf) < norm (AA)*1e1*eps ("single"));
*/
