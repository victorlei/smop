/*

Copyright (C) 1996-2015 John W. Eaton

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

#include "CmplxSVD.h"
#include "dbleSVD.h"
#include "fCmplxSVD.h"
#include "floatSVD.h"

#include "defun.h"
#include "error.h"
#include "gripes.h"
#include "oct-obj.h"
#include "pr-output.h"
#include "utils.h"
#include "variables.h"

static int Vsvd_driver = SVD::GESVD;

DEFUN (svd, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{s} =} svd (@var{A})\n\
@deftypefnx {Built-in Function} {[@var{U}, @var{S}, @var{V}] =} svd (@var{A})\n\
@deftypefnx {Built-in Function} {[@var{U}, @var{S}, @var{V}] =} svd (@var{A}, @var{econ})\n\
@cindex singular value decomposition\n\
Compute the singular value decomposition of @var{A}\n\
@tex\n\
$$\n\
 A = U S V^{\\dagger}\n\
$$\n\
@end tex\n\
@ifnottex\n\
\n\
@example\n\
A = U*S*V'\n\
@end example\n\
\n\
@end ifnottex\n\
\n\
The function @code{svd} normally returns only the vector of singular values.\n\
When called with three return values, it computes\n\
@tex\n\
$U$, $S$, and $V$.\n\
@end tex\n\
@ifnottex\n\
@var{U}, @var{S}, and @var{V}.\n\
@end ifnottex\n\
For example,\n\
\n\
@example\n\
svd (hilb (3))\n\
@end example\n\
\n\
@noindent\n\
returns\n\
\n\
@example\n\
@group\n\
ans =\n\
\n\
  1.4083189\n\
  0.1223271\n\
  0.0026873\n\
@end group\n\
@end example\n\
\n\
@noindent\n\
and\n\
\n\
@example\n\
[u, s, v] = svd (hilb (3))\n\
@end example\n\
\n\
@noindent\n\
returns\n\
\n\
@example\n\
@group\n\
u =\n\
\n\
  -0.82704   0.54745   0.12766\n\
  -0.45986  -0.52829  -0.71375\n\
  -0.32330  -0.64901   0.68867\n\
\n\
s =\n\
\n\
  1.40832  0.00000  0.00000\n\
  0.00000  0.12233  0.00000\n\
  0.00000  0.00000  0.00269\n\
\n\
v =\n\
\n\
  -0.82704   0.54745   0.12766\n\
  -0.45986  -0.52829  -0.71375\n\
  -0.32330  -0.64901   0.68867\n\
@end group\n\
@end example\n\
\n\
If given a second argument, @code{svd} returns an economy-sized\n\
decomposition, eliminating the unnecessary rows or columns of @var{U} or\n\
@var{V}.\n\
@seealso{svd_driver, svds, eig, lu, chol, hess, qr, qz}\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin < 1 || nargin > 2 || nargout == 2 || nargout > 3)
    {
      print_usage ();
      return retval;
    }

  octave_value arg = args(0);

  octave_idx_type nr = arg.rows ();
  octave_idx_type nc = arg.columns ();

  if (arg.ndims () != 2)
    {
      error ("svd: A must be a 2-D matrix");
      return retval;
    }

  bool isfloat = arg.is_single_type ();

  SVD::type type = ((nargout == 0 || nargout == 1)
                    ? SVD::sigma_only
                    : (nargin == 2) ? SVD::economy : SVD::std);

  SVD::driver driver = static_cast<SVD::driver> (Vsvd_driver);

  if (nr == 0 || nc == 0)
    {
      if (isfloat)
        {
          switch (type)
            {
            case SVD::std:
              retval(2) = FloatDiagMatrix (nc, nc, 1.0f);
              retval(1) = FloatMatrix (nr, nc);
              retval(0) = FloatDiagMatrix (nr, nr, 1.0f);
              break;
            case SVD::economy:
              retval(2) = FloatDiagMatrix (0, nc, 1.0f);
              retval(1) = FloatMatrix (0, 0);
              retval(0) = FloatDiagMatrix (nr, 0, 1.0f);
              break;
          case SVD::sigma_only: default:
              retval(0) = FloatMatrix (0, 1);
              break;
            }
        }
      else
        {
          switch (type)
            {
            case SVD::std:
              retval(2) = DiagMatrix (nc, nc, 1.0);
              retval(1) = Matrix (nr, nc);
              retval(0) = DiagMatrix (nr, nr, 1.0);
              break;
            case SVD::economy:
              retval(2) = DiagMatrix (0, nc, 1.0);
              retval(1) = Matrix (0, 0);
              retval(0) = DiagMatrix (nr, 0, 1.0);
              break;
          case SVD::sigma_only: default:
              retval(0) = Matrix (0, 1);
              break;
            }
        }
    }
  else
    {
      if (isfloat)
        {
          if (arg.is_real_type ())
            {
              FloatMatrix tmp = arg.float_matrix_value ();

              if (! error_state)
                {
                  if (tmp.any_element_is_inf_or_nan ())
                    {
                      error ("svd: cannot take SVD of matrix containing Inf or NaN values");
                      return retval;
                    }

                  FloatSVD result (tmp, type, driver);

                  FloatDiagMatrix sigma = result.singular_values ();

                  if (nargout == 0 || nargout == 1)
                    {
                      retval(0) = sigma.extract_diag ();
                    }
                  else
                    {
                      retval(2) = result.right_singular_matrix ();
                      retval(1) = sigma;
                      retval(0) = result.left_singular_matrix ();
                    }
                }
            }
          else if (arg.is_complex_type ())
            {
              FloatComplexMatrix ctmp = arg.float_complex_matrix_value ();

              if (! error_state)
                {
                  if (ctmp.any_element_is_inf_or_nan ())
                    {
                      error ("svd: cannot take SVD of matrix containing Inf or NaN values");
                      return retval;
                    }

                  FloatComplexSVD result (ctmp, type, driver);

                  FloatDiagMatrix sigma = result.singular_values ();

                  if (nargout == 0 || nargout == 1)
                    {
                      retval(0) = sigma.extract_diag ();
                    }
                  else
                    {
                      retval(2) = result.right_singular_matrix ();
                      retval(1) = sigma;
                      retval(0) = result.left_singular_matrix ();
                    }
                }
            }
        }
      else
        {
          if (arg.is_real_type ())
            {
              Matrix tmp = arg.matrix_value ();

              if (! error_state)
                {
                  if (tmp.any_element_is_inf_or_nan ())
                    {
                      error ("svd: cannot take SVD of matrix containing Inf or NaN values");
                      return retval;
                    }

                  SVD result (tmp, type, driver);

                  DiagMatrix sigma = result.singular_values ();

                  if (nargout == 0 || nargout == 1)
                    {
                      retval(0) = sigma.extract_diag ();
                    }
                  else
                    {
                      retval(2) = result.right_singular_matrix ();
                      retval(1) = sigma;
                      retval(0) = result.left_singular_matrix ();
                    }
                }
            }
          else if (arg.is_complex_type ())
            {
              ComplexMatrix ctmp = arg.complex_matrix_value ();

              if (! error_state)
                {
                  if (ctmp.any_element_is_inf_or_nan ())
                    {
                      error ("svd: cannot take SVD of matrix containing Inf or NaN values");
                      return retval;
                    }

                  ComplexSVD result (ctmp, type, driver);

                  DiagMatrix sigma = result.singular_values ();

                  if (nargout == 0 || nargout == 1)
                    {
                      retval(0) = sigma.extract_diag ();
                    }
                  else
                    {
                      retval(2) = result.right_singular_matrix ();
                      retval(1) = sigma;
                      retval(0) = result.left_singular_matrix ();
                    }
                }
            }
          else
            {
              gripe_wrong_type_arg ("svd", arg);
              return retval;
            }
        }
    }

  return retval;
}

/*
%!assert (svd ([1, 2; 2, 1]), [3; 1], sqrt (eps))

%!test
%! [u, s, v] = svd ([1, 2; 2, 1]);
%! x = 1 / sqrt (2);
%! assert (u, [-x, -x; -x, x], sqrt (eps));
%! assert (s, [3, 0; 0, 1], sqrt (eps));
%! assert (v, [-x, x; -x, -x], sqrt (eps));

%!test
%! a = [1, 2, 3; 4, 5, 6];
%! [u, s, v] = svd (a);
%! assert (u * s * v', a, sqrt (eps));

%!test
%! a = [1, 2; 3, 4; 5, 6];
%! [u, s, v] = svd (a);
%! assert (u * s * v', a, sqrt (eps));

%!test
%! a = [1, 2, 3; 4, 5, 6];
%! [u, s, v] = svd (a, 1);
%! assert (u * s * v', a, sqrt (eps));

%!test
%! a = [1, 2; 3, 4; 5, 6];
%! [u, s, v] = svd (a, 1);
%! assert (u * s * v', a, sqrt (eps));

%!assert (svd (single ([1, 2; 2, 1])), single ([3; 1]), sqrt (eps ("single")))

%!test
%! [u, s, v] = svd (single ([1, 2; 2, 1]));
%! x = single (1 / sqrt (2));
%! assert (u, [-x, -x; -x, x], sqrt (eps ("single")));
%! assert (s, single ([3, 0; 0, 1]), sqrt (eps ("single")));
%! assert (v, [-x, x; -x, -x], sqrt (eps ("single")));

%!test
%! a = single ([1, 2, 3; 4, 5, 6]);
%! [u, s, v] = svd (a);
%! assert (u * s * v', a, sqrt (eps ("single")));

%!test
%! a = single ([1, 2; 3, 4; 5, 6]);
%! [u, s, v] = svd (a);
%! assert (u * s * v', a, sqrt (eps ("single")));

%!test
%! a = single ([1, 2, 3; 4, 5, 6]);
%! [u, s, v] = svd (a, 1);
%! assert (u * s * v', a, sqrt (eps ("single")));

%!test
%! a = single ([1, 2; 3, 4; 5, 6]);
%! [u, s, v] = svd (a, 1);
%! assert (u * s * v', a, sqrt (eps ("single")));

%!test
%! a = zeros (0, 5);
%! [u, s, v] = svd (a);
%! assert (size (u), [0, 0]);
%! assert (size (s), [0, 5]);
%! assert (size (v), [5, 5]);

%!test
%! a = zeros (5, 0);
%! [u, s, v] = svd (a, 1);
%! assert (size (u), [5, 0]);
%! assert (size (s), [0, 0]);
%! assert (size (v), [0, 0]);

%!error svd ()
%!error svd ([1, 2; 4, 5], 2, 3)
%!error [u, v] = svd ([1, 2; 3, 4])
*/

DEFUN (svd_driver, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{val} =} svd_driver ()\n\
@deftypefnx {Built-in Function} {@var{old_val} =} svd_driver (@var{new_val})\n\
@deftypefnx {Built-in Function} {} svd_driver (@var{new_val}, \"local\")\n\
Query or set the underlying @sc{lapack} driver used by @code{svd}.\n\
\n\
Currently recognized values are @qcode{\"gesvd\"} and @qcode{\"gesdd\"}.\n\
The default is @qcode{\"gesvd\"}.\n\
\n\
When called from inside a function with the @qcode{\"local\"} option, the\n\
variable is changed locally for the function and any subroutines it calls.\n\
The original variable value is restored when exiting the function.\n\
@seealso{svd}\n\
@end deftypefn")
{
  static const char *driver_names[] = { "gesvd", "gesdd", 0 };

  return SET_INTERNAL_VARIABLE_CHOICES (svd_driver, driver_names);
}

/*
%!test
%! A = [1+1i, 1-1i, 0; 0, 2, 0; 1i, 1i, 1+2i];
%! old_driver = svd_driver ("gesvd");
%! [U1, S1, V1] = svd (A);
%! svd_driver ("gesdd");
%! [U2, S2, V2] = svd (A);
%! assert (U1, U2, 5*eps);
%! assert (S1, S2, 5*eps);
%! assert (V1, V2, 5*eps);
%! svd_driver (old_driver);
*/
