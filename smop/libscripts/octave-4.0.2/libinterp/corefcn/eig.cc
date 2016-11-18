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

#include "EIG.h"
#include "fEIG.h"

#include "defun.h"
#include "error.h"
#include "gripes.h"
#include "oct-obj.h"
#include "utils.h"

DEFUN (eig, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{lambda} =} eig (@var{A})\n\
@deftypefnx {Built-in Function} {@var{lambda} =} eig (@var{A}, @var{B})\n\
@deftypefnx {Built-in Function} {[@var{V}, @var{lambda}] =} eig (@var{A})\n\
@deftypefnx {Built-in Function} {[@var{V}, @var{lambda}] =} eig (@var{A}, @var{B})\n\
Compute the eigenvalues (and optionally the eigenvectors) of a matrix\n\
or a pair of matrices\n\
\n\
The algorithm used depends on whether there are one or two input\n\
matrices, if they are real or complex, and if they are symmetric\n\
(Hermitian if complex) or non-symmetric.\n\
\n\
The eigenvalues returned by @code{eig} are not ordered.\n\
@seealso{eigs, svd}\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin > 2 || nargin == 0 || nargout > 2)
    {
      print_usage ();
      return retval;
    }

  octave_value arg_a, arg_b;

  octave_idx_type nr_a, nr_b, nc_a, nc_b;
  nr_a = nr_b = nc_a = nc_b = 0;

  arg_a = args(0);
  nr_a = arg_a.rows ();
  nc_a = arg_a.columns ();

  int arg_is_empty = empty_arg ("eig", nr_a, nc_a);
  if (arg_is_empty < 0)
    return retval;
  else if (arg_is_empty > 0)
    return octave_value_list (2, Matrix ());

  if (!(arg_a.is_single_type () || arg_a.is_double_type ()))
    {
      gripe_wrong_type_arg ("eig", arg_a);
      return retval;
    }

  if (nargin == 2)
    {
      arg_b = args(1);
      nr_b = arg_b.rows ();
      nc_b = arg_b.columns ();

      arg_is_empty = empty_arg ("eig", nr_b, nc_b);
      if (arg_is_empty < 0)
        return retval;
      else if (arg_is_empty > 0)
        return octave_value_list (2, Matrix ());

      if (!(arg_b.is_single_type () || arg_b.is_double_type ()))
        {
          gripe_wrong_type_arg ("eig", arg_b);
          return retval;
        }
    }

  if (nr_a != nc_a)
    {
      gripe_square_matrix_required ("eig");
      return retval;
    }

  if (nargin == 2 && nr_b != nc_b)
    {
      gripe_square_matrix_required ("eig");
      return retval;
    }

  Matrix tmp_a, tmp_b;
  ComplexMatrix ctmp_a, ctmp_b;
  FloatMatrix ftmp_a, ftmp_b;
  FloatComplexMatrix fctmp_a, fctmp_b;

  if (arg_a.is_single_type ())
    {
      FloatEIG result;

      if (nargin == 1)
        {
          if (arg_a.is_real_type ())
            {
              ftmp_a = arg_a.float_matrix_value ();

              if (error_state)
                return retval;
              else
                result = FloatEIG (ftmp_a, nargout > 1);
            }
          else
            {
              fctmp_a = arg_a.float_complex_matrix_value ();

              if (error_state)
                return retval;
              else
                result = FloatEIG (fctmp_a, nargout > 1);
            }
        }
      else if (nargin == 2)
        {
          if (arg_a.is_real_type () && arg_b.is_real_type ())
            {
              ftmp_a = arg_a.float_matrix_value ();
              ftmp_b = arg_b.float_matrix_value ();

              if (error_state)
                return retval;
              else
                result = FloatEIG (ftmp_a, ftmp_b, nargout > 1);
            }
          else
            {
              fctmp_a = arg_a.float_complex_matrix_value ();
              fctmp_b = arg_b.float_complex_matrix_value ();

              if (error_state)
                return retval;
              else
                result = FloatEIG (fctmp_a, fctmp_b, nargout > 1);
            }
        }

      if (! error_state)
        {
          if (nargout == 0 || nargout == 1)
            {
              retval(0) = result.eigenvalues ();
            }
          else
            {
              // Blame it on Matlab.

              FloatComplexDiagMatrix d (result.eigenvalues ());

              retval(1) = d;
              retval(0) = result.eigenvectors ();
            }
        }
    }
  else
    {
      EIG result;

      if (nargin == 1)
        {
          if (arg_a.is_real_type ())
            {
              tmp_a = arg_a.matrix_value ();

              if (error_state)
                return retval;
              else
                result = EIG (tmp_a, nargout > 1);
            }
          else
            {
              ctmp_a = arg_a.complex_matrix_value ();

              if (error_state)
                return retval;
              else
                result = EIG (ctmp_a, nargout > 1);
            }
        }
      else if (nargin == 2)
        {
          if (arg_a.is_real_type () && arg_b.is_real_type ())
            {
              tmp_a = arg_a.matrix_value ();
              tmp_b = arg_b.matrix_value ();

              if (error_state)
                return retval;
              else
                result = EIG (tmp_a, tmp_b, nargout > 1);
            }
          else
            {
              ctmp_a = arg_a.complex_matrix_value ();
              ctmp_b = arg_b.complex_matrix_value ();

              if (error_state)
                return retval;
              else
                result = EIG (ctmp_a, ctmp_b, nargout > 1);
            }
        }

      if (! error_state)
        {
          if (nargout == 0 || nargout == 1)
            {
              retval(0) = result.eigenvalues ();
            }
          else
            {
              // Blame it on Matlab.

              ComplexDiagMatrix d (result.eigenvalues ());

              retval(1) = d;
              retval(0) = result.eigenvectors ();
            }
        }
    }

  return retval;
}

/*
%!assert (eig ([1, 2; 2, 1]), [-1; 3], sqrt (eps))

%!test
%! [v, d] = eig ([1, 2; 2, 1]);
%! x = 1 / sqrt (2);
%! assert (d, [-1, 0; 0, 3], sqrt (eps));
%! assert (v, [-x, x; x, x], sqrt (eps));

%!assert (eig (single ([1, 2; 2, 1])), single ([-1; 3]), sqrt (eps ("single")))

%!test
%! [v, d] = eig (single ([1, 2; 2, 1]));
%! x = single (1 / sqrt (2));
%! assert (d, single ([-1, 0; 0, 3]), sqrt (eps ("single")));
%! assert (v, [-x, x; x, x], sqrt (eps ("single")));

%!test
%! A = [1, 2; -1, 1];  B = [3, 3; 1, 2];
%! [v, d] = eig (A, B);
%! assert (A * v(:, 1), d(1, 1) * B * v(:, 1), sqrt (eps));
%! assert (A * v(:, 2), d(2, 2) * B * v(:, 2), sqrt (eps));

%!test
%! A = single ([1, 2; -1, 1]);  B = single ([3, 3; 1, 2]);
%! [v, d] = eig (A, B);
%! assert (A * v(:, 1), d(1, 1) * B * v(:, 1), sqrt (eps ("single")));
%! assert (A * v(:, 2), d(2, 2) * B * v(:, 2), sqrt (eps ("single")));

%!test
%! A = [1, 2; 2, 1];  B = [3, -2; -2, 3];
%! [v, d] = eig (A, B);
%! assert (A * v(:, 1), d(1, 1) * B * v(:, 1), sqrt (eps));
%! assert (A * v(:, 2), d(2, 2) * B * v(:, 2), sqrt (eps));

%!test
%! A = single ([1, 2; 2, 1]);  B = single ([3, -2; -2, 3]);
%! [v, d] = eig (A, B);
%! assert (A * v(:, 1), d(1, 1) * B * v(:, 1), sqrt (eps ("single")));
%! assert (A * v(:, 2), d(2, 2) * B * v(:, 2), sqrt (eps ("single")));

%!test
%! A = [1+3i, 2+i; 2-i, 1+3i];  B = [5+9i, 2+i; 2-i, 5+9i];
%! [v, d] = eig (A, B);
%! assert (A * v(:, 1), d(1, 1) * B * v(:, 1), sqrt (eps));
%! assert (A * v(:, 2), d(2, 2) * B * v(:, 2), sqrt (eps));

%!test
%! A = single ([1+3i, 2+i; 2-i, 1+3i]);  B = single ([5+9i, 2+i; 2-i, 5+9i]);
%! [v, d] = eig (A, B);
%! assert (A * v(:, 1), d(1, 1) * B * v(:, 1), sqrt (eps ("single")));
%! assert (A * v(:, 2), d(2, 2) * B * v(:, 2), sqrt (eps ("single")));

%!test
%! A = [1+3i, 2+3i; 3-8i, 8+3i];  B = [8+i, 3+i; 4-9i, 3+i];
%! [v, d] = eig (A, B);
%! assert (A * v(:, 1), d(1, 1) * B * v(:, 1), sqrt (eps));
%! assert (A * v(:, 2), d(2, 2) * B * v(:, 2), sqrt (eps));

%!test
%! A = single ([1+3i, 2+3i; 3-8i, 8+3i]);  B = single ([8+i, 3+i; 4-9i, 3+i]);
%! [v, d] = eig (A, B);
%! assert (A * v(:, 1), d(1, 1) * B * v(:, 1), sqrt (eps ("single")));
%! assert (A * v(:, 2), d(2, 2) * B * v(:, 2), sqrt (eps ("single")));

%!test
%! A = [1, 2; 3, 8];  B = [8, 3; 4, 3];
%! [v, d] = eig (A, B);
%! assert (A * v(:, 1), d(1, 1) * B * v(:, 1), sqrt (eps));
%! assert (A * v(:, 2), d(2, 2) * B * v(:, 2), sqrt (eps));

%!error eig ()
%!error eig ([1, 2; 3, 4], [4, 3; 2, 1], 1)
%!error <EIG requires same size matrices> eig ([1, 2; 3, 4], 2)
%!error <argument must be a square matrix> eig ([1, 2; 3, 4; 5, 6])
%!error <wrong type argument> eig ("abcd")
%!error <wrong type argument> eig ([1 2 ; 2 3], "abcd")
%!error <wrong type argument> eig (false, [1 2 ; 2 3])
*/
