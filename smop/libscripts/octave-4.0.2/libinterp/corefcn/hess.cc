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

#include "CmplxHESS.h"
#include "dbleHESS.h"
#include "fCmplxHESS.h"
#include "floatHESS.h"

#include "defun.h"
#include "error.h"
#include "gripes.h"
#include "oct-obj.h"
#include "utils.h"

DEFUN (hess, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{H} =} hess (@var{A})\n\
@deftypefnx {Built-in Function} {[@var{P}, @var{H}] =} hess (@var{A})\n\
@cindex Hessenberg decomposition\n\
Compute the Hessenberg decomposition of the matrix @var{A}.\n\
\n\
The Hessenberg decomposition is\n\
@tex\n\
$$\n\
A = PHP^T\n\
$$\n\
where $P$ is a square unitary matrix ($P^TP = I$), and $H$\n\
is upper Hessenberg ($H_{i,j} = 0, \\forall i > j+1$).\n\
@end tex\n\
@ifnottex\n\
@code{@var{P} * @var{H} * @var{P}' = @var{A}} where @var{P} is a square\n\
unitary matrix (@code{@var{P}' * @var{P} = I}, using complex-conjugate\n\
transposition) and @var{H} is upper Hessenberg\n\
(@code{@var{H}(i, j) = 0 forall i > j+1)}.\n\
@end ifnottex\n\
\n\
The Hessenberg decomposition is usually used as the first step in an\n\
eigenvalue computation, but has other applications as well\n\
(see @nospell{Golub, Nash, and Van Loan},\n\
IEEE Transactions on Automatic Control, 1979).\n\
@seealso{eig, chol, lu, qr, qz, schur, svd}\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin != 1 || nargout > 2)
    {
      print_usage ();
      return retval;
    }

  octave_value arg = args(0);

  octave_idx_type nr = arg.rows ();
  octave_idx_type nc = arg.columns ();

  int arg_is_empty = empty_arg ("hess", nr, nc);

  if (arg_is_empty < 0)
    return retval;
  else if (arg_is_empty > 0)
    return octave_value_list (2, Matrix ());

  if (nr != nc)
    {
      gripe_square_matrix_required ("hess");
      return retval;
    }

  if (arg.is_single_type ())
    {
      if (arg.is_real_type ())
        {
          FloatMatrix tmp = arg.float_matrix_value ();

          if (! error_state)
            {
              FloatHESS result (tmp);

              if (nargout <= 1)
                retval(0) = result.hess_matrix ();
              else
                {
                  retval(1) = result.hess_matrix ();
                  retval(0) = result.unitary_hess_matrix ();
                }
            }
        }
      else if (arg.is_complex_type ())
        {
          FloatComplexMatrix ctmp = arg.float_complex_matrix_value ();

          if (! error_state)
            {
              FloatComplexHESS result (ctmp);

              if (nargout <= 1)
                retval(0) = result.hess_matrix ();
              else
                {
                  retval(1) = result.hess_matrix ();
                  retval(0) = result.unitary_hess_matrix ();
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
              HESS result (tmp);

              if (nargout <= 1)
                retval(0) = result.hess_matrix ();
              else
                {
                  retval(1) = result.hess_matrix ();
                  retval(0) = result.unitary_hess_matrix ();
                }
            }
        }
      else if (arg.is_complex_type ())
        {
          ComplexMatrix ctmp = arg.complex_matrix_value ();

          if (! error_state)
            {
              ComplexHESS result (ctmp);

              if (nargout <= 1)
                retval(0) = result.hess_matrix ();
              else
                {
                  retval(1) = result.hess_matrix ();
                  retval(0) = result.unitary_hess_matrix ();
                }
            }
        }
      else
        {
          gripe_wrong_type_arg ("hess", arg);
        }
    }

  return retval;
}

/*
%!test
%! a = [1, 2, 3; 5, 4, 6; 8, 7, 9];
%! [p, h] = hess (a);
%! assert (p * h * p', a, sqrt (eps));

%!test
%! a = single ([1, 2, 3; 5, 4, 6; 8, 7, 9]);
%! [p, h] = hess (a);
%! assert (p * h * p', a, sqrt (eps ("single")));

%!error hess ()
%!error hess ([1, 2; 3, 4], 2)
%!error <argument must be a square matrix> hess ([1, 2; 3, 4; 5, 6])
*/
