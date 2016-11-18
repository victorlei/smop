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

#include "defun.h"
#include "error.h"
#include "gripes.h"
#include "oct-obj.h"
#include "utils.h"
#include "ops.h"
#include "ov-re-diag.h"
#include "ov-cx-diag.h"
#include "ov-flt-re-diag.h"
#include "ov-flt-cx-diag.h"
#include "ov-perm.h"

DEFUN (pinv, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} pinv (@var{x})\n\
@deftypefnx {Built-in Function} {} pinv (@var{x}, @var{tol})\n\
Return the pseudoinverse of @var{x}.\n\
\n\
Singular values less than @var{tol} are ignored.\n\
\n\
If the second argument is omitted, it is taken to be\n\
\n\
@example\n\
tol = max (size (@var{x})) * sigma_max (@var{x}) * eps,\n\
@end example\n\
\n\
@noindent\n\
where @code{sigma_max (@var{x})} is the maximal singular value of @var{x}.\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin < 1 || nargin > 2)
    {
      print_usage ();
      return retval;
    }

  octave_value arg = args(0);

  int arg_is_empty = empty_arg ("pinv", arg.rows (), arg.columns ());

  if (arg_is_empty < 0)
    return retval;
  else if (arg_is_empty > 0)
    return octave_value (Matrix ());

  bool isfloat = arg.is_single_type ();

  if (arg.is_diag_matrix ())
    {
      if (isfloat)
        {
          float tol = 0.0;
          if (nargin == 2)
            tol = args(1).float_value ();

          if (error_state)
            return retval;

          if (tol < 0.0)
            {
              error ("pinv: TOL must be greater than zero");
              return retval;
            }

          if (arg.is_real_type ())
            retval = arg.float_diag_matrix_value ().pseudo_inverse (tol);
          else
            retval = arg.float_complex_diag_matrix_value ().pseudo_inverse (tol);
        }
      else
        {
          double tol = 0.0;
          if (nargin == 2)
            tol = args(1).double_value ();

          if (error_state)
            return retval;

          if (tol < 0.0)
            {
              error ("pinv: TOL must be greater than zero");
              return retval;
            }

          if (arg.is_real_type ())
            retval = arg.diag_matrix_value ().pseudo_inverse (tol);
          else
            retval = arg.complex_diag_matrix_value ().pseudo_inverse (tol);
        }
    }
  else if (arg.is_perm_matrix ())
    {
      retval = arg.perm_matrix_value ().inverse ();
    }
  else if (isfloat)
    {
      float tol = 0.0;
      if (nargin == 2)
        tol = args(1).float_value ();

      if (error_state)
        return retval;

      if (tol < 0.0)
        {
          error ("pinv: TOL must be greater than zero");
          return retval;
        }

      if (arg.is_real_type ())
        {
          FloatMatrix m = arg.float_matrix_value ();

          if (! error_state)
            retval = m.pseudo_inverse (tol);
        }
      else if (arg.is_complex_type ())
        {
          FloatComplexMatrix m = arg.float_complex_matrix_value ();

          if (! error_state)
            retval = m.pseudo_inverse (tol);
        }
      else
        {
          gripe_wrong_type_arg ("pinv", arg);
        }
    }
  else
    {
      double tol = 0.0;
      if (nargin == 2)
        tol = args(1).double_value ();

      if (error_state)
        return retval;

      if (tol < 0.0)
        {
          error ("pinv: TOL must be greater than zero");
          return retval;
        }

      if (arg.is_real_type ())
        {
          Matrix m = arg.matrix_value ();

          if (! error_state)
            retval = m.pseudo_inverse (tol);
        }
      else if (arg.is_complex_type ())
        {
          ComplexMatrix m = arg.complex_matrix_value ();

          if (! error_state)
            retval = m.pseudo_inverse (tol);
        }
      else
        {
          gripe_wrong_type_arg ("pinv", arg);
        }
    }

  return retval;
}

/*
%!shared a, b, tol, hitol, d, u, x, y
%! a = reshape (rand*[1:16], 4, 4);  # Rank 2 matrix
%! b = pinv (a);
%! tol = 4e-14;
%! hitol = 40*sqrt (eps);
%! d = diag ([rand, rand, hitol, hitol]);
%! u = rand (4);                     # Could be singular by freak accident
%! x = inv (u)*d*u;
%! y = pinv (x, sqrt (eps));
%!
%!assert (a*b*a, a, tol)
%!assert (b*a*b, b, tol)
%!assert ((b*a)', b*a, tol)
%!assert ((a*b)', a*b, tol)
%!assert (x*y*x, x, -hitol)
%!assert (y*x*y, y, -hitol)
%!assert ((x*y)', x*y, hitol)
%!assert ((y*x)', y*x, hitol)

## Clear shared variables
%!shared

## Test pinv for Diagonal matrices
%!test
%! x = diag ([3 2 1 0 -0.5]);
%! y = pinv (x);
%! assert (typeinfo (y)(1:8), "diagonal");
%! assert (isa (y, "double"));
%! assert (diag (y), [1/3, 1/2, 1, 0  1/-0.5]');
%! y = pinv (x, 1);
%! assert (diag (y), [1/3 1/2 1 0 0]');
%! y = pinv (x, 2);
%! assert (diag (y), [1/3 1/2 0 0 0]');

*/
