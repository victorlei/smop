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
#include "ops.h"
#include "ov-re-diag.h"
#include "ov-cx-diag.h"
#include "ov-flt-re-diag.h"
#include "ov-flt-cx-diag.h"
#include "ov-perm.h"
#include "utils.h"

DEFUN (inv, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{x} =} inv (@var{A})\n\
@deftypefnx {Built-in Function} {[@var{x}, @var{rcond}] =} inv (@var{A})\n\
Compute the inverse of the square matrix @var{A}.\n\
\n\
Return an estimate of the reciprocal condition number if requested,\n\
otherwise warn of an ill-conditioned matrix if the reciprocal condition\n\
number is small.\n\
\n\
In general it is best to avoid calculating the inverse of a matrix directly.\n\
For example, it is both faster and more accurate to solve systems of\n\
equations (@var{A}*@math{x} = @math{b}) with\n\
@code{@var{y} = @var{A} \\ @math{b}}, rather than\n\
@code{@var{y} = inv (@var{A}) * @math{b}}.\n\
\n\
If called with a sparse matrix, then in general @var{x} will be a full\n\
matrix requiring significantly more storage.  Avoid forming the inverse of a\n\
sparse matrix if possible.\n\
@seealso{ldivide, rdivide}\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin != 1)
    {
      print_usage ();
      return retval;
    }

  octave_value arg = args(0);

  octave_idx_type nr = arg.rows ();
  octave_idx_type nc = arg.columns ();

  int arg_is_empty = empty_arg ("inverse", nr, nc);

  if (arg_is_empty < 0)
    return retval;
  else if (arg_is_empty > 0)
    return octave_value (Matrix ());

  if (nr != nc)
    {
      gripe_square_matrix_required ("inverse");
      return retval;
    }

  octave_value result;
  octave_idx_type info;
  double rcond = 0.0;
  float frcond = 0.0;
  bool isfloat = arg.is_single_type ();

  if (arg.is_diag_matrix ())
    {
      rcond = 1.0;
      frcond = 1.0f;
      if (arg.is_complex_type ())
        {
          if (isfloat)
            {
              result = arg.float_complex_diag_matrix_value ().inverse (info);
              if (nargout > 1)
                frcond = arg.float_complex_diag_matrix_value ().rcond ();
            }
          else
            {
              result = arg.complex_diag_matrix_value ().inverse (info);
              if (nargout > 1)
                rcond = arg.complex_diag_matrix_value ().rcond ();
            }
        }
      else
        {
          if (isfloat)
            {
              result = arg.float_diag_matrix_value ().inverse (info);
              if (nargout > 1)
                frcond = arg.float_diag_matrix_value ().rcond ();
            }
          else
            {
              result = arg.diag_matrix_value ().inverse (info);
              if (nargout > 1)
                rcond = arg.diag_matrix_value ().rcond ();
            }
        }
    }
  else if (arg.is_perm_matrix ())
    {
      rcond = 1.0;
      info = 0;
      result = arg.perm_matrix_value ().inverse ();
    }
  else if (isfloat)
    {
      if (arg.is_real_type ())
        {
          FloatMatrix m = arg.float_matrix_value ();
          if (! error_state)
            {
              MatrixType mattyp = args(0).matrix_type ();
              result = m.inverse (mattyp, info, frcond, 1);
              args(0).matrix_type (mattyp);
            }
        }
      else if (arg.is_complex_type ())
        {
          FloatComplexMatrix m = arg.float_complex_matrix_value ();
          if (! error_state)
            {
              MatrixType mattyp = args(0).matrix_type ();
              result = m.inverse (mattyp, info, frcond, 1);
              args(0).matrix_type (mattyp);
            }
        }
    }
  else
    {
      if (arg.is_real_type ())
        {
          if (arg.is_sparse_type ())
            {
              SparseMatrix m = arg.sparse_matrix_value ();
              if (! error_state)
                {
                  MatrixType mattyp = args(0).matrix_type ();
                  result = m.inverse (mattyp, info, rcond, 1);
                  args(0).matrix_type (mattyp);
                }
            }
          else
            {
              Matrix m = arg.matrix_value ();
              if (! error_state)
                {
                  MatrixType mattyp = args(0).matrix_type ();
                  result = m.inverse (mattyp, info, rcond, 1);
                  args(0).matrix_type (mattyp);
                }
            }
        }
      else if (arg.is_complex_type ())
        {
          if (arg.is_sparse_type ())
            {
              SparseComplexMatrix m = arg.sparse_complex_matrix_value ();
              if (! error_state)
                {
                  MatrixType mattyp = args(0).matrix_type ();
                  result = m.inverse (mattyp, info, rcond, 1);
                  args(0).matrix_type (mattyp);
                }
            }
          else
            {
              ComplexMatrix m = arg.complex_matrix_value ();
              if (! error_state)
                {
                  MatrixType mattyp = args(0).matrix_type ();
                  result = m.inverse (mattyp, info, rcond, 1);
                  args(0).matrix_type (mattyp);
                }
            }
        }
      else
        gripe_wrong_type_arg ("inv", arg);
    }

  if (! error_state)
    {
      if (nargout > 1)
        retval(1) = isfloat ? octave_value (frcond) : octave_value (rcond);

      retval(0) = result;

      bool rcond_plus_one_eq_one = false;

      if (isfloat)
        {
          volatile float xrcond = frcond;
          rcond_plus_one_eq_one = xrcond + 1.0F == 1.0F;
        }
      else
        {
          volatile double xrcond = rcond;
          rcond_plus_one_eq_one = xrcond + 1.0 == 1.0;
        }

      if (nargout < 2 && (info == -1 || rcond_plus_one_eq_one))
        gripe_singular_matrix (isfloat ? frcond : rcond);
    }

  return retval;
}

/*
%!assert (inv ([1, 2; 3, 4]), [-2, 1; 1.5, -0.5], sqrt (eps))
%!assert (inv (single ([1, 2; 3, 4])), single ([-2, 1; 1.5, -0.5]), sqrt (eps ("single")))

%!error inv ()
%!error inv ([1, 2; 3, 4], 2)
%!error <argument must be a square matrix> inv ([1, 2; 3, 4; 5, 6])

%!test
%! [xinv, rcond] = inv (single ([1,2;3,4]));
%! assert (isa (xinv, 'single'));
%! assert (isa (rcond, 'single'));

%!test
%! [xinv, rcond] = inv ([1,2;3,4]);
%! assert (isa (xinv, 'double'));
%! assert (isa (rcond, 'double'));
*/

// FIXME: this should really be done with an alias, but
// alias_builtin() won't do the right thing if we are actually using
// dynamic linking.

DEFUN (inverse, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{x} =} inverse (@var{A})\n\
@deftypefnx {Built-in Function} {[@var{x}, @var{rcond}] =} inverse (@var{A})\n\
Compute the inverse of the square matrix @var{A}.\n\
\n\
This is an alias for @code{inv}.\n\
@seealso{inv}\n\
@end deftypefn")
{
  return Finv (args, nargout);
}
