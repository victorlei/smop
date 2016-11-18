/*

Copyright (C) 2009-2015 Carlo de Falco
Copyright (C) 2010 VZLU Prague

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

#include "oct-norm.h"
#include "defun.h"
#include "error.h"
#include "gripes.h"

template <class ColumnVector, class Matrix, class RowVector>
static void
do_mgorth (ColumnVector& x, const Matrix& V, RowVector& h)
{
  octave_idx_type Vc = V.columns ();
  h = RowVector (Vc + 1);
  for (octave_idx_type j = 0; j < Vc; j++)
    {
      ColumnVector Vcj = V.column (j);
      h(j) = RowVector (Vcj.hermitian ()) * x;
      x -= h(j) * Vcj;
    }

  h(Vc) = xnorm (x);
  if (real (h(Vc)) > 0)
    x = x / h(Vc);
}

DEFUN (mgorth, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {[@var{y}, @var{h}] =} mgorth (@var{x}, @var{v})\n\
Orthogonalize a given column vector @var{x} with respect to a set of\n\
orthonormal vectors comprising the columns of @var{v} using the modified\n\
Gram-Schmidt method.\n\
\n\
On exit, @var{y} is a unit vector such that:\n\
\n\
@example\n\
@group\n\
  norm (@var{y}) = 1\n\
  @var{v}' * @var{y} = 0\n\
  @var{x} = [@var{v}, @var{y}]*@var{h}'\n\
@end group\n\
@end example\n\
\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin != 2 || nargout > 2)
    {
      print_usage ();
      return retval;
    }

  octave_value arg_x = args(0);
  octave_value arg_v = args(1);

  if (arg_v.ndims () != 2 || arg_x.ndims () != 2 || arg_x.columns () != 1
      || arg_v.rows () != arg_x.rows ())
    {
      error ("mgorth: V should be a matrix, and X a column vector with"
             " the same number of rows as V.");
      return retval;
    }

  if (! arg_x.is_numeric_type () && ! arg_v.is_numeric_type ())
    {
      error ("mgorth: X and V must be numeric");
    }

  bool iscomplex = (arg_x.is_complex_type () || arg_v.is_complex_type ());
  if (arg_x.is_single_type () || arg_v.is_single_type ())
    {
      if (iscomplex)
        {
          FloatComplexColumnVector x
            = arg_x.float_complex_column_vector_value ();
          FloatComplexMatrix V = arg_v.float_complex_matrix_value ();
          FloatComplexRowVector h;
          do_mgorth (x, V, h);
          retval(1) = h;
          retval(0) = x;
        }
      else
        {
          FloatColumnVector x = arg_x.float_column_vector_value ();
          FloatMatrix V = arg_v.float_matrix_value ();
          FloatRowVector h;
          do_mgorth (x, V, h);
          retval(1) = h;
          retval(0) = x;
        }
    }
  else
    {
      if (iscomplex)
        {
          ComplexColumnVector x = arg_x.complex_column_vector_value ();
          ComplexMatrix V = arg_v.complex_matrix_value ();
          ComplexRowVector h;
          do_mgorth (x, V, h);
          retval(1) = h;
          retval(0) = x;
        }
      else
        {
          ColumnVector x = arg_x.column_vector_value ();
          Matrix V = arg_v.matrix_value ();
          RowVector h;
          do_mgorth (x, V, h);
          retval(1) = h;
          retval(0) = x;
        }
    }

  return retval;
}

/*
%!test
%! for ii=1:100
%!   assert (abs (mgorth (randn (5, 1), eye (5, 4))), [0 0 0 0 1]', eps);
%! endfor

%!test
%! a = hilb (5);
%! a(:, 1) /= norm (a(:, 1));
%! for ii = 1:5
%!   a(:, ii) = mgorth (a(:, ii), a(:, 1:ii-1));
%! endfor
%! assert (a' * a, eye (5), 1e10);
*/
