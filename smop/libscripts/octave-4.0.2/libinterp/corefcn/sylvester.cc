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

// Author: A. S. Hodel <scotte@eng.auburn.edu>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "defun.h"
#include "error.h"
#include "gripes.h"
#include "oct-obj.h"
#include "utils.h"

DEFUN (sylvester, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {@var{X} =} syl (@var{A}, @var{B}, @var{C})\n\
Solve the Sylvester equation\n\
@tex\n\
$$\n\
 A X + X B = C\n\
$$\n\
@end tex\n\
@ifnottex\n\
\n\
@example\n\
A X + X B = C\n\
@end example\n\
\n\
@end ifnottex\n\
using standard @sc{lapack} subroutines.\n\
\n\
For example:\n\
\n\
@example\n\
@group\n\
sylvester ([1, 2; 3, 4], [5, 6; 7, 8], [9, 10; 11, 12])\n\
   @result{} [ 0.50000, 0.66667; 0.66667, 0.50000 ]\n\
@end group\n\
@end example\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin != 3 || nargout > 1)
    {
      print_usage ();
      return retval;
    }

  octave_value arg_a = args(0);
  octave_value arg_b = args(1);
  octave_value arg_c = args(2);

  octave_idx_type a_nr = arg_a.rows ();
  octave_idx_type a_nc = arg_a.columns ();

  octave_idx_type b_nr = arg_b.rows ();
  octave_idx_type b_nc = arg_b.columns ();

  octave_idx_type c_nr = arg_c.rows ();
  octave_idx_type c_nc = arg_c.columns ();

  int arg_a_is_empty = empty_arg ("sylvester", a_nr, a_nc);
  int arg_b_is_empty = empty_arg ("sylvester", b_nr, b_nc);
  int arg_c_is_empty = empty_arg ("sylvester", c_nr, c_nc);

  bool isfloat = arg_a.is_single_type ()
                 || arg_b.is_single_type ()
                 || arg_c.is_single_type ();

  if (arg_a_is_empty > 0 && arg_b_is_empty > 0 && arg_c_is_empty > 0)
    if (isfloat)
      return octave_value (FloatMatrix ());
    else
      return octave_value (Matrix ());
  else if (arg_a_is_empty || arg_b_is_empty || arg_c_is_empty)
    return retval;

  // Arguments are not empty, so check for correct dimensions.

  if (a_nr != a_nc)
    {
      gripe_square_matrix_required ("sylvester: input A");
      return retval;
    }
  else if (b_nr != b_nc)
    {
      gripe_square_matrix_required ("sylvester: input B");
      return retval;
    }
  else if (a_nr != c_nr || b_nr != c_nc)
    {
      gripe_nonconformant ();
      return retval;
    }

  if (isfloat)
    {
      if (arg_a.is_complex_type ()
          || arg_b.is_complex_type ()
          || arg_c.is_complex_type ())
        {
          // Do everything in complex arithmetic;

          FloatComplexMatrix ca = arg_a.float_complex_matrix_value ();

          if (error_state)
            return retval;

          FloatComplexMatrix cb = arg_b.float_complex_matrix_value ();

          if (error_state)
            return retval;

          FloatComplexMatrix cc = arg_c.float_complex_matrix_value ();

          if (error_state)
            return retval;

          retval = Sylvester (ca, cb, cc);
        }
      else
        {
          // Do everything in real arithmetic.

          FloatMatrix ca = arg_a.float_matrix_value ();

          if (error_state)
            return retval;

          FloatMatrix cb = arg_b.float_matrix_value ();

          if (error_state)
            return retval;

          FloatMatrix cc = arg_c.float_matrix_value ();

          if (error_state)
            return retval;

          retval = Sylvester (ca, cb, cc);
        }
    }
  else
    {
      if (arg_a.is_complex_type ()
          || arg_b.is_complex_type ()
          || arg_c.is_complex_type ())
        {
          // Do everything in complex arithmetic;

          ComplexMatrix ca = arg_a.complex_matrix_value ();

          if (error_state)
            return retval;

          ComplexMatrix cb = arg_b.complex_matrix_value ();

          if (error_state)
            return retval;

          ComplexMatrix cc = arg_c.complex_matrix_value ();

          if (error_state)
            return retval;

          retval = Sylvester (ca, cb, cc);
        }
      else
        {
          // Do everything in real arithmetic.

          Matrix ca = arg_a.matrix_value ();

          if (error_state)
            return retval;

          Matrix cb = arg_b.matrix_value ();

          if (error_state)
            return retval;

          Matrix cc = arg_c.matrix_value ();

          if (error_state)
            return retval;

          retval = Sylvester (ca, cb, cc);
        }
    }

  return retval;
}

/*
%!assert (sylvester ([1, 2; 3, 4], [5, 6; 7, 8], [9, 10; 11, 12]), [1/2, 2/3; 2/3, 1/2], sqrt (eps))
%!assert (sylvester (single ([1, 2; 3, 4]), single ([5, 6; 7, 8]), single ([9, 10; 11, 12])), single ([1/2, 2/3; 2/3, 1/2]), sqrt (eps ("single")))

%% Test input validation
%!error sylvester ()
%!error sylvester (1)
%!error sylvester (1,2)
%!error sylvester (1, 2, 3, 4)
%!error <input A: .* must be a square matrix> sylvester (ones (2,3), ones (2,2), ones (2,2))
%!error <input B: .* must be a square matrix> sylvester (ones (2,2), ones (2,3), ones (2,2))
%!error <nonconformant matrices> sylvester (ones (2,2), ones (2,2), ones (3,3))
*/
