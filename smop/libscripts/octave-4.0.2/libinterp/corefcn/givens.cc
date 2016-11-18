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

// Originally written by A. S. Hodel <scotte@eng.auburn.edu>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "defun.h"
#include "error.h"
#include "oct-obj.h"

DEFUN (givens, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{G} =} givens (@var{x}, @var{y})\n\
@deftypefnx {Built-in Function} {[@var{c}, @var{s}] =} givens (@var{x}, @var{y})\n\
Compute the Givens rotation matrix @var{G}.\n\
\n\
@tex\n\
The Givens matrix is a $2\\times 2$ orthogonal matrix\n\
$$\n\
 G = \\left[\\matrix{c & s\\cr -s'& c\\cr}\\right]\n\
$$\n\
such that\n\
$$\n\
 G \\left[\\matrix{x\\cr y}\\right] = \\left[\\matrix{\\ast\\cr 0}\\right]\n\
$$\n\
with $x$ and $y$ scalars.\n\
@end tex\n\
@ifnottex\n\
The Givens matrix is a 2 by 2 orthogonal matrix\n\
\n\
@code{@var{g} = [@var{c} @var{s}; -@var{s}' @var{c}]}\n\
\n\
such that\n\
\n\
@code{@var{g} [@var{x}; @var{y}] = [*; 0]}\n\
\n\
with @var{x} and @var{y} scalars.\n\
@end ifnottex\n\
\n\
If two output arguments are requested, return the factors @var{c} and\n\
@var{s} rather than the Givens rotation matrix.\n\
\n\
For example:\n\
\n\
@example\n\
@group\n\
givens (1, 1)\n\
   @result{}   0.70711   0.70711\n\
       -0.70711   0.70711\n\
@end group\n\
@end example\n\
@seealso{planerot}\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin != 2 || nargout > 2)
    {
      print_usage ();
      return retval;
    }
  else
    {
      if (args(0).is_single_type () || args(1).is_single_type ())
        {
          if (args(0).is_complex_type () || args(1).is_complex_type ())
            {
              FloatComplex cx = args(0).float_complex_value ();
              FloatComplex cy = args(1).float_complex_value ();

              if (! error_state)
                {
                  FloatComplexMatrix result = Givens (cx, cy);

                  if (! error_state)
                    {
                      switch (nargout)
                        {
                        case 0:
                        case 1:
                          retval(0) = result;
                          break;

                        case 2:
                          retval(1) = result (0, 1);
                          retval(0) = result (0, 0);
                          break;
                        }
                    }
                }
            }
          else
            {
              float x = args(0).float_value ();
              float y = args(1).float_value ();

              if (! error_state)
                {
                  FloatMatrix result = Givens (x, y);

                  if (! error_state)
                    {
                      switch (nargout)
                        {
                        case 0:
                        case 1:
                          retval(0) = result;
                          break;

                        case 2:
                          retval(1) = result (0, 1);
                          retval(0) = result (0, 0);
                          break;
                        }
                    }
                }
            }
        }
      else
        {
          if (args(0).is_complex_type () || args(1).is_complex_type ())
            {
              Complex cx = args(0).complex_value ();
              Complex cy = args(1).complex_value ();

              if (! error_state)
                {
                  ComplexMatrix result = Givens (cx, cy);

                  if (! error_state)
                    {
                      switch (nargout)
                        {
                        case 0:
                        case 1:
                          retval(0) = result;
                          break;

                        case 2:
                          retval(1) = result (0, 1);
                          retval(0) = result (0, 0);
                          break;
                        }
                    }
                }
            }
          else
            {
              double x = args(0).double_value ();
              double y = args(1).double_value ();

              if (! error_state)
                {
                  Matrix result = Givens (x, y);

                  if (! error_state)
                    {
                      switch (nargout)
                        {
                        case 0:
                        case 1:
                          retval(0) = result;
                          break;

                        case 2:
                          retval(1) = result (0, 1);
                          retval(0) = result (0, 0);
                          break;
                        }
                    }
                }
            }
        }
    }

  return retval;
}

/*
%!assert (givens (1,1), [1, 1; -1, 1] / sqrt (2), 2*eps)
%!assert (givens (1,0), eye (2))
%!assert (givens (0,1), [0, 1; -1 0])

%!error givens ()
%!error givens (1)
%!error [a,b,c] = givens (1, 1)
*/
