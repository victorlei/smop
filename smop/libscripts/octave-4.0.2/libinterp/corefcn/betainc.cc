/*

Copyright (C) 1997-2015 John W. Eaton

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

#include "lo-specfun.h"

#include "defun.h"
#include "error.h"
#include "gripes.h"
#include "oct-obj.h"
#include "utils.h"


DEFUN (betainc, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} betainc (@var{x}, @var{a}, @var{b})\n\
Compute the regularized incomplete Beta function.\n\
\n\
The regularized incomplete Beta function is defined by\n\
@tex\n\
$$\n\
 I (x, a, b) = {1 \\over {B (a, b)}} \\int_0^x t^{(a-z)} (1-t)^{(b-1)} dt.\n\
$$\n\
@end tex\n\
@ifnottex\n\
@c Set example in small font to prevent overfull line\n\
\n\
@smallexample\n\
@group\n\
                                   x\n\
                          1       /\n\
betainc (x, a, b) = -----------   | t^(a-1) (1-t)^(b-1) dt.\n\
                    beta (a, b)   /\n\
                               t=0\n\
@end group\n\
@end smallexample\n\
\n\
@end ifnottex\n\
\n\
If @var{x} has more than one component, both @var{a} and @var{b} must be\n\
scalars.  If @var{x} is a scalar, @var{a} and @var{b} must be of\n\
compatible dimensions.\n\
@seealso{betaincinv, beta, betaln}\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 3)
    {
      octave_value x_arg = args(0);
      octave_value a_arg = args(1);
      octave_value b_arg = args(2);

      // FIXME: Can we make a template version of the duplicated code below
      if (x_arg.is_single_type () || a_arg.is_single_type ()
          || b_arg.is_single_type ())
        {
          if (x_arg.is_scalar_type ())
            {
              float x = x_arg.float_value ();

              if (a_arg.is_scalar_type ())
                {
                  float a = a_arg.float_value ();

                  if (! error_state)
                    {
                      if (b_arg.is_scalar_type ())
                        {
                          float b = b_arg.float_value ();

                          if (! error_state)
                            retval = betainc (x, a, b);
                        }
                      else
                        {
                          Array<float> b = b_arg.float_array_value ();

                          if (! error_state)
                            retval = betainc (x, a, b);
                        }
                    }
                }
              else
                {
                  Array<float> a = a_arg.float_array_value ();

                  if (! error_state)
                    {
                      if (b_arg.is_scalar_type ())
                        {
                          float b = b_arg.float_value ();

                          if (! error_state)
                            retval = betainc (x, a, b);
                        }
                      else
                        {
                          Array<float> b = b_arg.float_array_value ();

                          if (! error_state)
                            retval = betainc (x, a, b);
                        }
                    }
                }
            }
          else
            {
              Array<float> x = x_arg.float_array_value ();

              if (a_arg.is_scalar_type ())
                {
                  float a = a_arg.float_value ();

                  if (! error_state)
                    {
                      if (b_arg.is_scalar_type ())
                        {
                          float b = b_arg.float_value ();

                          if (! error_state)
                            retval = betainc (x, a, b);
                        }
                      else
                        {
                          Array<float> b = b_arg.float_array_value ();

                          if (! error_state)
                            retval = betainc (x, a, b);
                        }
                    }
                }
              else
                {
                  Array<float> a = a_arg.float_array_value ();

                  if (! error_state)
                    {
                      if (b_arg.is_scalar_type ())
                        {
                          float b = b_arg.float_value ();

                          if (! error_state)
                            retval = betainc (x, a, b);
                        }
                      else
                        {
                          Array<float> b = b_arg.float_array_value ();

                          if (! error_state)
                            retval = betainc (x, a, b);
                        }
                    }
                }
            }
        }
      else
        {
          if (x_arg.is_scalar_type ())
            {
              double x = x_arg.double_value ();

              if (a_arg.is_scalar_type ())
                {
                  double a = a_arg.double_value ();

                  if (! error_state)
                    {
                      if (b_arg.is_scalar_type ())
                        {
                          double b = b_arg.double_value ();

                          if (! error_state)
                            retval = betainc (x, a, b);
                        }
                      else
                        {
                          Array<double> b = b_arg.array_value ();

                          if (! error_state)
                            retval = betainc (x, a, b);
                        }
                    }
                }
              else
                {
                  Array<double> a = a_arg.array_value ();

                  if (! error_state)
                    {
                      if (b_arg.is_scalar_type ())
                        {
                          double b = b_arg.double_value ();

                          if (! error_state)
                            retval = betainc (x, a, b);
                        }
                      else
                        {
                          Array<double> b = b_arg.array_value ();

                          if (! error_state)
                            retval = betainc (x, a, b);
                        }
                    }
                }
            }
          else
            {
              Array<double> x = x_arg.array_value ();

              if (a_arg.is_scalar_type ())
                {
                  double a = a_arg.double_value ();

                  if (! error_state)
                    {
                      if (b_arg.is_scalar_type ())
                        {
                          double b = b_arg.double_value ();

                          if (! error_state)
                            retval = betainc (x, a, b);
                        }
                      else
                        {
                          Array<double> b = b_arg.array_value ();

                          if (! error_state)
                            retval = betainc (x, a, b);
                        }
                    }
                }
              else
                {
                  Array<double> a = a_arg.array_value ();

                  if (! error_state)
                    {
                      if (b_arg.is_scalar_type ())
                        {
                          double b = b_arg.double_value ();

                          if (! error_state)
                            retval = betainc (x, a, b);
                        }
                      else
                        {
                          Array<double> b = b_arg.array_value ();

                          if (! error_state)
                            retval = betainc (x, a, b);
                        }
                    }
                }
            }
        }
    }
  else
    print_usage ();

  return retval;
}

/*
## Double precision
%!test
%! a = [1, 1.5, 2, 3];
%! b = [4, 3, 2, 1];
%! v1 = betainc (1,a,b);
%! v2 = [1,1,1,1];
%! x = [.2, .4, .6, .8];
%! v3 = betainc (x, a, b);
%! v4 = 1 - betainc (1.-x, b, a);
%! assert (v1, v2, sqrt (eps));
%! assert (v3, v4, sqrt (eps));

## Single precision
%!test
%! a = single ([1, 1.5, 2, 3]);
%! b = single ([4, 3, 2, 1]);
%! v1 = betainc (1,a,b);
%! v2 = single ([1,1,1,1]);
%! x = single ([.2, .4, .6, .8]);
%! v3 = betainc (x, a, b);
%! v4 = 1 - betainc (1.-x, b, a);
%! assert (v1, v2, sqrt (eps ("single")));
%! assert (v3, v4, sqrt (eps ("single")));

## Mixed double/single precision
%!test
%! a = single ([1, 1.5, 2, 3]);
%! b = [4, 3, 2, 1];
%! v1 = betainc (1,a,b);
%! v2 = single ([1,1,1,1]);
%! x = [.2, .4, .6, .8];
%! v3 = betainc (x, a, b);
%! v4 = 1-betainc (1.-x, b, a);
%! assert (v1, v2, sqrt (eps ("single")));
%! assert (v3, v4, sqrt (eps ("single")));

%!error betainc ()
%!error betainc (1)
%!error betainc (1,2)
%!error betainc (1,2,3,4)
*/

DEFUN (betaincinv, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} betaincinv (@var{y}, @var{a}, @var{b})\n\
Compute the inverse of the incomplete Beta function.\n\
\n\
The inverse is the value @var{x} such that\n\
\n\
@example\n\
@var{y} == betainc (@var{x}, @var{a}, @var{b})\n\
@end example\n\
@seealso{betainc, beta, betaln}\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 3)
    {
      octave_value x_arg = args(0);
      octave_value a_arg = args(1);
      octave_value b_arg = args(2);

      if (x_arg.is_scalar_type ())
        {
          double x = x_arg.double_value ();

          if (a_arg.is_scalar_type ())
            {
              double a = a_arg.double_value ();

              if (! error_state)
                {
                  if (b_arg.is_scalar_type ())
                    {
                      double b = b_arg.double_value ();

                      if (! error_state)
                        retval = betaincinv (x, a, b);
                    }
                  else
                    {
                      Array<double> b = b_arg.array_value ();

                      if (! error_state)
                        retval = betaincinv (x, a, b);
                    }
                }
            }
          else
            {
              Array<double> a = a_arg.array_value ();

              if (! error_state)
                {
                  if (b_arg.is_scalar_type ())
                    {
                      double b = b_arg.double_value ();

                      if (! error_state)
                        retval = betaincinv (x, a, b);
                    }
                  else
                    {
                      Array<double> b = b_arg.array_value ();

                      if (! error_state)
                        retval = betaincinv (x, a, b);
                    }
                }
            }
        }
      else
        {
          Array<double> x = x_arg.array_value ();

          if (a_arg.is_scalar_type ())
            {
              double a = a_arg.double_value ();

              if (! error_state)
                {
                  if (b_arg.is_scalar_type ())
                    {
                      double b = b_arg.double_value ();

                      if (! error_state)
                        retval = betaincinv (x, a, b);
                    }
                  else
                    {
                      Array<double> b = b_arg.array_value ();

                      if (! error_state)
                        retval = betaincinv (x, a, b);
                    }
                }
            }
          else
            {
              Array<double> a = a_arg.array_value ();

              if (! error_state)
                {
                  if (b_arg.is_scalar_type ())
                    {
                      double b = b_arg.double_value ();

                      if (! error_state)
                        retval = betaincinv (x, a, b);
                    }
                  else
                    {
                      Array<double> b = b_arg.array_value ();

                      if (! error_state)
                        retval = betaincinv (x, a, b);
                    }
                }
            }
        }

      // FIXME: It would be better to have an algorithm for betaincinv which
      // accepted float inputs and returned float outputs.  As it is, we do
      // extra work to calculate betaincinv to double precision and then throw
      // that precision away.
      if (x_arg.is_single_type () || a_arg.is_single_type ()
          || b_arg.is_single_type ())
        {
          retval = Array<float> (retval.array_value ());
        }
    }
  else
    print_usage ();

  return retval;
}

/*
%!assert (betaincinv ([0.875 0.6875], [1 2], 3), [0.5 0.5], sqrt (eps))
%!assert (betaincinv (0.5, 3, 3), 0.5, sqrt (eps))
%!assert (betaincinv (0.34375, 4, 3), 0.5, sqrt (eps))
%!assert (betaincinv (0.2265625, 5, 3), 0.5, sqrt (eps))
%!assert (betaincinv (0.14453125, 6, 3), 0.5, sqrt (eps))
%!assert (betaincinv (0.08984375, 7, 3), 0.5, sqrt (eps))
%!assert (betaincinv (0.0546875, 8, 3), 0.5, sqrt (eps))
%!assert (betaincinv (0.03271484375, 9, 3), 0.5, sqrt (eps))
%!assert (betaincinv (0.019287109375, 10, 3), 0.5, sqrt (eps))

## Test class single as well
%!assert (betaincinv ([0.875 0.6875], [1 2], single (3)), [0.5 0.5], sqrt (eps ("single")))
%!assert (betaincinv (0.5, 3, single (3)), 0.5, sqrt (eps ("single")))
%!assert (betaincinv (0.34375, 4, single (3)), 0.5, sqrt (eps ("single")))

## Extreme values
%!assert (betaincinv (0, 42, 42), 0, sqrt (eps))
%!assert (betaincinv (1, 42, 42), 1, sqrt (eps))

%!error betaincinv ()
%!error betaincinv (1, 2)
*/

