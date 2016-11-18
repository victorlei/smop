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

DEFUN (gammainc, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Mapping Function} {} gammainc (@var{x}, @var{a})\n\
@deftypefnx {Mapping Function} {} gammainc (@var{x}, @var{a}, \"lower\")\n\
@deftypefnx {Mapping Function} {} gammainc (@var{x}, @var{a}, \"upper\")\n\
Compute the normalized incomplete gamma function.\n\
\n\
This is defined as\n\
@tex\n\
$$\n\
 \\gamma (x, a) = {1 \\over {\\Gamma (a)}}\\displaystyle{\\int_0^x t^{a-1} e^{-t} dt}\n\
$$\n\
@end tex\n\
@ifnottex\n\
\n\
@example\n\
@group\n\
                                x\n\
                       1       /\n\
gammainc (x, a) = ---------    | exp (-t) t^(a-1) dt\n\
                  gamma (a)    /\n\
                            t=0\n\
@end group\n\
@end example\n\
\n\
@end ifnottex\n\
with the limiting value of 1 as @var{x} approaches infinity.\n\
The standard notation is @math{P(a,x)}, e.g., @nospell{Abramowitz} and\n\
@nospell{Stegun} (6.5.1).\n\
\n\
If @var{a} is scalar, then @code{gammainc (@var{x}, @var{a})} is returned\n\
for each element of @var{x} and vice versa.\n\
\n\
If neither @var{x} nor @var{a} is scalar, the sizes of @var{x} and\n\
@var{a} must agree, and @code{gammainc} is applied element-by-element.\n\
\n\
By default the incomplete gamma function integrated from 0 to @var{x} is\n\
computed.  If @qcode{\"upper\"} is given then the complementary function\n\
integrated from @var{x} to infinity is calculated.  It should be noted that\n\
\n\
@example\n\
gammainc (@var{x}, @var{a}) @equiv{} 1 - gammainc (@var{x}, @var{a}, \"upper\")\n\
@end example\n\
@seealso{gamma, gammaln}\n\
@end deftypefn")
{
  octave_value retval;
  bool lower = true;

  int nargin = args.length ();

  if (nargin == 3)
    {
      if (args(2).is_string ())
        {
          std::string s = args(2).string_value ();
          std::transform (s.begin (), s.end (), s.begin (), tolower);
          if (s == "upper")
            lower = false;
          else if (s != "lower")
            error ("gammainc: third argument must be \"lower\" or \"upper\"");
        }
      else
        error ("gammainc: third argument must be \"lower\" or \"upper\"");

    }

  if (!error_state && nargin >= 2  && nargin <= 3)
    {
      octave_value x_arg = args(0);
      octave_value a_arg = args(1);

      // FIXME: Can we make a template version of the duplicated code below
      if (x_arg.is_single_type () || a_arg.is_single_type ())
        {
          if (x_arg.is_scalar_type ())
            {
              float x = x_arg.float_value ();

              if (! error_state)
                {
                  if (a_arg.is_scalar_type ())
                    {
                      float a = a_arg.float_value ();

                      if (! error_state)
                        retval = lower ? gammainc (x, a)
                                       : 1.0f - gammainc (x, a);
                    }
                  else
                    {
                      FloatNDArray a = a_arg.float_array_value ();

                      if (! error_state)
                        retval = lower ? gammainc (x, a)
                                       : 1.0f - gammainc (x, a);
                    }
                }
            }
          else
            {
              FloatNDArray x = x_arg.float_array_value ();

              if (! error_state)
                {
                  if (a_arg.is_scalar_type ())
                    {
                      float a = a_arg.float_value ();

                      if (! error_state)
                        retval = lower ? gammainc (x, a)
                                       : 1.0f - gammainc (x, a);
                    }
                  else
                    {
                      FloatNDArray a = a_arg.float_array_value ();

                      if (! error_state)
                        retval = lower ? gammainc (x, a)
                                       : 1.0f - gammainc (x, a);
                    }
                }
            }
        }
      else
        {
          if (x_arg.is_scalar_type ())
            {
              double x = x_arg.double_value ();

              if (! error_state)
                {
                  if (a_arg.is_scalar_type ())
                    {
                      double a = a_arg.double_value ();

                      if (! error_state)
                        retval = lower ? gammainc (x, a) : 1. - gammainc (x, a);
                    }
                  else
                    {
                      NDArray a = a_arg.array_value ();

                      if (! error_state)
                        retval = lower ? gammainc (x, a) : 1. - gammainc (x, a);
                    }
                }
            }
          else
            {
              NDArray x = x_arg.array_value ();

              if (! error_state)
                {
                  if (a_arg.is_scalar_type ())
                    {
                      double a = a_arg.double_value ();

                      if (! error_state)
                        retval = lower ? gammainc (x, a) : 1. - gammainc (x, a);
                    }
                  else
                    {
                      NDArray a = a_arg.array_value ();

                      if (! error_state)
                        retval = lower ? gammainc (x, a) : 1. - gammainc (x, a);
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
%!test
%! a = [.5 .5 .5 .5 .5];
%! x = [0 1 2 3 4];
%! v1 = sqrt (pi)*erf (x)./gamma (a);
%! v3 = gammainc (x.*x, a);
%! assert (v1, v3, sqrt (eps));

%!assert (gammainc (0:4,0.5, "upper"), 1-gammainc (0:4,0.5), 1e-10)

%!test
%! a = single ([.5 .5 .5 .5 .5]);
%! x = single ([0 1 2 3 4]);
%! v1 = sqrt (pi ("single"))*erf (x)./gamma (a);
%! v3 = gammainc (x.*x, a);
%! assert (v1, v3, sqrt (eps ("single")));

%!assert (gammainc (single (0:4), single (0.5), "upper"),
%!        single (1)-gammainc (single (0:4), single (0.5)),
%!        single (1e-7))
*/
