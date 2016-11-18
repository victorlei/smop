/*

Copyright (C) 1993-2015 John W. Eaton
Copyright (C) 2009-2010 VZLU Prague

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

#include <cctype>
#include <cfloat>

#include "lo-ieee.h"
#include "lo-specfun.h"
#include "lo-mappers.h"

#include "defun.h"
#include "error.h"
#include "variables.h"

DEFUN (abs, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} abs (@var{z})\n\
Compute the magnitude of @var{z}.\n\
\n\
The magnitude is defined as\n\
@tex\n\
$|z| = \\sqrt{x^2 + y^2}$.\n\
@end tex\n\
@ifnottex\n\
|@var{z}| = @code{sqrt (x^2 + y^2)}.\n\
@end ifnottex\n\
\n\
For example:\n\
\n\
@example\n\
@group\n\
abs (3 + 4i)\n\
     @result{} 5\n\
@end group\n\
@end example\n\
@seealso{arg}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).abs ();
  else
    print_usage ();

  return retval;
}

/*
%!assert (abs (1), 1)
%!assert (abs (-3.5), 3.5)
%!assert (abs (3+4i), 5)
%!assert (abs (3-4i), 5)
%!assert (abs ([1.1, 3i; 3+4i, -3-4i]), [1.1, 3; 5, 5])

%!assert (abs (single (1)), single (1))
%!assert (abs (single (-3.5)), single (3.5))
%!assert (abs (single (3+4i)), single (5))
%!assert (abs (single (3-4i)), single (5))
%!assert (abs (single ([1.1, 3i; 3+4i, -3-4i])), single ([1.1, 3; 5, 5]))

%!error abs ()
%!error abs (1, 2)
*/

DEFUN (acos, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} acos (@var{x})\n\
Compute the inverse cosine in radians for each element of @var{x}.\n\
@seealso{cos, acosd}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).acos ();
  else
    print_usage ();

  return retval;
}

/*
%!shared rt2, rt3
%! rt2 = sqrt (2);
%! rt3 = sqrt (3);

%!test
%! x = [1, rt3/2, rt2/2, 1/2, 0, -1/2, -rt2/2, -rt3/2, -1];
%! v = [0, pi/6, pi/4, pi/3, pi/2, 2*pi/3, 3*pi/4, 5*pi/6, pi];
%! assert (acos (x), v, sqrt (eps));

%!test
%! x = single ([1, rt3/2, rt2/2, 1/2, 0, -1/2, -rt2/2, -rt3/2, -1]);
%! v = single ([0, pi/6, pi/4, pi/3, pi/2, 2*pi/3, 3*pi/4, 5*pi/6, pi]);
%! assert (acos (x), v, sqrt (eps ("single")));

## Test values on either side of branch cut
%!test
%! rval = 0;
%! ival = 1.31695789692481635;
%! obs = acos ([2, 2-i*eps, 2+i*eps]);
%! exp = [rval + ival*i, rval + ival*i, rval - ival*i];
%! assert (obs, exp, 2*eps);
%! rval = pi;
%! obs = acos ([-2, -2-i*eps, -2+i*eps]);
%! exp = [rval - ival*i, rval + ival*i, rval - ival*i];
%! assert (obs, exp, 2*eps);
%! assert (acos ([2 0]),  [ival*i, pi/2], 2*eps);
%! assert (acos ([2 0i]), [ival*i, pi/2], 2*eps);

%!error acos ()
%!error acos (1, 2)
*/

DEFUN (acosh, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} acosh (@var{x})\n\
Compute the inverse hyperbolic cosine for each element of @var{x}.\n\
@seealso{cosh}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).acosh ();
  else
    print_usage ();

  return retval;
}

/*
%!test
%! x = [1, 0, -1, 0];
%! v = [0, pi/2*i, pi*i, pi/2*i];
%! assert (acosh (x), v, sqrt (eps));

%!test
%! re = 2.99822295029797;
%! im = pi/2;
%! assert (acosh (-10i), re - i*im);

%!test
%! x = single ([1, 0, -1, 0]);
%! v = single ([0, pi/2*i, pi*i, pi/2*i]);
%! assert (acosh (x), v, sqrt (eps ("single")));

%!test
%! re = single (2.99822295029797);
%! im = single (pi/2);
%! assert (acosh (single (10i)), re + i*im, 5*eps ("single"));
%! assert (acosh (single (-10i)), re - i*im, 5*eps ("single"));

%!error acosh ()
%!error acosh (1, 2)
*/

DEFUN (angle, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} angle (@var{z})\n\
See @code{arg}.\n\
@seealso{arg}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).arg ();
  else
    print_usage ();

  return retval;
}

DEFUN (arg, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Mapping Function} {} arg (@var{z})\n\
@deftypefnx {Mapping Function} {} angle (@var{z})\n\
Compute the argument, i.e., angle of @var{z}.\n\
\n\
This is defined as,\n\
@tex\n\
$\\theta = atan2 (y, x),$\n\
@end tex\n\
@ifnottex\n\
@var{theta} = @code{atan2 (@var{y}, @var{x})},\n\
@end ifnottex\n\
in radians.\n\
\n\
For example:\n\
\n\
@example\n\
@group\n\
arg (3 + 4i)\n\
     @result{} 0.92730\n\
@end group\n\
@end example\n\
@seealso{abs}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).arg ();
  else
    print_usage ();

  return retval;
}

/*
%!assert (arg (1), 0)
%!assert (arg (i), pi/2)
%!assert (arg (-1), pi)
%!assert (arg (-i), -pi/2)
%!assert (arg ([1, i; -1, -i]), [0, pi/2; pi, -pi/2])

%!assert (arg (single (1)), single (0))
%!assert (arg (single (i)), single (pi/2))
%!test
%! if (ismac ())
%!   ## Avoid failing for a MacOS feature
%!   assert (arg (single (-1)), single (pi), 2*eps (single (1)));
%! else
%!   assert (arg (single (-1)), single (pi));
%! endif
%!assert (arg (single (-i)), single (-pi/2))
%!assert (arg (single ([1, i; -1, -i])), single ([0, pi/2; pi, -pi/2]), 2e1*eps ("single"))

%!error arg ()
%!error arg (1, 2)
*/

DEFUN (asin, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} asin (@var{x})\n\
Compute the inverse sine in radians for each element of @var{x}.\n\
@seealso{sin, asind}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).asin ();
  else
    print_usage ();

  return retval;
}

/*
%!shared rt2, rt3
%! rt2 = sqrt (2);
%! rt3 = sqrt (3);

%!test
%! x = [0, 1/2, rt2/2, rt3/2, 1, rt3/2, rt2/2, 1/2, 0];
%! v = [0, pi/6, pi/4, pi/3, pi/2, pi/3, pi/4, pi/6, 0];
%! assert (asin (x), v, sqrt (eps));

%!test
%! x = single ([0, 1/2, rt2/2, rt3/2, 1, rt3/2, rt2/2, 1/2, 0]);
%! v = single ([0, pi/6, pi/4, pi/3, pi/2, pi/3, pi/4, pi/6, 0]);
%! assert (asin (x), v, sqrt (eps ("single")));

## Test values on either side of branch cut
%!test
%! rval = pi/2;
%! ival = 1.31695789692481635;
%! obs = asin ([2, 2-i*eps, 2+i*eps]);
%! exp = [rval - ival*i, rval - ival*i, rval + ival*i];
%! assert (obs, exp, 2*eps);
%! obs = asin ([-2, -2-i*eps, -2+i*eps]);
%! exp = [-rval + ival*i, -rval - ival*i, -rval + ival*i];
%! assert (obs, exp, 2*eps);
%! assert (asin ([2 0]),  [rval - ival*i, 0], 2*eps);
%! assert (asin ([2 0i]), [rval - ival*i, 0], 2*eps);

%!error asin ()
%!error asin (1, 2)
*/

DEFUN (asinh, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} asinh (@var{x})\n\
Compute the inverse hyperbolic sine for each element of @var{x}.\n\
@seealso{sinh}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).asinh ();
  else
    print_usage ();

  return retval;
}

/*
%!test
%! v = [0, pi/2*i, 0, -pi/2*i];
%! x = [0, i, 0, -i];
%! assert (asinh (x), v,  sqrt (eps));

%!test
%! v = single ([0, pi/2*i, 0, -pi/2*i]);
%! x = single ([0, i, 0, -i]);
%! assert (asinh (x), v,  sqrt (eps ("single")));

%!error asinh ()
%!error asinh (1, 2)
*/

DEFUN (atan, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} atan (@var{x})\n\
Compute the inverse tangent in radians for each element of @var{x}.\n\
@seealso{tan, atand}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).atan ();
  else
    print_usage ();

  return retval;
}

/*
%!shared rt2, rt3
%! rt2 = sqrt (2);
%! rt3 = sqrt (3);

%!test
%! v = [0, pi/6, pi/4, pi/3, -pi/3, -pi/4, -pi/6, 0];
%! x = [0, rt3/3, 1, rt3, -rt3, -1, -rt3/3, 0];
%! assert (atan (x), v, sqrt (eps));

%!test
%! v = single ([0, pi/6, pi/4, pi/3, -pi/3, -pi/4, -pi/6, 0]);
%! x = single ([0, rt3/3, 1, rt3, -rt3, -1, -rt3/3, 0]);
%! assert (atan (x), v, sqrt (eps ("single")));

%!error atan ()
%!error atan (1, 2)
*/

DEFUN (atanh, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} atanh (@var{x})\n\
Compute the inverse hyperbolic tangent for each element of @var{x}.\n\
@seealso{tanh}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).atanh ();
  else
    print_usage ();

  return retval;
}

/*
%!test
%! v = [0, 0];
%! x = [0, 0];
%! assert (atanh (x), v, sqrt (eps));

%!test
%! v = single ([0, 0]);
%! x = single ([0, 0]);
%! assert (atanh (x), v, sqrt (eps ("single")));

%!error atanh ()
%!error atanh (1, 2)
*/

DEFUN (cbrt, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} cbrt (@var{x})\n\
Compute the real cube root of each element of @var{x}.\n\
\n\
Unlike @code{@var{x}^(1/3)}, the result will be negative if @var{x} is\n\
negative.\n\
@seealso{nthroot}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).cbrt ();
  else
    print_usage ();

  return retval;
}

/*
%!assert (cbrt (64), 4)
%!assert (cbrt (-125), -5)
%!assert (cbrt (0), 0)
%!assert (cbrt (Inf), Inf)
%!assert (cbrt (-Inf), -Inf)
%!assert (cbrt (NaN), NaN)
%!assert (cbrt (2^300), 2^100)
%!assert (cbrt (125*2^300), 5*2^100)

%!error cbrt ()
%!error cbrt (1, 2)
*/

DEFUN (ceil, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} ceil (@var{x})\n\
Return the smallest integer not less than @var{x}.\n\
\n\
This is equivalent to rounding towards positive infinity.\n\
\n\
If @var{x} is complex, return\n\
@code{ceil (real (@var{x})) + ceil (imag (@var{x})) * I}.\n\
\n\
@example\n\
@group\n\
ceil ([-2.7, 2.7])\n\
    @result{} -2    3\n\
@end group\n\
@end example\n\
@seealso{floor, round, fix}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).ceil ();
  else
    print_usage ();

  return retval;
}

/*
## double precision
%!assert (ceil ([2, 1.1, -1.1, -1]), [2, 2, -1, -1])

## complex double precison
%!assert (ceil ([2+2i, 1.1+1.1i, -1.1-1.1i, -1-i]), [2+2i, 2+2i, -1-i, -1-i])

## single precision
%!assert (ceil (single ([2, 1.1, -1.1, -1])), single ([2, 2, -1, -1]))

## complex single precision
%!assert (ceil (single ([2+2i, 1.1+1.1i, -1.1-1.1i, -1-i])), single ([2+2i, 2+2i, -1-i, -1-i]))

%!error ceil ()
%!error ceil (1, 2)
*/

DEFUN (conj, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} conj (@var{z})\n\
Return the complex conjugate of @var{z}.\n\
\n\
The complex conjugate is defined as\n\
@tex\n\
$\\bar{z} = x - iy$.\n\
@end tex\n\
@ifnottex\n\
@code{conj (@var{z})} = @var{x} - @var{i}@var{y}.\n\
@end ifnottex\n\
@seealso{real, imag}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).conj ();
  else
    print_usage ();

  return retval;
}

/*
%!assert (conj (1), 1)
%!assert (conj (i), -i)
%!assert (conj (1+i), 1-i)
%!assert (conj (1-i), 1+i)
%!assert (conj ([-1, -i; -1+i, -1-i]), [-1, i; -1-i, -1+i])

%!assert (conj (single (1)), single (1))
%!assert (conj (single (i)), single (-i))
%!assert (conj (single (1+i)), single (1-i))
%!assert (conj (single (1-i)), single (1+i))
%!assert (conj (single ([-1, -i; -1+i, -1-i])), single ([-1, i; -1-i, -1+i]))

%!error conj ()
%!error conj (1, 2)
*/

DEFUN (cos, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} cos (@var{x})\n\
Compute the cosine for each element of @var{x} in radians.\n\
@seealso{acos, cosd, cosh}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).cos ();
  else
    print_usage ();

  return retval;
}

/*
%!shared rt2, rt3
%! rt2 = sqrt (2);
%! rt3 = sqrt (3);

%!test
%! x = [0, pi/6, pi/4, pi/3, pi/2, 2*pi/3, 3*pi/4, 5*pi/6, pi];
%! v = [1, rt3/2, rt2/2, 1/2, 0, -1/2, -rt2/2, -rt3/2, -1];
%! assert (cos (x), v, sqrt (eps));

%!test
%! rt2 = sqrt (2);
%! rt3 = sqrt (3);
%! x = single ([0, pi/6, pi/4, pi/3, pi/2, 2*pi/3, 3*pi/4, 5*pi/6, pi]);
%! v = single ([1, rt3/2, rt2/2, 1/2, 0, -1/2, -rt2/2, -rt3/2, -1]);
%! assert (cos (x), v, sqrt (eps ("single")));

%!error cos ()
%!error cos (1, 2)
*/

DEFUN (cosh, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} cosh (@var{x})\n\
Compute the hyperbolic cosine for each element of @var{x}.\n\
@seealso{acosh, sinh, tanh}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).cosh ();
  else
    print_usage ();

  return retval;
}

/*
%!test
%! x = [0, pi/2*i, pi*i, 3*pi/2*i];
%! v = [1, 0, -1, 0];
%! assert (cosh (x), v, sqrt (eps));

%!test
%! x = single ([0, pi/2*i, pi*i, 3*pi/2*i]);
%! v = single ([1, 0, -1, 0]);
%! assert (cosh (x), v, sqrt (eps ("single")));

%!error cosh ()
%!error cosh (1, 2)
*/

DEFUN (erf, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} erf (@var{z})\n\
Compute the error function.\n\
\n\
The error function is defined as\n\
@tex\n\
$$\n\
 {\\rm erf} (z) = {2 \\over \\sqrt{\\pi}}\\int_0^z e^{-t^2} dt\n\
$$\n\
@end tex\n\
@ifnottex\n\
\n\
@example\n\
@group\n\
                        z\n\
              2        /\n\
erf (z) = --------- *  | e^(-t^2) dt\n\
          sqrt (pi)    /\n\
                    t=0\n\
@end group\n\
@end example\n\
\n\
@end ifnottex\n\
@seealso{erfc, erfcx, erfi, dawson, erfinv, erfcinv}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).erf ();
  else
    print_usage ();

  return retval;
}

/*
%!test
%! a = -1i*sqrt (-1/(6.4187*6.4187));
%! assert (erf (a), erf (real (a)));

%!test
%! x = [0,.5,1];
%! v = [0, .520499877813047, .842700792949715];
%! assert (erf (x), v, 1.e-10);
%! assert (erf (-x), -v, 1.e-10);
%! assert (erfc (x), 1-v, 1.e-10);
%! assert (erfinv (v), x, 1.e-10);

%!test
%! a = -1i*sqrt (single (-1/(6.4187*6.4187)));
%! assert (erf (a), erf (real (a)));

%!test
%! x = single ([0,.5,1]);
%! v = single ([0, .520499877813047, .842700792949715]);
%! assert (erf (x), v, 1.e-6);
%! assert (erf (-x), -v, 1.e-6);
%! assert (erfc (x), 1-v, 1.e-6);
%! assert (erfinv (v), x, 1.e-6);

%!test
%! x = [1+2i,-1+2i,1e-6+2e-6i,0+2i];
%! v = [-0.53664356577857-5.04914370344703i, 0.536643565778565-5.04914370344703i, 0.112837916709965e-5+0.225675833419178e-5i, 18.5648024145755526i];
%! assert (erf (x), v, -1.e-10);
%! assert (erf (-x), -v, -1.e-10);
%! assert (erfc (x), 1-v, -1.e-10);

%!error erf ()
%!error erf (1, 2)
*/

DEFUN (erfinv, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} erfinv (@var{x})\n\
Compute the inverse error function.\n\
\n\
The inverse error function is defined such that\n\
\n\
@example\n\
erf (@var{y}) == @var{x}\n\
@end example\n\
@seealso{erf, erfc, erfcx, erfi, dawson, erfcinv}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).erfinv ();
  else
    print_usage ();

  return retval;
}

/*
## middle region
%!assert (erf (erfinv ([-0.9 -0.3 0 0.4 0.8])), [-0.9 -0.3 0 0.4 0.8], eps)
%!assert (erf (erfinv (single ([-0.9 -0.3 0 0.4 0.8]))), single ([-0.9 -0.3 0 0.4 0.8]), eps ("single"))
## tail region
%!assert (erf (erfinv ([-0.999 -0.99 0.9999 0.99999])), [-0.999 -0.99 0.9999 0.99999], eps)
%!assert (erf (erfinv (single ([-0.999 -0.99 0.9999 0.99999]))), single ([-0.999 -0.99 0.9999 0.99999]), eps ("single"))
## backward - loss of accuracy
%!assert (erfinv (erf ([-3 -1 -0.4 0.7 1.3 2.8])), [-3 -1 -0.4 0.7 1.3 2.8], -1e-12)
%!assert (erfinv (erf (single ([-3 -1 -0.4 0.7 1.3 2.8]))), single ([-3 -1 -0.4 0.7 1.3 2.8]), -1e-4)
## exceptional
%!assert (erfinv ([-1, 1, 1.1, -2.1]), [-Inf, Inf, NaN, NaN])
%!error erfinv (1+2i)

%!error erfinv ()
%!error erfinv (1, 2)
*/

DEFUN (erfcinv, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} erfcinv (@var{x})\n\
Compute the inverse complementary error function.\n\
\n\
The inverse complementary error function is defined such that\n\
\n\
@example\n\
erfc (@var{y}) == @var{x}\n\
@end example\n\
@seealso{erfc, erf, erfcx, erfi, dawson, erfinv}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).erfcinv ();
  else
    print_usage ();

  return retval;
}

/*
## middle region
%!assert (erfc (erfcinv ([1.9 1.3 1 0.6 0.2])), [1.9 1.3 1 0.6 0.2], eps)
%!assert (erfc (erfcinv (single ([1.9 1.3 1 0.6 0.2]))), single ([1.9 1.3 1 0.6 0.2]), eps ("single"))
## tail region
%!assert (erfc (erfcinv ([0.001 0.01 1.9999 1.99999])), [0.001 0.01 1.9999 1.99999], eps)
%!assert (erfc (erfcinv (single ([0.001 0.01 1.9999 1.99999]))), single ([0.001 0.01 1.9999 1.99999]), eps ("single"))
## backward - loss of accuracy
%!assert (erfcinv (erfc ([-3 -1 -0.4 0.7 1.3 2.8])), [-3 -1 -0.4 0.7 1.3 2.8], -1e-12)
%!assert (erfcinv (erfc (single ([-3 -1 -0.4 0.7 1.3 2.8]))), single ([-3 -1 -0.4 0.7 1.3 2.8]), -1e-4)
## exceptional
%!assert (erfcinv ([2, 0, -0.1, 2.1]), [-Inf, Inf, NaN, NaN])
%!error erfcinv (1+2i)

%!error erfcinv ()
%!error erfcinv (1, 2)
*/

DEFUN (erfc, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} erfc (@var{z})\n\
Compute the complementary error function.\n\
\n\
The complementary error function is defined as\n\
@tex\n\
$1 - {\\rm erf} (z)$.\n\
@end tex\n\
@ifnottex\n\
@w{@code{1 - erf (@var{z})}}.\n\
@end ifnottex\n\
@seealso{erfcinv, erfcx, erfi, dawson, erf, erfinv}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).erfc ();
  else
    print_usage ();

  return retval;
}

/*
%!test
%! a = -1i*sqrt (-1/(6.4187*6.4187));
%! assert (erfc (a), erfc (real (a)));

%!error erfc ()
%!error erfc (1, 2)
*/

DEFUN (erfcx, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} erfcx (@var{z})\n\
Compute the scaled complementary error function.\n\
\n\
The scaled complementary error function is defined as\n\
@tex\n\
$$\n\
 e^{z^2} {\\rm erfc} (z) \\equiv e^{z^2} (1 - {\\rm erf} (z))\n\
$$\n\
@end tex\n\
@ifnottex\n\
\n\
@example\n\
exp (z^2) * erfc (z)\n\
@end example\n\
\n\
@end ifnottex\n\
@seealso{erfc, erf, erfi, dawson, erfinv, erfcinv}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).erfcx ();
  else
    print_usage ();

  return retval;
}

/*

%!test
%! x = [1+2i,-1+2i,1e-6+2e-6i,0+2i];
%! assert (erfcx (x), exp (x.^2) .* erfc(x), -1.e-10);

%!test
%! x = [100, 100+20i];
%! v = [0.0056416137829894329, 0.0054246791754558-0.00108483153786434i];
%! assert (erfcx (x), v, -1.e-10);

%!error erfcx ()
%!error erfcx (1, 2)
*/

DEFUN (erfi, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} erfi (@var{z})\n\
Compute the imaginary error function.\n\
\n\
The imaginary error function is defined as\n\
@tex\n\
$$\n\
 -i {\\rm erf} (iz)\n\
$$\n\
@end tex\n\
@ifnottex\n\
\n\
@example\n\
-i * erf (i*z)\n\
@end example\n\
\n\
@end ifnottex\n\
@seealso{erfc, erf, erfcx, dawson, erfinv, erfcinv}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).erfi ();
  else
    print_usage ();

  return retval;
}

/*

%!test
%! x = [-0.1, 0.1, 1, 1+2i,-1+2i,1e-6+2e-6i,0+2i];
%! assert (erfi (x), -i * erf(i*x), -1.e-10);

%!error erfi ()
%!error erfi (1, 2)
*/

DEFUN (dawson, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} dawson (@var{z})\n\
Compute the Dawson (scaled imaginary error) function.\n\
\n\
The Dawson function is defined as\n\
@tex\n\
$$\n\
 {\\sqrt{\\pi} \\over 2} e^{-z^2} {\\rm erfi} (z) \\equiv -i {\\sqrt{\\pi} \\over 2} e^{-z^2} {\\rm erf} (iz)\n\
$$\n\
@end tex\n\
@ifnottex\n\
\n\
@example\n\
(sqrt (pi) / 2) * exp (-z^2) * erfi (z)\n\
@end example\n\
\n\
@end ifnottex\n\
@seealso{erfc, erf, erfcx, erfi, erfinv, erfcinv}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).dawson ();
  else
    print_usage ();

  return retval;
}

/*

%!test
%! x = [0.1, 1, 1+2i,-1+2i,1e-4+2e-4i,0+2i];
%! v = [0.099335992397852861, 0.53807950691, -13.38892731648-11.828715104i, 13.38892731648-11.828715104i, 0.0001000000073333+0.000200000001333i, 48.160012114291i];
%! assert (dawson (x), v, -1.e-10);
%! assert (dawson (-x), -v, -1.e-10);

%!error dawson ()
%!error dawson (1, 2)
*/

DEFUN (exp, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} exp (@var{x})\n\
Compute\n\
@tex\n\
$e^{x}$\n\
@end tex\n\
@ifnottex\n\
@code{e^x}\n\
@end ifnottex\n\
for each element of @var{x}.\n\
\n\
To compute the matrix exponential, see @ref{Linear Algebra}.\n\
@seealso{log}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).exp ();
  else
    print_usage ();

  return retval;
}

/*
%!assert (exp ([0, 1, -1, -1000]), [1, e, 1/e, 0], sqrt (eps))
%!assert (exp (1+i), e * (cos (1) + sin (1) * i), sqrt (eps))
%!assert (exp (single ([0, 1, -1, -1000])), single ([1, e, 1/e, 0]), sqrt (eps ("single")))
%!assert (exp (single (1+i)), single (e * (cos (1) + sin (1) * i)), sqrt (eps ("single")))

%!assert (exp ([Inf, -Inf, NaN]), [Inf 0 NaN])
%!assert (exp (single ([Inf, -Inf, NaN])), single ([Inf 0 NaN]))

%!error exp ()
%!error exp (1, 2)
*/

DEFUN (expm1, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} expm1 (@var{x})\n\
Compute\n\
@tex\n\
$ e^{x} - 1 $\n\
@end tex\n\
@ifnottex\n\
@code{exp (@var{x}) - 1}\n\
@end ifnottex\n\
accurately in the neighborhood of zero.\n\
@seealso{exp}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).expm1 ();
  else
    print_usage ();

  return retval;
}

/*
%!assert (expm1 (2*eps), 2*eps, 1e-29)

%!assert (expm1 ([Inf, -Inf, NaN]), [Inf -1 NaN])
%!assert (expm1 (single ([Inf, -Inf, NaN])), single ([Inf -1 NaN]))

%!error expm1 ()
%!error expm1 (1, 2)
*/

DEFUN (isfinite, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} isfinite (@var{x})\n\
Return a logical array which is true where the elements of @var{x} are\n\
finite values and false where they are not.\n\
\n\
For example:\n\
\n\
@example\n\
@group\n\
isfinite ([13, Inf, NA, NaN])\n\
     @result{} [ 1, 0, 0, 0 ]\n\
@end group\n\
@end example\n\
@seealso{isinf, isnan, isna}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).finite ();
  else
    print_usage ();

  return retval;
}

/*
%!assert (!isfinite (Inf))
%!assert (!isfinite (NaN))
%!assert (isfinite (rand (1,10)))

%!assert (!isfinite (single (Inf)))
%!assert (!isfinite (single (NaN)))
%!assert (isfinite (single (rand (1,10))))

%!error isfinite ()
%!error isfinite (1, 2)
*/

DEFUN (fix, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} fix (@var{x})\n\
Truncate fractional portion of @var{x} and return the integer portion.\n\
\n\
This is equivalent to rounding towards zero.  If @var{x} is complex, return\n\
@code{fix (real (@var{x})) + fix (imag (@var{x})) * I}.\n\
\n\
@example\n\
@group\n\
fix ([-2.7, 2.7])\n\
   @result{} -2    2\n\
@end group\n\
@end example\n\
@seealso{ceil, floor, round}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).fix ();
  else
    print_usage ();

  return retval;
}

/*
%!assert (fix ([1.1, 1, -1.1, -1]), [1, 1, -1, -1])
%!assert (fix ([1.1+1.1i, 1+i, -1.1-1.1i, -1-i]), [1+i, 1+i, -1-i, -1-i])
%!assert (fix (single ([1.1, 1, -1.1, -1])), single ([1, 1, -1, -1]))
%!assert (fix (single ([1.1+1.1i, 1+i, -1.1-1.1i, -1-i])), single ([1+i, 1+i, -1-i, -1-i]))

%!error fix ()
%!error fix (1, 2)
*/

DEFUN (floor, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} floor (@var{x})\n\
Return the largest integer not greater than @var{x}.\n\
\n\
This is equivalent to rounding towards negative infinity.  If @var{x} is\n\
complex, return @code{floor (real (@var{x})) + floor (imag (@var{x})) * I}.\n\
\n\
@example\n\
@group\n\
floor ([-2.7, 2.7])\n\
     @result{} -3    2\n\
@end group\n\
@end example\n\
@seealso{ceil, round, fix}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).floor ();
  else
    print_usage ();

  return retval;
}

/*
%!assert (floor ([2, 1.1, -1.1, -1]), [2, 1, -2, -1])
%!assert (floor ([2+2i, 1.1+1.1i, -1.1-1.1i, -1-i]), [2+2i, 1+i, -2-2i, -1-i])
%!assert (floor (single ([2, 1.1, -1.1, -1])), single ([2, 1, -2, -1]))
%!assert (floor (single ([2+2i, 1.1+1.1i, -1.1-1.1i, -1-i])), single ([2+2i, 1+i, -2-2i, -1-i]))

%!error floor ()
%!error floor (1, 2)
*/

DEFUN (gamma, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} gamma (@var{z})\n\
Compute the Gamma function.\n\
\n\
The Gamma function is defined as\n\
@tex\n\
$$\n\
 \\Gamma (z) = \\int_0^\\infty t^{z-1} e^{-t} dt.\n\
$$\n\
@end tex\n\
@ifnottex\n\
\n\
@example\n\
@group\n\
             infinity\n\
            /\n\
gamma (z) = | t^(z-1) exp (-t) dt.\n\
            /\n\
         t=0\n\
@end group\n\
@end example\n\
\n\
@end ifnottex\n\
\n\
Programming Note: The gamma function can grow quite large even for small\n\
input values.  In many cases it may be preferable to use the natural\n\
logarithm of the gamma function (@code{gammaln}) in calculations to minimize\n\
loss of precision.  The final result is then\n\
@code{exp (@var{result_using_gammaln}).}\n\
@seealso{gammainc, gammaln, factorial}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).gamma ();
  else
    print_usage ();

  return retval;
}

/*
%!test
%! a = -1i*sqrt (-1/(6.4187*6.4187));
%! assert (gamma (a), gamma (real (a)));

%!test
%! x = [.5, 1, 1.5, 2, 3, 4, 5];
%! v = [sqrt(pi), 1, .5*sqrt(pi), 1, 2, 6, 24];
%! assert (gamma (x), v, sqrt (eps));

%!test
%! a = single (-1i*sqrt (-1/(6.4187*6.4187)));
%! assert (gamma (a), gamma (real (a)));

%!test
%! x = single ([.5, 1, 1.5, 2, 3, 4, 5]);
%! v = single ([sqrt(pi), 1, .5*sqrt(pi), 1, 2, 6, 24]);
%! assert (gamma (x), v, sqrt (eps ("single")));

%!test
%! ## Test exceptional values
%! x = [-Inf, -1, -0, 0, 1, Inf, NaN];
%! v = [Inf, Inf, -Inf, Inf, 1, Inf, NaN];
%! assert (gamma (x), v);
%! assert (gamma (single (x)), single (v));

%!error gamma ()
%!error gamma (1, 2)
*/

DEFUN (imag, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} imag (@var{z})\n\
Return the imaginary part of @var{z} as a real number.\n\
@seealso{real, conj}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).imag ();
  else
    print_usage ();

  return retval;
}

/*
%!assert (imag (1), 0)
%!assert (imag (i), 1)
%!assert (imag (1+i), 1)
%!assert (imag ([i, 1; 1, i]), full (eye (2)))

%!assert (imag (single (1)), single (0))
%!assert (imag (single (i)), single (1))
%!assert (imag (single (1+i)), single (1))
%!assert (imag (single ([i, 1; 1, i])), full (eye (2,"single")))

%!error imag ()
%!error imag (1, 2)
*/

DEFUNX ("isalnum", Fisalnum, args, ,
        "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} isalnum (@var{s})\n\
Return a logical array which is true where the elements of @var{s} are\n\
letters or digits and false where they are not.\n\
\n\
This is equivalent to (@code{isalpha (@var{s}) | isdigit (@var{s})}).\n\
@seealso{isalpha, isdigit, ispunct, isspace, iscntrl}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).xisalnum ();
  else
    print_usage ();

  return retval;
}

/*
%!test
%! charset = char (0:127);
%! result = false (1, 128);
%! result(toascii ("A":"Z") + 1) = true;
%! result(toascii ("0":"9") + 1) = true;
%! result(toascii ("a":"z") + 1) = true;
%! assert (isalnum (charset), result);

%!error isalnum ()
%!error isalnum (1, 2)
*/

DEFUNX ("isalpha", Fisalpha, args, ,
        "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} isalpha (@var{s})\n\
Return a logical array which is true where the elements of @var{s} are\n\
letters and false where they are not.\n\
\n\
This is equivalent to (@code{islower (@var{s}) | isupper (@var{s})}).\n\
@seealso{isdigit, ispunct, isspace, iscntrl, isalnum, islower, isupper}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).xisalpha ();
  else
    print_usage ();

  return retval;
}

/*
%!test
%! charset = char (0:127);
%! result = false (1, 128);
%! result(toascii ("A":"Z") + 1) = true;
%! result(toascii ("a":"z") + 1) = true;
%! assert (isalpha (charset), result);

%!error isalpha ()
%!error isalpha (1, 2)
*/

DEFUNX ("isascii", Fisascii, args, ,
        "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} isascii (@var{s})\n\
Return a logical array which is true where the elements of @var{s} are\n\
ASCII characters (in the range 0 to 127 decimal) and false where they are\n\
not.\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).xisascii ();
  else
    print_usage ();

  return retval;
}

/*
%!test
%! charset = char (0:127);
%! result = true (1, 128);
%! assert (isascii (charset), result);

%!error isascii ()
%!error isascii (1, 2)
*/

DEFUNX ("iscntrl", Fiscntrl, args, ,
        "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} iscntrl (@var{s})\n\
Return a logical array which is true where the elements of @var{s} are\n\
control characters and false where they are not.\n\
@seealso{ispunct, isspace, isalpha, isdigit}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).xiscntrl ();
  else
    print_usage ();

  return retval;
}

/*
%!test
%! charset = char (0:127);
%! result = false (1, 128);
%! result(1:32) = true;
%! result(128) = true;
%! assert (iscntrl (charset), result);

%!error iscntrl ()
%!error iscntrl (1, 2)
*/

DEFUNX ("isdigit", Fisdigit, args, ,
        "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} isdigit (@var{s})\n\
Return a logical array which is true where the elements of @var{s} are\n\
decimal digits (0-9) and false where they are not.\n\
@seealso{isxdigit, isalpha, isletter, ispunct, isspace, iscntrl}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).xisdigit ();
  else
    print_usage ();

  return retval;
}

/*
%!test
%! charset = char (0:127);
%! result = false (1, 128);
%! result(toascii ("0":"9") + 1) = true;
%! assert (isdigit (charset), result);

%!error isdigit ()
%!error isdigit (1, 2)
*/

DEFUN (isinf, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} isinf (@var{x})\n\
Return a logical array which is true where the elements of @var{x} are\n\
infinite and false where they are not.\n\
\n\
For example:\n\
\n\
@example\n\
@group\n\
isinf ([13, Inf, NA, NaN])\n\
      @result{} [ 0, 1, 0, 0 ]\n\
@end group\n\
@end example\n\
@seealso{isfinite, isnan, isna}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).isinf ();
  else
    print_usage ();

  return retval;
}

/*
%!assert (isinf (Inf))
%!assert (!isinf (NaN))
%!assert (!isinf (NA))
%!assert (isinf (rand (1,10)), false (1,10))
%!assert (isinf ([NaN -Inf -1 0 1 Inf NA]), [false, true, false, false, false, true, false])

%!assert (isinf (single (Inf)))
%!assert (!isinf (single (NaN)))
%!assert (!isinf (single (NA)))
%!assert (isinf (single (rand (1,10))), false (1,10))
%!assert (isinf (single ([NaN -Inf -1 0 1 Inf NA])), [false, true, false, false, false, true, false])

%!error isinf ()
%!error isinf (1, 2)
*/

DEFUNX ("isgraph", Fisgraph, args, ,
        "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} isgraph (@var{s})\n\
Return a logical array which is true where the elements of @var{s} are\n\
printable characters (but not the space character) and false where they are\n\
not.\n\
@seealso{isprint}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).xisgraph ();
  else
    print_usage ();

  return retval;
}

/*
%!test
%! charset = char (0:127);
%! result = false (1, 128);
%! result(34:127) = true;
%! assert (isgraph (charset), result);

%!error isgraph ()
%!error isgraph (1, 2)
*/

DEFUNX ("islower", Fislower, args, ,
        "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} islower (@var{s})\n\
Return a logical array which is true where the elements of @var{s} are\n\
lowercase letters and false where they are not.\n\
@seealso{isupper, isalpha, isletter, isalnum}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).xislower ();
  else
    print_usage ();

  return retval;
}

/*
%!test
%! charset = char (0:127);
%! result = false (1, 128);
%! result(toascii ("a":"z") + 1) = true;
%! assert (islower (charset), result);

%!error islower ()
%!error islower (1, 2)
*/

DEFUN (isna, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} isna (@var{x})\n\
Return a logical array which is true where the elements of @var{x} are\n\
NA (missing) values and false where they are not.\n\
\n\
For example:\n\
\n\
@example\n\
@group\n\
isna ([13, Inf, NA, NaN])\n\
     @result{} [ 0, 0, 1, 0 ]\n\
@end group\n\
@end example\n\
@seealso{isnan, isinf, isfinite}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).isna ();
  else
    print_usage ();

  return retval;
}

/*
%!assert (!isna (Inf))
%!assert (!isna (NaN))
%!assert (isna (NA))
%!assert (isna (rand (1,10)), false (1,10))
%!assert (isna ([NaN -Inf -1 0 1 Inf NA]), [false, false, false, false, false, false, true])

%!assert (!isna (single (Inf)))
%!assert (!isna (single (NaN)))
%!assert (isna (single (NA)))
%!assert (isna (single (rand (1,10))), false (1,10))
%!assert (isna (single ([NaN -Inf -1 0 1 Inf NA])), [false, false, false, false, false, false, true])

%!error isna ()
%!error isna (1, 2)
*/

DEFUN (isnan, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} isnan (@var{x})\n\
Return a logical array which is true where the elements of @var{x} are\n\
NaN values and false where they are not.\n\
\n\
NA values are also considered NaN values.  For example:\n\
\n\
@example\n\
@group\n\
isnan ([13, Inf, NA, NaN])\n\
      @result{} [ 0, 0, 1, 1 ]\n\
@end group\n\
@end example\n\
@seealso{isna, isinf, isfinite}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).isnan ();
  else
    print_usage ();

  return retval;
}

/*
%!assert (!isnan (Inf))
%!assert (isnan (NaN))
%!assert (isnan (NA))
%!assert (isnan (rand (1,10)), false (1,10))
%!assert (isnan ([NaN -Inf -1 0 1 Inf NA]), [true, false, false, false, false, false, true])

%!assert (!isnan (single (Inf)))
%!assert (isnan (single (NaN)))
%!assert (isnan (single (NA)))
%!assert (isnan (single (rand (1,10))), false (1,10))
%!assert (isnan (single ([NaN -Inf -1 0 1 Inf NA])), [true, false, false, false, false, false, true])

%!error isnan ()
%!error isnan (1, 2)
*/

DEFUNX ("isprint", Fisprint, args, ,
        "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} isprint (@var{s})\n\
Return a logical array which is true where the elements of @var{s} are\n\
printable characters (including the space character) and false where they\n\
are not.\n\
@seealso{isgraph}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).xisprint ();
  else
    print_usage ();

  return retval;
}

/*
%!test
%! charset = char (0:127);
%! result = false (1, 128);
%! result(33:127) = true;
%! assert (isprint (charset), result);

%!error isprint ()
%!error isprint (1, 2)
*/

DEFUNX ("ispunct", Fispunct, args, ,
        "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} ispunct (@var{s})\n\
Return a logical array which is true where the elements of @var{s} are\n\
punctuation characters and false where they are not.\n\
@seealso{isalpha, isdigit, isspace, iscntrl}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).xispunct ();
  else
    print_usage ();

  return retval;
}

/*
%!test
%! charset = char (0:127);
%! result = false (1, 128);
%! result(34:48) = true;
%! result(59:65) = true;
%! result(92:97) = true;
%! result(124:127) = true;
%! assert (ispunct (charset), result);

%!error ispunct ()
%!error ispunct (1, 2)
*/

DEFUNX ("isspace", Fisspace, args, ,
        "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} isspace (@var{s})\n\
Return a logical array which is true where the elements of @var{s} are\n\
whitespace characters (space, formfeed, newline, carriage return, tab, and\n\
vertical tab) and false where they are not.\n\
@seealso{iscntrl, ispunct, isalpha, isdigit}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).xisspace ();
  else
    print_usage ();

  return retval;
}

/*
%!test
%! charset = char (0:127);
%! result = false (1, 128);
%! result(toascii (" \f\n\r\t\v") + 1) = true;
%! assert (isspace (charset), result);

%!error isspace ()
%!error isspace (1, 2)
*/

DEFUNX ("isupper", Fisupper, args, ,
        "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} isupper (@var{s})\n\
Return a logical array which is true where the elements of @var{s} are\n\
uppercase letters and false where they are not.\n\
@seealso{islower, isalpha, isletter, isalnum}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).xisupper ();
  else
    print_usage ();

  return retval;
}

/*
%!test
%! charset = char (0:127);
%! result = false (1, 128);
%! result(toascii ("A":"Z") + 1) = true;
%! assert (isupper (charset), result);

%!error isupper ()
%!error isupper (1, 2)
*/

DEFUNX ("isxdigit", Fisxdigit, args, ,
        "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} isxdigit (@var{s})\n\
Return a logical array which is true where the elements of @var{s} are\n\
hexadecimal digits (0-9 and @nospell{a-fA-F}).\n\
@seealso{isdigit}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).xisxdigit ();
  else
    print_usage ();

  return retval;
}

/*
%!test
%! charset = char (0:127);
%! result = false (1, 128);
%! result(toascii ("A":"F") + 1) = true;
%! result(toascii ("0":"9") + 1) = true;
%! result(toascii ("a":"f") + 1) = true;
%! assert (isxdigit (charset), result);

%!error isxdigit ()
%!error isxdigit (1, 2)
*/

DEFUN (lgamma, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Mapping Function} {} gammaln (@var{x})\n\
@deftypefnx {Mapping Function} {} lgamma (@var{x})\n\
Return the natural logarithm of the gamma function of @var{x}.\n\
@seealso{gamma, gammainc}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).lgamma ();
  else
    print_usage ();

  return retval;
}

/*
%!test
%! a = -1i*sqrt (-1/(6.4187*6.4187));
%! assert (gammaln (a), gammaln (real (a)));

%!test
%! x = [.5, 1, 1.5, 2, 3, 4, 5];
%! v = [sqrt(pi), 1, .5*sqrt(pi), 1, 2, 6, 24];
%! assert (gammaln (x), log (v), sqrt (eps))

%!test
%! a = single (-1i*sqrt (-1/(6.4187*6.4187)));
%! assert (gammaln (a), gammaln (real (a)));

%!test
%! x = single ([.5, 1, 1.5, 2, 3, 4, 5]);
%! v = single ([sqrt(pi), 1, .5*sqrt(pi), 1, 2, 6, 24]);
%! assert (gammaln (x), log (v), sqrt (eps ("single")))

%!test
%! x = [-1, 0, 1, Inf];
%! v = [Inf, Inf, 0, Inf];
%! assert (gammaln (x), v);
%! assert (gammaln (single (x)), single (v));

%!error gammaln ()
%!error gammaln (1,2)
*/

DEFUN (log, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} log (@var{x})\n\
Compute the natural logarithm,\n\
@tex\n\
$\\ln{(x)},$\n\
@end tex\n\
@ifnottex\n\
@code{ln (@var{x})},\n\
@end ifnottex\n\
for each element of @var{x}.\n\
\n\
To compute the matrix logarithm, see @ref{Linear Algebra}.\n\
@seealso{exp, log1p, log2, log10, logspace}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).log ();
  else
    print_usage ();

  return retval;
}

/*
%!assert (log ([1, e, e^2]), [0, 1, 2], sqrt (eps))
%!assert (log ([-0.5, -1.5, -2.5]), log ([0.5, 1.5, 2.5]) + pi*1i, sqrt (eps))

%!assert (log (single ([1, e, e^2])), single ([0, 1, 2]), sqrt (eps ("single")))
%!assert (log (single ([-0.5, -1.5, -2.5])), single (log ([0.5, 1.5, 2.5]) + pi*1i), 4*eps ("single"))

%!error log ()
%!error log (1, 2)
*/

DEFUN (log10, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} log10 (@var{x})\n\
Compute the base-10 logarithm of each element of @var{x}.\n\
@seealso{log, log2, logspace, exp}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).log10 ();
  else
    print_usage ();

  return retval;
}

/*
%!assert (log10 ([0.01, 0.1, 1, 10, 100]), [-2, -1, 0, 1, 2], sqrt (eps))
%!assert (log10 (single ([0.01, 0.1, 1, 10, 100])), single ([-2, -1, 0, 1, 2]), sqrt (eps ("single")))

%!error log10 ()
%!error log10 (1, 2)
*/

DEFUN (log1p, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} log1p (@var{x})\n\
Compute\n\
@tex\n\
$\\ln{(1 + x)}$\n\
@end tex\n\
@ifnottex\n\
@code{log (1 + @var{x})}\n\
@end ifnottex\n\
accurately in the neighborhood of zero.\n\
@seealso{log, exp, expm1}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).log1p ();
  else
    print_usage ();

  return retval;
}

/*
%!assert (log1p ([0, 2*eps, -2*eps]), [0, 2*eps, -2*eps], 1e-29)
%!assert (log1p (single ([0, 2*eps, -2*eps])), single ([0, 2*eps, -2*eps]), 1e-29)

%!error log1p ()
%!error log1p (1, 2)
*/

DEFUN (real, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} real (@var{z})\n\
Return the real part of @var{z}.\n\
@seealso{imag, conj}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).real ();
  else
    print_usage ();

  return retval;
}

/*
%!assert (real (1), 1)
%!assert (real (i), 0)
%!assert (real (1+i), 1)
%!assert (real ([1, i; i, 1]), full (eye (2)))

%!assert (real (single (1)), single (1))
%!assert (real (single (i)), single (0))
%!assert (real (single (1+i)), single (1))
%!assert (real (single ([1, i; i, 1])), full (eye (2,"single")))

%!error real ()
%!error real (1, 2)
*/

DEFUN (round, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} round (@var{x})\n\
Return the integer nearest to @var{x}.\n\
\n\
If @var{x} is complex, return\n\
@code{round (real (@var{x})) + round (imag (@var{x})) * I}.  If there\n\
are two nearest integers, return the one further away from zero.\n\
\n\
@example\n\
@group\n\
round ([-2.7, 2.7])\n\
     @result{} -3    3\n\
@end group\n\
@end example\n\
@seealso{ceil, floor, fix, roundb}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).round ();
  else
    print_usage ();

  return retval;
}

/*
%!assert (round (1), 1)
%!assert (round (1.1), 1)
%!assert (round (5.5), 6)
%!assert (round (i), i)
%!assert (round (2.5+3.5i), 3+4i)
%!assert (round (-2.6), -3)
%!assert (round ([1.1, -2.4; -3.7, 7.1]), [1, -2; -4, 7])

%!assert (round (single (1)), single (1))
%!assert (round (single (1.1)), single (1))
%!assert (round (single (5.5)), single (6))
%!assert (round (single (i)), single (i))
%!assert (round (single (2.5+3.5i)), single (3+4i))
%!assert (round (single (-2.6)), single (-3))
%!assert (round (single ([1.1, -2.4; -3.7, 7.1])), single ([1, -2; -4, 7]))

%!error round ()
%!error round (1, 2)
*/

DEFUN (roundb, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} roundb (@var{x})\n\
Return the integer nearest to @var{x}.  If there are two nearest\n\
integers, return the even one (banker's rounding).\n\
\n\
If @var{x} is complex,\n\
return @code{roundb (real (@var{x})) + roundb (imag (@var{x})) * I}.\n\
@seealso{round}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).roundb ();
  else
    print_usage ();

  return retval;
}

/*
%!assert (roundb (1), 1)
%!assert (roundb (1.1), 1)
%!assert (roundb (1.5), 2)
%!assert (roundb (4.5), 4)
%!assert (roundb (i), i)
%!assert (roundb (2.5+3.5i), 2+4i)
%!assert (roundb (-2.6), -3)
%!assert (roundb ([1.1, -2.4; -3.7, 7.1]), [1, -2; -4, 7])

%!assert (roundb (single (1)), single (1))
%!assert (roundb (single (1.1)), single (1))
%!assert (roundb (single (1.5)), single (2))
%!assert (roundb (single (4.5)), single (4))
%!assert (roundb (single (i)), single (i))
%!assert (roundb (single (2.5+3.5i)), single (2+4i))
%!assert (roundb (single (-2.6)), single (-3))
%!assert (roundb (single ([1.1, -2.4; -3.7, 7.1])), single ([1, -2; -4, 7]))

%!error roundb ()
%!error roundb (1, 2)
*/

DEFUN (sign, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} sign (@var{x})\n\
Compute the @dfn{signum} function.\n\
\n\
This is defined as\n\
@tex\n\
$$\n\
{\\rm sign} (@var{x}) = \\cases{1,&$x>0$;\\cr 0,&$x=0$;\\cr -1,&$x<0$.\\cr}\n\
$$\n\
@end tex\n\
@ifnottex\n\
\n\
@example\n\
@group\n\
           -1, x < 0;\n\
sign (x) =  0, x = 0;\n\
            1, x > 0.\n\
@end group\n\
@end example\n\
\n\
@end ifnottex\n\
\n\
For complex arguments, @code{sign} returns @code{x ./ abs (@var{x})}.\n\
\n\
Note that @code{sign (-0.0)} is 0.  Although IEEE 754 floating point\n\
allows zero to be signed, 0.0 and -0.0 compare equal.  If you must test\n\
whether zero is signed, use the @code{signbit} function.\n\
@seealso{signbit}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).signum ();
  else
    print_usage ();

  return retval;
}

/*
%!assert (sign (-2) , -1)
%!assert (sign (0), 0)
%!assert (sign (3), 1)
%!assert (sign ([1, -pi; e, 0]), [1, -1; 1, 0])

%!assert (sign (single (-2)) , single (-1))
%!assert (sign (single (0)), single (0))
%!assert (sign (single (3)), single (1))
%!assert (sign (single ([1, -pi; e, 0])), single ([1, -1; 1, 0]))

%!error sign ()
%!error sign (1, 2)
*/

DEFUNX ("signbit", Fsignbit, args, ,
        "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} signbit (@var{x})\n\
Return logical true if the value of @var{x} has its sign bit set and false\n\
otherwise.\n\
\n\
This behavior is consistent with the other logical functions.\n\
See @ref{Logical Values}.  The behavior differs from the C language function\n\
which returns nonzero if the sign bit is set.\n\
\n\
This is not the same as @code{x < 0.0}, because IEEE 754 floating point\n\
allows zero to be signed.  The comparison @code{-0.0 < 0.0} is false,\n\
but @code{signbit (-0.0)} will return a nonzero value.\n\
@seealso{sign}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    {
      retval = args(0).xsignbit ();
      retval = (retval != 0);
    }
  else
    print_usage ();

  return retval;
}

/*
%!assert (signbit (1) == 0)
%!assert (signbit (-2) != 0)
%!assert (signbit (0) == 0)
%!assert (signbit (-0) != 0)

%!assert (signbit (single (1)) == 0)
%!assert (signbit (single (-2)) != 0)
%!assert (signbit (single (0)) == 0)
%!assert (signbit (single (-0)) != 0)

%!error sign ()
%!error sign (1, 2)
*/

DEFUN (sin, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} sin (@var{x})\n\
Compute the sine for each element of @var{x} in radians.\n\
@seealso{asin, sind, sinh}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).sin ();
  else
    print_usage ();

  return retval;
}

/*
%!shared rt2, rt3
%! rt2 = sqrt (2);
%! rt3 = sqrt (3);

%!test
%! x = [0, pi/6, pi/4, pi/3, pi/2, 2*pi/3, 3*pi/4, 5*pi/6, pi];
%! v = [0, 1/2, rt2/2, rt3/2, 1, rt3/2, rt2/2, 1/2, 0];
%! assert (sin (x), v, sqrt (eps));

%!test
%! x = single ([0, pi/6, pi/4, pi/3, pi/2, 2*pi/3, 3*pi/4, 5*pi/6, pi]);
%! v = single ([0, 1/2, rt2/2, rt3/2, 1, rt3/2, rt2/2, 1/2, 0]);
%! assert (sin (x), v, sqrt (eps ("single")));

%!error sin ()
%!error sin (1, 2)
*/

DEFUN (sinh, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} sinh (@var{x})\n\
Compute the hyperbolic sine for each element of @var{x}.\n\
@seealso{asinh, cosh, tanh}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).sinh ();
  else
    print_usage ();

  return retval;
}

/*
%!test
%! x = [0, pi/2*i, pi*i, 3*pi/2*i];
%! v = [0, i, 0, -i];
%! assert (sinh (x), v, sqrt (eps));

%!test
%! x = single ([0, pi/2*i, pi*i, 3*pi/2*i]);
%! v = single ([0, i, 0, -i]);
%! assert (sinh (x), v, sqrt (eps ("single")));

%!error sinh ()
%!error sinh (1, 2)
*/

DEFUN (sqrt, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} sqrt (@var{x})\n\
Compute the square root of each element of @var{x}.\n\
\n\
If @var{x} is negative, a complex result is returned.\n\
\n\
To compute the matrix square root, see @ref{Linear Algebra}.\n\
@seealso{realsqrt, nthroot}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).sqrt ();
  else
    print_usage ();

  return retval;
}

/*
%!assert (sqrt (4), 2)
%!assert (sqrt (-1), i)
%!assert (sqrt (1+i), exp (0.5 * log (1+i)), sqrt (eps))
%!assert (sqrt ([4, -4; i, 1-i]), [2, 2i; exp(0.5 * log (i)), exp(0.5 * log (1-i))], sqrt (eps))

%!assert (sqrt (single (4)), single (2))
%!assert (sqrt (single (-1)), single (i))
%!assert (sqrt (single (1+i)), single (exp (0.5 * log (1+i))), sqrt (eps ("single")))
%!assert (sqrt (single ([4, -4; i, 1-i])), single ([2, 2i; exp(0.5 * log (i)), exp(0.5 * log (1-i))]), sqrt (eps ("single")))

%!error sqrt ()
%!error sqrt (1, 2)
*/

DEFUN (tan, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} tan (@var{z})\n\
Compute the tangent for each element of @var{x} in radians.\n\
@seealso{atan, tand, tanh}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).tan ();
  else
    print_usage ();

  return retval;
}

/*
%!shared rt2, rt3
%! rt2 = sqrt (2);
%! rt3 = sqrt (3);

%!test
%! x = [0, pi/6, pi/4, pi/3, 2*pi/3, 3*pi/4, 5*pi/6, pi];
%! v = [0, rt3/3, 1, rt3, -rt3, -1, -rt3/3, 0];
%! assert (tan (x), v,  sqrt (eps));

%!test
%! x = single ([0, pi/6, pi/4, pi/3, 2*pi/3, 3*pi/4, 5*pi/6, pi]);
%! v = single ([0, rt3/3, 1, rt3, -rt3, -1, -rt3/3, 0]);
%! assert (tan (x), v,  sqrt (eps ("single")));

%!error tan ()
%!error tan (1, 2)
*/

DEFUN (tanh, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} tanh (@var{x})\n\
Compute hyperbolic tangent for each element of @var{x}.\n\
@seealso{atanh, sinh, cosh}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).tanh ();
  else
    print_usage ();

  return retval;
}

/*
%!test
%! x = [0, pi*i];
%! v = [0, 0];
%! assert (tanh (x), v, sqrt (eps));

%!test
%! x = single ([0, pi*i]);
%! v = single ([0, 0]);
%! assert (tanh (x), v, sqrt (eps ("single")));

%!error tanh ()
%!error tanh (1, 2)
*/

DEFUNX ("toascii", Ftoascii, args, ,
        "-*- texinfo -*-\n\
@deftypefn {Mapping Function} {} toascii (@var{s})\n\
Return ASCII representation of @var{s} in a matrix.\n\
\n\
For example:\n\
\n\
@example\n\
@group\n\
toascii (\"ASCII\")\n\
     @result{} [ 65, 83, 67, 73, 73 ]\n\
@end group\n\
\n\
@end example\n\
@seealso{char}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).xtoascii ();
  else
    print_usage ();

  return retval;
}

/*
%!assert (toascii (char (0:127)), 0:127)
%!assert (toascii (" ":"@"), 32:64)
%!assert (toascii ("A":"Z"), 65:90)
%!assert (toascii ("[":"`"), 91:96)
%!assert (toascii ("a":"z"), 97:122)
%!assert (toascii ("{":"~"), 123:126)

%!error toascii ()
%!error toascii (1, 2)
*/

DEFUNX ("tolower", Ftolower, args, ,
        "-*- texinfo -*-\n\
@deftypefn  {Mapping Function} {} tolower (@var{s})\n\
@deftypefnx {Mapping Function} {} lower (@var{s})\n\
Return a copy of the string or cell string @var{s}, with each uppercase\n\
character replaced by the corresponding lowercase one; non-alphabetic\n\
characters are left unchanged.\n\
\n\
For example:\n\
\n\
@example\n\
@group\n\
tolower (\"MiXeD cAsE 123\")\n\
      @result{} \"mixed case 123\"\n\
@end group\n\
@end example\n\
@seealso{toupper}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).xtolower ();
  else
    print_usage ();

  return retval;
}

DEFALIAS (lower, tolower);

/*
%!assert (tolower ("OCTAVE"), "octave")
%!assert (tolower ("123OCTave!_&"), "123octave!_&")
%!assert (tolower ({"ABC", "DEF", {"GHI", {"JKL"}}}), {"abc", "def", {"ghi", {"jkl"}}})
%!assert (tolower (["ABC"; "DEF"]), ["abc"; "def"])
%!assert (tolower ({["ABC"; "DEF"]}), {["abc";"def"]})
%!assert (tolower (68), 68)
%!assert (tolower ({[68, 68; 68, 68]}), {[68, 68; 68, 68]})
%!test
%! classes = {@char, @double, @single, ...
%!            @int8, @int16, @int32, @int64, ...
%!            @uint8, @uint16, @uint32, @uint64};
%! for i = 1:numel (classes)
%!   cls = classes{i};
%!   assert (class (tolower (cls (97))), class (cls (97)));
%!   assert (class (tolower (cls ([98, 99]))), class (cls ([98, 99])));
%! endfor
%!test
%! a(3,3,3,3) = "D";
%! assert (tolower (a)(3,3,3,3), "d");

%!test
%! charset = char (0:127);
%! result = charset;
%! result (toascii ("A":"Z") + 1) = result (toascii ("a":"z") + 1);
%! assert (tolower (charset), result);

%!error <Invalid call to tolower> lower ()
%!error <Invalid call to tolower> tolower ()
%!error tolower (1, 2)
*/

DEFUNX ("toupper", Ftoupper, args, ,
        "-*- texinfo -*-\n\
@deftypefn  {Mapping Function} {} toupper (@var{s})\n\
@deftypefnx {Mapping Function} {} upper (@var{s})\n\
Return a copy of the string or cell string @var{s}, with each lowercase\n\
character replaced by the corresponding uppercase one; non-alphabetic\n\
characters are left unchanged.\n\
\n\
For example:\n\
\n\
@example\n\
@group\n\
toupper (\"MiXeD cAsE 123\")\n\
      @result{} \"MIXED CASE 123\"\n\
@end group\n\
@end example\n\
@seealso{tolower}\n\
@end deftypefn")
{
  octave_value retval;
  if (args.length () == 1)
    retval = args(0).xtoupper ();
  else
    print_usage ();

  return retval;
}

DEFALIAS (upper, toupper);

/*
%!assert (toupper ("octave"), "OCTAVE")
%!assert (toupper ("123OCTave!_&"), "123OCTAVE!_&")
%!assert (toupper ({"abc", "def", {"ghi", {"jkl"}}}), {"ABC", "DEF", {"GHI", {"JKL"}}})
%!assert (toupper (["abc"; "def"]), ["ABC"; "DEF"])
%!assert (toupper ({["abc"; "def"]}), {["ABC";"DEF"]})
%!assert (toupper (100), 100)
%!assert (toupper ({[100, 100; 100, 100]}), {[100, 100; 100, 100]})
%!test
%! classes = {@char, @double, @single, ...
%!            @int8, @int16, @int32, @int64, ...
%!            @uint8, @uint16, @uint32, @uint64};
%! for i = 1:numel (classes)
%!   cls = classes{i};
%!   assert (class (toupper (cls (97))), class (cls (97)));
%!   assert (class (toupper (cls ([98, 99]))), class (cls ([98, 99])));
%! endfor
%!test
%! a(3,3,3,3) = "d";
%! assert (toupper (a)(3,3,3,3), "D");
%!test
%! charset = char (0:127);
%! result = charset;
%! result (toascii  ("a":"z") + 1) = result (toascii  ("A":"Z") + 1);
%! assert (toupper (charset), result);

%!error <Invalid call to toupper> toupper ()
%!error <Invalid call to toupper> upper ()
%!error toupper (1, 2)
*/

DEFALIAS (gammaln, lgamma);

