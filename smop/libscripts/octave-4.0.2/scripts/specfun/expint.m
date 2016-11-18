## Copyright (C) 2006, 2013 Sylvain Pelissier
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## Author: Sylvain Pelissier <sylvain.pelissier@gmail.com>

## -*- texinfo -*-
## @deftypefn {Function File} {} expint (@var{x})
## Compute the exponential integral:
## @tex
## $$
## {\rm E_1} (x) = \int_x^\infty {e^{-t} \over t} dt
## $$
## @end tex
## @ifnottex
##
## @example
## @group
##            infinity
##           /
## E_1 (x) = | exp (-t)/t dt
##           /
##          x
## @end group
## @end example
##
## @end ifnottex
## Note: For compatibility, this functions uses the @sc{matlab} definition
## of the exponential integral.  Most other sources refer to this particular
## value as @math{E_1 (x)}, and the exponential integral as
## @tex
## $$
## {\rm Ei} (x) = - \int_{-x}^\infty {e^{-t} \over t} dt.
## $$
## @end tex
## @ifnottex
##
## @example
## @group
##             infinity
##            /
## Ei (x) = - | exp (-t)/t dt
##            /
##          -x
## @end group
## @end example
##
## @end ifnottex
## The two definitions are related, for positive real values of @var{x}, by
## @tex
## $
## E_1 (-x) = -{\rm Ei} (x) - i\pi.
## $
## @end tex
## @ifnottex
## @w{@code{E_1 (-x) = -Ei (x) - i*pi}}.
## @end ifnottex
## @end deftypefn

function y = expint (x)

  if (nargin != 1)
    print_usage ();
  endif

  y = x;  # Copy over all values, including NaNs

  if (isreal (x))
    idx = (x >= 0);
    y(idx) = -expint_Ei (-x(idx));

    idx = (x < 0);
    y(idx) = -expint_Ei (-x(idx)) - i*pi;
  else
    idx = (imag (x) > 0);
    y(idx) = -expint_Ei (-x(idx)) - i*pi;

    idx = (imag (x) < 0);
    y(idx) = -expint_Ei (-x(idx)) + i*pi;

    isreal_idx = (imag (x) == 0);
    idx = (isreal_idx & real (x) >= 0);
    y(idx) = -expint_Ei (-x(idx));

    idx = (isreal_idx & real (x) < 0);
    y(idx) = -expint_Ei (-x(idx)) - i*pi;
  endif

endfunction

## -*- texinfo -*-
## @deftypefn {Function File} {@var{y} =} expint_Ei (@var{x})
## Compute the exponential integral:
## @verbatim
##                       infinity
##                      /
##    expint_Ei (x) = - | exp(-t)/t dt
##                      /
##                    -x
## @end verbatim
## @end deftypefn

function y = expint_Ei (x)

  y = zeros (size (x));
  F = @(x) exp (-x)./x;

  for t = 1:numel (x)
    xt = x(t);
    if (xt < 0 && imag (xt) == 0)
      ## Direct integration for most real inputs
      y(t) = -quad (F, -xt, Inf, [0, 1e-10]);
    elseif (xt > 2 && imag (xt) == 0)
      persistent Ei_2 = 4.954234356001890;
      y(t) = Ei_2 - quad (F, -xt, -2);
    elseif (abs (xt) < 10)
      ## Series Expansion for real (range [0,2]) or complex inputs (r < 10)
      k = 1;
      do
        term = xt^k / (k*factorial (k));
        y(t) += term;
      until (abs (term) < eps (abs (y(t))) / 2 || k++ >= 100)
      y(t) = 0.57721566490153286 + log (xt) + y(t);
    else
      ## FIXME: This expansion is accurate to only 1e-13 at the beginning
      ##        near 10+i, although it becomes more accurate as the magnitude
      ##        of xt grows.
      if (imag (xt) <= 0)
        persistent a1 = 4.03640;
        persistent a2 = 1.15198;
        persistent b1 = 5.03637;
        persistent b2 = 4.19160;
        y(t) = -(xt^2 - a1*xt + a2) ...
               / ((xt^2 - b1*xt + b2) * (-xt) * exp (-xt)) ...
               - i*pi;
      else
        y(t) = conj (expint_Ei (conj (xt)));
      endif;
    endif
  endfor
endfunction


## Test against A&S Table 5.1
%!test
%! x = [5:5:50]'/100;
%! gamma = 0.5772156649;
%! y_exp = [0.9876375971;
%!          0.9755453033;
%!          0.9637156702;
%!          0.9521414833;
%!          0.9408157528;
%!          0.9297317075;
%!          0.9188827858;
%!          0.9082626297;
%!          0.8978650778;
%!          0.8876841584 ];
%! y = (expint (x) + log(x) + gamma) ./ x;
%! assert (y, y_exp, 1e-9);
%!test
%! x = [50:5:95]'/100;
%! y_exp = [0.559773595;
%!          0.503364081;
%!          0.454379503;
%!          0.411516976;
%!          0.373768843;
%!          0.340340813;
%!          0.310596579;
%!          0.284019269;
%!          0.260183939;
%!          0.238737524 ];
%! y = expint (x);
%! assert (y, y_exp, 1e-9);
%!test
%! x = [100:5:145]'/100;
%! y_exp = [0.219383934;
%!          0.201872813;
%!          0.185990905;
%!          0.171555354;
%!          0.158408437;
%!          0.146413373;
%!          0.135450958;
%!          0.125416844;
%!          0.116219313;
%!          0.107777440 ];
%! y = expint (x);
%! assert (y, y_exp, 1e-9);
%!test
%! x = [150:5:200]'/100;
%! y_exp = [0.100019582;
%!          0.092882108;
%!          0.086308334;
%!          0.080247627;
%!          0.074654644;
%!          0.069488685;
%!          0.064713129;
%!          0.060294967;
%!          0.056204378;
%!          0.052414380;
%!          0.048900511 ];
%! y = expint (x);
%! assert (y, y_exp, 1e-9);

## Series expansion (-2 < x < 0)
## Expected values from Mathematica
%!test
%! x = [-0.1; -0.5; -1; -1.5; -2];
%! y_exp = [ 1.6228128139692767  - i*pi;
%!          -0.45421990486317358 - i*pi;
%!          -1.8951178163559368  - i*pi;
%!          -3.3012854491297978  - i*pi;
%!          -4.9542343560018902  - i*pi];
%! y = expint (x);
%! assert (y, y_exp, eps (real (y_exp)));

## (x < -2, x real)
%!test
%! x = [-2.5; -3; -10;-15; -25];
%! y_exp = [-7.0737658945786007   - i*pi;
%!          -9.9338325706254165   - i*pi;
%!          -2492.2289762418777   - i*pi;
%!          -234955.85249076830   - i*pi;
%!          -3.0059509065255486e9 - i*pi];
%! y = expint (x);
%! assert (y, y_exp, 8*eps (real (y_exp)));

## Complex values
%!test
%! x = [i; -1-i; 10-i; 10+i];
%! y_exp = [-0.33740392290096813   - i*0.62471325642771360;
%!          -1.7646259855638540    + i*0.75382280207927082;
%!          1.90746381979783120e-6 + i*3.67354374003294739e-6;
%!          1.90746381979783120e-6 - i*3.67354374003294739e-6];
%! y = expint (x);
%! assert (y, y_exp, 1e-12);

## Exceptional values (-Inf, Inf, NaN, 0, 0.37250741078)
%!test
%! x = [-Inf; Inf; NaN; 0; -0.3725074107813668];
%! y_exp = [-Inf - i*pi;
%!          -Inf;  # should be 0;
%!          NaN;
%!          Inf;
%!          0 - i*pi];
%! y = expint (x);
%! assert (y, y_exp, 5*eps);

## Test input validation
%!error expint ()
%!error expint (1,2)


