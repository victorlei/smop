## Copyright (C) 2000-2015 Kai Habel
## Copyright (C) 2008 Marco Caliari
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

## -*- texinfo -*-
## @deftypefn  {Function File} {@var{l} =} legendre (@var{n}, @var{x})
## @deftypefnx {Function File} {@var{l} =} legendre (@var{n}, @var{x}, @var{normalization})
## Compute the Legendre function of degree @var{n} and order
## @var{m} = 0 @dots{} @var{n}.
##
## The value @var{n} must be a real non-negative integer.
##
## @var{x} is a vector with real-valued elements in the range [-1, 1].
##
## The optional argument @var{normalization} may be one of @qcode{"unnorm"},
## @qcode{"sch"}, or @qcode{"norm"}.  The default if no normalization is given
## is @qcode{"unnorm"}.
##
## When the optional argument @var{normalization} is @qcode{"unnorm"}, compute
## the Legendre function of degree @var{n} and order @var{m} and return all
## values for @var{m} = 0 @dots{} @var{n}.  The return value has one dimension
## more than @var{x}.
##
## The Legendre Function of degree @var{n} and order @var{m}:
##
## @tex
## $$
## P^m_n(x) = (-1)^m (1-x^2)^{m/2}{d^m\over {dx^m}}P_n (x)
## $$
## @end tex
## @ifnottex
##
## @example
## @group
##  m         m      2  m/2   d^m
## P(x) = (-1) * (1-x  )    * ----  P(x)
##  n                         dx^m   n
## @end group
## @end example
##
## @end ifnottex
##
## @noindent
## with Legendre polynomial of degree @var{n}:
##
## @tex
## $$
## P(x) = {1\over{2^n n!}}\biggl({d^n\over{dx^n}}(x^2 - 1)^n\biggr)
## $$
## @end tex
## @ifnottex
##
## @example
## @group
##           1    d^n   2    n
## P(x) = ------ [----(x - 1) ]
##  n     2^n n!  dx^n
## @end group
## @end example
##
## @end ifnottex
##
## @noindent
## @code{legendre (3, [-1.0, -0.9, -0.8])} returns the matrix:
##
## @example
## @group
##  x  |   -1.0   |   -0.9   |   -0.8
## ------------------------------------
## m=0 | -1.00000 | -0.47250 | -0.08000
## m=1 |  0.00000 | -1.99420 | -1.98000
## m=2 |  0.00000 | -2.56500 | -4.32000
## m=3 |  0.00000 | -1.24229 | -3.24000
## @end group
## @end example
##
## When the optional argument @code{normalization} is @qcode{"sch"}, compute
## the Schmidt semi-normalized associated Legendre function.  The Schmidt
## semi-normalized associated Legendre function is related to the unnormalized
## Legendre functions by the following:
##
## For Legendre functions of degree @var{n} and order 0:
##
## @tex
## $$
## SP^0_n (x) = P^0_n (x)
## $$
## @end tex
## @ifnottex
##
## @example
## @group
##   0      0
## SP(x) = P(x)
##   n      n
## @end group
## @end example
##
## @end ifnottex
##
## For Legendre functions of degree n and order m:
##
## @tex
## $$
## SP^m_n (x) = P^m_n (x)(-1)^m\biggl({2(n-m)!\over{(n+m)!}}\biggl)^{0.5}
## $$
## @end tex
## @ifnottex
##
## @example
## @group
##   m      m         m    2(n-m)! 0.5
## SP(x) = P(x) * (-1)  * [-------]
##   n      n              (n+m)!
## @end group
## @end example
##
## @end ifnottex
##
## When the optional argument @var{normalization} is @qcode{"norm"}, compute
## the fully normalized associated Legendre function.  The fully normalized
## associated Legendre function is related to the unnormalized Legendre
## functions by the following:
##
## For Legendre functions of degree @var{n} and order @var{m}
##
## @tex
## $$
## NP^m_n (x) = P^m_n (x)(-1)^m\biggl({(n+0.5)(n-m)!\over{(n+m)!}}\biggl)^{0.5}
## $$
## @end tex
## @ifnottex
##
## @example
## @group
##   m      m         m    (n+0.5)(n-m)! 0.5
## NP(x) = P(x) * (-1)  * [-------------]
##   n      n                  (n+m)!
## @end group
## @end example
##
## @end ifnottex
## @end deftypefn

## Author: Marco Caliari <marco.caliari@univr.it>

function retval = legendre (n, x, normalization)

  persistent warned_overflow = false;

  if (nargin < 2 || nargin > 3)
    print_usage ();
  endif

  if (! isreal (n) || ! isscalar (n) || n < 0 || n != fix (n))
    error ("legendre: N must be a real non-negative integer");
  elseif (! isreal (x) || any (x(:) < -1 | x(:) > 1))
    error ("legendre: X must be real-valued vector in the range -1 <= X <= 1");
  endif

  if (nargin == 3)
    normalization = lower (normalization);
  else
    normalization = "unnorm";
  endif

  unnorm = false;
  switch (normalization)
    case "unnorm"
      scale = 1;
      unnorm = true;
    case "norm"
      scale = sqrt (n+0.5);
    case "sch"
      scale = sqrt (2);
    otherwise
      error ('legendre: NORMALIZATION option must be "unnorm", "norm", or "sch"');
  endswitch

  scale = scale * ones (size (x));

  ## Based on the recurrence relation below
  ##            m                 m              m
  ## (n-m+1) * P (x) = (2*n+1)*x*P (x)  - (n+1)*P (x)
  ##            n+1               n              n-1
  ## http://en.wikipedia.org/wiki/Associated_Legendre_function

  overflow = false;
  retval = zeros ([n+1, size(x)]);
  for m = 1:n
    lpm1 = scale;
    lpm2 = (2*m-1) .* x .* scale;
    lpm3 = lpm2;
    for k = m+1:n
      lpm3a = (2*k-1) .* x .* lpm2;
      lpm3b = (k+m-2) .* lpm1;
      lpm3 = (lpm3a - lpm3b) / (k-m+1);
      lpm1 = lpm2;
      lpm2 = lpm3;
      if (! warned_overflow)
        if (   any (abs (lpm3a) > realmax)
            || any (abs (lpm3b) > realmax)
            || any (abs (lpm3)  > realmax))
          overflow = true;
        endif
      endif
    endfor
    retval(m,:) = lpm3(:);
    if (unnorm)
      scale *= -(2*m-1);
    else  # normalization = "sch" or "norm"
      scale *= (2*m-1) / sqrt ((n-m+1)*(n+m));
    endif
    scale .*= sqrt (1-x.^2);
  endfor

  retval(n+1,:) = scale(:);

  if (isvector (x))
    ## vector case is special
    retval = reshape (retval, n + 1, length (x));
  endif

  if (strcmp (normalization, "sch"))
    retval(1,:) ./= sqrt (2);
  endif

  if (overflow && ! warned_overflow)
    warning ("legendre: overflow - results may be unstable for high orders");
    warned_overflow = true;
  endif

endfunction


%!test
%! result = legendre (3, [-1.0 -0.9 -0.8]);
%! expected = [
%!    -1.00000  -0.47250  -0.08000
%!     0.00000  -1.99420  -1.98000
%!     0.00000  -2.56500  -4.32000
%!     0.00000  -1.24229  -3.24000
%! ];
%! assert (result, expected, 1e-5);

%!test
%! result = legendre (3, [-1.0 -0.9 -0.8], "sch");
%! expected = [
%!    -1.00000  -0.47250  -0.08000
%!     0.00000   0.81413   0.80833
%!    -0.00000  -0.33114  -0.55771
%!     0.00000   0.06547   0.17076
%! ];
%! assert (result, expected, 1e-5);

%!test
%! result = legendre (3, [-1.0 -0.9 -0.8], "norm");
%! expected = [
%!    -1.87083  -0.88397  -0.14967
%!     0.00000   1.07699   1.06932
%!    -0.00000  -0.43806  -0.73778
%!     0.00000   0.08661   0.22590
%! ];
%! assert (result, expected, 1e-5);

%!test
%! result = legendre (151, 0);
%! ## Don't compare to "-Inf" since it would fail on 64 bit systems.
%! assert (result(end) < -1.7976e308 && all (isfinite (result(1:end-1))));

%!test
%! result = legendre (150, 0);
%! ## This agrees with Matlab's result.
%! assert (result(end), 3.7532741115719e+306, 0.0000000000001e+306);

%!test
%! result = legendre (0, 0:0.1:1);
%! assert (result, full (ones (1,11)));

%!test
%! ## Test matrix input
%! result = legendre (3, [-1,0,1;1,0,-1]);
%! expected(:,:,1) = [-1,1;0,0;0,0;0,0];
%! expected(:,:,2) = [0,0;1.5,1.5;0,0;-15,-15];
%! expected(:,:,3) = [1,-1;0,0;0,0;0,0];
%! assert (result, expected);

%!test
%! result = legendre (3, [-1,0,1;1,0,-1]');
%! expected(:,:,1) = [-1,0,1;0,1.5,0;0,0,0;0,-15,0];
%! expected(:,:,2) = [1,0,-1;0,1.5,0;0,0,0;0,-15,0];
%! assert (result, expected);

## Test input validation
%!error legendre ()
%!error legendre (1)
%!error legendre (1,2,3,4)
%!error <must be a real non-negative integer> legendre (i, [-1, 0, 1])
%!error <must be a real non-negative integer> legendre ([1, 2], [-1, 0, 1])
%!error <must be a real non-negative integer> legendre (-1, [-1, 0, 1])
%!error <must be a real non-negative integer> legendre (1.1, [-1, 0, 1])
%!error <must be real-valued vector> legendre (1, [-1+i, 0, 1])
%!error <in the range -1 .= X .= 1> legendre (1, [-2, 0, 1])
%!error <in the range -1 .= X .= 1> legendre (1, [-1, 0, 2])
%!error <NORMALIZATION option must be> legendre (1, [-1, 0, 1], "badnorm")

