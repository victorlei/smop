## Copyright (C) 2001-2015 Kai Habel
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
## @deftypefn  {Function File} {@var{pp} =} pchip (@var{x}, @var{y})
## @deftypefnx {Function File} {@var{yi} =} pchip (@var{x}, @var{y}, @var{xi})
## Return the Piecewise Cubic Hermite Interpolating Polynomial (pchip) of
## points @var{x} and @var{y}.
##
## If called with two arguments, return the piecewise polynomial @var{pp}
## that may be used with @code{ppval} to evaluate the polynomial at specific
## points.
##
## When called with a third input argument, @code{pchip} evaluates the pchip
## polynomial at the points @var{xi}.  The third calling form is equivalent to
## @code{ppval (pchip (@var{x}, @var{y}), @var{xi})}.
##
## The variable @var{x} must be a strictly monotonic vector (either increasing
## or decreasing) of length @var{n}.
##
## @var{y} can be either a vector or array.  If @var{y} is a vector then it
## must be the same length @var{n} as @var{x}.  If @var{y} is an array then
## the size of @var{y} must have the form
## @tex
## $$[s_1, s_2, \cdots, s_k, n]$$
## @end tex
## @ifnottex
## @code{[@var{s1}, @var{s2}, @dots{}, @var{sk}, @var{n}]}
## @end ifnottex
## The array is reshaped internally to a matrix where the leading dimension is
## given by
## @tex
## $$s_1 s_2 \cdots s_k$$
## @end tex
## @ifnottex
## @code{@var{s1} * @var{s2} * @dots{} * @var{sk}}
## @end ifnottex
## and each row of this matrix is then treated separately.  Note that this is
## exactly opposite to @code{interp1} but is done for @sc{matlab} compatibility.
##
## @seealso{spline, ppval, mkpp, unmkpp}
## @end deftypefn

## Author:  Kai Habel <kai.habel@gmx.de>
## Date: 9. mar 2001
##
## S_k = a_k + b_k*x + c_k*x^2 + d_k*x^3; (spline polynom)
##
## 4 conditions:
## S_k(x_k) = y_k;
## S_k(x_k+1) = y_k+1;
## S_k'(x_k) = y_k';
## S_k'(x_k+1) = y_k+1';

function ret = pchip (x, y, xi)

  if (nargin < 2 || nargin > 3)
    print_usage ();
  endif

  ## make row vector
  x = x(:).';
  n = length (x);

  ## Check the size and shape of y
  if (isvector (y))
    y = y(:).';  # force row vector
    szy = size (y);
    if (! size_equal (x, y))
      error ("pchip: length of X and Y must match");
    endif
  else
    szy = size (y);
    if (n != szy(end))
      error ("pchip: length of X and last dimension of Y must match");
    endif
    y = reshape (y, [prod(szy(1:end-1)), szy(end)]);
  endif

  h = diff (x);
  if (all (h < 0))
    x = fliplr (x);
    h = diff (x);
    y = fliplr (y);
  elseif (any (h <= 0))
    error ("pchip: X must be strictly monotonic");
  endif

  f1 = y(:, 1:n-1);

  ## Compute derivatives.
  d = __pchip_deriv__ (x, y, 2);
  d1 = d(:, 1:n-1);
  d2 = d(:, 2:n);

  ## This is taken from SLATEC.
  h = diag (h);

  delta = diff (y, 1, 2) / h;
  del1 = (d1 - delta) / h;
  del2 = (d2 - delta) / h;
  c3 = del1 + del2;
  c2 = -c3 - del1;
  c3 = c3 / h;
  coeffs = cat (3, c3, c2, d1, f1);

  ret = mkpp (x, coeffs, szy(1:end-1));

  if (nargin == 3)
    ret = ppval (ret, xi);
  endif

endfunction


%!demo
%! x = 0:8;
%! y = [1, 1, 1, 1, 0.5, 0, 0, 0, 0];
%! xi = 0:0.01:8;
%! yspline = spline (x,y,xi);
%! ypchip = pchip (x,y,xi);
%! title ("pchip and spline fit to discontinuous function");
%! plot (xi,yspline, xi,ypchip,"-", x,y,"+");
%! legend ("spline", "pchip", "data");
%! %-------------------------------------------------------------------
%! % confirm that pchip agreed better to discontinuous data than spline

%!shared x, y, y2, pp, yi1, yi2, yi3
%! x = 0:8;
%! y = [1, 1, 1, 1, 0.5, 0, 0, 0, 0];
%!assert (pchip (x,y,x), y)
%!assert (pchip (x,y,x'), y')
%!assert (pchip (x',y',x'), y')
%!assert (pchip (x',y',x), y)
%!assert (isempty (pchip (x',y',[])))
%!assert (isempty (pchip (x,y,[])))
%!assert (pchip (x,[y;y],x), [pchip(x,y,x);pchip(x,y,x)])
%!assert (pchip (x,[y;y],x'), [pchip(x,y,x);pchip(x,y,x)])
%!assert (pchip (x',[y;y],x), [pchip(x,y,x);pchip(x,y,x)])
%!assert (pchip (x',[y;y],x'), [pchip(x,y,x);pchip(x,y,x)])
%!test
%! x = (0:8)*pi/4; y = [sin(x); cos(x)];
%! y2(:,:,1) = y; y2(:,:,2) = y+1; y2(:,:,3) = y-1;
%! pp = pchip (x, shiftdim (y2,2));
%! yi1 = ppval (pp, (1:4)*pi/4);
%! yi2 = ppval (pp, repmat ((1:4)*pi/4, [5,1]));
%! yi3 = ppval (pp, [pi/2,pi]);
%!assert (size (pp.coefs), [48,4])
%!assert (pp.pieces, 8)
%!assert (pp.order, 4)
%!assert (pp.dim, [3,2])
%!assert (ppval (pp,pi), [0,-1;1,0;-1,-2], 1e-14)
%!assert (yi3(:,:,2), ppval (pp,pi), 1e-14)
%!assert (yi3(:,:,1), [1,0;2,1;0,-1], 1e-14)
%!assert (squeeze (yi1(1,2,:)), [1/sqrt(2); 0; -1/sqrt(2);-1], 1e-14)
%!assert (size (yi2), [3,2,5,4])
%!assert (squeeze (yi2(1,2,3,:)), [1/sqrt(2); 0; -1/sqrt(2);-1], 1e-14)

%!error (pchip (1,2));
%!error (pchip (1,2,3));

