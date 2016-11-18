## Copyright (C) 2000-2015 Kai Habel
## Copyright (C) 2006 David Bateman
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
## @deftypefn  {Function File} {@var{pp} =} spline (@var{x}, @var{y})
## @deftypefnx {Function File} {@var{yi} =} spline (@var{x}, @var{y}, @var{xi})
## Return the cubic spline interpolant of points @var{x} and @var{y}.
##
## When called with two arguments, return the piecewise polynomial @var{pp}
## that may be used with @code{ppval} to evaluate the polynomial at specific
## points.
##
## When called with a third input argument, @code{spline} evaluates the spline
## at the points @var{xi}.  The third calling form
## @code{spline (@var{x}, @var{y}, @var{xi})} is equivalent to
## @code{ppval (spline (@var{x}, @var{y}), @var{xi})}.
##
## The variable @var{x} must be a vector of length @var{n}.
##
## @var{y} can be either a vector or array.  If @var{y} is a vector it must
## have a length of either @var{n} or @code{@var{n} + 2}.  If the length of
## @var{y} is @var{n}, then the @qcode{"not-a-knot"} end condition is used.
## If the length of @var{y} is @code{@var{n} + 2}, then the first and last
## values of the vector @var{y} are the values of the first derivative of the
## cubic spline at the endpoints.
##
## If @var{y} is an array, then the size of @var{y} must have the form
## @tex
## $$[s_1, s_2, \cdots, s_k, n]$$
## @end tex
## @ifnottex
## @code{[@var{s1}, @var{s2}, @dots{}, @var{sk}, @var{n}]}
## @end ifnottex
## or
## @tex
## $$[s_1, s_2, \cdots, s_k, n + 2].$$
## @end tex
## @ifnottex
## @code{[@var{s1}, @var{s2}, @dots{}, @var{sk}, @var{n} + 2]}.
## @end ifnottex
## The array is reshaped internally to a matrix where the leading
## dimension is given by
## @tex
## $$s_1 s_2 \cdots s_k$$
## @end tex
## @ifnottex
## @code{@var{s1} * @var{s2} * @dots{} * @var{sk}}
## @end ifnottex
## and each row of this matrix is then treated separately.  Note that this is
## exactly the opposite of @code{interp1} but is done for @sc{matlab}
## compatibility.
##
## @seealso{pchip, ppval, mkpp, unmkpp}
## @end deftypefn

## This code is based on csape.m from octave-forge, but has been
## modified to use the sparse solver code in octave that itself allows
## special casing of tri-diagonal matrices, modified for NDArrays and
## for the treatment of vectors y 2 elements longer than x as complete
## splines.

function ret = spline (x, y, xi)

  x = x(:);
  n = length (x);
  if (n < 2)
    error ("spline: requires at least 2 points");
  endif

  ## Check the size and shape of y
  ndy = ndims (y);
  szy = size (y);
  if (ndy == 2 && (any (szy == n) || any (szy == n+2)))
    if (szy(2) == n || szy(2) == n+2)
      a = y.';
    else
      a = y;
      szy = szy([2 1]);
    endif
  else
    a = shiftdim (reshape (y, [prod(szy(1:end-1)), szy(end)]), 1);
  endif

  for k = (1:columns (a))(any (isnan (a)))
    ok = ! isnan (a(:,k));
    a(!ok,k) = spline (x(ok), a(ok,k), x(!ok));
  endfor

  complete = false;
  if (rows (a) == n + 2)
    complete = true;
    dfs = a(1,:);
    dfe = a(end,:);
    a = a(2:end-1,:);
  endif

  if (! issorted (x))
    [x, idx] = sort (x);
    a = a(idx,:);
  endif

  b = c = zeros (size (a));
  h = diff (x);
  idx = ones (columns (a), 1);

  if (complete)

    if (n == 2)
      d = (dfs + dfe) / (x(2) - x(1)) ^ 2 + ...
          2 * (a(1,:) - a(2,:)) / (x(2) - x(1)) ^ 3;
      c = (-2 * dfs - dfe) / (x(2) - x(1)) - ...
          3 * (a(1,:) - a(2,:)) / (x(2) - x(1)) ^ 2;
      b = dfs;
      a = a(1,:);
    else
      g(1,:) = (a(2,:) - a(1,:)) / h(1) - dfs;
      g(2:n-1,:) = (a(3:n,:) - a(2:n-1,:)) ./ h(2:n-1) - ...
                   (a(2:n-1,:) - a(1:n-2,:)) ./ h(1:n-2);
      g(n,:) = dfe - (a(n,:) - a(n-1,:)) / h(n-1);
      c = spdiags ([[h/6;0],[h(1)/3;(h(1:n-2)+h(2:n-1))/3;h(n-1)/3],[0;h/6]],...
                   [-1,0,1],n,n) \ (g / 2);
      b = diff (a) ./ h(1:n-1, idx) ...
          - h(1:n-1,idx) / 3 .* (c(2:n,:) + 2 * c(1:n-1,:));
      d = diff (c) ./ (3 * h(1:n-1, idx));

      d = d.'(:);
      c = c(1:n-1,:).'(:);
      b = b.'(:);
      a = a(1:n-1,:).'(:);
    endif
  else

    if (n == 2)
      b = (a(2,:) - a(1,:)) / (x(2) - x(1));
      a = a(1,:);
      d = [];
      c = [];
    elseif (n == 3)

      n = 2;
      c = (a(1,:) - a(3,:)) / ((x(3) - x(1)) * (x(2) - x(3))) ...
          + (a(2,:) - a(1,:)) / ((x(2) - x(1)) * (x(2) - x(3)));
      b = (a(2,:) - a(1,:)) * (x(3) - x(1)) ...
          / ((x(2) - x(1)) * (x(3) - x(2))) ...
          + (a(1,:) - a(3,:)) * (x(2) - x(1)) ...
          / ((x(3) - x(1)) * (x(3) - x(2)));
      a = a(1,:);
      d = [];
      x = [min(x), max(x)];
    else

      g = zeros (n-2, columns (a));
      g(1,:) = 3 / (h(1) + h(2)) ...
          * (a(3,:) - a(2,:) - h(2) / h(1) * (a(2,:) - a(1,:)));
      g(n-2,:) = 3 / (h(n-1) + h(n-2)) ...
          * (h(n-2) / h(n-1) * (a(n,:) - a(n-1,:)) - (a(n-1,:) - a(n-2,:)));

      if (n > 4)

        g(2:n - 3,:) = 3 * diff (a(3:n-1,:)) ./ h(3:n-2,idx) ...
            - 3 * diff (a(2:n-2,:)) ./ h(2:n - 3,idx);

        dg = 2 * (h(1:n-2) .+ h(2:n-1));
        dg(1) = dg(1) - h(1);
        dg(n-2) = dg(n-2) - h(n-1);

        ldg = udg = h(2:n-2);
        udg(1) = udg(1) - h(1);
        ldg(n - 3) = ldg(n-3) - h(n-1);
        c(2:n-1,:) = spdiags ([[ldg(:); 0], dg, [0; udg(:)]],
                              [-1, 0, 1], n-2, n-2) \ g;

      elseif (n == 4)

        dg = [h(1) + 2 * h(2); 2 * h(2) + h(3)];
        ldg = h(2) - h(3);
        udg = h(2) - h(1);
        c(2:n-1,:) = spdiags ([[ldg(:);0], dg, [0; udg(:)]],
                              [-1, 0, 1], n-2, n-2) \ g;

      endif

      c(1,:) = c(2,:) + h(1) / h(2) * (c(2,:) - c(3,:));
      c(n,:) = c(n-1,:) + h(n-1) / h(n-2) * (c(n-1,:) - c(n-2,:));
      b = diff (a) ./ h(1:n-1, idx) ...
          - h(1:n-1, idx) / 3 .* (c(2:n,:) + 2 * c(1:n-1,:));
      d = diff (c) ./ (3 * h(1:n-1, idx));

      d = d.'(:);
      c = c(1:n-1,:).'(:);
      b = b.'(:);
      a = a(1:n-1,:).'(:);
    endif

  endif
  ret = mkpp (x, cat (2, d, c, b, a), szy(1:end-1));

  if (nargin == 3)
    ret = ppval (ret, xi);
  endif

endfunction


%!demo
%! x = 0:10; y = sin (x);
%! xspline = 0:0.1:10;  yspline = spline (x,y,xspline);
%! title ("spline fit to points from sin (x)");
%! plot (xspline,sin(xspline),"r", xspline,yspline,"g-", x,y,"b+");
%! legend ("original", "interpolation", "interpolation points");
%! %--------------------------------------------------------
%! % confirm that interpolated function matches the original

%!shared x,y,abserr
%! x = [0:10]; y = sin (x); abserr = 1e-14;
%!assert (spline (x,y,x), y, abserr)
%!assert (spline (x,y,x'), y', abserr)
%!assert (spline (x',y',x'), y', abserr)
%!assert (spline (x',y',x), y, abserr)
%!assert (isempty (spline (x',y',[])))
%!assert (isempty (spline (x,y,[])))
%!assert (spline (x,[y;y],x), [spline(x,y,x);spline(x,y,x)], abserr)
%!assert (spline (x,[y;y],x'), [spline(x,y,x);spline(x,y,x)], abserr)
%!assert (spline (x',[y;y],x), [spline(x,y,x);spline(x,y,x)], abserr)
%!assert (spline (x',[y;y],x'), [spline(x,y,x);spline(x,y,x)], abserr)
%! y = cos (x) + i*sin (x);
%!assert (spline (x,y,x), y, abserr)
%!assert (real (spline (x,y,x)), real (y), abserr)
%!assert (real (spline (x,y,x.')), real (y).', abserr)
%!assert (real (spline (x.',y.',x.')), real (y).', abserr)
%!assert (real (spline (x.',y,x)), real (y), abserr)
%!assert (imag (spline (x,y,x)), imag (y), abserr)
%!assert (imag (spline (x,y,x.')), imag (y).', abserr)
%!assert (imag (spline (x.',y.',x.')), imag (y).', abserr)
%!assert (imag (spline (x.',y,x)), imag (y), abserr)
%!test
%! xnan = 5;
%! y(x==xnan) = NaN;
%! ok = ! isnan (y);
%! assert (spline (x, y, x(ok)), y(ok), abserr);
%!test
%! ok = ! isnan (y);
%! assert (! isnan (spline (x, y, x(!ok))));
%!test
%! x = [1,2];
%! y = [1,4];
%! assert (spline (x,y,x), [1,4], abserr);
%!test
%! x = [2,1];
%! y = [1,4];
%! assert (spline (x,y,x), [1,4], abserr);
%!test
%! x = [1,2];
%! y = [1,2,3,4];
%! pp = spline (x,y);
%! [x,P] = unmkpp (pp);
%! assert (P, [3,-3,1,2], abserr);
%!test
%! x = [2,1];
%! y = [1,2,3,4];
%! pp = spline (x,y);
%! pp2 = spline (x', y');
%! [x,P] = unmkpp (pp);
%! assert (P, [7,-9,1,3], abserr);
%! assert (pp2, pp);
%!test
%! x = [0,1,2];
%! y = [0,0,1,0,0];
%! pp = spline (x,y);
%! pp2 = spline (x', y');
%! [x,P] = unmkpp (pp);
%! assert (P, [-2,3,0,0;2,-3,0,1], abserr);
%! assert (pp2, pp);
%!test
%! x = [0,1,2,3];
%! y = [0,0,1,1,0,0];
%! pp = spline (x,y);
%! pp2 = spline (x', y');
%! [x,P] = unmkpp (pp);
%! assert (P, [-1,2,0,0;0,-1,1,1;1,-1,-1,1], abserr);
%! assert (pp2, pp);

