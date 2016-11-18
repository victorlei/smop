## Copyright (C) 2000-2015 Paul Kienzle
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
## @deftypefn  {Function File} {@var{pp} =} mkpp (@var{breaks}, @var{coefs})
## @deftypefnx {Function File} {@var{pp} =} mkpp (@var{breaks}, @var{coefs}, @var{d})
##
## Construct a piecewise polynomial (pp) structure from sample points
## @var{breaks} and coefficients @var{coefs}.
##
## @var{breaks} must be a vector of strictly increasing values.  The number of
## intervals is given by @code{@var{ni} = length (@var{breaks}) - 1}.
##
## When @var{m} is the polynomial order @var{coefs} must be of size:
## @var{ni} x @var{m} + 1.
##
## The i-th row of @var{coefs}, @code{@var{coefs} (@var{i},:)}, contains the
## coefficients for the polynomial over the @var{i}-th interval, ordered from
## highest (@var{m}) to lowest (@var{0}).
##
## @var{coefs} may also be a multi-dimensional array, specifying a vector-valued
## or array-valued polynomial.  In that case the polynomial order is defined
## by the length of the last dimension of @var{coefs}.  The size of first
## dimension(s) are given by the scalar or vector @var{d}.  If @var{d} is not
## given it is set to @code{1}.  In any case @var{coefs} is reshaped to a 2-D
## matrix of size @code{[@var{ni}*prod(@var{d} @var{m})] }
##
## @seealso{unmkpp, ppval, spline, pchip, ppder, ppint, ppjumps}
## @end deftypefn

function pp = mkpp (x, P, d)

  ## check number of arguments
  if (nargin < 2 || nargin > 3)
    print_usage ();
  endif

  ## check x
  if (length (x) < 2)
    error ("mkpp: at least one interval is needed");
  endif

  if (! isvector (x))
    error ("mkpp: x must be a vector");
  endif

  len = length (x) - 1;
  dP = length (size (P));

  pp = struct ("form", "pp",
               "breaks", x(:).',
               "coefs", [],
               "pieces", len,
               "order", prod (size (P)) / len,
               "dim", 1);

  if (nargin == 3)
    pp.dim = d;
    pp.order /= prod (d);
  endif

  dim_vec = [pp.pieces * prod(pp.dim), pp.order];
  pp.coefs = reshape (P, dim_vec);

endfunction


%!demo # linear interpolation
%! x = linspace (0,pi,5)';
%! t = [sin(x), cos(x)];
%! m = diff (t) ./ (x(2)-x(1));
%! b = t(1:4,:);
%! pp = mkpp (x, [m(:),b(:)]);
%! xi = linspace (0,pi,50);
%! plot (x,t,"x", xi,ppval (pp,xi));
%! legend ("control", "interp");

%!shared b,c,pp
%! b = 1:3; c = 1:24; pp = mkpp (b,c);
%!assert (pp.pieces, 2)
%!assert (pp.order, 12)
%!assert (pp.dim, 1)
%!assert (size (pp.coefs), [2,12])
%! pp = mkpp (b,c,2);
%!assert (pp.pieces, 2)
%!assert (pp.order, 6)
%!assert (pp.dim, 2)
%!assert (size (pp.coefs), [4,6])
%! pp = mkpp (b,c,3);
%!assert (pp.pieces, 2)
%!assert (pp.order, 4)
%!assert (pp.dim, 3)
%!assert (size (pp.coefs), [6,4])
%! pp = mkpp (b,c,[2,3]);
%!assert (pp.pieces, 2)
%!assert (pp.order, 2)
%!assert (pp.dim, [2,3])
%!assert (size (pp.coefs), [12,2])

