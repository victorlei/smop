## Copyright (C) 2007-2015 David Bateman
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
## @deftypefn  {Function File} {@var{idx} =} tsearchn (@var{x}, @var{t}, @var{xi})
## @deftypefnx {Function File} {[@var{idx}, @var{p}] =} tsearchn (@var{x}, @var{t}, @var{xi})
## Search for the enclosing Delaunay convex hull.
##
## For @code{@var{t} = delaunayn (@var{x})}, finds the index in @var{t}
## containing the points @var{xi}.  For points outside the convex hull,
## @var{idx} is NaN.
##
## If requested @code{tsearchn} also returns the Barycentric coordinates @var{p}
## of the enclosing triangles.
## @seealso{delaunay, delaunayn}
## @end deftypefn

function [idx, p] = tsearchn (x, t, xi)
  if (nargin != 3)
    print_usage ();
  endif

  nt = rows (t);
  [m, n] = size (x);
  mi = rows (xi);
  idx = NaN (mi, 1);
  p = NaN (mi, n + 1);

  ni = [1:mi].';
  for i = 1 : nt
    ## Only calculate the Barycentric coordinates for points that have not
    ## already been found in a triangle.
    b = cart2bary (x (t (i, :), :), xi(ni,:));

    ## Our points xi are in the current triangle if
    ## (all (b >= 0) && all (b <= 1)). However as we impose that
    ## sum (b,2) == 1 we only need to test all(b>=0). Note need to add
    ## a small margin for rounding errors
    intri = all (b >= -1e-12, 2);
    idx(ni(intri)) = i;
    p(ni(intri),:) = b(intri, :);
    ni(intri) = [];
  endfor
endfunction

function Beta = cart2bary (T, P)
  ## Conversion of Cartesian to Barycentric coordinates.
  ## Given a reference simplex in N dimensions represented by a
  ## (N+1)-by-(N) matrix, and arbitrary point P in cartesion coordinates,
  ## represented by a N-by-1 row vector can be written as
  ##
  ## P = Beta * T
  ##
  ## Where Beta is a N+1 vector of the barycentric coordinates. A criteria
  ## on Beta is that
  ##
  ## sum (Beta) == 1
  ##
  ## and therefore we can write the above as
  ##
  ## P - T(end, :) = Beta(1:end-1) * (T(1:end-1,:) - ones (N,1) * T(end,:))
  ##
  ## and then we can solve for Beta as
  ##
  ## Beta(1:end-1) = (P - T(end,:)) / (T(1:end-1,:) - ones (N,1) * T(end,:))
  ## Beta(end) = sum (Beta)
  ##
  ## Note below is generalize for multiple values of P, one per row.
  [M, N] = size (P);
  Beta = (P - ones (M,1) * T(end,:)) / (T(1:end-1,:) - ones (N,1) * T(end,:));
  Beta (:,end+1) = 1 - sum (Beta, 2);
endfunction


%!shared x, tri
%! x = [-1,-1;-1,1;1,-1];
%! tri = [1, 2, 3];
%!test
%! [idx, p] = tsearchn (x,tri,[-1,-1]);
%! assert (idx, 1);
%! assert (p, [1,0,0], 1e-12);
%!test
%! [idx, p] = tsearchn (x,tri,[-1,1]);
%! assert (idx, 1);
%! assert (p, [0,1,0], 1e-12);
%!test
%! [idx, p] = tsearchn (x,tri,[1,-1]);
%! assert (idx, 1);
%! assert (p, [0,0,1], 1e-12);
%!test
%! [idx, p] = tsearchn (x,tri,[-1/3,-1/3]);
%! assert (idx, 1);
%! assert (p, [1/3,1/3,1/3], 1e-12);
%!test
%! [idx, p] = tsearchn (x,tri,[1,1]);
%! assert (idx, NaN);
%! assert (p, [NaN, NaN, NaN]);

