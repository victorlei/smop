## Copyright (C) 2008-2015 VZLU Prague, a.s., Czech Republic
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or
## (at your option) any later version.
##
## Octave is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn {Function File} {@var{jumps} =} ppjumps (@var{pp})
## Evaluate the boundary jumps of a piecewise polynomial.
##
## If there are @math{n} intervals, and the dimensionality of @var{pp} is
## @math{d}, the resulting array has dimensions @code{[d, n-1]}.
## @seealso{mkpp}
## @end deftypefn

function jumps = ppjumps (pp)
  if (nargin != 1)
    print_usage ();
  endif

  if (! (isstruct (pp) && strcmp (pp.form, "pp")))
    error ("ppjumps: PP must be a structure");
  endif

  ## Extract info.
  [x, P, n, k, d] = unmkpp (pp);
  nd = length (d) + 1;

  ## Offsets.
  dx = diff (x(1:n));
  dx = repmat (dx, [prod(d), 1]);
  dx = reshape (dx, [d, n-1]);
  dx = shiftdim (dx, nd - 1);

  ## Use Horner scheme.
  if (k>1)
    llim = shiftdim (reshape (P(1:(n-1) * prod (d), 1), [d, n-1]), nd - 1);
  endif

  for i = 2 : k;
    llim .*= dx;
    llim += shiftdim (reshape (P(1:(n-1) * prod (d), i), [d, n-1]), nd - 1);
  endfor

  rlim = shiftdim (ppval (pp, x(2:end-1)), nd - 1);
  jumps = shiftdim (rlim - llim, 1);
endfunction


%!test
%! p = [1 6 11 6];
%! x = linspace (5, 6, 4);
%! y = polyval (p, x);
%! pp = spline (x, y);
%! jj = ppjumps (pp);
%! assert (jj, [0 0], eps);

%!test
%! breaks = [0 1 2];
%! pp1 = poly (-[1 2 3]);
%! pp2 = poly (-([1 2 3]+1));
%! pp = mkpp (breaks, [pp1;pp2]);
%! assert (ppjumps (pp), 0, eps);

%!test
%! breaks = [0 1 2];
%! pp1 = poly (-[1 2 3]);
%! pp2 = poly (([1 2 3]+1));
%! pp = mkpp (breaks, [pp1;pp2]);
%! j  = - 2 * polyval (pp1, 1);
%! assert (ppjumps (pp), j, eps);

