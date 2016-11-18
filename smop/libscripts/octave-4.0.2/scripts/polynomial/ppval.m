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
## @deftypefn {Function File} {@var{yi} =} ppval (@var{pp}, @var{xi})
## Evaluate the piecewise polynomial structure @var{pp} at the points @var{xi}.
##
## If @var{pp} describes a scalar polynomial function, the result is an array
## of the same shape as @var{xi}.  Otherwise, the size of the result is
## @code{[pp.dim, length(@var{xi})]} if @var{xi} is a vector, or
## @code{[pp.dim, size(@var{xi})]} if it is a multi-dimensional array.
## @seealso{mkpp, unmkpp, spline, pchip}
## @end deftypefn

function yi = ppval (pp, xi)

  if (nargin != 2)
    print_usage ();
  endif
  if (! (isstruct (pp) && isfield (pp, "form") && strcmp (pp.form, "pp")))
    error ("ppval: first argument must be a pp-form structure");
  endif

  ## Extract info.
  [x, P, n, k, d] = unmkpp (pp);

  ## dimension checks
  sxi = size (xi);
  if (isvector (xi))
    xi = xi(:).';
  endif

  nd = length (d);

  ## Determine intervals.
  xn = numel (xi);
  idx = lookup (x, xi, "lr");

  P = reshape (P, [d, n * k]);
  P = shiftdim (P, nd);
  P = reshape (P, [n, k, d]);
  Pidx = P(idx(:), :);  # 2D matrix size: x = coefs*prod(d), y = prod(sxi)

  if (isvector (xi))
    Pidx = reshape (Pidx, [xn, k, d]);
    Pidx = shiftdim (Pidx, 1);
    dimvec = [d, xn];
  else
    Pidx = reshape (Pidx, [sxi, k, d]);
    Pidx = shiftdim (Pidx, length (sxi));
    dimvec = [d, sxi];
  endif
  ndv = length (dimvec);

  ## Offsets.
  dx = (xi - x(idx))(:)';
  dx = repmat (dx, [prod(d), 1]);
  dx = reshape (dx, dimvec);
  dx = shiftdim (dx, ndv - 1);

  ## Use Horner scheme.
  if (k > 1)
    yi = shiftdim (reshape (Pidx(1,:), dimvec), ndv - 1);
  else
    yi = shiftdim (reshape (Pidx, dimvec), ndv - 1);
  endif

  for i = 2 : k;
    yi .*= dx;
    yi += shiftdim (reshape (Pidx(i,:), dimvec), ndv - 1);
  endfor

  ## Adjust shape.
  if ((numel (xi) > 1) || (length (d) == 1))
    yi = reshape (shiftdim (yi, 1), dimvec);
  endif

  if (isvector (xi) && (d == 1))
    yi = reshape (yi, sxi);
  elseif (isfield (pp, "orient") && strcmp (pp.orient, "first"))
    yi = shiftdim (yi, nd);
  endif

  if (d == 1)
    yi = reshape (yi, sxi);
  endif

endfunction


%!shared b, c, pp, pp2, xi, abserr
%! b = 1:3;
%! c = ones (2);
%! pp = mkpp (b, c);
%! abserr = 1e-14;
%! pp2 = mkpp (b, [c;c], 2);
%! xi = [1.1 1.3 1.9 2.1];
%!
%!assert (ppval (pp, 1.1), 1.1, abserr)
%!assert (ppval (pp, 2.1), 1.1, abserr)
%!assert (ppval (pp, xi), [1.1 1.3 1.9 1.1], abserr)
%!assert (ppval (pp, xi.'), [1.1 1.3 1.9 1.1].', abserr)
%!assert (ppval (pp2, 1.1), [1.1;1.1], abserr)
%!assert (ppval (pp2, 2.1), [1.1;1.1], abserr)
%!assert (ppval (pp2, xi), [1.1 1.3 1.9 1.1;1.1 1.3 1.9 1.1], abserr)
%!assert (ppval (pp2, xi'), [1.1 1.3 1.9 1.1;1.1 1.3 1.9 1.1], abserr)
%!assert (size (ppval (pp2, [xi;xi])), [2 2 4])
%!assert (ppval (mkpp([0 1],1), magic (3)), ones(3,3))
%!
%!test
%! breaks = [0, 1, 2, 3];
%! coefs = rand (6, 4);
%! pp = mkpp (breaks, coefs, 2);
%! ret = zeros (2, 4, 2);
%! ret(:,:,1) = ppval (pp, breaks');
%! ret(:,:,2) = ppval (pp, breaks');
%! assert (ppval (pp, [breaks',breaks']), ret)

## Test input validation
%!error ppval ()
%!error ppval (1)
%!error ppval (1,2,3)
%!error <argument must be a pp-form structure> ppval (1,2)
%!error <argument must be a pp-form structure> ppval (struct ("a", 1), 2)
%!error <argument must be a pp-form structure> ppval (struct ("form", "ab"), 2)

