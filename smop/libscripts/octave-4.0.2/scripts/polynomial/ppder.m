## Copyright (C) 2008-2015 VZLU Prague, a.s., Czech Republic
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn  {Function File} {ppd =} ppder (pp)
## @deftypefnx {Function File} {ppd =} ppder (pp, m)
## Compute the piecewise @var{m}-th derivative of a piecewise polynomial
## struct @var{pp}.
##
## If @var{m} is omitted the first derivative is calculated.
## @seealso{mkpp, ppval, ppint}
## @end deftypefn

function ppd = ppder (pp, m = 1)

  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif

  if (! (isstruct (pp) && strcmp (pp.form, "pp")))
    error ("ppder: PP must be a structure");
  endif

  [x, p, n, k, d] = unmkpp (pp);

  if (k - m <= 0)
    x = [x(1) x(end)];
    pd = zeros (prod (d), 1);
  else
    f = k : -1 : 1;
    ff = bincoeff (f, m + 1) .* factorial (m + 1) ./ f;
    k -= m;
    pd = p(:,1:k) * diag (ff(1:k));
  endif

  ppd = mkpp (x, pd, d);
endfunction


%!shared x,y,pp,ppd
%! x = 0:8;
%! y = [x.^2; x.^3+1];
%! pp = spline (x, y);
%! ppd = ppder (pp);
%!assert (ppval (ppd, x), [2*x; 3*x.^2], 1e-14)
%!assert (ppd.order, 3)
%! ppd = ppder (pp, 2);
%!assert (ppval (ppd, x), [2*ones(size (x)); 6*x], 1e-14)
%!assert (ppd.order, 2)
%! ppd = ppder (pp, 3);
%!assert (ppd.order, 1)
%!assert (ppd.pieces, 8)
%!assert (size (ppd.coefs), [16, 1])
%! ppd = ppder (pp, 4);
%!assert (ppd.order, 1)
%!assert (ppd.pieces, 1)
%!assert (size (ppd.coefs), [2, 1])
%!assert (ppval (ppd,x), zeros (size (y)), 1e-14)

