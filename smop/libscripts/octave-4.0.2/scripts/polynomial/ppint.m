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
## @deftypefn  {Function File} {@var{ppi} =} ppint (@var{pp})
## @deftypefnx {Function File} {@var{ppi} =} ppint (@var{pp}, @var{c})
## Compute the integral of the piecewise polynomial struct @var{pp}.
##
## @var{c}, if given, is the constant of integration.
## @seealso{mkpp, ppval, ppder}
## @end deftypefn

function ppi = ppint (pp, c)
  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif
  if (! (isstruct (pp) && strcmp (pp.form, "pp")))
    error ("ppint: PP must be a structure");
  endif

  [x, p, n, k, d] = unmkpp (pp);
  p = reshape (p, [], k);

  ## Get piecewise antiderivatives
  pi = p / diag (k:-1:1);
  k += 1;
  if (nargin == 1)
    pi(:, k) = 0;
  else
    pi(:, k) = repmat (c(:), n, 1);
  endif

  ppi = mkpp (x, pi, d);

  tmp = -cumsum (ppjumps (ppi), length (d) + 1);
  ppi.coefs(prod (d)+1 : end, k) = tmp(:);

endfunction


%!shared x,y,pp,ppi
%! x = 0:8;
%! y = [ ones(size(x)); x+1 ];
%! pp = spline (x, y);
%! ppi = ppint (pp);
%!assert (ppval (ppi, x), [x; 0.5*x.^2 + x], 1e-14)
%!assert (ppi.order, 5)

