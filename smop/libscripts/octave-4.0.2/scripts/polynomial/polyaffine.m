## Copyright (C) 2009-2015 Tony Richardson, Jaroslav Hajek
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
## @deftypefn {Function File} {} polyaffine (@var{f}, @var{mu})
## Return the coefficients of the polynomial vector @var{f} after an affine
## transformation.
##
## If @var{f} is the vector representing the polynomial f(x), then
## @code{@var{g} = polyaffine (@var{f}, @var{mu})} is the vector representing:
##
## @example
## g(x) = f( (x - @var{mu}(1)) / @var{mu}(2) )
## @end example
##
## @seealso{polyval, polyfit}
## @end deftypefn

function g = polyaffine (f, mu)

   if (nargin != 2)
      print_usage ();
   endif

   if (! isvector (f))
      error ("polyaffine: F must be a vector");
   endif

   if (! isvector (mu) || length (mu) != 2)
      error ("polyaffine: MU must be a two-element vector");
   endif

   lf = length (f);

   ## Ensure that f is a row vector
   if (rows (f) > 1)
      f = f.';
   endif

   g = f;

   ## Scale.
   if (mu(2) != 1)
     g = g ./ (mu(2) .^ (lf-1:-1:0));
   endif

   ## Translate.
   if (mu(1) != 0)
     w = (-mu(1)) .^ (0:lf-1);
     ii = lf:-1:1;
     g = g(ii) * (toeplitz (w) .* pascal (lf, -1));
     g = g(ii);
   endif

endfunction


%!demo
%! f = [1/5 4/5 -7/5 -2];
%! g = polyaffine (f, [1, 1.2]);
%! x = linspace (-4,4,100);
%! plot (x,polyval (f, x), x,polyval (g, x));
%! legend ("original", "affine");
%! axis ([-4 4 -3 5]);
%! grid on;

%!test
%! f = [1/5 4/5 -7/5 -2];
%! mu = [1, 1.2];
%! g = polyaffine (f, mu);
%! x = linspace (-4,4,100);
%! assert (polyval (f, x, [], mu), polyval (g, x), 1e-10);

