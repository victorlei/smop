## Copyright (C) 2008-2015 Ben Abbott
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
## @deftypefn  {Function File} {@var{q} =} prctile (@var{x})
## @deftypefnx {Function File} {@var{q} =} prctile (@var{x}, @var{p})
## @deftypefnx {Function File} {@var{q} =} prctile (@var{x}, @var{p}, @var{dim})
## For a sample @var{x}, compute the quantiles, @var{q}, corresponding
## to the cumulative probability values, @var{p}, in percent.
##
## If @var{x} is a matrix, compute the percentiles for each column and return
## them in a matrix, such that the i-th row of @var{y} contains the
## @var{p}(i)th percentiles of each column of @var{x}.
##
## If @var{p} is unspecified, return the quantiles for @code{[0 25 50 75 100]}.
##
## The optional argument @var{dim} determines the dimension along which the
## percentiles are calculated.  If @var{dim} is omitted it defaults to the
## first non-singleton dimension.
##
## Programming Note: All non-numeric values (NaNs) of @var{x} are ignored.
## @seealso{quantile}
## @end deftypefn

## Author: Ben Abbott <bpabbott@mac.com>
## Description: Matlab style prctile function.

function q = prctile (x, p = [], dim)

  if (nargin < 1 || nargin > 3)
    print_usage ();
  endif

  if (! (isnumeric (x) || islogical (x)))
    error ("prctile: X must be a numeric vector or matrix");
  endif

  if (isempty (p))
    p = [0, 25, 50, 75, 100];
  endif

  if (! (isnumeric (p) && isvector (p)))
    error ("prctile: P must be a numeric vector");
  endif

  nd = ndims (x);
  sz = size (x);
  if (nargin < 3)
    ## Find the first non-singleton dimension.
    (dim = find (sz > 1, 1)) || (dim = 1);
  else
    if (!(isscalar (dim) && dim == fix (dim))
        || !(1 <= dim && dim <= nd))
      error ("prctile: DIM must be an integer and a valid dimension");
    endif
  endif

  ## Convert from percent to decimal.
  p /= 100;

  q = quantile (x, p, dim);

endfunction


%!test
%! pct = 50;
%! q = prctile (1:4, pct);
%! qa = 2.5;
%! assert (q, qa);
%! q = prctile (1:4, pct, 1);
%! qa = [1, 2, 3, 4];
%! assert (q, qa);
%! q = prctile (1:4, pct, 2);
%! qa = 2.5;
%! assert (q, qa);

%!test
%! pct = [50 75];
%! q = prctile (1:4, pct);
%! qa = [2.5 3.5];
%! assert (q, qa);
%! q = prctile (1:4, pct, 1);
%! qa = [1, 2, 3, 4; 1, 2, 3, 4];
%! assert (q, qa);
%! q = prctile (1:4, pct, 2);
%! qa = [2.5 3.5];
%! assert (q, qa);

%!test
%! pct = 50;
%! x = [0.1126, 0.1148, 0.0521, 0.2364, 0.1393
%!      0.1718, 0.7273, 0.2041, 0.4531, 0.1585
%!      0.2795, 0.7978, 0.3296, 0.5567, 0.7307
%!      0.4288, 0.8753, 0.6477, 0.6287, 0.8165
%!      0.9331, 0.9312, 0.9635, 0.7796, 0.8461];
%! tol = 0.0001;
%! q = prctile (x, pct, 1);
%! qa = [0.2795, 0.7978, 0.3296, 0.5567, 0.7307];
%! assert (q, qa, tol);
%! q = prctile (x, pct, 2);
%! qa = [0.1148; 0.2041; 0.5567; 0.6477; 0.9312];
%! assert (q, qa, tol);

%!test
%! pct = 50;
%! tol = 0.0001;
%! x = [0.1126, 0.1148, 0.0521, 0.2364, 0.1393
%!      0.1718, 0.7273, 0.2041, 0.4531, 0.1585
%!      0.2795, 0.7978, 0.3296, 0.5567, 0.7307
%!      0.4288, 0.8753, 0.6477, 0.6287, 0.8165
%!      0.9331, 0.9312, 0.9635, 0.7796, 0.8461];
%! x(5,5) = Inf;
%! q = prctile (x, pct, 1);
%! qa = [0.2795, 0.7978, 0.3296, 0.5567, 0.7307];
%! assert (q, qa, tol);
%! x(5,5) = -Inf;
%! q = prctile (x, pct, 1);
%! qa = [0.2795, 0.7978, 0.3296, 0.5567, 0.1585];
%! assert (q, qa, tol);
%! x(1,1) = Inf;
%! q = prctile (x, pct, 1);
%! qa = [0.4288, 0.7978, 0.3296, 0.5567, 0.1585];
%! assert (q, qa, tol);

%!test
%! pct = 50;
%! tol = 0.0001;
%! x = [0.1126, 0.1148, 0.0521, 0.2364, 0.1393
%!      0.1718, 0.7273, 0.2041, 0.4531, 0.1585
%!      0.2795, 0.7978, 0.3296, 0.5567, 0.7307
%!      0.4288, 0.8753, 0.6477, 0.6287, 0.8165
%!      0.9331, 0.9312, 0.9635, 0.7796, 0.8461];
%! x(3,3) = Inf;
%! q = prctile (x, pct, 1);
%! qa = [0.2795, 0.7978, 0.6477, 0.5567, 0.7307];
%! assert (q, qa, tol);
%! q = prctile (x, pct, 2);
%! qa = [0.1148; 0.2041; 0.7307; 0.6477; 0.9312];
%! assert (q, qa, tol);

%!test
%! pct = 50;
%! tol = 0.0001;
%! x = [0.1126, 0.1148, 0.0521, 0.2364, 0.1393
%!      0.1718, 0.7273, 0.2041, 0.4531, 0.1585
%!      0.2795, 0.7978, 0.3296, 0.5567, 0.7307
%!      0.4288, 0.8753, 0.6477, 0.6287, 0.8165
%!      0.9331, 0.9312, 0.9635, 0.7796, 0.8461];
%! x(5,5) = NaN;
%! q = prctile (x, pct, 2);
%! qa = [0.1148; 0.2041; 0.5567; 0.6477; 0.9322];
%! assert (q, qa, tol);
%! x(1,1) = NaN;
%! q = prctile (x, pct, 2);
%! qa = [0.1270; 0.2041; 0.5567; 0.6477; 0.9322];
%! assert (q, qa, tol);
%! x(3,3) = NaN;
%! q = prctile (x, pct, 2);
%! qa = [0.1270; 0.2041; 0.6437; 0.6477; 0.9322];
%! assert (q, qa, tol);

## Test input validation
%!error prctile ()
%!error prctile (1, 2, 3, 4)
%!error prctile (['A'; 'B'], 10)
%!error prctile (1:10, [true, false])
%!error prctile (1:10, ones (2,2))
%!error prctile (1, 1, 1.5)
%!error prctile (1, 1, 0)
%!error prctile (1, 1, 3)

