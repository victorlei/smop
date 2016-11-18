## Copyright (C) 1996-2015 John W. Eaton
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
## @deftypefn {Function File} {} mahalanobis (@var{x}, @var{y})
## Return the Mahalanobis' D-square distance between the multivariate
## samples @var{x} and @var{y}.
##
## The data @var{x} and @var{y} must have the same number of components
## (columns), but may have a different number of observations (rows).
## @end deftypefn

## Author: Friedrich Leisch <leisch@ci.tuwien.ac.at>
## Created: July 1993
## Adapted-By: jwe

function retval = mahalanobis (x, y)

  if (nargin != 2)
    print_usage ();
  endif

  if (   ! (isnumeric (x) || islogical (x))
      || ! (isnumeric (y) || islogical (y)))
    error ("mahalanobis: X and Y must be numeric matrices or vectors");
  endif

  if (ndims (x) != 2 || ndims (y) != 2)
    error ("mahalanobis: X and Y must be 2-D matrices or vectors");
  endif

  [xr, xc] = size (x);
  [yr, yc] = size (y);

  if (xc != yc)
    error ("mahalanobis: X and Y must have the same number of columns");
  endif

  if (isinteger (x))
    x = double (x);
  endif

  xm = mean (x);
  ym = mean (y);

  ## Center data by subtracting means
  x = bsxfun (@minus, x, xm);
  y = bsxfun (@minus, y, ym);

  w = (x' * x + y' * y) / (xr + yr - 2);

  winv = inv (w);

  retval = (xm - ym) * winv * (xm - ym)';

endfunction


## Test input validation
%!error mahalanobis ()
%!error mahalanobis (1, 2, 3)
%!error mahalanobis ('A', 'B')
%!error mahalanobis ([1, 2], ['A', 'B'])
%!error mahalanobis (ones (2,2,2))
%!error mahalanobis (ones (2,2), ones (2,2,2))
%!error mahalanobis (ones (2,2), ones (2,3))

