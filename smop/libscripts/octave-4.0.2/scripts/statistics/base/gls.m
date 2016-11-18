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
## @deftypefn {Function File} {[@var{beta}, @var{v}, @var{r}] =} gls (@var{y}, @var{x}, @var{o})
## Generalized least squares model.
##
## Perform a generalized least squares estimation for the multivariate model
## @tex
## $y = x b + e$
## with $\bar{e} = 0$ and cov(vec($e$)) = $(s^2)o$,
## @end tex
## @ifnottex
## @w{@math{y = x*b + e}} with @math{mean (e) = 0} and
## @math{cov (vec (e)) = (s^2) o},
## @end ifnottex
## where
## @tex
## $y$ is a $t \times p$ matrix, $x$ is a $t \times k$ matrix, $b$ is a $k
## \times p$ matrix, $e$ is a $t \times p$ matrix, and $o$ is a $tp \times
## tp$ matrix.
## @end tex
## @ifnottex
## @math{y} is a @math{t} by @math{p} matrix, @math{x} is a @math{t} by
## @math{k} matrix, @math{b} is a @math{k} by @math{p} matrix, @math{e}
## is a @math{t} by @math{p} matrix, and @math{o} is a @math{t*p} by
## @math{t*p} matrix.
## @end ifnottex
##
## @noindent
## Each row of @var{y} and @var{x} is an observation and each column a
## variable.  The return values @var{beta}, @var{v}, and @var{r} are
## defined as follows.
##
## @table @var
## @item beta
## The GLS estimator for @math{b}.
##
## @item v
## The GLS estimator for @math{s^2}.
##
## @item r
## The matrix of GLS residuals, @math{r = y - x*beta}.
## @end table
## @seealso{ols}
## @end deftypefn

## Author: Teresa Twaroch <twaroch@ci.tuwien.ac.at>
## Created: May 1993
## Adapted-By: jwe

function [beta, v, r] = gls (y, x, o)

  if (nargin != 3)
    print_usage ();
  endif

  if (! (isnumeric (x) && isnumeric (y) && isnumeric (o)))
    error ("gls: X, Y, and O must be numeric matrices or vectors");
  endif

  if (ndims (x) != 2 || ndims (y) != 2 || ndims (o) != 2)
    error ("gls: X, Y and O must be 2-D matrices or vectors");
  endif

  [rx, cx] = size (x);
  [ry, cy] = size (y);
  [ro, co] = size (o);
  if (rx != ry)
    error ("gls: number of rows of X and Y must be equal");
  endif
  if (! issquare (o) || ro != ry*cy)
    error ("gls: matrix O must be square matrix with rows = rows (Y) * cols (Y)");
  endif

  if (isinteger (x))
    x = double (x);
  endif
  if (isinteger (y))
    y = double (y);
  endif
  if (isinteger (o))
    o = double (o);
  endif

  ## Start of algorithm
  o = o^(-1/2);
  z = kron (eye (cy), x);
  z = o * z;
  y1 = o * reshape (y, ry*cy, 1);
  u = z' * z;
  r = rank (u);

  if (r == cx*cy)
    b = inv (u) * z' * y1;
  else
    b = pinv (z) * y1;
  endif

  beta = reshape (b, cx, cy);

  if (isargout (2) || isargout (3))
    r = y - x * beta;
    if (isargout (2))
      v = (reshape (r, ry*cy, 1))' * (o^2) * reshape (r, ry*cy, 1) / (rx*cy - r);
    endif
  endif

endfunction


%!test
%! x = [1:5]';
%! y = 3*x + 2;
%! x = [x, ones(5,1)];
%! o = diag (ones (5,1));
%! assert (gls (y,x,o), [3; 2], 50*eps);

## Test input validation
%!error gls ()
%!error gls (1)
%!error gls (1, 2)
%!error gls (1, 2, 3, 4)
%!error gls ([true, true], [1, 2], ones (2))
%!error gls ([1, 2], [true, true], ones (2))
%!error gls ([1, 2], [1, 2], true (2))
%!error gls (ones (2,2,2), ones (2,2), ones (4,4))
%!error gls (ones (2,2), ones (2,2,2), ones (4,4))
%!error gls (ones (2,2), ones (2,2), ones (4,4,4))
%!error gls (ones (1,2), ones (2,2), ones (2,2))
%!error gls (ones (2,2), ones (2,2), ones (2,2))

