## Copyright (C) 2012 Rik Wehbring
## Copyright (C) 1995-2015 Kurt Hornik
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
## @deftypefn  {Function File} {} cauchy_inv (@var{x})
## @deftypefnx {Function File} {} cauchy_inv (@var{x}, @var{location}, @var{scale})
## For each element of @var{x}, compute the quantile (the inverse of the CDF)
## at @var{x} of the Cauchy distribution with location parameter
## @var{location} and scale parameter @var{scale}.
##
## Default values are @var{location} = 0, @var{scale} = 1.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Quantile function of the Cauchy distribution

function inv = cauchy_inv (x, location = 0, scale = 1)

  if (nargin != 1 && nargin != 3)
    print_usage ();
  endif

  if (! isscalar (location) || ! isscalar (scale))
    [retval, x, location, scale] = common_size (x, location, scale);
    if (retval > 0)
      error ("cauchy_inv: X, LOCATION, and SCALE must be of common size or scalars");
    endif
  endif

  if (iscomplex (x) || iscomplex (location) || iscomplex (scale))
    error ("cauchy_inv: X, LOCATION, and SCALE must not be complex");
  endif

  if (isa (x, "single") || isa (location, "single") || isa (scale, "single"))
    inv = NaN (size (x), "single");
  else
    inv = NaN (size (x));
  endif

  ok = ! isinf (location) & (scale > 0) & (scale < Inf);

  k = (x == 0) & ok;
  inv(k) = -Inf;

  k = (x == 1) & ok;
  inv(k) = Inf;

  k = (x > 0) & (x < 1) & ok;
  if (isscalar (location) && isscalar (scale))
    inv(k) = location - scale * cot (pi * x(k));
  else
    inv(k) = location(k) - scale(k) .* cot (pi * x(k));
  endif

endfunction


%!shared x
%! x = [-1 0 0.5 1 2];
%!assert (cauchy_inv (x, ones (1,5), 2*ones (1,5)), [NaN -Inf 1 Inf NaN], eps)
%!assert (cauchy_inv (x, 1, 2*ones (1,5)), [NaN -Inf 1 Inf NaN], eps)
%!assert (cauchy_inv (x, ones (1,5), 2), [NaN -Inf 1 Inf NaN], eps)
%!assert (cauchy_inv (x, [1 -Inf NaN Inf 1], 2), [NaN NaN NaN NaN NaN])
%!assert (cauchy_inv (x, 1, 2*[1 0 NaN Inf 1]), [NaN NaN NaN NaN NaN])
%!assert (cauchy_inv ([x(1:2) NaN x(4:5)], 1, 2), [NaN -Inf NaN Inf NaN])

## Test class of input preserved
%!assert (cauchy_inv ([x, NaN], 1, 2), [NaN -Inf 1 Inf NaN NaN], eps)
%!assert (cauchy_inv (single ([x, NaN]), 1, 2), single ([NaN -Inf 1 Inf NaN NaN]), eps ("single"))
%!assert (cauchy_inv ([x, NaN], single (1), 2), single ([NaN -Inf 1 Inf NaN NaN]), eps ("single"))
%!assert (cauchy_inv ([x, NaN], 1, single (2)), single ([NaN -Inf 1 Inf NaN NaN]), eps ("single"))

## Test input validation
%!error cauchy_inv ()
%!error cauchy_inv (1,2)
%!error cauchy_inv (1,2,3,4)
%!error cauchy_inv (ones (3), ones (2), ones (2))
%!error cauchy_inv (ones (2), ones (3), ones (2))
%!error cauchy_inv (ones (2), ones (2), ones (3))
%!error cauchy_inv (i, 2, 2)
%!error cauchy_inv (2, i, 2)
%!error cauchy_inv (2, 2, i)

