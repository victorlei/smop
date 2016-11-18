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
## @deftypefn  {Function File} {} wblinv (@var{x})
## @deftypefnx {Function File} {} wblinv (@var{x}, @var{scale})
## @deftypefnx {Function File} {} wblinv (@var{x}, @var{scale}, @var{shape})
## Compute the quantile (the inverse of the CDF) at @var{x} of the
## Weibull distribution with scale parameter @var{scale} and
## shape parameter @var{shape}.
##
## Default values are @var{scale} = 1, @var{shape} = 1.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Quantile function of the Weibull distribution

function inv = wblinv (x, scale = 1, shape = 1)

  if (nargin < 1 || nargin > 3)
    print_usage ();
  endif

  if (! isscalar (scale) || ! isscalar (shape))
    [retval, x, scale, shape] = common_size (x, scale, shape);
    if (retval > 0)
      error ("wblinv: X, SCALE, and SHAPE must be of common size or scalars");
    endif
  endif

  if (iscomplex (x) || iscomplex (scale) || iscomplex (shape))
    error ("wblinv: X, SCALE, and SHAPE must not be complex");
  endif

  if (isa (x, "single") || isa (scale, "single") || isa (shape, "single"))
    inv = NaN (size (x), "single");
  else
    inv = NaN (size (x));
  endif

  ok = (scale > 0) & (scale < Inf) & (shape > 0) & (shape < Inf);

  k = (x == 0) & ok;
  inv(k) = 0;

  k = (x == 1) & ok;
  inv(k) = Inf;

  k = (x > 0) & (x < 1) & ok;
  if (isscalar (scale) && isscalar (shape))
    inv(k) = scale * (- log (1 - x(k))) .^ (1 / shape);
  else
    inv(k) = scale(k) .* (- log (1 - x(k))) .^ (1 ./ shape(k));
  endif

endfunction


%!shared x
%! x = [-1 0 0.63212055882855778 1 2];
%!assert (wblinv (x, ones (1,5), ones (1,5)), [NaN 0 1 Inf NaN], eps)
%!assert (wblinv (x, 1, ones (1,5)), [NaN 0 1 Inf NaN], eps)
%!assert (wblinv (x, ones (1,5), 1), [NaN 0 1 Inf NaN], eps)
%!assert (wblinv (x, [1 -1 NaN Inf 1], 1), [NaN NaN NaN NaN NaN])
%!assert (wblinv (x, 1, [1 -1 NaN Inf 1]), [NaN NaN NaN NaN NaN])
%!assert (wblinv ([x(1:2) NaN x(4:5)], 1, 1), [NaN 0 NaN Inf NaN])

## Test class of input preserved
%!assert (wblinv ([x, NaN], 1, 1), [NaN 0 1 Inf NaN NaN], eps)
%!assert (wblinv (single ([x, NaN]), 1, 1), single ([NaN 0 1 Inf NaN NaN]), eps ("single"))
%!assert (wblinv ([x, NaN], single (1), 1), single ([NaN 0 1 Inf NaN NaN]), eps ("single"))
%!assert (wblinv ([x, NaN], 1, single (1)), single ([NaN 0 1 Inf NaN NaN]), eps ("single"))

## Test input validation
%!error wblinv ()
%!error wblinv (1,2,3,4)
%!error wblinv (ones (3), ones (2), ones (2))
%!error wblinv (ones (2), ones (3), ones (2))
%!error wblinv (ones (2), ones (2), ones (3))
%!error wblinv (i, 2, 2)
%!error wblinv (2, i, 2)
%!error wblinv (2, 2, i)

