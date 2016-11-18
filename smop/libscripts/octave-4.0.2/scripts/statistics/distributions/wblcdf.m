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
## @deftypefn  {Function File} {} wblcdf (@var{x})
## @deftypefnx {Function File} {} wblcdf (@var{x}, @var{scale})
## @deftypefnx {Function File} {} wblcdf (@var{x}, @var{scale}, @var{shape})
## Compute the cumulative distribution function (CDF) at @var{x} of the
## Weibull distribution with scale parameter @var{scale} and shape
## parameter @var{shape}.
##
## This is defined as
## @tex
## $$ 1 - e^{-({x \over scale})^{shape}} $$
## for $x \geq 0$.
## @end tex
## @ifnottex
##
## @example
## 1 - exp (-(x/scale)^shape)
## @end example
##
## @noindent
## for @var{x} @geq{} 0.
##
## Default values are @var{scale} = 1, @var{shape} = 1.
## @end ifnottex
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: CDF of the Weibull distribution

function cdf = wblcdf (x, scale = 1, shape = 1)

  if (nargin < 1 || nargin > 3)
    print_usage ();
  endif

  if (! isscalar (shape) || ! isscalar (scale))
    [retval, x, shape, scale] = common_size (x, shape, scale);
    if (retval > 0)
      error ("wblcdf: X, SCALE, and SHAPE must be of common size or scalars");
    endif
  endif

  if (iscomplex (x) || iscomplex (scale) || iscomplex (shape))
    error ("wblcdf: X, SCALE, and SHAPE must not be complex");
  endif

  if (isa (x, "single") || isa (scale, "single") || isa (shape, "single"))
    cdf = NaN (size (x), "single");
  else
    cdf = NaN (size (x));
  endif

  ok = (shape > 0) & (shape < Inf) & (scale > 0) & (scale < Inf);

  k = (x <= 0) & ok;
  cdf(k) = 0;

  k = (x == Inf) & ok;
  cdf(k) = 1;

  k = (x > 0) & (x < Inf) & ok;
  if (isscalar (shape) && isscalar (scale))
    cdf(k) = 1 - exp (- (x(k) / scale) .^ shape);
  else
    cdf(k) = 1 - exp (- (x(k) ./ scale(k)) .^ shape(k));
  endif

endfunction


%!shared x,y
%! x = [-1 0 0.5 1 Inf];
%! y = [0, 1-exp(-x(2:4)), 1];
%!assert (wblcdf (x, ones (1,5), ones (1,5)), y)
%!assert (wblcdf (x, 1, ones (1,5)), y)
%!assert (wblcdf (x, ones (1,5), 1), y)
%!assert (wblcdf (x, [0 1 NaN Inf 1], 1), [NaN 0 NaN NaN 1])
%!assert (wblcdf (x, 1, [0 1 NaN Inf 1]), [NaN 0 NaN NaN 1])
%!assert (wblcdf ([x(1:2) NaN x(4:5)], 1, 1), [y(1:2) NaN y(4:5)])

## Test class of input preserved
%!assert (wblcdf ([x, NaN], 1, 1), [y, NaN])
%!assert (wblcdf (single ([x, NaN]), 1, 1), single ([y, NaN]))
%!assert (wblcdf ([x, NaN], single (1), 1), single ([y, NaN]))
%!assert (wblcdf ([x, NaN], 1, single (1)), single ([y, NaN]))

## Test input validation
%!error wblcdf ()
%!error wblcdf (1,2,3,4)
%!error wblcdf (ones (3), ones (2), ones (2))
%!error wblcdf (ones (2), ones (3), ones (2))
%!error wblcdf (ones (2), ones (2), ones (3))
%!error wblcdf (i, 2, 2)
%!error wblcdf (2, i, 2)
%!error wblcdf (2, 2, i)

