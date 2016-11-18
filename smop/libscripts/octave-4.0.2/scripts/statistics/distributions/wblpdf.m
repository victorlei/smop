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
## @deftypefn  {Function File} {} wblpdf (@var{x})
## @deftypefnx {Function File} {} wblpdf (@var{x}, @var{scale})
## @deftypefnx {Function File} {} wblpdf (@var{x}, @var{scale}, @var{shape})
## Compute the probability density function (PDF) at @var{x} of the
## Weibull distribution with scale parameter @var{scale} and
## shape parameter @var{shape}.
##
## This is given by
## @tex
## $$  {shape \over scale^{shape}} \cdot x^{shape-1} \cdot e^{-({x \over scale})^{shape}} $$
## @end tex
## @ifnottex
##
## @example
## shape * scale^(-shape) * x^(shape-1) * exp (-(x/scale)^shape)
## @end example
##
## @end ifnottex
## @noindent
## for @var{x} @geq{} 0.
##
## Default values are @var{scale} = 1, @var{shape} = 1.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: PDF of the Weibull distribution

function pdf = wblpdf (x, scale = 1, shape = 1)

  if (nargin < 1 || nargin > 3)
    print_usage ();
  endif

  if (! isscalar (scale) || ! isscalar (shape))
    [retval, x, scale, shape] = common_size (x, scale, shape);
    if (retval > 0)
      error ("wblpdf: X, SCALE, and SHAPE must be of common size or scalars");
    endif
  endif

  if (iscomplex (x) || iscomplex (scale) || iscomplex (shape))
    error ("wblpdf: X, SCALE, and SHAPE must not be complex");
  endif

  if (isa (x, "single") || isa (scale, "single") || isa (shape, "single"))
    pdf = NaN (size (x), "single");
  else
    pdf = NaN (size (x));
  endif

  ok = ((scale > 0) & (scale < Inf) & (shape > 0) & (shape < Inf));

  k = (x < 0) & ok;
  pdf(k) = 0;

  k = (x >= 0) & (x < Inf) & ok;
  if (isscalar (scale) && isscalar (shape))
    pdf(k) = (shape * (scale .^ -shape)
              .* (x(k) .^ (shape - 1))
              .* exp (- (x(k) / scale) .^ shape));
  else
    pdf(k) = (shape(k) .* (scale(k) .^ -shape(k))
              .* (x(k) .^ (shape(k) - 1))
              .* exp (- (x(k) ./ scale(k)) .^ shape(k)));
  endif

endfunction


%!shared x,y
%! x = [-1 0 0.5 1 Inf];
%! y = [0, exp(-x(2:4)), NaN];
%!assert (wblpdf (x, ones (1,5), ones (1,5)), y)
%!assert (wblpdf (x, 1, ones (1,5)), y)
%!assert (wblpdf (x, ones (1,5), 1), y)
%!assert (wblpdf (x, [0 NaN Inf 1 1], 1), [NaN NaN NaN y(4:5)])
%!assert (wblpdf (x, 1, [0 NaN Inf 1 1]), [NaN NaN NaN y(4:5)])
%!assert (wblpdf ([x, NaN], 1, 1), [y, NaN])

## Test class of input preserved
%!assert (wblpdf (single ([x, NaN]), 1, 1), single ([y, NaN]))
%!assert (wblpdf ([x, NaN], single (1), 1), single ([y, NaN]))
%!assert (wblpdf ([x, NaN], 1, single (1)), single ([y, NaN]))

## Test input validation
%!error wblpdf ()
%!error wblpdf (1,2,3,4)
%!error wblpdf (ones (3), ones (2), ones (2))
%!error wblpdf (ones (2), ones (3), ones (2))
%!error wblpdf (ones (2), ones (2), ones (3))
%!error wblpdf (i, 2, 2)
%!error wblpdf (2, i, 2)
%!error wblpdf (2, 2, i)

