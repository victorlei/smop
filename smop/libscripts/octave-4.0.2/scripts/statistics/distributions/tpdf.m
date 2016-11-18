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
## @deftypefn {Function File} {} tpdf (@var{x}, @var{n})
## For each element of @var{x}, compute the probability density function (PDF)
## at @var{x} of the @var{t} (Student) distribution with
## @var{n} degrees of freedom.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: PDF of the t distribution

function pdf = tpdf (x, n)

  if (nargin != 2)
    print_usage ();
  endif

  if (! isscalar (n))
    [retval, x, n] = common_size (x, n);
    if (retval > 0)
      error ("tpdf: X and N must be of common size or scalars");
    endif
  endif

  if (iscomplex (x) || iscomplex (n))
    error ("tpdf: X and N must not be complex");
  endif

  if (isa (x, "single") || isa (n, "single"))
    pdf = zeros (size (x), "single");
  else
    pdf = zeros (size (x));
  endif

  k = isnan (x) | !(n > 0) | !(n < Inf);
  pdf(k) = NaN;

  k = isfinite (x) & (n > 0) & (n < Inf);
  if (isscalar (n))
    pdf(k) = (exp (- (n + 1) * log (1 + x(k) .^ 2 / n)/2)
              / (sqrt (n) * beta (n/2, 1/2)));
  else
    pdf(k) = (exp (- (n(k) + 1) .* log (1 + x(k) .^ 2 ./ n(k))/2)
              ./ (sqrt (n(k)) .* beta (n(k)/2, 1/2)));
  endif

endfunction


%!test
%! x = rand (10,1);
%! y = 1./(pi * (1 + x.^2));
%! assert (tpdf (x, 1), y, 5*eps);

%!shared x,y
%! x = [-Inf 0 0.5 1 Inf];
%! y = 1./(pi * (1 + x.^2));
%!assert (tpdf (x, ones (1,5)), y, eps)
%!assert (tpdf (x, 1), y, eps)
%!assert (tpdf (x, [0 NaN 1 1 1]), [NaN NaN y(3:5)], eps)

## Test class of input preserved
%!assert (tpdf ([x, NaN], 1), [y, NaN], eps)
%!assert (tpdf (single ([x, NaN]), 1), single ([y, NaN]), eps ("single"))
%!assert (tpdf ([x, NaN], single (1)), single ([y, NaN]), eps ("single"))

## Test input validation
%!error tpdf ()
%!error tpdf (1)
%!error tpdf (1,2,3)
%!error tpdf (ones (3), ones (2))
%!error tpdf (ones (2), ones (3))
%!error tpdf (i, 2)
%!error tpdf (2, i)

