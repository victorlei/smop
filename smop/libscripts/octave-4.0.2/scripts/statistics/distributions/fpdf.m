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
## @deftypefn {Function File} {} fpdf (@var{x}, @var{m}, @var{n})
## For each element of @var{x}, compute the probability density function (PDF)
## at @var{x} of the F distribution with @var{m} and @var{n} degrees of freedom.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: PDF of the F distribution

function pdf = fpdf (x, m, n)

  if (nargin != 3)
    print_usage ();
  endif

  if (! isscalar (m) || ! isscalar (n))
    [retval, x, m, n] = common_size (x, m, n);
    if (retval > 0)
      error ("fpdf: X, M, and N must be of common size or scalars");
    endif
  endif

  if (iscomplex (x) || iscomplex (m) || iscomplex (n))
    error ("fpdf: X, M, and N must not be complex");
  endif

  if (isa (x, "single") || isa (m, "single") || isa (n, "single"))
    pdf = zeros (size (x), "single");
  else
    pdf = zeros (size (x));
  endif

  k = isnan (x) | !(m > 0) | !(m < Inf) | !(n > 0) | !(n < Inf);
  pdf(k) = NaN;

  k = (x > 0) & (x < Inf) & (m > 0) & (m < Inf) & (n > 0) & (n < Inf);
  if (isscalar (m) && isscalar (n))
    tmp = m / n * x(k);
    pdf(k) = (exp ((m/2 - 1) * log (tmp)
                   - ((m + n) / 2) * log (1 + tmp))
              * (m / n) ./ beta (m/2, n/2));
  else
    tmp = m(k) .* x(k) ./ n(k);
    pdf(k) = (exp ((m(k)/2 - 1) .* log (tmp)
                   - ((m(k) + n(k)) / 2) .* log (1 + tmp))
              .* (m(k) ./ n(k)) ./ beta (m(k)/2, n(k)/2));
  endif

endfunction


## F (x, 1, m) == T distribution (sqrt (x), m) / sqrt (x)
%!test
%! x = rand (10,1);
%! x = x(x > 0.1 & x < 0.9);
%! y = tpdf (sqrt (x), 2) ./ sqrt (x);
%! assert (fpdf (x, 1, 2), y, 5*eps);

%!shared x,y
%! x = [-1 0 0.5 1 2];
%! y = [0 0 4/9 1/4 1/9];
%!assert (fpdf (x, 2*ones (1,5), 2*ones (1,5)), y, eps)
%!assert (fpdf (x, 2, 2*ones (1,5)), y, eps)
%!assert (fpdf (x, 2*ones (1,5), 2), y, eps)
%!assert (fpdf (x, [0 NaN Inf 2 2], 2), [NaN NaN NaN y(4:5)], eps)
%!assert (fpdf (x, 2, [0 NaN Inf 2 2]), [NaN NaN NaN y(4:5)], eps)
%!assert (fpdf ([x, NaN], 2, 2), [y, NaN], eps)

## Test class of input preserved
%!assert (fpdf (single ([x, NaN]), 2, 2), single ([y, NaN]), eps ("single"))
%!assert (fpdf ([x, NaN], single (2), 2), single ([y, NaN]), eps ("single"))
%!assert (fpdf ([x, NaN], 2, single (2)), single ([y, NaN]), eps ("single"))

## Test input validation
%!error fpdf ()
%!error fpdf (1)
%!error fpdf (1,2)
%!error fpdf (1,2,3,4)
%!error fpdf (ones (3), ones (2), ones (2))
%!error fpdf (ones (2), ones (3), ones (2))
%!error fpdf (ones (2), ones (2), ones (3))
%!error fpdf (i, 2, 2)
%!error fpdf (2, i, 2)
%!error fpdf (2, 2, i)

