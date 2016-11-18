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
## @deftypefn  {Function File} {} unifpdf (@var{x})
## @deftypefnx {Function File} {} unifpdf (@var{x}, @var{a}, @var{b})
## For each element of @var{x}, compute the probability density function (PDF)
## at @var{x} of the uniform distribution on the interval [@var{a}, @var{b}].
##
## Default values are @var{a} = 0, @var{b} = 1.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: PDF of the uniform distribution

function pdf = unifpdf (x, a = 0, b = 1)

  if (nargin != 1 && nargin != 3)
    print_usage ();
  endif

  if (! isscalar (a) || ! isscalar (b))
    [retval, x, a, b] = common_size (x, a, b);
    if (retval > 0)
      error ("unifpdf: X, A, and B must be of common size or scalars");
    endif
  endif

  if (iscomplex (x) || iscomplex (a) || iscomplex (b))
    error ("unifpdf: X, A, and B must not be complex");
  endif

  if (isa (x, "single") || isa (a, "single") || isa (b, "single"))
    pdf = zeros (size (x), "single");
  else
    pdf = zeros (size (x));
  endif

  k = isnan (x) | !(a < b);
  pdf(k) = NaN;

  k = (x >= a) & (x <= b) & (a < b);
  if (isscalar (a) && isscalar (b))
    pdf(k) = 1 / (b - a);
  else
    pdf(k) = 1 ./ (b(k) - a(k));
  endif

endfunction


%!shared x,y
%! x = [-1 0 0.5 1 2] + 1;
%! y = [0 1 1 1 0];
%!assert (unifpdf (x, ones (1,5), 2*ones (1,5)), y)
%!assert (unifpdf (x, 1, 2*ones (1,5)), y)
%!assert (unifpdf (x, ones (1,5), 2), y)
%!assert (unifpdf (x, [2 NaN 1 1 1], 2), [NaN NaN y(3:5)])
%!assert (unifpdf (x, 1, 2*[0 NaN 1 1 1]), [NaN NaN y(3:5)])
%!assert (unifpdf ([x, NaN], 1, 2), [y, NaN])

## Test class of input preserved
%!assert (unifpdf (single ([x, NaN]), 1, 2), single ([y, NaN]))
%!assert (unifpdf (single ([x, NaN]), single (1), 2), single ([y, NaN]))
%!assert (unifpdf ([x, NaN], 1, single (2)), single ([y, NaN]))

## Test input validation
%!error unifpdf ()
%!error unifpdf (1,2)
%!error unifpdf (1,2,3,4)
%!error unifpdf (ones (3), ones (2), ones (2))
%!error unifpdf (ones (2), ones (3), ones (2))
%!error unifpdf (ones (2), ones (2), ones (3))
%!error unifpdf (i, 2, 2)
%!error unifpdf (2, i, 2)
%!error unifpdf (2, 2, i)

