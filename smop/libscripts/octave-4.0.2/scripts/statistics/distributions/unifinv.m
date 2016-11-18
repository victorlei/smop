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
## @deftypefn  {Function File} {} unifinv (@var{x})
## @deftypefnx {Function File} {} unifinv (@var{x}, @var{a}, @var{b})
## For each element of @var{x}, compute the quantile (the inverse of the CDF)
## at @var{x} of the uniform distribution on the interval [@var{a}, @var{b}].
##
## Default values are @var{a} = 0, @var{b} = 1.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Quantile function of the uniform distribution

function inv = unifinv (x, a = 0, b = 1)

  if (nargin != 1 && nargin != 3)
    print_usage ();
  endif

  if (! isscalar (a) || ! isscalar (b))
    [retval, x, a, b] = common_size (x, a, b);
    if (retval > 0)
      error ("unifinv: X, A, and B must be of common size or scalars");
    endif
  endif

  if (iscomplex (x) || iscomplex (a) || iscomplex (b))
    error ("unifinv: X, A, and B must not be complex");
  endif

  if (isa (x, "single") || isa (a, "single") || isa (b, "single"))
    inv = NaN (size (x), "single");
  else
    inv = NaN (size (x));
  endif

  k = (x >= 0) & (x <= 1) & (a < b);
  if (isscalar (a) && isscalar (b))
    inv(k) = a + x(k) * (b - a);
  else
    inv(k) = a(k) + x(k) .* (b(k) - a(k));
  endif

endfunction


%!shared x
%! x = [-1 0 0.5 1 2];
%!assert (unifinv (x, ones (1,5), 2*ones (1,5)), [NaN 1 1.5 2 NaN])
%!assert (unifinv (x, 1, 2*ones (1,5)), [NaN 1 1.5 2 NaN])
%!assert (unifinv (x, ones (1,5), 2), [NaN 1 1.5 2 NaN])
%!assert (unifinv (x, [1 2 NaN 1 1], 2), [NaN NaN NaN 2 NaN])
%!assert (unifinv (x, 1, 2*[1 0 NaN 1 1]), [NaN NaN NaN 2 NaN])
%!assert (unifinv ([x(1:2) NaN x(4:5)], 1, 2), [NaN 1 NaN 2 NaN])

## Test class of input preserved
%!assert (unifinv ([x, NaN], 1, 2), [NaN 1 1.5 2 NaN NaN])
%!assert (unifinv (single ([x, NaN]), 1, 2), single ([NaN 1 1.5 2 NaN NaN]))
%!assert (unifinv ([x, NaN], single (1), 2), single ([NaN 1 1.5 2 NaN NaN]))
%!assert (unifinv ([x, NaN], 1, single (2)), single ([NaN 1 1.5 2 NaN NaN]))

## Test input validation
%!error unifinv ()
%!error unifinv (1,2)
%!error unifinv (1,2,3,4)
%!error unifinv (ones (3), ones (2), ones (2))
%!error unifinv (ones (2), ones (3), ones (2))
%!error unifinv (ones (2), ones (2), ones (3))
%!error unifinv (i, 2, 2)
%!error unifinv (2, i, 2)
%!error unifinv (2, 2, i)

