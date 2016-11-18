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
## @deftypefn {Function File} {} finv (@var{x}, @var{m}, @var{n})
## For each element of @var{x}, compute the quantile (the inverse of the CDF)
## at @var{x} of the F distribution with @var{m} and @var{n} degrees of freedom.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Quantile function of the F distribution

function inv = finv (x, m, n)

  if (nargin != 3)
    print_usage ();
  endif

  if (! isscalar (m) || ! isscalar (n))
    [retval, x, m, n] = common_size (x, m, n);
    if (retval > 0)
      error ("finv: X, M, and N must be of common size or scalars");
    endif
  endif

  if (iscomplex (x) || iscomplex (m) || iscomplex (n))
    error ("finv: X, M, and N must not be complex");
  endif

  if (isa (x, "single") || isa (m, "single") || isa (n, "single"))
    inv = NaN (size (x), "single");
  else
    inv = NaN (size (x));
  endif

  k = (x == 1) & (m > 0) & (m < Inf) & (n > 0) & (n < Inf);
  inv(k) = Inf;

  k = (x >= 0) & (x < 1) & (m > 0) & (m < Inf) & (n > 0) & (n < Inf);
  if (isscalar (m) && isscalar (n))
    inv(k) = ((1 ./ betainv (1 - x(k), n/2, m/2) - 1) * n / m);
  else
    inv(k) = ((1 ./ betainv (1 - x(k), n(k)/2, m(k)/2) - 1)
              .* n(k) ./ m(k));
  endif

endfunction


%!shared x
%! x = [-1 0 0.5 1 2];
%!assert (finv (x, 2*ones (1,5), 2*ones (1,5)), [NaN 0 1 Inf NaN])
%!assert (finv (x, 2, 2*ones (1,5)), [NaN 0 1 Inf NaN])
%!assert (finv (x, 2*ones (1,5), 2), [NaN 0 1 Inf NaN])
%!assert (finv (x, [2 -Inf NaN Inf 2], 2), [NaN NaN NaN NaN NaN])
%!assert (finv (x, 2, [2 -Inf NaN Inf 2]), [NaN NaN NaN NaN NaN])
%!assert (finv ([x(1:2) NaN x(4:5)], 2, 2), [NaN 0 NaN Inf NaN])

## Test class of input preserved
%!assert (finv ([x, NaN], 2, 2), [NaN 0 1 Inf NaN NaN])
%!assert (finv (single ([x, NaN]), 2, 2), single ([NaN 0 1 Inf NaN NaN]))
%!assert (finv ([x, NaN], single (2), 2), single ([NaN 0 1 Inf NaN NaN]))
%!assert (finv ([x, NaN], 2, single (2)), single ([NaN 0 1 Inf NaN NaN]))

## Test input validation
%!error finv ()
%!error finv (1)
%!error finv (1,2)
%!error finv (1,2,3,4)
%!error finv (ones (3), ones (2), ones (2))
%!error finv (ones (2), ones (3), ones (2))
%!error finv (ones (2), ones (2), ones (3))
%!error finv (i, 2, 2)
%!error finv (2, i, 2)
%!error finv (2, 2, i)

