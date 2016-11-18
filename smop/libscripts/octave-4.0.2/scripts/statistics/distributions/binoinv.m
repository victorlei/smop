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
## @deftypefn {Function File} {} binoinv (@var{x}, @var{n}, @var{p})
## For each element of @var{x}, compute the quantile (the inverse of the CDF)
## at @var{x} of the binomial distribution with parameters
## @var{n} and @var{p}, where @var{n} is the number of trials and
## @var{p} is the probability of success.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Quantile function of the binomial distribution

function inv = binoinv (x, n, p)

  if (nargin != 3)
    print_usage ();
  endif

  if (! isscalar (n) || ! isscalar (p))
    [retval, x, n, p] = common_size (x, n, p);
    if (retval > 0)
      error ("binoinv: X, N, and P must be of common size or scalars");
    endif
  endif

  if (iscomplex (x) || iscomplex (n) || iscomplex (p))
    error ("binoinv: X, N, and P must not be complex");
  endif

  if (isa (x, "single") || isa (n, "single") || isa (p, "single"));
    inv = zeros (size (x), "single");
  else
    inv = zeros (size (x));
  endif

  k = (!(x >= 0) | !(x <= 1) | !(n >= 0) | (n != fix (n)) |
       !(p >= 0) | !(p <= 1));
  inv(k) = NaN;

  k = find ((x >= 0) & (x <= 1) & (n >= 0) & (n == fix (n)
             & (p >= 0) & (p <= 1)));
  if (any (k))
    if (isscalar (n) && isscalar (p))
      cdf = binopdf (0, n, p) * ones (size (k));
      while (any (inv(k) < n))
        m = find (cdf < x(k));
        if (any (m))
          inv(k(m)) = inv(k(m)) + 1;
          cdf(m) = cdf(m) + binopdf (inv(k(m)), n, p);
        else
          break;
        endif
      endwhile
    else
      cdf = binopdf (0, n(k), p(k));
      while (any (inv(k) < n(k)))
        m = find (cdf < x(k));
        if (any (m))
          inv(k(m)) = inv(k(m)) + 1;
          cdf(m) = cdf(m) + binopdf (inv(k(m)), n(k(m)), p(k(m)));
        else
          break;
        endif
      endwhile
    endif
  endif

endfunction


%!shared x
%! x = [-1 0 0.5 1 2];
%!assert (binoinv (x, 2*ones (1,5), 0.5*ones (1,5)), [NaN 0 1 2 NaN])
%!assert (binoinv (x, 2, 0.5*ones (1,5)), [NaN 0 1 2 NaN])
%!assert (binoinv (x, 2*ones (1,5), 0.5), [NaN 0 1 2 NaN])
%!assert (binoinv (x, 2*[0 -1 NaN 1.1 1], 0.5), [NaN NaN NaN NaN NaN])
%!assert (binoinv (x, 2, 0.5*[0 -1 NaN 3 1]), [NaN NaN NaN NaN NaN])
%!assert (binoinv ([x(1:2) NaN x(4:5)], 2, 0.5), [NaN 0 NaN 2 NaN])

## Test class of input preserved
%!assert (binoinv ([x, NaN], 2, 0.5), [NaN 0 1 2 NaN NaN])
%!assert (binoinv (single ([x, NaN]), 2, 0.5), single ([NaN 0 1 2 NaN NaN]))
%!assert (binoinv ([x, NaN], single (2), 0.5), single ([NaN 0 1 2 NaN NaN]))
%!assert (binoinv ([x, NaN], 2, single (0.5)), single ([NaN 0 1 2 NaN NaN]))

## Test input validation
%!error binoinv ()
%!error binoinv (1)
%!error binoinv (1,2)
%!error binoinv (1,2,3,4)
%!error binoinv (ones (3), ones (2), ones (2))
%!error binoinv (ones (2), ones (3), ones (2))
%!error binoinv (ones (2), ones (2), ones (3))
%!error binoinv (i, 2, 2)
%!error binoinv (2, i, 2)
%!error binoinv (2, 2, i)

