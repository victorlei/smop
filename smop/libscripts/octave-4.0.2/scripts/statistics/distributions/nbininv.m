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
## @deftypefn {Function File} {} nbininv (@var{x}, @var{n}, @var{p})
## For each element of @var{x}, compute the quantile (the inverse of the CDF)
## at @var{x} of the negative binomial distribution with parameters
## @var{n} and @var{p}.
##
## When @var{n} is integer this is the Pascal distribution.
## When @var{n} is extended to real numbers this is the Polya distribution.
##
## The number of failures in a Bernoulli experiment with success probability
## @var{p} before the @var{n}-th success follows this distribution.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Quantile function of the Pascal distribution

function inv = nbininv (x, n, p)

  if (nargin != 3)
    print_usage ();
  endif

  if (! isscalar (n) || ! isscalar (p))
    [retval, x, n, p] = common_size (x, n, p);
    if (retval > 0)
      error ("nbininv: X, N, and P must be of common size or scalars");
    endif
  endif

  if (iscomplex (x) || iscomplex (n) || iscomplex (p))
    error ("nbininv: X, N, and P must not be complex");
  endif

  if (isa (x, "single") || isa (n, "single") || isa (p, "single"))
    inv = zeros (size (x), "single");
  else
    inv = zeros (size (x));
  endif

  k = (isnan (x) | (x < 0) | (x > 1) | isnan (n) | (n < 1) | (n == Inf)
       | isnan (p) | (p < 0) | (p > 1));
  inv(k) = NaN;

  k = (x == 1) & (n > 0) & (n < Inf) & (p >= 0) & (p <= 1);
  inv(k) = Inf;

  k = find ((x >= 0) & (x < 1) & (n > 0) & (n < Inf)
            & (p > 0) & (p <= 1));
  m = zeros (size (k));
  x = x(k);
  if (isscalar (n) && isscalar (p))
    s = p ^ n * ones (size (k));
    while (1)
      l = find (s < x);
      if (any (l))
        m(l) = m(l) + 1;
        s(l) = s(l) + nbinpdf (m(l), n, p);
      else
        break;
      endif
    endwhile
  else
    n = n(k);
    p = p(k);
    s = p .^ n;
    while (1)
      l = find (s < x);
      if (any (l))
        m(l) = m(l) + 1;
        s(l) = s(l) + nbinpdf (m(l), n(l), p(l));
      else
        break;
      endif
    endwhile
  endif
  inv(k) = m;

endfunction


%!shared x
%! x = [-1 0 3/4 1 2];
%!assert (nbininv (x, ones (1,5), 0.5*ones (1,5)), [NaN 0 1 Inf NaN])
%!assert (nbininv (x, 1, 0.5*ones (1,5)), [NaN 0 1 Inf NaN])
%!assert (nbininv (x, ones (1,5), 0.5), [NaN 0 1 Inf NaN])
%!assert (nbininv (x, [1 0 NaN Inf 1], 0.5), [NaN NaN NaN NaN NaN])
%!assert (nbininv (x, [1 0 1.5 Inf 1], 0.5), [NaN NaN 2 NaN NaN])
%!assert (nbininv (x, 1, 0.5*[1 -Inf NaN Inf 1]), [NaN NaN NaN NaN NaN])
%!assert (nbininv ([x(1:2) NaN x(4:5)], 1, 0.5), [NaN 0 NaN Inf NaN])

## Test class of input preserved
%!assert (nbininv ([x, NaN], 1, 0.5), [NaN 0 1 Inf NaN NaN])
%!assert (nbininv (single ([x, NaN]), 1, 0.5), single ([NaN 0 1 Inf NaN NaN]))
%!assert (nbininv ([x, NaN], single (1), 0.5), single ([NaN 0 1 Inf NaN NaN]))
%!assert (nbininv ([x, NaN], 1, single (0.5)), single ([NaN 0 1 Inf NaN NaN]))

## Test input validation
%!error nbininv ()
%!error nbininv (1)
%!error nbininv (1,2)
%!error nbininv (1,2,3,4)
%!error nbininv (ones (3), ones (2), ones (2))
%!error nbininv (ones (2), ones (3), ones (2))
%!error nbininv (ones (2), ones (2), ones (3))
%!error nbininv (i, 2, 2)
%!error nbininv (2, i, 2)
%!error nbininv (2, 2, i)

