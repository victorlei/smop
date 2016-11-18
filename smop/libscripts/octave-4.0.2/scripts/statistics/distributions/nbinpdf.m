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
## @deftypefn {Function File} {} nbinpdf (@var{x}, @var{n}, @var{p})
## For each element of @var{x}, compute the probability density function (PDF)
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
## Description: PDF of the Pascal (negative binomial) distribution

function pdf = nbinpdf (x, n, p)

  if (nargin != 3)
    print_usage ();
  endif

  if (! isscalar (n) || ! isscalar (p))
    [retval, x, n, p] = common_size (x, n, p);
    if (retval > 0)
      error ("nbinpdf: X, N, and P must be of common size or scalars");
    endif
  endif

  if (iscomplex (x) || iscomplex (n) || iscomplex (p))
    error ("nbinpdf: X, N, and P must not be complex");
  endif

  if (isa (x, "single") || isa (n, "single") || isa (p, "single"))
    pdf = NaN (size (x), "single");
  else
    pdf = NaN (size (x));
  endif

  ok = (x < Inf) & (x == fix (x)) & (n > 0) & (n < Inf) & (p >= 0) & (p <= 1);

  k = (x < 0) & ok;
  pdf(k) = 0;

  k = (x >= 0) & ok;
  if (isscalar (n) && isscalar (p))
    pdf(k) = bincoeff (-n, x(k)) .* (p ^ n) .* ((p - 1) .^ x(k));
  else
    pdf(k) = bincoeff (-n(k), x(k)) .* (p(k) .^ n(k)) .* ((p(k) - 1) .^ x(k));
  endif


endfunction


%!shared x,y
%! x = [-1 0 1 2 Inf];
%! y = [0 1/2 1/4 1/8 NaN];
%!assert (nbinpdf (x, ones (1,5), 0.5*ones (1,5)), y)
%!assert (nbinpdf (x, 1, 0.5*ones (1,5)), y)
%!assert (nbinpdf (x, ones (1,5), 0.5), y)
%!assert (nbinpdf (x, [0 1 NaN 1.5 Inf], 0.5), [NaN 1/2 NaN 1.875*0.5^1.5/4 NaN], eps)
%!assert (nbinpdf (x, 1, 0.5*[-1 NaN 4 1 1]), [NaN NaN NaN y(4:5)])
%!assert (nbinpdf ([x, NaN], 1, 0.5), [y, NaN])

## Test class of input preserved
%!assert (nbinpdf (single ([x, NaN]), 1, 0.5), single ([y, NaN]))
%!assert (nbinpdf ([x, NaN], single (1), 0.5), single ([y, NaN]))
%!assert (nbinpdf ([x, NaN], 1, single (0.5)), single ([y, NaN]))

## Test input validation
%!error nbinpdf ()
%!error nbinpdf (1)
%!error nbinpdf (1,2)
%!error nbinpdf (1,2,3,4)
%!error nbinpdf (ones (3), ones (2), ones (2))
%!error nbinpdf (ones (2), ones (3), ones (2))
%!error nbinpdf (ones (2), ones (2), ones (3))
%!error nbinpdf (i, 2, 2)
%!error nbinpdf (2, i, 2)
%!error nbinpdf (2, 2, i)

