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
## @deftypefn {Function File} {} binopdf (@var{x}, @var{n}, @var{p})
## For each element of @var{x}, compute the probability density function (PDF)
## at @var{x} of the binomial distribution with parameters @var{n} and @var{p},
## where @var{n} is the number of trials and @var{p} is the probability of
## success.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: PDF of the binomial distribution

function pdf = binopdf (x, n, p)

  if (nargin != 3)
    print_usage ();
  endif

  if (! isscalar (n) || ! isscalar (p))
    [retval, x, n, p] = common_size (x, n, p);
    if (retval > 0)
      error ("binopdf: X, N, and P must be of common size or scalars");
    endif
  endif

  if (iscomplex (x) || iscomplex (n) || iscomplex (p))
    error ("binopdf: X, N, and P must not be complex");
  endif

  if (isa (x, "single") || isa (n, "single") || isa (p, "single"));
    pdf = zeros (size (x), "single");
  else
    pdf = zeros (size (x));
  endif

  k = (x == fix (x)) & (n == fix (n)) & (n >= 0) & (p >= 0) & (p <= 1);

  pdf(! k) = NaN;

  k &= ((x >= 0) & (x <= n));
  if (isscalar (n) && isscalar (p))
    pdf(k) = exp (gammaln (n+1) - gammaln (x(k)+1) - gammaln (n-x(k)+1)
                  + x(k)*log (p) + (n-x(k))*log (1-p));
  else
    pdf(k) = exp (gammaln (n(k)+1) - gammaln (x(k)+1) - gammaln (n(k)-x(k)+1)
                  + x(k).*log (p(k)) + (n(k)-x(k)).*log (1-p(k)));
  endif

  ## Special case inputs
  ksp = k & (p == 0) & (x == 0);
  pdf(ksp) = 1;
  ksp = k & (p == 1) & (x == n);
  pdf(ksp) = 1;

endfunction


%!shared x,y,tol
%! if (ismac ())
%!   tol = eps ();
%! else
%!   tol = 0;
%! endif
%! x = [-1 0 1 2 3];
%! y = [0 1/4 1/2 1/4 0];
%!assert (binopdf (x, 2*ones (1,5), 0.5*ones (1,5)), y, tol)
%!assert (binopdf (x, 2, 0.5*ones (1,5)), y, tol)
%!assert (binopdf (x, 2*ones (1,5), 0.5), y, tol)
%!assert (binopdf (x, 2*[0 -1 NaN 1.1 1], 0.5), [0 NaN NaN NaN 0])
%!assert (binopdf (x, 2, 0.5*[0 -1 NaN 3 1]), [0 NaN NaN NaN 0])
%!assert (binopdf ([x, NaN], 2, 0.5), [y, NaN], tol)

## Test Special input values
%!assert (binopdf (0, 3, 0), 1);
%!assert (binopdf (2, 2, 1), 1);
%!assert (binopdf (1, 2, 1), 0);

## Test class of input preserved
%!assert (binopdf (single ([x, NaN]), 2, 0.5), single ([y, NaN]))
%!assert (binopdf ([x, NaN], single (2), 0.5), single ([y, NaN]))
%!assert (binopdf ([x, NaN], 2, single (0.5)), single ([y, NaN]))

## Test input validation
%!error binopdf ()
%!error binopdf (1)
%!error binopdf (1,2)
%!error binopdf (1,2,3,4)
%!error binopdf (ones (3), ones (2), ones (2))
%!error binopdf (ones (2), ones (3), ones (2))
%!error binopdf (ones (2), ones (2), ones (3))
%!error binopdf (i, 2, 2)
%!error binopdf (2, i, 2)
%!error binopdf (2, 2, i)

