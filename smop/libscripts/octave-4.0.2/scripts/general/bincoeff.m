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
## @deftypefn {Mapping Function} {} bincoeff (@var{n}, @var{k})
## Return the binomial coefficient of @var{n} and @var{k}, defined as
## @tex
## $$
##  {n \choose k} = {n (n-1) (n-2) \cdots (n-k+1) \over k!}
## $$
## @end tex
## @ifnottex
##
## @example
## @group
##  /   \
##  | n |    n (n-1) (n-2) @dots{} (n-k+1)
##  |   |  = -------------------------
##  | k |               k!
##  \   /
## @end group
## @end example
##
## @end ifnottex
## For example:
##
## @example
## @group
## bincoeff (5, 2)
##    @result{} 10
## @end group
## @end example
##
## In most cases, the @code{nchoosek} function is faster for small
## scalar integer arguments.  It also warns about loss of precision for
## big arguments.
##
## @seealso{nchoosek}
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Created: 8 October 1994
## Adapted-By: jwe

function b = bincoeff (n, k)

  if (nargin != 2)
    print_usage ();
  endif

  [retval, n, k] = common_size (n, k);
  if (retval > 0)
    error ("bincoeff: N and K must be of common size or scalars");
  endif

  if (iscomplex (n) || iscomplex (k))
    error ("bincoeff: N and K must not be complex");
  endif

  b = zeros (size (n));

  ok = (k >= 0) & (k == fix (k)) & (! isnan (n));
  b(! ok) = NaN;

  n_int = (n == fix (n));
  idx = n_int & (n < 0) & ok;
  b(idx) = (-1) .^ k(idx) .* exp (gammaln (abs (n(idx)) + k(idx))
                                  - gammaln (k(idx) + 1)
                                  - gammaln (abs (n(idx))));

  idx = (n >= k) & ok;
  b(idx) = exp (gammaln (n(idx) + 1)
                - gammaln (k(idx) + 1)
                - gammaln (n(idx) - k(idx) + 1));

  idx = (! n_int) & (n < k) & ok;
  b(idx) = (1/pi) * exp (gammaln (n(idx) + 1)
                         - gammaln (k(idx) + 1)
                         + gammaln (k(idx) - n(idx))
                         + log (sin (pi * (n(idx) - k(idx) + 1))));

  ## Clean up rounding errors.
  b(n_int) = round (b(n_int));

  idx = ! n_int;
  b(idx) = real (b(idx));

endfunction


%!assert (bincoeff (4, 2), 6)
%!assert (bincoeff (2, 4), 0)
%!assert (bincoeff (-4, 2), 10)
%!assert (bincoeff (5, 2), 10)
%!assert (bincoeff (50, 6), 15890700)
%!assert (bincoeff (0.4, 2), -.12, 8*eps)

%!assert (bincoeff ([4 NaN 4], [-1, 2, 2.5]), NaN (1, 3))

## Test input validation
%!error bincoeff ()
%!error bincoeff (1, 2, 3)
%!error bincoeff (ones (3),ones (2))
%!error bincoeff (ones (2),ones (3))

