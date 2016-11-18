## Copyright (C) 2012 Rik Wehbring
## Copyright (C) 2007-2015 David Bateman
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
## @deftypefn {Function File} {} unidinv (@var{x}, @var{n})
## For each element of @var{x}, compute the quantile (the inverse of the CDF)
## at @var{x} of the discrete uniform distribution which assumes
## the integer values 1--@var{n} with equal probability.
## @end deftypefn

function inv = unidinv (x, n)

  if (nargin != 2)
    print_usage ();
  endif

  if (! isscalar (n))
    [retval, x, n] = common_size (x, n);
    if (retval > 0)
      error ("unidcdf: X and N must be of common size or scalars");
    endif
  endif

  if (iscomplex (x) || iscomplex (n))
    error ("unidinv: X and N must not be complex");
  endif

  if (isa (x, "single") || isa (n, "single"))
    inv = NaN (size (x), "single");
  else
    inv = NaN (size (x));
  endif

  ## For Matlab compatibility, unidinv(0) = NaN
  k = (x > 0) & (x <= 1) & (n > 0 & n == fix (n));
  if (isscalar (n))
    inv(k) = floor (x(k) * n);
  else
    inv(k) = floor (x(k) .* n(k));
  endif

endfunction


%!shared x
%! x = [-1 0 0.5 1 2];
%!assert (unidinv (x, 10*ones (1,5)), [NaN NaN 5 10 NaN], eps)
%!assert (unidinv (x, 10), [NaN NaN 5 10 NaN], eps)
%!assert (unidinv (x, 10*[0 1 NaN 1 1]), [NaN NaN NaN 10 NaN], eps)
%!assert (unidinv ([x(1:2) NaN x(4:5)], 10), [NaN NaN NaN 10 NaN], eps)

## Test class of input preserved
%!assert (unidinv ([x, NaN], 10), [NaN NaN 5 10 NaN NaN], eps)
%!assert (unidinv (single ([x, NaN]), 10), single ([NaN NaN 5 10 NaN NaN]), eps)
%!assert (unidinv ([x, NaN], single (10)), single ([NaN NaN 5 10 NaN NaN]), eps)

## Test input validation
%!error unidinv ()
%!error unidinv (1)
%!error unidinv (1,2,3)
%!error unidinv (ones (3), ones (2))
%!error unidinv (ones (2), ones (3))
%!error unidinv (i, 2)
%!error unidinv (2, i)

