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
## @deftypefn  {Function File} {} norminv (@var{x})
## @deftypefnx {Function File} {} norminv (@var{x}, @var{mu}, @var{sigma})
## For each element of @var{x}, compute the quantile (the inverse of the CDF)
## at @var{x} of the normal distribution with mean @var{mu} and
## standard deviation @var{sigma}.
##
## Default values are @var{mu} = 0, @var{sigma} = 1.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Quantile function of the normal distribution

function inv = norminv (x, mu = 0, sigma = 1)

  if (nargin != 1 && nargin != 3)
    print_usage ();
  endif

  if (! isscalar (mu) || ! isscalar (sigma))
    [retval, x, mu, sigma] = common_size (x, mu, sigma);
    if (retval > 0)
      error ("norminv: X, MU, and SIGMA must be of common size or scalars");
    endif
  endif

  if (iscomplex (x) || iscomplex (mu) || iscomplex (sigma))
    error ("norminv: X, MU, and SIGMA must not be complex");
  endif

  if (isa (x, "single") || isa (mu, "single") || isa (sigma, "single"))
    inv = NaN (size (x), "single");
  else
    inv = NaN (size (x));
  endif

  if (isscalar (mu) && isscalar (sigma))
    if (isfinite (mu) && (sigma > 0) && (sigma < Inf))
      inv = mu + sigma * stdnormal_inv (x);
    endif
  else
    k = isfinite (mu) & (sigma > 0) & (sigma < Inf);
    inv(k) = mu(k) + sigma(k) .* stdnormal_inv (x(k));
  endif

endfunction


%!shared x
%! x = [-1 0 0.5 1 2];
%!assert (norminv (x, ones (1,5), ones (1,5)), [NaN -Inf 1 Inf NaN])
%!assert (norminv (x, 1, ones (1,5)), [NaN -Inf 1 Inf NaN])
%!assert (norminv (x, ones (1,5), 1), [NaN -Inf 1 Inf NaN])
%!assert (norminv (x, [1 -Inf NaN Inf 1], 1), [NaN NaN NaN NaN NaN])
%!assert (norminv (x, 1, [1 0 NaN Inf 1]), [NaN NaN NaN NaN NaN])
%!assert (norminv ([x(1:2) NaN x(4:5)], 1, 1), [NaN -Inf NaN Inf NaN])

## Test class of input preserved
%!assert (norminv ([x, NaN], 1, 1), [NaN -Inf 1 Inf NaN NaN])
%!assert (norminv (single ([x, NaN]), 1, 1), single ([NaN -Inf 1 Inf NaN NaN]))
%!assert (norminv ([x, NaN], single (1), 1), single ([NaN -Inf 1 Inf NaN NaN]))
%!assert (norminv ([x, NaN], 1, single (1)), single ([NaN -Inf 1 Inf NaN NaN]))

## Test input validation
%!error norminv ()
%!error norminv (1,2)
%!error norminv (1,2,3,4)
%!error norminv (ones (3), ones (2), ones (2))
%!error norminv (ones (2), ones (3), ones (2))
%!error norminv (ones (2), ones (2), ones (3))
%!error norminv (i, 2, 2)
%!error norminv (2, i, 2)
%!error norminv (2, 2, i)

