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
## @deftypefn {Function File} {} geoinv (@var{x}, @var{p})
## For each element of @var{x}, compute the quantile (the inverse of the CDF)
## at @var{x} of the geometric distribution with parameter @var{p}.
##
## The geometric distribution models the number of failures (@var{x}-1) of a
## Bernoulli trial with probability @var{p} before the first success (@var{x}).
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Quantile function of the geometric distribution

function inv = geoinv (x, p)

  if (nargin != 2)
    print_usage ();
  endif

  if (! isscalar (p))
    [retval, x, p] = common_size (x, p);
    if (retval > 0)
      error ("geoinv: X and P must be of common size or scalars");
    endif
  endif

  if (iscomplex (x) || iscomplex (p))
    error ("geoinv: X and P must not be complex");
  endif

  if (isa (x, "single") || isa (p, "single"))
    inv = NaN (size (x), "single");
  else
    inv = NaN (size (x));
  endif

  k = (x == 1) & (p >= 0) & (p <= 1);
  inv(k) = Inf;

  k = (x >= 0) & (x < 1) & (p > 0) & (p <= 1);
  if (isscalar (p))
    inv(k) = max (ceil (log (1 - x(k)) / log (1 - p)) - 1, 0);
  else
    inv(k) = max (ceil (log (1 - x(k)) ./ log (1 - p(k))) - 1, 0);
  endif

endfunction


%!shared x
%! x = [-1 0 0.75 1 2];
%!assert (geoinv (x, 0.5*ones (1,5)), [NaN 0 1 Inf NaN])
%!assert (geoinv (x, 0.5), [NaN 0 1 Inf NaN])
%!assert (geoinv (x, 0.5*[1 -1 NaN 4 1]), [NaN NaN NaN NaN NaN])
%!assert (geoinv ([x(1:2) NaN x(4:5)], 0.5), [NaN 0 NaN Inf NaN])

## Test class of input preserved
%!assert (geoinv ([x, NaN], 0.5), [NaN 0 1 Inf NaN NaN])
%!assert (geoinv (single ([x, NaN]), 0.5), single ([NaN 0 1 Inf NaN NaN]))
%!assert (geoinv ([x, NaN], single (0.5)), single ([NaN 0 1 Inf NaN NaN]))

## Test input validation
%!error geoinv ()
%!error geoinv (1)
%!error geoinv (1,2,3)
%!error geoinv (ones (3), ones (2))
%!error geoinv (ones (2), ones (3))
%!error geoinv (i, 2)
%!error geoinv (2, i)

