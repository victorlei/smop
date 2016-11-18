## Copyright (C) 2012 Rik Wehbring
## Copyright (C) 2010-2015 David Bateman
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
## @deftypefn {Function File} {} discrete_cdf (@var{x}, @var{v}, @var{p})
## For each element of @var{x}, compute the cumulative distribution function
## (CDF) at @var{x} of a univariate discrete distribution which assumes the
## values in @var{v} with probabilities @var{p}.
## @end deftypefn

function cdf = discrete_cdf (x, v, p)

  if (nargin != 3)
    print_usage ();
  endif

  if (! isvector (v))
    error ("discrete_cdf: V must be a vector");
  elseif (any (isnan (v)))
    error ("discrete_cdf: V must not have any NaN elements");
  elseif (! isvector (p) || (length (p) != length (v)))
    error ("discrete_cdf: P must be a vector with length (V) elements");
  elseif (! (all (p >= 0) && any (p)))
    error ("discrete_cdf: P must be a nonzero, non-negative vector");
  endif

  p = p(:) / sum (p);   # Reshape and normalize probability vector

  if (isa (x, "single") || isa (v, "single") || isa (p, "single"));
    cdf = NaN (size (x), "single");
  else
    cdf = NaN (size (x));
  endif

  k = ! isnan (x);
  [vs, vi] = sort (v);
  cdf(k) = [0 ; cumsum(p(vi))](lookup (vs, x(k)) + 1);

endfunction


%!shared x,v,p,y
%! x = [-1 0.1 1.1 1.9 3];
%! v = 0.1:0.2:1.9;
%! p = 1/length(v) * ones (1, length(v));
%! y = [0 0.1 0.6 1 1];
%!assert (discrete_cdf ([x, NaN], v, p), [y, NaN], eps)

## Test class of input preserved
%!assert (discrete_cdf (single ([x, NaN]), v, p), single ([y, NaN]), 2*eps ("single"))
%!assert (discrete_cdf ([x, NaN], single (v), p), single ([y, NaN]), 2*eps ("single"))
%!assert (discrete_cdf ([x, NaN], v, single (p)), single ([y, NaN]), 2*eps ("single"))

## Test input validation
%!error discrete_cdf ()
%!error discrete_cdf (1)
%!error discrete_cdf (1,2)
%!error discrete_cdf (1,2,3,4)
%!error discrete_cdf (1, ones (2), ones (2,1))
%!error discrete_cdf (1, [1 ; NaN], ones (2,1))
%!error discrete_cdf (1, ones (2,1), ones (1,1))
%!error discrete_cdf (1, ones (2,1), [1 -1])
%!error discrete_cdf (1, ones (2,1), [1 NaN])
%!error discrete_cdf (1, ones (2,1), [0  0])

