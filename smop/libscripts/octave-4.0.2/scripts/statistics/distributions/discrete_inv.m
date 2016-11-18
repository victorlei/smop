## Copyright (C) 2012 Rik Wehbring
## Copyright (C) 1996-2015 Kurt Hornik
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
## @deftypefn {Function File} {} discrete_inv (@var{x}, @var{v}, @var{p})
## For each element of @var{x}, compute the quantile (the inverse of the CDF)
## at @var{x} of the univariate distribution which assumes the values in
## @var{v} with probabilities @var{p}.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Quantile function of a discrete distribution

function inv = discrete_inv (x, v, p)

  if (nargin != 3)
    print_usage ();
  endif

  if (! isvector (v))
    error ("discrete_inv: V must be a vector");
  elseif (! isvector (p) || (length (p) != length (v)))
    error ("discrete_inv: P must be a vector with length (V) elements");
  elseif (any (isnan (p)))
    error ("discrete_rnd: P must not have any NaN elements");
  elseif (! (all (p >= 0) && any (p)))
    error ("discrete_inv: P must be a nonzero, non-negative vector");
  endif

  if (isa (x, "single") || isa (v, "single") || isa (p, "single"));
    inv = NaN (size (x), "single");
  else
    inv = NaN (size (x));
  endif

  ## FIXME: This isn't elegant.  But cumsum and lookup together produce
  ## different results when called with a single or a double.
  if (isa (p, "single"));
    p = double (p);
  endif

  [v, idx] = sort (v);
  p = cumsum (p(idx)(:)) / sum (p);  # Reshape and normalize probability vector

  k = (x == 0);
  inv(k) = v(1);

  k = (x == 1);
  inv(k) = v(end);

  k = (x > 0) & (x < 1);
  inv(k) = v(length (p) - lookup (sort (p, "descend"), x(k)) + 1);

endfunction


%!shared x,v,p,y
%! x = [-1 0 0.1 0.5 1 2];
%! v = 0.1:0.2:1.9;
%! p = 1/length(v) * ones (1, length(v));
%! y = [NaN v(1) v(1) v(end/2) v(end) NaN];
%!assert (discrete_inv ([x, NaN], v, p), [y, NaN], eps)

## Test class of input preserved
%!assert (discrete_inv (single ([x, NaN]), v, p), single ([y, NaN]), eps ("single"))
%!assert (discrete_inv ([x, NaN], single (v), p), single ([y, NaN]), eps ("single"))
%!assert (discrete_inv ([x, NaN], v, single (p)), single ([y, NaN]), eps ("single"))

## Test input validation
%!error discrete_inv ()
%!error discrete_inv (1)
%!error discrete_inv (1,2)
%!error discrete_inv (1,2,3,4)
%!error discrete_inv (1, ones (2), ones (2,1))
%!error discrete_inv (1, ones (2,1), ones (1,1))
%!error discrete_inv (1, ones (2,1), [1 NaN])
%!error discrete_inv (1, ones (2,1), [1 -1])
%!error discrete_inv (1, ones (2,1), [0  0])

