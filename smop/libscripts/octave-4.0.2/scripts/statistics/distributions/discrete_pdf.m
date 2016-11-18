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
## @deftypefn {Function File} {} discrete_pdf (@var{x}, @var{v}, @var{p})
## For each element of @var{x}, compute the probability density function (PDF)
## at @var{x} of a univariate discrete distribution which assumes the values
## in @var{v} with probabilities @var{p}.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: PDF of a discrete distribution

function pdf = discrete_pdf (x, v, p)

  if (nargin != 3)
    print_usage ();
  endif

  if (! isvector (v))
    error ("discrete_pdf: V must be a vector");
  elseif (any (isnan (v)))
    error ("discrete_pdf: V must not have any NaN elements");
  elseif (! isvector (p) || (length (p) != length (v)))
    error ("discrete_pdf: P must be a vector with length (V) elements");
  elseif (! (all (p >= 0) && any (p)))
    error ("discrete_pdf: P must be a nonzero, non-negative vector");
  endif

  ## Reshape and normalize probability vector.  Values not in table get 0 prob.
  p = [0 ; p(:)/sum(p)];

  if (isa (x, "single") || isa (v, "single") || isa (p, "single"))
    pdf = NaN (size (x), "single");
  else
    pdf = NaN (size (x));
  endif

  k = ! isnan (x);
  [vs, vi] = sort (v(:));
  pdf(k) = p([0 ; vi](lookup (vs, x(k), 'm') + 1) + 1);

endfunction


%!shared x,v,p,y
%! x = [-1 0.1 1.1 1.9 3];
%! v = 0.1:0.2:1.9;
%! p = 1/length (v) * ones (1, length (v));
%! y = [0 0.1 0.1 0.1 0];
%!assert (discrete_pdf ([x, NaN], v, p), [y, NaN], 5*eps)

## Test class of input preserved
%!assert (discrete_pdf (single ([x, NaN]), v, p), single ([y, NaN]), 5*eps ("single"))
%!assert (discrete_pdf ([x, NaN], single (v), p), single ([y, NaN]), 5*eps ("single"))
%!assert (discrete_pdf ([x, NaN], v, single (p)), single ([y, NaN]), 5*eps ("single"))

## Test input validation
%!error discrete_pdf ()
%!error discrete_pdf (1)
%!error discrete_pdf (1,2)
%!error discrete_pdf (1,2,3,4)
%!error discrete_pdf (1, ones (2), ones (2,1))
%!error discrete_pdf (1, [1 ; NaN], ones (2,1))
%!error discrete_pdf (1, ones (2,1), ones (1,1))
%!error discrete_pdf (1, ones (2,1), [1 -1])
%!error discrete_pdf (1, ones (2,1), [1 NaN])
%!error discrete_pdf (1, ones (2,1), [0  0])

