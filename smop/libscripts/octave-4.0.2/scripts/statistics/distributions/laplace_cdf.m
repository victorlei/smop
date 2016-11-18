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
## @deftypefn {Function File} {} laplace_cdf (@var{x})
## For each element of @var{x}, compute the cumulative distribution function
## (CDF) at @var{x} of the Laplace distribution.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: CDF of the Laplace distribution

function cdf = laplace_cdf (x)

  if (nargin != 1)
    print_usage ();
  endif

  if (iscomplex (x))
    error ("laplace_cdf: X must not be complex");
  endif

  cdf = (1 + sign (x) .* (1 - exp (- abs (x)))) / 2;

endfunction


%!shared x,y
%! x = [-Inf -log(2) 0 log(2) Inf];
%! y = [0, 1/4, 1/2, 3/4, 1];
%!assert (laplace_cdf ([x, NaN]), [y, NaN])

## Test class of input preserved
%!assert (laplace_cdf (single ([x, NaN])), single ([y, NaN]))

## Test input validation
%!error laplace_cdf ()
%!error laplace_cdf (1,2)
%!error laplace_cdf (i)

