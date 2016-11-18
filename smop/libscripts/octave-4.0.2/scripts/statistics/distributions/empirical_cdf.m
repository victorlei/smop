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
## @deftypefn {Function File} {} empirical_cdf (@var{x}, @var{data})
## For each element of @var{x}, compute the cumulative distribution function
## (CDF) at @var{x} of the empirical distribution obtained from
## the univariate sample @var{data}.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: CDF of the empirical distribution

function cdf = empirical_cdf (x, data)

  if (nargin != 2)
    print_usage ();
  endif

  if (! isvector (data))
    error ("empirical_cdf: DATA must be a vector");
  endif

  cdf = discrete_cdf (x, data, ones (size (data)));

endfunction


%!shared x,v,y
%! x = [-1 0.1 1.1 1.9 3];
%! v = 0.1:0.2:1.9;
%! y = [0 0.1 0.6 1 1];
%!assert (empirical_cdf (x, v), y, eps)
%!assert (empirical_cdf ([x(1) NaN x(3:5)], v), [0 NaN 0.6 1 1], eps)

## Test class of input preserved
%!assert (empirical_cdf ([x, NaN], v), [y, NaN], eps)
%!assert (empirical_cdf (single ([x, NaN]), v), single ([y, NaN]), eps)
%!assert (empirical_cdf ([x, NaN], single (v)), single ([y, NaN]), eps)

## Test input validation
%!error empirical_cdf ()
%!error empirical_cdf (1)
%!error empirical_cdf (1,2,3)
%!error empirical_cdf (1, ones (2))

