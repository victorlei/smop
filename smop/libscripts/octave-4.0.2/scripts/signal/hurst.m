## Copyright (C) 1995-2015 Friedrich Leisch
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
## @deftypefn {Function File} {} hurst (@var{x})
## Estimate the Hurst parameter of sample @var{x} via the rescaled range
## statistic.
##
## If @var{x} is a matrix, the parameter is estimated for every column.
## @end deftypefn

## Author: FL <Friedrich.Leisch@ci.tuwien.ac.at>
## Description: Estimate the Hurst parameter

function H = hurst (x)

  if (nargin != 1)
    print_usage ();
  endif

  if (isscalar (x))
    error ("hurst: X must not be a scalar");
  elseif (isvector (x))
    x = reshape (x, length (x), 1);
  endif

  [xr, xc] = size (x);

  s = std (x);
  w = cumsum (x - mean (x));
  RS = (max (w) - min (w)) ./ s;
  H = log (RS) / log (xr);

endfunction

