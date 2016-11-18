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
## @deftypefn  {Function File} {[@var{t}, @var{l_x}] =} table (@var{x})
## @deftypefnx {Function File} {[@var{t}, @var{l_x}, @var{l_y}] =} table (@var{x}, @var{y})
## Create a contingency table @var{t} from data vectors.
##
## The @var{l_x} and @var{l_y} vectors are the corresponding levels.
##
## Currently, only 1- and 2-dimensional tables are supported.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Cross tabulation

function [t, v, w] = table (x, y)

  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif

  if (nargin == 1)
    if (! isnumeric (x) || ! isvector (x))
      error ("table: X must be a numeric vector");
    endif
    v = unique (x);
    for i = 1 : length (v)
      t(i) = sum (x == v(i) | isnan (v(i)) * isnan (x));
    endfor
  elseif (nargin == 2)
    if (! (   isvector (x) && isnumeric (x)
           && isvector (y) && isnumeric (y)
           && (length (x) == length (y))))
      error ("table: X and Y must be numeric vectors of the same length");
    endif
    v = unique (x);
    w = unique (y);
    for i = 1 : length (v)
      for j = 1 : length (w)
        t(i,j) = sum ((x == v(i) | isnan (v(i)) * isnan (x)) &
                      (y == w(j) | isnan (w(j)) * isnan (y)));
      endfor
    endfor
  endif

endfunction


## Test input validation
%!error table ()
%!error table (1, 2, 3)
%!error table (ones (2))
%!error table ([true true])
%!error table (ones (2,1), true (2,1))
%!error table (true (2,1), ones (2,1))
%!error table (ones (2,2), ones (2,1))
%!error table (ones (2,1), ones (2,2))
%!error table (ones (2,1), ones (3,1))

