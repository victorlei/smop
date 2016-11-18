## Copyright (C) 1995-2015 Kurt Hornik
## Copyright (C) 2009 Jaroslav Hajek
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
## @deftypefn  {Function File} {} range (@var{x})
## @deftypefnx {Function File} {} range (@var{x}, @var{dim})
## Return the range, i.e., the difference between the maximum and the minimum
## of the input data.
##
## If @var{x} is a vector, the range is calculated over the elements of
## @var{x}.  If @var{x} is a matrix, the range is calculated over each column
## of @var{x}.
##
## If the optional argument @var{dim} is given, operate along this dimension.
##
## The range is a quickly computed measure of the dispersion of a data set, but
## is less accurate than @code{iqr} if there are outlying data points.
## @seealso{iqr, std}
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Compute range

function y = range (x, dim)

  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif

  if (nargin == 1)
    y = max (x) - min (x);
  else
    y = max (x, [], dim) - min (x, [], dim);
  endif

endfunction


%!assert (range (1:10), 9)
%!assert (range (single (1:10)), single (9))
%!assert (range (magic (3)), [5, 8, 5])
%!assert (range (magic (3), 2), [7; 4; 7])
%!assert (range (2), 0)

## Test input validation
%!error range ()
%!error range (1, 2, 3)

