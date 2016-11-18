## Copyright (C) 1995-2015 Kurt Hornik
## Copyright (C) 2009 VZLU Prague
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
## @deftypefn  {Function File} {} center (@var{x})
## @deftypefnx {Function File} {} center (@var{x}, @var{dim})
## Center data by subtracting its mean.
##
## If @var{x} is a vector, subtract its mean.
##
## If @var{x} is a matrix, do the above for each column.
##
## If the optional argument @var{dim} is given, operate along this dimension.
##
## Programming Note: @code{center} has obvious application for normalizing
## statistical data.  It is also useful for improving the precision of general
## numerical calculations.  Whenever there is a large value that is common
## to a batch of data, the mean can be subtracted off, the calculation
## performed, and then the mean added back to obtain the final answer.
## @seealso{zscore}
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Center by subtracting means

function retval = center (x, dim)

  if (nargin != 1 && nargin != 2)
    print_usage ();
  endif

  if (! (isnumeric (x) || islogical (x)))
    error ("center: X must be a numeric vector or matrix");
  endif

  if (isinteger (x))
    x = double (x);
  endif

  nd = ndims (x);
  sz = size (x);
  if (nargin != 2)
    ## Find the first non-singleton dimension.
    (dim = find (sz > 1, 1)) || (dim = 1);
  else
    if (!(isscalar (dim) && dim == fix (dim))
        || !(1 <= dim && dim <= nd))
      error ("center: DIM must be an integer and a valid dimension");
    endif
  endif

  n = sz(dim);

  if (n == 0)
    retval = x;
  else
    retval = bsxfun (@minus, x, mean (x, dim));
  endif

endfunction


%!assert (center ([1,2,3]), [-1,0,1])
%!assert (center (single ([1,2,3])), single ([-1,0,1]))
%!assert (center (int8 ([1,2,3])), [-1,0,1])
%!assert (center (logical ([1, 0, 0, 1])), [0.5, -0.5, -0.5, 0.5])
%!assert (center (ones (3,2,0,2)), zeros (3,2,0,2))
%!assert (center (ones (3,2,0,2, "single")), zeros (3,2,0,2, "single"))
%!assert (center (magic (3)), [3,-4,1;-2,0,2;-1,4,-3])
%!assert (center ([1 2 3; 6 5 4], 2), [-1 0 1; 1 0 -1])

## Test input validation
%!error center ()
%!error center (1, 2, 3)
%!error center (1, ones (2,2))
%!error center (1, 1.5)
%!error center (1, 0)
%!error center (1, 3)

