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
## @deftypefn  {Function File} {} meansq (@var{x})
## @deftypefnx {Function File} {} meansq (@var{x}, @var{dim})
## Compute the mean square of the elements of the vector @var{x}.
##
## The mean square is defined as
## @tex
## $$
## {\rm meansq} (x) = {\sum_{i=1}^N {x_i}^2 \over N}
## $$
## where $\bar{x}$ is the mean value of $x$.
## @end tex
## @ifnottex
##
## @example
## @group
## meansq (x) = 1/N SUM_i x(i)^2
## @end group
## @end example
##
## @end ifnottex
## For matrix arguments, return a row vector containing the mean square
## of each column.
##
## If the optional argument @var{dim} is given, operate along this dimension.
## @seealso{var, std, moment}
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Compute mean square

function y = meansq (x, dim)

  if (nargin != 1 && nargin != 2)
    print_usage ();
  endif

  if (! (isnumeric (x) || islogical (x)))
    error ("mean: X must be a numeric vector or matrix");
  endif

  nd = ndims (x);
  sz = size (x);
  if (nargin < 2)
    ## Find the first non-singleton dimension.
    (dim = find (sz > 1, 1)) || (dim = 1);
  else
    if (!(isscalar (dim) && dim == fix (dim))
        || !(1 <= dim && dim <= nd))
      error ("mean: DIM must be an integer and a valid dimension");
    endif
  endif

  y = sumsq (x, dim) / sz(dim);

endfunction


%!assert (meansq (1:5), 11)
%!assert (meansq (single (1:5)), single (11))
%!assert (meansq (magic (4)), [94.5, 92.5, 92.5, 94.5])
%!assert (meansq (magic (4), 2), [109.5; 77.5; 77.5; 109.5])

## Test input validation
%!error meansq ()
%!error meansq (1, 2, 3)
%!error meansq (['A'; 'B'])
%!error meansq (1, ones (2,2))
%!error meansq (1, 1.5)
%!error meansq (1, 0)
%!error meansq (1, 3)

