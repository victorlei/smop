## Copyright (C) 1993-2015 John W. Eaton
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
## @deftypefn {Function File} {} hilb (@var{n})
## Return the Hilbert matrix of order @var{n}.
##
## The @math{i,j} element of a Hilbert matrix is defined as
## @tex
## $$
## H(i, j) = {1 \over (i + j - 1)}
## $$
## @end tex
## @ifnottex
##
## @example
## H(i, j) = 1 / (i + j - 1)
## @end example
##
## @end ifnottex
##
## Hilbert matrices are close to being singular which make them difficult to
## invert with numerical routines.  Comparing the condition number of a random
## matrix 5x5 matrix with that of a Hilbert matrix of order 5 reveals just how
## difficult the problem is.
##
## @example
## @group
## cond (rand (5))
##    @result{} 14.392
## cond (hilb (5))
##    @result{} 4.7661e+05
## @end group
## @end example
##
## @seealso{invhilb}
## @end deftypefn

## Author: jwe

function retval = hilb (n)

  if (nargin != 1)
    print_usage ();
  elseif (! isscalar (n))
    error ("hilb: N must be a scalar integer");
  endif

  retval = zeros (n);
  tmp = 1:n;
  for i = 1:n
    retval(i, :) = 1.0 ./ tmp;
    tmp++;
  endfor

endfunction


%!assert (hilb (2), [1, 1/2; 1/2, 1/3])
%!assert (hilb (3), [1, 1/2, 1/3; 1/2, 1/3, 1/4; 1/3, 1/4, 1/5])

%!error hilb ()
%!error hilb (1, 2)
%!error <N must be a scalar integer> hilb (ones (2))

