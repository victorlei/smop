## Copyright (C) 1999-2015 Peter Ekberg
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
## @deftypefn {Function File} {} wilkinson (@var{n})
## Return the Wilkinson matrix of order @var{n}.
##
## Wilkinson matrices are symmetric and tridiagonal with pairs of nearly, but
## not exactly, equal eigenvalues.  They are useful in testing the behavior and
## performance of eigenvalue solvers.
##
## @seealso{rosser, eig}
## @end deftypefn

## Author: Peter Ekberg
##         (peda)

function retval = wilkinson (n)

  if (nargin != 1)
    print_usage ();
  endif

  if (! (isscalar (n) && n >= 0 && (n == fix (n))))
    error ("wilkinson: N must be a non-negative integer");
  endif

  side = ones (n-1, 1);
  center = abs (-(n-1)/2:(n-1)/2);
  retval = diag (side, -1) + diag (center) + diag (side, 1);

endfunction


%!assert (wilkinson (0), [])
%!assert (wilkinson (1), 0)
%!assert (wilkinson (2), [0.5,1;1,0.5])
%!assert (wilkinson (3), [1,1,0;1,0,1;0,1,1])
%!assert (wilkinson (4), [1.5,1,0,0;1,0.5,1,0;0,1,0.5,1;0,0,1,1.5])

## Test input validation
%!error wilkinson ()
%!error wilkinson (1,2)
%!error <N must be a non-negative integer> wilkinson (ones (2))
%!error <N must be a non-negative integer> wilkinson (-1)
%!error <N must be a non-negative integer> wilkinson (1.5)

