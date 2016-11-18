## Copyright (C) 1999-2015 Peter Ekberg
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
## @deftypefn  {Function File} {} pascal (@var{n})
## @deftypefnx {Function File} {} pascal (@var{n}, @var{t})
## Return the Pascal matrix of order @var{n} if @code{@var{t} = 0}.
##
## The default value of @var{t} is 0.
##
## When @code{@var{t} = 1}, return the pseudo-lower triangular
## Cholesky@tie{}factor of the Pascal matrix (The sign of some columns may be
## negative).  This matrix is its own inverse, that is
## @code{pascal (@var{n}, 1) ^ 2 == eye (@var{n})}.
##
## If @code{@var{t} = -1}, return the true Cholesky@tie{}factor with strictly
## positive values on the diagonal.
##
## If @code{@var{t} = 2}, return a transposed and permuted version of
## @code{pascal (@var{n}, 1)}, which is the cube root of the identity matrix.
## That is, @code{pascal (@var{n}, 2) ^ 3 == eye (@var{n})}.
##
## @seealso{chol}
## @end deftypefn

## Author: Peter Ekberg
##         (peda)

function retval = pascal (n, t = 0)

  if (nargin < 1 || nargin > 2)
    print_usage ();
  elseif (! (isscalar (n) && isscalar (t)))
    error ("pascal: N and T must be scalars");
  elseif (! any (t == [-1, 0, 1, 2]))
    error ("pascal: expecting T to be -1, 0, 1, or 2, found %d", t);
  endif

  retval = zeros (n);
  if (n > 0)
    retval(:,1) = 1;
  endif

  if (t == -1)
    for j = 2:n
      retval(j:n,j) = cumsum (retval(j-1:n-1,j-1));
    endfor
  else
    for j = 2:n
      retval(j:n,j) = -cumsum (retval(j-1:n-1,j-1));
    endfor
  endif

  if (t == 0)
    retval = retval*retval';
  elseif (t == 2)
    retval = rot90 (retval, 3);
    if (rem (n,2) != 1)
      retval *= -1;
    endif
  endif

endfunction


%!assert (pascal (3,-1), [1,0,0;1,1,0;1,2,1])
%!assert (pascal (3,0), [1,1,1;1,2,3;1,3,6])
%!assert (pascal (3,0), pascal (3))
%!assert (pascal (3,1), [1,0,0;1,-1,0;1,-2,1])
%!assert (pascal (3,2), [1,1,1;-2,-1,0;1,0,0])
%!assert (pascal (0,2), [])

## Test input validation
%!error pascal ()
%!error pascal (1,2,3)
%!error <N and T must be scalars> pascal ([1 2])
%!error <N and T must be scalars> pascal (1, [1 2])
%!error <expecting T to be> pascal (3,-2)
%!error <expecting T to be> pascal (3,4)

