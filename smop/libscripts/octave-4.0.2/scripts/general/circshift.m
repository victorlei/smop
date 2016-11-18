## Copyright (C) 2004-2015 David Bateman
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
## @deftypefn {Function File} {@var{y} =} circshift (@var{x}, @var{n})
## Circularly shift the values of the array @var{x}.
##
## @var{n} must be a vector of integers no longer than the number of
## dimensions in @var{x}.  The values of @var{n} can be either positive or
## negative, which determines the direction in which the values or @var{x}
## are shifted.  If an element of @var{n} is zero, then the corresponding
## dimension of @var{x} will not be shifted.  For example:
##
## @example
## @group
## x = [1, 2, 3; 4, 5, 6; 7, 8, 9];
## circshift (x, 1)
## @result{}  7, 8, 9
##     1, 2, 3
##     4, 5, 6
## circshift (x, -2)
## @result{}  7, 8, 9
##     1, 2, 3
##     4, 5, 6
## circshift (x, [0,1])
## @result{}  3, 1, 2
##     6, 4, 5
##     9, 7, 8
## @end group
## @end example
## @seealso{permute, ipermute, shiftdim}
## @end deftypefn

function y = circshift (x, n)

  if (nargin != 2)
    print_usage ();
  endif

  if (isempty (x))
    y = x;
    return;
  endif

  nd = ndims (x);
  sz = size (x);

  if (! isvector (n) || length (n) > nd)
    error ("circshift: N must be a vector, no longer than the number of dimension in X");
  elseif (any (n != fix (n)))
    error ("circshift: all values of N must be integers");
  endif

  idx = repmat ({':'}, 1, nd);
  for i = 1:length (n);
    b = n(i);
    d = sz(i);
    if (b > 0)
      b = rem (b, d);
      idx{i} = [d-b+1:d, 1:d-b];
    elseif (b < 0)
      b = rem (abs (b), d);
      idx{i} = [b+1:d, 1:b];
    endif
  endfor

  y = x(idx{:});

endfunction


%!shared x
%! x = [1, 2, 3; 4, 5, 6; 7, 8, 9];

%!assert (circshift (x, 1), [7, 8, 9; 1, 2, 3; 4, 5, 6])
%!assert (circshift (x, -2), [7, 8, 9; 1, 2, 3; 4, 5, 6])
%!assert (circshift (x, [0, 1]), [3, 1, 2; 6, 4, 5; 9, 7, 8])
%!assert (circshift ([], 1), [])

%!assert (circshift (eye (3), 1), circshift (eye (3), 1))
%!assert (circshift (eye (3), 1), [0,0,1;1,0,0;0,1,0])

## Test input validation
%!error circshift ()
%!error circshift (1)
%!error circshift (1,2,3)
%!error circshift (1, ones (2,2))
%!error circshift (1, [1 2 3])
%!error circshift (1, 1.5)

