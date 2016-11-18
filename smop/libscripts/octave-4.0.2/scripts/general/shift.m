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
## @deftypefn  {Function File} {} shift (@var{x}, @var{b})
## @deftypefnx {Function File} {} shift (@var{x}, @var{b}, @var{dim})
## If @var{x} is a vector, perform a circular shift of length @var{b} of
## the elements of @var{x}.
##
## If @var{x} is a matrix, do the same for each column of @var{x}.
##
## If the optional @var{dim} argument is given, operate along this dimension.
## @end deftypefn

## Author: AW <Andreas.Weingessel@ci.tuwien.ac.at>
## Created: 14 September 1994
## Adapted-By: jwe

function y = shift (x, b, dim)

  if (nargin != 2 && nargin != 3)
    print_usage ();
  endif

  if (numel (x) < 1)
    error ("shift: X must not be empty");
  elseif (! (isscalar (b) && b == fix (b)))
    error ("shift: B must be an integer");
  endif

  nd = ndims (x);
  sz = size (x);

  if (nargin == 3)
    if (!(isscalar (dim) && dim == fix (dim))
        || !(1 <= dim && dim <= nd))
      error ("shift: DIM must be an integer and a valid dimension");
    endif
  else
    ## Find the first non-singleton dimension.
    (dim = find (sz > 1, 1)) || (dim = 1);
  endif

  d = sz(dim);

  idx = repmat ({':'}, nd, 1);
  if (b > 0)
    b = rem (b, d);
    idx{dim} = [d-b+1:d, 1:d-b];
  elseif (b < 0)
    b = rem (abs (b), d);
    idx{dim} = [b+1:d, 1:b];
  endif

  y = x(idx{:});

endfunction


%!test
%! a = [1, 2, 3];
%! b = [4, 5, 6];
%! c = [7, 8, 9];
%!
%! r = [a, b, c];
%! m = [a; b; c];
%!
%! assert (shift (r, 0), r);
%! assert (shift (r, 3), [c, a, b]);
%! assert (shift (r, -6), [c, a, b]);
%! assert (shift (r, -3), [b, c, a]);
%! assert (shift (m, 1), [c; a; b]);
%! assert (shift (m, -2), [c; a; b]);

## Test input validation
%!error shift ()
%!error shift (1, 2, 3, 4)
%!error shift ([], 1)
%!error shift (ones (2), ones (2))
%!error shift (ones (2), 1.5)
%!error shift (1, 1, 1.5)
%!error shift (1, 1, 0)
%!error shift (1, 1, 3)

