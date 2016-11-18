## Copyright (C) 2004-2015 David Bateman
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
## @deftypefn  {Function File} {} flip (@var{x})
## @deftypefnx {Function File} {} flip (@var{x}, @var{dim})
## Flip array across dimension @var{dim}.
##
## Return a copy of @var{x} flipped about the dimension @var{dim}.
## @var{dim} defaults to the first non-singleton dimension.
## For example:
##
## @example
## @group
## flip ([1  2  3  4])
##       @result{}  4  3  2  1
##
## flip ([1; 2; 3; 4])
##       @result{}  4
##           3
##           2
##           1
##
## flip ([1 2; 3 4])
##       @result{}  3  4
##           1  2
##
## flip ([1 2; 3 4], 2)
##       @result{}  2  1
##           4  3
## @end group
## @end example
##
## @seealso{fliplr, flipud, rot90, rotdim, permute, transpose}
## @end deftypefn

## Author: David Bateman, Jaroslav Hajek

function y = flip (x, dim)

  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif

  nd = ndims (x);
  sz = size (x);
  if (nargin == 1)
    ## Find the first non-singleton dimension.
    (dim = find (sz > 1, 1)) || (dim = 1);
  elseif (! (isscalar (dim) && isindex (dim)))
    error ("flip: DIM must be a positive integer");
  endif

  idx(1:max(nd, dim)) = {':'};
  idx{dim} = size (x, dim):-1:1;
  y = x(idx{:});

endfunction


%!assert (flip ([1 2; 3 4], 2), [2 1; 4 3])
%!assert (flip ([1 2; 3 4], 3), [1 2; 3 4])

## Test defaults
%!assert (flip ([1 2 3 4]), [4 3 2 1])
%!assert (flip ([1 2 3 4].'), [4 3 2 1].')
%!assert (flip ([1 2; 3 4]), flip ([1 2 ; 3 4], 1))

## Test NDArrays
%!test
%! a(1:2,1:2,1) = [1 2; 3 4];
%! a(1:2,1:2,2) = [5 6; 7 8];
%! b(1:2,1:2,1) = [5 6; 7 8];
%! b(1:2,1:2,2) = [1 2; 3 4];
%! assert (flip (a, 3), b)

%!test
%! a = b = zeros (2, 2, 1, 2);
%! a(1:2,1:2,:,1) = [1 2; 3 4];
%! a(1:2,1:2,:,2) = [5 6; 7 8];
%! b(1:2,1:2,:,1) = [5 6; 7 8];
%! b(1:2,1:2,:,2) = [1 2; 3 4];
%! assert (flip (a, 3), a)
%! assert (flip (a, 4), b)
%! assert (flip (a, 5), a)

%!error flip ()
%!error flip (1, 2, 3)
%!error <DIM must be a positive integer> flip (magic (3), -1)

