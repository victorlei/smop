## Copyright (C) 1997-2015 Vincent Cautaerts
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
## @deftypefn  {Function File} {} fftshift (@var{x})
## @deftypefnx {Function File} {} fftshift (@var{x}, @var{dim})
## Perform a shift of the vector @var{x}, for use with the @code{fft} and
## @code{ifft} functions, in order the move the frequency 0 to the center of
## the vector or matrix.
##
## If @var{x} is a vector of @math{N} elements corresponding to @math{N} time
## samples spaced by @nospell{@math{dt}}, then
## @code{fftshift (fft (@var{x}))} corresponds to frequencies
##
## @example
## f = [ -(ceil((N-1)/2):-1:1)*df 0 (1:floor((N-1)/2))*df ]
## @end example
##
## @noindent
## where @nospell{@math{df} = 1 / @math{dt}}.
##
## If @var{x} is a matrix, the same holds for rows and columns.  If @var{x}
## is an array, then the same holds along each dimension.
##
## The optional @var{dim} argument can be used to limit the dimension along
## which the permutation occurs.
## @seealso{ifftshift}
## @end deftypefn

## Author: Vincent Cautaerts <vincent@comf5.comm.eng.osaka-u.ac.jp>
## Created: July 1997
## Adapted-By: jwe

function retval = fftshift (x, dim)

  if (nargin != 1 && nargin != 2)
    print_usage ();
  endif

  if (! (isnumeric (x) || islogical (x) || ischar (x)))
    error ("fftshift: X must be a vector or matrix");
  endif

  if (nargin == 2)
    if (! (isscalar (dim) && dim > 0 && dim == fix (dim)))
      error ("fftshift: dimension DIM must be a positive integer");
    endif
    nd = ndims (x);
    sz = size (x);
    sz2 = ceil (sz(dim) / 2);
    idx = cell ();
    idx = repmat ({':'}, nd, 1);
    idx{dim} = [sz2+1:sz(dim), 1:sz2];
    retval = x(idx{:});
  else
    if (isvector (x))
      xl = length (x);
      xx = ceil (xl/2);
      retval = x([xx+1:xl, 1:xx]);
    else
      nd = ndims (x);
      sz = size (x);
      sz2 = ceil (sz ./ 2);
      idx = cell ();
      for i = 1:nd
        idx{i} = [sz2(i)+1:sz(i), 1:sz2(i)];
      endfor
      retval = x(idx{:});
    endif
  endif

endfunction


%!test
%! x = [0:7];
%! y = fftshift (x);
%! assert (y, [4 5 6 7 0 1 2 3]);
%! assert (fftshift (y), x);

%!test
%! x = [0:6];
%! y = fftshift (x);
%! assert (y, [4 5 6 0 1 2 3]);
%! assert (fftshift (y), [1 2 3 4 5 6 0]);

%!test
%! x = [0:7]';
%! y = fftshift (x);
%! assert (y, [4;5;6;7;0;1;2;3]);
%! assert (fftshift (y), x);

%!test
%! x = [0:6]';
%! y = fftshift (x);
%! assert (y, [4;5;6;0;1;2;3]);
%! assert (fftshift (y), [1;2;3;4;5;6;0]);

%!test
%! x = [0:3];
%! x = [x;2*x;3*x+1;4*x+1];
%! y = fftshift (x);
%! assert (y, [[7 10 1 4];[9 13 1 5];[2 3 0 1];[4 6 0 2]]);
%! assert (fftshift (y), x);

%!test
%! x = [0:3];
%! x = [x;2*x;3*x+1;4*x+1];
%! y = fftshift (x,1);
%! assert (y, [[1 4 7 10];[1 5 9 13];[0 1 2 3];[0 2 4 6]]);
%! assert (fftshift (y,1), x);

%!test
%! x = [0:3];
%! x = [x;2*x;3*x+1;4*x+1];
%! y = fftshift (x,2);
%! assert (y, [[2 3 0 1];[4 6 0 2];[7 10 1 4];[9 13 1 5]]);
%! assert (fftshift (y,2), x);

%!test
%! x = "abcdefg";
%! y = fftshift (x);
%! assert (y, "efgabcd");
%! assert (fftshift (y), "bcdefga");

## Test N-dimensional input (bug #45207)
%!test
%! x = [0:3];
%! x = x + x' + reshape (x, [1 1 4]);
%! y1 = [4 5 2 3; 5 6 3 4; 2 3 0 1; 3 4 1 2];
%! y = fftshift (x);
%! assert (y, reshape ([y1 + 2, y1 + 3, y1, y1 + 1], [4 4 4]));
%! assert (fftshift (y), x);

%% Test input validation
%!error fftshift ()
%!error fftshift (1, 2, 3)
%!error fftshift (0:3, -1)
%!error fftshift (0:3, 0:3)

