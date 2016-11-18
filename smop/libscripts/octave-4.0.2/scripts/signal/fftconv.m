## Copyright (C) 1994-2015 John W. Eaton
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
## @deftypefn  {Function File} {} fftconv (@var{x}, @var{y})
## @deftypefnx {Function File} {} fftconv (@var{x}, @var{y}, @var{n})
## Convolve two vectors using the FFT for computation.
##
## @code{c = fftconv (@var{x}, @var{y})} returns a vector of length equal to
## @code{length (@var{x}) + length (@var{y}) - 1}.  If @var{x} and @var{y}
## are the coefficient vectors of two polynomials, the returned value is the
## coefficient vector of the product polynomial.
##
## The computation uses the FFT by calling the function @code{fftfilt}.  If
## the optional argument @var{n} is specified, an N-point FFT is used.
## @seealso{deconv, conv, conv2}
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Created: 3 September 1994
## Adapted-By: jwe

function c = fftconv (x, y, n)

  if (nargin < 2 || nargin > 3)
    print_usage ();
  endif

  if (! (isvector (x) && isvector (y)))
    error ("fftconv: both A and B must be vectors");
  endif
  la = length (x);
  lb = length (y);
  if ((la == 1) || (lb == 1))
    c = x * y;
  else
    lc = la + lb - 1;
    x(lc) = 0;
    y(lc) = 0;
    if (nargin == 2)
      c = fftfilt (x, y);
    else
      if (! isscalar (n))
        error ("fftconv: N must be a scalar");
      endif
      c = fftfilt (x, y, n);
    endif
  endif

endfunction


## FIXME: Borrow tests from conv.m.  May need tolerance on the assert stmt.
%!test
%! x = ones (3,1);
%! y = ones (1,3);
%! b = 2;
%! c = 3;
%! assert (fftconv (x, x), [1; 2; 3; 2; 1], 5*eps);
%! assert (fftconv (y, y), [1, 2, 3, 2, 1], 5*eps);
%! assert (fftconv (x, y), [1, 2, 3, 2, 1], 5*eps);
%! assert (fftconv (y, x), [1; 2; 3; 2; 1], 5*eps);
%! assert (fftconv (c, x), [3; 3; 3], 5*eps);
%! assert (fftconv (c, y), [3, 3, 3], 5*eps);
%! assert (fftconv (x, c), [3; 3; 3], 5*eps);
%! assert (fftconv (y, c), [3, 3, 3], 5*eps);
%! assert (fftconv (b, c), 6, 5*eps);

%!test
%! a = 1:10;
%! b = 1:3;
%! assert (size (conv (a,b)), [1, numel(a)+numel(b)-1]);
%! assert (size (conv (b,a)), [1, numel(a)+numel(b)-1]);

%! a = (1:10).';
%! b = 1:3;
%! assert (size (conv (a,b)), [numel(a)+numel(b)-1, 1]);
%! assert (size (conv (b,a)), [numel(a)+numel(b)-1, 1]);

%!test
%! a = 1:10;
%! b = (1:3).';
%! assert (size (conv (a,b)), [1, numel(a)+numel(b)-1]);
%! assert (size (conv (b,a)), [1, numel(a)+numel(b)-1]);

## Test input validation
%!error fftconv (1)
%!error fftconv (1,2,3,4)
%!error fftconv ([1, 2; 3, 4], 3)
%!error fftconv (2, [])
%!error fftconv ([1,1], [2,2] , [3, 4])

