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
## @deftypefn  {Function File} {} fftfilt (@var{b}, @var{x})
## @deftypefnx {Function File} {} fftfilt (@var{b}, @var{x}, @var{n})
## Filter @var{x} with the FIR filter @var{b} using the FFT.
##
## If @var{x} is a matrix, filter each column of the matrix.
##
## Given the optional third argument, @var{n}, @code{fftfilt} uses the
## overlap-add method to filter @var{x} with @var{b} using an N-point FFT@.
## The FFT size must be an even power of 2 and must be greater than or equal to
## the length of @var{b}.  If the specified @var{n} does not meet these
## criteria, it is automatically adjusted to the nearest value that does.
##
## @seealso{filter, filter2}
## @end deftypefn

## Author: Kurt Hornik <Kurt.Hornik@wu-wien.ac.at>
## Created: 3 September 1994
## Adapted-By: jwe

function y = fftfilt (b, x, n)

  ## If N is not specified explicitly, we do not use the overlap-add
  ## method at all because loops are really slow.  Otherwise, we only
  ## ensure that the number of points in the FFT is the smallest power
  ## of two larger than N and length(b).  This could result in length
  ## one blocks, but if the user knows better ...

  if (nargin < 2 || nargin > 3)
    print_usage ();
  endif

  transpose = (rows (x) == 1);

  if (transpose)
    x = x.';
  endif

  [r_x, c_x] = size (x);
  [r_b, c_b] = size (b);

  if (! isvector (b))
    error ("fftfilt: B must be a vector");
  endif

  if (ndims (x) != 2)
    error ("fftfilt: X must be a 1-D or 2-D array");
  endif

  l_b = r_b * c_b;
  b = reshape (b, l_b, 1);

  if (nargin == 2)
    ## Use FFT with the smallest power of 2 which is >= length (x) +
    ## length (b) - 1 as number of points ...
    n = 2 ^ nextpow2 (r_x + l_b - 1);
    B = fft (b, n);
    y = ifft (fft (x, n) .* B(:, ones (1, c_x)));
  else
    ## Use overlap-add method ...
    if (! (isscalar (n)))
      error ("fftfilt: N has to be a scalar");
    endif
    n = 2 ^ nextpow2 (max ([n, l_b]));
    L = n - l_b + 1;
    B = fft (b, n);
    B = B(:, ones (c_x,1));
    R = ceil (r_x / L);
    y = zeros (r_x, c_x);
    for r = 1:R;
      lo = (r - 1) * L + 1;
      hi = min (r * L, r_x);
      tmp = zeros (n, c_x);
      tmp(1:(hi-lo+1),:) = x(lo:hi,:);
      tmp = ifft (fft (tmp) .* B);
      hi  = min (lo+n-1, r_x);
      y(lo:hi,:) = y(lo:hi,:) + tmp(1:(hi-lo+1),:);
    endfor
  endif

  y = y(1:r_x, :);

  ## Final cleanups:

  ## - If both b and x are real, y should be real.
  ## - If b is real and x is imaginary, y should be imaginary.
  ## - If b is imaginary and x is real, y should be imaginary.
  ## - If both b and x are imaginary, y should be real.
  xisreal = all (imag (x) == 0);
  xisimag = all (real (x) == 0);

  if (all (imag (b) == 0))
    y (:,xisreal) = real (y (:,xisreal));
    y (:,xisimag) = complex (real (y (:,xisimag)) * 0, imag (y (:,xisimag)));
  elseif (all (real (b) == 0))
    y (:,xisreal) = complex (real (y (:,xisreal)) * 0, imag (y (:,xisreal)));
    y (:,xisimag) = real (y (:,xisimag));
  endif

  ## - If both x and b are integer in both real and imaginary
  ##   components, y should be integer.
  if (! any (b - fix (b)))
    idx = find (! any (x - fix (x)));
    y (:, idx) = round (y (:, idx));
  endif

  ## Transpose after cleanup, otherwise rounding fails.
  if (transpose)
    y = y.';
  endif

endfunction


%!shared b, x, r

%!test
%! b = [1 1];
%! x = [1, zeros(1,9)];
%! assert (fftfilt (b,  x  ), [1 1 0 0 0 0 0 0 0 0]  );
%! assert (fftfilt (b,  x.'), [1 1 0 0 0 0 0 0 0 0].');
%! assert (fftfilt (b.',x  ), [1 1 0 0 0 0 0 0 0 0]  );
%! assert (fftfilt (b.',x.'), [1 1 0 0 0 0 0 0 0 0].');
%! assert (fftfilt (b,  [x.' x.']), [1 1 0 0 0 0 0 0 0 0].'*[1 1]);
%! assert (fftfilt (b,  [x.'+2*eps x.']) == [1 1 0 0 0 0 0 0 0 0].'*[1 1], [false(10, 1) true(10, 1)]);

%!test
%! r = sqrt (1/2) * (1+i);
%! b = b*r;
%! assert (fftfilt (b, x  ), r*[1 1 0 0 0 0 0 0 0 0]  , eps  );
%! assert (fftfilt (b, r*x), r*r*[1 1 0 0 0 0 0 0 0 0], 2*eps);
%! assert (fftfilt (b, x.'), r*[1 1 0 0 0 0 0 0 0 0].', eps  );

%!test
%! b  = [1 1];
%! x  = zeros (10,3); x(1,1)=-1; x(1,2)=1;
%! y0 = zeros (10,3); y0(1:2,1)=-1; y0(1:2,2)=1;
%! y  = fftfilt (b, x);
%! assert (y0, y);
%! y  = fftfilt (b*i, x);
%! assert (y0*i, y);
%! y  = fftfilt (b, x*i);
%! assert (y0*i, y);
%! y  = fftfilt (b*i, x*i);
%! assert (-y0, y);
%! x  = rand (10, 1);
%! y  = fftfilt (b, [x x*i]);
%! assert (true, isreal (y(:,1)));
%! assert (false, any (real (y(:,2))));

%!test
%! b  = rand (10, 1);
%! x  = rand (10, 1);
%! y0 = filter (b, 1, x);
%! y  = fftfilt (b, x);
%! assert (y0, y, 16*eps);
%! y0 = filter (b*i, 1, x*i);
%! y  = fftfilt (b*i, x*i);
%! assert (y0, y, 16*eps);

%!test
%! b  = rand (10, 1) + i*rand (10, 1);
%! x  = rand (10, 1) + i*rand (10, 1);
%! y0 = filter (b, 1, x);
%! y  = fftfilt (b, x);
%! assert (y0, y, 55*eps);

## Test input validation
%!error fftfilt (1)
%!error fftfilt (1, 2, 3, 4)
%!error fftfilt (ones (2), 1)
%!error fftfilt (2, ones (3,3,3))
%!error fftfilt (2, 1, ones (2))

