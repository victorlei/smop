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
## @deftypefn  {Function File} {[@var{h}, @var{w}] =} freqz (@var{b}, @var{a}, @var{n}, "whole")
## @deftypefnx {Function File} {[@var{h}, @var{w}] =} freqz (@var{b})
## @deftypefnx {Function File} {[@var{h}, @var{w}] =} freqz (@var{b}, @var{a})
## @deftypefnx {Function File} {[@var{h}, @var{w}] =} freqz (@var{b}, @var{a}, @var{n})
## @deftypefnx {Function File} {@var{h} =} freqz (@var{b}, @var{a}, @var{w})
## @deftypefnx {Function File} {[@var{h}, @var{w}] =} freqz (@dots{}, @var{Fs})
## @deftypefnx {Function File} {} freqz (@dots{})
##
## Return the complex frequency response @var{h} of the rational IIR filter
## whose numerator and denominator coefficients are @var{b} and @var{a},
## respectively.
##
## The response is evaluated at @var{n} angular frequencies between 0 and
## @ifnottex
## 2*pi.
## @end ifnottex
## @tex
## $2\pi$.
## @end tex
##
## @noindent
## The output value @var{w} is a vector of the frequencies.
##
## If @var{a} is omitted, the denominator is assumed to be 1 (this
## corresponds to a simple FIR filter).
##
## If @var{n} is omitted, a value of 512 is assumed.  For fastest computation,
## @var{n} should factor into a small number of small primes.
##
## If the fourth argument, @qcode{"whole"}, is omitted the response is
## evaluated at frequencies between 0 and
## @ifnottex
## pi.
## @end ifnottex
## @tex
## $\pi$.
## @end tex
##
## @code{freqz (@var{b}, @var{a}, @var{w})}
##
## Evaluate the response at the specific frequencies in the vector @var{w}.
## The values for @var{w} are measured in radians.
##
## @code{[@dots{}] = freqz (@dots{}, @var{Fs})}
##
## Return frequencies in Hz instead of radians assuming a sampling rate
## @var{Fs}.  If you are evaluating the response at specific frequencies
## @var{w}, those frequencies should be requested in Hz rather than radians.
##
## @code{freqz (@dots{})}
##
## Plot the magnitude and phase response of @var{h} rather than returning them.
##
## @seealso{freqz_plot}
## @end deftypefn

## Author: jwe ???

function [h_r, f_r] = freqz (b, a, n, region, Fs)

  if (nargin < 1 || nargin > 5)
    print_usage ();
  elseif (nargin == 1)
    ## Response of an FIR filter.
    a = n = region = Fs = [];
  elseif (nargin == 2)
    ## Response of an IIR filter
    n = region = Fs = [];
  elseif (nargin == 3)
    region = Fs = [];
  elseif (nargin == 4)
    Fs = [];
    if (! ischar (region) && ! isempty (region))
      Fs = region;
      region = [];
    endif
  endif

  if (isempty (b))
    b = 1;
  elseif (! isvector (b))
    error ("freqz: B must be a vector");
  endif
  if (isempty (a))
    a = 1;
  elseif (! isvector (a))
    error ("freqz: A must be a vector");
  endif
  if (isempty (n))
    n = 512;
  elseif (isscalar (n) && n < 1)
    error ("freqz: N must be a positive integer");
  endif
  if (isempty (region))
    if (isreal (b) && isreal (a))
      region = "half";
    else
      region = "whole";
    endif
  endif
  if (isempty (Fs))
    freq_norm = true;
    if (nargout == 0)
      Fs = 2;
    else
      Fs = 2*pi;
    endif
  else
    freq_norm = false;
  endif
  plot_output = (nargout == 0);
  whole_region = strcmp (region, "whole");

  a = a(:);
  b = b(:);

  if (! isscalar (n))
    ## Explicit frequency vector given
    w = f = n;
    if (nargin == 4)
      ## Sampling rate Fs was specified
      w = 2*pi*f/Fs;
    endif
    k = max (length (b), length (a));
    hb = polyval (postpad (b, k), exp (j*w));
    ha = polyval (postpad (a, k), exp (j*w));
  else
    ## polyval(fliplr(P),exp(jw)) is O(p n) and fft(x) is O(n log(n)),
    ## where p is the order of the polynomial P.  For small p it
    ## would be faster to use polyval but in practice the overhead for
    ## polyval is much higher and the little bit of time saved isn't
    ## worth the extra code.
    k = max (length (b), length (a));
    if (k > n/2 && nargout == 0)
      ## Ensure a causal phase response.
      n = n * 2 .^ ceil (log2 (2*k/n));
    endif

    if (whole_region)
      N = n;
      if (plot_output)
        f = Fs * (0:n).' / N;    # do 1 more for the plot
      else
        f = Fs * (0:n-1).' / N;
      endif
    else
      N = 2*n;
      if (plot_output)
        n++;
      endif
      f = Fs * (0:n-1).' / N;
    endif

    pad_sz = N*ceil (k/N);
    b = postpad (b, pad_sz);
    a = postpad (a, pad_sz);

    hb = zeros (n, 1);
    ha = zeros (n, 1);

    for i = 1:N:pad_sz
      hb = hb + fft (postpad (b(i:i+N-1), N))(1:n);
      ha = ha + fft (postpad (a(i:i+N-1), N))(1:n);
    endfor

  endif

  h = hb ./ ha;

  if (plot_output)
    ## Plot and don't return values.
    if (whole_region)
      h(end+1) = h(1); # Solution is periodic.  Copy first value to end.
    endif
    freqz_plot (f, h, freq_norm);
  else
    ## Return values and don't plot.
    h_r = h;
    f_r = f;
  endif

endfunction


%!test # correct values and fft-polyval consistency
%! ## butterworth filter, order 2, cutoff pi/2 radians
%! b = [0.292893218813452  0.585786437626905  0.292893218813452];
%! a = [1  0  0.171572875253810];
%! [h,w] = freqz (b,a,32);
%! assert (h(1),1,10*eps);
%! assert (abs (h(17)).^2,0.5,10*eps);
%! assert (h,freqz (b,a,w),10*eps); # fft should be consistent with polyval

%!test # whole-half consistency
%! b = [1 1 1]/3; # 3-sample average
%! [h,w] = freqz (b,1,32,"whole");
%! assert (h(2:16),conj (h(32:-1:18)),20*eps);
%! [h2,w2] = freqz (b,1,16,"half");
%! assert (h(1:16),h2,20*eps);
%! assert (w(1:16),w2,20*eps);

%!test # Sampling frequency properly interpreted
%! b = [1 1 1]/3; a = [1 0.2];
%! [h,f] = freqz (b,a,16,320);
%! assert (f,[0:15]'*10,10*eps);
%! [h2,f2] = freqz (b,a,[0:15]*10,320);
%! assert (f2,[0:15]*10,10*eps);
%! assert (h,h2.',20*eps);
%! [h3,f3] = freqz (b,a,32,"whole",320);
%! assert (f3,[0:31]'*10,10*eps);

## Test input validation
## FIXME: Need to put tests here and simplify input validation in the main code.

