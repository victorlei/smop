## Copyright (C) 1995-2015 Friedrich Leisch
## Copyright (C) 2010 Alois Schloegl
## Copyright (C) 2014-2015 Drew Abbot
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
## @deftypefn  {Function File} {[@var{Pxx}, @var{w}] =} periodogram (@var{x})
## @deftypefnx {Function File} {[@var{Pxx}, @var{w}] =} periodogram (@var{x}, @var{win})
## @deftypefnx {Function File} {[@var{Pxx}, @var{w}] =} periodogram (@var{x}, @var{win}, @var{nfft})
## @deftypefnx {Function File} {[@var{Pxx}, @var{f}] =} periodogram (@var{x}, @var{win}, @var{nfft}, @var{Fs})
## @deftypefnx {Function File} {[@var{Pxx}, @var{f}] =} periodogram (@dots{}, "@var{range}")
## @deftypefnx {Function File} {} periodogram (@dots{})
## Return the periodogram (Power Spectral Density) of @var{x}.
##
## The possible inputs are:
##
## @table @var
## @item x
##
## data vector.  If @var{x} is real-valued a one-sided spectrum is estimated.
## If @var{x} is complex-valued, or @qcode{"@var{range}"} specifies
## @qcode{"@nospell{twosided}"}, the full spectrum is estimated.
##
## @item win
## window weight data.  If window is empty or unspecified a default rectangular
## window is used.  Otherwise, the window is applied to the signal
## (@code{@var{x} .* @var{win}}) before computing the periodogram.  The window
## data must be a vector of the same length as @var{x}.
##
## @item nfft
## number of frequency bins.  The default is 256 or the next higher power of
## 2 greater than the length of @var{x}
## (@code{max (256, 2.^nextpow2 (length (x)))}).  If @var{nfft} is greater
## than the length of the input then @var{x} will be zero-padded to the length
## of @var{nfft}.
##
## @item Fs
## sampling rate.  The default is 1.
##
## @item range
## range of spectrum.  @qcode{"@nospell{onesided}"} computes spectrum from
## [0..nfft/2+1].  @qcode{"@nospell{twosided}"} computes spectrum from
## [0..nfft-1].
## @end table
##
## The optional second output @var{w} are the normalized angular frequencies.
## For a one-sided calculation @var{w} is in the range [0, pi] if @var{nfft}
## is even and [0, pi) if @var{nfft} is odd.  Similarly, for a two-sided
## calculation @var{w} is in the range [0, 2*pi] or [0, 2*pi) depending on
## @var{nfft}.
##
## If a sampling frequency is specified, @var{Fs}, then the output frequencies
## @var{f} will be in the range [0, @var{Fs}/2] or [0, @var{Fs}/2) for
## one-sided calculations.  For two-sided calculations the range will be
## [0, @var{Fs}).
##
## When called with no outputs the periodogram is immediately plotted in the
## current figure window.
## @seealso{fft}
## @end deftypefn

## Author: FL <Friedrich.Leisch@ci.tuwien.ac.at>
## Description: Compute the periodogram

function [pxx, f] = periodogram (x, varargin)

  ## check input arguments
  if (nargin < 1 || nargin > 5)
    print_usage ();
  endif

  nfft = fs = range = window = [];
  j = 2;
  for k = 1:length (varargin)
    if (ischar (varargin{k}))
      range = varargin{k};
    else
      switch (j)
        case 2
          window = varargin{k};
        case 3
          nfft   = varargin{k};
        case 4
          fs     = varargin{k};
      endswitch
      j++;
    endif
  endfor

  if (! isvector (x))
    error ("periodogram: X must be a real or complex vector");
  endif
  x = x(:);  # Use column vectors from now on

  n = rows (x);

  if (! isempty (window))
    if (! isvector (window) || length (window) != n)
      error ("periodogram: WIN must be a vector of the same length as X");
    endif
    window = window(:);
    x .*= window;
  endif

  if (isempty (nfft))
    nfft = max (256, 2.^nextpow2 (n));
  elseif (! isscalar (nfft))
    error ("periodogram: NFFT must be a scalar");
  endif

  use_w_freq = isempty (fs);
  if (! use_w_freq && ! isscalar (fs))
    error ("periodogram: FS must be a scalar");
  endif

  if (strcmpi (range, "onesided"))
    range = 1;
  elseif (strcmpi (range, "twosided"))
    range = 2;
  elseif (strcmpi (range, "centered"))
    error ('periodogram: "centered" range type is not implemented');
  else
    range = 2-isreal (x);
  endif

  ## compute periodogram

  if (n > nfft)
    Pxx = 0;
    rr = rem (length (x), nfft);
    if (rr)
      x = [x(:); zeros(nfft-rr, 1)];
    endif
    x = sum (reshape (x, nfft, []), 2);
  endif

  if (! isempty (window))
    n = sumsq (window);
  endif;
  Pxx = (abs (fft (x, nfft))) .^ 2 / n;

  if (use_w_freq)
    Pxx /= 2*pi;
  else
    Pxx /= fs;
  endif

  ## generate output arguments

  if (range == 1)  # onesided
    if (! rem (nfft,2))  # nfft is even
      psd_len = nfft/2+1;
      Pxx = Pxx(1:psd_len) + [0; Pxx(nfft:-1:psd_len+1); 0];
    else                 # nfft is odd
      psd_len = (nfft+1)/2;
      Pxx = Pxx(1:psd_len) + [0; Pxx(nfft:-1:psd_len+1)];
    endif
  endif

  if (nargout != 1)
    if (range == 1)
      f = (0:nfft/2)' / nfft;
    elseif (range == 2)
      f = (0:nfft-1)' / nfft;
    endif
    if (use_w_freq)
      f *= 2*pi;  # generate w=2*pi*f
    else
      f *= fs;
    endif
  endif

  if (nargout == 0)
    if (use_w_freq)
      plot (f/(2*pi), 10*log10 (Pxx));
      xlabel ("normalized frequency [x pi rad]");
      ylabel ("Power density [dB/rad/sample]");
    else
      plot (f, 10*log10 (Pxx));
      xlabel ("frequency [Hz]");
      ylabel ("Power density [dB/Hz]");
    endif
    grid on;
    title ("Periodogram Power Spectral Density Estimate");
  else
    pxx = Pxx;
  endif

endfunction


## FIXME: Need some functional tests


## Test input validation
%!error periodogram ()
%!error periodogram (1,2,3,4,5,6)
%!error <X must be a real or complex vector> periodogram (ones (2,2))
%!error <WIN must be a vector.*same length> periodogram (1:5, ones (2,2))
%!error <WIN must be a vector.*same length> periodogram (1:5, 1:6)
%!error <NFFT must be a scalar> periodogram (1:5, 1:5, 1:5)
%!error <FS must be a scalar> periodogram (1:5, [], [], 1:5)
%!error <"centered" range type is not implemented> periodogram (1:5, "centered")

