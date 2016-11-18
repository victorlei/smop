## Copyright (C) 2002-2015 John W. Eaton
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
## @deftypefn  {Function File} {} freqz_plot (@var{w}, @var{h})
## @deftypefnx {Function File} {} freqz_plot (@var{w}, @var{h}, @var{freq_norm})
## Plot the magnitude and phase response of @var{h}.
##
## If the optional @var{freq_norm} argument is true, the frequency vector
## @var{w} is in units of normalized radians.  If @var{freq_norm} is false, or
## not given, then @var{w} is measured in Hertz.
## @seealso{freqz}
## @end deftypefn

## Author: Paul Kienzle <pkienzle@users.sf.net>

function freqz_plot (w, h, freq_norm = false)

  if (nargin < 2 || nargin > 3)
    print_usage ();
  endif

  n = length (w);

  ## ## exclude zero-frequency
  ## h = h (2 : length (h));
  ## w = w (2 : length (w));
  ## n = n-1;

  mag = 20 * log10 (abs (h));
  phase = unwrap (arg (h));

  if (freq_norm)
    x_label = 'Normalized Frequency (\times\pi rad/sample)';
  else
    x_label = "Frequency (Hz)";
  endif

  subplot (2, 1, 1);
  plot (w, mag);
  grid ("on");
  axis ([w(1), w(n)], "autoy");
  xlabel (x_label);
  ylabel ("Magnitude (dB)");

  subplot (2, 1, 2);
  plot (w, phase*360/(2*pi));
  grid ("on");
  axis ([w(1), w(n)], "autoy");
  xlabel (x_label);
  ylabel ("Phase (degrees)");

endfunction

