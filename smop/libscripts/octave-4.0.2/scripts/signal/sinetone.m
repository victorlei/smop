## Copyright (C) 1995-2015 Friedrich Leisch
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
## @deftypefn {Function File} {} sinetone (@var{freq}, @var{rate}, @var{sec}, @var{ampl})
## Return a sinetone of frequency @var{freq} with a length of @var{sec}
## seconds at sampling rate @var{rate} and with amplitude @var{ampl}.
##
## The arguments @var{freq} and @var{ampl} may be vectors of common size.
##
## The defaults are @var{rate} = 8000, @var{sec} = 1, and @var{ampl} = 64.
## @seealso{sinewave}
## @end deftypefn

## Author: FL <Friedrich.Leisch@ci.tuwien.ac.at>
## Description: Compute a sine tone

function retval = sinetone (freq, rate = 8000, sec = 1, ampl = 64)

  if (nargin < 1 || nargin > 4)
    print_usage ();
  endif

  [err, freq, ampl] = common_size (freq, ampl);
  if (err || ! isvector (freq))
    error ("sinetone: FREQ and AMPL must be vectors of common size");
  endif

  if (! (isscalar (rate) && isscalar (sec)))
    error ("sinetone: RATE and SEC must be scalars");
  endif

  n = length (freq);
  ns = round (rate * sec);

  retval = zeros (ns, n);

  for k = 1:n
    retval(:, k) = ampl(k) * sin (2 * pi * (1:ns) / rate * freq(k))';
  endfor

endfunction


%!assert (size (sinetone (18e6, 150e6, 19550/150e6, 1)), [19550, 1])

