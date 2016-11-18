## Copyright (C) 1995-2015 Andreas Weingessel
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
## @deftypefn {Function File} {@var{x} =} synthesis (@var{y}, @var{c})
## Compute a signal from its short-time Fourier transform @var{y} and a
## 3-element vector @var{c} specifying window size, increment, and window type.
##
## The values @var{y} and @var{c} can be derived by
##
## @example
## [@var{y}, @var{c}] = stft (@var{x} , @dots{})
## @end example
## @seealso{stft}
## @end deftypefn

## Author: AW <Andreas.Weingessel@ci.tuwien.ac.at>
## Description: Recover a signal from its short-term Fourier transform

function x = synthesis (y, c)

  if (nargin != 2)
    print_usage ();
  endif

  if (numel (c) != 3)
    error ("synthesis: C must contain exactly 3 elements");
  endif

  w_size = c(1);
  inc    = c(2);
  w_type = c(3);

  if (w_type == 1)
    w_coeff = hanning (w_size);
  elseif (w_type == 2)
    w_coeff = hamming (w_size);
  elseif (w_type == 3)
    w_coeff = ones (w_size, 1);
  else
    error ("synthesis: window_type must be 1, 2, or 3");
  endif

  z = real (ifft (y));
  st = fix ((w_size-inc) / 2);
  z = z(st+1:st+inc, :);
  w_coeff = w_coeff(st+1:st+inc);

  nc = columns (z);
  for i = 1:nc
    z(:, i) ./= w_coeff;
  endfor

  x = reshape (z, inc * nc, 1);

endfunction

