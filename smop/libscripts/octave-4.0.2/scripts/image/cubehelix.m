## Copyright (C) 2014-2015 Carnë Draug
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
## @deftypefn  {Function File} {@var{map} =} cubehelix ()
## @deftypefnx {Function File} {@var{map} =} cubehelix (@var{n})
## Create cubehelix colormap.
##
## This colormap varies from black to white going though blue, green, and red
## tones while maintaining a monotonically increasing perception of intensity.
## This is achieved by transversing a color cube from black to white through
## a helix, hence the name cubehelix, while taking into account the perceived
## brightness of each channel according to the NTSC specifications from 1953.
##
## @example
## rgbplot (cubehelix (256))
## @end example
##
## The argument @var{n} must be a scalar.
## If unspecified, the length of the current colormap, or 64, is used.
##
## Reference: Green, D. A., 2011,
## @cite{"A @nospell{colour} scheme for the display of astronomical intensity
## images"}, Bulletin of the Astronomical Society of India, 39, 289.
##
## @seealso{colormap}
## @end deftypefn

## Author: Carnë Draug <carandraug@octave.org>

## PKG_ADD: colormap ("register", "cubehelix");
## PKG_DEL: colormap ("unregister", "cubehelix");

function map = cubehelix (n = rows (colormap ()), start = 0.5,
                          rots = -1.5, hue = 1, gamma = 1)

  if (nargin > 5)
    print_usage ()
  elseif (! isscalar (n))
    error ("cubehelix: N must be a scalar");
  endif
  n = double (n);

  if (n > 1)
    coeff = [ -0.14861  -0.29227   1.97294
               1.78277  -0.90649   0.00000];

    fract = ((0:n-1) / (n-1))';
    angle = 2 * pi * (start/3 + 1 + rots*fract);
    fract = fract .^ gamma;
    amp   = hue * fract .* (1-fract) /2;
    map   = fract + amp .* ([cos(angle) sin(angle)] * coeff);

    ## Clip values (only in case users have changed values of hue or gamma)
    map(map < 0) = 0;
    map(map > 1) = 1;

  elseif (n > 0)
    map = [0, 0, 0];
  else
    map = zeros (0, 3);
  endif

endfunction


%!demo
%! subplot (1, 2, 1)
%! rgbplot (cubehelix (256), "composite")
%! subplot (1, 2, 2)
%! rgbplot (cubehelix (256))

