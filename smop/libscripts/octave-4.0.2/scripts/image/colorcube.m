## Copyright (C) 2012-2015 Rik Wehbring
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
## @deftypefn  {Function File} {@var{map} =} colorcube ()
## @deftypefnx {Function File} {@var{map} =} colorcube (@var{n})
## Create color colormap.  This colormap is composed of as many equally
## spaced colors (not grays) in the RGB color space as possible.
##
## If there are not a perfect number @var{n} of regularly spaced colors then the
## remaining entries in the colormap are gradients of pure red, green, blue,
## and gray.
##
## The argument @var{n} must be a scalar.
## If unspecified, the length of the current colormap, or 64, is used.
## @seealso{colormap}
## @end deftypefn

function map = colorcube (n)

  if (nargin == 0)
    n = rows (colormap);
  elseif (nargin == 1)
    if (! isscalar (n))
      error ("colorcube: N must be a scalar");
    endif
  else
    print_usage ();
  endif

  if (n < 9)
    map = gray (n);
    return;
  endif

  ## Create colorcube of evenly spaced points with side length of n^1/3
  cubelen = fix (cbrt (n));
  reserve = n - cubelen^3;

  if (reserve == 0)
    ## Steal space from blue to put the gray gradient
    [r, g, b] = meshgrid (linspace (0,1,cubelen),
                          linspace (0,1,cubelen),
                          linspace (0,1,cubelen-1));
  else
    [r, g, b] = meshgrid (linspace (0,1,cubelen),
                          linspace (0,1,cubelen),
                          linspace (0,1,cubelen));
  endif

  ## Create map and weed out grays
  map = [r(:), g(:), b(:)];
  idx = any (bsxfun (@ne, map(:, 1), map(:, 2:3)), 2);
  map = map(idx, :);

  ## Weed out pure colors
  idx = sum (map == 0, 2);
  map = map(idx != 2, :);

  ## Put in remaining gradients of pure red, green, blue, and gray
  reserve = n - rows (map) - 1;
  csteps = fix (reserve/4);
  cstepsz = 1 / csteps;
  cgrad = (cstepsz:cstepsz:1)';
  gsteps = reserve - 3*csteps;
  gstepsz = 1 / gsteps;
  ggrad = (gstepsz:gstepsz:1)';
  map = [map
         cgrad, zeros(csteps, 1), zeros(csteps, 1)
         zeros(csteps, 1), cgrad, zeros(csteps, 1)
         zeros(csteps, 1), zeros(csteps, 1), cgrad
         0, 0, 0
         ggrad, ggrad, ggrad];

endfunction


%!demo
%! ## Show the 'colorcube' colormap as an image
%! image (1:64, linspace (0, 1, 64), repmat ((1:64)', 1, 64));
%! axis ([1, 64, 0, 1], "ticy", "xy");
%! colormap (colorcube (64));

