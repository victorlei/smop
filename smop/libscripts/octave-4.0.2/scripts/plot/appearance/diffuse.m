## Copyright (C) 2009-2015 Kai Habel
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
## @deftypefn {Function File} {} diffuse (@var{sx}, @var{sy}, @var{sz}, @var{lv})
## Calculate the diffuse reflection strength of a surface defined by the normal
## vector elements @var{sx}, @var{sy}, @var{sz}.
##
## The light source location vector @var{lv} can be given as a 2-element vector
## [azimuth, elevation] in degrees or as a 3-element vector [x, y, z].
## @seealso{specular, surfl}
## @end deftypefn

## Author: Kai Habel <kai.habel@gmx.de>

function retval = diffuse (sx, sy, sz, lv)

  if (nargin != 4)
    print_usage ();
  endif

  ## Check normal vectors
  if (! size_equal (sx, sy, sz))
    error ("diffuse: SX, SY, and SZ must be the same size");
  endif

  ## Check light vector (lv) argument
  if (! isvector (lv) || length (lv) < 2 || length (lv) > 3)
    error ("diffuse: light vector LV must be a 2- or 3-element vector");
  elseif (length (lv) == 2)
    [lv(1), lv(2), lv(3)] = sph2cart (lv(1) * pi/180, lv(2) * pi/180, 1.0);
  endif

  ## Normalize view and light vector.
  if (sum (abs (lv)) > 0)
    lv /= norm (lv);
  endif

  ns = sqrt (sx.^2 + sy.^2 + sz.^2);
  retval = (sx * lv(1) + sy * lv(2) + sz * lv(3)) ./ ns;
  retval(retval < 0) = 0;

endfunction

