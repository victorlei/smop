## Copyright (C) 2007-2015 Michael Goffioul
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
## @deftypefn  {Function File} {} sphere ()
## @deftypefnx {Function File} {} sphere (@var{n})
## @deftypefnx {Function File} {} sphere (@var{hax}, @dots{})
## @deftypefnx {Function File} {[@var{x}, @var{y}, @var{z}] =} sphere (@dots{})
## Plot a 3-D unit sphere.
##
## The optional input @var{n} determines the number of faces around the
## circumference of the sphere.  The default value is 20.
##
## If the first argument @var{hax} is an axes handle, then plot into this axis,
## rather than the current axes returned by @code{gca}.
##
## If outputs are requested @code{sphere} returns three matrices in
## @code{meshgrid} format such that @code{surf (@var{x}, @var{y}, @var{z})}
## generates a unit sphere.
##
## Example:
##
## @example
## @group
## [x, y, z] = sphere (40);
## surf (3*x, 3*y, 3*z);
## axis equal;
## title ("sphere of radius 3");
## @end group
## @end example
## @seealso{cylinder, ellipsoid, rectangle}
## @end deftypefn

function [xx, yy, zz] = sphere (varargin)

  [hax, varargin, nargin] = __plt_get_axis_arg__ ("sphere", varargin{:});

  if (nargin > 1)
    print_usage ();
  elseif (nargin == 1)
    n = varargin{1};
  else
    n = 20;
  endif

  theta = linspace (0, 2*pi, n+1);
  phi = linspace (-pi/2, pi/2, n+1);
  [theta,phi] = meshgrid (theta, phi);

  x = cos (phi) .* cos (theta);
  y = cos (phi) .* sin (theta);
  z = sin (phi);

  if (nargout > 0)
    xx = x;
    yy = y;
    zz = z;
  else
    oldfig = [];
    if (! isempty (hax))
      oldfig = get (0, "currentfigure");
    endif
    unwind_protect
      hax = newplot (hax);

      surf (x, y, z);
    unwind_protect_cleanup
      if (! isempty (oldfig))
        set (0, "currentfigure", oldfig);
      endif
    end_unwind_protect
  endif

endfunction

