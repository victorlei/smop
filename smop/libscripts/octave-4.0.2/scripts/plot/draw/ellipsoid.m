## Copyright (C) 2007-2015 Sylvain Pelissier
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
## @deftypefn  {Function File} {} ellipsoid (@var{xc}, @var{yc}, @var{zc}, @var{xr}, @var{yr}, @var{zr}, @var{n})
## @deftypefnx {Function File} {} ellipsoid (@dots{}, @var{n})
## @deftypefnx {Function File} {} ellipsoid (@var{hax}, @dots{})
## @deftypefnx {Function File} {[@var{x}, @var{y}, @var{z}] =} ellipsoid (@dots{})
## Plot a 3-D ellipsoid.
##
## The inputs @var{xc}, @var{yc}, @var{zc} specify the center of the ellipsoid.
## The inputs @var{xr}, @var{yr}, @var{zr} specify the semi-major axis lengths.
##
## The optional input @var{n} determines the number of faces around the
## circumference of the cylinder.  The default value is 20.
##
## If the first argument @var{hax} is an axes handle, then plot into this axis,
## rather than the current axes returned by @code{gca}.
##
## If outputs are requested @code{ellipsoid} returns three matrices in
## @code{meshgrid} format, such that @code{surf (@var{x}, @var{y}, @var{z})}
## generates the ellipsoid.
## @seealso{cylinder, rectangle, sphere}
## @end deftypefn

## Author: Sylvain Pelissier <sylvain.pelissier@gmail.com>

function [xx, yy, zz] = ellipsoid (varargin)

  [hax, varargin, nargin] = __plt_get_axis_arg__ ("ellipsoid", varargin{:});

  if (nargin != 6 && nargin != 7)
    print_usage ();
  endif

  xc = varargin{1};
  yc = varargin{2};
  zc = varargin{3};
  xr = varargin{4};
  yr = varargin{5};
  zr = varargin{6};

  if (nargin == 6)
    n = 20;
  else
    n = varargin{7};
  endif

  theta = linspace (0, 2 * pi, n + 1);
  phi = linspace (-pi / 2, pi / 2, n + 1);
  [theta, phi] = meshgrid (theta, phi);

  x = xr .* cos (phi) .* cos (theta) + xc;
  y = yr .* cos (phi) .* sin (theta) + yc;
  z = zr .* sin (phi) + zc;

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


%!demo
%! clf;
%! ellipsoid (0, 0, 1, 2, 3, 4, 20);
%! title ('ellipsoid()');

