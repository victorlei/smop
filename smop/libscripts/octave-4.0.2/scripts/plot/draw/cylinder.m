## Copyright (C) 2007-2015 Michael Goffioul and Kai Habel
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
## @deftypefn  {Command} {} cylinder
## @deftypefnx {Function File} {} cylinder (@var{r})
## @deftypefnx {Function File} {} cylinder (@var{r}, @var{n})
## @deftypefnx {Function File} {} cylinder (@var{hax}, @dots{})
## @deftypefnx {Function File} {[@var{x}, @var{y}, @var{z}] =} cylinder (@dots{})
## Plot a 3-D unit cylinder.
##
## The optional input @var{r} is a vector specifying the radius along the
## unit z-axis.  The default is [1 1] indicating radius 1 at @code{Z == 0}
## and at @code{Z == 1}.
##
## The optional input @var{n} determines the number of faces around the
## circumference of the cylinder.  The default value is 20.
##
## If the first argument @var{hax} is an axes handle, then plot into this axis,
## rather than the current axes returned by @code{gca}.
##
## If outputs are requested @code{cylinder} returns three matrices in
## @code{meshgrid} format, such that @code{surf (@var{x}, @var{y}, @var{z})}
## generates a unit cylinder.
##
## Example:
##
## @example
## @group
## [x, y, z] = cylinder (10:-1:0, 50);
## surf (x, y, z);
## title ("a cone");
## @end group
## @end example
## @seealso{ellipsoid, rectangle, sphere}
## @end deftypefn

function [xx, yy, zz] = cylinder (varargin)

  [hax, args, nargs] = __plt_get_axis_arg__ ("cylinder", varargin{:});

  if (nargs == 0)
    r = [1, 1];
    n = 20;
  elseif (nargs == 1)
    r = args{1};
    n = 20;
  elseif (nargs == 2)
    r = args{1};
    n = args{2};
  else
    print_usage ();
  endif

  if (length (r) < 2)
    error ("cylinder: length (R) must be larger than 2");
  endif

  phi = linspace (0, 2*pi, n+1);
  idx = 1:length (r);
  [phi, idx] = meshgrid (phi, idx);
  z = (idx - 1) / (length (r) - 1);
  r = r(idx);
  [x, y] = pol2cart (phi, r);

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
%! colormap ('default');
%! [x, y, z] = cylinder (10:-1:0, 50);
%! surf (x, y, z);
%! title ('cylinder() with linearly shrinking radius produces a cone');

