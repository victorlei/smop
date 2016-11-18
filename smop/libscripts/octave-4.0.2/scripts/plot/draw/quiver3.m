## Copyright (C) 2007-2015 David Bateman
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
## @deftypefn  {Function File} {} quiver3 (@var{u}, @var{v}, @var{w})
## @deftypefnx {Function File} {} quiver3 (@var{x}, @var{y}, @var{z}, @var{u}, @var{v}, @var{w})
## @deftypefnx {Function File} {} quiver3 (@dots{}, @var{s})
## @deftypefnx {Function File} {} quiver3 (@dots{}, @var{style})
## @deftypefnx {Function File} {} quiver3 (@dots{}, "filled")
## @deftypefnx {Function File} {} quiver3 (@var{hax}, @dots{})
## @deftypefnx {Function File} {@var{h} =} quiver3 (@dots{})
##
## Plot a 3-D vector field with arrows.
##
## Plot the (@var{u}, @var{v}, @var{w}) components of a vector field in
## an (@var{x}, @var{y}, @var{z}) meshgrid.  If the grid is uniform then
## @var{x}, @var{y}, and @var{z} can be specified as vectors.
##
## If @var{x}, @var{y}, and @var{z} are undefined they are assumed to be
## @code{(1:@var{m}, 1:@var{n}, 1:@var{p})} where @code{[@var{m}, @var{n}] =
## size (@var{u})} and @code{@var{p} = max (size (@var{w}))}.
##
## The variable @var{s} is a scalar defining a scaling factor to use for
## the arrows of the field relative to the mesh spacing.  A value of 0
## disables all scaling.  The default value is 0.9.
##
## The style to use for the plot can be defined with a line style @var{style}
## of the same format as the @code{plot} command.
## If a marker is specified then markers at the grid points of the vectors are
## drawn rather than arrows.  If the argument @qcode{"filled"} is given then the
## markers are filled.
##
## If the first argument @var{hax} is an axes handle, then plot into this axis,
## rather than the current axes returned by @code{gca}.
##
## The optional return value @var{h} is a graphics handle to a quiver object.
## A quiver object regroups the components of the quiver plot (body, arrow,
## and marker), and allows them to be changed together.
##
## @example
## @group
## [x, y, z] = peaks (25);
## surf (x, y, z);
## hold on;
## [u, v, w] = surfnorm (x, y, z / 10);
## h = quiver3 (x, y, z, u, v, w);
## set (h, "maxheadsize", 0.33);
## @end group
## @end example
##
## @seealso{quiver, compass, feather, plot}
## @end deftypefn

function h = quiver3 (varargin)

  [hax, varargin, nargin] = __plt_get_axis_arg__ ("quiver3", varargin{:});

  if (nargin < 2)
    print_usage ();
  endif

  oldfig = [];
  if (! isempty (hax))
    oldfig = get (0, "currentfigure");
  endif
  unwind_protect
    hax = newplot (hax);
    htmp = __quiver__ (hax, true, varargin{:});

    if (! ishold (hax))
      set (hax, "view", [-37.5, 30], "box", "off",
                "xgrid", "on", "ygrid", "on", "zgrid", "on");
    endif
  unwind_protect_cleanup
    if (! isempty (oldfig))
      set (0, "currentfigure", oldfig);
    endif
  end_unwind_protect

  if (nargout > 0)
    h = htmp;
  endif

endfunction


%!demo
%! clf;
%! colormap ('default');
%! [x, y, z] = peaks (25);
%! surf (x, y, z);
%! hold on;
%! [u, v, w] = surfnorm (x, y, z / 10);
%! h = quiver3 (x, y, z, u, v, w);
%! set (h, 'maxheadsize', 0.25);
%! hold off;
%! title ('quiver3 of surface normals to peaks() function');

%!demo
%! clf;
%! colormap ('default');
%! [x, y, z] = peaks (25);
%! surf (x, y, z);
%! hold on;
%! [u, v, w] = surfnorm (x, y, z / 10);
%! h = quiver3 (x, y, z, u, v, w);
%! set (h, 'maxheadsize', 0.25);
%! hold off;
%! shading interp;
%! title ({'quiver3 of surface normals to peaks() function'; ...
%!         'shading "interp"'});

