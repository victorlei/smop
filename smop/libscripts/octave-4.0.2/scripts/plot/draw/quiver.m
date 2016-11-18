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
## @deftypefn  {Function File} {} quiver (@var{u}, @var{v})
## @deftypefnx {Function File} {} quiver (@var{x}, @var{y}, @var{u}, @var{v})
## @deftypefnx {Function File} {} quiver (@dots{}, @var{s})
## @deftypefnx {Function File} {} quiver (@dots{}, @var{style})
## @deftypefnx {Function File} {} quiver (@dots{}, "filled")
## @deftypefnx {Function File} {} quiver (@var{hax}, @dots{})
## @deftypefnx {Function File} {@var{h} =} quiver (@dots{})
##
## Plot a 2-D vector field with arrows.
##
## Plot the (@var{u}, @var{v}) components of a vector field in
## an (@var{x}, @var{y}) meshgrid.  If the grid is uniform then @var{x} and
## @var{y} can be specified as vectors.
##
## If @var{x} and @var{y} are undefined they are assumed to be
## @code{(1:@var{m}, 1:@var{n})} where
## @code{[@var{m}, @var{n}] = size (@var{u})}.
##
## The variable @var{s} is a scalar defining a scaling factor to use for
## the arrows of the field relative to the mesh spacing.  A value of 0
## disables all scaling.  The default value is 0.9.
##
## The style to use for the plot can be defined with a line style @var{style}
## of the same format as the @code{plot} command.
## If a marker is specified then markers at the grid points of the vectors are
## drawn rather than arrows.  If the argument @qcode{"filled"} is given then
## the markers are filled.
##
## If the first argument @var{hax} is an axes handle, then plot into this axis,
## rather than the current axes returned by @code{gca}.
##
## The optional return value @var{h} is a graphics handle to a quiver object.
## A quiver object regroups the components of the quiver plot (body, arrow,
## and marker), and allows them to be changed together.
##
## Example:
##
## @example
## @group
## [x, y] = meshgrid (1:2:20);
## h = quiver (x, y, sin (2*pi*x/10), sin (2*pi*y/10));
## set (h, "maxheadsize", 0.33);
## @end group
## @end example
##
## @seealso{quiver3, compass, feather, plot}
## @end deftypefn

function h = quiver (varargin)

  [hax, varargin, nargin] = __plt_get_axis_arg__ ("quiver", varargin{:});

  if (nargin < 2)
    print_usage ();
  endif

  oldfig = [];
  if (! isempty (hax))
    oldfig = get (0, "currentfigure");
  endif
  unwind_protect
    hax = newplot (hax);
    htmp = __quiver__ (hax, false, varargin{:});
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
%! [x,y] = meshgrid (1:2:20);
%! h = quiver (x,y, sin (2*pi*x/10), sin (2*pi*y/10));
%! title ('quiver plot')

%!demo
%! clf;
%! x = linspace (0, 3, 80);
%! y = sin (2*pi*x);
%! theta = 2*pi*x + pi/2;
%! quiver (x, y, sin (theta)/10, cos (theta)/10, 0.4);
%! axis equal tight;
%! hold on; plot (x,y,'r'); hold off;
%! title ('quiver() with scaled arrows');

