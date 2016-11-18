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
## @deftypefn  {Function File} {} scatter3 (@var{x}, @var{y}, @var{z})
## @deftypefnx {Function File} {} scatter3 (@var{x}, @var{y}, @var{z}, @var{s})
## @deftypefnx {Function File} {} scatter3 (@var{x}, @var{y}, @var{z}, @var{s}, @var{c})
## @deftypefnx {Function File} {} scatter3 (@dots{}, @var{style})
## @deftypefnx {Function File} {} scatter3 (@dots{}, "filled")
## @deftypefnx {Function File} {} scatter3 (@dots{}, @var{prop}, @var{val})
## @deftypefnx {Function File} {} scatter3 (@var{hax}, @dots{})
## @deftypefnx {Function File} {@var{h} =} scatter3 (@dots{})
## Draw a 3-D scatter plot.
##
## A marker is plotted at each point defined by the coordinates in the vectors
## @var{x}, @var{y}, and @var{z}.
##
## The size of the markers is determined by @var{s}, which can be a scalar
## or a vector of the same length as @var{x}, @var{y}, and @var{z}.  If @var{s}
## is not given, or is an empty matrix, then a default value of 8 points is
## used.
##
## The color of the markers is determined by @var{c}, which can be a string
## defining a fixed color; a 3-element vector giving the red, green, and blue
## components of the color; a vector of the same length as @var{x} that gives
## a scaled index into the current colormap; or an @nospell{Nx3} matrix defining
## the RGB color of each marker individually.
##
## The marker to use can be changed with the @var{style} argument, that is a
## string defining a marker in the same manner as the @code{plot} command.
## If no marker is specified it defaults to @qcode{"o"} or circles.
## If the argument @qcode{"filled"} is given then the markers are filled.
##
## Additional property/value pairs are passed directly to the underlying
## patch object.
##
## If the first argument @var{hax} is an axes handle, then plot into this axis,
## rather than the current axes returned by @code{gca}.
##
## The optional return value @var{h} is a graphics handle to the hggroup
## object representing the points.
##
## @example
## @group
## [x, y, z] = peaks (20);
## scatter3 (x(:), y(:), z(:), [], z(:));
## @end group
## @end example
##
## @seealso{scatter, patch, plot}
## @end deftypefn

function retval = scatter3 (varargin)

  [hax, varargin, nargin] = __plt_get_axis_arg__ ("scatter3", varargin{:});

  if (nargin < 2)
    print_usage ();
  endif

  oldfig = [];
  if (! isempty (hax))
    oldfig = get (0, "currentfigure");
  endif
  unwind_protect
    hax = newplot (hax);

    htmp = __scatter__ (hax, 3, "scatter3", varargin{:});

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
    retval = htmp;
  endif

endfunction


%!demo
%! clf;
%! [x, y, z] = peaks (20);
%! scatter3 (x(:), y(:), z(:), [], z(:));
%! title ({'Default scatter3() plot', ...
%!         'constant size bubbles and color determined by Z'});

%!demo
%! clf;
%! x = rand (20,1);  y = rand (20,1);  z = rand (20,1);
%! scatter3 (x(:), y(:), z(:), 10, z(:), 's');
%! title ({'scatter3() plot', ...
%!         'marker is square, size is 10, color determined by Z'});

%!demo
%! clf;
%! x = rand (20,1);  y = rand (20,1);  z = rand (20,1);
%! scatter3 (x(:), y(:), z(:), 20*z(:), [], 's');
%! title ({'scatter3() plot', ...
%!         'marker is square, size is determined by Z'});

%!demo
%! clf;
%! x = rand (20,1);  y = rand (20,1);  z = rand (20,1);
%! scatter3 (x(:), y(:), z(:), 20*z(:), z(:), 's');
%! title ({'scatter3() plot', ...
%!         'marker is square, size and color determined by Z'});

