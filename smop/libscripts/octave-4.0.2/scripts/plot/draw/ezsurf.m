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
## @deftypefn  {Function File} {} ezsurf (@var{f})
## @deftypefnx {Function File} {} ezsurf (@var{fx}, @var{fy}, @var{fz})
## @deftypefnx {Function File} {} ezsurf (@dots{}, @var{dom})
## @deftypefnx {Function File} {} ezsurf (@dots{}, @var{n})
## @deftypefnx {Function File} {} ezsurf (@dots{}, "circ")
## @deftypefnx {Function File} {} ezsurf (@var{hax}, @dots{})
## @deftypefnx {Function File} {@var{h} =} ezsurf (@dots{})
##
## Plot the surface defined by a function.
##
## @var{f} is a string, inline function, or function handle with two arguments
## defining the function.  By default the plot is over the meshed domain
## @code{-2*pi <= @var{x} | @var{y} <= 2*pi} with 60 points in each dimension.
##
## If three functions are passed, then plot the parametrically defined
## function @code{[@var{fx} (@var{s}, @var{t}), @var{fy} (@var{s}, @var{t}),
## @var{fz} (@var{s}, @var{t})]}.
##
## If @var{dom} is a two element vector, it represents the minimum and maximum
## values of both @var{x} and @var{y}.  If @var{dom} is a four element vector,
## then the minimum and maximum values are @code{[xmin xmax ymin ymax]}.
##
## @var{n} is a scalar defining the number of points to use in each dimension.
##
## If the argument @qcode{"circ"} is given, then the function is plotted over
## a disk centered on the middle of the domain @var{dom}.
##
## If the first argument @var{hax} is an axes handle, then plot into this axis,
## rather than the current axes returned by @code{gca}.
##
## The optional return value @var{h} is a graphics handle to the created
## surface object.
##
## Example 1: 2-argument function
##
## @example
## @group
## f = @@(x,y) sqrt (abs (x .* y)) ./ (1 + x.^2 + y.^2);
## ezsurf (f, [-3, 3]);
## @end group
## @end example
##
## Example 2: parametrically defined function
##
## @example
## @group
## fx = @@(s,t) cos (s) .* cos (t);
## fy = @@(s,t) sin (s) .* cos (t);
## fz = @@(s,t) sin (t);
## ezsurf (fx, fy, fz, [-pi, pi, -pi/2, pi/2], 20);
## @end group
## @end example
##
## @seealso{surf, ezsurfc, ezplot, ezmesh, ezmeshc, shading}
## @end deftypefn

function h = ezsurf (varargin)

  [htmp, needusage] = __ezplot__ ("surf", varargin{:});

  if (needusage)
    print_usage ();
  endif

  if (nargout > 0)
    h = htmp;
  endif

endfunction


%!demo
%! clf;
%! colormap ('default');
%! f = @(x,y) sqrt (abs (x .* y)) ./ (1 + x.^2 + y.^2);
%! ezsurf (f, [-3, 3]);

%!demo
%! clf;
%! colormap ('default');
%! fx = @(s,t) cos (s) .* cos (t);
%! fy = @(s,t) sin (s) .* cos (t);
%! fz = @(s,t) sin (t);
%! ezsurf (fx, fy, fz, [-pi,pi,-pi/2,pi/2], 20);
%! axis equal;

%!demo
%! clf;
%! colormap ('default');
%! f = @(x,y) x.^2 + y.^2;
%! subplot (1,2,1);
%!  ezsurf (f, [-2,2]);
%!  title ({'x^2 + y^2'; 'plotted over rectangular grid (default)'});
%! subplot (1,2,2);
%!  ezsurf (f, [-2,2], 'circ');
%!  title ({'x^2 + y^2'; 'plotted over circular disk with "circ"'});

