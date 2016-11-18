## Copyright (C) 2008-2015 David Bateman
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
## @deftypefn  {Function File} {} ezplot (@var{f})
## @deftypefnx {Function File} {} ezplot (@var{f2v})
## @deftypefnx {Function File} {} ezplot (@var{fx}, @var{fy})
## @deftypefnx {Function File} {} ezplot (@dots{}, @var{dom})
## @deftypefnx {Function File} {} ezplot (@dots{}, @var{n})
## @deftypefnx {Function File} {} ezplot (@var{hax}, @dots{})
## @deftypefnx {Function File} {@var{h} =} ezplot (@dots{})
##
## Plot the 2-D curve defined by the function @var{f}.
##
## The function @var{f} may be a string, inline function, or function handle
## and can have either one or two variables.  If @var{f} has one variable, then
## the function is plotted over the domain @code{-2*pi < @var{x} < 2*pi}
## with 500 points.
##
## If @var{f2v} is a function of two variables then the implicit function
## @code{@var{f}(@var{x},@var{y}) = 0} is calculated over the meshed domain
## @code{-2*pi <= @var{x} | @var{y} <= 2*pi} with 60 points in each dimension.
##
## For example:
##
## @example
## ezplot (@@(@var{x}, @var{y}) @var{x}.^2 - @var{y}.^2 - 1)
## @end example
##
## If two functions are passed as inputs then the parametric function
##
## @example
## @group
## @var{x} = @var{fx} (@var{t})
## @var{y} = @var{fy} (@var{t})
## @end group
## @end example
##
## @noindent
## is plotted over the domain @code{-2*pi <= @var{t} <= 2*pi} with 500 points.
##
## If @var{dom} is a two element vector, it represents the minimum and maximum
## values of both @var{x} and @var{y}, or @var{t} for a parametric plot.  If
## @var{dom} is a four element vector, then the minimum and maximum values are
## @code{[xmin xmax ymin ymax]}.
##
## @var{n} is a scalar defining the number of points to use in plotting
## the function.
##
## If the first argument @var{hax} is an axes handle, then plot into this axis,
## rather than the current axes returned by @code{gca}.
##
## The optional return value @var{h} is a vector of graphics handles to
## the created line objects.
##
## @seealso{plot, ezplot3, ezpolar, ezcontour, ezcontourf, ezmesh, ezmeshc, ezsurf, ezsurfc}
## @end deftypefn

function h = ezplot (varargin)

  [htmp, needusage] = __ezplot__ ("plot", varargin{:});

  if (needusage)
    print_usage ();
  endif

  if (nargout > 0)
    h = htmp;
  endif

endfunction


%!demo
%! %% sinc function using function handle
%! f = @(x) sin (pi*x) ./ (pi*x);
%! ezplot (f);

%!demo
%! %% example of a function string and explicit limits
%! clf;
%! ezplot ('1/x', [-2 2]);

%!demo
%! %% parameterized function example over -2*pi <= t <= +2*pi
%! clf;
%! ezplot (@cos, @sin);

%!demo
%! %% implicit function of 2 variables
%! clf;
%! ezplot (inline ('x^2 - y^2 - 1'));

