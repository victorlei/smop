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
## @deftypefn  {Function File} {} ezpolar (@var{f})
## @deftypefnx {Function File} {} ezpolar (@dots{}, @var{dom})
## @deftypefnx {Function File} {} ezpolar (@dots{}, @var{n})
## @deftypefnx {Function File} {} ezpolar (@var{hax}, @dots{})
## @deftypefnx {Function File} {@var{h} =} ezpolar (@dots{})
##
## Plot a 2-D function in polar coordinates.
##
## The function @var{f} is a string, inline function, or function handle with
## a single argument.  The expected form of the function is
## @code{@var{rho} = @var{f}(@var{theta})}.
## By default the plot is over the domain @code{0 <= @var{theta} <= 2*pi}
## with 500 points.
##
## If @var{dom} is a two element vector, it represents the minimum and maximum
## values of @var{theta}.
##
## @var{n} is a scalar defining the number of points to use in plotting
## the function.
##
## If the first argument @var{hax} is an axes handle, then plot into this axis,
## rather than the current axes returned by @code{gca}.
##
## The optional return value @var{h} is a graphics handle to the created plot.
##
## Example:
##
## @example
## ezpolar (@@(t) sin (5/4 * t), [0, 8*pi]);
## @end example
##
## @seealso{polar, ezplot}
## @end deftypefn

function h = ezpolar (varargin)

  [htmp, needusage] = __ezplot__ ("polar", varargin{:});

  if (needusage)
    print_usage ();
  endif

  if (nargout > 0)
    h = htmp;
  endif

endfunction


%!demo
%! clf;
%! ezpolar (@(t) sin (5/4 * t), [0, 8*pi]);

