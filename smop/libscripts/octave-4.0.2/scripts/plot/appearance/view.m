## Copyright (C) 2007-2015 John W. Eaton
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
## @deftypefn  {Function File} {} view (@var{azimuth}, @var{elevation})
## @deftypefnx {Function File} {} view ([@var{azimuth} @var{elevation}])
## @deftypefnx {Function File} {} view ([@var{x} @var{y} @var{z}])
## @deftypefnx {Function File} {} view (2)
## @deftypefnx {Function File} {} view (3)
## @deftypefnx {Function File} {} view (@var{hax}, @dots{})
## @deftypefnx {Function File} {[@var{azimuth}, @var{elevation}] =} view ()
## Query or set the viewpoint for the current axes.
##
## The parameters @var{azimuth} and @var{elevation} can be given as two
## arguments or as 2-element vector.  The viewpoint can also be specified with
## Cartesian coordinates @var{x}, @var{y}, and @var{z}.
##
## The call @code{view (2)} sets the viewpoint to @w{@var{azimuth} = 0}
## and @w{@var{elevation} = 90}, which is the default for 2-D graphs.
##
## The call @code{view (3)} sets the viewpoint to @w{@var{azimuth} = -37.5}
## and @w{@var{elevation} = 30}, which is the default for 3-D graphs.
##
## If the first argument @var{hax} is an axes handle, then operate on
## this axis rather than the current axes returned by @code{gca}.
##
## If no inputs are given, return the current @var{azimuth} and @var{elevation}.
## @end deftypefn

## Author: jwe

function [azimuth, elevation] = view (varargin)

  [hax, varargin, nargin] = __plt_get_axis_arg__ ("view", varargin{:});
  if (isempty (hax))
    hax = gca ();
  endif

  if (nargin > 3)
    print_usage ();
  endif

  if (nargin == 0)
    x = get (hax, "view");
    az = x(1);
    el = x(2);
  elseif (length (varargin) == 1)
    x = varargin{1};
    if (length (x) == 2)
      az = x(1);
      el = x(2);
    elseif (length (x) == 3)
      [az, el] = cart2sph (x(1), x(2), x(3));
      az *= 180/pi;
      az += 90;
      el *= 180/pi;
    elseif (x == 2)
      az = 0;
      el = 90;
    elseif (x == 3)
      az = -37.5;
      el = 30;
    else
      print_usage ();
    endif
  elseif (length (varargin) == 2)
    az = varargin{1};
    el = varargin{2};
  endif

  if (nargin > 0)
    set (hax, "view", [az, el]);
  else
    if (nargout == 1)
      azimuth = [az, el];
    elseif (nargout == 2)
      azimuth = az;
      elevation = el;
    endif
  endif

endfunction


%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   plot3 ([0,1], [0,1], [0,1]);
%!   [az, el] = view;
%!   assert ([az, el], [-37.5, 30], eps);
%!   view (2);
%!   [az, el] = view;
%!   assert ([az, el], [0, 90], eps);
%!   view ([1 1 0]);
%!   [az, el] = view;
%!   assert ([az, el], [135, 0], eps);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   line;
%!   [az, el] = view;
%!   assert ([az, el], [0, 90], eps);
%!   view (3);
%!   [az, el] = view;
%!   assert ([az, el], [-37.5, 30], eps);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

