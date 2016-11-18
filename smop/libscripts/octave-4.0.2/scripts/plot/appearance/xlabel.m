## Copyright (C) 1993-2015 John W. Eaton
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
## @deftypefn  {Function File} {} xlabel (@var{string})
## @deftypefnx {Function File} {} xlabel (@var{string}, @var{property}, @var{val}, @dots{})
## @deftypefnx {Function File} {} xlabel (@var{hax}, @dots{})
## @deftypefnx {Function File} {@var{h} =} xlabel (@dots{})
## Specify the string used to label the x-axis of the current axis.
##
## An optional list of @var{property}/@var{value} pairs can be used to change
## the properties of the created text label.
##
## If the first argument @var{hax} is an axes handle, then operate on
## this axis rather than the current axes returned by @code{gca}.
##
## The optional return value @var{h} is a graphics handle to the created text
## object.
## @seealso{ylabel, zlabel, datetick, title, text}
## @end deftypefn

## Author: jwe

function h = xlabel (varargin)

  [hax, varargin, nargin] = __plt_get_axis_arg__ ("xlabel", varargin{:});

  if (isempty (hax))
    hax = gca ();
  endif

  if (rem (nargin, 2) != 1)
    print_usage ();
  endif

  htmp = __axis_label__ (hax, "xlabel", varargin{1},
                         "color", get (hax, "xcolor"), varargin{2:end});

  if (nargout > 0)
    h = htmp;
  endif

endfunction


%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   x = xlabel ("xlabel_string", "color", "r");
%!   assert (get (gca, "xlabel"), x);
%!   assert (get (x, "type"), "text");
%!   assert (get (x, "visible"), "on");
%!   assert (get (x, "string"), "xlabel_string");
%!   assert (get (x, "color"), [1 0 0]);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

