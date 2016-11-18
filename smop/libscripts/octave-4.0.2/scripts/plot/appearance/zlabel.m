## Copyright (C) 1995-2015 John W. Eaton
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
## @deftypefn  {Function File} {} zlabel (@var{string})
## @deftypefnx {Function File} {} zlabel (@var{string}, @var{property}, @var{val}, @dots{})
## @deftypefnx {Function File} {} zlabel (@var{hax}, @dots{})
## @deftypefnx {Function File} {@var{h} =} zlabel (@dots{})
## Specify the string used to label the z-axis of the current axis.
##
## An optional list of @var{property}/@var{value} pairs can be used to change
## the properties of the created text label.
##
## If the first argument @var{hax} is an axes handle, then operate on
## this axis rather than the current axes returned by @code{gca}.
##
## The optional return value @var{h} is a graphics handle to the created text
## object.
## @seealso{xlabel, ylabel, datetick, title, text}
## @end deftypefn
## Author: jwe

function h = zlabel (varargin)

  [hax, varargin, nargin] = __plt_get_axis_arg__ ("zlabel", varargin{:});

  if (isempty (hax))
    hax = gca ();
  endif

  if (rem (nargin, 2) != 1)
    print_usage ();
  endif

  htmp = __axis_label__ (hax, "zlabel", varargin{1},
                         "color", get (hax, "zcolor"), varargin{2:end});

  if (nargout > 0)
    h = htmp;
  endif

endfunction


%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   z = zlabel ("zlabel_string", "color", "r");
%!   assert (get (gca, "zlabel"), z);
%!   assert (get (z, "type"), "text");
%!   assert (get (z, "visible"), "off");
%!   assert (get (z, "string"), "zlabel_string");
%!   assert (get (z, "color"), [1 0 0]);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test
%! hf = figure ("visible", "off");
%! plot3 (0, 0, 0);
%! unwind_protect
%!   z = zlabel ("zlabel_string");
%!   assert (get (gca, "zlabel"), z);
%!   assert (get (z, "type"), "text");
%!   assert (get (z, "string"), "zlabel_string");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

