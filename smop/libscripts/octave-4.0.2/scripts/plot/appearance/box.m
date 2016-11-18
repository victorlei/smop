## Copyright (C) 2006-2015 John W. Eaton
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
## @deftypefn  {Command} {} box
## @deftypefnx {Command} {} box on
## @deftypefnx {Command} {} box off
## @deftypefnx {Function File} {} box (@var{hax}, @dots{})
## Control display of the axis border.
##
## The argument may be either @qcode{"on"} or @qcode{"off"}.  If it is
## omitted, the current box state is toggled.
##
## If the first argument @var{hax} is an axes handle, then operate on this
## axis rather than the current axes returned by @code{gca}.
## @seealso{axis, grid}
## @end deftypefn

## Author: jwe

function box (varargin)

  [hax, varargin, nargs] = __plt_get_axis_arg__ ("box", varargin{:});

  if (isempty (hax))
    hax = gca ();
  endif

  if (nargs == 0)
    box_state = get (hax, "box");
    if (strcmp (box_state, "on"))
      box_state = "off";
    else
      box_state = "on";
    endif
  elseif (nargs == 1)
    state = varargin{1};
    if (ischar (state))
      if (strcmpi (state, "off"))
        box_state = "off";
      elseif (strcmpi (state, "on"))
        box_state = "on";
      else
        error ('box: argument must be "on" or "off"');
      endif
    else
      error ('box: argument must be "on" or "off"');
    endif
  else
    print_usage ();
  endif

  set (hax, "box", box_state);

endfunction

