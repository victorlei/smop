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
## @deftypefn  {Function File} {[@var{x}, @var{y}, @var{buttons}] =} ginput (@var{n})
## @deftypefnx {Function File} {[@var{x}, @var{y}, @var{buttons}] =} ginput ()
## Return the position and type of mouse button clicks and/or key strokes
## in the current figure window.
##
## If @var{n} is defined, then capture @var{n} events before returning.
## When @var{n} is not defined @code{ginput} will loop until the return key
## @key{RET} is pressed.
##
## The return values @var{x}, @var{y} are the coordinates where the mouse
## was clicked in the units of the current axes.  The return value @var{button}
## is 1, 2, or 3 for the left, middle, or right button.  If a key is pressed
## the ASCII value is returned in @var{button}.
##
## Implementation Note: @code{ginput} is intenteded for 2-D plots.  For 3-D
## plots see the @var{currentpoint} property of the current axes which can be
## transformed with knowledge of the current @code{view} into data units.
## @seealso{gtext, waitforbuttonpress}
## @end deftypefn

function varargout = ginput (n = -1)

  if (nargin > 1)
    print_usage ();
  endif

  ## Create an axis, if necessary.
  fig = gcf ();
  ax = gca ();
  drawnow ();

  if (isempty (ax))
    error ("ginput: must have at least one axes");
  endif

  toolkit = get (fig, "__graphics_toolkit__");
  toolkit_fcn = sprintf ("__%s_ginput__", toolkit);

  if (exist (toolkit_fcn))
    varargout = cell (1, nargout);
    if (nargin == 0)
      [varargout{:}] = feval (toolkit_fcn, fig);
    else
      [varargout{:}] = feval (toolkit_fcn, fig, n);
    endif
    return
  endif

  x = y = button = [];
  ginput_accumulator (0, 0, 0, 0);  # initialize accumulator

  orig_windowbuttondownfcn = get (fig, "windowbuttondownfcn");
  orig_ginput_keypressfcn = get (fig, "keypressfcn");

  unwind_protect

    set (fig, "windowbuttondownfcn", @ginput_windowbuttondownfcn);
    set (fig, "keypressfcn", @ginput_keypressfcn);

    do
      if (strcmp (toolkit, "fltk"))
        __fltk_check__ ();
      endif

      ## Release CPU.
      sleep (0.01);

      [x, y, n0, button] = ginput_accumulator (-1, 0, 0, 0);
    until ((n > -1 && n0 >= n) || n0 < 0)

    if (n0 > n)
      ## More clicks than requested due to double-click or too fast clicking
      x = x(1:n);
      y = y(1:n);
      button = button(1:n);
    endif

  unwind_protect_cleanup
    set (fig, "windowbuttondownfcn", orig_windowbuttondownfcn);
    set (fig, "keypressfcn", orig_ginput_keypressfcn);
  end_unwind_protect

  varargout = {x, y, button};

endfunction

function [x, y, n, button] = ginput_accumulator (mode, xn, yn, btn)
  persistent x y n button;

  if (mode == 0)
    ## Initialize.
    x = y = button = [];
    n = 0;
  elseif (mode == 1)
    ## Append mouse button or key press.
    x = [x; xn];
    y = [y; yn];
    button = [button; btn];
    n += 1;
  elseif (mode == 2)
    ## The end due to Enter.
    n = -1;
 endif

endfunction

function ginput_windowbuttondownfcn (src, button)
  point = get (gca (), "currentpoint");
  ginput_accumulator (1, point(1,1), point(1,2), button);
endfunction

function ginput_keypressfcn (src, evt)
  point = get (gca (), "currentpoint");
  if (strcmp (evt.Key, "return"))
    ## Enter key stops ginput.
    ginput_accumulator (2, NaN, NaN, NaN);
  else
    character = evt.Character;
    if (! isempty (character))
      ginput_accumulator (1, point(1,1), point(1,2), uint8 (character(1)));
    endif
  endif
endfunction


## Remove from test statistics.  No real tests possible.
%!test
%! assert (1);
