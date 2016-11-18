## Copyright (C) 1996-2015 John W. Eaton
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
## @deftypefn  {Command} {} figure
## @deftypefnx {Command} {} figure @var{n}
## @deftypefnx {Function File} {} figure (@var{n})
## @deftypefnx {Function File} {} figure (@dots{}, "@var{property}", @var{value}, @dots{})
## @deftypefnx {Function File} {@var{h} =} figure (@dots{})
## Create a new figure window for plotting.
##
## If no arguments are specified, a new figure with the next available number
## is created.
##
## If called with an integer @var{n}, and no such numbered figure exists, then
## a new figure with the specified number is created.  If the figure already
## exists then it is made visible and becomes the current figure for plotting.
##
## Multiple property-value pairs may be specified for the figure object, but
## they must appear in pairs.
##
## The optional return value @var{h} is a graphics handle to the created figure
## object.
## @seealso{axes, gcf, clf, close}
## @end deftypefn

## Author: jwe, Bill Denney

function h = figure (varargin)

  nargs = nargin;

  if (mod (nargs, 2) == 0)
    f = NaN;
    init_new_figure = true;
  else
    arg = varargin{1};
    if (ischar (arg))
      arg = str2double (arg);
    endif
    if (isscalar (arg) && isfigure (arg))
      f = arg;
      init_new_figure = false;
      varargin(1) = [];
      nargs--;
    elseif (isscalar (arg) && isnumeric (arg) && arg > 0 && arg == fix (arg))
      f = arg;
      init_new_figure = true;
      varargin(1) = [];
      nargs--;
    else
      error ("figure: N must be figure handle or figure number");
    endif
  endif

  if (rem (nargs, 2) == 1)
    error ("figure: PROPERTY/VALUE arguments must be in pairs");
  endif

  ## Check to see if we already have a figure on the screen.  If we do,
  ## then update it if it is different from the figure we are creating
  ## or switching to.
  cf = get (0, "currentfigure");   # Can't use gcf () because it calls figure()
  if (! isempty (cf) && cf != 0)
    if (init_new_figure || cf != f)
      drawnow ();
    endif
  endif

  if (init_new_figure)
    f = __go_figure__ (f, varargin{:});
    __add_default_menu__ (f);
    __add_default_mouse_modes__ (f);
  elseif (nargs > 0)
    set (f, varargin{:});
  endif

  set (0, "currentfigure", f);
  ## When switching to figure N, make figure visible and on top of stack,
  ## unless visibility is explicitly switched off
  if (! init_new_figure && ! any (strcmpi (varargin(1:2:end), "visible")
                                  && strcmpi (varargin(2:2:end), "off")))
    set (f, "visible", "on");
  endif

  if (nargout > 0)
    h = f;
  endif

endfunction

function __add_default_mouse_modes__ (fig)

  set (fig, "__pan_mode__", struct ("Enable", "off",
                                    "Motion", "both",
                                    "FigureHandle", fig));

  set (fig, "__rotate_mode__", struct ("Enable", "off",
                                       "RotateStyle", "box",
                                       "FigureHandle", fig));

  set (fig, "__zoom_mode__", struct ("Enable", "off",
                                     "Motion", "both",
                                     "Direction", "in",
                                     "FigureHandle", fig));

endfunction


%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   assert (hf, gcf);
%!   assert (isfigure (hf));
%!   hf2 = figure (hf, "visible", "off");
%!   assert (hf, hf2);
%!   assert (hf2, gcf);
%!   assert (isfigure (hf2));
%!   assert (get (hf2, "visible"), "off");
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!error <N must be figure handle or figure number> figure ({1})
%!error <N must be figure handle or figure number> figure ([1 2])
%!error <N must be figure handle or figure number> figure (-1)
%!error <N must be figure handle or figure number> figure (1.5)

