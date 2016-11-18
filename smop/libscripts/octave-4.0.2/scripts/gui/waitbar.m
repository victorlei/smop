## Copyright (C) 2012-2015 John W. Eaton
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
## @deftypefn  {Function File} {@var{h} =} waitbar (@var{frac})
## @deftypefnx {Function File} {@var{h} =} waitbar (@var{frac}, @var{msg})
## @deftypefnx {Function File} {@var{h} =} waitbar (@dots{}, "FigureProperty", "Value", @dots{})
## @deftypefnx {Function File} {} waitbar (@var{frac})
## @deftypefnx {Function File} {} waitbar (@var{frac}, @var{hwbar})
## @deftypefnx {Function File} {} waitbar (@var{frac}, @var{hwbar}, @var{msg})
## Return a handle @var{h} to a new waitbar object.
##
## The waitbar is filled to fraction @var{frac} which must be in the range
## [0, 1].
##
## The optional message @var{msg} is centered and displayed above the waitbar.
##
## The appearance of the waitbar figure window can be configured by passing
## property/value pairs to the function.
##
## When called with a single input the current waitbar, if it exists, is
## updated to the new value @var{frac}.  If there are multiple outstanding
## waitbars they can be updated individually by passing the handle @var{hwbar}
## of the specific waitbar to modify.
## @end deftypefn

## Author: jwe

function h = waitbar (varargin)

  persistent curr_waitbar;

  if (nargin < 1)
    print_usage ();
  endif

  frac = varargin{1};
  varargin(1) = [];

  if (! (isnumeric (frac) && isscalar (frac) && frac >= 0 && frac <= 1))
    error ("waitbar: FRAC must be between 0 and 1");
  endif

  ## Use existing waitbar if it still points to a valid graphics handle.
  if (nargin == 1 && ishandle (curr_waitbar))
    hf = curr_waitbar;
  else
    hf = false;
  endif

  if (! isempty (varargin) && isnumeric (varargin{1}))
    hf = varargin{1};
    varargin(1) = [];
    if (! isfigure (hf) || ! strcmp (get (hf, "tag"), "waitbar"))
      error ("waitbar: H must be a handle to a waitbar object");
    endif
  endif

  msg = false;

  if (! isempty (varargin))
    msg = varargin{1};
    varargin(1) = [];
    if (! (ischar (msg) || iscellstr (msg)))
      error ("waitbar: MSG must be a character string or cell array of strings");
    endif
  endif

  if (rem (numel (varargin), 2) != 0)
    error ("waitbar: invalid number of property/value pairs");
  endif

  if (hf)
    gd = get (hf, "__guidata__");
    ## Get the cached handles.
    ax = gd(1);
    hp = gd(2);

    set (hp, "xdata", [0; frac; frac; 0]);

    if (ischar (msg) || iscellstr (msg))
      th = get (ax, "title");
      curr_msg = get (th, "string");
      ## graphics handles always store data as column vectors
      if (iscellstr (msg))
        msg = msg(:);
      endif
      cmp = strcmp (msg, curr_msg);
      if (! all (cmp(:)))
        set (th, "string", msg);
      endif
    endif
  else
    ## Save and restore current figure
    cf = get (0, "currentfigure");

    hf = figure ("units", "pixels",
                 "position", [250, 500, 400, 100],
                 "numbertitle", "off",
                 "menubar", "none", "toolbar", "none",
                 "integerhandle", "off",
                 "handlevisibility", "callback",
                 "tag", "waitbar",
                 varargin{:});

    ax = axes ("parent", hf,
               "xtick", [], "ytick", [],
               "xlim", [0, 1], "ylim", [0, 1],
               "position", [0.1, 0.3, 0.8, 0.2]);

    hp = patch (ax, [0; frac; frac; 0], [0; 0; 1; 1], [0, 0.35, 0.75]);

    ## Cache the axes and patch handles.
    set (hf, "__guidata__", [ax hp]);

    if (! (ischar (msg) || iscellstr (msg)))
      msg = "Please wait...";
    endif
    title (ax, msg);

    set (0, "currentfigure", cf);
  endif

  drawnow ();

  if (nargout > 0)
    h = hf;
  endif

  ## If there were no errors, update current waitbar.
  curr_waitbar = hf;

endfunction


%!demo
%! h = waitbar (0, '0.00%');
%! for i = 0:0.01:1
%!   waitbar (i, h, sprintf ('%.2f%%', 100*i));
%! end
%! close (h);

%!demo
%! h = waitbar (0, 'please wait...');
%! for i = 0:0.01:0.6
%!   waitbar (i);
%! end
%! i = 0.3;
%! waitbar (i, h, 'don''t you hate taking a step backward?');
%! pause (0.5);
%! for i = i:0.005:0.7
%!   waitbar (i, h);
%! end
%! waitbar (i, h, 'or stalling?');
%! pause (1);
%! for i = i:0.003:0.8
%!   waitbar (i, h, 'just a little longer now');
%! end
%! for i = i:0.001:1
%!   waitbar (i, h, 'please don''t be impatient');
%! end
%! close (h);

%!demo
%! h1 = waitbar (0, 'Waitbar #1');
%! h2 = waitbar (0, 'Waitbar #2');
%! h2pos = get (h2, 'position');
%! h2pos(1) = h2pos(1) + (h2pos(3) + 50);
%! set (h2, 'position', h2pos);
%! pause (0.5);
%! for i = 1:4
%!   waitbar (i/4, h1);
%!   pause (0.5);
%!   waitbar (i/4, h2);
%!   pause (0.5);
%! end
%! pause (0.5);
%! close (h1);
%! close (h2);

## Test input validation
%!error <FRAC must be between 0 and 1> waitbar (-0.5)
%!error <FRAC must be between 0 and 1> waitbar (1.5)
%!error <MSG must be a character string> waitbar (0.5, struct ())
%!error <invalid number of property/value pairs> waitbar (0.5, "msg", "Name")

