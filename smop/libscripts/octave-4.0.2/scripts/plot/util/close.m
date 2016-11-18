## Copyright (C) 2002-2015 John W. Eaton
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
## @deftypefn  {Command} {} close
## @deftypefnx {Command} {} close (@var{h})
## @deftypefnx {Command} {} close @var{h}
## @deftypefnx {Command} {} close all
## @deftypefnx {Command} {} close all hidden
## @deftypefnx {Command} {} close all force
## Close figure window(s).
##
## When called with no arguments, close the current figure.  This is equivalent
## to @code{close (gcf)}.  If the input @var{h} is a graphic handle, or vector
## of graphics handles, then close each figure in @var{h}.
##
## If the argument @qcode{"all"} is given then all figures with visible handles
## (HandleVisibility = @qcode{"on"}) are closed.
##
## If the argument @qcode{"all hidden"} is given then all figures, including
## hidden ones, are closed.
##
## If the argument @qcode{"all force"} is given then all figures are closed
## even when @qcode{"closerequestfcn"} has been altered to prevent closing
## the window.
##
## Implementation Note: @code{close} operates by calling the function specified
## by the @qcode{"closerequestfcn"} property for each figure.  By default, the
## function @code{closereq} is used.  It is possible that the function invoked
## will delay or abort removing the figure.  To remove a figure without
## executing any callback functions use @code{delete}.  When writing a callback
## function to close a window do not use @code{close} to avoid recursion.
##
## @seealso{closereq, delete}
## @end deftypefn

## Author: jwe
## 2010-05-02   PBig    allow empty argument

function retval = close (arg1, arg2)

  figs = [];

  if (nargin > 2)
    print_usage ();
  elseif (nargin == 0)
    ## Close current figure.
    ## Can't use gcf because it opens a new plot window if one does not exist.
    figs = get (0, "currentfigure");
    if (figs == 0)  # don't close root figure
      figs = [];
    endif
  elseif (nargin == 1)
    if (ischar (arg1) && strcmpi (arg1, "all"))
      figs = (get (0, "children"))';
      figs = figs(isfigure (figs));
    elseif (any (isfigure (arg1)))
      figs = arg1(isfigure (arg1));
    elseif (isempty (arg1))
      figs = [];  # Silently accept null argument for Matlab compatibility
    else
      error ('close: first argument must be "all" or a figure handle');
    endif
  elseif (ischar (arg2)
          && (strcmpi (arg2, "hidden") || strcmpi (arg2, "force")))
    if (ischar (arg1) && strcmpi (arg1, "all"))
      figs = (allchild (0))';
      figs = figs(isfigure (figs));
    else
      error ('close: first argument must be "all" with "hidden" or "force"');
    endif
    if (strcmpi (arg2, "force"))
      delete (figs);
      return;
    endif
  else
    error ('close: second argument must be "hidden" or "force"');
  endif

  for h = figs
    __go_execute_callback__ (h, "closerequestfcn");
  endfor

  if (nargout > 0)
    retval = 1;
  endif

endfunction


%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   close (hf);
%!   objs = findobj ("type", "figure");
%!   assert (! any (objs == hf));
%! unwind_protect_cleanup
%!   if (isfigure (hf))
%!     close (hf);
%!   endif
%! end_unwind_protect

## Test input validation
%!error close (1,2,3)
%!error <first argument must be "all" or a figure> close ({"all"})
%!error <first argument must be "all" or a figure> close ("all_and_more")
%!error <first argument must be "all" or a figure> close (-1)
%!error <first argument must be "all" with "hidden"> close foo hidden
%!error <first argument must be "all" with "hidden"> close foo force
%!error <second argument must be "hidden"> close all hid
%!error <second argument must be "hidden"> close all for

