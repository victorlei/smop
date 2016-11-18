## Copyright (C) 2012-2015 Michael Goffioul
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
## @deftypefn  {Function File} {} uiwait
## @deftypefnx {Function File} {} uiwait (@var{h})
## @deftypefnx {Function File} {} uiwait (@var{h}, @var{timeout})
## Suspend program execution until the figure with handle @var{h} is deleted
## or @code{uiresume} is called.
##
## When no figure handle is specified this function uses the current figure.
## If the figure handle is invalid or there is no current figure, this
## functions returns immediately.
##
## When specified, @var{timeout} defines the number of seconds to wait
## for the figure deletion or the @code{uiresume} call.  The timeout value
## must be at least 1.  If a smaller value is specified, a warning is issued
## and a timeout value of 1 is used instead.  If a non-integer value is
## specified, it is truncated towards 0.  If @var{timeout} is not specified,
## the program execution is suspended indefinitely.
## @seealso{uiresume, waitfor}
## @end deftypefn

## Author: goffioul

function uiwait (varargin)

  h = [];
  timeout = [];

  if (nargin == 0)
    h = get (0, "currentfigure");
  else
    h = varargin{1};
    if (! isfigure (h))
      error ("uiwait: invalid figure handle H");
    endif
    if (nargin > 1)
      timeout = varargin{2};
    endif
  endif

  if (! isempty (h))
    unwind_protect
      try
        addproperty ("__uiwait_state__", h, "radio", "none|{active}|triggered");
      catch
        if (! strcmp (get (h, "__uiwait_state__"), "none"))
          error ("uiwait: an active uiwait call for this figure already exists");
        endif
        set (h, "__uiwait_state__", "active");
      end_try_catch
      waitfor_args = {h, "__uiwait_state__", "triggered"};
      if (! isempty (timeout))
        waitfor_args(end+1:end+2) = {"timeout", timeout};
      endif
      waitfor (waitfor_args{:});
    unwind_protect_cleanup
      if (ishandle (h) && isprop (h, "__uiwait_state__"))
        set (h, "__uiwait_state__", "none");
      endif
    end_unwind_protect
  endif

endfunction

