## Copyright (C) 2005-2015 John W. Eaton
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
## @deftypefn  {Command} {} clf
## @deftypefnx {Command} {} clf reset
## @deftypefnx {Function File} {} clf (@var{hfig})
## @deftypefnx {Function File} {} clf (@var{hfig}, "reset")
## @deftypefnx {Function File} {@var{h} =} clf (@dots{})
## Clear the current figure window.
##
## @code{clf} operates by deleting child graphics objects with visible
## handles (HandleVisibility = @qcode{"on"}).
##
## If the optional argument @qcode{"reset"} is specified, delete all child
## objects including those with hidden handles and reset all figure
## properties to their defaults.  However, the following properties are not
## reset: Position, Units, PaperPosition, PaperUnits.
##
## If the first argument @var{hfig} is a figure handle, then operate on
## this figure rather than the current figure returned by @code{gcf}.
##
## The optional return value @var{h} is the graphics handle of the figure
## window that was cleared.
## @seealso{cla, close, delete, reset}
## @end deftypefn

## Author: jwe

function h = clf (varargin)

  if (nargin > 2)
    print_usage ();
  elseif (nargin == 0)
    hfig = gcf;
    do_reset = false;
  elseif (nargin == 1)
    if (isscalar (varargin{1}) && isfigure (varargin{1}))
      hfig = varargin{1};
      do_reset = false;
    elseif (ischar (varargin{1}) && strcmpi (varargin{1}, "reset"))
      hfig = gcf;
      do_reset = true;
    else
      print_usage ();
    endif
  else
    if (isscalar (varargin{1}) && isfigure (varargin{1})
        && ischar (varargin{2}) && strcmpi (varargin{2}, "reset"))
      hfig = varargin{1};
      do_reset = true;
    else
      print_usage ();
    endif
  endif

  if (do_reset)
    ## Select all the children, including the one with hidden handles.
    delete (allchild (hfig));
    reset (hfig);
  else
    ## Select only the chilren with visible handles.
    delete (get (hfig, "children"));

    ## Also delete the annotation axes
    hover = findall (hfig, "-depth", 1, "tag", "scribeoverlay");
    delete (hover);
  endif

  if (nargout > 0)
    h = hfig;
  endif

endfunction


%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   l = line;
%!   assert (! isempty (get (gcf, "children")));
%!   clf;
%!   assert (isempty (get (gcf, "children")));
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   clf;
%!   assert (isempty (get (gcf, "children")));
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

%!xtest
%! set (0, "defaultfigurevisible", "off")
%! hf = figure ("visible", "off");
%! unwind_protect
%!   plot (1:10);
%!   set (hf, "papertype", "tabloid");
%!   clf (hf);
%!   assert (isempty (get (gcf, "children")));
%!   assert (get (hf, "papertype"), "tabloid");
%!   plot (1:10);
%!   clf (hf, "reset");
%!   kids = get (hf, "children");
%!   assert (isempty (get (gcf, "children")));
%!   assert (get (hf, "papertype"), "usletter");
%! unwind_protect_cleanup
%!   set (0, "defaultfigurevisible", "remove")
%!   close (hf);
%! end_unwind_protect

