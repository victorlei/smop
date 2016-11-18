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
## @deftypefn  {Command} {} ishold
## @deftypefnx {Function File} {} ishold (@var{hax})
## @deftypefnx {Function File} {} ishold (@var{hfig})
## Return true if the next plot will be added to the current plot, or
## false if the plot device will be cleared before drawing the next plot.
##
## If the first argument is an axes handle @var{hax} or figure handle
## @var{hfig} then operate on this plot rather than the current one.
## @seealso{hold, newplot}
## @end deftypefn

function retval = ishold (h)

  if (nargin > 1)
    print_usage ();
  endif

  if (nargin == 0)
    fig = gcf ();
    ax = get (fig, "currentaxes");
  else
    if (ishandle (h))
      if (strcmp (get (h, "type"), "figure"))
        fig = h;
        ax = get (fig, "currentaxes");
      elseif (strcmp (get (h, "type"), "axes"))
        ax = h;
        fig = ancestor (ax, "figure");
      else
        error ("ishold: H must be an axes or figure graphics handle");
      endif
    else
      error ("ishold: H must be an axes or figure graphics handle");
    endif
  endif

  retval = (strcmp (get (fig, "nextplot"), "add")
            && ! isempty (ax) && strcmp (get (ax, "nextplot"), "add"));

endfunction


%!test
%! hf = figure ("visible", "off");
%! unwind_protect
%!   assert (! ishold);
%!   assert (isempty (get (hf, "currentaxes")));
%!   assert (get (hf, "NextPlot"), "add");
%!   l = plot ([0 1]);
%!   assert (! ishold);
%!   assert (! ishold (gca));
%!   assert (get (gca, "NextPlot"), "replace");
%!   assert (get (hf, "NextPlot"), "add");
%!   hold;
%!   assert (ishold);
%!   assert (ishold (gca));
%!   assert (get (gca, "NextPlot"), "add");
%!   assert (get (hf, "NextPlot"), "add");
%!   p = fill ([0 1 1], [0 0 1],"black");
%!   assert (length (get (hf, "children")), 1);
%!   assert (length (get (gca, "children")), 2);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

