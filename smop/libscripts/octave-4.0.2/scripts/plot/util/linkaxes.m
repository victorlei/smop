## Copyright (C) 2014-2015 Willem Atsma
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
## @deftypefn  {Function File} {} linkaxes (@var{hax})
## @deftypefnx {Function File} {} linkaxes (@var{hax}, @var{optstr})
## Link the axis limits of 2-D plots such that a change in one is propagated
## to the others.
##
## The axes handles to be linked are passed as the first argument @var{hax}.
##
## The optional second argument is a string which defines which axis limits
## will be linked.  The possible values for @var{optstr} are:
##
## @table @asis
## @item @qcode{"x"}
## Link x-axes
##
## @item @qcode{"y"}
## Link y-axes
##
## @item @qcode{"xy"} (default)
## Link both axes
##
## @item @qcode{"off"}
## Turn off linking
## @end table
##
## If unspecified the default is to link both X and Y axes.
##
## When linking, the limits from the first axes in @var{hax} are applied to the
## other axes in the list.  Subsequent changes to any one of the axes will be
## propagated to the others.
##
## @seealso{linkprop, addproperty}
## @end deftypefn

## Author: Willem Atsma willem.atsma at tanglebridge.com
## Created: 2014-03-18

function linkaxes (hax, optstr = "xy")

  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif

  if (numel (hax) < 2)
    error ("linkaxes: HAX must contain at least 2 handles");
  elseif (! all (isaxes (hax(:))))
    error ("linkaxes: HAX must be a vector of axes handles");
  endif

  ## Check if axes are linked already and clear if found.
  ## Otherwise, add the necessary linkaxes_data property.
  for i = 1:length (hax)
    if (isprop (hax(i), "linkaxes_data"))
      hld = get (hax(i), "linkaxes_data");
      try
        rmappdata (hld, "linkprop_data");
      end_try_catch
    else
      addproperty ("linkaxes_data", hax(i), "any");
    endif
  endfor

  switch (optstr)
    case "x"
      hlink = linkprop (hax, "xlim");
    case "y"
      hlink = linkprop (hax, "ylim");
    case "xy"
      hlink = linkprop (hax, {"xlim" "ylim"});
    case "off"
      ## do nothing - link already deleted
      hlink = [];
    otherwise
      error ("linkaxes: unrecognized OPTSTR '%s'", optstr);
  endswitch

  if (! isempty (hlink))
    setappdata (hax(1), "linkprop_data", hlink);
    set (hax, "linkaxes_data", hax(1));
  else
    set (hax, "linkaxes_data", []);
  endif

endfunction


%!demo
%! clf;
%! hax1 = subplot (3,1,1);
%! bar (rand (4, 1), 'facecolor', 'r');
%! hax2 = subplot (3,1,2);
%! bar (5*rand (4, 1), 'facecolor', 'g');
%! hax3 = subplot (3,1,3);
%! bar (10*rand (4, 1), 'facecolor', 'b');
%! input ('Type <RETURN> to link axes');
%! linkaxes ([hax1, hax2, hax3]);
%! input ('Type <RETURN> to change ylim');
%! ylim (hax3, [0 10]);

%!test
%! hf1 = figure ("visible", "off");
%! hax1 = axes ();
%! plot (1:10);
%! hf2 = figure ("visible", "off");
%! hax2 = axes ();
%! plot (10:-1:1, "-*g");
%! hf3 = figure ("visible", "off");
%! hax3 = axes ();
%! plot (1:10:100, "-xb");
%!  unwind_protect
%!   linkaxes ([hax1, hax2, hax3]);
%!   ## Test initial values taken from first object in list
%!   assert (xlim (hax3), [0 10]);
%!   assert (ylim (hax3), [0 10]);
%!   ## Test linking
%!   xlim (hax2, [2 8]);
%!   assert (xlim (hax1), [2 8]);
%!   assert (xlim (hax3), [2 8]);
%!   ylim (hax3, "auto");
%!   assert (ylim (hax1), [0 100]);
%!   assert (ylim (hax2), [0 100]);
%!   ## Test re-linking removes old link
%!   linkaxes ([hax1, hax2]);
%!   ylim (hax3, [0 50]);
%!   assert (ylim (hax1), [0 100]);
%!   assert (ylim (hax2), [0 100]);
%!   xlim (hax1, [0 4]);
%!   assert (xlim (hax2), [0 4]);
%!   ## Test linking of remaining objects after deletion of one object
%!   linkaxes ([hax1, hax2, hax3]);
%!   xlim (hax2, [0 1]);
%!   assert (xlim (hax1), [0 1]);
%!   assert (xlim (hax3), [0 1]);
%!   delete (hax2);
%!   xlim (hax3, [0 2]);
%!   assert (xlim (hax1), [0 2]);
%!   ## Test deletion of link
%!   linkaxes ([hax1, hax3], "off");
%!   xlim (hax3, [0 3]);
%!   assert (xlim (hax1), [0 2]);
%!  unwind_protect_cleanup
%!   close ([hf1 hf2 hf3]);
%!  end_unwind_protect

## Test input validation
%!error linkaxes ()
%!error linkaxes (1,2,3)
%!error <HAX must be a vector of axes handles> linkaxes ([pi, e])

