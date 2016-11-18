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
## @deftypefn {Function File} {@var{h} =} gca ()
## Return a handle to the current axis object.
##
## The current axis is the default target for graphics output.  In the case
## of a figure with multiple axes, @code{gca} returns the last created axes
## or the last axes that was clicked on with the mouse.
##
## If no current axes object exists, create one and return its handle.  The
## handle may then be used to examine or set properties of the axes.  For
## example,
##
## @example
## @group
## ax = gca ();
## set (ax, "position", [0.5, 0.5, 0.5, 0.5]);
## @end group
## @end example
##
## @noindent
## creates an empty axes object and then changes its location and size in the
## figure window.
##
## Note: To find the current axis without creating a new axes object if it
## does not exist, query the @qcode{"CurrentAxes"} property of a figure.
##
## @example
## get (gcf, "currentaxes");
## @end example
## @seealso{gcf, gco, gcbf, gcbo, get, set}
## @end deftypefn

## Author: jwe

function h = gca ()

  if (nargin == 0)
    h = get (gcf (), "currentaxes");
    if (isempty (h))
      h = axes ();
    endif
  else
    print_usage ();
  endif

endfunction


%!test
%! hf = figure ("visible", "off");
%! ax = axes;
%! unwind_protect
%!   assert (gca, ax);
%! unwind_protect_cleanup
%!   close (hf);
%! end_unwind_protect

