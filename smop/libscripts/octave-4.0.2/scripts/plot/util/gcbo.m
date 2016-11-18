## Copyright (C) 2008-2015 Michael Goffioul
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
## @deftypefn  {Function File} {@var{h} =} gcbo ()
## @deftypefnx {Function File} {[@var{h}, @var{fig}] =} gcbo ()
## Return a handle to the object whose callback is currently executing.
##
## If no callback is executing, this function returns the empty matrix.  This
## handle is obtained from the root object property @qcode{"CallbackObject"}.
##
## When called with a second output argument, return the handle of the figure
## containing the object whose callback is currently executing.  If no callback
## is executing the second output is also set to the empty matrix.
##
## @seealso{gcbf, gco, gca, gcf, get, set}
## @end deftypefn

function [h, fig] = gcbo ()

  h = get (0, "callbackobject");
  fig = [];

  if (! isempty (h) && nargout > 1)
    fig = ancestor (h, "figure");
  endif

endfunction


%!test
%! assert (isempty (gcbo));

