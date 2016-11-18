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
## @deftypefn {Function File} {} closereq ()
## Close the current figure and delete all graphics objects associated with it.
##
## By default, the @qcode{"closerequestfcn"} property of a new plot figure
## points to this function.
## @seealso{close, delete}
## @end deftypefn

## Author: jwe

function closereq ()

  if (nargin != 0)
    print_usage ();
  endif

  cf = gcbf ();
  if (isempty (cf))
    warning ("closereq: calling closereq from octave prompt is not supported, use 'close' instead");
    cf = get (0, "currentfigure");
  endif
  if (! isempty (cf) && isfigure (cf))
    delete (cf);
  endif

endfunction

