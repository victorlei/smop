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
## @deftypefn {Function File} {@var{fig} =} gcbf ()
## Return a handle to the figure containing the object whose callback is
## currently executing.
##
## If no callback is executing, this function returns the empty matrix.  The
## handle returned by this function is the same as the second output argument
## of @code{gcbo}.
##
## @seealso{gcbo, gcf, gco, gca, get, set}
## @end deftypefn

function fig = gcbf ()

  [~, fig] = gcbo ();

endfunction


%!assert (isempty (gcbf))

