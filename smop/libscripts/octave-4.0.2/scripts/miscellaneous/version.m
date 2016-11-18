## Copyright (C) 1994-2015 John W. Eaton
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
## @deftypefn {Function File} {} version ()
## Return the version number of Octave as a string.
##
## This is an alias for the function @w{@env{OCTAVE_VERSION}} provided for
## compatibility.
## @seealso{OCTAVE_VERSION, ver}
## @end deftypefn

## Author: jwe

function vs = version ()

  if (nargin != 0)
    warning ("version: ignoring extra arguments");
  endif

  vs = OCTAVE_VERSION;

endfunction


%!assert (ischar (version ()))
%!assert (version (), OCTAVE_VERSION)

%!warning version (1);

