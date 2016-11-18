## Copyright (C) 2012-2015 John W. Eaton
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
## @deftypefn {Function File} {} loadprefs ()
## Undocumented internal function.
## @end deftypefn

## Author: jwe

function retval = loadprefs ()

  file = prefsfile ();

  s = stat (file);

  if (isstruct (s) && S_ISREG (s.mode))
    tmp = load (file);
    retval= tmp.prefs;
  else
    retval = [];
  endif

endfunction


## Testing these functions will require some care to avoid wiping out
## existing (or creating unwanted) preferences for the user running the
## tests.

