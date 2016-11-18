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
## @deftypefn  {Function File} {} ispref (@var{group}, @var{pref})
## @deftypefnx {Function File} {} ispref (@var{group})
## Return true if the named preference @var{pref} exists in the preference
## group @var{group}.
##
## The named preference group must be a character string.
##
## The preference @var{pref} may be a character string or a cell array of
## character strings.
##
## If @var{pref} is not specified, return true if the preference group
## @var{group} exists.
## @seealso{getpref, addpref, setpref, rmpref}
## @end deftypefn

## Author: jwe

function retval = ispref (group, pref)

  if (nargin == 1)
    retval = isfield (loadprefs (), group);
  elseif (nargin == 2)
    prefs = loadprefs ();
    if (isfield (prefs, group))
      grp = prefs.(group);
      if (ischar (pref) || iscellstr (pref))
        retval = isfield (grp, pref);
      else
        retval = false;
      endif
    else
      retval = false;
    endif
  else
    print_usage ();
  endif

endfunction


## Testing these functions will require some care to avoid wiping out
## existing (or creating unwanted) preferences for the user running the
## tests.

