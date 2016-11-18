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
## @deftypefn  {Function File} {} rmpref (@var{group}, @var{pref})
## @deftypefnx {Function File} {} rmpref (@var{group})
## Remove the named preference @var{pref} from the preference group @var{group}.
##
## The named preference group must be a character string.
##
## The preference @var{pref} may be a character string or cell array of strings.
##
## If @var{pref} is not specified, remove the preference group @var{group}.
##
## It is an error to remove a nonexistent preference or group.
## @seealso{addpref, ispref, setpref, getpref}
## @end deftypefn

## Author: jwe

function retval = rmpref (group, pref)

  if (nargin < 1 || nargin > 2)
    print_usage ();
  elseif (! ischar (group))
    error ("rmpref: GROUP must be a string");
  elseif (nargin == 2 && ! (ischar (pref) || iscellstr (pref)))
    error ("rmpref: PREF must be a string or cell array of strings");
  endif

  if (nargin == 1)
    if (ispref (group))
      prefs = loadprefs ();
      prefs = rmfield (prefs, group);
      saveprefs (prefs);
    else
      error ("rmpref: group <%s> does not exist", group);
    endif
  else
    valid = ispref (group, pref);
    if (all (valid))
      prefs = loadprefs ();
      prefs.(group) = rmfield (prefs.(group), pref);
      saveprefs (prefs);
    else
      if (! ispref (group))
        error ("rmpref: group <%s> does not exist", group);
      else
        idx = find (! valid, 1);
        error ("rmpref: pref <%s> does not exist", (cellstr (pref)){idx} );
      endif
    endif
  endif

endfunction


## Testing these functions will require some care to avoid wiping out
## existing (or creating unwanted) preferences for the user running the
## tests.

## Test input validation
%!error rmpref ()
%!error rmpref (1,2,3)
%!error rmpref ({"__group1__"})
%!error rmpref ("__group1__", 1)

