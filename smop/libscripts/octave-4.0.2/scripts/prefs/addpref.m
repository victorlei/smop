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
## @deftypefn {Function File} {} addpref (@var{group}, @var{pref}, @var{val})
## Add a preference @var{pref} and associated value @var{val} to the named
## preference group @var{group}.
##
## The named preference group must be a character string.
##
## The preference @var{pref} may be a character string or a cell array of
## character strings.
##
## The corresponding value @var{val} may be any value, or, if @var{pref} is a
## cell array of strings, @var{val} must be a cell array of values with the
## same size as @var{pref}.
## @seealso{setpref, getpref, ispref, rmpref}
## @end deftypefn

## Author: jwe

function addpref (group, pref, val)

  if (nargin == 3)
    if (ischar (group))
      prefs = loadprefs ();
      if (ischar (pref))
        if (isfield (group, pref))
          error ("preference %s already exists in group %s", pref, group);
        else
          prefs.(group).(pref) = val;
        endif
      elseif (iscellstr (pref))
        if (size_equal (pref, val))
          for i = 1:numel (pref)
            if (isfield (group, pref{i}))
              error ("preference %s already exists in group %s",
                     pref{i}, group);
            else
              prefs.(group).(pref{i}) = val;
            endif
          endfor
        else
          error ("size mismatch for pref and val");
        endif
      else
        error ("expecting pref to be a character string or cellstr");
      endif
      saveprefs (prefs);
    else
      error ("expecting group to be a character string");
    endif
  else
    print_usage ();
  endif

endfunction


## Testing these functions will require some care to avoid wiping out
## existing (or creating unwanted) preferences for the user running the
## tests.

