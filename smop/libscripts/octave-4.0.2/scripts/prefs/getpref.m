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
## @deftypefn  {Function File} {} getpref (@var{group}, @var{pref})
## @deftypefnx {Function File} {} getpref (@var{group}, @var{pref}, @var{default})
## @deftypefnx {Function File} {} getpref (@var{group})
## Return the preference value corresponding to the named preference @var{pref}
## in the preference group @var{group}.
##
## The named preference group must be a character string.
##
## If @var{pref} does not exist in @var{group} and @var{default} is
## specified, return @var{default}.
##
## The preference @var{pref} may be a character string or a cell array of
## character strings.
##
## The corresponding default value @var{default} may be any value, or, if
## @var{pref} is a cell array of strings, @var{default} must be a cell array
## of values with the same size as @var{pref}.
##
## If neither @var{pref} nor @var{default} are specified, return a structure
## of preferences for the preference group @var{group}.
##
## If no arguments are specified, return a structure containing all groups of
## preferences and their values.
## @seealso{addpref, setpref, ispref, rmpref}
## @end deftypefn

## Author: jwe

function retval = getpref (group, pref, default)

  if (nargin == 0)
    retval = loadprefs ();
  elseif (nargin == 1)
    if (ischar (group))
      prefs = loadprefs ();
      if (isfield (prefs, group))
        retval = prefs.(group);
      else
        retval = [];
      endif
    else
      error ("expecting group to be a character string");
    endif
  elseif (nargin == 2 || nargin == 3)
    grp = getpref (group);
    if (ischar (pref))
      if (isfield (grp, pref))
        retval = grp.(pref);
      elseif (nargin == 3)
        retval = default;
      else
        error ("preference %s does not exist in group %s", pref, group);
      endif
    elseif (iscellstr (pref))
      if (nargin == 2 || size_equal (pref, default))
        for i = 1:numel (pref)
          if (isfield (grp, pref{i}))
            retval.(pref) = grp.(pref{i});
          elseif (nargin == 3)
            retval.(pref) = default{i};
          else
            error ("preference %s does not exist in group %s", pref{i}, group);
          endif
        endfor
      else
        error ("size mismatch for pref and default");
      endif
    else
      error ("expecting pref to be a character string or cellstr");
    endif
  else
    print_usage ();
  endif

endfunction


## Testing these functions will require some care to avoid wiping out
## existing (or creating unwanted) preferences for the user running the
## tests.

