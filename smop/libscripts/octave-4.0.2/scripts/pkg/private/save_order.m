## Copyright (C) 2005-2015 Søren Hauberg
## Copyright (C) 2010 VZLU Prague, a.s.
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
## @deftypefn {Function File} {@var{newdesc} =} save_order (@var{desc})
## Undocumented internal function.
## @end deftypefn

function newdesc = save_order (desc)
  newdesc = {};
  for i = 1 : length (desc)
    deps = desc{i}.depends;
    if (isempty (deps)
        || (length (deps) == 1 && strcmp (deps{1}.package, "octave")))
      newdesc{end + 1} = desc{i};
    else
      tmpdesc = {};
      for k = 1 : length (deps)
        for j = 1 : length (desc)
          if (strcmp (desc{j}.name, deps{k}.package))
            tmpdesc{end+1} = desc{j};
            break;
          endif
        endfor
      endfor
      if (! isempty (tmpdesc))
        newdesc = {newdesc{:}, save_order(tmpdesc){:}, desc{i}};
      else
        newdesc{end+1} = desc{i};
      endif
    endif
  endfor
  ## Eliminate the duplicates.
  idx = [];
  for i = 1 : length (newdesc)
    for j = (i + 1) : length (newdesc)
      if (strcmp (newdesc{i}.name, newdesc{j}.name))
        idx(end + 1) = j;
      endif
    endfor
  endfor
  newdesc(idx) = [];
endfunction

