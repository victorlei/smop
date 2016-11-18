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
## @deftypefn {Function File} {@var{dep} =} is_architecture_dependent (@var{nm})
## Undocumented internal function.
## @end deftypefn

function dep = is_architecture_dependent (nm)
  persistent archdepsuffix = {".oct",".mex",".a",".lib",".so",".so.*",".dll","dylib"};

  dep = false;
  for i = 1 : length (archdepsuffix)
    ext = archdepsuffix{i};
    if (ext(end) == "*")
      isglob = true;
      ext(end) = [];
    else
      isglob = false;
    endif
    pos = strfind (nm, ext);
    if (pos)
      if (! isglob && (length (nm) - pos(end) != length (ext) - 1))
        continue;
      endif
      dep = true;
      break;
    endif
  endfor
endfunction

