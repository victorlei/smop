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
## @deftypefn {Function File} {@var{idx} =} load_package_dirs (@var{lidx}, @var{idx}, @var{handle_deps}, @var{installed_pkgs_lst})
## Undocumented internal function.
## @end deftypefn

function idx = load_package_dirs (lidx, idx, handle_deps, installed_pkgs_lst)
  for i = lidx
    if (isfield (installed_pkgs_lst{i}, "loaded")
        && installed_pkgs_lst{i}.loaded)
      continue;
    else
      ## Insert this package at the front before recursing over dependencies.
      if (! any (idx == i))
        idx = [i, idx];
      endif

      if (handle_deps)
        deps = installed_pkgs_lst{i}.depends;
        if ((length (deps) > 1)
            || (length (deps) == 1 && ! strcmp (deps{1}.package, "octave")))
          tmplidx = [];
          for k = 1 : length (deps)
            for j = 1 : length (installed_pkgs_lst)
              if (strcmp (installed_pkgs_lst{j}.name, deps{k}.package))
                if (! any (idx == j))
                  tmplidx(end + 1) = j;
                  break;
                endif
              endif
            endfor
          endfor
          idx = load_package_dirs (tmplidx, idx, handle_deps,
                                 installed_pkgs_lst);
        endif
      endif
    endif
  endfor
endfunction

