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
## @deftypefn {Function File} {@var{deps_cell} =} fix_depends (@var{depends})
## Undocumented internal function.
## @end deftypefn

## Make sure the depends field is of the right format.
## This function returns a cell of structures with the following fields:
##   package, version, operator
function deps_cell = fix_depends (depends)
  deps = strtrim (ostrsplit (tolower (depends), ","));
  deps_cell = cell (1, length (deps));
  dep_pat = ...
  '\s*(?<name>[-\w]+)\s*(\(\s*(?<op>[<>=]+)\s*(?<ver>\d+\.\d+(\.\d+)*)\s*\))*\s*';

  ## For each dependency.
  for i = 1:length (deps)
    dep = deps{i};
    [start, nm] = regexp (dep, dep_pat, 'start', 'names');
    ## Is the dependency specified
    ## in the correct format?
    if (! isempty (start))
      package = tolower (strtrim (nm.name));
      ## Does the dependency specify a version
      ## Example: package(>= version).
      if (! isempty (nm.ver))
        operator = nm.op;
        if (! any (strcmp (operator, {">", ">=", "<=", "<", "=="})))
          error ("unsupported operator: %s", operator);
        endif
        version = fix_version (nm.ver);
        ## If no version is specified for the dependency
        ## we say that the version should be greater than
        ## or equal to "0.0.0".
      else
        package = tolower (strtrim (dep));
        operator = ">=";
        version  = "0.0.0";
      endif
      deps_cell{i} = struct ("package", package,
                             "operator", operator,
                             "version", version);
    else
      error ("incorrect syntax for dependency '%s' in the DESCRIPTION file\n",
             dep);
    endif
  endfor
endfunction

