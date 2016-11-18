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
## @deftypefn {Function File} {} unload_packages (@var{files}, @var{handle_deps}, @var{local_list}, @var{global_list})
## Undocumented internal function.
## @end deftypefn

function unload_packages (files, handle_deps, local_list, global_list)
  installed_pkgs_lst = installed_packages (local_list, global_list);
  num_packages = length (installed_pkgs_lst);

  ## Read package names and installdirs into a more convenient format.
  pnames = pdirs = cell (1, num_packages);
  for i = 1:num_packages
    pnames{i} = installed_pkgs_lst{i}.name;
    pdirs{i} = installed_pkgs_lst{i}.dir;
    pdeps{i} = installed_pkgs_lst{i}.depends;
  endfor

  ## Get the current octave path.
  p = strtrim (ostrsplit (path (), pathsep ()));

  if (length (files) == 1 && strcmp (files{1}, "all"))
    ## Unload all.
    dirs = pdirs;
    desc = installed_pkgs_lst;
  else
    ## Unload package_name1 ...
    dirs = {};
    desc = {};
    for i = 1:length (files)
      idx = strcmp (pnames, files{i});
      if (! any (idx))
        error ("package %s is not installed", files{i});
      endif
        dirs{end+1} = pdirs{idx};
        desc{end+1} = installed_pkgs_lst{idx};
      endfor
  endif

  ## Check for architecture dependent directories.
  archdirs = {};
  for i = 1:length (dirs)
    tmpdir = getarchdir (desc{i});
    if (exist (tmpdir, "dir"))
      archdirs{end+1} = dirs{i};
      archdirs{end+1} = tmpdir;
    else
      archdirs{end+1} = dirs{i};
    endif
  endfor

  ## Unload the packages.
  for i = 1:length (archdirs)
    d = archdirs{i};
    idx = strcmp (p, d);
    if (any (idx))
      rmpath (d);
      ## FIXME: We should also check if we need to remove items from EXEC_PATH.
    endif
  endfor
endfunction

