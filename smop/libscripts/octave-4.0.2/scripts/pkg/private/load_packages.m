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
## @deftypefn {Function File} {} load_packages (@var{files}, @var{handle_deps}, @var{local_list}, @var{global_list})
## Undocumented internal function.
## @end deftypefn

function load_packages (files, handle_deps, local_list, global_list)
  installed_pkgs_lst = installed_packages (local_list, global_list);
  num_packages = length (installed_pkgs_lst);

  ## Read package names and installdirs into a more convenient format.
  pnames = pdirs = cell (1, num_packages);
  for i = 1:num_packages
    pnames{i} = installed_pkgs_lst{i}.name;
    pdirs{i} = installed_pkgs_lst{i}.dir;
  endfor

  ## Load all.
  if (length (files) == 1 && strcmp (files{1}, "all"))
    idx = [1:length(installed_pkgs_lst)];
  ## Load auto.
  elseif (length (files) == 1 && strcmp (files{1}, "auto"))
    idx = [];
    for i = 1:length (installed_pkgs_lst)
      if (exist (fullfile (pdirs{i}, "packinfo", ".autoload"), "file"))
        idx(end + 1) = i;
      endif
    endfor
  ## Load package_name1 ...
  else
    idx = [];
    for i = 1:length (files)
      idx2 = find (strcmp (pnames, files{i}));
      if (! any (idx2))
          error ("package %s is not installed", files{i});
      endif
      idx(end + 1) = idx2;
    endfor
  endif

  ## Load the packages, but take care of the ordering of dependencies.
  load_packages_and_dependencies (idx, handle_deps, installed_pkgs_lst, true);
endfunction

