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
## @deftypefn {Function File} {} load_packages_and_dependencies (@var{idx}, @var{handle_deps}, @var{installed_pkgs_lst}, @var{global_install})
## Undocumented internal function.
## @end deftypefn

function load_packages_and_dependencies (idx, handle_deps, installed_pkgs_lst,
                                         global_install)
  idx = load_package_dirs (idx, [], handle_deps, installed_pkgs_lst);
  dirs = {};
  execpath = EXEC_PATH ();
  for i = idx;
    ndir = installed_pkgs_lst{i}.dir;
    dirs{end+1} = ndir;
    if (exist (fullfile (dirs{end}, "bin"), "dir"))
      execpath = [execpath pathsep() fullfile(dirs{end}, "bin")];
    endif
    tmpdir = getarchdir (installed_pkgs_lst{i});
    if (exist (tmpdir, "dir"))
      dirs{end + 1} = tmpdir;
      if (exist (fullfile (dirs{end}, "bin"), "dir"))
        execpath = [execpath pathsep() fullfile(dirs{end}, "bin")];
      endif
    endif
  endfor

  ## Load the packages.
  if (length (dirs) > 0)
    addpath (dirs{:});
  endif

  ## Add the binaries to exec_path.
  if (! strcmp (EXEC_PATH, execpath))
    EXEC_PATH (execpath);
  endif
endfunction

