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
## @deftypefn {Function File} {} uninstall (@var{pkgnames}, @var{handle_deps}, @var{verbose}, @var{local_list}, @var{global_list}, @var{global_install})
## Undocumented internal function.
## @end deftypefn

function uninstall (pkgnames, handle_deps, verbose, local_list,
                    global_list, global_install)
  ## Get the list of installed packages.
  [local_packages, global_packages] = installed_packages(local_list,
                                                         global_list);
  if (global_install)
    installed_pkgs_lst = {local_packages{:}, global_packages{:}};
  else
    installed_pkgs_lst = local_packages;
  endif

  num_packages = length (installed_pkgs_lst);
  delete_idx = [];
  for i = 1:num_packages
    cur_name = installed_pkgs_lst{i}.name;
    if (any (strcmp (cur_name, pkgnames)))
      delete_idx(end+1) = i;
    endif
  endfor

  ## Are all the packages that should be uninstalled already installed?
  if (length (delete_idx) != length (pkgnames))
    if (global_install)
      ## Try again for a locally installed package.
      installed_pkgs_lst = local_packages;

      num_packages = length (installed_pkgs_lst);
      delete_idx = [];
      for i = 1:num_packages
        cur_name = installed_pkgs_lst{i}.name;
        if (any (strcmp (cur_name, pkgnames)))
          delete_idx(end+1) = i;
        endif
      endfor
      if (length (delete_idx) != length (pkgnames))
        ## FIXME: We should have a better error message.
        warning ("some of the packages you want to uninstall are not installed");
      endif
    else
      ## FIXME: We should have a better error message.
      warning ("some of the packages you want to uninstall are not installed");
    endif
  endif

  if (isempty (delete_idx))
    warning ("no packages will be uninstalled");
  else

    ## Compute the packages that will remain installed.
    idx = setdiff (1:num_packages, delete_idx);
    remaining_packages = {installed_pkgs_lst{idx}};

    ## Check dependencies.
    if (handle_deps)
      error_text = "";
      for i = 1:length (remaining_packages)
        desc = remaining_packages{i};
        bad_deps = get_unsatisfied_deps (desc, remaining_packages);

        ## Will the uninstallation break any dependencies?
        if (! isempty (bad_deps))
          for i = 1:length (bad_deps)
            dep = bad_deps{i};
            error_text = [error_text " " desc.name " needs " ...
                          dep.package " " dep.operator " " dep.version "\n"];
          endfor
        endif
      endfor

      if (! isempty (error_text))
        error ("the following dependencies where unsatisfied:\n  %s", error_text);
      endif
    endif

    ## Delete the directories containing the packages.
    for i = delete_idx
      desc = installed_pkgs_lst{i};
      ## If an 'on_uninstall.m' exist, call it!
      if (exist (fullfile (desc.dir, "packinfo", "on_uninstall.m"), "file"))
        wd = pwd ();
        cd (fullfile (desc.dir, "packinfo"));
        on_uninstall (desc);
        cd (wd);
      endif
      ## Do the actual deletion.
      if (desc.loaded)
        rmpath (desc.dir);
        if (exist (getarchdir (desc)))
          rmpath (getarchdir (desc));
        endif
      endif
      if (exist (desc.dir, "dir"))
        [status, msg] = rmdir (desc.dir, "s");
        if (status != 1 && exist (desc.dir, "dir"))
          error ("couldn't delete directory %s: %s", desc.dir, msg);
        endif
        [status, msg] = rmdir (getarchdir (desc), "s");
        if (status != 1 && exist (getarchdir (desc), "dir"))
          error ("couldn't delete directory %s: %s", getarchdir (desc), msg);
        endif
        if (dirempty (desc.archprefix))
          rmdir (desc.archprefix, "s");
        endif
      else
        warning ("directory %s previously lost", desc.dir);
      endif
    endfor

    ## Write a new ~/.octave_packages.
    if (global_install)
      if (length (remaining_packages) == 0)
        unlink (global_list);
      else
        global_packages = save_order (remaining_packages);
        save (global_list, "global_packages");
      endif
    else
      if (length (remaining_packages) == 0)
        unlink (local_list);
      else
        local_packages = save_order (remaining_packages);
        save (local_list, "local_packages");
      endif
    endif
  endif

endfunction

