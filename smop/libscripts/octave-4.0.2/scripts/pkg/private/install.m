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
## @deftypefn {Function File} {} install (@var{files}, @var{handle_deps}, @var{autoload}, @var{prefix}, @var{archprefix}, @var{verbose}, @var{local_list}, @var{global_list}, @var{global_install})
## Undocumented internal function.
## @end deftypefn

function install (files, handle_deps, autoload, prefix, archprefix, verbose,
                  local_list, global_list, global_install)

  ## Check that the directory in prefix exist. If it doesn't: create it!
  if (! exist (prefix, "dir"))
    warning ("creating installation directory %s", prefix);
    [status, msg] = mkdir (prefix);
    if (status != 1)
      error ("could not create installation directory: %s", msg);
    endif
  endif

  ## Get the list of installed packages.
  [local_packages, global_packages] = installed_packages (local_list,
                                                          global_list);

  installed_pkgs_lst = {local_packages{:}, global_packages{:}};

  if (global_install)
    packages = global_packages;
  else
    packages = local_packages;
  endif

  ## Uncompress the packages and read the DESCRIPTION files.
  tmpdirs = packdirs = descriptions = {};
  try
    ## Warn about non existent files.
    for i = 1:length (files)
      if (isempty (glob (files{i})))
        warning ("file %s does not exist", files{i});
      endif
    endfor

    ## Unpack the package files and read the DESCRIPTION files.
    files = glob (files);
    packages_to_uninstall = [];
    for i = 1:length (files)
      tgz = files{i};

      if (exist (tgz, "file"))
        ## Create a temporary directory.
        tmpdir = tempname ();
        tmpdirs{end+1} = tmpdir;
        if (verbose)
          printf ("mkdir (%s)\n", tmpdir);
        endif
        [status, msg] = mkdir (tmpdir);
        if (status != 1)
          error ("couldn't create temporary directory: %s", msg);
        endif

        ## Uncompress the package.
        if (verbose)
          printf ("untar (%s, %s)\n", tgz, tmpdir);
        endif
        untar (tgz, tmpdir);

        ## Get the name of the directories produced by tar.
        [dirlist, err, msg] = readdir (tmpdir);
        if (err)
          error ("couldn't read directory produced by tar: %s", msg);
        endif

        if (length (dirlist) > 3)
          error ("bundles of packages are not allowed");
        endif
      endif

      ## The filename pointed to an uncompressed package to begin with.
      if (exist (tgz, "dir"))
        dirlist = {".", "..", tgz};
      endif

      if (exist (tgz, "file") || exist (tgz, "dir"))
        ## The two first entries of dirlist are "." and "..".
        if (exist (tgz, "file"))
          packdir = fullfile (tmpdir, dirlist{3});
        else
          packdir = fullfile (pwd (), dirlist{3});
        endif
        packdirs{end+1} = packdir;

        ## Make sure the package contains necessary files.
        verify_directory (packdir);

        ## Read the DESCRIPTION file.
        filename = fullfile (packdir, "DESCRIPTION");
        desc = get_description (filename);

        ## Verify that package name corresponds with filename.
        [dummy, nm] = fileparts (tgz);
        if ((length (nm) >= length (desc.name))
            && ! strcmp (desc.name, nm(1:length (desc.name))))
          error ("package name '%s' doesn't correspond to its filename '%s'",
                 desc.name, nm);
        endif

        ## Set default installation directory.
        desc.dir = fullfile (prefix, [desc.name "-" desc.version]);

        ## Set default architectire dependent installation directory.
        desc.archprefix = fullfile (archprefix, [desc.name "-" desc.version]);

        ## Save desc.
        descriptions{end+1} = desc;

        ## Are any of the new packages already installed?
        ## If so we'll remove the old version.
        for j = 1:length (packages)
          if (strcmp (packages{j}.name, desc.name))
            packages_to_uninstall(end+1) = j;
          endif
        endfor
      endif
    endfor
  catch
    ## Something went wrong, delete tmpdirs.
    for i = 1:length (tmpdirs)
      rmdir (tmpdirs{i}, "s");
    endfor
    rethrow (lasterror ());
  end_try_catch

  ## Check dependencies.
  if (handle_deps)
    ok = true;
    error_text = "";
    for i = 1:length (descriptions)
      desc = descriptions{i};
      idx2 = setdiff (1:length (descriptions), i);
      if (global_install)
        ## Global installation is not allowed to have dependencies on locally
        ## installed packages.
        idx1 = setdiff (1:length (global_packages), packages_to_uninstall);
        pseudo_installed_packages = {global_packages{idx1}, ...
                                     descriptions{idx2}};
      else
        idx1 = setdiff (1:length (local_packages), packages_to_uninstall);
        pseudo_installed_packages = {local_packages{idx1}, ...
                                     global_packages{:}, ...
                                     descriptions{idx2}};
      endif
      bad_deps = get_unsatisfied_deps (desc, pseudo_installed_packages);
      ## Are there any unsatisfied dependencies?
      if (! isempty (bad_deps))
        ok = false;
        for i = 1:length (bad_deps)
          dep = bad_deps{i};
          error_text = [error_text " " desc.name " needs " ...
                        dep.package " " dep.operator " " dep.version "\n"];
        endfor
      endif
    endfor

    ## Did we find any unsatisfied dependencies?
    if (! ok)
      error ("the following dependencies were unsatisfied:\n  %s", error_text);
    endif
  endif

  ## Prepare each package for installation.
  try
    for i = 1:length (descriptions)
      desc = descriptions{i};
      pdir = packdirs{i};
      prepare_installation (desc, pdir);
      configure_make (desc, pdir, verbose);
    endfor
  catch
    ## Something went wrong, delete tmpdirs.
    for i = 1:length (tmpdirs)
      rmdir (tmpdirs{i}, "s");
    endfor
    rethrow (lasterror ());
  end_try_catch

  ## Uninstall the packages that will be replaced.
  try
    for i = packages_to_uninstall
      if (global_install)
        uninstall ({global_packages{i}.name}, false, verbose, local_list,
                   global_list, global_install);
      else
        uninstall ({local_packages{i}.name}, false, verbose, local_list,
                   global_list, global_install);
      endif
    endfor
  catch
    ## Something went wrong, delete tmpdirs.
    for i = 1:length (tmpdirs)
      rmdir (tmpdirs{i}, "s");
    endfor
    rethrow (lasterror ());
  end_try_catch

  ## Install each package.
  try
    for i = 1:length (descriptions)
      desc = descriptions{i};
      pdir = packdirs{i};
      copy_files (desc, pdir, global_install);
      create_pkgadddel (desc, pdir, "PKG_ADD", global_install);
      create_pkgadddel (desc, pdir, "PKG_DEL", global_install);
      finish_installation (desc, pdir, global_install);
      generate_lookfor_cache (desc);
    endfor
  catch
    ## Something went wrong, delete tmpdirs.
    for i = 1:length (tmpdirs)
      rmdir (tmpdirs{i}, "s");
    endfor
    for i = 1:length (descriptions)
      rmdir (descriptions{i}.dir, "s");
      rmdir (getarchdir (descriptions{i}), "s");
    endfor
    rethrow (lasterror ());
  end_try_catch

  ## Check if the installed directory is empty. If it is remove it
  ## from the list.
  for i = length (descriptions):-1:1
    if (dirempty (descriptions{i}.dir, {"packinfo", "doc"})
        && dirempty (getarchdir (descriptions{i})))
      warning ("package %s is empty\n", descriptions{i}.name);
      rmdir (descriptions{i}.dir, "s");
      rmdir (getarchdir (descriptions{i}), "s");
      descriptions(i) = [];
    endif
  endfor

  ## If the package requested that it is autoloaded, or the installer
  ## requested that it is, then mark the package as autoloaded.
  str_true = {"true", "on", "yes", "1"};
  for i = length (descriptions):-1:1

    desc_autoload = false;
    if (isfield (descriptions{i}, "autoload"))
      a = descriptions{i}.autoload;
      desc_autoload = ((isnumeric (a) && a > 0)
                       || (ischar (a)
                           && any (strcmpi (a, str_true))));
    endif

    if (autoload > 0 || (autoload == 0 && desc_autoload))
      fclose (fopen (fullfile (descriptions{i}.dir, "packinfo",
                               ".autoload"), "wt"));
      descriptions{i}.autoload = 1;
    else
      descriptions{i}.autoload = 0;
    endif

  endfor

  ## Add the packages to the package list.
  try
    if (global_install)
      idx = setdiff (1:length (global_packages), packages_to_uninstall);
      global_packages = save_order ({global_packages{idx}, descriptions{:}});
      save (global_list, "global_packages");
      installed_pkgs_lst = {local_packages{:}, global_packages{:}};
    else
      idx = setdiff (1:length (local_packages), packages_to_uninstall);
      local_packages = save_order ({local_packages{idx}, descriptions{:}});
      save (local_list, "local_packages");
      installed_pkgs_lst = {local_packages{:}, global_packages{:}};
    endif
  catch
    ## Something went wrong, delete tmpdirs.
    for i = 1:length (tmpdirs)
      rmdir (tmpdirs{i}, "s");
    endfor
    for i = 1:length (descriptions)
      rmdir (descriptions{i}.dir, "s");
    endfor
    if (global_install)
      printf ("error: couldn't append to %s\n", global_list);
    else
      printf ("error: couldn't append to %s\n", local_list);
    endif
    rethrow (lasterror ());
  end_try_catch

  ## All is well, let's clean up.
  for i = 1:length (tmpdirs)
    [status, msg] = rmdir (tmpdirs{i}, "s");
    if (status != 1 && exist (tmpdirs{i}, "dir"))
      warning ("couldn't clean up after my self: %s\n", msg);
    endif
  endfor

  ## Add the newly installed packages to the path, so the user
  ## can begin using them. Only load them if they are marked autoload.
  if (length (descriptions) > 0)
    idx = [];
    for i = 1:length (descriptions)
      if (descriptions{i}.autoload > 0)
        nm = descriptions{i}.name;
        for j = 1:length (installed_pkgs_lst)
          if (strcmp (nm, installed_pkgs_lst{j}.name))
            idx(end + 1) = j;
            break;
          endif
        endfor
      endif
    endfor
    load_packages_and_dependencies (idx, handle_deps, installed_pkgs_lst,
                                    global_install);
  endif

  ## If there is a NEWS file, mention it.
  ## Check if desc exists too because it's possible to get to this point
  ## without creating it such as giving an invalid filename for the package
  if (exist ("desc", "var")
      && exist (fullfile (desc.dir, "packinfo", "NEWS"), "file"))
    printf ("For information about changes from previous versions of the %s package, run 'news %s'.\n",
            desc.name, desc.name);
  endif

endfunction

