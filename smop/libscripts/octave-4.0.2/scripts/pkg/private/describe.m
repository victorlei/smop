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
## @deftypefn {Function File} {[@var{pkg_desc_list}, @var{flag}] =} describe (@var{pkgnames}, @var{verbose}, @var{local_list}, @var{global_list})
## Undocumented internal function.
## @end deftypefn

function [pkg_desc_list, flag] = describe (pkgnames, verbose,
                                           local_list, global_list)

  ## Get the list of installed packages.
  installed_pkgs_lst = installed_packages(local_list, global_list);
  num_packages = length (installed_pkgs_lst);

  describe_all = false;
  if (any (strcmp ("all", pkgnames)))
    describe_all = true;
    flag(1:num_packages) = {"Not Loaded"};
    num_pkgnames = num_packages;
  else
    num_pkgnames = length (pkgnames);
    flag(1:num_pkgnames) = {"Not installed"};
  endif

  for i = 1:num_packages
    curr_name = installed_pkgs_lst{i}.name;
    if (describe_all)
      name_pos = i;
    else
      name_pos = find (strcmp (curr_name, pkgnames));
    endif

    if (! isempty (name_pos))
      if (installed_pkgs_lst{i}.loaded)
        flag{name_pos} = "Loaded";
      else
        flag{name_pos} = "Not loaded";
      endif

      pkg_desc_list{name_pos}.name = installed_pkgs_lst{i}.name;
      pkg_desc_list{name_pos}.version = installed_pkgs_lst{i}.version;
      pkg_desc_list{name_pos}.description = installed_pkgs_lst{i}.description;
      pkg_desc_list{name_pos}.provides = parse_pkg_idx (installed_pkgs_lst{i}.dir);

    endif
  endfor

  non_inst = find (strcmp (flag, "Not installed"));
  if (! isempty (non_inst))
    if (nargout < 2)
      non_inst_str = sprintf (" %s ", pkgnames{non_inst});
      error ("some packages are not installed: %s", non_inst_str);
    else
      pkg_desc_list{non_inst} = struct ("name", {}, "description",
                                        {}, "provides", {});
    endif
  endif

  if (nargout == 0)
    for i = 1:num_pkgnames
      print_package_description (pkg_desc_list{i}.name,
                                 pkg_desc_list{i}.version,
                                 pkg_desc_list{i}.provides,
                                 pkg_desc_list{i}.description,
                                 flag{i}, verbose);
    endfor
  endif

endfunction

