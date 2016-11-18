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
## @deftypefn {Function File} {@var{descriptions} =} rebuild (@var{prefix}, @var{archprefix}, @var{list}, @var{files}, @var{auto}, @var{verbose})
## Undocumented internal function.
## @end deftypefn

function descriptions = rebuild (prefix, archprefix, list, files, auto, verbose)
  if (isempty (files))
    [dirlist, err, msg] = readdir (prefix);
    if (err)
      error ("couldn't read directory %s: %s", prefix, msg);
    endif
    ## the two first entries of dirlist are "." and ".."
    dirlist([1,2]) = [];
  else
    old_descriptions = installed_packages (list, list);
    wd = pwd ();
    unwind_protect
      cd (prefix);
      dirlist = glob (strcat (files, '-*'));
    unwind_protect_cleanup
      cd (wd);
    end_unwind_protect
  endif
  descriptions = {};
  for k = 1:length (dirlist)
    descfile = fullfile (prefix, dirlist{k}, "packinfo", "DESCRIPTION");
    if (verbose)
      printf ("recreating package description from %s\n", dirlist{k});
    endif
    if (exist (descfile, "file"))
      desc = get_description (descfile);
      desc.dir = fullfile (prefix, dirlist{k});
      desc.archprefix = fullfile (archprefix, [desc.name "-" desc.version]);
      if (auto != 0)
        if (exist (fullfile (desc.dir, "packinfo", ".autoload"), "file"))
          unlink (fullfile (desc.dir, "packinfo", ".autoload"));
        endif
        if (auto < 0)
          desc.autoload = 0;
        elseif (auto > 0)
          desc.autoload = 1;
          fclose (fopen (fullfile (desc.dir, "packinfo", ".autoload"), "wt"));
        endif
      else
        if (exist (fullfile (desc.dir, "packinfo", ".autoload"), "file"))
          desc.autoload = 1;
        else
          desc.autoload = 0;
        endif
      endif
      descriptions{end + 1} = desc;
    elseif (verbose)
      warning ("directory %s is not a valid package", dirlist{k});
    endif
  endfor

  if (! isempty (files))
    ## We are rebuilding for a particular package(s) so we should take
    ## care to keep the other untouched packages in the descriptions
    descriptions = {descriptions{:}, old_descriptions{:}};

    dup = [];
    for i = 1:length (descriptions)
      if (any (dup == i))
        continue;
      endif
      for j = (i+1):length (descriptions)
        if (any (dup == j))
          continue;
        endif
        if (strcmp (descriptions{i}.name, descriptions{j}.name))
          dup = [dup, j];
        endif
      endfor
    endfor
    if (! isempty (dup))
      descriptions(dup) = [];
    endif
  endif
endfunction

