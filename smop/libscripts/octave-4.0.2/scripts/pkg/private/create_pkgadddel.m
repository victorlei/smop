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
## @deftypefn {Function File} {} create_pkgadddel (@var{desc}, @var{packdir}, @var{nm}, @var{global_install})
## Undocumented internal function.
## @end deftypefn

function create_pkgadddel (desc, packdir, nm, global_install)
  instpkg = fullfile (desc.dir, nm);
  instfid = fopen (instpkg, "wt");
  ## If it is exists, most of the  PKG_* file should go into the
  ## architecture dependent directory so that the autoload/mfilename
  ## commands work as expected. The only part that doesn't is the
  ## part in the main directory.
  archdir = fullfile (getarchprefix (desc, global_install),
                      [desc.name "-" desc.version], getarch ());
  if (exist (getarchdir (desc, global_install), "dir"))
    archpkg = fullfile (getarchdir (desc, global_install), nm);
    archfid = fopen (archpkg, "at");
  else
    archpkg = instpkg;
    archfid = instfid;
  endif

  if (archfid >= 0 && instfid >= 0)
    ## Search all dot-m files for PKG commands.
    lst = dir (fullfile (packdir, "inst", "*.m"));
    for i = 1:length (lst)
      nam = fullfile (packdir, "inst", lst(i).name);
      fwrite (instfid, extract_pkg (nam, ['^[#%][#%]* *' nm ': *(.*)$']));
    endfor

    ## Search all C++ source files for PKG commands.
    lst = dir (fullfile (packdir, "src", "*.cc"));
    for i = 1:length (lst)
      nam = fullfile (packdir, "src", lst(i).name);
      fwrite (archfid, extract_pkg (nam, ['^//* *' nm ': *(.*)$']));
      fwrite (archfid, extract_pkg (nam, ['^/\** *' nm ': *(.*) *\*/$']));
    endfor

    ## Add developer included PKG commands.
    packdirnm = fullfile (packdir, nm);
    if (exist (packdirnm, "file"))
      fid = fopen (packdirnm, "rt");
      if (fid >= 0)
        while (! feof (fid))
          ln = fgets (fid);
          if (ln > 0)
            fwrite (archfid, ln);
          endif
        endwhile
        fclose (fid);
      endif
    endif

    ## If the files is empty remove it.
    fclose (instfid);
    t = dir (instpkg);
    if (t.bytes <= 0)
      unlink (instpkg);
    endif

    if (instfid != archfid)
      fclose (archfid);
      t = dir (archpkg);
      if (t.bytes <= 0)
        unlink (archpkg);
      endif
    endif
  endif
endfunction

