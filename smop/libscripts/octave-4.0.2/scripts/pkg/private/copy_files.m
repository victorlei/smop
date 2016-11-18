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
## @deftypefn {Function File} {} copy_files (@var{desc}, @var{packdir}, @var{global_install})
## Undocumented internal function.
## @end deftypefn

function copy_files (desc, packdir, global_install)
  ## Create the installation directory.
  if (! exist (desc.dir, "dir"))
    [status, output] = mkdir (desc.dir);
    if (status != 1)
      error ("couldn't create installation directory %s : %s",
      desc.dir, output);
    endif
  endif

  octfiledir = getarchdir (desc);

  ## Copy the files from "inst" to installdir.
  instdir = fullfile (packdir, "inst");
  if (! dirempty (instdir))
    [status, output] = copyfile (fullfile (instdir, "*"), desc.dir);
    if (status != 1)
      rmdir (desc.dir, "s");
      error ("couldn't copy files to the installation directory");
    endif
    if (exist (fullfile (desc.dir, getarch ()), "dir")
        && ! strcmp (fullfile (desc.dir, getarch ()), octfiledir))
      if (! exist (octfiledir, "dir"))
        ## Can be required to create upto three levels of dirs.
        octm1 = fileparts (octfiledir);
        if (! exist (octm1, "dir"))
          octm2 = fileparts (octm1);
          if (! exist (octm2, "dir"))
            octm3 = fileparts (octm2);
            if (! exist (octm3, "dir"))
              [status, output] = mkdir (octm3);
              if (status != 1)
                rmdir (desc.dir, "s");
                error ("couldn't create installation directory %s : %s",
                       octm3, output);
              endif
            endif
            [status, output] = mkdir (octm2);
            if (status != 1)
              rmdir (desc.dir, "s");
              error ("couldn't create installation directory %s : %s",
                     octm2, output);
            endif
          endif
          [status, output] = mkdir (octm1);
          if (status != 1)
            rmdir (desc.dir, "s");
            error ("couldn't create installation directory %s : %s",
                   octm1, output);
          endif
        endif
        [status, output] = mkdir (octfiledir);
        if (status != 1)
          rmdir (desc.dir, "s");
          error ("couldn't create installation directory %s : %s",
          octfiledir, output);
        endif
      endif
      [status, output] = movefile (fullfile (desc.dir, getarch (), "*"),
                                   octfiledir);
      rmdir (fullfile (desc.dir, getarch ()), "s");

      if (status != 1)
        rmdir (desc.dir, "s");
        rmdir (octfiledir, "s");
        error ("couldn't copy files to the installation directory");
      endif
    endif

  endif

  ## Create the "packinfo" directory.
  packinfo = fullfile (desc.dir, "packinfo");
  [status, msg] = mkdir (packinfo);
  if (status != 1)
    rmdir (desc.dir, "s");
    rmdir (octfiledir, "s");
    error ("couldn't create packinfo directory: %s", msg);
  endif

  packinfo_copy_file ("DESCRIPTION", "required", packdir, packinfo, desc, octfiledir);
  packinfo_copy_file ("COPYING", "required", packdir, packinfo, desc, octfiledir);
  packinfo_copy_file ("CITATION", "optional", packdir, packinfo, desc, octfiledir);
  packinfo_copy_file ("NEWS", "optional", packdir, packinfo, desc, octfiledir);
  packinfo_copy_file ("ONEWS", "optional", packdir, packinfo, desc, octfiledir);
  packinfo_copy_file ("ChangeLog", "optional", packdir, packinfo, desc, octfiledir);

  ## Is there an INDEX file to copy or should we generate one?
  index_file = fullfile (packdir, "INDEX");
  if (exist (index_file, "file"))
    packinfo_copy_file ("INDEX", "required", packdir, packinfo, desc, octfiledir);
  else
    try
      write_index (desc, fullfile (packdir, "inst"),
                   fullfile (packinfo, "INDEX"), global_install);
    catch
      rmdir (desc.dir, "s");
      rmdir (octfiledir, "s");
      rethrow (lasterror ());
    end_try_catch
  endif

  ## Is there an 'on_uninstall.m' to install?
  packinfo_copy_file ("on_uninstall.m", "optional", packdir, packinfo, desc, octfiledir);

  ## Is there a doc/ directory that needs to be installed?
  docdir = fullfile (packdir, "doc");
  if (exist (docdir, "dir") && ! dirempty (docdir))
    [status, output] = copyfile (docdir, desc.dir);
  endif

  ## Is there a bin/ directory that needs to be installed?
  ## FIXME: Need to treat architecture dependent files in bin/
  bindir = fullfile (packdir, "bin");
  if (exist (bindir, "dir") && ! dirempty (bindir))
    [status, output] = copyfile (bindir, desc.dir);
  endif
endfunction

