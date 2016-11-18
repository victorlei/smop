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
## @deftypefn {Function File} {} build (@var{files}, @var{handle_deps}, @var{autoload}, @var{verbose})
## Undocumented internal function.
## @end deftypefn

function build (files, handle_deps, autoload, verbose)
  if (length (files) < 1)
    error ("insufficient number of files");
  endif
  builddir = files{1};
  if (! exist (builddir, "dir"))
    warning ("creating build directory %s", builddir);
    [status, msg] = mkdir (builddir);
    if (status != 1)
      error ("could not create installation directory: %s", msg);
    endif
  endif
  [builddir, status] = canonicalize_file_name (builddir);
  if (status != 0)
    error ("cannot find directory %s", builddir);
  endif
  installdir = fullfile (builddir, "install");
  if (! exist (installdir, "dir"))
    [status, msg] = mkdir (installdir);
    if (status != 1)
      error ("could not create installation directory: %s", msg);
    endif
  endif
  files(1) = [];
  buildlist = fullfile (builddir, "octave_packages");
  install (files, handle_deps, autoload, installdir, installdir, verbose,
           buildlist, "", false);
  unwind_protect
    repackage (builddir, buildlist);
  unwind_protect_cleanup
    unload_packages ({"all"}, handle_deps, buildlist, "");
    if (exist (installdir, "dir"))
      rmdir (installdir, "s");
    endif
    if (exist (buildlist, "file"))
      unlink (buildlist);
    endif
  end_unwind_protect
endfunction

