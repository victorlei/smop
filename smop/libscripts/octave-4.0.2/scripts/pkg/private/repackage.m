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
## @deftypefn {Function File} {} repackage (@var{builddir}, @var{buildlist})
## Undocumented internal function.
## @end deftypefn

function repackage (builddir, buildlist)
  packages = installed_packages (buildlist, buildlist);

  wd = pwd ();
  for i = 1 : length (packages)
    pack = packages{i};
    unwind_protect
      cd (builddir);
      mkdir (pack.name);
      mkdir (fullfile (pack.name, "inst"));
      copyfile (fullfile (pack.dir, "*"), fullfile (pack.name, "inst"));
      movefile (fullfile (pack.name, "inst","packinfo", "*"), pack.name);
      if (exist (fullfile (pack.name, "inst","packinfo", ".autoload"), "file"))
        unlink (fullfile (pack.name, "inst","packinfo", ".autoload"));
      endif
      rmdir (fullfile (pack.name, "inst", "packinfo"));
      if (exist (fullfile (pack.name, "inst", "doc"), "dir"))
        movefile (fullfile (pack.name, "inst", "doc"), pack.name);
      endif
      if (exist (fullfile (pack.name, "inst", "bin"), "dir"))
        movefile (fullfile (pack.name, "inst", "bin"), pack.name);
      endif
      archdir = fullfile (pack.archprefix, [pack.name "-" pack.version],
                          getarch ());
      if (exist (archdir, "dir"))
        if (exist (fullfile (pack.name, "inst", "PKG_ADD"), "file"))
          unlink (fullfile (pack.name, "inst", "PKG_ADD"));
        endif
        if (exist (fullfile (pack.name, "inst", "PKG_DEL"), "file"))
          unlink (fullfile (pack.name, "inst", "PKG_DEL"));
        endif
        if (exist (fullfile (archdir, "PKG_ADD"), "file"))
          movefile (fullfile (archdir, "PKG_ADD"),
                    fullfile (pack.name, "PKG_ADD"));
        endif
        if (exist (fullfile (archdir, "PKG_DEL"), "file"))
          movefile (fullfile (archdir, "PKG_DEL"),
                    fullfile (pack.name, "PKG_DEL"));
        endif
      else
        if (exist (fullfile (pack.name, "inst", "PKG_ADD"), "file"))
          movefile (fullfile (pack.name, "inst", "PKG_ADD"),
                    fullfile (pack.name, "PKG_ADD"));
        endif
        if (exist (fullfile (pack.name, "inst", "PKG_DEL"), "file"))
          movefile (fullfile (pack.name, "inst", "PKG_DEL"),
                    fullfile (pack.name, "PKG_DEL"));
        endif
      endif
      tfile = [pack.name "-" pack.version ".tar"];
      tar (tfile, pack.name);
      try
        gzip (tfile);
        unlink (tfile);
      catch
        warning ("failed to compress %s", tfile);
      end_try_catch
    unwind_protect_cleanup
      if (exist (pack.name, "dir"))
        rmdir (pack.name, "s");
      endif
      cd (wd);
    end_unwind_protect
  endfor
endfunction

