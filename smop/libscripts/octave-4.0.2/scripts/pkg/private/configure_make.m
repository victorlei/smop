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
## @deftypefn {Function File} {} configure_make (@var{desc}, @var{packdir}, @var{verbose})
## Undocumented internal function.
## @end deftypefn

function configure_make (desc, packdir, verbose)
  ## Perform ./configure, make, make install in "src".
  if (exist (fullfile (packdir, "src"), "dir"))
    src = fullfile (packdir, "src");
    octave_bindir = octave_config_info ("bindir");
    ver = version ();
    ext = octave_config_info ("EXEEXT");
    mkoctfile_program = fullfile (octave_bindir, ...
                                  sprintf ("mkoctfile-%s%s", ver, ext));
    octave_config_program = fullfile (octave_bindir, ...
                                      sprintf ("octave-config-%s%s", ver, ext));
    octave_binary = fullfile (octave_bindir, sprintf ("octave-%s%s", ver, ext));

    if (! exist (mkoctfile_program, "file"))
      __gripe_missing_component__ ("pkg", "mkoctfile");
    endif
    if (! exist (octave_config_program, "file"))
      __gripe_missing_component__ ("pkg", "octave-config");
    endif
    if (! exist (octave_binary, "file"))
      __gripe_missing_component__ ("pkg", "octave");
    endif

    if (verbose)
      mkoctfile_program = [mkoctfile_program " --verbose"];
    endif

    cenv = {"MKOCTFILE"; mkoctfile_program;
            "OCTAVE_CONFIG"; octave_config_program;
            "OCTAVE"; octave_binary;
            "INSTALLDIR"; desc.dir};
    scenv = sprintf ("%s='%s' ", cenv{:});

    ## Configure.
    if (exist (fullfile (src, "configure"), "file"))
      flags = "";
      if (isempty (getenv ("CC")))
        flags = [flags ' CC="' mkoctfile("-p", "CC") '"'];
      endif
      if (isempty (getenv ("CXX")))
        flags = [flags ' CXX="' mkoctfile("-p", "CXX") '"'];
      endif
      if (isempty (getenv ("AR")))
        flags = [flags ' AR="' mkoctfile("-p", "AR") '"'];
      endif
      if (isempty (getenv ("RANLIB")))
        flags = [flags ' RANLIB="' mkoctfile("-p", "RANLIB") '"'];
      endif
      cmd = ["cd '" src "'; " ...
             scenv "./configure --prefix=\"" desc.dir "\"" flags];
      [status, output] = shell (cmd, verbose);
      if (status != 0)
        rmdir (desc.dir, "s");
        disp (output);
        error ("pkg: error running the configure script for %s.", desc.name);
      endif
    endif

    ## Make.
    if (ispc ())
      jobs = 1;
    else
      jobs = nproc ("overridable");
    endif

    if (exist (fullfile (src, "Makefile"), "file"))
      [status, output] = shell (sprintf ("%s make --jobs %i --directory '%s'",
                                         scenv, jobs, src), verbose);
      if (status != 0)
        rmdir (desc.dir, "s");
        disp (output);
        error ("pkg: error running `make' for the %s package.", desc.name);
      endif
    endif

    ## Copy files to "inst" and "inst/arch" (this is instead of 'make
    ## install').
    files = fullfile (src, "FILES");
    instdir = fullfile (packdir, "inst");
    archdir = fullfile (packdir, "inst", getarch ());

    ## Get file names.
    if (exist (files, "file"))
      [fid, msg] = fopen (files, "r");
      if (fid < 0)
        error ("couldn't open %s: %s", files, msg);
      endif
      filenames = char (fread (fid))';
      fclose (fid);
      if (filenames(end) == "\n")
        filenames(end) = [];
      endif
      filenames = strtrim (ostrsplit (filenames, "\n"));
      delete_idx = [];
      for i = 1:length (filenames)
        if (! all (isspace (filenames{i})))
          filenames{i} = fullfile (src, filenames{i});
        else
          delete_idx(end+1) = i;
        endif
      endfor
      filenames(delete_idx) = [];
    else
      m = dir (fullfile (src, "*.m"));
      oct = dir (fullfile (src, "*.oct"));
      mex = dir (fullfile (src, "*.mex"));

      filenames = cellfun (@(x) fullfile (src, x),
                           {m.name, oct.name, mex.name},
                           "uniformoutput", false);
    endif

    ## Split into architecture dependent and independent files.
    if (isempty (filenames))
      idx = [];
    else
      idx = cellfun ("is_architecture_dependent", filenames);
    endif
    archdependent = filenames(idx);
    archindependent = filenames(!idx);

    ## Copy the files.
    if (! all (isspace ([filenames{:}])))
        if (! exist (instdir, "dir"))
          mkdir (instdir);
        endif
        if (! all (isspace ([archindependent{:}])))
          if (verbose)
            printf ("copyfile");
            printf (" %s", archindependent{:});
            printf ("%s\n", instdir);
          endif
          [status, output] = copyfile (archindependent, instdir);
          if (status != 1)
            rmdir (desc.dir, "s");
            error ("Couldn't copy files from 'src' to 'inst': %s", output);
          endif
        endif
        if (! all (isspace ([archdependent{:}])))
          if (verbose)
            printf ("copyfile");
            printf (" %s", archdependent{:});
            printf (" %s\n", archdir);
          endif
          if (! exist (archdir, "dir"))
            mkdir (archdir);
          endif
          [status, output] = copyfile (archdependent, archdir);
          if (status != 1)
            rmdir (desc.dir, "s");
            error ("Couldn't copy files from 'src' to 'inst': %s", output);
          endif
        endif
    endif
  endif
endfunction

