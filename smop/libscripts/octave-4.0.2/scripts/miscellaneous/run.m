## Copyright (C) 2007-2015 David Bateman
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
## @deftypefn  {Command} {} run @var{script}
## @deftypefnx {Function File} {} run ("@var{script}")
## Run @var{script} in the current workspace.
##
## Scripts which reside in directories specified in Octave's load path, and
## which end with the extension @file{".m"}, can be run simply by typing
## their name.  For scripts not located on the load path, use @code{run}.
##
## The file name @var{script} can be a bare, fully qualified, or relative
## filename and with or without a file extension.  If no extension is specified,
## Octave will first search for a script with the @file{".m"} extension before
## falling back to the script name without an extension.
##
## Implementation Note: If @var{script} includes a path component, then
## @code{run} first changes the working directory to the directory where
## @var{script} is found.  Next, the script is executed.  Finally, @code{run}
## returns to the original working directory unless @code{script} has
## specifically changed directories.
## @seealso{path, addpath, source}
## @end deftypefn

function run (script)

  if (nargin != 1)
    print_usage ();
  endif

  [d, f, ext] = fileparts (script);
  if (! strcmp (ext, ".m"))
    ## prefer files with .m extension for compatibility with Matlab
    if (exist ([script ".m"], "file"))
      f = [f ext];
      ext = ".m";
      script = [script ".m"];
    endif
  endif

  if (! exist (script, "file"))
    error ("run: file SCRIPT must exist and be a valid Octave scriptfile");
  endif

  if (! isempty (d))
    if (exist (d, "dir"))
      startdir = pwd ();
      d = make_absolute_filename (d);
      unwind_protect
        cd (d);
        evalin ("caller", sprintf ("source ('%s%s');", f, ext),
                "rethrow (lasterror ())");
      unwind_protect_cleanup
        if (strcmp (d, pwd ()))
          cd (startdir);
        endif
      end_unwind_protect
    else
      error ("run: the path %s doesn't exist", d);
    endif
  else
    if (! isempty (ext))
      script = which (script);
    else
      ## Search PATH with null extension ('.' will be stripped and ext = "")
      script = which ([script "."]);
    endif
    evalin ("caller", sprintf ("source ('%s');", script),
            "rethrow (lasterror ())");
  endif
endfunction


## Test input validation
%!error run ()
%!error run ("a", "b")
%!error <SCRIPT must exist> run ("__A_very_#unlikely#_file_name__")

