## Copyright (C) 2005-2015 Bill Denney
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
## @deftypefn  {Function File} {} savepath ()
## @deftypefnx {Function File} {} savepath (@var{file})
## @deftypefnx {Function File} {@var{status} =} savepath (@dots{})
## Save the unique portion of the current function search path that is
## not set during Octave's initialization process to @var{file}.
##
## If @var{file} is omitted, Octave looks in the current directory for a
## project-specific @file{.octaverc} file in which to save the path
## information.  If no such file is present then the user's configuration file
## @file{~/.octaverc} is used.
##
## If successful, @code{savepath} returns 0.
##
## The @code{savepath} function makes it simple to customize a user's
## configuration file to restore the working paths necessary for a particular
## instance of Octave.  Assuming no filename is specified, Octave will
## automatically restore the saved directory paths from the appropriate
## @file{.octaverc} file when starting up.  If a filename has been specified
## then the paths may be restored manually by calling @code{source @var{file}}.
## @seealso{path, addpath, rmpath, genpath, pathdef}
## @end deftypefn

## Author: Bill Denney <bill@givebillmoney.com>

function retval = savepath (file)

  beginstring = "## Begin savepath auto-created section, do not edit";
  endstring   = "## End savepath auto-created section";

  ## Use project-specific or user's .octaverc when no file specified
  if (nargin == 0)
    file = fullfile (pwd, ".octaverc");
    if (! exist (file, "file"))
      file = fullfile ("~", ".octaverc");
    endif
  endif

  ## Read in the file
  [filelines, startline, endline] = getsavepath (file);

  ## Determine where the savepath lines are placed in the file.
  if (isempty (filelines)
      || (startline == 1 && endline == length (filelines)))
    ## savepath is the entire file.
    pre = post = {};
  elseif (endline == 0)
    ## Drop the savepath statements at the end of the file.
    pre = filelines;
    post = {};
  elseif (startline == 1)
    pre = {};
    post = filelines(endline+1:end);
  elseif (endline == length (filelines))
    pre = filelines(1:startline-1);
    post = {};
  else
    ## Insert in the middle.
    pre = filelines(1:startline-1);
    post = filelines(endline+1:end);
  endif

  ## Write the results.
  [fid, msg] = fopen (file, "wt");
  if (fid < 0)
    error ("savepath: unable to open file for writing, %s, %s", file, msg);
  endif
  unwind_protect
    fprintf (fid, "%s\n", pre{:});

    ## Remove the portion of the path defined via the command line
    ## and/or the environment.
    workingpath = parsepath (path);
    cmd_line_path = parsepath (command_line_path ());
    octave_path = parsepath (getenv ("OCTAVE_PATH"));
    default_path = pathdef ();
    if (isempty (default_path))
      ## This occurs when running octave via run-octave.  In this instance
      ## the entire path is specified via the command line and pathdef()
      ## is empty.
      [~, n] = setdiff (workingpath, octave_path);
      default_path = cmd_line_path;
    else
      [~, n] = setdiff (workingpath, union (cmd_line_path, octave_path));
      default_path = parsepath (default_path);
    endif
    ## This is the path we'd like to preserve when octave is run.
    path_to_preserve = workingpath(sort (n));

    ## Determine the path to Octave's user and system wide packages.
    [pkg_user, pkg_system] = pkg ("list");

    ## Conversion from cell array of structs to cellstr of archprefixes.
    pkg_path = unique (cellfun (@(elt) elt.archprefix,
                                [pkg_user, pkg_system],
                                "uniformoutput", false));

    ## Rely on Octave's initialization to include the pkg path elements.
    if (! isempty (pkg_path))
      [~, n] = setdiff (path_to_preserve, strcat (pkg_path, ":"));
      path_to_preserve = path_to_preserve(sort (n));
    endif

    ## Split the path to be saved into two groups.  Those path elements that
    ## belong at the beginning and those at the end.
    if (! isempty (default_path))
      n1 = find (strcmp (default_path{1}, path_to_preserve));
      n2 = find (strcmp (default_path{end}, path_to_preserve));
      n_middle = round ((n1+n2)/2);
      [~, n] = setdiff (path_to_preserve, default_path);
      path_to_save = path_to_preserve(sort (n));
      ## Remove pwd
      path_to_save(strcmp (path_to_save, ["." pathsep])) = [];
      if (! isempty (path_to_save))
        n = ones (numel (path_to_save), 1);
        for m = 1:numel (path_to_save)
          n(m) = find (strcmp (path_to_save{m}, path_to_preserve));
        endfor
        path_to_save_begin = path_to_save(n <= n_middle);
        path_to_save_end   = path_to_save(n > n_middle);
      else
        path_to_save_begin = {};
        path_to_save_end   = {};
      endif
    else
      path_to_save_begin = path_to_preserve;
      path_to_save_end   = {};
    endif
    path_to_save_begin = cell2mat (path_to_save_begin);
    path_to_save_end   = cell2mat (path_to_save_end);

    ## Use single quotes for PATH argument to avoid string escape
    ## processing.  Since we are using single quotes around the arg,
    ## double any single quote characters found in the string.
    fprintf (fid, "%s\n", beginstring);
    if (! isempty (path_to_save_begin))
      n = find (path_to_save_begin != pathsep, 1, "last");
      fprintf (fid, "  addpath ('%s', '-begin');\n",
               strrep (path_to_save_begin(1:n), "'", "''"));
    endif
    if (! isempty (path_to_save_end))
      n = find (path_to_save_end != pathsep, 1, "last");
      fprintf (fid, "  addpath ('%s', '-end');\n",
               strrep (path_to_save_end(1:n), "'", "''"));
    endif
    fprintf (fid, "%s\n", endstring);

    fprintf (fid, "%s\n", post{:});
  unwind_protect_cleanup
    status = fclose (fid);
    if (status < 0)
      error ("savepath: could not close savefile after writing, %s", file);
    elseif (nargin == 0)
      warning ("off", "backtrace", "local");
      warning ("Octave:savepath-local",
               "savepath: current path saved to %s", file);
    endif
  end_unwind_protect

  if (nargout > 0)
    retval = 0;
  endif

endfunction

## Convert single string of paths to cell array of paths
function path_elements = parsepath (p)
  path_elements = strcat (ostrsplit (p, pathsep), pathsep);
endfunction


%!test
%! fname = tempname ();
%! status = savepath (fname);
%! assert (status == 0);
%! old_dir = pwd;
%! unwind_protect
%!   cd (P_tmpdir);
%!   if (exist (fullfile (pwd, ".octaverc")))
%!     unlink (".octaverc");
%!   endif
%!   ## Create blank .octaverc file
%!   fid = fopen (".octaverc", "wt");
%!   assert (fid >= 0);
%!   fclose (fid);
%!   ## Save path into local .octaverc file
%!   warning ("off", "Octave:savepath-local");
%!   status = savepath ();
%!   assert (status == 0);
%!   ## Compare old and new versions
%!   fid = fopen (fname, "rb");
%!   assert (fid >= 0);
%!   orig_data = fread (fid);
%!   fclose (fid);
%!   fid = fopen (".octaverc", "rb");
%!   assert (fid >= 0);
%!   new_data = fread (fid);
%!   fclose (fid);
%!   assert (orig_data, new_data);
%! unwind_protect_cleanup
%!   cd (old_dir);
%! end_unwind_protect

