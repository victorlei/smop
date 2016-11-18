## Copyright (C) 2005-2015 John W. Eaton
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
## @deftypefn  {Function File} {} movefile (@var{f1})
## @deftypefnx {Function File} {} movefile (@var{f1}, @var{f2})
## @deftypefnx {Function File} {} movefile (@var{f1}, @var{f2}, 'f')
## @deftypefnx {Function File} {[@var{status}, @var{msg}, @var{msgid}] =} movefile (@dots{})
## Move the source files or directories @var{f1} to the destination @var{f2}.
##
## The name @var{f1} may contain globbing patterns.  If @var{f1} expands to
## multiple file names, @var{f2} must be a directory.  If no destination
## @var{f2} is specified then the destination is the present working directory.
## If @var{f2} is a file name then @var{f1} is renamed to @var{f2}.
##
## When the force flag @qcode{'f'} is given any existing files will be
## overwritten without prompting.
##
## If successful, @var{status} is 1, and @var{msg}, @var{msgid} are empty
## character strings ("").  Otherwise, @var{status} is 0, @var{msg} contains a
## system-dependent error message, and @var{msgid} contains a unique message
## identifier.  Note that the status code is exactly opposite that of the
## @code{system} command.
## @seealso{rename, copyfile, unlink, delete, glob}
## @end deftypefn

function [status, msg, msgid] = movefile (f1, f2, force)

  if (nargin < 1 || nargin > 3)
    print_usage ();
  endif

  max_cmd_line = 1024;
  status = true;
  msg = "";
  msgid = "";

  ## FIXME: maybe use the same method as in ls to allow users control
  ##        over the command that is executed.

  if (ispc () && ! isunix ()
      && isempty (file_in_path (getenv ("PATH"), "mv.exe")))
    ## Windows.
    cmd = "cmd /C move";
    cmd_force_flag = "/Y";
  else
    cmd = "mv";
    cmd_force_flag = "-f";
  endif

  ## Input type check.
  if (ischar (f1))
    f1 = cellstr (f1);
  elseif (! iscellstr (f1))
    error ("copyfile: F1 must be a string or a cell array of strings");
  endif

  if (nargin == 1)
    f2 = pwd ();
  elseif (! ischar (f2))
    error ("movefile: F2 must be a string");
  endif

  if (nargin == 3 && strcmp (force, "f"))
    cmd = [cmd " " cmd_force_flag];
  endif

  ## If f1 has more than 1 element f2 must be a directory
  isdir = (exist (f2, "dir") != 0);
  if (numel (f1) > 1 && ! isdir)
    error ("movefile: when moving multiple files, F2 must be a directory");
  endif

  ## Protect the file name(s).
  f1 = glob (f1);
  if (isempty (f1))
    error ("movefile: no files to move");
  endif
  p1 = sprintf ('"%s" ', f1{:});
  p2 = tilde_expand (f2);

  if (isdir && length (p1) > max_cmd_line)
    l2 = length (p2) + length (cmd) + 6;
    while (! isempty (f1))
      p1 = sprintf ('"%s" ', f1{1});
      f1(1) = [];
      while (! isempty (f1)
             && (length (p1) + length (f1{1}) + l2 < max_cmd_line))
        p1 = sprintf ('%s"%s" ', p1, f1{1});
        f1(1) = [];
      endwhile

      if (ispc () && ! isunix ()
          && ! isempty (file_in_path (getenv ("PATH"), "cp.exe")))
        p1 = strrep (p1, '\', '/');
        p2 = strrep (p2, '\', '/');
      endif

      ## Move the file(s).
      [err, msg] = system (sprintf ('%s %s "%s"', cmd, p1, p2));
      if (err != 0)
        status = false;
        msgid = "movefile";
      endif
    endwhile
  else
    if (ispc () && ! isunix ()
        && ! isempty (file_in_path (getenv ("PATH"), "cp.exe")))
      p1 = strrep (p1, '\', '/');
      p2 = strrep (p2, '\', '/');
    endif

    ## Move the file(s).
    [err, msg] = system (sprintf ('%s %s "%s"', cmd, p1, p2));
    if (err != 0)
      status = false;
      msgid = "movefile";
    endif
  endif

endfunction


%!test
%! unwind_protect
%!   f1 = tempname;
%!   tmp_var = pi;
%!   save (f1, "tmp_var");
%!   fid = fopen (f1, "rb");
%!   assert (fid >= 0);
%!   orig_data = fread (fid);
%!   fclose (fid);
%!   f2 = tempname;
%!   assert (movefile (f1, f2));
%!   assert (! exist (f1, "file"));
%!   assert (exist (f2, "file"));
%!   fid = fopen (f2, "rb");
%!   assert (fid >= 0);
%!   new_data = fread (fid);
%!   fclose (fid);
%!   if (orig_data != new_data)
%!     error ("moved file not equal to original file!");
%!   endif
%! unwind_protect_cleanup
%!   delete (f2);
%! end_unwind_protect

## Test input validation
%!error movefile ()
%!error movefile (1,2,3,4)
%!error <F1 must be a string> movefile (1, "foobar")
%!error <F2 must be a string> movefile ("foobar", 1)
%!error <F2 must be a directory> movefile ({"a", "b"}, "%_NOT_A_DIR_%")

