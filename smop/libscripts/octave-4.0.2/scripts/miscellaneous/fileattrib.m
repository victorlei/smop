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
## @deftypefn {Function File} {[@var{status}, @var{result}, @var{msgid}] =} fileattrib (@var{file})
## Return information about @var{file}.
##
## If successful, @var{status} is 1, with @var{result} containing a structure
## with the following fields:
##
## @table @code
## @item Name
## Full name of @var{file}.
##
## @item archive
## True if @var{file} is an archive (Windows).
##
## @item system
## True if @var{file} is a system file (Windows).
##
## @item hidden
## True if @var{file} is a hidden file (Windows).
##
## @item directory
## True if @var{file} is a directory.
##
## @item  UserRead
## @itemx GroupRead
## @itemx OtherRead
## True if the user (group; other users) has read permission for @var{file}.
##
## @item  UserWrite
## @itemx GroupWrite
## @itemx OtherWrite
## True if the user (group; other users) has write permission for @var{file}.
##
## @item  UserExecute
## @itemx GroupExecute
## @itemx OtherExecute
## True if the user (group; other users) has execute permission for @var{file}.
## @end table
##
## If an attribute does not apply (i.e., archive on a Unix system) then the
## field is set to NaN.
##
## With no input arguments, return information about the current directory.
##
## If @var{file} contains globbing characters, return information about all
## the matching files.
## @seealso{glob}
## @end deftypefn

function [status, msg, msgid] = fileattrib (file)

  status = true;
  msg = "";
  msgid = "";

  if (nargin == 0)
    file = ".";
  endif

  if (ischar (file))
    files = glob (file);
    if (isempty (files))
      files = {file};
      nfiles = 1;
    else
      nfiles = length (files);
    endif
  else
    error ("fileattrib: expecting first argument to be a character string");
  endif

  if (nargin == 0 || nargin == 1)

    r_n = r_a = r_s = r_h = r_d ...
        = r_u_r = r_u_w = r_u_x ...
        = r_g_r = r_g_w = r_g_x ...
        = r_o_r = r_o_w = r_o_x = cell (nfiles, 1);

    curr_dir = pwd ();

    for i = 1:nfiles
      [info, err, msg] = stat (files{i});
      if (! err)
        r_n{i} = canonicalize_file_name (files{i});
        r_a{i} = NaN;
        r_s{i} = NaN;
        r_h{i} = NaN;
        r_d{i} = S_ISDIR (info.mode);
        ## FIXME: Maybe we should have S_IRUSR etc. masks?
        modestr = info.modestr;
        r_u_r{i} = modestr(2) == "r";
        r_u_w{i} = modestr(3) == "w";
        r_u_x{i} = modestr(4) == "x";
        r_g_r{i} = modestr(5) == "r";
        r_g_w{i} = modestr(6) == "w";
        r_g_x{i} = modestr(7) == "x";
        r_o_r{i} = modestr(8) == "r";
        r_o_w{i} = modestr(9) == "w";
        r_o_x{i} = modestr(10) == "x";
      else
        status = false;
        msgid = "fileattrib";
        break;
      endif
    endfor
    if (status)
      r = struct ("Name", r_n, "archive", r_a, "system", r_s,
                  "hidden", r_s, "directory", r_d, "UserRead", r_u_r,
                  "UserWrite", r_u_w, "UserExecute", r_u_x,
                  "GroupRead", r_g_r, "GroupWrite", r_g_w,
                  "GroupExecute", r_g_x, "OtherRead", r_o_r,
                  "OtherWrite", r_o_w, "OtherExecute", r_o_x);
      if (nargout == 0)
        status = r;
      else
        msg = r;
      endif
    endif
  else
    print_usage ();
  endif

endfunction

