## Copyright (C) 2014-2015 Rik Wehbring
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

function [filelines, startline, endline] = getsavepath (file)

  beginstring = "## Begin savepath auto-created section, do not edit";
  endstring   = "## End savepath auto-created section";

  ## Read in the file while checking for errors along the way.
  startline = endline = 0;
  filelines = {};
  if (exist (file) == 2)
    [fid, msg] = fopen (file, "rt");
    if (fid < 0)
      error ("getsavepath: could not open file, %s: %s", file, msg);
    endif
    linenum = 0;
    while (ischar (line = fgetl (fid)))
      filelines{++linenum} = line;
      ## Find the first and last lines if they exist in the file.
      if (strcmp (line, beginstring))
        startline = linenum;
      elseif (strcmp (line, endstring))
        endline = linenum;
      endif
    endwhile
    if (fclose (fid) < 0)
      error ("getsavepath: could not close file after reading, %s", file);
    endif
  endif

  ## Verify the file was correctly formatted.
  if (startline > endline || (startline > 0 && endline == 0))
    error ("getsavepath: unable to parse file, %s", file);
  endif

endfunction

