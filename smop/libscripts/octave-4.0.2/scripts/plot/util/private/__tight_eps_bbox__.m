## Copyright (C) 2010-2015 Ben Abbott
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
## @deftypefn {Function File} {@var{bbox} =} __tight_eps_bbox__ (@var{@dots{}})
## Undocumented internal function.
## @end deftypefn

## Author: Ben Abbott <bpabbott@mac.com>
## Created: 2010-07-26

function bb = __tight_eps_bbox__ (opts, eps_file_name)

  box_string = "%%BoundingBox:";

  cmd = sprintf ("\"%s\" \"%s\" 2>&1", "head", eps_file_name);
  [status, output] = system (cmd);

  if (status == 0)
    orig_bbox_line = get_bbox (output);
  else
    error ("print:noboundingbox",
           "print.m: no bounding box found in '%s'", eps_file_name);
  endif

  ghostscript_options = "-q -dBATCH -dSAFER -dNOPAUSE -dTextAlphaBits=4 -sDEVICE=bbox";
  cmd = sprintf ("\"%s\" %s \"%s\" 2>&1", opts.ghostscript.binary,
                 ghostscript_options, eps_file_name);
  [status, output] = system (cmd);

  if (status == 0)
    tight_bbox_line = get_bbox (output);
  else
    warning ("print:nogsboundingbox",
             "print.m: ghostscript failed to determine the bounding for '%s'",
             eps_file_name);
  endif

  ## Attempt to fix the bbox in place.
  fid = fopen (eps_file_name, "r+");
  unwind_protect
    bbox_replaced = false;
    looking_for_bbox = true;
    while (looking_for_bbox)
      current_line = fgetl (fid);
      if (strncmpi (current_line, box_string, length (box_string)))
        line_length = numel (current_line);
        num_spaces = line_length - numel (tight_bbox_line);
        if (numel (current_line) >= numel (tight_bbox_line))
          new_line = tight_bbox_line;
          new_line(end+1:numel (current_line)) = " ";
          bbox_replaced = true;
          ## Back up to the beginning of the line (include EOL characters).
          if (ispc ())
            fseek (fid, -line_length-2, "cof");
          else
            fseek (fid, -line_length-1, "cof");
          endif
          count = fprintf (fid, "%s", new_line);
        endif
        looking_for_bbox = false;
      elseif (! ischar (current_line))
        looking_for_bbox = false;
      endif
    endwhile
  unwind_protect_cleanup
    fclose (fid);
  end_unwind_protect

  ## If necessary load the eps-file and replace the bbox (can be slow).
  if (! bbox_replaced)
    fid = fopen (eps_file_name, "r");
    unwind_protect
      data = char (fread (fid, Inf)).';
    unwind_protect_cleanup
      fclose (fid);
    end_unwind_protect
    n = strfind (data, box_string);
    if (numel (n) > 1)
      ## Only replace one instance.
      n = n(1);
    elseif (isempty (n))
      error ("print:noboundingbox", ...
             "print.m: no bounding box found in '%s'.", eps_file_name);
    endif
    m = numel (orig_bbox_line);
    data = horzcat (data(1:(n-1)), tight_bbox_line, data((n+m):end));
    fid = fopen (eps_file_name, "w");
    unwind_protect
      fprintf (fid, "%s", data);
    unwind_protect_cleanup
      fclose (fid);
    end_unwind_protect
  endif

endfunction

function bbox_line = get_bbox (lines)
  box_string = "%%BoundingBox:";
  pattern = strcat (box_string, "[^%]*");
  pattern = pattern(1:find (double (pattern) > 32, 1, "last"));
  bbox_line = regexp (lines, pattern, "match");
  if (iscell (bbox_line))
    bbox_line = bbox_line{1};
  endif
  ## Remove the EOL characters.
  bbox_line(double (bbox_line) < 32) = "";
endfunction

