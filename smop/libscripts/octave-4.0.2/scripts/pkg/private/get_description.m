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
## @deftypefn {Function File} {@var{desc} =} get_description (@var{filename})
## Undocumented internal function.
## @end deftypefn

## Parse the DESCRIPTION file.
function desc = get_description (filename)
  [fid, msg] = fopen (filename, "r");
  if (fid == -1)
    error ("the DESCRIPTION file %s could not be read: %s", filename, msg);
  endif

  desc = struct ();

  line = fgetl (fid);
  while (line != -1)
    if (line(1) == "#")
      ## Comments, do nothing.
    elseif (isspace (line(1)))
      ## Continuation lines
      if (exist ("keyword", "var") && isfield (desc, keyword))
        desc.(keyword) = [desc.(keyword) " " deblank(line)];
      endif
    else
      ## Keyword/value pair
      colon = find (line == ":");
      if (length (colon) == 0)
        disp ("skipping line");
      else
        colon = colon(1);
        keyword = tolower (strtrim (line(1:colon-1)));
        value = strtrim (line (colon+1:end));
        if (length (value) == 0)
            fclose (fid);
            error ("The keyword '%s' of the package '%s' has an empty value",
                    keyword, desc.name);
        endif
        desc.(keyword) = value;
      endif
    endif
    line = fgetl (fid);
  endwhile
  fclose (fid);

  ## Make sure all is okay.
  needed_fields = {"name", "version", "date", "title", ...
                   "author", "maintainer", "description"};
  for f = needed_fields
    if (! isfield (desc, f{1}))
      error ("description is missing needed field %s", f{1});
    endif
  endfor
  desc.version = fix_version (desc.version);
  if (isfield (desc, "depends"))
    desc.depends = fix_depends (desc.depends);
  else
    desc.depends = "";
  endif
  desc.name = tolower (desc.name);
endfunction

