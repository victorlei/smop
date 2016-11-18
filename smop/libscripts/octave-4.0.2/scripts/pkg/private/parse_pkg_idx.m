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
## @deftypefn {Function File} {@var{pkg_idx_struct} =} parse_pkg_idx (@var{packdir})
## Undocumented internal function.
## @end deftypefn


## Read an INDEX file.
function pkg_idx_struct = parse_pkg_idx (packdir)

  index_file = fullfile (packdir, "packinfo", "INDEX");

  if (! exist (index_file, "file"))
    error ("could not find any INDEX file in directory %s, try 'pkg rebuild all' to generate missing INDEX files", packdir);
  endif


  [fid, msg] = fopen (index_file, "r");
  if (fid == -1)
    error ("the INDEX file %s could not be read: %s",
           index_file, msg);
  endif

  cat_num = 1;
  pkg_idx_struct{1}.category = "Uncategorized";
  pkg_idx_struct{1}.functions = {};

  line = fgetl (fid);
  while (isempty (strfind (line, ">>")) && ! feof (fid))
    line = fgetl (fid);
  endwhile

  while (! feof (fid) || line != -1)
    if (! any (! isspace (line)) || line(1) == "#" || any (line == "="))
      ## Comments,  blank lines or comments about unimplemented
      ## functions: do nothing
      ## FIXME: probably comments and pointers to external functions
      ## could be treated better when printing to screen?
    elseif (! isempty (strfind (line, ">>")))
      ## Skip package name and description as they are in DESCRIPTION
      ## already.
    elseif (! isspace (line(1)))
      ## Category.
      if (! isempty (pkg_idx_struct{cat_num}.functions))
        pkg_idx_struct{++cat_num}.functions = {};
      endif
      pkg_idx_struct{cat_num}.category = deblank (line);
    else
      ## Function names.
      while (any (! isspace (line)))
        [fun_name, line] = strtok (line);
        pkg_idx_struct{cat_num}.functions{end+1} = deblank (fun_name);
      endwhile
    endif
    line = fgetl (fid);
  endwhile
  fclose (fid);
endfunction

