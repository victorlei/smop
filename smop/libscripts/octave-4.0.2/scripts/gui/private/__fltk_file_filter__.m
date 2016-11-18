## Copyright (C) 2012-2015 Michael Goffioul
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
## @deftypefn {Function File} {@var{filterspec} =} __fltk_file_filter__ (@var{filter})
## Undocumented internal function.
## @end deftypefn

## Author: Michael Goffioul

function retval = __fltk_file_filter__ (file_filter)

  retval = "";
  [r, c] = size (file_filter);
  if ((c == 0) || (c > 2))
    error ("expecting 1 or to 2 columns for file filter cell");
  endif
  fltk_str = "";
  for idx = 1 : r

    curr_ext = file_filter{idx, 1};
    curr_ext = ostrsplit (curr_ext, ";");

    if (length (curr_ext) > 1)
      curr_ext = regexprep (curr_ext, '\*\.', ',');
      curr_ext = strcat (curr_ext{:})(2 : end);
      curr_ext = strcat ("*.{", curr_ext, "}");
    else
      curr_ext = curr_ext{:};
    endif

    curr_desc = strcat (curr_ext(3:end), "-Files");

    if (c == 2)
      curr_desc = file_filter{idx, 2};
      curr_desc = regexprep (curr_desc, '\(', '<');
      curr_desc = regexprep (curr_desc, '\)', '>');
    endif

    if (length (fltk_str) > 0)
      fltk_str = strcat (fltk_str, "\t", curr_desc, " (", curr_ext, ")");
    else
      fltk_str = strcat (curr_desc, " (", curr_ext, ")");
    endif

  endfor
  retval = fltk_str;

endfunction

