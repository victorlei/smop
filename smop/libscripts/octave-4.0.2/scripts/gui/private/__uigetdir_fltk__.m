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
## @deftypefn {Function File} {@var{dirname} =} __uigetdir_fltk__ (@var{start_path}, @var{dialog_title})
## Undocumented internal function.
## @end deftypefn

## Author: Michael Goffioul

function dirname = __uigetdir_fltk__ (start_path, dialog_title)

  if (exist ("__fltk_uigetfile__") != 3)
    error ("uigetdir: fltk graphics toolkit required");
  endif

  dirname = __fltk_uigetfile__ ("", dialog_title,
                                start_path, [240, 120], "dir");

endfunction

