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
## @deftypefn {Function File} {[@var{fname}, @var{fpath}, @var{fltidx}] =} __uiputfile_fltk__ ()
## Undocumented internal function.
## @end deftypefn

## Author: Michael Goffioul

function [retval, retpath, retindex] = __uiputfile_fltk__ (filters, title, defval, position, tag, defdir)

  if (exist ("__fltk_uigetfile__") != 3)
    error ("uiputfile: fltk graphics toolkit required");
  endif

  filters = __fltk_file_filter__ (filters);
  if (length (defdir) > 0)
    defval = fullfile (defdir, defval);
  endif
  [retval, retpath, retindex] = ...
    __fltk_uigetfile__ (filters, title, defval, position, tag);

endfunction

