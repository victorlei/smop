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
## @deftypefn {Function File} {} generate_lookfor_cache (@var{desc})
## Undocumented internal function.
## @end deftypefn

function generate_lookfor_cache (desc)
  dirs = strtrim (ostrsplit (genpath (desc.dir), pathsep ()));
  for i = 1 : length (dirs)
    doc_cache_create (fullfile (dirs{i}, "doc-cache"), dirs{i});
  endfor
endfunction

