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
## @deftypefn {Function File} {@var{list} =} list_forge_packages ()
## Undocumented internal function.
## @end deftypefn

function list = list_forge_packages ()
  [list, succ] = urlread ("http://packages.octave.org/list_packages.php");
  if (succ)
    list = ostrsplit (list, " \n\t", true);
  else
    error ("pkg: could not read URL, please verify internet connection");
  endif
  if (nargout == 0)
    page_screen_output (false, "local");
    puts ("OctaveForge provides these packages:\n");
    for i = 1:length (list)
      try
        ver = get_forge_pkg (list{i});
      catch
        ver = "unknown";
      end_try_catch
      printf ("  %s %s\n", list{i}, ver);
    endfor
  endif
endfunction

