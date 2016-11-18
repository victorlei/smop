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
## @deftypefn {Function File} {} print_package_description (@var{pkg_name}, @var{pkg_ver}, @var{pkg_idx_struct}, @var{pkg_desc}, @var{status}, @var{verbose})
## Undocumented internal function.
## @end deftypefn

function print_package_description (pkg_name, pkg_ver, pkg_idx_struct,
                                    pkg_desc, status, verbose)

  printf ("---\nPackage name:\n\t%s\n", pkg_name);
  printf ("Version:\n\t%s\n", pkg_ver);
  printf ("Short description:\n\t%s\n", pkg_desc);
  printf ("Status:\n\t%s\n", status);
  if (verbose)
    printf ("---\nProvides:\n");
    for i = 1:length (pkg_idx_struct)
      if (! isempty (pkg_idx_struct{i}.functions))
        printf ("%s\n", pkg_idx_struct{i}.category);
        for j = 1:length (pkg_idx_struct{i}.functions)
          printf ("\t%s\n", pkg_idx_struct{i}.functions{j});
        endfor
      endif
    endfor
  endif

endfunction


