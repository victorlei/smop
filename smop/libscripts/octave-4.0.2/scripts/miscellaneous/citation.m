## Copyright (C) 2013-2015 Carnë Draug
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
## @deftypefn  {Command} {} citation
## @deftypefnx {Command} {} citation @var{package}
## Display instructions for citing GNU Octave or its packages in publications.
##
## When called without an argument, display information on how to cite the core
## GNU Octave system.
##
## When given a package name @var{package}, display information on citing the
## specific named package.  Note that some packages may not yet have
## instructions on how to cite them.
##
## The GNU Octave developers and its active community of package authors have
## invested a lot of time and effort in creating GNU Octave as it is today.
## Please give credit where credit is due and cite GNU Octave and its packages
## when you use them.
##
## @end deftypefn

## Author: Carnë Draug <carandraug+dev@gmail.com>
## Idea and documentation from R's citation() (also under GPL)

function citation (package = "octave")

  if (nargin > 1)
    print_usage ();
  else
    display_info_file ("citation", package, "CITATION");
  endif

endfunction


## Test input validation
%!error citation (1, 2)
%!error <citation: PACKAGE must be a string> citation (1)
%!error <citation: package .* is not installed> citation ("__NOT_A_VALID_PKG_NAME__")

