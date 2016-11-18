## Copyright (C) 2013-2015 Rik Wehbring
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
## @deftypefn {Function File} {} atan2d (@var{y}, @var{x})
## Compute atan2 (@var{y} / @var{x}) in degrees for corresponding elements
## from @var{y} and @var{x}.
## @seealso{tand, atan2}
## @end deftypefn

function retval = atan2d (y, x)

  if (nargin != 2)
    print_usage ();
  endif

  retval = 180 ./ pi .* atan2 (y, x);

endfunction


%!assert (atan2d (-1:.1:1, 1:-.1:-1), 180/pi * atan2 (-1:.1:1, 1:-.1:-1), -10*eps)

%!error atan2d ()
%!error atan2d (1)

