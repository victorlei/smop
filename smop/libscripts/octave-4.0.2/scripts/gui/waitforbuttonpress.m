## Copyright (C) 2004-2015 Petr Mikulik
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
## @deftypefn  {Function File} {} waitforbuttonpress ()
## @deftypefnx {Function File} {@var{b} =} waitforbuttonpress ()
## Wait for mouse click or key press over the current figure window.
##
## The return value of @var{b} is 0 if a mouse button was pressed or 1 if a
## key was pressed.
## @seealso{waitfor, ginput, kbhit}
## @end deftypefn

## The original version of this code bore the copyright
## Author: Petr Mikulik
## License: public domain

function b = waitforbuttonpress ()

  if (nargin != 0 || nargout > 1)
    print_usage ();
  endif

  [x, y, k] = ginput (1);

  if (nargout == 1)
    if (k <= 5)
      b = 0;
    else
      b = 1;
    endif
  endif

endfunction


## Test input validation
%!error waitforbuttonpress (1)
%!error [a,b,c] = waitforbuttonpress ()

