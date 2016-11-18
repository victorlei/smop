## Copyright (C) 1993-2015 John W. Eaton
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
## @deftypefn {Function File} {} trace (@var{A})
## Compute the trace of @var{A}, the sum of the elements along the main
## diagonal.
##
## The implementation is straightforward: @code{sum (diag (@var{A}))}.
## @seealso{eig}
## @end deftypefn

## Author: jwe

function y = trace (A)

  if (nargin != 1)
    print_usage ();
  endif

  if (ndims (A) > 2)
    error ("trace: only valid on 2-D objects");
  elseif (isempty (A))
    y = 0;
  elseif (isvector (A))
    y = A(1);
  else
    y = sum (diag (A));
  endif

endfunction


%!assert (trace ([1, 2; 3, 4]), 5)
%!assert (trace ([1, 2; 3, 4; 5, 6]), 5)
%!assert (trace ([1, 3, 5; 2, 4, 6]), 5)
%!assert (trace ([]), 0)
%!assert (trace (rand (1,0)), 0)
%!assert (trace ([3:10]), 3)

%!error trace ()
%!error trace (1, 2)
%!error <only valid on 2-D objects> trace (reshape (1:9,[1,3,3]))

