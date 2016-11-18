## Copyright (C) 2014-2015 Massimiliano Fasi
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
## @deftypefn {Function File} {} isbanded (@var{A}, @var{lower}, @var{upper})
## Return true if @var{A} is a matrix with entries confined between
## @var{lower} diagonals below the main diagonal and @var{upper} diagonals
## above the main diagonal.
##
## @var{lower} and @var{upper} must be non-negative integers.
## @seealso{isdiag, istril, istriu, bandwidth}
## @end deftypefn

## Author: Massimiliano Fasi

function retval = isbanded (A, lower, upper)

  if (nargin != 3)
    print_usage ();
  endif

  if (! isreal (lower) || ! isreal (upper) || lower < 0 || upper < 0)
    error ("isbanded: LOWER and UPPER must be non-negative integers");
  endif

  if (isempty (A))
    retval = [];
  else
    retval = (isnumeric (A) || islogical (A)) && ndims (A) == 2;
    if (retval)
      [i, j] = find (A);
      pupp = j >= i;
      retval = all (j(pupp) - i(pupp) <= upper);
      if (retval)
        plow = i >= j;
        retval = all (i(plow) - j(plow) <= lower);
      endif
    endif
  endif

endfunction


%!assert (! isbanded ("string", 0, 0))
%!assert (! isbanded (zeros (2,2,2), 0, 0))
%!assert (isbanded ([], 0, 0), [])
%!assert (isbanded (1,0,0))
%!assert (isbanded (1,10,10))

%!assert (isbanded ([1, 1],1,1))
%!assert (isbanded ([1; 1],1,1))
%!assert (isbanded (eye(10),0,0))
%!assert (isbanded (eye(10),1,1))
%!assert (isbanded (i*eye(10),1,1))
%!assert (isbanded (logical (eye (10)),1,1))

%! A = [2 3 0 0 0; 1 2 3 0 0; 0 1 2 3 0; 0 0 1 2 3; 0 0 0 1 2];
%! assert (isbanded (A,1,1))
%! assert (! isbanded (A,0,1))
%! assert (! isbanded (A,1,0))

## Test input validation
%!error isbanded ()
%!error isbanded (1)
%!error isbanded (1,2)
%!error isbanded (1,2,3,4)
%!error <LOWER and UPPER must be non-negative> isbanded (1, -1, 1)
%!error <LOWER and UPPER must be non-negative> isbanded (1, 1, -1)
%!error <LOWER and UPPER must be non-negative> isbanded (1, {1}, 1)
%!error <LOWER and UPPER must be non-negative> isbanded (1, 1, {1})

