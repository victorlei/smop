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
## @deftypefn {Function File} {} istriu (@var{A})
## Return true if @var{A} is an upper triangular matrix.
##
## An upper triangular matrix has nonzero entries only on the main diagonal and
## above.
## @seealso{isdiag, isbanded, istril, triu, bandwidth}
## @end deftypefn

## Author: Massimiliano Fasi

function retval = istriu (A)

  if (nargin != 1)
    print_usage ();
  endif

  retval = (isnumeric (A) || islogical (A)) && ndims (A) == 2;
  if (retval)
    [i, j] = find (A);
    retval = all (i <= j);
  endif

endfunction


%!assert (! istriu ("string"))
%!assert (istriu ([]))
%!assert (! istriu (zeros (2,2,2)))

%!assert (istriu (1))
%!assert (istriu ([1, 1]))
%!assert (! istriu ([1; 1]))
%!assert (istriu (eye (10)))
%!assert (istriu (speye (100)))

%!assert (istriu (triu (randn (10))))
%!assert (! istriu (randn (10)))

## Test input validation
%!error istriu ()
%!error istriu (1,2)

