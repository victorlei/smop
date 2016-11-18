## Copyright (C) 1993-2015 John W. Eaton
## Copyright (C) 2009 VZLU Prague
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
## @deftypefn  {Function File} {} vander (@var{c})
## @deftypefnx {Function File} {} vander (@var{c}, @var{n})
## Return the Vandermonde matrix whose next to last column is @var{c}.
##
## If @var{n} is specified, it determines the number of columns; otherwise,
## @var{n} is taken to be equal to the length of @var{c}.
##
## A Vandermonde matrix has the form:
## @tex
## $$
## \left[\matrix{c_1^{n-1}  & \cdots & c_1^2  & c_1    & 1      \cr
##               c_2^{n-1}  & \cdots & c_2^2  & c_2    & 1      \cr
##               \vdots     & \ddots & \vdots & \vdots & \vdots \cr
##               c_n^{n-1}  & \cdots & c_n^2  & c_n    & 1      }\right]
## $$
## @end tex
## @ifnottex
##
## @example
## @group
## c(1)^(n-1) @dots{} c(1)^2  c(1)  1
## c(2)^(n-1) @dots{} c(2)^2  c(2)  1
##     .     .      .      .    .
##     .       .    .      .    .
##     .         .  .      .    .
## c(n)^(n-1) @dots{} c(n)^2  c(n)  1
## @end group
## @end example
##
## @end ifnottex
## @seealso{polyfit}
## @end deftypefn

## Author: jwe

function retval = vander (c, n)

  if (nargin == 1)
    n = length (c);
  elseif (nargin != 2)
    print_usage ();
  endif

  if (! isvector (c))
    error ("vander: polynomial C must be a vector");
  endif

  ## avoiding many ^s appears to be faster for n >= 100.
  retval = zeros (length (c), n, class (c));
  d = 1;
  c = c(:);
  for i = n:-1:1
    retval(:,i) = d;
    d .*= c;
  endfor

endfunction


%!test
%! c = [0,1,2,3];
%! expect = [0,0,0,1; 1,1,1,1; 8,4,2,1; 27,9,3,1];
%! assert (vander (c), expect);

%!assert (vander (1), 1)
%!assert (vander ([1, 2, 3]), vander ([1; 2; 3]))
%!assert (vander ([1, 2, 3]), [1, 1, 1; 4, 2, 1; 9, 3, 1])
%!assert (vander ([1, 2, 3]*i), [-1, i, 1; -4, 2i, 1; -9, 3i, 1])

%!assert (vander (2, 3), [4, 2, 1])
%!assert (vander ([2, 3], 3), [4, 2, 1; 9, 3, 1])

%!error vander ()
%!error vander (1, 2, 3)
%!error <polynomial C must be a vector> vander ([1, 2; 3, 4])

