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
## @deftypefn  {Function File} {} hankel (@var{c})
## @deftypefnx {Function File} {} hankel (@var{c}, @var{r})
## Return the Hankel matrix constructed from the first column @var{c}, and
## (optionally) the last row @var{r}.
##
## If the last element of @var{c} is not the same as the first element of
## @var{r}, the last element of @var{c} is used.  If the second argument is
## omitted, it is assumed to be a vector of zeros with the same size as @var{c}.
##
## A Hankel matrix formed from an m-vector @var{c}, and an n-vector @var{r},
## has the elements
## @tex
## $$
## H(i, j) = \cases{c_{i+j-1},&$i+j-1\le m$;\cr r_{i+j-m},&otherwise.\cr}
## $$
## @end tex
## @ifnottex
##
## @example
## @group
## H(i,j) = c(i+j-1),  i+j-1 <= m;
## H(i,j) = r(i+j-m),  otherwise
## @end group
## @end example
##
## @end ifnottex
## @seealso{hadamard, toeplitz}
## @end deftypefn

## Author: jwe

function retval = hankel (c, r)

  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif

  if (nargin == 1)

    if (! isvector (c))
      error ("hankel: C must be a vector");
    endif

    nr = length (c);
    nc = nr;
    data = [c(:) ; zeros(nr, 1)];

  else

    if (! (isvector (c) && isvector (r)))
      error ("hankel: C and R must be vectors");
    elseif (r(1) != c(end))
      warning ("hankel: column wins anti-diagonal conflict");
    endif

    nr = length (c);
    nc = length (r);
    data = [c(:) ; r(2:end)(:)];

  endif

  slices = cellslices (data, 1:nc, nr:1:nc+nr-1);
  retval = horzcat (slices{:});

endfunction


%!assert (hankel (1), [1])
%!assert (hankel ([1, 2]), [1, 2; 2, 0])
%!assert (hankel ([1, 2], [2; -1; -3]), [1, 2, -1; 2, -1, -3])
%!assert (hankel (1:3), [1,2,3;2,3,0;3,0,0])
%!assert (hankel (1:3,3:6), [1,2,3,4;2,3,4,5;3,4,5,6])
%!assert (hankel (1:3,3:4), [1,2;2,3;3,4])
%!assert (hankel (1:3,4:6), [1,2,3;2,3,5;3,5,6])

%!error hankel ()
%!error hankel (1, 2, 3)
%!error <C must be a vector> hankel ([1, 2; 3, 4])
%!error <C and R must be vectors> hankel (1:4, [1, 2; 3, 4])

