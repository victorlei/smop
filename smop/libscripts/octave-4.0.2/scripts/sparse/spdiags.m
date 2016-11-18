## Copyright (C) 2000-2015 Paul Kienzle
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
## @deftypefn  {Function File} {@var{B} =} spdiags (@var{A})
## @deftypefnx {Function File} {[@var{B}, @var{d}] =} spdiags (@var{A})
## @deftypefnx {Function File} {@var{B} =} spdiags (@var{A}, @var{d})
## @deftypefnx {Function File} {@var{A} =} spdiags (@var{v}, @var{d}, @var{A})
## @deftypefnx {Function File} {@var{A} =} spdiags (@var{v}, @var{d}, @var{m}, @var{n})
## A generalization of the function @code{diag}.
##
## Called with a single input argument, the nonzero diagonals @var{d} of
## @var{A} are extracted.
##
## With two arguments the diagonals to extract are given by the vector @var{d}.
##
## The other two forms of @code{spdiags} modify the input matrix by replacing
## the diagonals.  They use the columns of @var{v} to replace the diagonals
## represented by the vector @var{d}.  If the sparse matrix @var{A} is
## defined then the diagonals of this matrix are replaced.  Otherwise a
## matrix of @var{m} by @var{n} is created with the diagonals given by the
## columns of @var{v}.
##
## Negative values of @var{d} represent diagonals below the main diagonal, and
## positive values of @var{d} diagonals above the main diagonal.
##
## For example:
##
## @example
## @group
## spdiags (reshape (1:12, 4, 3), [-1 0 1], 5, 4)
##    @result{} 5 10  0  0
##       1  6 11  0
##       0  2  7 12
##       0  0  3  8
##       0  0  0  4
## @end group
## @end example
##
## @seealso{diag}
## @end deftypefn

function [B, d] = spdiags (v, d, m, n)

  if (nargin < 1 || nargin > 4)
    print_usage ();
  endif

  if (nargin == 1 || nargin == 2)
    ## extract nonzero diagonals of A into B,d
    [nr, nc] = size (v);
    [i, j] = find (v);

    if (nargin == 1)
      ## d contains the active diagonals
      d = unique (j-i);
    endif

    ## FIXME: Maybe this could be done faster using [i,j,v] = find (v)
    ##        and then massaging the indices i, j.  However, some
    ##        benchmarking has shown that diag() written in C++ makes
    ##        the following code faster even with the for loop.
    Brows = min (nr, nc);
    B = zeros (Brows, length (d));
    for k = 1:length (d)
      dn = d(k);
      if (dn <= -nr || dn > nc)
        continue;
      endif
      dv = diag (v, dn);
      len = rows (dv);
      ## Put sub/super-diagonals in the right place based on matrix size (MxN)
      if (nr >= nc)
        if (dn > 0)
          offset = Brows - len + 1;
          B(offset:Brows, k) = dv;
        else
          B(1:len, k) = dv;
        endif
      else
        if (dn < 0)
          offset = Brows - len + 1;
          B(offset:Brows, k) = dv;
        else
          B(1:len, k) = dv;
        endif
      endif
    endfor

  elseif (nargin == 3)
    ## Replace specific diagonals d of m with v,d
    [nr, nc] = size (m);
    A = spdiags (m, d);
    B = m - spdiags (A, d, nr, nc) + spdiags (v, d, nr, nc);

  else
    ## Create new matrix of size mxn using v,d
    [j, i, v] = find (v);
    if (m >= n)
      offset = max (min (d(:), n-m), 0);
    else
      offset = d(:);
    endif
    j = j(:) + offset(i(:));
    i = j - d(:)(i(:));
    idx = i > 0 & i <= m & j > 0 & j <= n;
    B = sparse (i(idx), j(idx), v(idx), m, n);

  endif

endfunction


%!test
%! [B,d] = spdiags (magic (3));
%! assert (d, [-2 -1 0 1 2]');
%! assert (B, [4 3 8 0 0
%!             0 9 5 1 0
%!             0 0 2 7 6]);
%! B = spdiags (magic (3), [-2 1]);
%! assert (B, [4 0; 0 1; 0 7]);

## Test zero filling for supra- and super-diagonals
%!test
%! ## Case 1: M = N
%! A = sparse (zeros (3,3));
%! A(1,3) = 13;
%! A(3,1) = 31;
%! [B, d] = spdiags (A);
%! assert (d, [-2 2]');
%! assert (B, [31 0; 0 0; 0 13]);
%! assert (spdiags (B, d, 3,3), A)

%!test
%! ## Case 1: M > N
%! A = sparse (zeros (4,3));
%! A(1,3) = 13;
%! A(3,1) = 31;
%! [B, d] = spdiags (A);
%! assert (d, [-2 2]');
%! assert (B, [31 0; 0 0; 0 13]);
%! assert (spdiags (B, d, 4,3), A)

%!test
%! ## Case 1: M < N
%! A = sparse (zeros (3,4));
%! A(1,3) = 13;
%! A(3,1) = 31;
%! [B, d] = spdiags (A);
%! assert (d, [-2 2]');
%! assert (B, [0 13; 0 0; 31 0]);
%! assert (spdiags (B, d, 3,4), A)

%!assert (spdiags (zeros (1,0),1,1,1), sparse (0))
%!assert (spdiags (zeros (0,1),1,1,1), sparse (0))
%!assert (spdiags ([0.5 -1 0.5], 0:2, 1, 1), sparse (0.5))

## Test input validation
%!error spdiags ()
%!error spdiags (1,2,3,4,5)

