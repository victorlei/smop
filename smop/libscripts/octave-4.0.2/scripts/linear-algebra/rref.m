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
## @deftypefn  {Function File} {} rref (@var{A})
## @deftypefnx {Function File} {} rref (@var{A}, @var{tol})
## @deftypefnx {Function File} {[@var{r}, @var{k}] =} rref (@dots{})
## Return the reduced row echelon form of @var{A}.
##
## @var{tol} defaults to
## @code{eps * max (size (@var{A})) * norm (@var{A}, inf)}.
##
## The optional return argument @var{k} contains the vector of
## "bound variables", which are those columns on which elimination has been
## performed.
##
## @end deftypefn

## Author: Paul Kienzle <pkienzle@users.sf.net>
##         (based on an anonymous source from the public domain)

function [A, k] = rref (A, tol)

  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif

  if (ndims (A) > 2)
    error ("rref: expecting matrix argument");
  endif

  [rows, cols] = size (A);

  if (nargin < 2)
    if (isa (A, "single"))
      tol = eps ("single") * max (rows, cols) * norm (A, inf ("single"));
    else
      tol = eps * max (rows, cols) * norm (A, inf);
    endif
  endif

  used = zeros (1, cols);
  r = 1;
  for c = 1:cols
    ## Find the pivot row
    [m, pivot] = max (abs (A(r:rows,c)));
    pivot = r + pivot - 1;

    if (m <= tol)
      ## Skip column c, making sure the approximately zero terms are
      ## actually zero.
      A(r:rows, c) = zeros (rows-r+1, 1);
    else
      ## keep track of bound variables
      used(1, c) = 1;

      ## Swap current row and pivot row
      A([pivot, r], c:cols) = A([r, pivot], c:cols);

      ## Normalize pivot row
      A(r, c:cols) = A(r, c:cols) / A(r, c);

      ## Eliminate the current column
      ridx = [1:r-1, r+1:rows];
      A(ridx, c:cols) = A(ridx, c:cols) - A(ridx, c) * A(r, c:cols);

      ## Check if done
      if (r++ == rows)
        break;
      endif
    endif
  endfor
  k = find (used);

endfunction


%!test
%! a = [1];
%! [r k] = rref (a);
%! assert (r, [1], 2e-8);
%! assert (k, [1], 2e-8);

%!test
%! a = [1 3; 4 5];
%! [r k] = rref (a);
%! assert (rank (a), rank (r), 2e-8);
%! assert (r, eye (2), 2e-8);
%! assert (k == [1, 2] || k == [2, 1]);

%!test
%! a = [1 3; 4 5; 7 9];
%! [r k] = rref (a);
%! assert (rank (a), rank (r), 2e-8);
%! assert (r, eye(3)(:,1:2), 2e-8);
%! assert (k, [1 2], 2e-8);

%!test
%! a = [1 2 3; 2 4 6; 7 2 0];
%! [r k] = rref (a);
%! assert (rank (a), rank (r), 2e-8);
%! assert (r, [1 0 (3-7/2); 0 1 (7/4); 0 0 0], 2e-8);
%! assert (k, [1 2], 2e-8);

%!test
%! a = [1 2 1; 2 4 2.01; 2 4 2.1];
%! tol = 0.02;
%! [r k] = rref (a, tol);
%! assert (rank (a, tol), rank (r, tol), 2e-8);
%! tol = 0.2;
%! [r k] = rref (a, tol);
%! assert (rank (a, tol), rank (r, tol), 2e-8);

%!error rref ();

