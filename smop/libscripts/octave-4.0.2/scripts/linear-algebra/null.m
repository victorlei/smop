## Copyright (C) 1994-2015 John W. Eaton
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
## @deftypefn  {Function File} {} null (@var{A})
## @deftypefnx {Function File} {} null (@var{A}, @var{tol})
## Return an orthonormal basis of the null space of @var{A}.
##
## The dimension of the null space is taken as the number of singular values of
## @var{A} not greater than @var{tol}.  If the argument @var{tol} is missing,
## it is computed as
##
## @example
## max (size (@var{A})) * max (svd (@var{A})) * eps
## @end example
## @seealso{orth}
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Created: 24 December 1993.
## Adapted-By: jwe

function retval = null (A, tol)

  if (isempty (A))
    retval = [];
  else
    [U, S, V] = svd (A);

    [rows, cols] = size (A);

    [S_nr, S_nc] = size (S);

    if (S_nr == 1 || S_nc == 1)
      s = S(1);
    else
      s = diag (S);
    endif

    if (nargin == 1)
      if (isa (A, "single"))
        tol = max (size (A)) * s (1) * eps ("single");
      else
        tol = max (size (A)) * s (1) * eps;
      endif
    elseif (nargin != 2)
      print_usage ();
    endif

    rank = sum (s > tol);

    if (rank < cols)
      retval = V(:, rank+1:cols);
      if (isa (A, "single"))
        retval (abs (retval) < eps ("single")) = 0;
      else
        retval (abs (retval) < eps) = 0;
      endif
    else
      retval = zeros (cols, 0);
    endif
  endif

endfunction


## FIXME: Need some tests for 'single' variables as well

%!test
%! A = 0;
%! assert (null (A), 1);

%!test
%! A = 1;
%! assert (null (A), zeros (1,0));

%!test
%! A = [1 0; 0 1];
%! assert (null (A), zeros (2,0));

%!test
%! A = [1 0; 1 0];
%! assert (null (A), [0 1]');

%!test
%! A = [1 1; 0 0];
%! assert (null (A), [-1/sqrt(2) 1/sqrt(2)]', eps);

%!test
%! tol = 1e-4;
%! A = [1 0; 0 tol-eps];
%! assert (null (A,tol), [0; 1]);

%!test
%! tol = 1e-4;
%! A = [1 0; 0 tol+eps];
%! assert (null (A,tol), zeros (2,0));

%!error null ()

