## Copyright (C) 2008-2015 VZLU Prague, a.s., Czech Republic
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
## @deftypefn {Function File} {@var{angle} =} subspace (@var{A}, @var{B})
## Determine the largest principal angle between two subspaces
## spanned by the columns of matrices @var{A} and @var{B}.
## @end deftypefn

## Author: Jaroslav Hajek <highegg@gmail.com>

## reference:
## [1]  Andrew V. Knyazev, Merico E. Argentati:
## Principal Angles between Subspaces in an A-Based Scalar Product:
## Algorithms and Perturbation Estimates.
## SIAM Journal on Scientific Computing, Vol. 23 no. 6, pp. 2008-2040
##
## other texts are also around...

function ang = subspace (A, B)

  if (nargin != 2)
    print_usage ();
  elseif (ndims (A) != 2 || ndims (B) != 2)
    error ("subspace: expecting A and B to be 2-dimensional arrays");
  elseif (rows (A) != rows (B))
    error ("subspace: column dimensions of A and B must match");
  endif

  A = orth (A);
  B = orth (B);
  c = A'*B;
  scos = min (svd (c));
  if (scos^2 > 1/2)
    if (columns (A) >= columns (B))
      c = B - A*c;
    else
      c = A - B*c';
    endif
    ssin = max (svd (c));
    ang = asin (min (ssin, 1));
  else
    ang = acos (scos);
  endif

endfunction


%!test
%! ## For random vectors
%! a = rand (2,1);
%! b = rand (2,1);
%! a1 = norm (a,2);
%! b1 = norm (b,2);
%! theta = acos (dot (a,b)/(a1*b1));
%! assert (theta, subspace (a, b), 100*eps);

%!test
%! ## For random matrices
%! M = rand (3, 3);
%! assert (0, subspace (M, M'), 100*eps);

