## Copyright (C) 2006-2015 David Bateman and Marco Caliari
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
## @deftypefn  {Function File} {@var{n} =} normest (@var{A})
## @deftypefnx {Function File} {@var{n} =} normest (@var{A}, @var{tol})
## @deftypefnx {Function File} {[@var{n}, @var{c}] =} normest (@dots{})
## Estimate the 2-norm of the matrix @var{A} using a power series analysis.
##
## This is typically used for large matrices, where the cost of calculating
## @code{norm (@var{A})} is prohibitive and an approximation to the 2-norm is
## acceptable.
##
## @var{tol} is the tolerance to which the 2-norm is calculated.  By default
## @var{tol} is 1e-6.
##
## The optional output @var{c} returns the number of iterations needed for
## @code{normest} to converge.
## @end deftypefn

function [n, c] = normest (A, tol = 1e-6)

  if (nargin != 1 && nargin != 2)
    print_usage ();
  endif

  if (! (isnumeric (A) && ndims (A) == 2))
    error ("normest: A must be a numeric 2-D matrix");
  endif

  if (! (isscalar (tol) && isreal (tol)))
    error ("normest: TOL must be a real scalar");
  endif

  if (! isfloat (A))
    A = double (A);
  endif

  tol = max (tol, eps (class (A)));
  ## Set random number generator to depend on target matrix
  v = rand ("state");
  rand ("state", full (trace (A)));
  ncols = columns (A);
  ## Randomize y to avoid bad guesses for important matrices.
  y = rand (ncols, 1);
  c = 0;
  n = 0;
  do
    n0 = n;
    x = A * y;
    normx = norm (x);
    if (normx == 0)
      x = rand (ncols, 1);
    else
      x = x / normx;
    endif
    y = A' * x;
    n = norm (y);
    c += 1;
  until (abs (n - n0) <= tol * n)

  rand ("state", v);    # restore state of random number generator
endfunction


%!test
%! A = toeplitz ([-2,1,0,0]);
%! assert (normest (A), norm (A), 1e-6);

%!test
%! A = rand (10);
%! assert (normest (A), norm (A), 1e-6);

## Test input validation
%!error normest ()
%!error normest (1, 2, 3)
%!error normest ([true true])
%!error normest (ones (3,3,3))
%!error normest (1, [1, 2])
%!error normest (1, 1+1i)

