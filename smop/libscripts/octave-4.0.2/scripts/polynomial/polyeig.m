## Copyright (C) 2012-2015 Fotios Kasolis
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
## @deftypefn  {Function File} {@var{z} =} polyeig (@var{C0}, @var{C1}, @dots{}, @var{Cl})
## @deftypefnx {Function File} {[@var{v}, @var{z}] =} polyeig (@var{C0}, @var{C1}, @dots{}, @var{Cl})
##
## Solve the polynomial eigenvalue problem of degree @var{l}.
##
## Given an @var{n*n} matrix polynomial
##
## @code{@var{C}(s) = @var{C0} + @var{C1} s + @dots{} + @var{Cl} s^l}
##
## @code{polyeig} solves the eigenvalue problem
##
## @code{(@var{C0} + @var{C1} + @dots{} + @var{Cl})v = 0}.
##
## Note that the eigenvalues @var{z} are the zeros of the matrix polynomial.
## @var{z} is a row vector with @var{n*l} elements.  @var{v} is a matrix
## (@var{n} x @var{n}*@var{l}) with columns that correspond to the
## eigenvectors.
##
## @seealso{eig, eigs, compan}
## @end deftypefn

## Author: Fotios Kasolis

function [z, v] = polyeig (varargin)

  if (nargin < 1 || nargout > 2)
    print_usage ();
  endif

  nin = numel (varargin);
  n = rows (varargin{1});

  for i = 1 : nin
    if (! issquare (varargin{i}))
      error ("polyeig: coefficients must be square matrices");
    endif
    if (rows (varargin{i}) != n)
      error ("polyeig: coefficients must have the same dimensions");
    endif
  endfor

  ## matrix polynomial degree
  l = nin - 1;

  ## form needed matrices
  C = [ zeros(n * (l - 1), n), eye(n * (l - 1));
       -cell2mat(varargin(1:end-1)) ];

  D = [ eye(n * (l - 1)), zeros(n * (l - 1), n);
       zeros(n, n * (l - 1)), varargin{end} ];

  ## solve generalized eigenvalue problem
  if (nargout == 2)
    [z, v] = eig (C, D);
    v = diag (v);
    ## return n-element eigenvectors normalized so that the infinity-norm = 1
    z = z(1:n,:);
    ## max() takes the abs if complex:
    t = max (z);
    z /= diag (t);
  else
    z = eig (C, D);
  endif

endfunction


%!shared C0, C1
%! C0 = [8, 0; 0, 4]; C1 = [1, 0; 0, 1];

%!test
%! z = polyeig (C0, C1);
%! assert (z, [-8; -4]);

%!test
%! [v,z] = polyeig (C0, C1);
%! assert (z, [-8; -4]);
%! z = diag (z);
%! d = C0*v + C1*v*z;
%! assert (norm (d), 0.0);

## Test input validation
%!error polyeig ()
%!error [a,b,c] = polyeig (1)
%!error <coefficients must be square matrices> polyeig (ones (3,2))
%!error <coefficients must have the same dimensions> polyeig (ones (3,3), ones (2,2))
