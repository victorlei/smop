## Copyright (C) 1995-2015 Kurt Hornik
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
## @deftypefn {Function File} {} autoreg_matrix (@var{y}, @var{k})
## Given a time series (vector) @var{y}, return a matrix with ones in the first
## column and the first @var{k} lagged values of @var{y} in the other columns.
##
## In other words, for @var{t} > @var{k},
## @code{[1, @var{y}(@var{t}-1), @dots{}, @var{y}(@var{t}-@var{k})]} is the
## t-th row of the result.
##
## The resulting matrix may be used as a regressor matrix in autoregressions.
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Design matrix for autoregressions

function X = autoreg_matrix (y, k)

  if (nargin != 2)
    print_usage ();
  endif

  if (! (isvector (y)))
    error ("autoreg_matrix: Y must be a vector");
  endif

  T = length (y);
  y = reshape (y, T, 1);
  X = ones (T, k+1);
  for j = 1 : k;
    X(:, j+1) = [(zeros (j, 1)); y(1:T-j)];
  endfor

endfunction


%!test
%! K = 4;
%! A = zeros (1,K+1);
%! A(1) = 1;
%! B = eye (K+1);
%! B(:,1) = 1;
%! assert (autoreg_matrix (A,K), B);

%!error autoreg_matrix ()
%!error autoreg_matrix (1)
%!error autoreg_matrix (ones (4,1), 5)

