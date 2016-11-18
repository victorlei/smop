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
## @deftypefn {Function File} {[@var{aa}, @var{bb}, @var{q}, @var{z}] =} qzhess (@var{A}, @var{B})
## Compute the Hessenberg-triangular decomposition of the matrix pencil
## @code{(@var{A}, @var{B})}, returning
## @code{@var{aa} = @var{q} * @var{A} * @var{z}},
## @code{@var{bb} = @var{q} * @var{B} * @var{z}}, with @var{q} and @var{z}
## orthogonal.
##
## For example:
##
## @example
## @group
## [aa, bb, q, z] = qzhess ([1, 2; 3, 4], [5, 6; 7, 8])
##      @result{} aa = [ -3.02244, -4.41741;  0.92998,  0.69749 ]
##      @result{} bb = [ -8.60233, -9.99730;  0.00000, -0.23250 ]
##      @result{}  q = [ -0.58124, -0.81373; -0.81373,  0.58124 ]
##      @result{}  z = [ 1, 0; 0, 1 ]
## @end group
## @end example
##
## The Hessenberg-triangular decomposition is the first step in
## @nospell{Moler and Stewart's} QZ@tie{}decomposition algorithm.
##
## Algorithm taken from @nospell{Golub and Van Loan},
## @cite{Matrix Computations, 2nd edition}.
##
## @seealso{lu, chol, hess, qr, qz, schur, svd}
## @end deftypefn

## Author: A. S. Hodel <scotte@eng.auburn.edu>
## Created: August 1993
## Adapted-By: jwe

function [aa, bb, q, z] = qzhess (A, B)

  if (nargin != 2)
    print_usage ();
  endif

  [na, ma] = size (A);
  [nb, mb] = size (B);
  if (na != ma || na != nb || nb != mb)
    error ("qzhess: incompatible dimensions");
  endif

  ## Reduce to hessenberg-triangular form.

  [q, bb] = qr (B);
  aa = q' * A;
  q = q';
  z = eye (na);
  for j = 1:(na-2)
    for i = na:-1:(j+2)

      ## disp (["zero out aa(", num2str(i), ",", num2str(j), ")"])

      rot = givens (aa (i-1, j), aa (i, j));
      aa((i-1):i, :) = rot *aa((i-1):i, :);
      bb((i-1):i, :) = rot *bb((i-1):i, :);
       q((i-1):i, :) = rot * q((i-1):i, :);

      ## disp (["now zero out bb(", num2str(i), ",", num2str(i-1), ")"])

      rot = givens (bb (i, i), bb (i, i-1))';
      bb(:, (i-1):i) = bb(:, (i-1):i) * rot';
      aa(:, (i-1):i) = aa(:, (i-1):i) * rot';
       z(:, (i-1):i) =  z(:, (i-1):i) * rot';

    endfor
  endfor

  bb(2, 1) = 0.0;
  for i = 3:na
    bb (i, 1:(i-1)) = zeros (1, i-1);
    aa (i, 1:(i-2)) = zeros (1, i-2);
  endfor

endfunction


%!test
%! a = [1 2 1 3;
%!      2 5 3 2;
%!      5 5 1 0;
%!      4 0 3 2];
%! b = [0 4 2 1;
%!      2 3 1 1;
%!      1 0 2 1;
%!      2 5 3 2];
%! mask = [0 0 0 0;
%!         0 0 0 0;
%!         1 0 0 0;
%!         1 1 0 0];
%! [aa, bb, q, z] = qzhess (a, b);
%! assert (inv (q) - q', zeros (4), 2e-8);
%! assert (inv (z) - z', zeros (4), 2e-8);
%! assert (q * a * z, aa, 2e-8);
%! assert (aa .* mask, zeros (4), 2e-8);
%! assert (q * b * z, bb, 2e-8);
%! assert (bb .* mask, zeros (4), 2e-8);

%!test
%! a = [1 2 3 4 5;
%!      3 2 3 1 0;
%!      4 3 2 1 1;
%!      0 1 0 1 0;
%!      3 2 1 0 5];
%! b = [5 0 4 0 1;
%!      1 1 1 2 5;
%!      0 3 2 1 0;
%!      4 3 0 3 5;
%!      2 1 2 1 3];
%! mask = [0 0 0 0 0;
%!         0 0 0 0 0;
%!         1 0 0 0 0;
%!         1 1 0 0 0;
%!         1 1 1 0 0];
%! [aa, bb, q, z] = qzhess (a, b);
%! assert (inv (q) - q', zeros (5), 2e-8);
%! assert (inv (z) - z', zeros (5), 2e-8);
%! assert (q * a * z, aa, 2e-8);
%! assert (aa .* mask, zeros (5), 2e-8);
%! assert (q * b * z, bb, 2e-8);
%! assert (bb .* mask, zeros (5), 2e-8);

%!error qzhess ([0])
%!error qzhess ()

