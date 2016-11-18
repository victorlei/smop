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
## @deftypefn {Function File} {} commutation_matrix (@var{m}, @var{n})
## Return the commutation matrix
## @tex
##  $K_{m,n}$
## @end tex
## @ifnottex
## K(m,n)
## @end ifnottex
## which is the unique
## @tex
##  $m n \times m n$
## @end tex
## @ifnottex
## @var{m}*@var{n} by @var{m}*@var{n}
## @end ifnottex
## matrix such that
## @tex
##  $K_{m,n} \cdot {\rm vec} (A) = {\rm vec} (A^T)$
## @end tex
## @ifnottex
## @math{K(m,n) * vec(A) = vec(A')}
## @end ifnottex
## for all
## @tex
##  $m\times n$
## @end tex
## @ifnottex
## @math{m} by @math{n}
## @end ifnottex
## matrices
## @tex
##  $A$.
## @end tex
## @ifnottex
## @math{A}.
## @end ifnottex
##
## If only one argument @var{m} is given,
## @tex
##  $K_{m,m}$
## @end tex
## @ifnottex
## @math{K(m,m)}
## @end ifnottex
## is returned.
##
## See @nospell{Magnus and Neudecker} (1988), @cite{Matrix Differential
## Calculus with Applications in Statistics and Econometrics.}
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Created: 8 May 1995
## Adapted-By: jwe

function k = commutation_matrix (m, n)

  if (nargin < 1 || nargin > 2)
    print_usage ();
  else
    if (! (isscalar (m) && m == fix (m) && m > 0))
      error ("commutation_matrix: M must be a positive integer");
    endif
    if (nargin == 1)
      n = m;
    elseif (! (isscalar (n) && n == fix (n) && n > 0))
      error ("commutation_matrix: N must be a positive integer");
    endif
  endif

  ## It is clearly possible to make this a LOT faster!
  k = zeros (m * n, m * n);
  for i = 1 : m
    for j = 1 : n
      k((i - 1) * n + j, (j - 1) * m + i) = 1;
    endfor
  endfor

endfunction


%!test
%! c = commutation_matrix (1,1);
%! assert (c,1);

%!test
%! A = rand (3,5);
%! vc = vec (A);
%! vr = vec (A');
%! c = commutation_matrix (3,5);
%! assert (c*vc, vr);

%!test
%! A = rand (4,6);
%! vc = vec (A);
%! vr = vec (A');
%! c = commutation_matrix (4,6);
%! assert (c*vc, vr);

%!error <M must be a positive integer> commutation_matrix (0,0)
%!error <N must be a positive integer> commutation_matrix (1,0)
%!error <M must be a positive integer> commutation_matrix (0,1)

