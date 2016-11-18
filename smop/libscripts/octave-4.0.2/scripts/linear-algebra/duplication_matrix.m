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
## @deftypefn {Function File} {} duplication_matrix (@var{n})
## Return the duplication matrix
## @tex
##  $D_n$
## @end tex
## @ifnottex
## @nospell{@math{Dn}}
## @end ifnottex
## which is the unique
## @tex
##  $n^2 \times n(n+1)/2$
## @end tex
## @ifnottex
## @math{n^2} by @math{n*(n+1)/2}
## @end ifnottex
## matrix such that
## @tex
##  $D_n * {\rm vech} (A) = {\rm vec} (A)$
## @end tex
## @ifnottex
## @nospell{@math{Dn vech (A) = vec (A)}}
## @end ifnottex
## for all symmetric
## @tex
##  $n \times n$
## @end tex
## @ifnottex
## @math{n} by @math{n}
## @end ifnottex
## matrices
## @tex
##  $A$.
## @end tex
## @ifnottex
## @math{A}.
## @end ifnottex
##
## See @nospell{Magnus and Neudecker} (1988), @cite{Matrix Differential
## Calculus with Applications in Statistics and Econometrics.}
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Created: 8 May 1995
## Adapged-By: jwe

function d = duplication_matrix (n)

  if (nargin != 1)
    print_usage ();
  endif

  if (! (isscalar (n) && n > 0 && n == fix (n)))
    error ("duplication_matrix: N must be a positive integer");
  endif

  d = zeros (n * n, n * (n + 1) / 2);

  ## It is clearly possible to make this a LOT faster!
  count = 0;
  for j = 1 : n
    d((j - 1) * n + j, count + j) = 1;
    for i = (j + 1) : n
      d((j - 1) * n + i, count + i) = 1;
      d((i - 1) * n + j, count + i) = 1;
    endfor
    count = count + n - j;
  endfor

endfunction


%!test
%! N = 2;
%! A = rand (N);
%! B = A * A';
%! C = A + A';
%! D = duplication_matrix (N);
%! assert (D * vech (B), vec (B), 1e-6);
%! assert (D * vech (C), vec (C), 1e-6);

%!test
%! N = 3;
%! A = rand (N);
%! B = A * A';
%! C = A + A';
%! D = duplication_matrix (N);
%! assert (D * vech (B), vec (B), 1e-6);
%! assert (D * vech (C), vec (C), 1e-6);

%!test
%! N = 4;
%! A = rand (N);
%! B = A * A';
%! C = A + A';
%! D = duplication_matrix (N);
%! assert (D * vech (B), vec (B), 1e-6);
%! assert (D * vech (C), vec (C), 1e-6);

%!error duplication_matrix ()
%!error duplication_matrix (0.5)
%!error duplication_matrix (-1)
%!error duplication_matrix (ones (1,4))

