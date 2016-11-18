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
## @deftypefn  {Function File} {} kendall (@var{x})
## @deftypefnx {Function File} {} kendall (@var{x}, @var{y})
## @cindex Kendall's Tau
## Compute Kendall's @var{tau}.
##
## For two data vectors @var{x}, @var{y} of common length @var{n}, Kendall's
## @var{tau} is the correlation of the signs of all rank differences of
## @var{x} and @var{y}; i.e., if both @var{x} and @var{y} have distinct
## entries, then
##
## @tex
## $$ \tau = {1 \over n(n-1)} \sum_{i,j} {\rm sign}(q_i-q_j) {\rm sign}(r_i-r_j) $$
## @end tex
## @ifnottex
##
## @example
## @group
##          1
## tau = -------   SUM sign (q(i) - q(j)) * sign (r(i) - r(j))
##       n (n-1)   i,j
## @end group
## @end example
##
## @end ifnottex
## @noindent
## in which the
## @tex
## $q_i$ and $r_i$
## @end tex
## @ifnottex
## @var{q}(@var{i}) and @var{r}(@var{i})
## @end ifnottex
## are the ranks of @var{x} and @var{y}, respectively.
##
## If @var{x} and @var{y} are drawn from independent distributions,
## Kendall's @var{tau} is asymptotically normal with mean 0 and variance
## @tex
## ${2 (2n+5) \over 9n(n-1)}$.
## @end tex
## @ifnottex
## @code{(2 * (2@var{n}+5)) / (9 * @var{n} * (@var{n}-1))}.
## @end ifnottex
##
## @code{kendall (@var{x})} is equivalent to @code{kendall (@var{x},
## @var{x})}.
## @seealso{ranks, spearman}
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Kendall's rank correlation tau

function tau = kendall (x, y = [])

  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif

  if (   ! (isnumeric (x) || islogical (x))
      || ! (isnumeric (y) || islogical (y)))
    error ("kendall: X and Y must be numeric matrices or vectors");
  endif

  if (ndims (x) != 2 || ndims (y) != 2)
    error ("kendall: X and Y must be 2-D matrices or vectors");
  endif

  if (isrow (x))
    x = x.';
  endif
  [n, c] = size (x);

  if (nargin == 2)
    if (isrow (y))
      y = y.';
    endif
    if (rows (y) != n)
      error ("kendall: X and Y must have the same number of observations");
    else
      x = [x, y];
    endif
  endif

  if (isa (x, "single") || isa (y, "single"))
    cls = "single";
  else
    cls = "double";
  endif
  r   = ranks (x);
  m   = sign (kron (r, ones (n, 1, cls)) - kron (ones (n, 1, cls), r));
  tau = corr (m);

  if (nargin == 2)
    tau = tau(1 : c, (c + 1) : columns (x));
  endif

endfunction


%!test
%! x = [1:2:10];
%! y = [100:10:149];
%! assert (kendall (x,y), 1, 5*eps);
%! assert (kendall (x,fliplr (y)), -1, 5*eps);

%!assert (kendall (logical (1)), 1)
%!assert (kendall (single (1)), single (1))

## Test input validation
%!error kendall ()
%!error kendall (1, 2, 3)
%!error kendall (['A'; 'B'])
%!error kendall (ones (2,1), ['A'; 'B'])
%!error kendall (ones (2,2,2))
%!error kendall (ones (2,2), ones (2,2,2))
%!error kendall (ones (2,2), ones (3,2))

