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
## @deftypefn {Function File} {} ranks (@var{x}, @var{dim})
## Return the ranks of @var{x} along the first non-singleton dimension
## adjusted for ties.
##
## If the optional argument @var{dim} is given, operate along this dimension.
## @seealso{spearman, kendall}
## @end deftypefn

## Author: KH <Kurt.Hornik@wu-wien.ac.at>
## Description: Compute ranks

## This code was rather ugly, since it didn't use sort due to the
## fact of how to deal with ties. Now it does use sort and its
## even uglier!!! At least it handles NDArrays..

function y = ranks (x, dim)

  if (nargin != 1 && nargin != 2)
    print_usage ();
  endif

  if (! (isnumeric (x) || islogical (x)))
    error ("ranks: X must be a numeric vector or matrix");
  endif

  nd = ndims (x);
  sz = size (x);
  if (nargin != 2)
    ## Find the first non-singleton dimension.
    (dim = find (sz > 1, 1)) || (dim = 1);
  else
    if (!(isscalar (dim) && dim == fix (dim))
        || !(1 <= dim && dim <= nd))
      error ("ranks: DIM must be an integer and a valid dimension");
    endif
  endif

  if (sz(dim) == 1)
    y = ones (sz);
  else
    ## The algorithm works only on dim = 1, so permute if necesary.
    if (dim != 1)
      perm = [1 : nd];
      perm(1) = dim;
      perm(dim) = 1;
      x = permute (x, perm);
    endif
    sz = size (x);
    infvec = -Inf ([1, sz(2 : end)]);
    [xs, xi] = sort (x);
    eq_el = find (diff ([xs; infvec]) == 0);
    if (isempty (eq_el))
      [eq_el, y] = sort (xi);
    else
      runs = setdiff (eq_el, eq_el+1);
      len = diff (find (diff ([Inf; eq_el; -Inf]) != 1)) + 1;
      [eq_el, y] = sort (xi);
      for i = 1 : length (runs)
        y (xi (runs (i) + [0:(len(i)-1)]) + floor (runs (i) ./ sz(1))
           * sz(1)) = eq_el(runs(i)) + (len(i) - 1) / 2;
      endfor
    endif
    if (dim != 1)
      y = permute (y, perm);
    endif
  endif

endfunction


%!assert (ranks (1:2:10), 1:5)
%!assert (ranks (10:-2:1), 5:-1:1)
%!assert (ranks ([2, 1, 2, 4]), [2.5, 1, 2.5, 4])
%!assert (ranks (ones (1, 5)), 3*ones (1, 5))
%!assert (ranks (1e6*ones (1, 5)), 3*ones (1, 5))
%!assert (ranks (rand (1, 5), 1), ones (1, 5))

## Test input validation
%!error ranks ()
%!error ranks (1, 2, 3)
%!error ranks ({1, 2})
%!error ranks (['A'; 'B'])
%!error ranks (1, 1.5)
%!error ranks (1, 0)
%!error ranks (1, 3)

