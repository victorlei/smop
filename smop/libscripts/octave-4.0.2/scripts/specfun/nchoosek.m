## Copyright (C) 2001-2015 Rolf Fabian and Paul Kienzle
## Copyright (C) 2008 Jaroslav Hajek
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
## @deftypefn  {Function File} {@var{c} =} nchoosek (@var{n}, @var{k})
## @deftypefnx {Function File} {@var{c} =} nchoosek (@var{set}, @var{k})
##
## Compute the binomial coefficient of @var{n} or list all possible
## combinations of a @var{set} of items.
##
## If @var{n} is a scalar then calculate the binomial coefficient
## of @var{n} and @var{k} which is defined as
## @tex
## $$
##  {n \choose k} = {n (n-1) (n-2) \cdots (n-k+1) \over k!}
##                = {n! \over k! (n-k)!}
## $$
## @end tex
## @ifnottex
##
## @example
## @group
##  /   \
##  | n |    n (n-1) (n-2) @dots{} (n-k+1)       n!
##  |   |  = ------------------------- =  ---------
##  | k |               k!                k! (n-k)!
##  \   /
## @end group
## @end example
##
## @end ifnottex
## @noindent
## This is the number of combinations of @var{n} items taken in groups of
## size @var{k}.
##
## If the first argument is a vector, @var{set}, then generate all
## combinations of the elements of @var{set}, taken @var{k} at a time, with
## one row per combination.  The result @var{c} has @var{k} columns and
## @w{@code{nchoosek (length (@var{set}), @var{k})}} rows.
##
## For example:
##
## How many ways can three items be grouped into pairs?
##
## @example
## @group
## nchoosek (3, 2)
##    @result{} 3
## @end group
## @end example
##
## What are the possible pairs?
##
## @example
## @group
## nchoosek (1:3, 2)
##    @result{}  1   2
##        1   3
##        2   3
## @end group
## @end example
##
## Programming Note: When calculating the binomial coefficient @code{nchoosek}
## works only for non-negative, integer arguments.  Use @code{bincoeff} for
## non-integer and negative scalar arguments, or for computing many binomial
## coefficients at once with vector inputs for @var{n} or @var{k}.
##
## @seealso{bincoeff, perms}
## @end deftypefn

## Author: Rolf Fabian  <fabian@tu-cottbus.de>
## Author: Paul Kienzle <pkienzle@users.sf.net>
## Author: Jaroslav Hajek

function C = nchoosek (v, k)

  if (nargin != 2
      || ! (isreal (k) && isscalar (k))
      || ! (isnumeric (v) && isvector (v)))
    print_usage ();
  endif
  if (k < 0 || k != fix (k))
    error ("nchoosek: K must be an integer >= 0");
  elseif (isscalar (v) && (iscomplex (v) || v < k || v < 0 || v != fix (v)))
    error ("nchoosek: N must be a non-negative integer >= K");
  endif

  n = length (v);

  if (n == 1)
    ## Improve precision at next step.
    k = min (k, v-k);
    C = round (prod ((v-k+1:v)./(1:k)));
    if (C*2*k*eps >= 0.5)
      warning ("nchoosek: possible loss of precision");
    endif
  elseif (k == 0)
    C = zeros (1,0);
  elseif (k == 1)
    C = v(:);
  elseif (k == n)
    C = v(:).';
  elseif (k > n)
    C = zeros (0, k, class (v));
  elseif (k == 2)
    ## Can do it without transpose.
    x = repelems (v(1:n-1), [1:n-1; n-1:-1:1]).';
    y = cat (1, cellslices (v(:), 2:n, n*ones (1, n-1)){:});
    C = [x, y];
  elseif (k < n)
    v = v(:).';
    C = v(k:n);
    l = 1:n-k+1;
    for j = 2:k
      c = columns (C);
      cA = cellslices (C, l, c*ones (1, n-k+1), 2);
      l = c-l+1;
      b = repelems (v(k-j+1:n-j+1), [1:n-k+1; l]);
      C = [b; cA{:}];
      l = cumsum (l);
      l = [1, 1 + l(1:n-k)];
    endfor
    C = C.';
  endif

endfunction


%!assert (nchoosek (80,10), bincoeff (80,10))
%!assert (nchoosek (1:5,3), [1:3;1,2,4;1,2,5;1,3,4;1,3,5;1,4,5;2:4;2,3,5;2,4,5;3:5])
%!assert (size (nchoosek (1:5,0)), [1 0])

## Test input validation
%!error nchoosek ()
%!error nchoosek (1)
%!error nchoosek (1,2,3)

%!error nchoosek (100, 2i)
%!error nchoosek (100, [2 3])
%!error nchoosek ("100", 45)
%!error nchoosek (100*ones (2, 2), 45)
%!error <K must be an integer .= 0> nchoosek (100, -45)
%!error <K must be an integer .= 0> nchoosek (100, 45.5)
%!error <N must be a non-negative integer .= K> nchoosek (100i, 2)
%!error <N must be a non-negative integer .= K> nchoosek (100, 145)
%!error <N must be a non-negative integer .= K> nchoosek (-100, 45)
%!error <N must be a non-negative integer .= K> nchoosek (100.5, 45)
%!warning <possible loss of precision> nchoosek (100, 45);

