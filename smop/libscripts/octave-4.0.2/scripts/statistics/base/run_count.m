## Copyright (C) 1995-2015 Friedrich Leisch
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
## @deftypefn  {Function File} {} run_count (@var{x}, @var{n})
## @deftypefnx {Function File} {} run_count (@var{x}, @var{n}, @var{dim})
## Count the upward runs along the first non-singleton dimension of @var{x}
## of length 1, 2, @dots{}, @var{n}-1 and greater than or equal to @var{n}.
##
## If the optional argument @var{dim} is given then operate along this
## dimension.
## @seealso{runlength}
## @end deftypefn

## Author: FL <Friedrich.Leisch@ci.tuwien.ac.at>
## Description: Count upward runs

function retval = run_count (x, n, dim)

  if (nargin != 2 && nargin != 3)
    print_usage ();
  endif

  if (! (isnumeric (x) || islogical (x)))
    error ("run_count: X must be a numeric vector or matrix");
  endif

  if (!(isscalar (n) && n == fix (n) && n > 0))
    error ("run_count: N must be a positive integer");
  endif

  nd = ndims (x);
  sz = size (x);
  if (nargin != 3)
    ## Find the first non-singleton dimension.
    (dim = find (sz > 1, 1)) || (dim = 1);
  else
    if (!(isscalar (dim) && dim == fix (dim))
        || !(1 <= dim && dim <= nd))
      error ("run_count: DIM must be an integer and a valid dimension");
    endif
  endif

  ## Algorithm works on rows.  Permute array if necessary, ipermute back at end
  if (dim != 1)
    perm = [1 : nd];
    perm(1) = dim;
    perm(dim) = 1;
    x = permute (x, perm);
  endif

  sz = size (x);
  idx = cell ();
  for i = 1 : nd
    idx{i} = 1 : sz(i);
  endfor
  c = sz(1);
  tmp = zeros ([c + 1, sz(2 : end)]);
  infvec = Inf ([1, sz(2 : end)]);

  ind = find (diff ([infvec; x; -infvec]) < 0);
  tmp(ind(2:end) - 1) = diff (ind);
  tmp = tmp(idx{:});

  sz(1) = n;
  retval = zeros (sz);
  for k = 1 : (n-1)
    idx{1} = k;
    retval(idx{:}) = sum (tmp == k);
  endfor
  idx{1} = n;
  retval(idx{:}) = sum (tmp >= n);

  if (dim != 1)
    retval = ipermute (retval, perm);
  endif

endfunction


%!assert (run_count (magic (3), 4), [1,0,1;1,0,1;0,1,0;0,0,0])
%!assert (run_count (magic (3), 4, 2), [1,0,1;1,0,1;0,1,0;0,0,0]')
%!assert (run_count (5:-1:1, 5), [5, 0, 0, 0, 0])
%!assert (run_count (ones (3), 4), [0,0,0;0,0,0;1,1,1;0,0,0])

## Test input validation
%!error run_count ()
%!error run_count (1)
%!error run_count (1, 2, 3, 4)
%!error run_count ({1, 2}, 3)
%!error run_count (['A'; 'A'; 'B'], 3)
%!error run_count (1:5, ones (2,2))
%!error run_count (1:5, 1.5)
%!error run_count (1:5, -2)
%!error run_count (1:5, 3, ones (2,2))
%!error run_count (1:5, 3, 1.5)
%!error run_count (1:5, 3, 0)

