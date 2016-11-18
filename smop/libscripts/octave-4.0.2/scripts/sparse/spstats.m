## Copyright (C) 2004-2015 Paul Kienzle
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
## @deftypefn  {Function File} {[@var{count}, @var{mean}, @var{var}] =} spstats (@var{S})
## @deftypefnx {Function File} {[@var{count}, @var{mean}, @var{var}] =} spstats (@var{S}, @var{j})
## Return the stats for the nonzero elements of the sparse matrix @var{S}.
##
## @var{count} is the number of nonzeros in each column, @var{mean} is the mean
## of the nonzeros in each column, and @var{var} is the variance of the
## nonzeros in each column.
##
## Called with two input arguments, if @var{S} is the data and @var{j} is the
## bin number for the data, compute the stats for each bin.  In this case,
## bins can contain data values of zero, whereas with
## @code{spstats (@var{S})} the zeros may disappear.
## @end deftypefn

function [count, mean, var] = spstats (S, j)

  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif

  if (nargin == 1)
    [i, j, v] = find (S);
  else
    v = S;
    i = 1:length (v);
    S = sparse (i, j, v);
  endif
  [n, m] = size (S);

  count = sum (sparse (i, j, 1, n, m));
  if (nargout > 1)
    mean = sum (S) ./ count;
  endif
  if (nargout > 2)
    ## FIXME: Variance with count = 0 or 1?
    diff = S - sparse (i, j, mean (j), n, m);
    var = sum (diff .* diff) ./ (count - 1);
  endif

endfunction


%!test
%! [n,m,v] = spstats ([1 2 1 2 3 4],[2 2 1 1 1 1]);
%! assert (n, sparse ([4,2]));
%! assert (m, sparse ([10/4,3/2]), 10*eps);
%! assert (v, sparse ([5/3,1/2]), 10*eps);

