## Copyright (C) 2009-2015 Søren Hauberg
## Copyright (C) 2009 VZLU Prague
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
## @deftypefn  {Function File} {@var{n} =} histc (@var{x}, @var{edges})
## @deftypefnx {Function File} {@var{n} =} histc (@var{x}, @var{edges}, @var{dim})
## @deftypefnx {Function File} {[@var{n}, @var{idx}] =} histc (@dots{})
## Compute histogram counts.
##
## When @var{x} is a vector, the function counts the number of elements of
## @var{x} that fall in the histogram bins defined by @var{edges}.  This must be
## a vector of monotonically increasing values that define the edges of the
## histogram bins.  @code{@var{n}(k)} contains the number of elements in
## @var{x} for which @code{@var{edges}(k) <= @var{x} < @var{edges}(k+1)}.
## The final element of @var{n} contains the number of elements of @var{x}
## exactly equal to the last element of @var{edges}.
##
## When @var{x} is an @math{N}-dimensional array, the computation is carried
## out along dimension @var{dim}.  If not specified @var{dim} defaults to the
## first non-singleton dimension.
##
## When a second output argument is requested an index matrix is also returned.
## The @var{idx} matrix has the same size as @var{x}.  Each element of @var{idx}
## contains the index of the histogram bin in which the corresponding element
## of @var{x} was counted.
## @seealso{hist}
## @end deftypefn

function [n, idx] = histc (x, edges, dim)

  if (nargin < 2 || nargin > 3)
    print_usage ();
  endif

  if (! isreal (x))
    error ("histc: X argument must be real-valued, not complex");
  endif

  num_edges = numel (edges);
  if (num_edges == 0)
    error ("histc: EDGES must not be empty");
  endif

  if (! isreal (edges))
    error ("histc: EDGES must be real-valued, not complex");
  else
    ## Make sure 'edges' is sorted
    edges = edges(:);
    if (! issorted (edges) || edges(1) > edges(end))
      warning ("histc: edge values not sorted on input");
      edges = sort (edges);
    endif
  endif

  nd = ndims (x);
  sz = size (x);
  if (nargin < 3)
    ## Find the first non-singleton dimension.
    (dim = find (sz > 1, 1)) || (dim = 1);
  else
    if (!(isscalar (dim) && dim == fix (dim))
        || !(1 <= dim && dim <= nd))
      error ("histc: DIM must be an integer and a valid dimension");
    endif
  endif

  nsz = sz;
  nsz(dim) = num_edges;

  ## the splitting point is 3 bins

  if (num_edges <= 3)

    ## This is the O(M*N) algorithm.

    ## Allocate the histogram
    n = zeros (nsz);

    ## Allocate 'idx'
    if (nargout > 1)
      idx = zeros (sz);
    endif

    ## Prepare indices
    idx1 = cell (1, dim-1);
    for k = 1:length (idx1)
      idx1{k} = 1:sz(k);
    endfor
    idx2 = cell (length (sz) - dim);
    for k = 1:length (idx2)
      idx2{k} = 1:sz(k+dim);
    endfor

    ## Compute the histograms
    for k = 1:num_edges-1
      b = (edges(k) <= x & x < edges(k+1));
      n(idx1{:}, k, idx2{:}) = sum (b, dim);
      if (nargout > 1)
        idx(b) = k;
      endif
    endfor
    b = (x == edges(end));
    n(idx1{:}, num_edges, idx2{:}) = sum (b, dim);
    if (nargout > 1)
      idx(b) = num_edges;
    endif

  else

    ## This is the O(M*log(N) + N) algorithm.

    ## Look-up indices.
    idx = lookup (edges, x);
    ## Zero invalid ones (including NaNs). x < edges(1) are already zero.
    idx(! (x <= edges(end))) = 0;

    iidx = idx;

    ## In case of matrix input, we adjust the indices.
    if (! isvector (x))
      nl = prod (sz(1:dim-1));
      nn = sz(dim);
      nu = prod (sz(dim+1:end));
      if (nl != 1)
        iidx = (iidx-1) * nl;
        iidx += reshape (kron (ones (1, nn*nu), 1:nl), sz);
      endif
      if (nu != 1)
        ne =length (edges);
        iidx += reshape (kron (nl*ne*(0:nu-1), ones (1, nl*nn)), sz);
      endif
    endif

    ## Select valid elements.
    iidx = iidx(idx != 0);

    ## Call accumarray to sum the indexed elements.
    n = accumarray (iidx(:), 1, nsz);

  endif

endfunction


%!test
%! x = linspace (0, 10, 1001);
%! n = histc (x, 0:10);
%! assert (n, [repmat(100, 1, 10), 1]);

%!test
%! x = repmat (linspace (0, 10, 1001), [2, 1, 3]);
%! n = histc (x, 0:10, 2);
%! assert (n, repmat ([repmat(100, 1, 10), 1], [2, 1, 3]));

%!error histc ()
%!error histc (1)
%!error histc (1, 2, 3, 4)
%!error histc ([1:10 1+i], 2)
%!error histc (1:10, [])
%!error histc (1, 1, 3)

