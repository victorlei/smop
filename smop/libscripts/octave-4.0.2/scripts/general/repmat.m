## Copyright (C) 2014-2015 Markus Bergholz
## Copyright (C) 2000-2015 Paul Kienzle
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
## @deftypefn  {Function File} {} repmat (@var{A}, @var{m})
## @deftypefnx {Function File} {} repmat (@var{A}, @var{m}, @var{n})
## @deftypefnx {Function File} {} repmat (@var{A}, @var{m}, @var{n}, @var{p} @dots{})
## @deftypefnx {Function File} {} repmat (@var{A}, [@var{m} @var{n}])
## @deftypefnx {Function File} {} repmat (@var{A}, [@var{m} @var{n} @var{p} @dots{}])
## Form a block matrix of size @var{m} by @var{n}, with a copy of matrix
## @var{A} as each element.
##
## If @var{n} is not specified, form an @var{m} by @var{m} block matrix.  For
## copying along more than two dimensions, specify the number of times to copy
## across each dimension @var{m}, @var{n}, @var{p}, @dots{}, in a vector in the
## second argument.
## @seealso{repelems}
## @end deftypefn

## Author: Paul Kienzle <pkienzle@kienzle.powernet.co.uk>
## Created: July 2000

function x = repmat (A, m, varargin)

  if (nargin < 2)
    print_usage ();
  endif

  if (nargin == 3)
    n = varargin{1};
    if (! isempty (m) && isempty (n))
      m = m(:).';
      n = 1;
    elseif (isempty (m) && ! isempty (n))
      m = n(:).';
      n = 1;
    elseif (isempty (m) && isempty (n))
      m = n = 1;
    else
      if (all (size (m) > 1))
        m = m(:,1);
        if (numel (m) < 3)
          n = n(end);
        else
          n = [];
        endif
      endif
      if (all (size (n) > 1))
        n = n(:,1);
      endif
      m = m(:).';
      n = n(:).';
    endif
  else
    if (nargin > 3)
      ## input check for m and varargin
      if (isscalar (m) && all (cellfun ("numel", varargin) == 1))
        m = [m varargin{:}];
        n = [];
      else
        error ("repmat: all input arguments must be scalar");
      end
    elseif (isempty (m))
      m = n = 1;
    elseif (isscalar (m))
      n = m;
    elseif (ndims (m) > 2)
      error ("repmat: M has more than 2 dimensions");
    elseif (all (size (m) > 1))
      m = m(:,1).';
      n = [];
    else
      m = m(:).';
      n = [];
    endif
  endif
  idx = [m, n];

  if (all (idx < 0))
    error ("repmat: invalid dimensions");
  else
    idx = max (idx, 0);
  endif

  if (numel (A) == 1)
    ## optimize the scalar fill case.
    if (any (idx == 0))
      x = resize (A, idx);
    else
      x(1:prod (idx)) = A;
      x = reshape (x, idx);
    endif
  elseif (ndims (A) == 2 && length (idx) < 3)
    if (issparse (A))
      x = kron (ones (idx), A);
    else
      ## indexing is now faster, so we use it rather than kron.
      m = rows (A); n = columns (A);
      p = idx(1); q = idx(2);
      x = reshape (A, m, 1, n, 1);
      x = x(:, ones (1, p), :, ones (1, q));
      x = reshape (x, m*p, n*q);
    endif
  else
    aidx = size (A);
    ## ensure matching size
    idx(end+1:length (aidx)) = 1;
    aidx(end+1:length (idx)) = 1;
    ## create subscript array
    cidx = cell (2, length (aidx));
    for i = 1:length (aidx)
      cidx{1,i} = ':';
      cidx{2,i} = ones (1, idx (i));
    endfor
    aaidx = aidx;
    ## add singleton dims
    aaidx(2,:) = 1;
    A = reshape (A, aaidx(:));
    x = reshape (A (cidx{:}), idx .* aidx);
  endif

endfunction


## Tests for ML compatibility
%!shared x
%! x = [1 2 3];
%!assert (repmat (x, [3, 1]), repmat (x, 3, []))
%!assert (repmat (x, [3, 1]), repmat (x, [], 3))
%!assert (repmat (x, [1, 3]), repmat (x, [], [1, 3]))
%!assert (repmat (x, [1, 3]), repmat (x, [1, 3], []))
%!assert (repmat (x, [1 3]), repmat (x, [1 3; 3 3]))
%!assert (repmat (x, [1 1 2]), repmat (x, [1 1; 1 3; 2 1]))
%!assert (repmat (x, [1 3; 1 3], [1; 3]), repmat (x, [1 1 3]))
%!assert (repmat (x, [1 1], 4), repmat (x, [1 3; 1 3], [1; 4]))
%!assert (repmat (x, [1 1], 4), repmat (x, [1 3; 1 3], [1 2; 3 4]))
%!assert (repmat (x, [1 1], 4), repmat (x, [1 1 4]));
%!assert (repmat (x, [1 1], 4), repmat (x, 1, [1 4]));

## Test various methods of providing size parameters
%!shared x
%! x = [1 2;3 4];
%!assert (repmat (x, [1 1]), repmat (x, 1))
%!assert (repmat (x, [3 3]), repmat (x, 3))
%!assert (repmat (x, [1 1]), repmat (x, 1, 1))
%!assert (repmat (x, [1 3]), repmat (x, 1, 3))
%!assert (repmat (x, [3 1]), repmat (x, 3, 1))
%!assert (repmat (x, [3 3]), repmat (x, 3, 3))
%!assert (repmat (pi, [1,2,3,4]), repmat (pi, 1,2,3,4))

## Tests for numel==1 case:
%!shared x, r
%! x = [ 65 ];
%! r = kron (ones (2,2), x);
%!assert (r, repmat (x, [2 2]))
%!assert (char (r), repmat (char (x), [2 2]))
%!assert (int8 (r), repmat (int8 (x), [2 2]))

## Tests for ndims==2 case:
%!shared x, r
%! x = [ 65 66 67 ];
%! r = kron (ones (2,2), x);
%!assert (r, repmat (x, [2 2]))
%!assert (char (r), repmat (char (x), [2 2]))
%!assert (int8 (r), repmat (int8 (x), [2 2]))

## Tests for dim>2 case:
%!shared x, r
%! x = [ 65 66 67 ];
%! r = kron (ones (2,2), x);
%! r(:,:,2) = r(:,:,1);
%!assert (r, repmat (x, [2 2 2]))
%!assert (char (r), repmat (char (x), [2 2 2]))
%!assert (int8 (r), repmat (int8 (x), [2 2 2]))

## Test that sparsity is kept
%!assert (sparse (4,4), repmat (sparse (2,2),[2 2]))

%!assert (size (repmat (".", -1, 1)), [0, 1])
%!assert (size (repmat (".", 1, -1)), [1, 0])

%!assert (size (repmat (1, [1, 0])), [1, 0])
%!assert (size (repmat (1, [5, 0])), [5, 0])
%!assert (size (repmat (1, [0, 1])), [0, 1])
%!assert (size (repmat (1, [0, 5])), [0, 5])

%!shared x
%! x = struct ("a", [], "b", []);
%!assert (size (repmat (x, [1, 0])), [1, 0])
%!assert (size (repmat (x, [5, 0])), [5, 0])
%!assert (size (repmat (x, [0, 1])), [0, 1])
%!assert (size (repmat (x, [0, 5])), [0, 5])

%!assert (size (repmat ({1}, [1, 0])), [1, 0])
%!assert (size (repmat ({1}, [5, 0])), [5, 0])
%!assert (size (repmat ({1}, [0, 1])), [0, 1])
%!assert (size (repmat ({1}, [0, 5])), [0, 5])

%!error (size (repmat (".", -1, -1)))

