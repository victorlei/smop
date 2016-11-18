## Copyright (C) 1994-2015 John W. Eaton
## Copyright (C) 2008-2009 Jaroslav Hajek
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
## @deftypefn  {Function File} {@var{c} =} union (@var{a}, @var{b})
## @deftypefnx {Function File} {@var{c} =} union (@var{a}, @var{b}, "rows")
## @deftypefnx {Function File} {[@var{c}, @var{ia}, @var{ib}] =} union (@dots{})
##
## Return the unique elements that are in either @var{a} or @var{b} sorted in
## ascending order.
##
## If @var{a} and @var{b} are both row vectors then return a row vector;
## Otherwise, return a column vector.  The inputs may also be cell arrays of
## strings.
##
## If the optional input @qcode{"rows"} is given then return rows that are in
## either @var{a} or @var{b}.  The inputs must be 2-D matrices to use this
## option.
##
## The optional outputs @var{ia} and @var{ib} are index vectors such that
## @code{@var{a}(@var{ia})} and @code{@var{b}(@var{ib})} are disjoint sets
## whose union is @var{c}.
##
## @seealso{unique, intersect, setdiff, setxor, ismember}
## @end deftypefn

## Author: jwe

function [y, ia, ib] = union (a, b, varargin)

  if (nargin < 2 || nargin > 3)
    print_usage ();
  endif

  [a, b] = validsetargs ("union", a, b, varargin{:});

  by_rows = nargin == 3;
  isrowvec = isvector (a) && isvector (b) && isrow (a) && isrow (b);

  if (by_rows)
    y = [a; b];
  else
    y = [a(:); b(:)];
    ## Adjust output orientation for Matlab compatibility
    if (isrowvec)
      y = y.';
    endif
  endif

  if (nargout <= 1)
    y = unique (y, varargin{:});
  else
    [y, idx] = unique (y, varargin{:});
    na = numel (a);
    ia = idx(idx <= na);
    ib = idx(idx > na) - na;
  endif

endfunction


%!assert (union ([1, 2, 4], [2, 3, 5]), [1, 2, 3, 4, 5])
%!assert (union ([1; 2; 4], [2, 3, 5]), [1; 2; 3; 4; 5])
%!assert (union ([1; 2; 4], [2; 3; 5]), [1; 2; 3; 4; 5])
%!assert (union ([1, 2, 3], [5; 7; 9]), [1; 2; 3; 5; 7; 9])

## Test multi-dimensional arrays
%!test
%! a = rand (3,3,3);
%! b = a;
%! b(1,1,1) = 2;
%! assert (union (a, b), sort ([a(1:end)'; 2]));

%!test
%! a = [3, 1, 4, 1, 5];
%! b = [1, 2, 3, 4];
%! [y, ia, ib] = union (a, b.');
%! assert (y, [1; 2; 3; 4; 5]);
%! assert (y, sort ([a(ia)'; b(ib)']));

## Test common input validation for set routines contained in validsetargs
%!error <cell array of strings cannot be combined> union ({"a"}, 1)
%!error <A and B must be arrays or cell arrays> union (@sin, 1)
%!error <invalid option: columns> union (1, 2, "columns")
%!error <cells not supported with "rows"> union ({"a"}, {"b"}, "rows")
%!error <A and B must be arrays or cell arrays> union (@sin, 1, "rows")
%!error <A and B must be 2-dimensional matrices> union (rand(2,2,2), 1, "rows")
%!error <number of columns in A and B must match> union ([1 2], 1, "rows")

