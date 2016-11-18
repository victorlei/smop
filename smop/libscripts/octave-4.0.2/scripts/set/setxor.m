## Copyright (C) 2014-2015 Julien Bect
## Copyright (C) 2008-2015 Jaroslav Hajek
## Copyright (C) 2000, 2006-2007 Paul Kienzle
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by the
## Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but WITHOUT
## ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
## FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
## for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <http://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn  {Function File} {@var{c} =} setxor (@var{a}, @var{b})
## @deftypefnx {Function File} {@var{c} =} setxor (@var{a}, @var{b}, "rows")
## @deftypefnx {Function File} {[@var{c}, @var{ia}, @var{ib}] =} setxor (@dots{})
##
## Return the unique elements exclusive to sets @var{a} or @var{b} sorted in
## ascending order.
##
## If @var{a} and @var{b} are both row vectors then return a row vector;
## Otherwise, return a column vector.  The inputs may also be cell arrays of
## strings.
##
## If the optional input @qcode{"rows"} is given then return the rows exclusive
## to sets @var{a} and @var{b}.  The inputs must be 2-D matrices to use this
## option.
##
## If requested, return index vectors @var{ia} and @var{ib} such that
## @code{@var{a}(@var{ia})} and @code{@var{b}(@var{ib})} are disjoint sets
## whose union is @var{c}.
##
## @seealso{unique, union, intersect, setdiff, ismember}
## @end deftypefn

function [c, ia, ib] = setxor (a, b, varargin)

  if (nargin < 2 || nargin > 3)
    print_usage ();
  endif

  [a, b] = validsetargs ("setxor", a, b, varargin{:});

  by_rows = nargin == 3;
  isrowvec = isvector (a) && isvector (b) && isrow (a) && isrow (b);

  ## Form A and B into sets.
  if (nargout > 1)
    [a, ia] = unique (a, varargin{:});
    [b, ib] = unique (b, varargin{:});
  else
    a = unique (a, varargin{:});
    b = unique (b, varargin{:});
  endif

  if (isempty (a))
    c = b;
  elseif (isempty (b))
    c = a;
  else
    ## Reject duplicates.
    if (by_rows)
      na = rows (a);  nb = rows (b);
      [c, i] = sortrows ([a; b]);
      n = rows (c);
      idx = find (all (c(1:n-1, :) == c(2:n, :), 2));
      if (! isempty (idx))
        c([idx, idx+1],:) = [];
        i([idx, idx+1],:) = [];
      endif
    else
      na = numel (a);  nb = numel (b);
      [c, i] = sort ([a(:); b(:)]);
      n = length (c);
      if (iscell (c))
        idx = find (strcmp (c(1:n-1), c(2:n)));
      else
        idx = find (c(1:n-1) == c(2:n));
      endif
      if (! isempty (idx))
        c([idx, idx+1]) = [];
        i([idx, idx+1]) = [];
      endif

      ## Adjust output orientation for Matlab compatibility
      if (isrowvec)
        c = c.';
      endif
    endif

    if (nargout > 1)
      ia = ia(i(i <= na));
      ib = ib(i(i > na) - na);
    endif
  endif

endfunction


%!assert (setxor ([1,2,3], [2,3,4]), [1,4])
%!assert (setxor ({'a'}, {'a', 'b'}), {'b'})

%!test
%! a = [3, 1, 4, 1, 5];
%! b = [1, 2, 3, 4];
%! [c, ia, ib] = setxor (a, b.');
%! assert (c, [2; 5]);
%! assert (c, sort ([a(ia)'; b(ib)']));

%!test
%! a = [1 2; 4 5; 1 3];
%! b = [1 1; 1 2; 4 5; 2 10];
%! [c, ia, ib] = setxor (a, b, "rows");
%! assert (c, [1 1; 1 3; 2 10]);
%! assert (c, sortrows ([a(ia,:); b(ib,:)]));

%!assert (setxor (1, []), 1)
%!assert (setxor ([], 1), 1)

%!test
%! [c, ia, ib] = setxor (1, []);
%! assert (c, 1);
%! assert (ia, 1);
%! assert (isempty (ib));

%!test
%! [c, ia, ib] = setxor ([], 1);
%! assert (c, 1);
%! assert (isempty (ia));
%! assert (ib, 1);

%!test
%! a = [2 1; 4 3];  b = [];
%! [c, ia, ib] = setxor (a, b);
%! assert (c, [1; 2; 3; 4]);
%! assert (ia, [3; 1; 4; 2]);
%! assert (isempty (ib));

%!test
%! a = [];  b = [2 1; 4 3];
%! [c, ia, ib] = setxor (a, b);
%! assert (c, [1; 2; 3; 4]);
%! assert (isempty (ia));
%! assert (ib, [3; 1; 4; 2]);
## Test orientation of output
%!shared x,y
%! x = 1:3;
%! y = 2:5;

%!assert (size (setxor (x, y)), [1 3])
%!assert (size (setxor (x', y)), [3 1])
%!assert (size (setxor (x, y')), [3 1])
%!assert (size (setxor (x', y')), [3 1])

## Test multi-dimensional arrays
%!test
%! a = rand (3,3,3);
%! b = a;
%! b(1,1,1) = 2;
%! assert (intersect (a, b), sort (a(2:end)'));

