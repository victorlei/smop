## Copyright (C) 2000-2015 Paul Kienzle
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
## @deftypefn  {Function File} {@var{c} =} setdiff (@var{a}, @var{b})
## @deftypefnx {Function File} {@var{c} =} setdiff (@var{a}, @var{b}, "rows")
## @deftypefnx {Function File} {[@var{c}, @var{ia}] =} setdiff (@dots{})
## Return the unique elements in @var{a} that are not in @var{b} sorted in
## ascending order.
##
## If @var{a} is a row vector return a column vector; Otherwise, return a
## column vector.  The inputs may also be cell arrays of strings.
##
## If the optional input @qcode{"rows"} is given then return the rows in
## @var{a} that are not in @var{b}.  The inputs must be 2-D matrices to use
## this option.
##
## If requested, return the index vector @var{ia} such that
## @code{@var{c} = @var{a}(@var{ia})}.
## @seealso{unique, union, intersect, setxor, ismember}
## @end deftypefn

## Author: Paul Kienzle
## Adapted-by: jwe

function [c, ia] = setdiff (a, b, varargin)

  if (nargin < 2 || nargin > 3)
    print_usage ();
  endif

  [a, b] = validsetargs ("setdiff", a, b, varargin{:});

  by_rows = nargin == 3;
  isrowvec = isvector (a) && isrow (a);

  if (by_rows)
    if (nargout > 1)
      [c, ia] = unique (a, "rows");
    else
      c = unique (a, "rows");
    endif
    if (! isempty (c) && ! isempty (b))
      ## Form A and B into combined set.
      b = unique (b, "rows");
      [tmp, idx] = sortrows ([c; b]);
      ## Eliminate those elements of A that are the same as in B.
      dups = find (all (tmp(1:end-1,:) == tmp(2:end,:), 2));
      c(idx(dups),:) = [];
      if (nargout > 1)
        ia(idx(dups),:) = [];
      endif
    endif
  else
    if (nargout > 1)
      [c, ia] = unique (a);
    else
      c = unique (a);
    endif
    if (! isempty (c) && ! isempty (b))
      ## Form a and b into combined set.
      b = unique (b);
      [tmp, idx] = sort ([c(:); b(:)]);
      ## Eliminate those elements of a that are the same as in b.
      if (iscellstr (tmp))
        dups = find (strcmp (tmp(1:end-1), tmp(2:end)));
      else
        dups = find (tmp(1:end-1) == tmp(2:end));
      endif
      c(idx(dups)) = [];
      if (nargout > 1)
        ia(idx(dups)) = [];
      endif
      ## Reshape if necessary for Matlab compatibility.
      if (isrowvec)
        c = c(:).';
      else
        c = c(:);
      endif
    endif
  endif

endfunction


%!assert (setdiff (["bb";"zz";"bb";"zz"], ["bb";"cc";"bb"], "rows"), "zz")
%!assert (setdiff (["b";"z";"b";"z"], ["b";"c";"b"], "rows"), "z")
%!assert (setdiff (["b";"z";"b";"z"], ["b";"c";"b"]), "z")
%!assert (setdiff ([1, 1; 2, 2; 3, 3; 4, 4], [1, 1; 2, 2; 4, 4], "rows"), [3 3])
%!assert (setdiff ([1; 2; 3; 4], [1; 2; 4], "rows"), 3)
%!assert (setdiff ([1, 2; 3, 4], [1, 2; 3, 6], "rows"), [3, 4])
%!assert (setdiff ({"one","two";"three","four"}, {"one","two";"three","six"}), {"four"})

%!test
%! a = [3, 1, 4, 1, 5];
%! b = [1, 2, 3, 4];
%! [c, ia] = setdiff (a, b');
%! assert (c, [5]);
%! assert (c, a(ia));

## Test output orientation compatibility (bug #42577)
%!assert (setdiff ([1:5], 2), [1,3,4,5])
%!assert (setdiff ([1:5]', 2), [1;3;4;5])
%!assert (setdiff ([1:5], [2:3]), [1,4,5])
%!assert (setdiff ([1:5], [2:3]'), [1,4,5])
%!assert (setdiff ([1:5]', [2:3]), [1;4;5])
%!assert (setdiff ([1:5]', [2:3]'), [1;4;5])

%!test
%! a = rand (3,3,3);
%! b = a(1);
%! assert (setdiff (a, b), sort (a(2:end)'));

