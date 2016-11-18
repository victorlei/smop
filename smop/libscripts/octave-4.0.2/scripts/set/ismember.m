## Copyright (C) 2000-2015 Paul Kienzle
## Copyright (C) 2009 Jaroslav Hajek
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
## @deftypefn  {Function File} {@var{tf} =} ismember (@var{a}, @var{s})
## @deftypefnx {Function File} {@var{tf} =} ismember (@var{a}, @var{s}, "rows")
## @deftypefnx {Function File} {[@var{tf}, @var{s_idx}] =} ismember (@dots{})
##
## Return a logical matrix @var{tf} with the same shape as @var{a} which is
## true (1) if the element in @var{a} is found in @var{s} and false (0) if it
## is not.
##
## If a second output argument is requested then the index into @var{s} of each
## matching element is also returned.
##
## @example
## @group
## a = [3, 10, 1];
## s = [0:9];
## [tf, s_idx] = ismember (a, s)
##      @result{} tf = [1, 0, 1]
##      @result{} s_idx = [4, 0, 2]
## @end group
## @end example
##
## The inputs @var{a} and @var{s} may also be cell arrays.
##
## @example
## @group
## a = @{"abc"@};
## s = @{"abc", "def"@};
## [tf, s_idx] = ismember (a, s)
##      @result{} tf = [1, 0]
##      @result{} s_idx = [1, 0]
## @end group
## @end example
##
## If the optional third argument @qcode{"rows"} is given then compare rows
## in @var{a} with rows in @var{s}.  The inputs must be 2-D matrices with the
## same number of columns to use this option.
##
## @example
## @group
## a = [1:3; 5:7; 4:6];
## s = [0:2; 1:3; 2:4; 3:5; 4:6];
## [tf, s_idx] = ismember (a, s, "rows")
##      @result{} tf = logical ([1; 0; 1])
##      @result{} s_idx = [2; 0; 5];
## @end group
## @end example
##
## @seealso{lookup, unique, union, intersect, setdiff, setxor}
## @end deftypefn

## Author: Paul Kienzle <pkienzle@users.sf.net>
## Author: Søren Hauberg <hauberg@gmail.com>
## Author: Ben Abbott <bpabbott@mac.com>
## Adapted-by: jwe
## Reimplemented using lookup & unique: Jaroslav Hajek <highegg@gmail.com>

function [tf, s_idx] = ismember (a, s, varargin)

  if (nargin < 2 || nargin > 3)
    print_usage ();
  endif

  ## lookup() does not handle logical values
  if (islogical (a))
    a = uint8 (a);
  endif
  if (islogical (s))
    s = uint8 (s);
  endif

  [a, s] = validsetargs ("ismember", a, s, varargin{:});

  by_rows = nargin == 3;

  if (! by_rows)
    s = s(:);
    ## Check sort status, because we expect the array will often be sorted.
    if (issorted (s))
      is = [];
    else
      [s, is] = sort (s);
    endif

    ## Remove NaNs from table because lookup can't handle them
    if (isreal (s) && ! isempty (s) && isnan (s(end)))
      s = s(1:end - sum (isnan (s)));
    endif

    if (nargout > 1)
      s_idx = lookup (s, a, "m");
      tf = logical (s_idx);
      if (! isempty (is))
        s_idx(tf) = is(s_idx(tf));
      endif
    else
      tf = lookup (s, a, "b");
    endif

  else  # "rows" argument
    if (isempty (a) || isempty (s))
      tf = false (rows (a), 1);
      s_idx = zeros (rows (a), 1);
    else
      if (rows (s) == 1)
        tf = all (bsxfun (@eq, a, s), 2);
        s_idx = double (tf);
      else
        ## FIXME: lookup does not support "rows", so we just use unique.
        [~, ii, jj] = unique ([a; s], "rows", "last");
        na = rows (a);
        jj = ii(jj(1:na));
        tf = jj > na;

        if (nargout > 1)
          s_idx = max (0, jj - na);
        endif
      endif
    endif
  endif

endfunction


%!assert (ismember ({""}, {"abc", "def"}), false)
%!assert (ismember ("abc", {"abc", "def"}), true)
%!assert (isempty (ismember ([], [1, 2])), true)
%!assert (isempty (ismember ({}, {'a', 'b'})), true)
%!assert (ismember ("", {"abc", "def"}), false)
%!fail ("ismember ([], {1, 2})")
%!fail ("ismember ({[]}, {1, 2})")
%!fail ("ismember ({}, {1, 2})")
%!fail ("ismember ({1}, {'1', '2'})")
%!fail ("ismember (1, 'abc')")
%!fail ("ismember ({'1'}, {'1' '2'},'rows')")
%!fail ("ismember ([1 2 3], [5 4 3 1], 'rows')")
%!assert (ismember ({"foo", "bar"}, {"foobar"}), [false false])
%!assert (ismember ({"foo"}, {"foobar"}), false)
%!assert (ismember ({"bar"}, {"foobar"}), false)
%!assert (ismember ({"bar"}, {"foobar", "bar"}), true)
%!assert (ismember ({"foo", "bar"}, {"foobar", "bar"}), [false true])
%!assert (ismember ({"xfb", "f", "b"}, {"fb", "b"}), [false false true])
%!assert (ismember ("1", "0123456789."), true)

%!test
%! [result, s_idx] = ismember ([1, 2], []);
%! assert (result, [false false])
%! assert (s_idx, [0, 0]);

%!test
%! [result, s_idx] = ismember ([], [1, 2]);
%! assert (result, logical ([]))
%! assert (s_idx, []);

%!test
%! [result, s_idx] = ismember ({"a", "b"}, "");
%! assert (result, [false false])
%! assert (s_idx, [0, 0]);

%!test
%! [result, s_idx] = ismember ({"a", "b"}, {});
%! assert (result, [false false])
%! assert (s_idx, [0, 0]);

%!test
%! [result, s_idx] = ismember ("", {"a", "b"});
%! assert (result, false)
%! assert (s_idx, 0);

%!test
%! [result, s_idx] = ismember ({}, {"a", "b"});
%! assert (result, logical ([]))
%! assert (s_idx, []);

%!test
%! [result, s_idx] = ismember ([1 2 3 4 5], [3]);
%! assert (result, logical ([0 0 1 0 0]))
%! assert (s_idx , [0 0 1 0 0]);

%!test
%! [result, s_idx] = ismember ([1 6], [1 2 3 4 5 1 6 1]);
%! assert (result, [true true]);
%! assert (s_idx(2), 7);

%!test
%! [result, s_idx] = ismember ([3,10,1], [0,1,2,3,4,5,6,7,8,9]);
%! assert (result, [true false true]);
%! assert (s_idx, [4, 0, 2]);

%!test
%! [result, s_idx] = ismember ("1.1", "0123456789.1");
%! assert (result, [true true true]);
%! assert (s_idx, [12, 11, 12]);

%!test
%! [result, s_idx] = ismember ([1:3; 5:7; 4:6], [0:2; 1:3; 2:4; 3:5; 4:6], "rows");
%! assert (result, [true; false; true]);
%! assert (s_idx, [2; 0; 5]);

%!test
%! [result, s_idx] = ismember ([1.1,1.2,1.3; 2.1,2.2,2.3; 10,11,12], [1.1,1.2,1.3; 10,11,12; 2.12,2.22,2.32], "rows");
%! assert (result, [true; false; true]);
%! assert (s_idx, [1; 0; 2]);

%!test
%! [result, s_idx] = ismember ([1:3; 5:7; 4:6; 0:2; 1:3; 2:4], [1:3], "rows");
%! assert (result, logical ([1 0 0 0 1 0]'));
%! assert (s_idx, [1 0 0 0 1 0]');

