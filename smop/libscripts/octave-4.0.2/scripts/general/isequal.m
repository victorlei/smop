## Copyright (C) 2005-2015 William Poetra Yoga Hadisoeseno
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
## @deftypefn {Function File} {} isequal (@var{x1}, @var{x2}, @dots{})
## Return true if all of @var{x1}, @var{x2}, @dots{} are equal.
## @seealso{isequaln}
## @end deftypefn

function retval = isequal (x1, varargin)

  if (nargin < 2)
    print_usage ();
  endif

  retval = __isequal__ (false, x1, varargin{:});

endfunction


## test empty input
%!assert (isequal ([], []), true)
%!assert (isequal ([], [], 1), false)
%!assert (isequal ([], 1, []), false)
%!assert (isequal (1, [], []), false)
%!assert (isequal (1, [], []), false)

## test size and shape
%!assert (isequal ([1,2,3,4], [1,2,3,4]), true)
%!assert (isequal ([1;2;3;4], [1;2;3;4]), true)
%!assert (isequal ([1,2,3,4], [1;2;3;4]), false)
%!assert (isequal ([1,2,3,4], [1,2;3,4]), false)
%!assert (isequal ([1,2,3,4], [1,3;2,4]), false)

%!test
%! A = 1:8;
%! B = reshape (A, 2, 2, 2);
%! assert (isequal (A, B), false);
%!test
%! A = reshape (1:8, 2, 2, 2);
%! B = A;
%! assert (isequal (A, B), true);
%!test
%! A = reshape (1:8, 2, 4);
%! B = reshape (A, 2, 2, 2);
%! assert (isequal (A, B), false);

## test all numeric built-in primitives
%!assert (isequal (false, logical (0), char (0),
%!                 int8 (0), int16 (0), int32 (0), int64 (0),
%!                 uint8 (0), uint16 (0), uint32 (0), uint64 (0),
%!                 double (0), single (0),
%!                 double (complex (0,0)), single (complex (0,0))),
%!        true)
%!assert (isequal (true, logical (1), char (1),
%!                 int8 (1), int16 (1), int32 (1), int64 (1),
%!                 uint8 (1), uint16 (1), uint32 (1), uint64 (1),
%!                 double (1), single (1),
%!                 double (complex (1,0)), single (complex (1,0))),
%!        true)

## test characters and strings
%!assert (isequal ('a', "a"), true)
%!assert (isequal ("abab", ["a", "b", "a", "b"]), true)
%!assert (isequal (["a","b","c","d"], ["a","b","c","d"]), true)
%!assert (isequal (["test   ";"strings"], ["test   ";"strings"],
%!                 ["test   ";"strings"]), true)
%!assert (isequal (["a","b","c","d"], ["a";"b";"c";"d"]), false)

## test function_handle
%!test
%! fcn = @(x) x.^2;
%! assert (isequal (fcn, fcn), true);
%! assert (isequal (fcn, @(x) x.^2), false);
%! assert (isequal (@(x) x.^2, fcn), false);

## test structures
%!assert (isequal (struct ([]),struct ([])), true)
%!assert (isequal (struct ("a",1), struct ("a",1)), true)
%!assert (isequal (struct ("a",1), struct ("a",2)), false)
%!assert (isequal (struct ("a",1), struct ("b",1)), false)
%!assert (isequal (struct ("a",1,"b",2), struct ("a",1,"b",2)), true)
%!assert (isequal (struct ("a",1,"b",2), struct ("b",2,"a",1)), true)
%!assert (isequal (struct ("a",1,"b",2), struct ("a",1,"b",2),
%!                 struct ("a",1,"b",2)), true)
%!assert (isequal (struct ("a","abc","b",2), struct ("a","abc","b",2)), true)

## recursive structure
%!test
%! x.a = "a1";
%! x.b.a = "ba1";
%! x.b.b = "bb1";
%! assert (isequal (x, x, x), true);
%! y = x;
%! y.b.b = "bb2";
%! assert (isequal (x, y), false);
%! y = x;
%! y.b = rmfield (y.b, "b");
%! y.b.b.a = "bba1";
%! assert (isequal (x, y), false);

## test cells
%!assert (isequal (cell (1,1), cell (1,1)), true)
%!assert (isequal (cell (1,1), cell (1,2)), false)
%!assert (isequal ({"a",1}, {"a",1}), true)
%!assert (isequal ({"a",1}, {"a",2}), false)
%!assert (isequal ({"a",1}, {"b",1}), false)
%!assert (isequal ({"a",1,"b",2}, {"a",1,"b",2}), true)
%!assert (isequal ({"a",1,"b",2}, {"b",2,"a",1}), false)
%!assert (isequal ({"a",1,"b",2}, {"a",1,"b",2}, {"a",1,"b",2}), true)
%!assert (isequal ({"a","abc","b",2}, {"a","abc","b",2}), true)
%!assert (isequal ({"a","b","c","d"}, {"a","b","c","d"}), true)
%!assert (isequal ({"a","b","c","d"}, {"a";"b";"c";"d"}), false)
%!assert (isequal (["a","b","c","d"], {"a","b","c","d"}), false)

## recursive cell
%!test
%! x = cell (1,3);
%! x{1} = {[1], [1 2]};
%! x{2} = true;
%! x{3} = {{"hello"}, {"world"}};
%! assert (isequal (x, x));
%! y = x;
%! y{3}{1}{1} = "goodbye";
%! assert (isequal (x, y), false);

## test for sparse matrices
%!assert (isequal (sparse ([]), []), true)
%!assert (isequal ([], sparse ([])), true)
%!assert (isequal (sparse (0,1), sparse (0,1)), true)
%!assert (isequal (sparse (0,1), zeros (0,1)), true)
%!assert (isequal (sparse (2,2), sparse (2,2)), true)
%!assert (isequal (zeros (2,2), sparse (2,2)), true)
%!assert (isequal (speye (1), eye (1)), true)
%!assert (isequal (eye (300), speye (300)), true)
%!assert (isequal (sparse (0,1), sparse (1,0)), false)

## test NaN
%!assert (isequal (NaN, NaN), false)
%!assert (isequal (NaN, Inf), false)
%!assert (isequal (NaN, 1.0), false)
%!assert (isequal ([1,2,NaN,4], [1,2,NaN,4]), false)
%!assert (isequal (struct ("a",NaN,"b",2), struct ("a",NaN,"b",2),
%!                 struct ("a",NaN,"b",2)), false)

## test input validation
%!error isequal ()
%!error isequal (1)
%!error isequal ([1,1])

