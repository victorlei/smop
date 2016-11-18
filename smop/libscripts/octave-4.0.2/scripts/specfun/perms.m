## Copyright (C) 2001-2015 Paul Kienzle
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
## @deftypefn {Function File} {} perms (@var{v})
## Generate all permutations of @var{v} with one row per permutation.
##
## The result has size @code{factorial (@var{n}) * @var{n}}, where @var{n}
## is the length of @var{v}.
##
## Example
##
## @example
## @group
## perms ([1, 2, 3])
## @result{}
##   1   2   3
##   2   1   3
##   1   3   2
##   2   3   1
##   3   1   2
##   3   2   1
## @end group
## @end example
##
## Programming Note: The maximum length of @var{v} should be less than or
## equal to 10 to limit memory consumption.
## @seealso{permute, randperm, nchoosek}
## @end deftypefn

function A = perms (v)

  if (nargin != 1)
    print_usage ();
  endif

  vidx = uint8 ([1:length(v)]');
  n = length (vidx);

  if (n == 0)
    p = [];
  else
    p = vidx(1);
    for j = 2:n
      B = p;
      p = zeros (prod (2:j), n, "uint8");
      k = rows (B);
      idx = 1:k;
      for i = j:-1:1
        p(idx,1:i-1) = B(:,1:i-1);
        p(idx,i) = vidx(j);
        p(idx,i+1:j) = B(:,i:j-1);
        idx += k;
      endfor
    endfor
  endif

  A = v(p);

endfunction


%!assert (perms ([1,2,3]), [1,2,3;2,1,3;1,3,2;2,3,1;3,1,2;3,2,1])
%!assert (perms ("abc"), ["abc"; "bac"; "acb"; "bca"; "cab"; "cba"])
%!assert (perms (int8 ([1,2,3])), int8 ([1,2,3;2,1,3;1,3,2;2,3,1;3,1,2;3,2,1]))

%!error perms ()
%!error perms (1, 2)

