## Copyright (C) 2010-2015 Jaroslav Hajek
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
## @deftypefn  {Function File} {} powerset (@var{a})
## @deftypefnx {Function File} {} powerset (@var{a}, "rows")
## Compute the powerset (all subsets) of the set @var{a}.
##
## The set @var{a} must be a numerical matrix or a cell array of strings.  The
## output will always be a cell array of either vectors or strings.
##
## With the optional argument @qcode{"rows"}, each row of the set @var{a} is
## considered one element of the set.  The input must be a 2-D numeric matrix
## to use this argument.
##
## @seealso{unique, union, intersect, setdiff, setxor, ismember}
## @end deftypefn

function p = powerset (a, byrows_arg)

  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif

  byrows = false;
  if (nargin == 2)
    if (! strcmpi (byrows_arg, "rows"))
      error ('powerset: expecting second argument to be "rows"');
    elseif (iscell (a))
      error ('powerset: "rows" not valid for cell arrays');
    else
      byrows = true;
    endif
  endif

  if (iscell (a) && ! iscellstr (a))
    error ("powerset: cell arrays can only be used for character strings");
  endif

  if (byrows)
    a = unique (a, "rows");
    n = rows (a);
  else
    a = unique (a);
    n = numel (a);
  endif

  if (n == 0)
    p = {};
  else
    if (n > 32)
      error ("powerset: not implemented for more than 32 elements");
    endif

    ## Logical rep
    b = reshape (bitunpack (uint32 (0:2^n-1)), 32, 2^n)(1:n,:);
    ## Convert to indices and lengths.
    [i, k] = find (b);
    k = sum (b, 1);

    ## Index and split.
    if (byrows)
      p = mat2cell (a(i,:), k, columns (a));
    else
      if (rows (a) == 1)
        p = mat2cell (a(i), 1, k);
      else
        p = mat2cell (a(i), k, 1);
      endif
    endif
  endif

endfunction


%!test
%! c = sort (cellstr ({[], [1], [2], [3], [1, 2], [1, 3], [2, 3], [1, 2, 3]}));
%! p = sort (cellstr (powerset ([1, 2, 3])));
%! assert (p, c);

%!test
%! c = sort (cellstr ({ [], [1:3], [2:4], [3:5], [1:3; 2:4], [1:3; 3:5], [2:4; 3:5], [1:3; 2:4; 3:5]}));
%! p = sort (cellstr (powerset ([1:3;2:4;3:5], "rows")));
%! assert (p,c);

%!assert (powerset([]), {});  # always return a cell array

## Test input validation
%!error powerset ()
%!error powerset (1,2,3)
%!error <expecting second argument to be "rows"> powerset (1, "cols")
%!error <"rows" not valid for cell arrays> powerset ({1}, "rows")
%!error <cell arrays can only be used for character> powerset ({1})
%!error <not implemented for more than 32 elements> powerset (1:33)

