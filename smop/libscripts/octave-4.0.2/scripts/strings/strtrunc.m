## Copyright (C) 2006-2015 William Poetra Yoga Hadisoeseno
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
## @deftypefn {Function File} {} strtrunc (@var{s}, @var{n})
## Truncate the character string @var{s} to length @var{n}.
##
## If @var{s} is a character matrix, then the number of columns is adjusted.
##
## If @var{s} is a cell array of strings, then the operation is performed
## on each cell element and the new cell array is returned.
## @end deftypefn

function s = strtrunc (s, n)

  if (nargin != 2)
    print_usage ();
  endif

  n = fix (n);
  if (! isscalar (n) || n < 0)
    error ("strtrunc: length N must be a positive integer (N >= 0)");
  endif

  if (ischar (s))
    if (n < columns (s))
      s = s(:, 1:n);
    endif
  elseif (iscellstr (s))
    ## Convoluted approach converts cellstr to char matrix, trims the character
    ## matrix using indexing, and then converts back to cellstr with mat2cell.
    ## This approach is 24X faster than using cellfun with call to strtrunc
    idx = cellfun ("size", s, 2) > n;
    rows = cellfun ("size", s(idx), 1);
    if (! isempty (rows))
      s(idx) = mat2cell (char (s(idx))(:, 1:n), rows);
    endif
  else
    error ("strtrunc: S must be a character string or a cell array of strings");
  endif

endfunction


%!assert (strtrunc ("abcdefg", 4), "abcd")
%!assert (strtrunc ("abcdefg", 10), "abcdefg")
%!assert (strtrunc (char ("abcdef", "fedcba"), 3), ["abc"; "fed"])
%!assert (strtrunc ({"abcdef", "fedcba"}, 3), {"abc", "fed"})
%!assert (strtrunc ({"", "1", "21", "321"}, 1), {"", "1", "2", "3"})
%!assert (strtrunc ({"1", "", "2"}, 1), {"1", "", "2"})
%!test
%! cstr = {"line1"; ["line2"; "line3"]; "line4"};
%! y = strtrunc (cstr, 4);
%! assert (size (y), [3, 1]);
%! assert (size (y{2}), [2, 4]);
%! assert (y{2}, repmat ("line", 2, 1));

## Test input validation
%!error strtrunc ()
%!error strtrunc ("abcd")
%!error strtrunc ("abcd", 4, 5)
%!error <N must be a positive integer> strtrunc ("abcd", ones (2,2))
%!error <N must be a positive integer> strtrunc ("abcd", -1)
%!error <S must be a character string or a cell array of strings> strtrunc (1, 1)

