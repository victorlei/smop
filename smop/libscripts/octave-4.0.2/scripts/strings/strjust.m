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
## @deftypefn  {Function File} {} strjust (@var{s})
## @deftypefnx {Function File} {} strjust (@var{s}, @var{pos})
## Return the text, @var{s}, justified according to @var{pos}, which may
## be @qcode{"left"}, @qcode{"center"}, or @qcode{"right"}.
##
## If @var{pos} is omitted it defaults to @qcode{"right"}.
##
## Null characters are replaced by spaces.  All other character data are
## treated as non-white space.
##
## Example:
##
## @example
## @group
## strjust (["a"; "ab"; "abc"; "abcd"])
##      @result{}
##         "   a"
##         "  ab"
##         " abc"
##         "abcd"
## @end group
## @end example
## @seealso{deblank, strrep, strtrim, untabify}
## @end deftypefn

function y = strjust (s, pos = "right")

  if (nargin < 1 || nargin > 2)
    print_usage ();
  elseif (! ischar (s) || ndims (s) > 2)
    error ("strjust: S must be a string or 2-D character matrix");
  endif

  if (isempty (s))
    y = s;
    return;
  endif

  ## Apparently, Matlab considers nulls to be blanks as well; however, does
  ## not preserve the nulls, but rather converts them to blanks.  That's a
  ## bit unexpected, but it allows simpler processing, because we can move
  ## just the nonblank characters. So we'll do the same here.

  [nr, nc] = size (s);
  ## Find the indices of all nonblanks.
  nonbl = s != " " & s != "\0";
  [idx, jdx] = find (nonbl);

  if (strcmpi (pos, "right"))
    ## We wish to find the maximum column index for each row. Because jdx is
    ## sorted, we can take advantage of the fact that assignment is processed
    ## sequentially and for duplicate indices the last value will remain.
    maxs = repmat (nc, [nr, 1]);
    maxs(idx) = jdx;
    shift = nc - maxs;
  elseif (strcmpi (pos, "left"))
    ## See above for explanation.
    mins = ones (nr, 1);
    mins(flipud (idx(:))) = flipud (jdx(:));
    shift = 1 - mins;
  else
    ## Use both of the above to achieve centering.
    mins = ones (nr, 1);
    mins(flipud (idx(:))) = flipud (jdx(:));
    maxs = repmat (nc, [nr, 1]);
    maxs(idx) = jdx;
    shift = floor ((nc + 1 - maxs - mins) / 2);
  endif

  ## Adjust the column indices.
  jdx += shift (idx);

  ## Create a blank matrix and position the nonblank characters.
  y = repmat (" ", nr, nc);
  y(sub2ind ([nr, nc], idx, jdx)) = s(nonbl);

endfunction


%!assert (strjust (["a"; "ab"; "abc"; "abcd"]),
%!        ["   a";"  ab"; " abc"; "abcd"])
%!assert (strjust ([" a"; "  ab"; "abc"; "abcd"], "left"),
%!        ["a   "; "ab  "; "abc "; "abcd"])
%!assert (strjust (["a"; "ab"; "abc"; "abcd"], "CENTER"),
%!        [" a  "; " ab"; "abc "; "abcd"])
%!assert (strjust (["";""]), "")

## Test input validation
%!error <Invalid call to strjust> strjust ()
%!error <Invalid call to strjust> strjust (["a";"ab"], "center", 1)
%!error <S must be a string> strjust (ones (3,3))
%!error <S must be a string> strjust (char (ones (3,3,3)))

