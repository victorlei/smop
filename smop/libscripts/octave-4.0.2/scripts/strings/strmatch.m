## Copyright (C) 2000-2013 Paul Kienzle
## Copyright (C) 2003 Alois Schloegl
## Copyright (C) 2010 VZLU Prague
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
## @deftypefn  {Function File} {} strmatch (@var{s}, @var{A})
## @deftypefnx {Function File} {} strmatch (@var{s}, @var{A}, "exact")
## Return indices of entries of @var{A} which begin with the string @var{s}.
##
## The second argument @var{A} must be a string, character matrix, or a cell
## array of strings.
##
## If the third argument @qcode{"exact"} is not given, then @var{s} only
## needs to match @var{A} up to the length of @var{s}.  Trailing spaces and
## nulls in @var{s} and @var{A} are ignored when matching.
##
## For example:
##
## @example
## @group
## strmatch ("apple", "apple juice")
##      @result{} 1
##
## strmatch ("apple", ["apple  "; "apple juice"; "an apple"])
##      @result{} [1; 2]
##
## strmatch ("apple", ["apple  "; "apple juice"; "an apple"], "exact")
##      @result{} [1]
## @end group
## @end example
##
## @strong{Caution:} @code{strmatch} is scheduled for deprecation.  Use
## @code{strncmp} (normal case), or @code{strcmp} (@qcode{"exact"} case), or
## @code{regexp} in all new code.
## @seealso{strfind, findstr, strcmp, strncmp, strcmpi, strncmpi, find}
## @end deftypefn

## Author: Paul Kienzle, Alois Schloegl
## Adapted-by: jwe

function idx = strmatch (s, A, exact)

  if (nargin < 2 || nargin > 3)
    print_usage ();
  endif

  if (! ischar (s) || (! isempty (s) && ! isvector (s)))
    error ("strmatch: S must be a string");
  elseif (! (ischar (A) || iscellstr (A)))
    error ("strmatch: A must be a string or cell array of strings");
  endif

  ## Trim blanks and nulls from search string
  s = regexprep (s, "[ \\0]+$", '');
  len = length (s);

  exact = nargin == 3 && ischar (exact) && strcmp (exact, "exact");

  if (ischar (A))
    [nr, nc] = size (A);
    if (len > nc)
      idx = [];
    else
      match = all (bsxfun (@eq, A(:,1:len), s), 2);
      if (exact)
        AA = A(:,len+1:nc);
        match &= all (AA == " " | AA == "\0", 2);
      endif
      idx = find (match);
    endif
  else
    if (len > 0)
      idx = find (strncmp (s, A, len));
    else
      idx = find (strcmp (s, A));
    endif
    if (exact)
      ## We can't just use strcmp, because we need to ignore spaces at end.
      B = regexprep (A(idx), "[ \\0]+$", '');
      idx = idx(strcmp (s, B));
    endif
  endif

endfunction


%!assert (strmatch ("a", {"aaa", "bab", "bbb"}), 1)
%!assert (strmatch ("apple", "apple juice"), 1)
%!assert (strmatch ("apple", ["apple pie"; "apple juice"; "an apple"]), [1; 2])
%!assert (strmatch ("apple", {"apple pie"; "apple juice"; "tomato"}), [1; 2])
%!assert (strmatch ("apple pie", "apple"), [])
%!assert (strmatch ("a ", "a"), 1)
%!assert (strmatch ("a", "a \0", "exact"), 1)
%!assert (strmatch ("a b", {"a b", "a c", "c d"}), 1)
%!assert (strmatch ("", {"", "foo", "bar", ""}), [1, 4])
%!assert (strmatch ('', { '', '% comment', 'var a = 5', ''}, 'exact'), [1,4])

%% Test input validation
%!error <Invalid call to strmatch> strmatch ()
%!error <Invalid call to strmatch> strmatch ("a")
%!error <Invalid call to strmatch> strmatch ("a", "aaa", "exact", 1)
%!error <S must be a string> strmatch (1, "aaa")
%!error <S must be a string> strmatch (char ("a", "bb"), "aaa")
%!error <A must be a string> strmatch ("a", 1)
%!error <A must be a string> strmatch ("a", {"hello", [1]})

