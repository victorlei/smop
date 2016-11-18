## Copyright (C) 1996-2015 Kurt Hornik
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
## @deftypefn  {Function File} {} index (@var{s}, @var{t})
## @deftypefnx {Function File} {} index (@var{s}, @var{t}, @var{direction})
## Return the position of the first occurrence of the string @var{t} in the
## string @var{s}, or 0 if no occurrence is found.
##
## @var{s} may also be a string array or cell array of strings.
##
## For example:
##
## @example
## @group
## index ("Teststring", "t")
##     @result{} 4
## @end group
## @end example
##
## If @var{direction} is @qcode{"first"}, return the first element found.
## If @var{direction} is @qcode{"last"}, return the last element found.
##
## @seealso{find, rindex}
## @end deftypefn

## Author: Kurt Hornik <Kurt.Hornik@wu-wien.ac.at>
## Adapted-By: jwe
## This is patterned after the AWK function of the same name.

function n = index (s, t, direction = "first")

  if (nargin < 2 || nargin > 3)
    print_usage ();
  endif

  if (ischar (s))
    if (! isrow (s))
      s = cellstr (s);  # Handle string arrays by conversion to cellstr
    endif
  elseif (! iscellstr (s))
    error ("index: S must be a string, string array, or cellstr");
  endif

  f = strfind (s, t);
  if (isempty (f))
    f = 0;
  elseif (iscell (f))
    f(cellfun ("isempty", f)) = {0};
  endif

  direction = tolower (direction);

  if (strcmp (direction, "first"))
    if (iscell (f))
      n = cellfun ("min", f);
    else
      n = f(1);
    endif
  elseif (strcmp (direction, "last"))
    if (iscell (f))
      n = cellfun ("max", f);
    else
      n = f(end);
    endif
  else
    error ('index: DIRECTION must be either "first" or "last"');
  endif

endfunction


%!assert (index ("foobarbaz", "b"), 4)
%!assert (index ("foobarbaz", "z"), 9)

%!assert (index ("astringbstringcstring", "s"), 2)
%!assert (index ("astringbstringcstring", "st"), 2)
%!assert (index ("astringbstringcstring", "str"), 2)
%!assert (index ("astringbstringcstring", "string"), 2)
%!assert (index ("abc---", "abc+++"), 0)

## test everything out in reverse
%!assert (index ("astringbstringcstring", "s", "last"), 16)
%!assert (index ("astringbstringcstring", "st", "last"), 16)
%!assert (index ("astringbstringcstring", "str", "last"), 16)
%!assert (index ("astringbstringcstring", "string", "last"), 16)
%!assert (index ("abc---", "abc+++", "last"), 0)

%!test
%! str = char ("Hello", "World", "Goodbye", "World");
%! assert (index (str, "o"), [5; 2; 2; 2]);
%! assert (index (str, "o", "last"), [5; 2; 3; 2]);
%! str = cellstr (str);
%! assert (index (str, "o"), [5; 2; 2; 2]);
%! assert (index (str, "o", "last"), [5; 2; 3; 2]);

## Test input validation
%!error index ()
%!error index ("a")
%!error index ("a", "b", "first", "d")
%!error index (1, "bar")
%!error index ("foo", "bar", 3)

