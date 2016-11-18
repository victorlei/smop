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
## @deftypefn {Function File} {} rindex (@var{s}, @var{t})
## Return the position of the last occurrence of the character string
## @var{t} in the character string @var{s}, or 0 if no occurrence is
## found.
##
## @var{s} may also be a string array or cell array of strings.
##
## For example:
##
## @example
## @group
## rindex ("Teststring", "t")
##      @result{} 6
## @end group
## @end example
##
## The @code{rindex} function is equivalent to @code{index} with
## @var{direction} set to @qcode{"last"}.
##
## @seealso{find, index}
## @end deftypefn

## Author: Kurt Hornik <Kurt.Hornik@wu-wien.ac.at>
## Adapted-By: jwe
## This is patterned after the AWK function of the same name.

function n = rindex (s, t)

  if (nargin != 2)
    print_usage ();
  endif

  n = index (s, t, "last");

endfunction


%!assert (rindex ("foobarbaz", "b"), 7)
%!assert (rindex ("foobarbaz", "o"), 3)

%!test
%! str = char ("Hello", "World", "Goodbye", "World");
%! assert (rindex (str, "o"), [5; 2; 3; 2]);
%! str = cellstr (str);
%! assert (rindex (str, "o"), [5; 2; 3; 2]);

## Test input validation
%!error rindex ()
%!error rindex ("foo")
%!error rindex ("foo", "bar", "last")

