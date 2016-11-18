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
## @deftypefn {Function File} {} blanks (@var{n})
## Return a string of @var{n} blanks.
##
## For example:
##
## @example
## @group
## blanks (10);
## whos ans
##      @result{}
##       Attr Name        Size                     Bytes  Class
##       ==== ====        ====                     =====  =====
##            ans         1x10                        10  char
## @end group
## @end example
## @seealso{repmat}
## @end deftypefn

## Author: Kurt Hornik <Kurt.Hornik@wu-wien.ac.at>
## Adapted-By: jwe

function s = blanks (n)

  if (nargin != 1)
    print_usage ();
  elseif (! (isscalar (n) && n == fix (n) && n >= 0))
    error ("blanks: N must be a non-negative integer");
  endif

  ## If 1:n is empty, the following expression will create an empty
  ## character string.  Otherwise, it will create a row vector.
  s(1:n) = " ";

endfunction


## There really isn't that much to test here
%!assert (blanks (0), "")
%!assert (blanks (5), "     ")
%!assert (blanks (10), "          ")

## Test input validation
%!error blanks ()
%!error blanks (1, 2)
%!error blanks (ones (2))
%!error blanks (2.1)
%!error blanks (-2)

