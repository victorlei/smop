## Copyright (C) 1998-2015 John W. Eaton
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
## @deftypefn {Function File} {} isletter (@var{s})
## Return a logical array which is true where the elements of @var{s}
## are letters and false where they are not.
##
## This is an alias for the @code{isalpha} function.
## @seealso{isalpha, isdigit, ispunct, isspace, iscntrl, isalnum}
## @end deftypefn

## Author: jwe

function retval = isletter (s)

  if (nargin != 1)
    print_usage ();
  endif

  retval = isalpha (s);

endfunction


%!error isletter ()
%!error isletter ("a", "b")

