## Copyright (C) 1995-2015 John W. Eaton
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
## @deftypefn {Function File} {} ctime (@var{t})
## Convert a value returned from @code{time} (or any other non-negative
## integer), to the local time and return a string of the same form as
## @code{asctime}.
##
## The function @code{ctime (time)} is equivalent to
## @code{asctime (localtime (time))}.  For example:
##
## @example
## @group
## ctime (time ())
##    @result{} "Mon Feb 17 01:15:06 1997@xbackslashchar{}n"
## @end group
## @end example
## @seealso{asctime, time, localtime}
## @end deftypefn

## Author: jwe

function retval = ctime (t)

  if (nargin != 1)
    print_usage ();
  endif

  retval = asctime (localtime (t));

endfunction


%!test
%! t = time ();
%! assert (strcmp (asctime (localtime (t)), ctime (t)));

%!assert (ctime (time ())(end), "\n")

%!error ctime ()
%!error ctime (1, 2)

