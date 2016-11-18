## Copyright (C) 1996-2015 John W. Eaton
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
## @deftypefn {Function File} {} etime (@var{t2}, @var{t1})
## Return the difference in seconds between two time values returned from
## @code{clock} (@math{@var{t2} - @var{t1}}).
##
## For example:
##
## @example
## @group
## t0 = clock ();
## # many computations later@dots{}
## elapsed_time = etime (clock (), t0);
## @end group
## @end example
##
## @noindent
## will set the variable @code{elapsed_time} to the number of seconds since the
## variable @code{t0} was set.
## @seealso{tic, toc, clock, cputime, addtodate}
## @end deftypefn

## Author: jwe

function secs = etime (t2, t1)

  if (nargin != 2)
    print_usage ();
  endif

  [~, s2] = datenum (t2);
  [~, s1] = datenum (t1);

  secs = s2 - s1;

endfunction


%!assert (etime ([1900,12,31,23,59,59], [1901,1,1,0,0,0]), -1)
%!assert (etime ([1900,2,28,23,59,59], [1900,3,1,0,0,0]), -1)
%!assert (etime ([2000,2,28,23,59,59], [2000,3,1,0,0,0]), -86401)
%!assert (etime ([1996,2,28,23,59,59], [1996,3,1,0,0,0]), -86401)
%!test
%! t1 = [1900,12,31,23,59,59; 1900,2,28,23,59,59];
%! t2 = [1901,1,1,0,0,0; 1900,3,1,0,0,0];
%! assert (etime (t2, t1), [1;1]);

%!test
%! t1 = [1993, 8, 20, 4, 56, 1];
%! t2 = [1993, 8, 21, 4, 56, 1];
%! t3 = [1993, 8, 20, 5, 56, 1];
%! t4 = [1993, 8, 20, 4, 57, 1];
%! t5 = [1993, 8, 20, 4, 56, 14];
%!
%! assert (etime (t2, t1), 86400);
%! assert (etime (t3, t1), 3600);
%! assert (etime (t4, t1), 60);
%! assert (etime (t5, t1), 13);

## Test input validation
%!error etime ();
%!error etime (1);
%!error etime (1, 2, 3);

