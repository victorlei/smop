## Copyright (C) 2000-2015 Paul Kienzle
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
## @deftypefn  {Function File} {[@var{n}, @var{s}] =} weekday (@var{d})
## @deftypefnx {Function File} {[@var{n}, @var{s}] =} weekday (@var{d}, @var{format})
## Return the day of the week as a number in @var{n} and as a string in @var{s}.
##
## The days of the week are numbered 1--7 with the first day being Sunday.
##
## @var{d} is a serial date number or a date string.
##
## If the string @var{format} is not present or is equal to @qcode{"short"}
## then @var{s} will contain the abbreviated name of the weekday.  If
## @var{format} is @qcode{"long"} then @var{s} will contain the full name.
##
## Table of return values based on @var{format}:
##
## @multitable @columnfractions .06 .13 .16
## @headitem @var{n} @tab @qcode{"short"} @tab @qcode{"long"}
## @item 1 @tab Sun @tab Sunday
## @item 2 @tab Mon @tab Monday
## @item 3 @tab Tue @tab Tuesday
## @item 4 @tab Wed @tab Wednesday
## @item 5 @tab Thu @tab Thursday
## @item 6 @tab Fri @tab Friday
## @item 7 @tab Sat @tab Saturday
## @end multitable
##
## @seealso{eomday, is_leap_year, calendar, datenum, datevec}
## @end deftypefn

## Author: pkienzle <pkienzle@users.sf.net>
## Created: 10 October 2001 (CVS)
## Adapted-By: William Poetra Yoga Hadisoeseno <williampoetra@gmail.com>

function [d, s] = weekday (d, format = "short")

  if (nargin < 1 || nargin > 2)
    print_usage ();
  endif

  if (iscellstr (d) || isnumeric (d))
    endsize = size (d);
  elseif (ischar (d))
    endsize = [rows(d), 1];
  endif
  if (ischar (d) || iscellstr (d))
    ## Make sure the date is numeric
    d = datenum (d);
  endif
  ## Find the offset from a known Sunday (2008-Jan-6), mod 7.
  d = floor (reshape (mod (d - 733048, 7), endsize));
  ## Make Saturdays a 7 and not a 0.
  d(!d) = 7;

  if (isargout (2))
    if (strcmpi (format, "long"))
      names = {"Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" ...
               "Friday" "Saturday"};
    else
      names = {"Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"};
    endif
    s = strvcat (names(d));
  endif

endfunction


%!demo
%! ## Current weekday
%! [n, s] = weekday (now ())

%!demo
%! ## Weekday from datenum input
%! [n, s] = weekday (728647)

%!demo
%! ## Weekday of new millennium from datestr input
%! [n, s] = weekday ("1-Jan-2000")

%!assert (weekday (728647), 2)
## Test vector inputs for both directions
%!assert (weekday ([728647 728648]), [2 3])
%!assert (weekday ([728647;728648]), [2;3])
## Test a full week before our reference day
%!assert (weekday ("19-Dec-1994"), 2)
%!assert (weekday ("20-Dec-1994"), 3)
%!assert (weekday ("21-Dec-1994"), 4)
%!assert (weekday ("22-Dec-1994"), 5)
%!assert (weekday ("23-Dec-1994"), 6)
%!assert (weekday ("24-Dec-1994"), 7)
%!assert (weekday ("25-Dec-1994"), 1)
## Test our reference day
%!assert (weekday ("6-Jan-2008"), 1)
## Test a full week after our reference day
%!assert (weekday ("1-Feb-2008"), 6)
%!assert (weekday ("2-Feb-2008"), 7)
%!assert (weekday ("3-Feb-2008"), 1)
%!assert (weekday ("4-Feb-2008"), 2)
%!assert (weekday ("5-Feb-2008"), 3)
%!assert (weekday ("6-Feb-2008"), 4)
%!assert (weekday ("7-Feb-2008"), 5)
## Test fractional dates
%!assert (weekday (728647.1), 2)
## Test "long" option
%!test
%! [n, s] = weekday ("25-Dec-1994", "long");
%! assert (n, 1);
%! assert (s, "Sunday");

