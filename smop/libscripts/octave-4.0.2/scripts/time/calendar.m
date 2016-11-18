## Copyright (C) 2004-2015 Paul Kienzle
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
## @deftypefn  {Function File} {@var{c} =} calendar ()
## @deftypefnx {Function File} {@var{c} =} calendar (@var{d})
## @deftypefnx {Function File} {@var{c} =} calendar (@var{y}, @var{m})
## @deftypefnx {Function File} {} calendar (@dots{})
## Return the current monthly calendar in a 6x7 matrix.
##
## If @var{d} is specified, return the calendar for the month containing the
## date @var{d}, which must be a serial date number or a date string.
##
## If @var{y} and @var{m} are specified, return the calendar for year @var{y}
## and month @var{m}.
##
## If no output arguments are specified, print the calendar on the screen
## instead of returning a matrix.
## @seealso{datenum, datestr}
## @end deftypefn

## Author: pkienzle <pkienzle@users.sf.net>
## Created: 25 July 2004
## Adapted-By: William Poetra Yoga Hadisoeseno <williampoetra@gmail.com>

function varargout = calendar (varargin)

  switch (nargin)
    case 0
      v = clock ();
      y = v(1);
      m = v(2);
      d = v(3);
    case 1
      v = datevec (varargin{1});
      y = v(1);
      m = v(2);
      d = v(3);
    case 2
      y = varargin{1};
      m = varargin{2};
      d = [];
    otherwise
      print_usage ();
  endswitch

  c = zeros (7, 6);
  dayone = datenum (y, m, 1);
  ndays = eomday (y, m);
  c(weekday (dayone) - 1 + [1:ndays]) = 1:ndays;

  if (nargout > 0)
    varargout{1} = c';
  else
    ## Layout the calendar days, 6 columns per day, 7 days per row.
    str = sprintf ("    %2d    %2d    %2d    %2d    %2d    %2d    %2d\n", c);

    ## Print an asterisk before the specified date
    if (! isempty (d))
      pos = weekday (dayone) + d - 1;
      idx = 6*pos + fix (pos / 7.1) - ifelse (d < 10, 1, 2);
      str(idx) = "*";
    endif

    ## Display the calendar.
    s.year = y - 1900;
    s.mon = m - 1;
    puts (strftime ("                    %b %Y\n", s));
    puts ("     S     M    Tu     W    Th     F     S\n");
    puts (str);
  endif

endfunction


%!demo
%! ## Calendar for current month
%! calendar ()

%!demo
%! ## Calendar for October, 1957
%! calendar (1957, 10)

%!assert ((calendar(2000,2))'(2:31), [0:29])
%!assert ((calendar(1957,10))'(2:33), [0:31])

## Test input validation
%!error calendar (1,2,3)

