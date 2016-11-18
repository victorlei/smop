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
## @deftypefn {Function File} {@var{e} =} eomday (@var{y}, @var{m})
## Return the last day of the month @var{m} for the year @var{y}.
## @seealso{weekday, datenum, datevec, is_leap_year, calendar}
## @end deftypefn

## Author: pkienzle <pkienzle@users.sf.net>
## Created: 25 July 2004
## Adapted-By: William Poetra Yoga Hadisoeseno <williampoetra@gmail.com>

function e = eomday (y, m)

  if (nargin != 2)
    print_usage ();
  endif

  eom = [31, 28, 31, 30 ,31, 30, 31, 31, 30, 31, 30, 31];
  e = reshape (eom(m), size (m));
  e += (m == 2) & (mod (y, 4) == 0 & (mod (y, 100) != 0 | mod (y, 400) == 0));

endfunction


%!demo
%! ## Find leap years in the 20th century
%! y = 1900:1999;
%! e = eomday (y, repmat (2, [1, 100]));
%! y(find (e == 29))

%!assert (eomday ([-4:4],2), [29,28,28,28,29,28,28,28,29])
%!assert (eomday ([-901,901],2), [28,28])
%!assert (eomday ([-100,100],2), [28,28])
%!assert (eomday ([-900,900],2), [28,28])
%!assert (eomday ([-400,400],2), [29,29])
%!assert (eomday ([-800,800],2), [29,29])
%!assert (eomday (2001,1:12), [31,28,31,30,31,30,31,31,30,31,30,31])
%!assert (eomday (1:3,1:3), [31,28,31])
%!assert (eomday (1:2000,2)', datevec (datenum (1:2000,3,0))(:,3))
%!assert ([1900:1999](find(eomday(1900:1999,2*ones(1,100))==29)), [1904,1908,1912,1916,1920,1924,1928,1932,1936,1940,1944,1948,1952,1956,1960,1964,1968,1972,1976,1980,1984,1988,1992,1996])
%!assert (eomday ([2004;2005], [2;2]), [29;28])

## Test input validation
%!error eomday ()
%!error eomday (1)
%!error eomday (1,2,3)

