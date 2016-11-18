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
## @deftypefn  {Function File} {} is_leap_year ()
## @deftypefnx {Function File} {} is_leap_year (@var{year})
## Return true if @var{year} is a leap year and false otherwise.
##
## If no year is specified, @code{is_leap_year} uses the current year.
##
## For example:
##
## @example
## @group
## is_leap_year (2000)
##    @result{} 1
## @end group
## @end example
## @seealso{weekday, eomday, calendar}
## @end deftypefn

## Author: jwe

function retval = is_leap_year (year)

  if (nargin > 1)
    print_usage ();
  endif

  if (nargin == 0)
    t = clock ();
    year = t(1);
  endif

  retval = (rem (year, 4) == 0 & rem (year, 100) != 0) | (rem (year, 400) == 0);

endfunction


%!assert (is_leap_year (2000), true)
%!assert (is_leap_year (1976), true)
%!assert (is_leap_year (1000), false)
%!assert (is_leap_year (1800), false)
%!assert (is_leap_year (1600), true)

%!error is_leap_year (1, 2)

