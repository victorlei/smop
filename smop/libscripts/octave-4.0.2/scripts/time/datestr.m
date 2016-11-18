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
## @deftypefn  {Function File} {@var{str} =} datestr (@var{date})
## @deftypefnx {Function File} {@var{str} =} datestr (@var{date}, @var{f})
## @deftypefnx {Function File} {@var{str} =} datestr (@var{date}, @var{f}, @var{p})
## Format the given date/time according to the format @code{f} and return
## the result in @var{str}.
##
## @var{date} is a serial date number (see @code{datenum}) or a date vector
## (see @code{datevec}).  The value of @var{date} may also be a string or cell
## array of strings.
##
## @var{f} can be an integer which corresponds to one of the codes in the table
## below, or a date format string.
##
## @var{p} is the year at the start of the century in which two-digit years are
## to be interpreted in.  If not specified, it defaults to the current year
## minus 50.
##
## For example, the date 730736.65149 (2000-09-07 15:38:09.0934) would be
## formatted as follows:
##
## @multitable @columnfractions 0.1 0.45 0.35
## @headitem Code @tab Format @tab Example
## @item 0 @tab dd-mmm-yyyy HH:MM:SS    @tab 07-Sep-2000 15:38:09
## @item 1 @tab dd-mmm-yyyy             @tab 07-Sep-2000
## @item 2 @tab mm/dd/yy                @tab 09/07/00
## @item 3 @tab mmm                     @tab Sep
## @item 4 @tab m                       @tab S
## @item 5 @tab mm                      @tab 09
## @item 6 @tab mm/dd                   @tab 09/07
## @item 7 @tab dd                      @tab 07
## @item 8 @tab ddd                     @tab Thu
## @item 9 @tab d                       @tab T
## @item 10 @tab yyyy                   @tab 2000
## @item 11 @tab yy                     @tab 00
## @item 12 @tab mmmyy                  @tab Sep00
## @item 13 @tab HH:MM:SS               @tab 15:38:09
## @item 14 @tab HH:MM:SS PM            @tab 03:38:09 PM
## @item 15 @tab HH:MM                  @tab 15:38
## @item 16 @tab HH:MM PM               @tab 03:38 PM
## @item 17 @tab QQ-YY                  @tab Q3-00
## @item 18 @tab QQ                     @tab Q3
## @item 19 @tab dd/mm                  @tab 07/09
## @item 20 @tab dd/mm/yy               @tab 07/09/00
## @item 21 @tab mmm.dd,yyyy HH:MM:SS   @tab Sep.07,2000 15:38:08
## @item 22 @tab mmm.dd,yyyy            @tab Sep.07,2000
## @item 23 @tab mm/dd/yyyy             @tab 09/07/2000
## @item 24 @tab dd/mm/yyyy             @tab 07/09/2000
## @item 25 @tab yy/mm/dd               @tab 00/09/07
## @item 26 @tab yyyy/mm/dd             @tab 2000/09/07
## @item 27 @tab QQ-YYYY                @tab Q3-2000
## @item 28 @tab mmmyyyy                @tab Sep2000
## @item 29 @tab yyyy-mm-dd             @tab 2000-09-07
## @item 30 @tab yyyymmddTHHMMSS        @tab 20000907T153808
## @item 31 @tab yyyy-mm-dd HH:MM:SS    @tab 2000-09-07 15:38:08
## @end multitable
##
## If @var{f} is a format string, the following symbols are recognized:
##
## @multitable @columnfractions 0.1 0.7 0.2
## @headitem Symbol @tab Meaning @tab Example
## @item yyyy @tab Full year                                    @tab 2005
## @item yy   @tab Two-digit year                               @tab 05
## @item mmmm @tab Full month name                              @tab December
## @item mmm  @tab Abbreviated month name                       @tab Dec
## @item mm   @tab Numeric month number (padded with zeros)     @tab 01, 08, 12
## @item m    @tab First letter of month name (capitalized)     @tab D
## @item dddd @tab Full weekday name                            @tab Sunday
## @item ddd  @tab Abbreviated weekday name                     @tab Sun
## @item dd   @tab Numeric day of month (padded with zeros)     @tab 11
## @item d    @tab First letter of weekday name (capitalized)   @tab S
## @item HH   @tab Hour of day, padded with zeros if PM is set  @tab 09:00
## @item      @tab and not padded with zeros otherwise          @tab 9:00 AM
## @item MM   @tab Minute of hour (padded with zeros)           @tab 10:05
## @item SS   @tab Second of minute (padded with zeros)         @tab 10:05:03
## @item FFF  @tab Milliseconds of second (padded with zeros)   @tab 10:05:03.012
## @item AM   @tab Use 12-hour time format                      @tab 11:30 AM
## @item PM   @tab Use 12-hour time format                      @tab 11:30 PM
## @end multitable
##
## If @var{f} is not specified or is @code{-1}, then use 0, 1 or 16, depending
## on whether the date portion or the time portion of @var{date} is empty.
##
## If @var{p} is nor specified, it defaults to the current year minus 50.
##
## If a matrix or cell array of dates is given, a column vector of date strings
## is returned.
##
## @seealso{datenum, datevec, date, now, clock}
## @end deftypefn

## FIXME: parse arbitrary code strings.
## e.g., for  Wednesday 2001-03-05 09:04:06 AM, use
##     yy    01
##     yyyy  2001
##     m     M
##     mm    03
##     mmm   Mar
##     d     W
##     dd    05
##     ddd   Wed
##     HH    09
##     MM    04
##     SS    06
##     PM    AM
## FIXME: Vectorize.  It is particularly easy since all the codes are
##    fixed width.  Just generate the parts in separate arrays and
##    concatenate.

## Author: pkienzle <pkienzle@users.sf.net>
## Created: 10 October 2001 (CVS)
## Adapted-By: William Poetra Yoga Hadisoeseno <williampoetra@gmail.com>

function retval = datestr (date, f = [], p = [])

  persistent dateform names_mmmm names_m names_d;

  if (isempty (dateform))
    dateform = cell (32, 1);
    dateform{1}  = "dd-mmm-yyyy HH:MM:SS";
    dateform{2}  = "dd-mmm-yyyy";
    dateform{3}  = "mm/dd/yy";
    dateform{4}  = "mmm";
    dateform{5}  = "m";
    dateform{6}  = "mm";
    dateform{7}  = "mm/dd";
    dateform{8}  = "dd";
    dateform{9}  = "ddd";
    dateform{10} = "d";
    dateform{11} = "yyyy";
    dateform{12} = "yy";
    dateform{13} = "mmmyy";
    dateform{14} = "HH:MM:SS";
    dateform{15} = "HH:MM:SS PM";
    dateform{16} = "HH:MM";
    dateform{17} = "HH:MM PM";
    dateform{18} = "QQ-YY";
    dateform{19} = "QQ";
    dateform{20} = "dd/mm";
    dateform{21} = "dd/mm/yy";
    dateform{22} = "mmm.dd,yyyy HH:MM:SS";
    dateform{23} = "mmm.dd,yyyy";
    dateform{24} = "mm/dd/yyyy";
    dateform{25} = "dd/mm/yyyy";
    dateform{26} = "yy/mm/dd";
    dateform{27} = "yyyy/mm/dd";
    dateform{28} = "QQ-YYYY";
    dateform{29} = "mmmyyyy";
    dateform{30} = "yyyy-mm-dd";
    dateform{31} = "yyyymmddTHHMMSS";
    dateform{32} = "yyyy-mm-dd HH:MM:SS";

    names_m = {"J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"};
    names_d = {"S", "M", "T", "W", "T", "F", "S"};
  endif

  if (nargin < 1 || nargin > 3)
    print_usage ();
  endif

  ## Guess input type.  We might be wrong.
  if (ischar (date) || iscellstr (date) || columns (date) != 6)
    v = datevec (date, p);
  else
    v = [];
    if (columns (date) == 6)
      ## Make sure that the input really is a datevec.
      maxdatevec = [Inf, 12, 31, 23, 59, 60];
      if (any (max (date, 1) > maxdatevec)
          || any (date(:,1:5) != floor (date(:,1:5))))
        v = datevec (date, p);
      endif
    endif
    if (isempty (v))
      v = date;
    endif
  endif

  retval = "";
  for i = 1 : rows (v)

    if (isempty (f))
      if (v(i,4:6) == 0)
        f = 1;
      elseif (v(i,1:3) == [-1, 12, 31])
        f = 16;
      else
        f = 0;
      endif
    endif

    if (isnumeric (f))
      df = dateform{f + 1};
    else
      df = f;
    endif

    df_orig = df;
    df = strrep (df, "AM", "%p");
    df = strrep (df, "PM", "%p");
    if (strcmp (df, df_orig))
      ## PM not set.
      df = strrep (df, "HH", "%H");
    else
      df = strrep (df, "HH", sprintf ("%2d", v(i,4)));
    endif

    df = regexprep (df, "[Yy][Yy][Yy][Yy]", "%Y");

    df = regexprep (df, "[Yy][Yy]", "%y");

    df = regexprep (df, "[Dd][Dd][Dd][Dd]", "%A");

    df = regexprep (df, "[Dd][Dd][Dd]", "%a");

    df = regexprep (df, "[Dd][Dd]", "%d");

    wday = weekday (datenum (v(i,1), v(i,2), v(i,3)));
    tmp = names_d{wday};
    df = regexprep (df, "([^%])[Dd]", sprintf ("$1%s", tmp));
    df = regexprep (df, "^[Dd]", sprintf ("%s", tmp));

    df = strrep (df, "mmmm", "%B");

    df = strrep (df, "mmm", "%b");

    df = strrep (df, "mm", "%m");

    tmp = names_m{v(i,2)};
    pos = regexp (df, "[^%]m") + 1;
    df(pos) = tmp;
    df = regexprep (df, "^m", tmp);

    df = strrep (df, "MM", "%M");

    df = regexprep (df, "[Ss][Ss]", "%S");

    df = strrep (df, "FFF", sprintf ("%03d",
                                     round (1000 * (v(i,6) - fix (v(i,6))))));

    df = strrep (df, "QQ", sprintf ("Q%d", fix ((v(i,2) + 2) / 3)));

    vi = v(i,:);
    tm.year = vi(1) - 1900;
    tm.mon = vi(2) - 1;
    tm.mday = vi(3);
    tm.hour = vi(4);
    tm.min = vi(5);
    sec = vi(6);
    tm.sec = fix (sec);
    tm.usec = fix ((sec - tm.sec) * 1e6);
    tm.wday = wday - 1;
    ## FIXME: Do we need YDAY and DST?  How should they be computed?
    ## We don't want to use "localtime (mktime (tm))" because that
    ## doesn't correctly handle dates before 1970-01-01 on some systems.
    ## tm.yday = ?;
    ## tm.isdst = ?;

    str = strftime (df, tm);

    retval = [retval; str];

  endfor

endfunction


%!demo
%! ## Current date and time in default format
%! datestr (now ())

%!demo
%! ## Current date (integer portion of datenum)
%! datestr (fix (now ()))

%!demo
%! ## Current time (fractional portion of datenum)
%! datestr (rem (now (), 1))

%!shared testtime
%! testtime = [2005.0000, 12.0000, 18.0000, 2.0000, 33.0000, 17.3822];
%!assert (datestr (testtime,0), "18-Dec-2005 02:33:17")
%!assert (datestr (testtime,1), "18-Dec-2005")
%!assert (datestr (testtime,2), "12/18/05")
%!assert (datestr (testtime,3), "Dec")
%!assert (datestr (testtime,4), "D")
%!assert (datestr (testtime,5), "12")
%!assert (datestr (testtime,6), "12/18")
%!assert (datestr (testtime,7), "18")
%!assert (datestr (testtime,8), "Sun")
%!assert (datestr (testtime,9), "S")
%!assert (datestr (testtime,10), "2005")
%!assert (datestr (testtime,11), "05")
%!assert (datestr (testtime,12), "Dec05")
%!assert (datestr (testtime,13), "02:33:17")
## Mac OS X interprets %p parameter to strftime as lower case am/pm indicator.
## Accomodate this, although no other UNIX-based OS does this.
%!test
%! obs = toupper (datestr (testtime,14));
%! assert (obs, " 2:33:17 AM");
%!assert (datestr (testtime,15), "02:33")
%!test
%! obs = toupper (datestr (testtime,16));
%! assert (obs, " 2:33 AM");
%!assert (datestr (testtime,17), "Q4-05")
%!assert (datestr (testtime,18), "Q4")
%!assert (datestr (testtime,19), "18/12")
%!assert (datestr (testtime,20), "18/12/05")
%!assert (datestr (testtime,21), "Dec.18,2005 02:33:17")
%!assert (datestr (testtime,22), "Dec.18,2005")
%!assert (datestr (testtime,23), "12/18/2005")
%!assert (datestr (testtime,24), "18/12/2005")
%!assert (datestr (testtime,25), "05/12/18")
%!assert (datestr (testtime,26), "2005/12/18")
%!assert (datestr (testtime,27), "Q4-2005")
%!assert (datestr (testtime,28), "Dec2005")
%!assert (datestr (testtime,29), "2005-12-18")
%!assert (datestr (testtime,30), "20051218T023317")
%!assert (datestr (testtime,31), "2005-12-18 02:33:17")
%!assert (datestr (testtime+[0 0 3 0 0 0], "dddd"), "Wednesday")
## Test possible bug where input is a vector of datenums that is exactly 6 wide
%!assert (datestr ([1944, 6, 6, 6, 30, 0], 0), "06-Jun-1944 06:30:00")
## Test fractional millisecond time extension
%!assert (datestr (testtime, "HH:MM:SS:FFF"), "02:33:17:382")

## Test input validation
%!error datestr ()
%!error datestr (1, 2, 3, 4)

