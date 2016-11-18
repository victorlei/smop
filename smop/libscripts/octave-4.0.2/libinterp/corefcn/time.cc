/*

Copyright (C) 1996-2015 John W. Eaton

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <string>

#include "defun.h"
#include "error.h"
#include "oct-map.h"
#include "oct-time.h"
#include "ov.h"
#include "oct-obj.h"

// Date and time functions.

static octave_scalar_map
mk_tm_map (const octave_base_tm& t)
{
  octave_scalar_map m;

  m.assign ("usec", static_cast<double> (t.usec ()));
  m.assign ("sec", static_cast<double> (t.sec ()));
  m.assign ("min", static_cast<double> (t.min ()));
  m.assign ("hour", static_cast<double> (t.hour ()));
  m.assign ("mday", static_cast<double> (t.mday ()));
  m.assign ("mon", static_cast<double> (t.mon ()));
  m.assign ("year", static_cast<double> (t.year ()));
  m.assign ("wday", static_cast<double> (t.wday ()));
  m.assign ("yday", static_cast<double> (t.yday ()));
  m.assign ("isdst", static_cast<double> (t.isdst ()));
  m.assign ("zone", t.zone ());

  return m;
}

static inline int
intfield (const octave_scalar_map& m, const std::string& k)
{
  int retval = 0;

  octave_value v = m.getfield (k);

  if (! v.is_empty ())
    retval = v.int_value ();

  return retval;
}

static inline std::string
stringfield (const octave_scalar_map& m, const std::string& k)
{
  std::string retval;

  octave_value v = m.getfield (k);

  if (! v.is_empty ())
    retval = v.string_value ();

  return retval;
}

static octave_base_tm
extract_tm (const octave_scalar_map& m)
{
  octave_base_tm tm;

  tm.usec (intfield (m, "usec"));
  tm.sec (intfield (m, "sec"));
  tm.min (intfield (m, "min"));
  tm.hour (intfield (m, "hour"));
  tm.mday (intfield (m, "mday"));
  tm.mon (intfield (m, "mon"));
  tm.year (intfield (m, "year"));
  tm.wday (intfield (m, "wday"));
  tm.yday (intfield (m, "yday"));
  tm.isdst (intfield (m, "isdst"));
  tm.zone (stringfield (m, "zone"));

  return tm;
}

DEFUN (time, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {@var{seconds} =} time ()\n\
Return the current time as the number of seconds since the epoch.\n\
\n\
The epoch is referenced to 00:00:00 CUT (Coordinated Universal Time) 1 Jan\n\
1970.  For example, on Monday February 17, 1997 at 07:15:06 CUT, the value\n\
returned by @code{time} was 856163706.\n\
@seealso{strftime, strptime, localtime, gmtime, mktime, now, date, clock, datenum, datestr, datevec, calendar, weekday}\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 0)
    retval = octave_time ();
  else
    print_usage ();

  return retval;
}

/*
%!assert (time () > 0)

%!error time (1)
*/

DEFUN (gmtime, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {@var{tm_struct} =} gmtime (@var{t})\n\
Given a value returned from @code{time}, or any non-negative integer,\n\
return a time structure corresponding to CUT (Coordinated Universal Time).\n\
\n\
For example:\n\
\n\
@example\n\
@group\n\
gmtime (time ())\n\
     @result{} @{\n\
           usec = 0\n\
           sec = 6\n\
           min = 15\n\
           hour = 7\n\
           mday = 17\n\
           mon = 1\n\
           year = 97\n\
           wday = 1\n\
           yday = 47\n\
           isdst = 0\n\
           zone = CST\n\
        @}\n\
@end group\n\
@end example\n\
@seealso{strftime, strptime, localtime, mktime, time, now, date, clock, datenum, datestr, datevec, calendar, weekday}\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 1)
    {
      double tmp = args(0).double_value ();

      if (! error_state)
        retval = octave_value (mk_tm_map (octave_gmtime (tmp)));
    }
  else
    print_usage ();

  return retval;
}

/*
%!test
%! ts = gmtime (time ());
%! assert (isstruct (ts));
%! assert (isfield (ts, "usec"));
%! assert (isfield (ts, "year"));
%! assert (isfield (ts, "mon"));
%! assert (isfield (ts, "mday"));
%! assert (isfield (ts, "sec"));
%! assert (isfield (ts, "min"));
%! assert (isfield (ts, "wday"));
%! assert (isfield (ts, "hour"));
%! assert (isfield (ts, "isdst"));
%! assert (isfield (ts, "yday"));

%!error gmtime ()
%!error gmtime (1, 2)
*/

DEFUN (localtime, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {@var{tm_struct} =} localtime (@var{t})\n\
Given a value returned from @code{time}, or any non-negative integer,\n\
return a time structure corresponding to the local time zone.\n\
\n\
@example\n\
@group\n\
localtime (time ())\n\
     @result{} @{\n\
           usec = 0\n\
           sec = 6\n\
           min = 15\n\
           hour = 1\n\
           mday = 17\n\
           mon = 1\n\
           year = 97\n\
           wday = 1\n\
           yday = 47\n\
           isdst = 0\n\
           zone = CST\n\
        @}\n\
@end group\n\
@end example\n\
@seealso{strftime, strptime, gmtime, mktime, time, now, date, clock, datenum, datestr, datevec, calendar, weekday}\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 1)
    {
      double tmp = args(0).double_value ();

      if (! error_state)
        retval = octave_value (mk_tm_map (octave_localtime (tmp)));
    }
  else
    print_usage ();

  return retval;
}

/*
%!test
%! ts = localtime (time ());
%! assert (isstruct (ts));
%! assert (isfield (ts, "usec"));
%! assert (isfield (ts, "year"));
%! assert (isfield (ts, "mon"));
%! assert (isfield (ts, "mday"));
%! assert (isfield (ts, "sec"));
%! assert (isfield (ts, "min"));
%! assert (isfield (ts, "wday"));
%! assert (isfield (ts, "hour"));
%! assert (isfield (ts, "isdst"));
%! assert (isfield (ts, "yday"));

%!error localtime ()
%!error localtime (1, 2)
*/

DEFUN (mktime, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {@var{seconds} =} mktime (@var{tm_struct})\n\
Convert a time structure corresponding to the local time to the number of\n\
seconds since the epoch.\n\
\n\
For example:\n\
\n\
@example\n\
@group\n\
mktime (localtime (time ()))\n\
     @result{} 856163706\n\
@end group\n\
@end example\n\
@seealso{strftime, strptime, localtime, gmtime, time, now, date, clock, datenum, datestr, datevec, calendar, weekday}\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 1)
    {
      octave_scalar_map map = args(0).scalar_map_value ();

      if (! error_state)
        {
          octave_base_tm tm = extract_tm (map);

          if (! error_state)
            retval = octave_time (tm);
          else
            error ("mktime: invalid TM_STRUCT argument");
        }
      else
        error ("mktime: TM_STRUCT argument must be a structure");
    }
  else
    print_usage ();

  return retval;
}

/*
%!test
%! t = time ();
%! assert (fix (mktime (localtime (t))) == fix (t));

## These tests fail on systems with mktime functions of limited
## intelligence:
%!assert (datestr (datenum (1969, 1, 1), 0), "01-Jan-1969 00:00:00")
%!assert (datestr (datenum (1901, 1, 1), 0), "01-Jan-1901 00:00:00")
%!assert (datestr (datenum (1795, 1, 1), 0), "01-Jan-1795 00:00:00")

%!error mktime ()
%!error mktime (1)
%!error mktime (1, 2, 3)
%!error mktime (struct ("year", "foo"))
*/

DEFUN (strftime, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} strftime (@var{fmt}, @var{tm_struct})\n\
Format the time structure @var{tm_struct} in a flexible way using the format\n\
string @var{fmt} that contains @samp{%} substitutions similar to those in\n\
@code{printf}.\n\
\n\
Except where noted, substituted fields have a fixed size; numeric fields are\n\
padded if necessary.  Padding is with zeros by default; for fields that\n\
display a single number, padding can be changed or inhibited by following\n\
the @samp{%} with one of the modifiers described below.  Unknown field\n\
specifiers are copied as normal characters.  All other characters are copied\n\
to the output without change.  For example:\n\
\n\
@example\n\
@group\n\
strftime (\"%r (%Z) %A %e %B %Y\", localtime (time ()))\n\
      @result{} \"01:15:06 AM (CST) Monday 17 February 1997\"\n\
@end group\n\
@end example\n\
\n\
Octave's @code{strftime} function supports a superset of the ANSI C field\n\
specifiers.\n\
\n\
@noindent\n\
Literal character fields:\n\
\n\
@table @code\n\
@item %%\n\
% character.\n\
\n\
@item %n\n\
Newline character.\n\
\n\
@item %t\n\
Tab character.\n\
@end table\n\
\n\
@noindent\n\
Numeric modifiers (a nonstandard extension):\n\
\n\
@table @code\n\
@item - (dash)\n\
Do not pad the field.\n\
\n\
@item _ (underscore)\n\
Pad the field with spaces.\n\
@end table\n\
\n\
@noindent\n\
Time fields:\n\
\n\
@table @code\n\
@item %H\n\
Hour (00-23).\n\
\n\
@item %I\n\
Hour (01-12).\n\
\n\
@item %k\n\
Hour (0-23).\n\
\n\
@item %l\n\
Hour (1-12).\n\
\n\
@item %M\n\
Minute (00-59).\n\
\n\
@item %p\n\
Locale's AM or PM.\n\
\n\
@item %r\n\
Time, 12-hour (hh:mm:ss [AP]M).\n\
\n\
@item %R\n\
Time, 24-hour (hh:mm).\n\
\n\
@item %s\n\
Time in seconds since 00:00:00, Jan 1, 1970 (a nonstandard extension).\n\
\n\
@item %S\n\
Second (00-61).\n\
\n\
@item %T\n\
Time, 24-hour (hh:mm:ss).\n\
\n\
@item %X\n\
Locale's time representation (%H:%M:%S).\n\
\n\
@item %Z\n\
Time zone (EDT), or nothing if no time zone is determinable.\n\
@end table\n\
\n\
@noindent\n\
Date fields:\n\
\n\
@table @code\n\
@item %a\n\
Locale's abbreviated weekday name (Sun-Sat).\n\
\n\
@item %A\n\
Locale's full weekday name, variable length (Sunday-Saturday).\n\
\n\
@item %b\n\
Locale's abbreviated month name (Jan-Dec).\n\
\n\
@item %B\n\
Locale's full month name, variable length (January-December).\n\
\n\
@item %c\n\
Locale's date and time (Sat Nov 04 12:02:33 EST 1989).\n\
\n\
@item %C\n\
Century (00-99).\n\
\n\
@item %d\n\
Day of month (01-31).\n\
\n\
@item %e\n\
Day of month ( 1-31).\n\
\n\
@item %D\n\
Date (mm/dd/yy).\n\
\n\
@item %h\n\
Same as %b.\n\
\n\
@item %j\n\
Day of year (001-366).\n\
\n\
@item %m\n\
Month (01-12).\n\
\n\
@item %U\n\
Week number of year with Sunday as first day of week (00-53).\n\
\n\
@item %w\n\
Day of week (0-6).\n\
\n\
@item %W\n\
Week number of year with Monday as first day of week (00-53).\n\
\n\
@item %x\n\
Locale's date representation (mm/dd/yy).\n\
\n\
@item %y\n\
Last two digits of year (00-99).\n\
\n\
@item %Y\n\
Year (1970-).\n\
@end table\n\
@seealso{strptime, localtime, gmtime, mktime, time, now, date, clock, datenum, datestr, datevec, calendar, weekday}\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 2)
    {
      if (args(0).is_string ())
        {
          std::string fmt = args(0).string_value ();

          octave_scalar_map map = args(1).scalar_map_value ();

          if (! error_state)
            {
              octave_base_tm tm = extract_tm (map);

              if (! error_state)
                retval = tm.strftime (fmt);
              else
                error ("strftime: invalid TM_STRUCT argument");
            }
          else
            error ("strftime: TM_STRUCT must be a structure");
        }
      else
        error ("strftime: FMT must be a string");
    }
  else
    print_usage ();

  return retval;
}

/*
%!assert (ischar (strftime ("%%%n%t%H%I%k%l", localtime (time ()))));
%!assert (ischar (strftime ("%M%p%r%R%s%S%T", localtime (time ()))));
%!assert (ischar (strftime ("%X%Z%z%a%A%b%B", localtime (time ()))));
%!assert (ischar (strftime ("%c%C%d%e%D%h%j", localtime (time ()))));
%!assert (ischar (strftime ("%m%U%w%W%x%y%Y", localtime (time ()))));

%!error strftime ()
%!error strftime ("foo", 1)
%!error strftime ("foo", struct ("year", "foo"))
%!error strftime ("foo", localtime (time ()), 1)
*/

DEFUN (strptime, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {[@var{tm_struct}, @var{nchars}] =} strptime (@var{str}, @var{fmt})\n\
Convert the string @var{str} to the time structure @var{tm_struct} under\n\
the control of the format string @var{fmt}.\n\
\n\
If @var{fmt} fails to match, @var{nchars} is 0; otherwise, it is set to the\n\
position of last matched character plus 1. Always check for this unless\n\
you're absolutely sure the date string will be parsed correctly.\n\
@seealso{strftime, localtime, gmtime, mktime, time, now, date, clock, datenum, datestr, datevec, calendar, weekday}\n\
@end deftypefn")
{
  octave_value_list retval;

  if (args.length () == 2)
    {
      if (args(0).is_string ())
        {
          std::string str = args(0).string_value ();

          if (args(1).is_string ())
            {
              std::string fmt = args(1).string_value ();

              octave_strptime t (str, fmt);

              retval(1) = t.characters_converted ();
              retval(0) = octave_value (mk_tm_map (t));
            }
          else
            error ("strptime: FMT must be a string");
        }
      else
        error ("strptime: argument STR must be a string");
    }
  else
    print_usage ();

  return retval;
}

/*
%!test
%! fmt = "%Y-%m-%d %H:%M:%S";
%! s = strftime (fmt, localtime (time ()));
%! ts = strptime  (s, fmt);
%! assert (isstruct (ts));
%! assert (isfield (ts, "usec"));
%! assert (isfield (ts, "year"));
%! assert (isfield (ts, "mon"));
%! assert (isfield (ts, "mday"));
%! assert (isfield (ts, "sec"));
%! assert (isfield (ts, "min"));
%! assert (isfield (ts, "wday"));
%! assert (isfield (ts, "hour"));
%! assert (isfield (ts, "isdst"));
%! assert (isfield (ts, "yday"));

%!error strptime ()
*/
