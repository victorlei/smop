/*

Copyright (C) 1999-2015 John W. Eaton

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

#include <limits>

#include <ctime>

#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>

#include "strftime.h"

#include "lo-error.h"
#include "lo-math.h"
#include "lo-utils.h"
#include "oct-locbuf.h"
#include "oct-time.h"

octave_time::octave_time (const octave_base_tm& tm)
  : ot_unix_time (), ot_usec ()
{
  struct tm t;

  t.tm_sec = tm.sec ();
  t.tm_min = tm.min ();
  t.tm_hour = tm.hour ();
  t.tm_mday = tm.mday ();
  t.tm_mon = tm.mon ();
  t.tm_year = tm.year ();
  t.tm_wday = tm.wday ();
  t.tm_yday = tm.yday ();
  t.tm_isdst = tm.isdst ();

#if defined (HAVE_STRUCT_TM_GMTOFF)
  t.tm_gmtoff = tm.gmtoff ();
#endif

#if defined (HAVE_STRUCT_TM_TM_ZONE)
  std::string s = tm.zone ();
  char *ps = strsave (s.c_str ());
  t.tm_zone = ps;
#endif

  ot_unix_time = gnulib::mktime (&t);

#if defined (HAVE_STRUCT_TM_TM_ZONE)
  delete [] ps;
#endif

  ot_usec = tm.usec ();
}

std::string
octave_time::ctime (void) const
{
  return octave_localtime (*this) . asctime ();
}

void
octave_time::stamp (void)
{
  struct timeval tp;

  gnulib::gettimeofday (&tp, 0);

  ot_unix_time = tp.tv_sec;
  ot_usec = tp.tv_usec;
}

// From the mktime() manual page:
//
//     The  mktime()  function converts a broken-down time structure,
//     expressed as local time, to calendar time representation.
//
//     <snip>
//
//     If structure members are outside  their  legal interval, they
//     will be normalized (so that, e.g., 40 October is changed into
//     9 November).
//
// So, we no longer check limits here.

#define DEFINE_SET_FIELD_FCN(type, f, lo, hi) \
  octave_base_tm& \
  octave_base_tm::f (type v) \
  { \
    tm_ ## f = v; \
 \
    return *this; \
  }

#define DEFINE_SET_INT_FIELD_FCN(f, lo, hi) \
  DEFINE_SET_FIELD_FCN (int, f, lo, hi)

DEFINE_SET_INT_FIELD_FCN (usec, 0, 1000000)
DEFINE_SET_INT_FIELD_FCN (sec, 0, 61)
DEFINE_SET_INT_FIELD_FCN (min, 0, 59)
DEFINE_SET_INT_FIELD_FCN (hour, 0, 23)
DEFINE_SET_INT_FIELD_FCN (mday, 1, 31)
DEFINE_SET_INT_FIELD_FCN (mon, 0, 11)
DEFINE_SET_INT_FIELD_FCN (year, std::numeric_limits<int>::min (),
                          std::numeric_limitd<int>::max ())
DEFINE_SET_INT_FIELD_FCN (wday, 0, 6)
DEFINE_SET_INT_FIELD_FCN (yday, 0, 365)
DEFINE_SET_INT_FIELD_FCN (isdst, 0, 1)
DEFINE_SET_FIELD_FCN (long, gmtoff, -86400, 0)

octave_base_tm&
octave_base_tm::zone (const std::string& s)
{
  tm_zone = s;
  return *this;
}

#if !defined STRFTIME_BUF_INITIAL_SIZE
#define STRFTIME_BUF_INITIAL_SIZE 128
#endif

std::string
octave_base_tm::strftime (const std::string& fmt) const
{
  std::string retval;

  if (! fmt.empty ())
    {
      struct tm t;

      t.tm_sec = tm_sec;
      t.tm_min = tm_min;
      t.tm_hour = tm_hour;
      t.tm_mday = tm_mday;
      t.tm_mon = tm_mon;
      t.tm_year = tm_year;
      t.tm_wday = tm_wday;
      t.tm_yday = tm_yday;
      t.tm_isdst = tm_isdst;

#if defined (HAVE_STRUCT_TM_GMTOFF)
      t.tm_gmtoff = tm_gmtoff;
#endif

#if defined (HAVE_STRUCT_TM_TM_ZONE)
      char *ps = strsave (tm_zone.c_str ());
      t.tm_zone = ps;
#endif

      const char *fmt_str = fmt.c_str ();

      char *buf = 0;
      size_t bufsize = STRFTIME_BUF_INITIAL_SIZE;
      size_t chars_written = 0;

      while (chars_written == 0)
        {
          delete [] buf;
          buf = new char [bufsize];
          buf[0] = '\0';

          chars_written = nstrftime (buf, bufsize, fmt_str, &t, 0, 0);

          bufsize *= 2;
        }

#if defined (HAVE_STRUCT_TM_TM_ZONE)
      delete [] ps;
#endif

      retval = buf;

      delete [] buf;
    }

  return retval;
}

void
octave_base_tm::init (void *p)
{
  if (! p)
    return;

  struct tm *t = static_cast<struct tm*> (p);

  tm_sec = t->tm_sec;
  tm_min = t->tm_min;
  tm_hour = t->tm_hour;
  tm_mday = t->tm_mday;
  tm_mon = t->tm_mon;
  tm_year = t->tm_year;
  tm_wday = t->tm_wday;
  tm_yday = t->tm_yday;
  tm_isdst = t->tm_isdst;

#if defined (HAVE_STRUCT_TM_GMTOFF)
  tm_gmtoff = t->tm_gmtoff;
#endif

#if defined (HAVE_STRUCT_TM_TM_ZONE)
  if (t->tm_zone)
    tm_zone = t->tm_zone;
#elif defined (HAVE_TZNAME)
  if (t->tm_isdst == 0 || t->tm_isdst == 1)
    tm_zone = tzname[t->tm_isdst];
#endif
}

void
octave_localtime::init (const octave_time& ot)
{
  tm_usec = ot.usec ();

  time_t t = ot.unix_time ();

  octave_base_tm::init (gnulib::localtime (&t));
}

void
octave_gmtime::init (const octave_time& ot)
{
  tm_usec = ot.usec ();

  time_t t = ot.unix_time ();

  octave_base_tm::init (gnulib::gmtime (&t));
}

void
octave_strptime::init (const std::string& str, const std::string& fmt)
{
  struct tm t;

  t.tm_sec = 0;
  t.tm_min = 0;
  t.tm_hour = 0;
  t.tm_mday = 0;
  t.tm_mon = -1;
  t.tm_year = std::numeric_limits<int>::min ();
  t.tm_wday = 0;
  t.tm_yday = 0;
  t.tm_isdst = 0;

#if defined (HAVE_STRUCT_TM_GMTOFF)
  t.tm_gmtoff = 0;
#endif

#if defined (HAVE_STRUCT_TM_TM_ZONE)
  char *ps = strsave ("");
  t.tm_zone = ps;
#endif

  const char *p = str.c_str ();

  char *q = gnulib::strptime (p, fmt.c_str (), &t);

  // Fill in wday and yday, but only if mday is valid and the mon and year
  // are filled in, avoiding issues with mktime and invalid dates.
  if (t.tm_mday != 0 && t.tm_mon >= 0
      && t.tm_year != std::numeric_limits<int>::min ())
    {
      t.tm_isdst = -1;
      gnulib::mktime (&t);
    }

  if (t.tm_mon < 0)
    t.tm_mon = 0;

  if (t.tm_year == std::numeric_limits<int>::min ())
    t.tm_year = 0;

  if (q)
    nchars = q - p + 1;
  else
    nchars = 0;

  octave_base_tm::init (&t);

#if defined (HAVE_STRUCT_TM_TM_ZONE)
  delete [] ps;
#endif
}
