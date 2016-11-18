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

#if !defined (octave_oct_time_h)
#define octave_oct_time_h 1

#include <ctime>
#include <string>

#include "lo-math.h"

class octave_base_tm;

class
OCTAVE_API
octave_time
{
public:

  octave_time (void)
    : ot_unix_time (0), ot_usec (0) { stamp (); }

  octave_time (time_t t)
    : ot_unix_time (t), ot_usec (0) { }

  octave_time (time_t t, int us)
    : ot_unix_time (t), ot_usec ()
  {
    int rem, extra;

    if (us >= 0)
      {
        rem = us % 1000000;
        extra = (us - rem) / 1000000;
      }
    else
      {
        us = -us;
        rem = us % 1000000;
        extra = - (1 + (us - rem) / 1000000);
        rem = 1000000 - us % 1000000;
      }

    ot_usec = rem;
    ot_unix_time += extra;
  }

  octave_time (double d)
    : ot_unix_time (static_cast<time_t> (d)), ot_usec (0)
  {
    double ip;
    ot_usec = static_cast<int> (std::modf (d, &ip) * 1e6);
  }

  octave_time (const octave_base_tm& tm);

  octave_time (const octave_time& ot)
    : ot_unix_time (ot.ot_unix_time), ot_usec (ot.ot_usec) { }

  octave_time& operator = (const octave_time& ot)
  {
    if (this != &ot)
      {
        ot_unix_time = ot.ot_unix_time;
        ot_usec = ot.ot_usec;
      }

    return *this;
  }

  ~octave_time (void) { }

  void stamp (void);

  double double_value (void) const { return ot_unix_time + ot_usec / 1e6; }

  time_t unix_time (void) const { return ot_unix_time; }

  int usec (void) const { return ot_usec; }

  std::string ctime (void) const;

private:

  // Seconds since the epoch.
  time_t ot_unix_time;

  // Additional microseconds.
  int ot_usec;
};

inline bool
operator == (const octave_time& t1, const octave_time& t2)
{
  return (t1.unix_time () == t2.unix_time () && t1.usec () == t2.usec ());
}

inline bool
operator != (const octave_time& t1, const octave_time& t2)
{
  return ! (t1 == t2);
}

inline bool
operator < (const octave_time& t1, const octave_time& t2)
{
  if (t1.unix_time () < t2.unix_time ())
    return true;
  else if (t1.unix_time () > t2.unix_time ())
    return false;
  else if (t1.usec () < t2.usec ())
    return true;
  else
    return false;
}

inline bool
operator <= (const octave_time& t1, const octave_time& t2)
{
  return (t1 < t2 || t1 == t2);
}

inline bool
operator > (const octave_time& t1, const octave_time& t2)
{
  if (t1.unix_time () > t2.unix_time ())
    return true;
  else if (t1.unix_time () < t2.unix_time ())
    return false;
  else if (t1.usec () > t2.usec ())
    return true;
  else
    return false;
}

inline bool
operator >= (const octave_time& t1, const octave_time& t2)
{
  return (t1 > t2 || t1 == t2);
}

inline octave_time
operator + (const octave_time& t1, const octave_time& t2)
{
  return octave_time (t1.unix_time () + t2.unix_time (),
                      t1.usec () + t2.usec ());
}

class
OCTAVE_API
octave_base_tm
{
public:

  octave_base_tm (void)
    : tm_usec (0), tm_sec (0), tm_min (0), tm_hour (0),
      tm_mday (0), tm_mon (0), tm_year (0), tm_wday (0),
      tm_yday (0), tm_isdst (0), tm_gmtoff (0), tm_zone ("unknown")
  { }

  octave_base_tm (const octave_base_tm& tm)
    : tm_usec (tm.tm_usec), tm_sec (tm.tm_sec), tm_min (tm.tm_min),
      tm_hour (tm.tm_hour), tm_mday (tm.tm_mday), tm_mon (tm.tm_mon),
      tm_year (tm.tm_year), tm_wday (tm.tm_wday), tm_yday (tm.tm_yday),
      tm_isdst (tm.tm_isdst), tm_gmtoff (tm.tm_gmtoff), tm_zone (tm.tm_zone)
  { }

  octave_base_tm& operator = (const octave_base_tm& tm)
  {
    if (this != &tm)
      {
        tm_usec = tm.tm_usec;
        tm_sec = tm.tm_sec;
        tm_min = tm.tm_min;
        tm_hour = tm.tm_hour;
        tm_mday = tm.tm_mday;
        tm_mon = tm.tm_mon;
        tm_year = tm.tm_year;
        tm_wday = tm.tm_wday;
        tm_yday = tm.tm_yday;
        tm_isdst = tm.tm_isdst;
        tm_gmtoff = tm.tm_gmtoff;
        tm_zone = tm.tm_zone;
      }

    return *this;
  }

  virtual ~octave_base_tm (void) { }

  int usec (void) const { return tm_usec; }
  int sec (void) const { return tm_sec; }
  int min (void) const { return tm_min; }
  int hour (void) const { return tm_hour; }
  int mday (void) const { return tm_mday; }
  int mon (void) const { return tm_mon; }
  int year (void) const { return tm_year; }
  int wday (void) const { return tm_wday; }
  int yday (void) const { return tm_yday; }
  int isdst (void) const { return tm_isdst; }
  long gmtoff (void) const { return tm_gmtoff; }
  std::string zone (void) const { return tm_zone; }

  octave_base_tm& usec (int v);
  octave_base_tm& sec (int v);
  octave_base_tm& min (int v);
  octave_base_tm& hour (int v);
  octave_base_tm& mday (int v);
  octave_base_tm& mon (int v);
  octave_base_tm& year (int v);
  octave_base_tm& wday (int v);
  octave_base_tm& yday (int v);
  octave_base_tm& isdst (int v);
  octave_base_tm& gmtoff (long v);
  octave_base_tm& zone (const std::string& s);

  std::string strftime (const std::string& fmt) const;

  std::string asctime (void) const
  { return strftime ("%a %b %d %H:%M:%S %Y\n"); }

protected:

  // Microseconds after the second (0, 999999).
  int tm_usec;

  // Seconds after the minute (0, 61).
  int tm_sec;

  // Minutes after the hour (0, 59).
  int tm_min;

  // Hours since midnight (0, 23).
  int tm_hour;

  // Day of the month (1, 31).
  int tm_mday;

  // Months since January (0, 11).
  int tm_mon;

  // Years since 1900.
  int tm_year;

  // Days since Sunday (0, 6).
  int tm_wday;

  // Days since January 1 (0, 365).
  int tm_yday;

  // Daylight Savings Time flag.
  int tm_isdst;

  // Time zone.
  long tm_gmtoff;

  // Time zone.
  std::string tm_zone;

  void init (void *p);
};

class
OCTAVE_API
octave_localtime : public octave_base_tm
{
public:

  octave_localtime (void)
    : octave_base_tm () { init (octave_time ()); }

  octave_localtime (const octave_time& ot)
    : octave_base_tm () { init (ot); }

  octave_localtime (const octave_localtime& t)
    : octave_base_tm (t) { }

  octave_localtime& operator = (const octave_localtime& t)
  {
    octave_base_tm::operator = (t);
    return *this;
  }

  ~octave_localtime (void) { }

private:

  void init (const octave_time& ot);
};

class
OCTAVE_API
octave_gmtime : public octave_base_tm
{
public:

  octave_gmtime (void)
    : octave_base_tm () { init (octave_time ()); }

  octave_gmtime (const octave_time& ot)
    : octave_base_tm () { init (ot); }

  octave_gmtime& operator = (const octave_gmtime& t)
  {
    octave_base_tm::operator = (t);
    return *this;
  }

  ~octave_gmtime (void) { }

private:

  void init (const octave_time& ot);
};

class
OCTAVE_API
octave_strptime : public octave_base_tm
{
public:

  octave_strptime (const std::string& str, const std::string& fmt)
    : octave_base_tm (), nchars (0)
  {
    init (str, fmt);
  }

  octave_strptime (const octave_strptime& s)
    : octave_base_tm (s), nchars (s.nchars) { }

  octave_strptime& operator = (const octave_strptime& s)
  {
    octave_base_tm::operator = (s);
    nchars = s.nchars;
    return *this;
  }

  int characters_converted (void) const { return nchars; }

  ~octave_strptime (void) { }

private:

  int nchars;

  void init (const std::string& str, const std::string& fmt);
};

#endif
