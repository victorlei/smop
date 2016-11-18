/* Get process times

   Copyright (C) 2008-2015 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, see <http://www.gnu.org/licenses/>.  */

/* Written by Simon Josefsson <simon@josefsson.org>, 2008.  */

#include <config.h>

/* Get times prototype. */
#include <sys/times.h>

/* Get round. */
#include <math.h>

/* Get GetProcessTimes etc. */
#include <windows.h>

static clock_t
filetime2clock (FILETIME time)
{
  float f;

  /* We have a 64-bit value, in the form of two DWORDS aka unsigned
     int, counting the number of 100-nanosecond intervals.  We need to
     convert these to clock ticks.  Older POSIX uses CLK_TCK to
     indicate the number of clock ticks per second while modern POSIX
     uses sysconf(_SC_CLK_TCK).  Mingw32 does not appear to have
     sysconf(_SC_CLK_TCK), but appears to have CLK_TCK = 1000 so we
     use it.  Note that CLOCKS_PER_SEC constant does not apply here,
     it is for use with the clock function.  */

  f = (unsigned long long) time.dwHighDateTime << 32;
  f += time.dwLowDateTime;
  f = f * CLK_TCK / 10000000;
  return (clock_t) round (f);
}

clock_t
times (struct tms * buffer)
{
  FILETIME creation_time, exit_time, kernel_time, user_time;

  if (GetProcessTimes (GetCurrentProcess (), &creation_time, &exit_time,
                       &kernel_time, &user_time) == 0)
    return (clock_t) -1;

  buffer->tms_utime = filetime2clock (user_time);
  buffer->tms_stime = filetime2clock (kernel_time);
  buffer->tms_cutime = 0;
  buffer->tms_cstime = 0;

  return clock ();
}
