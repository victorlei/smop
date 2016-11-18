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

#include <sys/time.h>
#include <sys/times.h>
#include <sys/types.h>

#ifdef HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#endif

#if defined (HAVE_SYS_PARAM_H)
#include <sys/param.h>
#endif

#include "defun.h"
#include "oct-map.h"
#include "sysdep.h"
#include "ov.h"
#include "oct-obj.h"
#include "utils.h"

#if !defined (HZ)
#if defined (CLK_TCK)
#define HZ CLK_TCK
#elif defined (USG)
#define HZ 100
#else
#define HZ 60
#endif
#endif

#ifndef RUSAGE_SELF
#define RUSAGE_SELF 0
#endif

// System resource functions.

DEFUN (getrusage, , ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} getrusage ()\n\
Return a structure containing a number of statistics about the current\n\
Octave process.\n\
\n\
Not all fields are available on all systems.  If it is not possible to get\n\
CPU time statistics, the CPU time slots are set to zero.  Other missing data\n\
are replaced by NaN@.  The list of possible fields is:\n\
\n\
@table @code\n\
@item idrss\n\
Unshared data size.\n\
\n\
@item inblock\n\
Number of block input operations.\n\
\n\
@item isrss\n\
Unshared stack size.\n\
\n\
@item ixrss\n\
Shared memory size.\n\
\n\
@item majflt\n\
Number of major page faults.\n\
\n\
@item maxrss\n\
Maximum data size.\n\
\n\
@item minflt\n\
Number of minor page faults.\n\
\n\
@item msgrcv\n\
Number of messages received.\n\
\n\
@item msgsnd\n\
Number of messages sent.\n\
\n\
@item nivcsw\n\
Number of involuntary context switches.\n\
\n\
@item nsignals\n\
Number of signals received.\n\
\n\
@item nswap\n\
Number of swaps.\n\
\n\
@item nvcsw\n\
Number of voluntary context switches.\n\
\n\
@item oublock\n\
Number of block output operations.\n\
\n\
@item stime\n\
A structure containing the system CPU time used.  The structure has the\n\
elements @code{sec} (seconds) @code{usec} (microseconds).\n\
\n\
@item utime\n\
A structure containing the user CPU time used.  The structure has the\n\
elements @code{sec} (seconds) @code{usec} (microseconds).\n\
@end table\n\
@end deftypefn")
{
  octave_scalar_map m;
  octave_scalar_map tv_tmp;

  // FIXME: maybe encapsulate all of this in a liboctave class
#if defined (HAVE_GETRUSAGE)

  struct rusage ru;

  getrusage (RUSAGE_SELF, &ru);

  tv_tmp.assign ("sec", static_cast<double> (ru.ru_utime.tv_sec));
  tv_tmp.assign ("usec", static_cast<double> (ru.ru_utime.tv_usec));
  m.assign ("utime", octave_value (tv_tmp));

  tv_tmp.assign ("sec", static_cast<double> (ru.ru_stime.tv_sec));
  tv_tmp.assign ("usec", static_cast<double> (ru.ru_stime.tv_usec));
  m.assign ("stime", octave_value (tv_tmp));

#if ! defined (RUSAGE_TIMES_ONLY)
  m.assign ("maxrss", static_cast<double> (ru.ru_maxrss));
  m.assign ("ixrss", static_cast<double> (ru.ru_ixrss));
  m.assign ("idrss", static_cast<double> (ru.ru_idrss));
  m.assign ("isrss", static_cast<double> (ru.ru_isrss));
  m.assign ("minflt", static_cast<double> (ru.ru_minflt));
  m.assign ("majflt", static_cast<double> (ru.ru_majflt));
  m.assign ("nswap", static_cast<double> (ru.ru_nswap));
  m.assign ("inblock", static_cast<double> (ru.ru_inblock));
  m.assign ("oublock", static_cast<double> (ru.ru_oublock));
  m.assign ("msgsnd", static_cast<double> (ru.ru_msgsnd));
  m.assign ("msgrcv", static_cast<double> (ru.ru_msgrcv));
  m.assign ("nsignals", static_cast<double> (ru.ru_nsignals));
  m.assign ("nvcsw", static_cast<double> (ru.ru_nvcsw));
  m.assign ("nivcsw", static_cast<double> (ru.ru_nivcsw));
#endif

#else

  struct tms t;

  times (&t);

  unsigned long ticks;
  unsigned long seconds;
  unsigned long fraction;

  ticks = t.tms_utime + t.tms_cutime;
  fraction = ticks % HZ;
  seconds = ticks / HZ;

  tv_tmp.assign ("sec", static_cast<double> (seconds));
  tv_tmp.assign ("usec", static_cast<double> (fraction * 1e6 / HZ));
  m.assign ("utime", octave_value (tv_tmp));

  ticks = t.tms_stime + t.tms_cstime;
  fraction = ticks % HZ;
  seconds = ticks / HZ;

  tv_tmp.assign ("sec", static_cast<double> (seconds));
  tv_tmp.assign ("usec", static_cast<double> (fraction * 1e6 / HZ));
  m.assign ("stime", octave_value (tv_tmp));

  double tmp = lo_ieee_nan_value ();

  m.assign ("maxrss", tmp);
  m.assign ("ixrss", tmp);
  m.assign ("idrss", tmp);
  m.assign ("isrss", tmp);
  m.assign ("minflt", tmp);
  m.assign ("majflt", tmp);
  m.assign ("nswap", tmp);
  m.assign ("inblock", tmp);
  m.assign ("oublock", tmp);
  m.assign ("msgsnd", tmp);
  m.assign ("msgrcv", tmp);
  m.assign ("nsignals", tmp);
  m.assign ("nvcsw", tmp);
  m.assign ("nivcsw", tmp);

#endif

  return octave_value (m);
}
