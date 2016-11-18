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

#include <stdio.h>
#include <time.h>

#include <sys/types.h>
#include <unistd.h>

#include "cutils.h"

void
octave_sleep (unsigned int seconds)
{
  sleep (seconds);
}

void
octave_usleep (unsigned int useconds)
{
  struct timespec delay;
  struct timespec remaining;

  unsigned int sec = useconds / 1000000;
  unsigned int usec = useconds % 1000000;

  delay.tv_sec = sec;
  delay.tv_nsec = usec * 1000;

  nanosleep (&delay, &remaining);
}

