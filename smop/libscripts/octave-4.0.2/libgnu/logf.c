/* Natural logarithmic function.
   Copyright (C) 2011-2015 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

#include <config.h>

/* Specification.  */
#include <math.h>

float
logf (float x)
#undef logf
{
#if HAVE_LOGF
  if (x <= 0.0f)
    {
      /* Work around the OSF/1 5.1 bug.  */
      if (x == 0.0f)
        /* Return -Infinity.  */
        return -1.0f / 0.0f;
      /* Work around the NetBSD 5.1 bug.  */
      else /* x < 0.0 */
        /* Return NaN.  */
        return 0.0f / 0.0f;
    }
  return logf (x);
#else
  return (float) log ((double) x);
#endif
}
