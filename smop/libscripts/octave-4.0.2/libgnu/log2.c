/* Base 2 logarithm.
   Copyright (C) 2012-2015 Free Software Foundation, Inc.

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

/* Best possible approximation of log(2) as a 'double'.  */
#define LOG2 0.693147180559945309417232121458176568075

/* Best possible approximation of 1/log(2) as a 'double'.  */
#define LOG2_INVERSE 1.44269504088896340735992468100189213743

/* sqrt(0.5).  */
#define SQRT_HALF 0.707106781186547524400844362104849039284

double
log2 (double x)
{
  if (isnand (x))
    return x;

  if (x <= 0.0)
    {
      if (x == 0.0)
        /* Return -Infinity.  */
        return - HUGE_VAL;
      else
        {
          /* Return NaN.  */
#if defined _MSC_VER || (defined __sgi && !defined __GNUC__)
          static double zero;
          return zero / zero;
#else
          return 0.0 / 0.0;
#endif
        }
    }

  /* Decompose x into
       x = 2^e * y
     where
       e is an integer,
       1/2 < y < 2.
     Then log2(x) = e + log2(y) = e + log(y)/log(2).  */
  {
    int e;
    double y;

    y = frexp (x, &e);
    if (y < SQRT_HALF)
      {
        y = 2.0 * y;
        e = e - 1;
      }

    return (double) e + log (y) * LOG2_INVERSE;
  }
}
