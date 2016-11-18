/* Round toward nearest, breaking ties away from zero.
   Copyright (C) 2007, 2010-2015 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License along
   with this program; if not, see <http://www.gnu.org/licenses/>.  */

/* Written by Ben Pfaff <blp@gnu.org>, 2007.
   Based heavily on code by Bruno Haible. */

#if ! defined USE_LONG_DOUBLE
# include <config.h>
#endif

/* Specification.  */
#include <math.h>

#include <float.h>

#undef MIN

#ifdef USE_LONG_DOUBLE
# define ROUND roundl
# define FLOOR floorl
# define CEIL ceill
# define DOUBLE long double
# define MANT_DIG LDBL_MANT_DIG
# define MIN LDBL_MIN
# define L_(literal) literal##L
# define HAVE_FLOOR_AND_CEIL HAVE_FLOORL_AND_CEILL
#elif ! defined USE_FLOAT
# define ROUND round
# define FLOOR floor
# define CEIL ceil
# define DOUBLE double
# define MANT_DIG DBL_MANT_DIG
# define MIN DBL_MIN
# define L_(literal) literal
# define HAVE_FLOOR_AND_CEIL 1
#else /* defined USE_FLOAT */
# define ROUND roundf
# define FLOOR floorf
# define CEIL ceilf
# define DOUBLE float
# define MANT_DIG FLT_MANT_DIG
# define MIN FLT_MIN
# define L_(literal) literal##f
# define HAVE_FLOOR_AND_CEIL HAVE_FLOORF_AND_CEILF
#endif

/* -0.0.  See minus-zero.h.  */
#if defined __hpux || defined __sgi || defined __ICC
# define MINUS_ZERO (-MIN * MIN)
#else
# define MINUS_ZERO L_(-0.0)
#endif

/* MSVC with option -fp:strict refuses to compile constant initializers that
   contain floating-point operations.  Pacify this compiler.  */
#ifdef _MSC_VER
# pragma fenv_access (off)
#endif

/* If we're being included from test-round2[f].c, it already defined names for
   our round implementations.  Otherwise, pick the preferred implementation for
   this machine. */
#if !defined FLOOR_BASED_ROUND && !defined FLOOR_FREE_ROUND
# if HAVE_FLOOR_AND_CEIL
#  define FLOOR_BASED_ROUND ROUND
# else
#  define FLOOR_FREE_ROUND ROUND
# endif
#endif

#ifdef FLOOR_BASED_ROUND
/* An implementation of the C99 round function based on floor and ceil.  We use
   this when floor and ceil are available, on the assumption that they are
   faster than the open-coded versions below. */
DOUBLE
FLOOR_BASED_ROUND (DOUBLE x)
{
  if (x >= L_(0.0))
    {
      DOUBLE y = FLOOR (x);
      if (x - y >= L_(0.5))
        y += L_(1.0);
      return y;
    }
  else
    {
      DOUBLE y = CEIL (x);
      if (y - x >= L_(0.5))
        y -= L_(1.0);
      return y;
    }
}
#endif /* FLOOR_BASED_ROUND */

#ifdef FLOOR_FREE_ROUND
/* An implementation of the C99 round function without floor or ceil.
   We use this when floor or ceil is missing. */
DOUBLE
FLOOR_FREE_ROUND (DOUBLE x)
{
  /* 2^(MANT_DIG-1).  */
  static const DOUBLE TWO_MANT_DIG =
    /* Assume MANT_DIG <= 5 * 31.
       Use the identity
       n = floor(n/5) + floor((n+1)/5) + ... + floor((n+4)/5).  */
    (DOUBLE) (1U << ((MANT_DIG - 1) / 5))
    * (DOUBLE) (1U << ((MANT_DIG - 1 + 1) / 5))
    * (DOUBLE) (1U << ((MANT_DIG - 1 + 2) / 5))
    * (DOUBLE) (1U << ((MANT_DIG - 1 + 3) / 5))
    * (DOUBLE) (1U << ((MANT_DIG - 1 + 4) / 5));

  /* The use of 'volatile' guarantees that excess precision bits are dropped at
     each addition step and before the following comparison at the caller's
     site.  It is necessary on x86 systems where double-floats are not IEEE
     compliant by default, to avoid that the results become platform and
     compiler option dependent.  'volatile' is a portable alternative to gcc's
     -ffloat-store option.  */
  volatile DOUBLE y = x;
  volatile DOUBLE z = y;

  if (z > L_(0.0))
    {
      /* Avoid rounding error for x = 0.5 - 2^(-MANT_DIG-1).  */
      if (z < L_(0.5))
        z = L_(0.0);
      /* Avoid rounding errors for values near 2^k, where k >= MANT_DIG-1.  */
      else if (z < TWO_MANT_DIG)
        {
          /* Add 0.5 to the absolute value.  */
          y = z += L_(0.5);
          /* Round to the next integer (nearest or up or down, doesn't
             matter).  */
          z += TWO_MANT_DIG;
          z -= TWO_MANT_DIG;
          /* Enforce rounding down.  */
          if (z > y)
            z -= L_(1.0);
        }
    }
  else if (z < L_(0.0))
    {
      /* Avoid rounding error for x = -(0.5 - 2^(-MANT_DIG-1)).  */
      if (z > - L_(0.5))
        z = MINUS_ZERO;
      /* Avoid rounding errors for values near -2^k, where k >= MANT_DIG-1.  */
      else if (z > -TWO_MANT_DIG)
        {
          /* Add 0.5 to the absolute value.  */
          y = z -= L_(0.5);
          /* Round to the next integer (nearest or up or down, doesn't
             matter).  */
          z -= TWO_MANT_DIG;
          z += TWO_MANT_DIG;
          /* Enforce rounding up.  */
          if (z < y)
            z += L_(1.0);
        }
    }
  return z;
}
#endif /* FLOOR_FREE_ROUND */
