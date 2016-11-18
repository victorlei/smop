# round.m4 serial 16
dnl Copyright (C) 2007, 2009-2015 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

AC_DEFUN([gl_FUNC_ROUND],
[
  m4_divert_text([DEFAULTS], [gl_round_required=plain])
  AC_REQUIRE([gl_MATH_H_DEFAULTS])

  dnl Persuade glibc <math.h> to declare round().
  AC_REQUIRE([gl_USE_SYSTEM_EXTENSIONS])

  gl_CHECK_MATH_LIB([ROUND_LIBM], [x = round (x);],
    [extern
     #ifdef __cplusplus
     "C"
     #endif
     double round (double);
    ])
  if test "$ROUND_LIBM" != missing; then
    HAVE_ROUND=1
    dnl Also check whether it's declared.
    dnl IRIX 6.5 has round() in libm but doesn't declare it in <math.h>.
    AC_CHECK_DECLS([round], , [HAVE_DECL_ROUND=0], [[#include <math.h>]])

    dnl Test whether round() produces correct results. On NetBSD 3.0, for
    dnl x = 1/2 - 2^-54, the system's round() returns a wrong result.
    AC_REQUIRE([AC_PROG_CC])
    AC_REQUIRE([AC_CANONICAL_HOST]) dnl for cross-compiles
    AC_CACHE_CHECK([whether round works], [gl_cv_func_round_works],
      [
        save_LIBS="$LIBS"
        LIBS="$LIBS $ROUND_LIBM"
        AC_RUN_IFELSE([AC_LANG_SOURCE([[
#include <float.h>
#include <math.h>
extern
#ifdef __cplusplus
"C"
#endif
double round (double);
#ifdef _MSC_VER
# pragma fenv_access (off)
#endif
int main()
{
  /* 2^DBL_MANT_DIG.  */
  static const double TWO_MANT_DIG =
    /* Assume DBL_MANT_DIG <= 5 * 31.
       Use the identity
       n = floor(n/5) + floor((n+1)/5) + ... + floor((n+4)/5).  */
    (double) (1U << (DBL_MANT_DIG / 5))
    * (double) (1U << ((DBL_MANT_DIG + 1) / 5))
    * (double) (1U << ((DBL_MANT_DIG + 2) / 5))
    * (double) (1U << ((DBL_MANT_DIG + 3) / 5))
    * (double) (1U << ((DBL_MANT_DIG + 4) / 5));
  volatile double x = 0.5 - 0.5 / TWO_MANT_DIG;
  exit (x < 0.5 && round (x) != 0.0);
}]])], [gl_cv_func_round_works=yes], [gl_cv_func_round_works=no],
        [case "$host_os" in
           netbsd* | aix*) gl_cv_func_round_works="guessing no";;
           *)              gl_cv_func_round_works="guessing yes";;
         esac
        ])
        LIBS="$save_LIBS"
      ])
    case "$gl_cv_func_round_works" in
      *no) REPLACE_ROUND=1 ;;
    esac

    m4_ifdef([gl_FUNC_ROUND_IEEE], [
      if test $gl_round_required = ieee && test $REPLACE_ROUND = 0; then
        AC_REQUIRE([AC_CANONICAL_HOST]) dnl for cross-compiles
        AC_CACHE_CHECK([whether round works according to ISO C 99 with IEC 60559],
          [gl_cv_func_round_ieee],
          [
            save_LIBS="$LIBS"
            LIBS="$LIBS $ROUND_LIBM"
            AC_RUN_IFELSE(
              [AC_LANG_SOURCE([[
#ifndef __NO_MATH_INLINES
# define __NO_MATH_INLINES 1 /* for glibc */
#endif
#include <math.h>
extern
#ifdef __cplusplus
"C"
#endif
double round (double);
]gl_DOUBLE_MINUS_ZERO_CODE[
]gl_DOUBLE_SIGNBIT_CODE[
static double dummy (double f) { return 0; }
int main (int argc, char *argv[])
{
  double (*my_round) (double) = argc ? round : dummy;
  /* Test whether round (-0.0) is -0.0.  */
  if (signbitd (minus_zerod) && !signbitd (my_round (minus_zerod)))
    return 1;
  return 0;
}
              ]])],
              [gl_cv_func_round_ieee=yes],
              [gl_cv_func_round_ieee=no],
              [case "$host_os" in
                         # Guess yes on glibc systems.
                 *-gnu*) gl_cv_func_round_ieee="guessing yes" ;;
                         # If we don't know, assume the worst.
                 *)      gl_cv_func_round_ieee="guessing no" ;;
               esac
              ])
            LIBS="$save_LIBS"
          ])
        case "$gl_cv_func_round_ieee" in
          *yes) ;;
          *) REPLACE_ROUND=1 ;;
        esac
      fi
    ])
  else
    HAVE_ROUND=0
    HAVE_DECL_ROUND=0
  fi
  if test $HAVE_ROUND = 0 || test $REPLACE_ROUND = 1; then
    dnl Find libraries needed to link lib/round.c.
    gl_FUNC_FLOOR_LIBS
    gl_FUNC_CEIL_LIBS
    ROUND_LIBM=
    dnl Append $FLOOR_LIBM to ROUND_LIBM, avoiding gratuitous duplicates.
    case " $ROUND_LIBM " in
      *" $FLOOR_LIBM "*) ;;
      *) ROUND_LIBM="$ROUND_LIBM $FLOOR_LIBM" ;;
    esac
    dnl Append $CEIL_LIBM to ROUND_LIBM, avoiding gratuitous duplicates.
    case " $ROUND_LIBM " in
      *" $CEIL_LIBM "*) ;;
      *) ROUND_LIBM="$ROUND_LIBM $CEIL_LIBM" ;;
    esac
  fi
  AC_SUBST([ROUND_LIBM])
])
