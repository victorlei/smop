# logf.m4 serial 6
dnl Copyright (C) 2011-2015 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

AC_DEFUN([gl_FUNC_LOGF],
[
  m4_divert_text([DEFAULTS], [gl_logf_required=plain])
  AC_REQUIRE([gl_MATH_H_DEFAULTS])
  AC_REQUIRE([gl_FUNC_LOG])

  dnl Persuade glibc <math.h> to declare logf().
  AC_REQUIRE([gl_USE_SYSTEM_EXTENSIONS])

  dnl Test whether logf() exists. Assume that logf(), if it exists, is
  dnl defined in the same library as log().
  save_LIBS="$LIBS"
  LIBS="$LIBS $LOG_LIBM"
  AC_CHECK_FUNCS([logf])
  LIBS="$save_LIBS"
  if test $ac_cv_func_logf = yes; then
    LOGF_LIBM="$LOG_LIBM"

    save_LIBS="$LIBS"
    LIBS="$LIBS $LOGF_LIBM"
    gl_FUNC_LOGF_WORKS
    LIBS="$save_LIBS"
    case "$gl_cv_func_logf_works" in
      *yes) ;;
      *) REPLACE_LOGF=1 ;;
    esac

    m4_ifdef([gl_FUNC_LOGF_IEEE], [
      if test $gl_logf_required = ieee && test $REPLACE_LOGF = 0; then
        AC_REQUIRE([AC_CANONICAL_HOST]) dnl for cross-compiles
        AC_CACHE_CHECK([whether logf works according to ISO C 99 with IEC 60559],
          [gl_cv_func_logf_ieee],
          [
            save_LIBS="$LIBS"
            LIBS="$LIBS $LOGF_LIBM"
            AC_RUN_IFELSE(
              [AC_LANG_SOURCE([[
#ifndef __NO_MATH_INLINES
# define __NO_MATH_INLINES 1 /* for glibc */
#endif
#include <math.h>
/* Compare two numbers with ==.
   This is a separate function because IRIX 6.5 "cc -O" miscompiles an
   'x == x' test.  */
static int
numeric_equal (float x, float y)
{
  return x == y;
}
static float dummy (float x) { return 0; }
int main (int argc, char *argv[])
{
  float (*my_logf) (float) = argc ? logf : dummy;
  /* Test logf(negative).
     This test fails on NetBSD 5.1.  */
  float y = my_logf (-1.0f);
  if (numeric_equal (y, y))
    return 1;
  return 0;
}
              ]])],
              [gl_cv_func_logf_ieee=yes],
              [gl_cv_func_logf_ieee=no],
              [case "$host_os" in
                         # Guess yes on glibc systems.
                 *-gnu*) gl_cv_func_logf_ieee="guessing yes" ;;
                         # If we don't know, assume the worst.
                 *)      gl_cv_func_logf_ieee="guessing no" ;;
               esac
              ])
            LIBS="$save_LIBS"
          ])
        case "$gl_cv_func_logf_ieee" in
          *yes) ;;
          *) REPLACE_LOGF=1 ;;
        esac
      fi
    ])
  else
    HAVE_LOGF=0
  fi
  if test $HAVE_LOGF = 0 || test $REPLACE_LOGF = 1; then
    dnl Find libraries needed to link lib/logf.c.
    if test $HAVE_LOGF = 0; then
      LOGF_LIBM="$LOG_LIBM"
    fi
  fi
  AC_SUBST([LOGF_LIBM])
])

dnl Test whether logf() works.
dnl On OSF/1 5.1, logf(-0.0f) is NaN.
AC_DEFUN([gl_FUNC_LOGF_WORKS],
[
  AC_REQUIRE([AC_PROG_CC])
  AC_REQUIRE([AC_CANONICAL_HOST]) dnl for cross-compiles
  AC_CACHE_CHECK([whether logf works], [gl_cv_func_logf_works],
    [
      AC_RUN_IFELSE(
        [AC_LANG_SOURCE([[
#include <math.h>
volatile float x;
float y;
int main ()
{
  x = -0.0f;
  y = logf (x);
  if (!(y + y == y))
    return 1;
  return 0;
}
]])],
        [gl_cv_func_logf_works=yes],
        [gl_cv_func_logf_works=no],
        [case "$host_os" in
           osf*) gl_cv_func_logf_works="guessing no";;
           *)    gl_cv_func_logf_works="guessing yes";;
         esac
        ])
    ])
])
