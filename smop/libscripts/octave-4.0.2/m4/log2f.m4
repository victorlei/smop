# log2f.m4 serial 3
dnl Copyright (C) 2010-2015 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

AC_DEFUN([gl_FUNC_LOG2F],
[
  m4_divert_text([DEFAULTS], [gl_log2f_required=plain])
  AC_REQUIRE([gl_MATH_H_DEFAULTS])
  AC_REQUIRE([gl_FUNC_LOG2])

  dnl Persuade glibc <math.h> to declare log2f().
  AC_REQUIRE([gl_USE_SYSTEM_EXTENSIONS])

  dnl Determine LOG2F_LIBM.
  gl_COMMON_DOUBLE_MATHFUNC([log2f])

  dnl Test whether log2f() exists.
  save_LIBS="$LIBS"
  LIBS="$LIBS $LOG2F_LIBM"
  AC_CHECK_FUNCS([log2f])
  LIBS="$save_LIBS"
  if test $ac_cv_func_log2f = yes; then
    HAVE_LOG2F=1
    dnl Also check whether it's declared.
    dnl IRIX 6.5 has log2f() in libm but doesn't declare it in <math.h>.
    AC_CHECK_DECL([log2f], , [HAVE_DECL_LOG2F=0], [[#include <math.h>]])

    save_LIBS="$LIBS"
    LIBS="$LIBS $LOG2F_LIBM"
    gl_FUNC_LOG2F_WORKS
    LIBS="$save_LIBS"
    case "$gl_cv_func_log2f_works" in
      *yes) ;;
      *) REPLACE_LOG2F=1 ;;
    esac

    m4_ifdef([gl_FUNC_LOG2F_IEEE], [
      if test $gl_log2f_required = ieee && test $REPLACE_LOG2F = 0; then
        AC_REQUIRE([AC_CANONICAL_HOST]) dnl for cross-compiles
        AC_CACHE_CHECK([whether log2f works according to ISO C 99 with IEC 60559],
          [gl_cv_func_log2f_ieee],
          [
            save_LIBS="$LIBS"
            LIBS="$LIBS $LOG2F_LIBM"
            AC_RUN_IFELSE(
              [AC_LANG_SOURCE([[
#ifndef __NO_MATH_INLINES
# define __NO_MATH_INLINES 1 /* for glibc */
#endif
#include <math.h>
#ifndef log2f /* for Cygwin 1.7.x */
extern
#ifdef __cplusplus
"C"
#endif
float log2f (float);
#endif
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
  float (*my_log2f) (float) = argc ? log2f : dummy;
  /* Test log2f(negative).
     This test fails on NetBSD 5.1 and Solaris 11 2011-11.  */
  float y = my_log2f (-1.0f);
  if (numeric_equal (y, y))
    return 1;
  return 0;
}
              ]])],
              [gl_cv_func_log2f_ieee=yes],
              [gl_cv_func_log2f_ieee=no],
              [case "$host_os" in
                         # Guess yes on glibc systems.
                 *-gnu*) gl_cv_func_log2f_ieee="guessing yes" ;;
                         # If we don't know, assume the worst.
                 *)      gl_cv_func_log2f_ieee="guessing no" ;;
               esac
              ])
            LIBS="$save_LIBS"
          ])
        case "$gl_cv_func_log2f_ieee" in
          *yes) ;;
          *) REPLACE_LOG2F=1 ;;
        esac
      fi
    ])
  else
    HAVE_LOG2F=0
    HAVE_DECL_LOG2F=0
  fi
  if test $HAVE_LOG2F = 0 || test $REPLACE_LOG2F = 1; then
    dnl Find libraries needed to link lib/log2f.c.
    if test $ac_cv_func_log2 = yes; then
      AC_DEFINE([HAVE_LOG2], [1],
        [Define to 1 if the log2() function is available in libc or libm.])
      LOG2F_LIBM="$LOG2_LIBM"
    else
      AC_REQUIRE([gl_FUNC_ISNANF])
      AC_REQUIRE([gl_FUNC_FREXPF])
      AC_REQUIRE([gl_FUNC_LOGF])
      LOG2F_LIBM=
      dnl Append $ISNANF_LIBM to LOG2F_LIBM, avoiding gratuitous duplicates.
      case " $LOG2F_LIBM " in
        *" $ISNANF_LIBM "*) ;;
        *) LOG2F_LIBM="$LOG2F_LIBM $ISNANF_LIBM" ;;
      esac
      dnl Append $FREXPF_LIBM to LOG2F_LIBM, avoiding gratuitous duplicates.
      case " $LOG2F_LIBM " in
        *" $FREXPF_LIBM "*) ;;
        *) LOG2F_LIBM="$LOG2F_LIBM $FREXPF_LIBM" ;;
      esac
      dnl Append $LOGF_LIBM to LOG2F_LIBM, avoiding gratuitous duplicates.
      case " $LOG2F_LIBM " in
        *" $LOGF_LIBM "*) ;;
        *) LOG2F_LIBM="$LOG2F_LIBM $LOGF_LIBM" ;;
      esac
    fi
  fi
])

dnl Test whether log2() works.
dnl On OSF/1 5.1, log2f(-0.0f) is NaN.
dnl On Cygwin 1.7.9, log2f(2^13) is not exactly 13.
AC_DEFUN([gl_FUNC_LOG2F_WORKS],
[
  AC_REQUIRE([AC_PROG_CC])
  AC_REQUIRE([AC_CANONICAL_HOST]) dnl for cross-compiles
  AC_CACHE_CHECK([whether log2f works], [gl_cv_func_log2f_works],
    [
      AC_RUN_IFELSE(
        [AC_LANG_SOURCE([[
#include <math.h>
#ifndef log2f /* for Cygwin 1.7.x */
extern
#ifdef __cplusplus
"C"
#endif
float log2f (float);
#endif
volatile float x;
volatile float y;
int main ()
{
  int result = 0;
  /* This test fails on OSF/1 5.1.  */
  x = -0.0f;
  y = log2f (x);
  if (!(y + y == y))
    result |= 1;
  /* This test fails on Cygwin 1.7.9.  */
  x = 8192.0f;
  y = log2f (x);
  if (!(y == 13.0f))
    result |= 2;
  return result;
}
]])],
        [gl_cv_func_log2f_works=yes],
        [gl_cv_func_log2f_works=no],
        [case "$host_os" in
           cygwin* | osf*) gl_cv_func_log2f_works="guessing no";;
           *)              gl_cv_func_log2f_works="guessing yes";;
         esac
        ])
    ])
])
