# log2.m4 serial 3
dnl Copyright (C) 2010-2015 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

AC_DEFUN([gl_FUNC_LOG2],
[
  m4_divert_text([DEFAULTS], [gl_log2_required=plain])
  AC_REQUIRE([gl_MATH_H_DEFAULTS])

  dnl Persuade glibc <math.h> to declare log2().
  AC_REQUIRE([gl_USE_SYSTEM_EXTENSIONS])

  dnl Determine LOG2_LIBM.
  gl_COMMON_DOUBLE_MATHFUNC([log2])

  dnl Test whether log2() exists.
  save_LIBS="$LIBS"
  LIBS="$LIBS $LOG2_LIBM"
  AC_CHECK_FUNCS([log2])
  LIBS="$save_LIBS"
  if test $ac_cv_func_log2 = yes; then
    HAVE_LOG2=1
    dnl Also check whether it's declared.
    dnl IRIX 6.5 has log2() in libm but doesn't declare it in <math.h>.
    AC_CHECK_DECL([log2], , [HAVE_DECL_LOG2=0], [[#include <math.h>]])

    save_LIBS="$LIBS"
    LIBS="$LIBS $LOG2_LIBM"
    gl_FUNC_LOG2_WORKS
    LIBS="$save_LIBS"
    case "$gl_cv_func_log2_works" in
      *yes) ;;
      *) REPLACE_LOG2=1 ;;
    esac

    m4_ifdef([gl_FUNC_LOG2_IEEE], [
      if test $gl_log2_required = ieee && test $REPLACE_LOG2 = 0; then
        AC_REQUIRE([AC_CANONICAL_HOST]) dnl for cross-compiles
        AC_CACHE_CHECK([whether log2 works according to ISO C 99 with IEC 60559],
          [gl_cv_func_log2_ieee],
          [
            save_LIBS="$LIBS"
            LIBS="$LIBS $LOG2_LIBM"
            AC_RUN_IFELSE(
              [AC_LANG_SOURCE([[
#ifndef __NO_MATH_INLINES
# define __NO_MATH_INLINES 1 /* for glibc */
#endif
#include <math.h>
#ifndef log2 /* for Cygwin 1.7.x */
extern
#ifdef __cplusplus
"C"
#endif
double log2 (double);
#endif
/* Compare two numbers with ==.
   This is a separate function because IRIX 6.5 "cc -O" miscompiles an
   'x == x' test.  */
static int
numeric_equal (double x, double y)
{
  return x == y;
}
static double dummy (double x) { return 0; }
int main (int argc, char *argv[])
{
  double (*my_log2) (double) = argc ? log2 : dummy;
  /* Test log2(negative).
     This test fails on NetBSD 5.1 and Solaris 11 2011-11.  */
  double y = my_log2 (-1.0);
  if (numeric_equal (y, y))
    return 1;
  return 0;
}
              ]])],
              [gl_cv_func_log2_ieee=yes],
              [gl_cv_func_log2_ieee=no],
              [case "$host_os" in
                         # Guess yes on glibc systems.
                 *-gnu*) gl_cv_func_log2_ieee="guessing yes" ;;
                         # If we don't know, assume the worst.
                 *)      gl_cv_func_log2_ieee="guessing no" ;;
               esac
              ])
            LIBS="$save_LIBS"
          ])
        case "$gl_cv_func_log2_ieee" in
          *yes) ;;
          *) REPLACE_LOG2=1 ;;
        esac
      fi
    ])
  else
    HAVE_LOG2=0
    HAVE_DECL_LOG2=0
  fi
  if test $HAVE_LOG2 = 0 || test $REPLACE_LOG2 = 1; then
    dnl Find libraries needed to link lib/log2.c.
    AC_REQUIRE([gl_FUNC_ISNAND])
    AC_REQUIRE([gl_FUNC_FREXP])
    AC_REQUIRE([gl_FUNC_LOG])
    LOG2_LIBM=
    dnl Append $ISNAND_LIBM to LOG2_LIBM, avoiding gratuitous duplicates.
    case " $LOG2_LIBM " in
      *" $ISNAND_LIBM "*) ;;
      *) LOG2_LIBM="$LOG2_LIBM $ISNAND_LIBM" ;;
    esac
    dnl Append $FREXP_LIBM to LOG2_LIBM, avoiding gratuitous duplicates.
    case " $LOG2_LIBM " in
      *" $FREXP_LIBM "*) ;;
      *) LOG2_LIBM="$LOG2_LIBM $FREXP_LIBM" ;;
    esac
    dnl Append $LOG_LIBM to LOG2_LIBM, avoiding gratuitous duplicates.
    case " $LOG2_LIBM " in
      *" $LOG_LIBM "*) ;;
      *) LOG2_LIBM="$LOG2_LIBM $LOG_LIBM" ;;
    esac
  fi
])

dnl Test whether log2() works.
dnl On OSF/1 5.1, log2(-0.0) is NaN.
dnl On Cygwin 1.7.9, log2(2^29) is not exactly 29.
AC_DEFUN([gl_FUNC_LOG2_WORKS],
[
  AC_REQUIRE([AC_PROG_CC])
  AC_REQUIRE([AC_CANONICAL_HOST]) dnl for cross-compiles
  AC_CACHE_CHECK([whether log2 works], [gl_cv_func_log2_works],
    [
      AC_RUN_IFELSE(
        [AC_LANG_SOURCE([[
#include <math.h>
#ifndef log2 /* for Cygwin 1.7.x */
extern
#ifdef __cplusplus
"C"
#endif
double log2 (double);
#endif
volatile double x;
volatile double y;
int main ()
{
  int result = 0;
  /* This test fails on OSF/1 5.1.  */
  x = -0.0;
  y = log2 (x);
  if (!(y + y == y))
    result |= 1;
  /* This test fails on Cygwin 1.7.9.  */
  x = 536870912.0;
  y = log2 (x);
  if (!(y == 29.0))
    result |= 2;
  return result;
}
]])],
        [gl_cv_func_log2_works=yes],
        [gl_cv_func_log2_works=no],
        [case "$host_os" in
           cygwin* | osf*) gl_cv_func_log2_works="guessing no";;
           *)              gl_cv_func_log2_works="guessing yes";;
         esac
        ])
    ])
])
