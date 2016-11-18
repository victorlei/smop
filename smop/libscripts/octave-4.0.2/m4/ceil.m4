# ceil.m4 serial 9
dnl Copyright (C) 2007, 2009-2015 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

AC_DEFUN([gl_FUNC_CEIL],
[
  m4_divert_text([DEFAULTS], [gl_ceil_required=plain])
  AC_REQUIRE([gl_MATH_H_DEFAULTS])
  dnl Test whether ceil() can be used without libm.
  gl_FUNC_CEIL_LIBS
  if test "$CEIL_LIBM" = "?"; then
    CEIL_LIBM=
  fi
  m4_ifdef([gl_FUNC_CEIL_IEEE], [
    if test $gl_ceil_required = ieee && test $REPLACE_CEIL = 0; then
      AC_REQUIRE([AC_CANONICAL_HOST]) dnl for cross-compiles
      AC_CACHE_CHECK([whether ceil works according to ISO C 99 with IEC 60559],
        [gl_cv_func_ceil_ieee],
        [
          save_LIBS="$LIBS"
          LIBS="$LIBS $CEIL_LIBM"
          AC_RUN_IFELSE(
            [AC_LANG_SOURCE([[
#ifndef __NO_MATH_INLINES
# define __NO_MATH_INLINES 1 /* for glibc */
#endif
#include <math.h>
]gl_DOUBLE_MINUS_ZERO_CODE[
]gl_DOUBLE_SIGNBIT_CODE[
static double dummy (double f) { return 0; }
int main (int argc, char *argv[])
{
  double (*my_ceil) (double) = argc ? ceil : dummy;
  int result = 0;
  /* Test whether ceil (-0.0) is -0.0.  */
  if (signbitd (minus_zerod) && !signbitd (my_ceil (minus_zerod)))
    result |= 1;
  /* Test whether ceil (-0.3) is -0.0.  */
  if (signbitd (-0.3) && !signbitd (my_ceil (-0.3)))
    result |= 2;
  return result;
}
            ]])],
            [gl_cv_func_ceil_ieee=yes],
            [gl_cv_func_ceil_ieee=no],
            [case "$host_os" in
                       # Guess yes on glibc systems.
               *-gnu*) gl_cv_func_ceil_ieee="guessing yes" ;;
                       # If we don't know, assume the worst.
               *)      gl_cv_func_ceil_ieee="guessing no" ;;
             esac
            ])
          LIBS="$save_LIBS"
        ])
      case "$gl_cv_func_ceil_ieee" in
        *yes) ;;
        *) REPLACE_CEIL=1 ;;
      esac
    fi
  ])
  if test $REPLACE_CEIL = 1; then
    dnl No libraries are needed to link lib/ceil.c.
    CEIL_LIBM=
  fi
  AC_SUBST([CEIL_LIBM])
])

# Determines the libraries needed to get the ceil() function.
# Sets CEIL_LIBM.
AC_DEFUN([gl_FUNC_CEIL_LIBS],
[
  gl_CACHE_VAL_SILENT([gl_cv_func_ceil_libm], [
    gl_cv_func_ceil_libm=?
    AC_LINK_IFELSE(
      [AC_LANG_PROGRAM(
         [[#ifndef __NO_MATH_INLINES
           # define __NO_MATH_INLINES 1 /* for glibc */
           #endif
           #include <math.h>
           double x;]],
         [[x = ceil(x);]])],
      [gl_cv_func_ceil_libm=])
    if test "$gl_cv_func_ceil_libm" = "?"; then
      save_LIBS="$LIBS"
      LIBS="$LIBS -lm"
      AC_LINK_IFELSE(
        [AC_LANG_PROGRAM(
           [[#ifndef __NO_MATH_INLINES
             # define __NO_MATH_INLINES 1 /* for glibc */
             #endif
             #include <math.h>
             double x;]],
           [[x = ceil(x);]])],
        [gl_cv_func_ceil_libm="-lm"])
      LIBS="$save_LIBS"
    fi
  ])
  CEIL_LIBM="$gl_cv_func_ceil_libm"
])
