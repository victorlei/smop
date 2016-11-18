# floor.m4 serial 8
dnl Copyright (C) 2007, 2009-2015 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

AC_DEFUN([gl_FUNC_FLOOR],
[
  m4_divert_text([DEFAULTS], [gl_floor_required=plain])
  AC_REQUIRE([gl_MATH_H_DEFAULTS])
  dnl Test whether floor() can be used without libm.
  gl_FUNC_FLOOR_LIBS
  if test "$FLOOR_LIBM" = "?"; then
    FLOOR_LIBM=
  fi
  m4_ifdef([gl_FUNC_FLOOR_IEEE], [
    if test $gl_floor_required = ieee && test $REPLACE_FLOOR = 0; then
      AC_REQUIRE([AC_CANONICAL_HOST]) dnl for cross-compiles
      AC_CACHE_CHECK([whether floor works according to ISO C 99 with IEC 60559],
        [gl_cv_func_floor_ieee],
        [
          save_LIBS="$LIBS"
          LIBS="$LIBS $FLOOR_LIBM"
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
  double (*my_floor) (double) = argc ? floor : dummy;
  /* Test whether floor (-0.0) is -0.0.  */
  if (signbitd (minus_zerod) && !signbitd (my_floor (minus_zerod)))
    return 1;
  return 0;
}
            ]])],
            [gl_cv_func_floor_ieee=yes],
            [gl_cv_func_floor_ieee=no],
            [case "$host_os" in
                       # Guess yes on glibc systems.
               *-gnu*) gl_cv_func_floor_ieee="guessing yes" ;;
                       # If we don't know, assume the worst.
               *)      gl_cv_func_floor_ieee="guessing no" ;;
             esac
            ])
          LIBS="$save_LIBS"
        ])
      case "$gl_cv_func_floor_ieee" in
        *yes) ;;
        *) REPLACE_FLOOR=1 ;;
      esac
    fi
  ])
  if test $REPLACE_FLOOR = 1; then
    dnl No libraries are needed to link lib/floor.c.
    FLOOR_LIBM=
  fi
  AC_SUBST([FLOOR_LIBM])
])

# Determines the libraries needed to get the floor() function.
# Sets FLOOR_LIBM.
AC_DEFUN([gl_FUNC_FLOOR_LIBS],
[
  gl_CACHE_VAL_SILENT([gl_cv_func_floor_libm], [
    gl_cv_func_floor_libm=?
    AC_LINK_IFELSE(
      [AC_LANG_PROGRAM(
         [[#ifndef __NO_MATH_INLINES
           # define __NO_MATH_INLINES 1 /* for glibc */
           #endif
           #include <math.h>
           double x;]],
         [[x = floor(x);]])],
      [gl_cv_func_floor_libm=])
    if test "$gl_cv_func_floor_libm" = "?"; then
      save_LIBS="$LIBS"
      LIBS="$LIBS -lm"
      AC_LINK_IFELSE(
        [AC_LANG_PROGRAM(
           [[#ifndef __NO_MATH_INLINES
             # define __NO_MATH_INLINES 1 /* for glibc */
             #endif
             #include <math.h>
             double x;]],
           [[x = floor(x);]])],
        [gl_cv_func_floor_libm="-lm"])
      LIBS="$save_LIBS"
    fi
  ])
  FLOOR_LIBM="$gl_cv_func_floor_libm"
])
