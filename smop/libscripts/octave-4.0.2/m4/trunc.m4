# trunc.m4 serial 9
dnl Copyright (C) 2007, 2010-2015 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

AC_DEFUN([gl_FUNC_TRUNC],
[
  m4_divert_text([DEFAULTS], [gl_trunc_required=plain])
  AC_REQUIRE([gl_MATH_H_DEFAULTS])
  dnl Persuade glibc <math.h> to declare trunc().
  AC_REQUIRE([gl_USE_SYSTEM_EXTENSIONS])
  dnl Test whether trunc() is declared.
  AC_CHECK_DECLS([trunc], , , [[#include <math.h>]])
  if test "$ac_cv_have_decl_trunc" = yes; then
    dnl Test whether trunc() can be used without libm.
    TRUNC_LIBM=?
    AC_LINK_IFELSE(
      [AC_LANG_PROGRAM(
         [[#ifndef __NO_MATH_INLINES
           # define __NO_MATH_INLINES 1 /* for glibc */
           #endif
           #include <math.h>
           double x;]],
         [[x = trunc(x);]])],
      [TRUNC_LIBM=])
    if test "$TRUNC_LIBM" = "?"; then
      save_LIBS="$LIBS"
      LIBS="$LIBS -lm"
      AC_LINK_IFELSE(
        [AC_LANG_PROGRAM(
           [[#ifndef __NO_MATH_INLINES
             # define __NO_MATH_INLINES 1 /* for glibc */
             #endif
             #include <math.h>
             double x;]],
           [[x = trunc(x);]])],
        [TRUNC_LIBM="-lm"])
      LIBS="$save_LIBS"
    fi
    if test "$TRUNC_LIBM" = "?"; then
      TRUNC_LIBM=
    fi
    m4_ifdef([gl_FUNC_TRUNC_IEEE], [
      if test $gl_trunc_required = ieee && test $REPLACE_TRUNC = 0; then
        AC_REQUIRE([AC_CANONICAL_HOST]) dnl for cross-compiles
        AC_CACHE_CHECK([whether trunc works according to ISO C 99 with IEC 60559],
          [gl_cv_func_trunc_ieee],
          [
            save_LIBS="$LIBS"
            LIBS="$LIBS $TRUNC_LIBM"
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
  double (*my_trunc) (double) = argc ? trunc : dummy;
  /* Test whether trunc (-0.0) is -0.0.  */
  if (signbitd (minus_zerod) && !signbitd (my_trunc (minus_zerod)))
    return 1;
  return 0;
}
              ]])],
              [gl_cv_func_trunc_ieee=yes],
              [gl_cv_func_trunc_ieee=no],
              [case "$host_os" in
                         # Guess yes on glibc systems.
                 *-gnu*) gl_cv_func_trunc_ieee="guessing yes" ;;
                         # If we don't know, assume the worst.
                 *)      gl_cv_func_trunc_ieee="guessing no" ;;
               esac
              ])
            LIBS="$save_LIBS"
          ])
        case "$gl_cv_func_trunc_ieee" in
          *yes) ;;
          *) REPLACE_TRUNC=1 ;;
        esac
      fi
    ])
  else
    HAVE_DECL_TRUNC=0
  fi
  if test $HAVE_DECL_TRUNC = 0 || test $REPLACE_TRUNC = 1; then
    dnl No libraries are needed to link lib/trunc.c.
    TRUNC_LIBM=
  fi
  AC_SUBST([TRUNC_LIBM])
])
