# truncf.m4 serial 9
dnl Copyright (C) 2007, 2010-2015 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

AC_DEFUN([gl_FUNC_TRUNCF],
[
  m4_divert_text([DEFAULTS], [gl_truncf_required=plain])
  AC_REQUIRE([gl_MATH_H_DEFAULTS])
  dnl Persuade glibc <math.h> to declare truncf().
  AC_REQUIRE([gl_USE_SYSTEM_EXTENSIONS])
  dnl Test whether truncf() is declared.
  AC_CHECK_DECLS([truncf], , , [[#include <math.h>]])
  if test "$ac_cv_have_decl_truncf" = yes; then
    dnl Test whether truncf() can be used without libm.
    TRUNCF_LIBM=?
    AC_LINK_IFELSE(
      [AC_LANG_PROGRAM(
         [[#ifndef __NO_MATH_INLINES
           # define __NO_MATH_INLINES 1 /* for glibc */
           #endif
           #include <math.h>
           float x;]],
         [[x = truncf(x);]])],
      [TRUNCF_LIBM=])
    if test "$TRUNCF_LIBM" = "?"; then
      save_LIBS="$LIBS"
      LIBS="$LIBS -lm"
      AC_LINK_IFELSE(
        [AC_LANG_PROGRAM(
           [[#ifndef __NO_MATH_INLINES
             # define __NO_MATH_INLINES 1 /* for glibc */
             #endif
             #include <math.h>
             float x;]],
           [[x = truncf(x);]])],
        [TRUNCF_LIBM="-lm"])
      LIBS="$save_LIBS"
    fi
    if test "$TRUNCF_LIBM" = "?"; then
      TRUNCF_LIBM=
    fi
    m4_ifdef([gl_FUNC_TRUNCF_IEEE], [
      if test $gl_truncf_required = ieee && test $REPLACE_TRUNCF = 0; then
        AC_REQUIRE([AC_CANONICAL_HOST]) dnl for cross-compiles
        AC_CACHE_CHECK([whether truncf works according to ISO C 99 with IEC 60559],
          [gl_cv_func_truncf_ieee],
          [
            save_LIBS="$LIBS"
            LIBS="$LIBS $TRUNCF_LIBM"
            AC_RUN_IFELSE(
              [AC_LANG_SOURCE([[
#ifndef __NO_MATH_INLINES
# define __NO_MATH_INLINES 1 /* for glibc */
#endif
#include <math.h>
]gl_FLOAT_MINUS_ZERO_CODE[
]gl_FLOAT_SIGNBIT_CODE[
static float dummy (float f) { return 0; }
int main (int argc, char *argv[])
{
  float (*my_truncf) (float) = argc ? truncf : dummy;
  /* Test whether truncf (-0.0f) is -0.0f.  */
  if (signbitf (minus_zerof) && !signbitf (my_truncf (minus_zerof)))
    return 1;
  return 0;
}
              ]])],
              [gl_cv_func_truncf_ieee=yes],
              [gl_cv_func_truncf_ieee=no],
              [case "$host_os" in
                         # Guess yes on glibc systems.
                 *-gnu*) gl_cv_func_truncf_ieee="guessing yes" ;;
                         # If we don't know, assume the worst.
                 *)      gl_cv_func_truncf_ieee="guessing no" ;;
               esac
              ])
            LIBS="$save_LIBS"
          ])
        case "$gl_cv_func_truncf_ieee" in
          *yes) ;;
          *) REPLACE_TRUNCF=1 ;;
        esac
      fi
    ])
  else
    HAVE_DECL_TRUNCF=0
  fi
  if test $HAVE_DECL_TRUNCF = 0 || test $REPLACE_TRUNCF = 1; then
    dnl No libraries are needed to link lib/truncf.c.
    TRUNCF_LIBM=
  fi
  AC_SUBST([TRUNCF_LIBM])
])
