# ceilf.m4 serial 14
dnl Copyright (C) 2007, 2009-2015 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

AC_DEFUN([gl_FUNC_CEILF],
[
  m4_divert_text([DEFAULTS], [gl_ceilf_required=plain])
  AC_REQUIRE([gl_MATH_H_DEFAULTS])
  dnl Persuade glibc <math.h> to declare ceilf().
  AC_REQUIRE([gl_USE_SYSTEM_EXTENSIONS])
  dnl Test whether ceilf() is declared.
  AC_CHECK_DECLS([ceilf], , , [[#include <math.h>]])
  if test "$ac_cv_have_decl_ceilf" = yes; then
    dnl Test whether ceilf() can be used without libm.
    gl_FUNC_CEILF_LIBS
    if test "$CEILF_LIBM" = "?"; then
      dnl Sun C 5.0 on Solaris declares ceilf() and has it in the system-wide
      dnl libm.so, but not in the libm.so that the compiler uses.
      REPLACE_CEILF=1
    fi
    m4_ifdef([gl_FUNC_CEILF_IEEE], [
      if test $gl_ceilf_required = ieee && test $REPLACE_CEILF = 0; then
        AC_REQUIRE([AC_CANONICAL_HOST]) dnl for cross-compiles
        AC_CACHE_CHECK([whether ceilf works according to ISO C 99 with IEC 60559],
          [gl_cv_func_ceilf_ieee],
          [
            save_LIBS="$LIBS"
            LIBS="$LIBS $CEILF_LIBM"
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
  float (*my_ceilf) (float) = argc ? ceilf : dummy;
  int result = 0;
  /* Test whether ceilf (-0.0f) is -0.0f.  */
  if (signbitf (minus_zerof) && !signbitf (my_ceilf (minus_zerof)))
    result |= 1;
  /* Test whether ceilf (-0.3f) is -0.0f.  */
  if (signbitf (-0.3f) && !signbitf (my_ceilf (-0.3f)))
    result |= 2;
  return result;
}
              ]])],
              [gl_cv_func_ceilf_ieee=yes],
              [gl_cv_func_ceilf_ieee=no],
              [case "$host_os" in
                         # Guess yes on glibc systems.
                 *-gnu*) gl_cv_func_ceilf_ieee="guessing yes" ;;
                         # If we don't know, assume the worst.
                 *)      gl_cv_func_ceilf_ieee="guessing no" ;;
               esac
              ])
            LIBS="$save_LIBS"
          ])
        case "$gl_cv_func_ceilf_ieee" in
          *yes) ;;
          *) REPLACE_CEILF=1 ;;
        esac
      fi
    ])
  else
    HAVE_DECL_CEILF=0
  fi
  if test $HAVE_DECL_CEILF = 0 || test $REPLACE_CEILF = 1; then
    dnl No libraries are needed to link lib/ceilf.c.
    CEILF_LIBM=
  fi
  AC_SUBST([CEILF_LIBM])
])

# Determines the libraries needed to get the ceilf() function.
# Sets CEILF_LIBM.
AC_DEFUN([gl_FUNC_CEILF_LIBS],
[
  gl_CACHE_VAL_SILENT([gl_cv_func_ceilf_libm], [
    gl_cv_func_ceilf_libm=?
    AC_LINK_IFELSE(
      [AC_LANG_PROGRAM(
         [[#ifndef __NO_MATH_INLINES
           # define __NO_MATH_INLINES 1 /* for glibc */
           #endif
           #include <math.h>
           float (*funcptr) (float) = ceilf;
           float x;]],
         [[x = funcptr(x) + ceilf(x);]])],
      [gl_cv_func_ceilf_libm=])
    if test "$gl_cv_func_ceilf_libm" = "?"; then
      save_LIBS="$LIBS"
      LIBS="$LIBS -lm"
      AC_LINK_IFELSE(
        [AC_LANG_PROGRAM(
           [[#ifndef __NO_MATH_INLINES
             # define __NO_MATH_INLINES 1 /* for glibc */
             #endif
             #include <math.h>
             float (*funcptr) (float) = ceilf;
             float x;]],
           [[x = funcptr(x) + ceilf(x);]])],
        [gl_cv_func_ceilf_libm="-lm"])
      LIBS="$save_LIBS"
    fi
  ])
  CEILF_LIBM="$gl_cv_func_ceilf_libm"
])
