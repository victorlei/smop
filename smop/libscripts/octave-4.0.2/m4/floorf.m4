# floorf.m4 serial 13
dnl Copyright (C) 2007, 2009-2015 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

AC_DEFUN([gl_FUNC_FLOORF],
[
  m4_divert_text([DEFAULTS], [gl_floorf_required=plain])
  AC_REQUIRE([gl_MATH_H_DEFAULTS])
  dnl Persuade glibc <math.h> to declare floorf().
  AC_REQUIRE([gl_USE_SYSTEM_EXTENSIONS])
  dnl Test whether floorf() is declared.
  AC_CHECK_DECLS([floorf], , , [[#include <math.h>]])
  if test "$ac_cv_have_decl_floorf" = yes; then
    dnl Test whether floorf() can be used without libm.
    gl_FUNC_FLOORF_LIBS
    if test "$FLOORF_LIBM" = "?"; then
      dnl Sun C 5.0 on Solaris declares floorf() and has it in the system-wide
      dnl libm.so, but not in the libm.so that the compiler uses.
      REPLACE_FLOORF=1
    fi
    m4_ifdef([gl_FUNC_FLOORF_IEEE], [
      if test $gl_floorf_required = ieee && test $REPLACE_FLOORF = 0; then
        AC_REQUIRE([AC_CANONICAL_HOST]) dnl for cross-compiles
        AC_CACHE_CHECK([whether floorf works according to ISO C 99 with IEC 60559],
          [gl_cv_func_floorf_ieee],
          [
            save_LIBS="$LIBS"
            LIBS="$LIBS $FLOORF_LIBM"
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
  float (*my_floorf) (float) = argc ? floorf : dummy;
  /* Test whether floorf (-0.0f) is -0.0f.  */
  if (signbitf (minus_zerof) && !signbitf (my_floorf (minus_zerof)))
    return 1;
  return 0;
}
              ]])],
              [gl_cv_func_floorf_ieee=yes],
              [gl_cv_func_floorf_ieee=no],
              [case "$host_os" in
                         # Guess yes on glibc systems.
                 *-gnu*) gl_cv_func_floorf_ieee="guessing yes" ;;
                         # If we don't know, assume the worst.
                 *)      gl_cv_func_floorf_ieee="guessing no" ;;
               esac
              ])
            LIBS="$save_LIBS"
          ])
        case "$gl_cv_func_floorf_ieee" in
          *yes) ;;
          *) REPLACE_FLOORF=1 ;;
        esac
      fi
    ])
  else
    HAVE_DECL_FLOORF=0
  fi
  if test $HAVE_DECL_FLOORF = 0 || test $REPLACE_FLOORF = 1; then
    dnl No libraries are needed to link lib/floorf.c.
    FLOORF_LIBM=
  fi
  AC_SUBST([FLOORF_LIBM])
])

# Determines the libraries needed to get the floorf() function.
# Sets FLOORF_LIBM.
AC_DEFUN([gl_FUNC_FLOORF_LIBS],
[
  gl_CACHE_VAL_SILENT([gl_cv_func_floorf_libm], [
    gl_cv_func_floorf_libm=?
    AC_LINK_IFELSE(
      [AC_LANG_PROGRAM(
         [[#ifndef __NO_MATH_INLINES
           # define __NO_MATH_INLINES 1 /* for glibc */
           #endif
           #include <math.h>
           float (*funcptr) (float) = floorf;
           float x;]],
         [[x = funcptr(x) + floorf(x);]])],
      [gl_cv_func_floorf_libm=])
    if test "$gl_cv_func_floorf_libm" = "?"; then
      save_LIBS="$LIBS"
      LIBS="$LIBS -lm"
      AC_LINK_IFELSE(
        [AC_LANG_PROGRAM(
           [[#ifndef __NO_MATH_INLINES
             # define __NO_MATH_INLINES 1 /* for glibc */
             #endif
             #include <math.h>
             float (*funcptr) (float) = floorf;
             float x;]],
           [[x = funcptr(x) + floorf(x);]])],
        [gl_cv_func_floorf_libm="-lm"])
      LIBS="$save_LIBS"
    fi
  ])
  FLOORF_LIBM="$gl_cv_func_floorf_libm"
])
