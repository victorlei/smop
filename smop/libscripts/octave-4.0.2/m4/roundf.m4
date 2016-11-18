# roundf.m4 serial 17
dnl Copyright (C) 2007-2015 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

AC_DEFUN([gl_FUNC_ROUNDF],
[
  m4_divert_text([DEFAULTS], [gl_roundf_required=plain])
  AC_REQUIRE([gl_MATH_H_DEFAULTS])

  dnl Persuade glibc <math.h> to declare roundf().
  AC_REQUIRE([gl_USE_SYSTEM_EXTENSIONS])

  gl_CHECK_MATH_LIB([ROUNDF_LIBM], [x = roundf (x);],
    [extern
     #ifdef __cplusplus
     "C"
     #endif
     float roundf (float);
    ])
  if test "$ROUNDF_LIBM" != missing; then
    HAVE_ROUNDF=1
    dnl Also check whether it's declared.
    dnl IRIX 6.5 has roundf() in libm but doesn't declare it in <math.h>.
    AC_CHECK_DECLS([roundf], , [HAVE_DECL_ROUNDF=0], [[#include <math.h>]])

    dnl Test whether roundf() produces correct results. On mingw, for
    dnl x = 1/2 - 2^-25, the system's roundf() returns a wrong result.
    AC_REQUIRE([AC_PROG_CC])
    AC_REQUIRE([AC_CANONICAL_HOST]) dnl for cross-compiles
    AC_CACHE_CHECK([whether roundf works], [gl_cv_func_roundf_works],
      [
        save_LIBS="$LIBS"
        LIBS="$LIBS $ROUNDF_LIBM"
        AC_RUN_IFELSE([AC_LANG_SOURCE([[
#include <float.h>
#include <math.h>
extern
#ifdef __cplusplus
"C"
#endif
float roundf (float);
#ifdef _MSC_VER
# pragma fenv_access (off)
#endif
int main()
{
  /* 2^FLT_MANT_DIG.  */
  static const float TWO_MANT_DIG =
    /* Assume FLT_MANT_DIG <= 3 * 31.
       Use the identity  n = floor(n/3) + floor((n+1)/3) + floor((n+2)/3).  */
    (float) (1U << (FLT_MANT_DIG / 3))
    * (float) (1U << ((FLT_MANT_DIG + 1) / 3))
    * (float) (1U << ((FLT_MANT_DIG + 2) / 3));
  volatile float x = 0.5f - 0.5f / TWO_MANT_DIG;
  exit (x < 0.5f && roundf (x) != 0.0f);
}]])], [gl_cv_func_roundf_works=yes], [gl_cv_func_roundf_works=no],
        [case "$host_os" in
           mingw*) gl_cv_func_roundf_works="guessing no";;
           *)      gl_cv_func_roundf_works="guessing yes";;
         esac
        ])
        LIBS="$save_LIBS"
      ])
    case "$gl_cv_func_roundf_works" in
      *no) REPLACE_ROUNDF=1 ;;
    esac

    m4_ifdef([gl_FUNC_ROUNDF_IEEE], [
      if test $gl_roundf_required = ieee && test $REPLACE_ROUNDF = 0; then
        AC_REQUIRE([AC_CANONICAL_HOST]) dnl for cross-compiles
        AC_CACHE_CHECK([whether roundf works according to ISO C 99 with IEC 60559],
          [gl_cv_func_roundf_ieee],
          [
            save_LIBS="$LIBS"
            LIBS="$LIBS $ROUNDF_LIBM"
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
float roundf (float);
]gl_FLOAT_MINUS_ZERO_CODE[
]gl_FLOAT_SIGNBIT_CODE[
static float dummy (float f) { return 0; }
int main (int argc, char *argv[])
{
  float (*my_roundf) (float) = argc ? roundf : dummy;
  int result = 0;
  /* Test whether roundf (-0.0f) is -0.0f.  */
  if (signbitf (minus_zerof) && !signbitf (my_roundf (minus_zerof)))
    result |= 1;
  /* Test whether roundf (-0.3f) is -0.0f.  */
  if (signbitf (-0.3f) && !signbitf (my_roundf (-0.3f)))
    result |= 2;
  return result;
}
              ]])],
              [gl_cv_func_roundf_ieee=yes],
              [gl_cv_func_roundf_ieee=no],
              [case "$host_os" in
                         # Guess yes on glibc systems.
                 *-gnu*) gl_cv_func_roundf_ieee="guessing yes" ;;
                         # If we don't know, assume the worst.
                 *)      gl_cv_func_roundf_ieee="guessing no" ;;
               esac
              ])
            LIBS="$save_LIBS"
          ])
        case "$gl_cv_func_roundf_ieee" in
          *yes) ;;
          *) REPLACE_ROUNDF=1 ;;
        esac
      fi
    ])
  else
    HAVE_ROUNDF=0
    HAVE_DECL_ROUNDF=0
  fi
  if test $HAVE_ROUNDF = 0 || test $REPLACE_ROUNDF = 1; then
    dnl Find libraries needed to link lib/roundf.c.
    AC_CHECK_DECLS([ceilf, floorf], , , [[#include <math.h>]])
    if test "$ac_cv_have_decl_floorf" = yes \
       && test "$ac_cv_have_decl_ceilf" = yes; then
      gl_FUNC_FLOORF_LIBS
      gl_FUNC_CEILF_LIBS
      if test "$FLOORF_LIBM" != '?' && test "$CEILF_LIBM" != '?'; then
        AC_DEFINE([HAVE_FLOORF_AND_CEILF], [1],
          [Define if the both the floorf() and ceilf() functions exist.])
        ROUNDF_LIBM=
        dnl Append $FLOORF_LIBM to ROUNDF_LIBM, avoiding gratuitous duplicates.
        case " $ROUNDF_LIBM " in
          *" $FLOORF_LIBM "*) ;;
          *) ROUNDF_LIBM="$ROUNDF_LIBM $FLOORF_LIBM" ;;
        esac
        dnl Append $CEILF_LIBM to ROUNDF_LIBM, avoiding gratuitous duplicates.
        case " $ROUNDF_LIBM " in
          *" $CEILF_LIBM "*) ;;
          *) ROUNDF_LIBM="$ROUNDF_LIBM $CEILF_LIBM" ;;
        esac
      else
        ROUNDF_LIBM=
      fi
    else
      ROUNDF_LIBM=
    fi
  fi
  AC_SUBST([ROUNDF_LIBM])
])
