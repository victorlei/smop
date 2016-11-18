# frexpf.m4 serial 5
dnl Copyright (C) 2011-2015 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

AC_DEFUN([gl_FUNC_FREXPF],
[
  AC_REQUIRE([gl_MATH_H_DEFAULTS])
  AC_REQUIRE([gl_FUNC_FREXP])

  dnl Persuade glibc <math.h> to declare frexpf().
  AC_REQUIRE([gl_USE_SYSTEM_EXTENSIONS])

  dnl Test whether frexpf() exists. We cannot assume that frexpf(), if it
  dnl exists, is defined in the same library as frexp(). This is not the case
  dnl on NetBSD, OpenBSD.
  gl_MATHFUNC([frexpf], [float], [(float, int *)])
  if test $gl_cv_func_frexpf_no_libm = yes \
     || test $gl_cv_func_frexpf_in_libm = yes; then
    save_LIBS="$LIBS"
    LIBS="$LIBS $FREXPF_LIBM"
    gl_FUNC_FREXPF_WORKS
    LIBS="$save_LIBS"
    case "$gl_cv_func_frexpf_works" in
      *yes) ;;
      *)    REPLACE_FREXPF=1 ;;
    esac
  else
    HAVE_FREXPF=0
  fi
  if test $HAVE_FREXPF = 0 || test $REPLACE_FREXPF = 1; then
    FREXPF_LIBM="$FREXP_LIBM"
  fi
  AC_SUBST([FREXPF_LIBM])
])

dnl Test whether frexpf() works also on infinite numbers (this fails e.g. on
dnl IRIX 6.5 and mingw) and on negative zero (this fails e.g. on mingw).
AC_DEFUN([gl_FUNC_FREXPF_WORKS],
[
  AC_REQUIRE([AC_PROG_CC])
  AC_REQUIRE([AC_CANONICAL_HOST]) dnl for cross-compiles
  AC_CACHE_CHECK([whether frexpf works], [gl_cv_func_frexpf_works],
    [
      AC_RUN_IFELSE(
        [AC_LANG_SOURCE([[
#include <float.h>
#include <math.h>
#include <string.h>
/* HP cc on HP-UX 10.20 has a bug with the constant expression -0.0.
   ICC 10.0 has a bug when optimizing the expression -zero.
   The expression -FLT_MIN * FLT_MIN does not work when cross-compiling
   to PowerPC on Mac OS X 10.5.  */
#if defined __hpux || defined __sgi || defined __ICC
static float
compute_minus_zero (void)
{
  return -FLT_MIN * FLT_MIN;
}
# define minus_zero compute_minus_zero ()
#else
float minus_zero = -0.0f;
#endif
int main()
{
  int result = 0;
  volatile float x;
  float zero = 0.0f;
  /* Test on infinite numbers.  */
  x = 1.0f / zero;
  {
    int exp;
    float y = frexpf (x, &exp);
    if (y != x)
      result |= 1;
  }
  /* Test on negative zero.  */
  x = minus_zero;
  {
    int exp;
    float y = frexpf (x, &exp);
    if (memcmp (&y, &x, sizeof x))
      result |= 2;
  }
  return result;
}]])],
        [gl_cv_func_frexpf_works=yes],
        [gl_cv_func_frexpf_works=no],
        [case "$host_os" in
           irix* | mingw*) gl_cv_func_frexpf_works="guessing no";;
           *)              gl_cv_func_frexpf_works="guessing yes";;
         esac
        ])
    ])
])
