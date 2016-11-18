# copysign.m4 serial 1
dnl Copyright (C) 2011-2015 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

AC_DEFUN([gl_FUNC_COPYSIGN],
[
  AC_REQUIRE([gl_MATH_H_DEFAULTS])

  dnl Determine COPYSIGN_LIBM.
  gl_MATHFUNC([copysign], [double], [(double, double)])
  if test $gl_cv_func_copysign_no_libm = no \
     && test $gl_cv_func_copysign_in_libm = no; then
    HAVE_COPYSIGN=0
    COPYSIGN_LIBM=
  fi
  AC_SUBST([COPYSIGN_LIBM])
])
