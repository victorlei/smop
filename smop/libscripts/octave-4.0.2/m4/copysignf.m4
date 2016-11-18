# copysignf.m4 serial 3
dnl Copyright (C) 2011-2015 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

AC_DEFUN([gl_FUNC_COPYSIGNF],
[
  AC_REQUIRE([gl_MATH_H_DEFAULTS])

  dnl Persuade glibc <math.h> to declare copysignf().
  AC_REQUIRE([gl_USE_SYSTEM_EXTENSIONS])

  dnl Determine COPYSIGNF_LIBM.
  gl_MATHFUNC([copysignf], [float], [(float, float)],
    [extern
     #ifdef __cplusplus
     "C"
     #endif
     float copysignf (float, float);
    ])
  if test $gl_cv_func_copysignf_no_libm = yes \
     || test $gl_cv_func_copysignf_in_libm = yes; then
    HAVE_COPYSIGNF=1
    dnl Also check whether it's declared.
    dnl IRIX 6.5 has copysignf() in libm but doesn't declare it in <math.h>.
    AC_CHECK_DECL([copysignf], , [HAVE_DECL_COPYSIGNF=0], [[#include <math.h>]])
  else
    HAVE_COPYSIGNF=0
    HAVE_DECL_COPYSIGNF=0
    COPYSIGNF_LIBM=
  fi
  AC_SUBST([COPYSIGNF_LIBM])
])
