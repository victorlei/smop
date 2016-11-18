# tmpfile.m4 serial 2
# Copyright (C) 2007, 2009-2015 Free Software Foundation, Inc.
# This file is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

# Written by Ben Pfaff.

# Check whether to use a replacement tmpfile() function.

# The native Windows tmpfile function always tries to put the temporary
# file in the root directory.  (This behaviour is even documented in
# Microsoft's documentation!)  This often fails for ordinary users who
# don't have the permissions to write in the root directory.
#
# We can't test for tmpfile even at runtime, since our test program
# might be running with privileges that allow it to write to the root
# directory, even though tmpfile wouldn't work in general.  Instead,
# just test for a Windows platform (excluding Cygwin).

AC_DEFUN([gl_FUNC_TMPFILE], [
  AC_REQUIRE([gl_STDIO_H_DEFAULTS])
  AC_CACHE_CHECK([whether tmpfile should be overridden],
    [gl_cv_func_tmpfile_unusable],
    [AC_EGREP_CPP([choke me], [
#if (defined _WIN32 || defined __WIN32__) && !defined __CYGWIN__
choke me
#endif
       ],
       [gl_cv_func_tmpfile_unusable=yes],
       [gl_cv_func_tmpfile_unusable=no])])
  if test $gl_cv_func_tmpfile_unusable = yes; then
    REPLACE_TMPFILE=1
  fi
])

# Prerequisites of lib/tmpfile.c.
AC_DEFUN([gl_PREREQ_TMPFILE], [:])
