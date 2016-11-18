dnl aclocal.m4 -- extra macros for configuring Octave
dnl
dnl Copyright (C) 1995-2015 John W. Eaton
dnl
dnl This file is part of Octave.
dnl
dnl Octave is free software; you can redistribute it and/or modify it
dnl under the terms of the GNU General Public License as published by the
dnl Free Software Foundation; either version 3 of the License, or (at
dnl your option) any later version.
dnl
dnl Octave is distributed in the hope that it will be useful, but WITHOUT
dnl ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
dnl FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
dnl for more details.
dnl
dnl You should have received a copy of the GNU General Public License
dnl along with Octave; see the file COPYING.  If not, see
dnl <http://www.gnu.org/licenses/>.
dnl
dnl
dnl Copyright (C) 2008 - 2009 Free Software Foundation, Inc.
dnl
dnl If needed, define the m4_ifblank and m4_ifnblank macros from autoconf 2.64
dnl This allows us to run with earlier Autoconfs as well.
dnl FIXME: these should go away once Autoconf 2.64 is required or ubiquitous.
dnl
ifdef([m4_ifblank],[],[
m4_define([m4_ifblank],
[m4_if(m4_translit([[$1]],  [ ][	][
]), [], [$2], [$3])])])
dnl
ifdef([m4_ifnblank],[],[
m4_define([m4_ifnblank],
[m4_if(m4_translit([[$1]],  [ ][	][
]), [], [$3], [$2])])])
dnl
dnl ----------------------------------------------------------------------
dnl

dnl
dnl Alphabetical list of macros in the OCTAVE_ namespace
dnl

dnl
dnl Check if the Carbon Framework defines CGDisplayBitsPerPixel.
dnl
AC_DEFUN([OCTAVE_CARBON_CGDISPLAYBITSPERPIXEL], [
  AC_CACHE_CHECK([whether CGDisplayBitsPerPixel is defined in the Carbon Framework],
    [octave_cv_func_carbon_cgdisplaybitsperpixel],
    [AC_LANG_PUSH(C++)
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
        #include <Carbon/Carbon.h>
        ]], [[
        CGDirectDisplayID display = CGMainDisplayID ();
        size_t depth = CGDisplayBitsPerPixel (display);
      ]])],
      octave_cv_func_carbon_cgdisplaybitsperpixel=yes,
      octave_cv_func_carbon_cgdisplaybitsperpixel=no)
    AC_LANG_POP(C++)
  ])
  if test $octave_cv_func_carbon_cgdisplaybitsperpixel = yes; then
    AC_DEFINE(HAVE_CARBON_CGDISPLAYBITSPERPIXEL, 1,
      [Define to 1 if Carbon Framework has CGDisplayBitsPerPixel.])
  fi
])
dnl
dnl Check if C compiler handles FLAG command line option.  If two
dnl arguments are specified, execute the second arg as shell commands.
dnl Otherwise, add FLAG to CFLAGS if the compiler accepts the flag.
dnl
AC_DEFUN([OCTAVE_CC_FLAG], [
  ac_safe=`echo "$1" | $SED 'y% ./+-:=%___p___%'`
  AC_MSG_CHECKING([whether ${CC-cc} accepts $1])
  AC_CACHE_VAL([octave_cv_cc_flag_$ac_safe],
    [AC_LANG_PUSH(C)
    ac_octave_save_CFLAGS="$CFLAGS"
    CFLAGS="$CFLAGS $1"
    AC_LINK_IFELSE([AC_LANG_PROGRAM([], [])],
      [eval "octave_cv_cc_flag_$ac_safe=yes"],
      [eval "octave_cv_cc_flag_$ac_safe=no"])
    CFLAGS="$ac_octave_save_CFLAGS"
    AC_LANG_POP(C)
  ])
  if eval "test \"`echo '$octave_cv_cc_flag_'$ac_safe`\" = yes"; then
    AC_MSG_RESULT([yes])
    ifelse([$2], ,
      [CFLAGS="$CFLAGS $1"
      AC_MSG_RESULT([adding $1 to CFLAGS])], [$2])
  else
    AC_MSG_RESULT([no])
    ifelse([$3], , , [$3])
  fi
])
dnl
dnl Check for broken stl_algo.h header file in gcc versions 4.8.0, 4.8.1, 4.8.2
dnl which leads to failures in nth_element.
dnl
AC_DEFUN([OCTAVE_CHECK_BROKEN_STL_ALGO_H], [
  AC_CACHE_CHECK([whether stl_algo.h is broken],
    [octave_cv_broken_stl_algo_h],
    [AC_LANG_PUSH(C++)
    AC_RUN_IFELSE([AC_LANG_PROGRAM([[
// Based on code from a GCC test program.

// Copyright (C) 2013 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library. This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3. If not see
// <http://www.gnu.org/licenses/>.

// 25.3.2 [lib.alg.nth.element]

// { dg-options "-std=gnu++11" }

#include <algorithm>
#include <vector>
      ]], [[
std::vector<int> v (7);

v[0] = 207089;
v[1] = 202585;
v[2] = 180067;
v[3] = 157549;
v[4] = 211592;
v[5] = 216096;
v[6] = 207089;

std::nth_element (v.begin (), v.begin () + 3, v.end ());

return v[3] == 207089 ? 0 : 1;
    ]])],
    octave_cv_broken_stl_algo_h=no,
    octave_cv_broken_stl_algo_h=yes,
    [case "$GXX_VERSION" in
       *4.8.2*)
         octave_cv_broken_stl_algo_h=yes
       ;;
       *)
         octave_cv_broken_stl_algo_h=no
       ;;
     esac
    ])
    AC_LANG_POP(C++)
  ])
  if test "$GXX" = yes; then
    if test $octave_cv_broken_stl_algo_h = yes; then
      case "$GXX_VERSION" in
        4.8.[[012]])
        ;;
        *)
          octave_cv_broken_stl_algo_h=no
          warn_stl_algo_h="UNEXPECTED: found nth_element broken in g++ $GXX_VERSION.  Refusing to fix except for g++ 4.8.0, 4.8.1, or 4.8.2.  You appear to have g++ $GXX_VERSION."
          OCTAVE_CONFIGURE_WARNING([warn_stl_algo_h])
        ;;
      esac
    else
      case "$GXX_VERSION" in
        4.8.2)
          warn_stl_algo_h="UNEXPECTED: found nth_element working in g++ 4.8.2.  Has it been patched on your system?"
          OCTAVE_CONFIGURE_WARNING([warn_stl_algo_h])
        ;;
      esac
    fi
  else
    octave_cv_broken_stl_algo_h=no
    warn_stl_algo_h="UNEXPECTED: nth_element test failed.  Refusing to fix except for g++ 4.8.2."
    OCTAVE_CONFIGURE_WARNING([warn_stl_algo_h])
  fi
])
dnl
dnl Check whether the FFTW library supports multi-threading. This macro
dnl should be called once per FFTW precision passing in the library
dnl variant (e.g. "fftw3") and a function in the thread support API
dnl (e.g. "fftw_plan_with_nthreads"). Depending on how FFTW was built,
dnl the thread functions could be compiled into the main FFTW library or
dnl could be a separate add-on library that is passed to the linker
dnl ahead of the main FFTW library.
dnl
AC_DEFUN([OCTAVE_CHECK_FFTW_THREADS], [
  ac_octave_save_CPPFLAGS="$CPPFLAGS"
  ac_octave_save_LDFLAGS="$LDFLAGS"
  ac_octave_save_LIBS="$LIBS"
  CPPFLAGS="$m4_toupper([$1])_CPPFLAGS $CPPFLAGS"
  LDFLAGS="$m4_toupper([$1])_LDFLAGS $LDFLAGS"
  LIBS="$m4_toupper([$1])_LIBS $LIBS"
  AC_CACHE_CHECK([for $1 multi-threading support],
    [octave_cv_[$1]_threads_lib],
    [AC_LINK_IFELSE([AC_LANG_PROGRAM([[
      #include <fftw3.h>
      ]], [[
      $2 (2);
      ]])],
      [octave_cv_[$1]_threads_lib=yes],
      [LIBS="-l[$1]_threads $LIBS"
      AC_LINK_IFELSE([AC_LANG_PROGRAM([[
        #include <fftw3.h>
        ]], [[
        $2 (2);
        ]])],
        [octave_cv_[$1]_threads_lib="-l[$1]_threads"],
        [octave_cv_[$1]_threads_lib=no])
    ])
  ])
  case $octave_cv_[$1]_threads_lib in
    -l*)
      m4_toupper([$1])_LIBS="$octave_cv_[$1]_threads_lib $m4_toupper([$1])_LIBS"
      ;;
    no)
      AC_MSG_WARN([No $1 multi-threading support found.])
      AC_MSG_WARN([The single-threaded library will be used instead.])
      ;;
  esac
  if test $octave_cv_[$1]_threads_lib != no; then
    AC_DEFINE([HAVE_]m4_toupper([$1])[_THREADS], 1,
      [Define to 1 if ]m4_toupper([$1])[ has multi-threading support.])
  fi
  CPPFLAGS="$ac_octave_save_CPPFLAGS"
  LDFLAGS="$ac_octave_save_LDFLAGS"
  LIBS="$ac_octave_save_LIBS"
])
dnl
dnl Check whether a math mapper function is available in <cmath>.
dnl Will define HAVE_CMATH_FUNC if there is a double variant and
dnl HAVE_CMATH_FUNCF if there is a float variant.
dnl Currently capable of checking for functions with single
dnl argument and returning bool/int/real.
dnl
AC_DEFUN([OCTAVE_CHECK_FUNC_CMATH], [
  ac_safe=`echo "$1" | $SED 'y% ./+-:=%___p___%'`

  AC_CACHE_CHECK([for std::$1 in <cmath>],
    [octave_cv_func_cmath_$ac_safe],
    [AC_LANG_PUSH(C++)
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
        #include <cmath>
        void take_func (bool (*func) (double x));
        void take_func (int (*func) (double x));
        void take_func (double (*func) (double x));
        ]], [[
        take_func(std::$1);
        ]])],
      [eval "octave_cv_func_cmath_$ac_safe=yes"],
      [eval "octave_cv_func_cmath_$ac_safe=no"])
    AC_LANG_POP(C++)
  ])
  if eval "test \"`echo '$octave_cv_func_cmath_'$ac_safe`\" = yes"; then
    AC_DEFINE(AS_TR_CPP([[HAVE_CMATH_][$1]]), 1,
      [Define to 1 if <cmath> provides $1.])
  fi

  AC_CACHE_CHECK([for std::$1 (float variant) in <cmath>],
    [octave_cv_func_cmath_f$ac_safe],
    [AC_LANG_PUSH(C++)
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
        #include <cmath>
        void take_func (bool (*func) (float x));
        void take_func (int (*func) (float x));
        void take_func (float (*func) (float x));
        ]], [[
        take_func(std::$1);
        ]])],
      [eval "octave_cv_func_cmath_f$ac_safe=yes"],
      [eval "octave_cv_func_cmath_f$ac_safe=no"])
    AC_LANG_POP(C++)
  ])
  if eval "test \"`echo '$octave_cv_func_cmath_f'$ac_safe`\" = yes"; then
    AC_DEFINE(AS_TR_CPP([[HAVE_CMATH_][$1][F]]), 1,
      [Define to 1 if <cmath> provides float variant of $1.])
  fi
])
dnl
dnl Check whether Qscintilla has version 2.6.0 or later
dnl FIXME: This test uses a version number.  It potentially could
dnl        be re-written to actually call the function, but is it worth it?
dnl
AC_DEFUN([OCTAVE_CHECK_VERSION_2_6_0], [
  AC_CACHE_CHECK([whether Qscintilla has version 2.6.0 or later],
    [octave_cv_version_2_6_0],
    [AC_LANG_PUSH(C++)
    ac_octave_save_CPPFLAGS="$CPPFLAGS"
    CPPFLAGS="$QT_CPPFLAGS $CPPFLAGS"
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
        #include <Qsci/qsciglobal.h>
        ]], [[
        #if QSCINTILLA_VERSION < 0x020600
        #error Old FindFirst function found.
        #endif
        ]])],
      octave_cv_version_2_6_0=yes,
      octave_cv_version_2_6_0=no)
    CPPFLAGS="$ac_octave_save_CPPFLAGS"
    AC_LANG_POP(C++)
  ])
  if test $octave_cv_version_2_6_0 = yes; then
    AC_DEFINE(HAVE_QSCI_VERSION_2_6_0, 1,
      [Define to 1 if Qscintilla is of Version 2.6.0 or later.])
  fi
])
dnl
dnl Check if Fortran compiler has the intrinsic function ISNAN.
dnl
AC_DEFUN([OCTAVE_CHECK_FUNC_FORTRAN_ISNAN], [
  AC_CACHE_CHECK([whether $F77 has the intrinsic function ISNAN],
    [octave_cv_func_fortran_isnan],
    [AC_LANG_PUSH(Fortran 77)
    AC_COMPILE_IFELSE([[
      program foo
      implicit none
      real x
      double precision y
      if (isnan(x)) then
        print *, 'x is NaN'
      end if
      if (isnan(y)) then
        print *, 'y is NaN'
      end if
      end program
      ]],
      octave_cv_func_fortran_isnan=yes, octave_cv_func_fortran_isnan=no)
    AC_LANG_POP(Fortran 77)
  ])
])
dnl
dnl Check if function gluTessCallback is called with "(...)".
dnl
AC_DEFUN([OCTAVE_CHECK_FUNC_GLUTESSCALLBACK_THREEDOTS], [
  AC_CACHE_CHECK([whether gluTessCallback is called with "(...)"],
    [octave_cv_func_glutesscallback_threedots],
    [AC_LANG_PUSH(C++)
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
        #ifdef HAVE_GL_GLU_H
        # include <GL/glu.h>
        #elif defined HAVE_OPENGL_GLU_H || defined HAVE_FRAMEWORK_OPENGL
        # include <OpenGL/glu.h>
        #endif
        ]], [[
        GLvoid (*func)(...);
        gluTessCallback(0, 0, func);
        ]])],
      octave_cv_func_glutesscallback_threedots=yes,
      octave_cv_func_glutesscallback_threedots=no)
    AC_LANG_POP(C++)
  ])
  if test $octave_cv_func_glutesscallback_threedots = yes; then
    AC_DEFINE(HAVE_GLUTESSCALLBACK_THREEDOTS, 1,
      [Define to 1 if gluTessCallback is called with (...).])
  fi
])
dnl
dnl Check whether Qt provides QFont::Monospace
dnl
AC_DEFUN([OCTAVE_CHECK_QFONT_MONOSPACE], [
  AC_CACHE_CHECK([whether Qt provides QFont::Monospace],
    [octave_cv_decl_qfont_monospace],
    [AC_LANG_PUSH(C++)
    ac_octave_save_CPPFLAGS="$CPPFLAGS"
    CPPFLAGS="$QT_CPPFLAGS $CPPFLAGS"
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
        #include <QFont>
        ]], [[
        QFont::StyleHint hint = QFont::Monospace;
        ]])],
      octave_cv_decl_qfont_monospace=yes,
      octave_cv_decl_qfont_monospace=no)
    CPPFLAGS="$ac_octave_save_CPPFLAGS"
    AC_LANG_POP(C++)
  ])
  if test $octave_cv_decl_qfont_monospace = yes; then
    AC_DEFINE(HAVE_QFONT_MONOSPACE, 1,
      [Define to 1 if Qt provides QFont::Monospace.])
  fi
])
dnl
dnl Check whether Qt provides QFont::ForceIntegerMetrics
dnl
AC_DEFUN([OCTAVE_CHECK_QFONT_FORCE_INTEGER_METRICS], [
  AC_CACHE_CHECK([whether Qt provides QFont::ForceIntegerMetrics],
    [octave_cv_decl_qfont_force_integer_metrics],
    [AC_LANG_PUSH(C++)
    ac_octave_save_CPPFLAGS="$CPPFLAGS"
    CPPFLAGS="$QT_CPPFLAGS $CPPFLAGS"
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
        #include <QFont>
        ]], [[
        QFont::StyleStrategy strategy = QFont::ForceIntegerMetrics;
        ]])],
      octave_cv_decl_qfont_force_integer_metrics=yes,
      octave_cv_decl_qfont_force_integer_metrics=no)
    CPPFLAGS="$ac_octave_save_CPPFLAGS"
    AC_LANG_POP(C++)
  ])
  if test $octave_cv_decl_qfont_force_integer_metrics = yes; then
    AC_DEFINE(HAVE_QFONT_FORCE_INTEGER_METRICS, 1,
      [Define to 1 if Qt provides QFont::ForceIntegerMetrics.])
  fi
])
dnl
dnl Check whether Qscintilla SetPlaceholderText function exists.
dnl FIXME: This test uses a version number.  It potentially could
dnl        be re-written to actually call the function, but is it worth it?
dnl
AC_DEFUN([OCTAVE_CHECK_FUNC_SETPLACEHOLDERTEXT], [
  AC_CACHE_CHECK([whether Qt has SetPlaceholderText function],
    [octave_cv_func_setplaceholdertext],
    [AC_LANG_PUSH(C++)
    ac_octave_save_CPPFLAGS="$CPPFLAGS"
    CPPFLAGS="$QT_CPPFLAGS $CPPFLAGS"
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
        #include <Qt/qglobal.h>
        ]], [[
        #if QT_VERSION < 0x040700
        #error No SetPlacholderText function available.
        #endif
        ]])],
      octave_cv_func_setplaceholdertext=yes,
      octave_cv_func_setplaceholdertext=no)
    CPPFLAGS="$ac_octave_save_CPPFLAGS"
    AC_LANG_POP(C++)
  ])
  if test $octave_cv_func_setplaceholdertext = yes; then
    AC_DEFINE(HAVE_SETPLACEHOLDERTEXT, 1,
      [Define to 1 if you have the Qt SetPlaceholderText function.])
  fi
])
dnl
dnl Check whether the Qt QAbstractItemModel::beginResetModel() function exists.
dnl Also checks for QAbstractItemModel::endResetModel().  These are two of the
dnl newest Qt functions that the Octave GUI depends on, added in Qt 4.6.
dnl
AC_DEFUN([OCTAVE_CHECK_FUNC_QABSTRACTITEMMODEL_BEGINRESETMODEL], [
  AC_CACHE_CHECK([whether Qt has the QAbstractItemModel::beginResetModel() function],
    [octave_cv_func_qabstractitemmodel_beginresetmodel],
    [AC_LANG_PUSH(C++)
    ac_octave_save_CPPFLAGS="$CPPFLAGS"
    CPPFLAGS="$QT_CPPFLAGS $CPPFLAGS"
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
        #include <QAbstractItemModel>
        class item_model : public QAbstractItemModel
        {
        public:
          item_model (QObject *parent = 0) : QAbstractItemModel (parent) {}
          ~item_model () {}
          QModelIndex index (int, int, const QModelIndex& m) const { return m; }
          QModelIndex parent (const QModelIndex& m) const { return m; }
          int columnCount (const QModelIndex&) const { return 0; }
          int rowCount (const QModelIndex&) const { return 0; }
          QVariant data (const QModelIndex&, int) const { return QVariant(); }
          void update_model ()
          {
            this->beginResetModel ();
            this->endResetModel ();
          }
        };
        ]], [[
        item_model model;
        model.update_model ();
        ]])],
      octave_cv_func_qabstractitemmodel_beginresetmodel=yes,
      octave_cv_func_qabstractitemmodel_beginresetmodel=no)
    CPPFLAGS="$ac_octave_save_CPPFLAGS"
    AC_LANG_POP(C++)
  ])
  if test $octave_cv_func_qabstractitemmodel_beginresetmodel = yes; then
    AC_DEFINE(HAVE_QABSTRACTITEMMODEL_BEGINRESETMODEL, 1,
      [Define to 1 if Qt has the QAbstractItemModel::beginResetModel() function.])
  fi
])
dnl
dnl Check whether the Qt QTabWidget::setMovable() function exists.
dnl This function was added in Qt 4.5.
dnl
AC_DEFUN([OCTAVE_CHECK_FUNC_QTABWIDGET_SETMOVABLE], [
  AC_CACHE_CHECK([whether Qt has the QTabWidget::setMovable() function],
    [octave_cv_func_qtabwidget_setmovable],
    [AC_LANG_PUSH(C++)
    ac_octave_save_CPPFLAGS="$CPPFLAGS"
    CPPFLAGS="$QT_CPPFLAGS $CPPFLAGS"
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
        #include <QTabWidget>
        class tab_widget : public QTabWidget
        {
        public:
          tab_widget (QWidget *parent = 0) : QTabWidget (parent) { this->setMovable (true); }
          ~tab_widget () {}
        };
        ]], [[
        tab_widget tw;
        ]])],
      octave_cv_func_qtabwidget_setmovable=yes,
      octave_cv_func_qtabwidget_setmovable=no)
    CPPFLAGS="$ac_octave_save_CPPFLAGS"
    AC_LANG_POP(C++)
  ])
  if test $octave_cv_func_qtabwidget_setmovable = yes; then
    AC_DEFINE(HAVE_QTABWIDGET_SETMOVABLE, 1,
      [Define to 1 if Qt has the QTabWidget::setMovable() function.])
  fi
])
dnl
dnl Check whether the QsciScintilla::findFirstInSelection () function exists.
dnl This function was added in QScintilla 2.7.
dnl
AC_DEFUN([OCTAVE_CHECK_FUNC_QSCI_FINDSELECTION], [
  AC_CACHE_CHECK([whether QSci has the QsciScintilla::findFirstInSelection () function],
    [octave_cv_func_qsci_findfirstinselection],
    [AC_LANG_PUSH(C++)
    ac_octave_save_CPPFLAGS="$CPPFLAGS"
    CPPFLAGS="$QT_CPPFLAGS $CPPFLAGS"
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
        #include <Qsci/qsciscintilla.h>
        class qsci : public QsciScintilla
        {
        public:
          qsci (QWidget *parent = 0) : QsciScintilla (parent)
          { this->findFirstInSelection (QString ("x"),true,true,true,true,true); }
          ~qsci () {}
        };
        ]], [[
        qsci edit;
        ]])],
      octave_cv_func_qsci_findfirstinselection=yes,
      octave_cv_func_qsci_findfirstinselection=no)
    CPPFLAGS="$ac_octave_save_CPPFLAGS"
    AC_LANG_POP(C++)
  ])
  if test $octave_cv_func_qsci_findfirstinselection = yes; then
    AC_DEFINE(HAVE_QSCI_FINDSELECTION, 1,
      [Define to 1 if Qsci has the QsciScintilla::findFirstInSelection () function.])
  fi
])
dnl
dnl Check whether HDF5 library has version 1.6 API functions.
dnl
AC_DEFUN([OCTAVE_CHECK_HDF5_HAS_VER_16_API], [
  AC_CACHE_CHECK([whether HDF5 library has enforced version 1.6 API],
    [octave_cv_hdf5_has_ver_16_api],
    [AC_LINK_IFELSE([AC_LANG_PROGRAM([[
      #include <hdf5.h>
      ]], [[
      H5Eset_auto (0, 0);
      ]])],
      octave_cv_hdf5_has_ver_16_api=yes,
      octave_cv_hdf5_has_ver_16_api=no)
  ])
  if test $octave_cv_hdf5_has_ver_16_api != yes; then
    AC_DEFINE(HAVE_HDF5_18, 1, [Define to 1 if >=HDF5-1.8 is available.])
  fi
])
dnl
dnl Usage:
dnl OCTAVE_CHECK_LIB(LIBRARY, DOC-NAME, WARN-MSG, HEADER, FUNC,
dnl                  LANG, DOC-STRING, EXTRA-CHECK)
dnl
AC_DEFUN([OCTAVE_CHECK_LIB], [
  AC_ARG_WITH([$1-includedir],
    [AS_HELP_STRING([--with-$1-includedir=DIR],
      [look for $2 include files in DIR])],
    [m4_toupper([$1])_CPPFLAGS="-I$withval"])
  AC_SUBST(m4_toupper([$1])_CPPFLAGS)

  AC_ARG_WITH([$1-libdir],
    [AS_HELP_STRING([--with-$1-libdir=DIR],
      [look for $2 libraries in DIR])],
    [m4_toupper([$1])_LDFLAGS="-L$withval"])
  AC_SUBST(m4_toupper([$1])_LDFLAGS)

  AC_ARG_WITH([$1],
    [m4_ifblank([$7],
      [AS_HELP_STRING([--without-$1], [don't use $2 library])],
      [AS_HELP_STRING([--without-$1], [$7])])],
    with_$1=$withval, with_$1=yes)

  m4_toupper([$1])_LIBS=
  case $with_$1 in
    no)
      m4_toupper([$1])_LIBS=
    ;;
    yes | "")
      m4_toupper([$1])_LIBS="-l$1"
    ;;
    -* | */* | *.a | *.so | *.so.* | *.o)
      m4_toupper([$1])_LIBS="$with_$1"
    ;;
    *)
      m4_toupper([$1])_LIBS="-l$with_$1"
    ;;
  esac

  warn_$1="$3"
  m4_set_add([summary_warning_list], [warn_$1])

  if test -n "$m4_toupper([$1])_LIBS"; then
    ac_octave_save_CPPFLAGS="$CPPFLAGS"
    ac_octave_save_LDFLAGS="$LDFLAGS"
    ac_octave_save_LIBS="$LIBS"
    CPPFLAGS="$m4_toupper([$1])_CPPFLAGS $CPPFLAGS"
    LDFLAGS="$m4_toupper([$1])_LDFLAGS $LDFLAGS"
    LIBS="$m4_toupper([$1])_LIBS $LIBS"
    m4_ifnblank([$6], [AC_LANG_PUSH($6)])
    ac_octave_$1_check_for_lib=no
    m4_ifblank([$4], [ac_octave_$1_check_for_lib=yes],
               [AC_CHECK_HEADERS([$4], [ac_octave_$1_check_for_lib=yes; break])])
    if test $ac_octave_$1_check_for_lib = yes; then
      AC_CACHE_CHECK([for $5 in $m4_toupper([$1])_LIBS],
        [octave_cv_lib_$1],
        [AC_LINK_IFELSE([AC_LANG_CALL([], [$5])],
          [octave_cv_lib_$1=yes], [octave_cv_lib_$1=no])
      ])
      if test "$octave_cv_lib_$1" = yes; then
        m4_ifblank([$8], [
          warn_$1=
          AC_DEFINE([HAVE_]m4_toupper([$1]), 1,
            [Define to 1 if $2 is available.])], [$8])
      fi
    fi
    m4_ifnblank([$6], [AC_LANG_POP($6)])
    CPPFLAGS="$ac_octave_save_CPPFLAGS"
    LDFLAGS="$ac_octave_save_LDFLAGS"
    LIBS="$ac_octave_save_LIBS"
  fi

  AC_SUBST(m4_toupper([$1])_LIBS)
  if test -n "$warn_$1"; then
    AC_MSG_WARN([$warn_$1])
    m4_toupper([$1])_LIBS=
  fi
])
dnl
dnl Check whether ARPACK works (does not crash).
dnl
dnl Using a pure Fortran program doesn't seem to crash when linked
dnl with the buggy ARPACK library but the C++ program does.  Maybe it
dnl is the memory allocation that exposes the bug and using statically
dnl allocated arrays in Fortran does not?
dnl
dnl FIXME: it would be nice to avoid the duplication of F77 macros
dnl and typedefs here and in the f77-fcn.h header file.
dnl
AC_DEFUN([OCTAVE_CHECK_LIB_ARPACK_OK], [
  AC_CACHE_CHECK([whether the arpack library works],
    [octave_cv_lib_arpack_ok],
    [AC_LANG_PUSH(C++)
    AC_RUN_IFELSE([AC_LANG_PROGRAM([[

#include <cfloat>

#include <stdint.h>

typedef OCTAVE_IDX_TYPE octave_idx_type;
typedef int F77_RET_T;

#define F77_CHAR_ARG2(x, l) x
#define F77_CONST_CHAR_ARG2(x, l) F77_CHAR_ARG2 (x, l)

#define F77_CHAR_ARG_LEN(l) , l

#define F77_CONST_CHAR_ARG_DECL const char *
#define F77_CHAR_ARG_LEN_DECL , long

extern "C"
{
  F77_RET_T
  F77_FUNC (dnaupd, DNAUPD) (octave_idx_type&,
                             F77_CONST_CHAR_ARG_DECL,
                             const octave_idx_type&,
                             F77_CONST_CHAR_ARG_DECL,
                             octave_idx_type&, const double&,
                             double*, const octave_idx_type&, double*,
                             const octave_idx_type&, octave_idx_type*,
                             octave_idx_type*, double*, double*,
                             const octave_idx_type&, octave_idx_type&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (dneupd, DNEUPD) (const octave_idx_type&,
                             F77_CONST_CHAR_ARG_DECL,
                             octave_idx_type*, double*, double*,
                             double*, const octave_idx_type&, const double&,
                             const double&, double*,
                             F77_CONST_CHAR_ARG_DECL,
                             const octave_idx_type&,
                             F77_CONST_CHAR_ARG_DECL,
                             octave_idx_type&, const double&, double*,
                             const octave_idx_type&, double*,
                             const octave_idx_type&, octave_idx_type*,
                             octave_idx_type*, double*, double*,
                             const octave_idx_type&, octave_idx_type&
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL
                             F77_CHAR_ARG_LEN_DECL);

  F77_RET_T
  F77_FUNC (dgemv, DGEMV) (F77_CONST_CHAR_ARG_DECL,
                           const octave_idx_type&, const octave_idx_type&,
                           const double&, const double*,
                           const octave_idx_type&, const double*,
                           const octave_idx_type&, const double&, double*,
                           const octave_idx_type&
                           F77_CHAR_ARG_LEN_DECL);
}

void
doit (void)
{
  // Based on function EigsRealNonSymmetricMatrix from liboctave/eigs-base.cc.

  // Problem matrix.  See bug #31479
  octave_idx_type n = 4;
  double *m = new double [n * n];
  m[0] = 1, m[4] = 0, m[8]  = 0, m[12] = -1;
  m[1] = 0, m[5] = 1, m[9]  = 0, m[13] = 0;
  m[2] = 0, m[6] = 0, m[10] = 1, m[14] = 0;
  m[3] = 0, m[7] = 0, m[11] = 2, m[15] = 1;

  double *resid = new double [4];

  resid[0] = 0.960966;
  resid[1] = 0.741195;
  resid[2] = 0.150143;
  resid[3] = 0.868067;

  octave_idx_type *ip = new octave_idx_type [11];

  ip[0] = 1;   // ishift
  ip[1] = 0;   // ip[1] not referenced
  ip[2] = 300; // mxiter, maximum number of iterations
  ip[3] = 1;   // NB blocksize in recurrence
  ip[4] = 0;   // nconv, number of Ritz values that satisfy convergence
  ip[5] = 0;   // ip[5] not referenced
  ip[6] = 1;   // mode
  ip[7] = 0;   // ip[7] to ip[10] are return values
  ip[8] = 0;
  ip[9] = 0;
  ip[10] = 0;

  octave_idx_type *ipntr = new octave_idx_type [14];

  octave_idx_type k = 1;
  octave_idx_type p = 3;
  octave_idx_type lwork = 3 * p * (p + 2);

  double *v = new double [n * (p + 1)];
  double *workl = new double [lwork + 1];
  double *workd = new double [3 * n + 1];

  octave_idx_type ido = 0;
  octave_idx_type info = 0;

  double tol = DBL_EPSILON;

  do
    {
      F77_FUNC (dnaupd, DNAUPD) (ido, F77_CONST_CHAR_ARG2 ("I", 1),
                                 n, F77_CONST_CHAR_ARG2 ("LM", 2),
                                 k, tol, resid, p, v, n, ip, ipntr,
                                 workd, workl, lwork, info
                                 F77_CHAR_ARG_LEN (1)
                                 F77_CHAR_ARG_LEN (2));

      if (ido == -1 || ido == 1 || ido == 2)
        {
          double *x = workd + ipntr[0] - 1;
          double *y = workd + ipntr[1] - 1;

          F77_FUNC (dgemv, DGEMV) (F77_CONST_CHAR_ARG2 ("N", 1),
                                   n, n, 1.0, m, n, x, 1, 0.0, y, 1
                                   F77_CHAR_ARG_LEN (1));
        }
      else
        {
          if (info < 0)
            {
              return;  // Error
            }

          break;
        }
    }
  while (1);

  octave_idx_type *sel = new octave_idx_type [p];

  // In Octave, the dimensions of dr and di are k+1, but k+2 avoids segfault
  double *dr = new double [k + 1];
  double *di = new double [k + 1];
  double *workev = new double [3 * p];

  for (octave_idx_type i = 0; i < k + 1; i++)
    dr[i] = di[i] = 0.;

  octave_idx_type rvec = 1;

  double sigmar = 0.0;
  double sigmai = 0.0;

  // In Octave, this is n*(k+1), but n*(k+2) avoids segfault
  double *z = new double [n * (k + 1)];

  F77_FUNC (dneupd, DNEUPD) (rvec, F77_CONST_CHAR_ARG2 ("A", 1),
                             sel, dr, di, z, n, sigmar, sigmai, workev,
                             F77_CONST_CHAR_ARG2 ("I", 1), n,
                             F77_CONST_CHAR_ARG2 ("LM", 2), k, tol,
                             resid, p, v, n, ip, ipntr, workd,
                             workl, lwork, info
                             F77_CHAR_ARG_LEN (1)
                             F77_CHAR_ARG_LEN (1)
                             F77_CHAR_ARG_LEN (2));
}

]], [[

  for (int i = 0; i < 10; i++)
    doit ();
    ]])],
    octave_cv_lib_arpack_ok=yes,
    octave_cv_lib_arpack_ok=no,
    octave_cv_lib_arpack_ok=yes)
    AC_LANG_POP(C++)
  ])
  if test $octave_cv_lib_arpack_ok = yes; then
    $1
    :
  else
    $2
    :
  fi
])
dnl
dnl Check whether GLPK provides the latest API functions required
dnl for the glpk function. The glp_iptcp structure was introduced
dnl in GLPK version 4.38.
dnl
AC_DEFUN([OCTAVE_CHECK_LIB_GLPK_OK], [
  AC_CACHE_CHECK([whether the glpk library has glp_interior(glp_prob*, glp_iptcp*)],
    [octave_cv_lib_glpk_ok],
    [AC_LANG_PUSH(C++)
    AC_LINK_IFELSE([AC_LANG_PROGRAM([[
        extern "C"
        {
        #if defined (HAVE_GLPK_GLPK_H)
        #include <glpk/glpk.h>
        #else
        #include <glpk.h>
        #endif
        }
        ]], [[
        glp_prob *lp = glp_create_prob ();
        glp_iptcp iptcp;
        glp_init_iptcp (&iptcp);
        int retval = glp_interior (lp, &iptcp);
        ]])],
      octave_cv_lib_glpk_ok=yes,
      octave_cv_lib_glpk_ok=no)
    AC_LANG_POP(C++)
  ])
  if test $octave_cv_lib_glpk_ok = yes; then
    $1
    :
  else
    $2
    :
  fi
])
dnl
dnl Check whether using HDF5 DLL under Windows.  This is done by
dnl testing for a data symbol in the HDF5 library, which would
dnl require the definition of _HDF5USEDL_ under MSVC compiler.
dnl
AC_DEFUN([OCTAVE_CHECK_LIB_HDF5_DLL], [
  AC_CACHE_CHECK([if _HDF5USEDLL_ needs to be defined],
    [octave_cv_lib_hdf5_dll],
    [AC_LINK_IFELSE([AC_LANG_PROGRAM([[
        #include <hdf5.h>
        ]], [[
        hid_t x = H5T_NATIVE_DOUBLE;
        return x
      ]])],
      [octave_cv_lib_hdf5_dll=no],
      [save_CFLAGS="$CFLAGS"
      CFLAGS="$CFLAGS -DWIN32 -D_HDF5USEDLL_"
      save_LIBS="$LIBS"
      LIBS="$HDF5_LIBS $LIBS"
      AC_LINK_IFELSE([AC_LANG_PROGRAM([[
          #include <hdf5.h>
          ]], [[
          hid_t x = H5T_NATIVE_DOUBLE;
          return x
        ]])],
        octave_cv_lib_hdf5_dll=yes,
        octave_cv_lib_hdf5_dll=no)
      CFLAGS="$save_CFLAGS"
      LIBS="$save_LIBS"
    ])
  ])
  if test $octave_cv_lib_hdf5_dll = yes; then
    AC_DEFINE(_HDF5USEDLL_, 1, [Define to 1 if using HDF5 dll (Win32).])
  fi
])
dnl
dnl Check for OpenGL.  If found, define OPENGL_LIBS.
dnl
dnl FIXME: The following tests should probably check for the
dnl libraries separately.
dnl
dnl FIXME: Should we allow a way to specify a directory for OpenGL
dnl libraries and header files?
dnl
AC_DEFUN([OCTAVE_CHECK_LIB_OPENGL], [
  OPENGL_LIBS=

  ## On MacOSX systems the OpenGL framework can be used
  OCTAVE_HAVE_FRAMEWORK(OpenGL, [[
    #include <OpenGL/gl.h>
    #include <OpenGL/glu.h>
    ]], [[
    GLint par; glGetIntegerv (GL_VIEWPORT, &par);
    ]],
    have_framework_opengl=yes, have_framework_opengl=no)

  if test $have_framework_opengl = yes; then
    AC_DEFINE(HAVE_FRAMEWORK_OPENGL, 1,
      [Define to 1 if framework OPENGL is available.])
    OPENGL_LIBS="-Wl,-framework -Wl,OpenGL"
    AC_MSG_NOTICE([adding -Wl,-framework -Wl,OpenGL to OPENGL_LIBS])
    OCTAVE_CHECK_FUNC_GLUTESSCALLBACK_THREEDOTS
  else
    case $canonical_host_type in
      *-*-mingw32* | *-*-msdosmsvc)
        AC_CHECK_HEADERS([windows.h])
      ;;
    esac
    have_opengl_incs=no
    AC_CHECK_HEADERS([GL/gl.h OpenGL/gl.h],
      [AC_CHECK_HEADERS([GL/glu.h OpenGL/glu.h],
        [have_opengl_incs=yes; break], [], [
#ifdef HAVE_WINDOWS_H
#include <windows.h>
#endif
      ])
      break
      ], [], [
#ifdef HAVE_WINDOWS_H
# include <windows.h>
#endif
    ])

    if test $have_opengl_incs = yes; then
      AC_CHECK_HEADERS([GL/glext.h OpenGL/glext.h], [], [], [
#ifdef HAVE_WINDOWS_H
# include <windows.h>
#endif
#if defined (HAVE_GL_GL_H)
# include <GL/gl.h>
#elif defined (HAVE_OPENGL_GL_H)
# include <OpenGL/gl.h>
#endif
      ])
      case $canonical_host_type in
        *-*-mingw32* | *-*-msdosmsvc)
          save_LIBS="$LIBS"
          LIBS="$LIBS -lopengl32"
          AC_MSG_CHECKING([for glEnable in -lopengl32])
          AC_LINK_IFELSE([AC_LANG_PROGRAM([[
            #if HAVE_WINDOWS_H
            # include <windows.h>
            #endif
            #if defined (HAVE_GL_GL_H)
            # include <GL/gl.h>
            #elif defined (HAVE_OPENGL_GL_H)
            # include <OpenGL/gl.h>
            #endif
            ]], [[
            glEnable(GL_SMOOTH);
            ]])], [OPENGL_LIBS="-lopengl32 -lglu32"])

          LIBS="$save_LIBS"
          if test "x$OPENGL_LIBS" != "x"; then
            AC_MSG_RESULT([yes])
          else
            AC_MSG_RESULT([no])
          fi
          ;;
        *)
          ## Non-Mac, Non-Windows systems use this check
          AC_CHECK_LIB([GL], [glEnable], [OPENGL_LIBS="-lGL -lGLU"])
          ;;
      esac
    fi
  fi
  AC_SUBST(OPENGL_LIBS)
])
dnl
dnl Check whether Qhull works (does not crash).
dnl
AC_DEFUN([OCTAVE_CHECK_LIB_QHULL_OK], [
  AC_CACHE_CHECK([whether the qhull library works],
    [octave_cv_lib_qhull_ok],
    [AC_RUN_IFELSE([AC_LANG_PROGRAM([[
        #include <stdio.h>
        #if defined (HAVE_LIBQHULL_LIBQHULL_H)
        # include <libqhull/libqhull.h>
        # include <libqhull/qset.h>
        # include <libqhull/geom.h>
        # include <libqhull/poly.h>
        # include <libqhull/io.h>
        #elif defined (HAVE_QHULL_LIBQHULL_H) || defined (HAVE_QHULL_QHULL_H)
        # if defined (HAVE_QHULL_LIBQHULL_H)
        #  include <qhull/libqhull.h>
        # else
        #  include <qhull/qhull.h>
        # endif
        # include <qhull/qset.h>
        # include <qhull/geom.h>
        # include <qhull/poly.h>
        # include <qhull/io.h>
        #elif defined (HAVE_LIBQHULL_H) || defined (HAVE_QHULL_H)
        # if defined (HAVE_LIBQHULL_H)
        #  include <libqhull.h>
        # else
        #  include <qhull.h>
        # endif
        # include <qset.h>
        # include <geom.h>
        # include <poly.h>
        # include <io.h>
        #endif
        #ifdef NEED_QHULL_VERSION
          char *qh_version = "version";
        #endif
        ]], [[
        int dim = 2;
        int n = 4;
        coordT points[8] = { -0.5, -0.5, -0.5, 0.5, 0.5, -0.5, 0.5, 0.5 };
        boolT ismalloc = 0;
        return qh_new_qhull (dim, n, points, ismalloc, "qhull ", 0, stderr);
      ]])],
      octave_cv_lib_qhull_ok=yes,
      octave_cv_lib_qhull_ok=no,
      octave_cv_lib_qhull_ok=yes)
  ])
  if test $octave_cv_lib_qhull_ok = yes; then
    $1
    :
  else
    $2
    :
  fi
])
dnl 
dnl Check whether sndfile library is modern enough to include things like Ogg
dnl
AC_DEFUN([OCTAVE_CHECK_LIB_SNDFILE_OK], [
  AC_CACHE_CHECK([whether sndfile library is modern enough],
    [octave_cv_lib_sndfile_ok],
    [AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
        #include <sndfile.h>
        ]], [[
        int x = SF_FORMAT_OGG;
      ]])],
      octave_cv_lib_sndfile_ok=yes,
      octave_cv_lib_sndfile_ok=no)
  ])
  if test $octave_cv_lib_sndfile_ok = yes; then
    $1
    :
  else
    $2
    :
  fi
])
dnl
dnl Find a suitable termlib to use.
dnl
AC_DEFUN([OCTAVE_CHECK_LIB_TERMLIB], [
  TERM_LIBS=
  ac_octave_save_LIBS="$LIBS"
  AC_SEARCH_LIBS([tputs],
                 [ncurses curses termcap terminfo termlib],
                 [], [])
  LIBS="$ac_octave_save_LIBS"
  case "$ac_cv_search_tputs" in
    -l*)
      TERM_LIBS="$ac_cv_search_tputs"
    ;;
    no)
      warn_termlibs="I couldn't find -ltermcap, -lterminfo, -lncurses, -lcurses, or -ltermlib!"
      AC_MSG_WARN([$warn_termlibs])
    ;;
  esac

dnl  Old code (9/9/2012).  Delete when new code is definitely proven.
dnl
dnl  for _termlib in ncurses curses termcap terminfo termlib; do
dnl    AC_CHECK_LIB([${_termlib}], [tputs], [
dnl      TERM_LIBS="-l${termlib}"
dnl      octave_cv_lib_found_termlib=yes
dnl      break])
dnl  done

  AC_SUBST(TERM_LIBS)
])
dnl
dnl Check for the Qhull version.
dnl
AC_DEFUN([OCTAVE_CHECK_QHULL_VERSION], [
  AC_CACHE_CHECK([for qh_version in $QHULL_LIBS],
    [octave_cv_lib_qhull_version],
    [AC_LINK_IFELSE([AC_LANG_PROGRAM([[
        #include <stdio.h>
        #if defined (HAVE_LIBQHULL_LIBQHULL_H)
        # include <libqhull/libqhull.h>
        # include <libqhull/qset.h>
        # include <libqhull/geom.h>
        # include <libqhull/poly.h>
        # include <libqhull/io.h>
        #elif defined (HAVE_QHULL_LIBQHULL_H) || defined (HAVE_QHULL_QHULL_H)
        # if defined (HAVE_QHULL_LIBQHULL_H)
        #  include <qhull/libqhull.h>
        # else
        #  include <qhull/qhull.h>
        # endif
        # include <qhull/qset.h>
        # include <qhull/geom.h>
        # include <qhull/poly.h>
        # include <qhull/io.h>
        #elif defined (HAVE_LIBQHULL_H) || defined (HAVE_QHULL_H)
        # if defined (HAVE_LIBQHULL_H)
        #  include <libqhull.h>
        # else
        #  include <qhull.h>
        # endif
        # include <qset.h>
        # include <geom.h>
        # include <poly.h>
        # include <io.h>
        #endif
        ]], [[
        const char *tmp = qh_version;
      ]])],
      octave_cv_lib_qhull_version=yes, octave_cv_lib_qhull_version=no)
  ])
  if test $octave_cv_lib_qhull_version = no; then
    AC_DEFINE(NEED_QHULL_VERSION, 1,
      [Define to 1 if the Qhull library needs a qh_version variable defined.])
  fi
])
dnl
dnl Check if the default Fortran INTEGER is 64 bits wide.
dnl
AC_DEFUN([OCTAVE_CHECK_SIZEOF_FORTRAN_INTEGER], [
  AC_CACHE_CHECK([whether $F77 generates correct size integers],
    [octave_cv_sizeof_fortran_integer],
    [ac_octave_save_FFLAGS="$FFLAGS"
    FFLAGS="$FFLAGS $F77_INTEGER_8_FLAG"
    AC_LANG_PUSH(Fortran 77)
    AC_COMPILE_IFELSE([[
      subroutine foo(n, in, out)
      integer n, in(n), out(n)
      integer i
      do 10 i = 1, n
        out(i) = in(i)
   10 continue
      return
      end
      ]],
      [mv conftest.$ac_objext fintsize.$ac_objext
      ac_octave_save_LIBS="$LIBS"
      LIBS="fintsize.$ac_objext $[]_AC_LANG_PREFIX[]LIBS"
      AC_LANG_PUSH(C)
      AC_RUN_IFELSE([AC_LANG_PROGRAM([[
          #include <assert.h>
          #include <stdint.h>
          ]], [[
          #ifdef USE_64_BIT_IDX_T
            typedef int64_t octave_idx_type;
          #else
            typedef int octave_idx_type;
          #endif
          octave_idx_type n = 2;
          octave_idx_type in[2];
          octave_idx_type out[2];
          in[0] = 13;
          in[0] = 42;
          F77_FUNC(foo,FOO) (&n, &in, &out);
          assert (in[0] == out[0] && in[1] == out[1]);
        ]])],
        octave_cv_sizeof_fortran_integer=yes,
        octave_cv_sizeof_fortran_integer=no,
        octave_cv_sizeof_fortran_integer=yes)
      AC_LANG_POP(C)
      LIBS="$ac_octave_save_LIBS"
      rm -f conftest.$ac_objext fintsize.$ac_objext],
      [rm -f conftest.$ac_objext
      AC_MSG_FAILURE([cannot compile a simple Fortran program])
      octave_cv_sizeof_fortran_integer=no])
    AC_LANG_POP(Fortran 77)
    FFLAGS="$ac_octave_save_FFLAGS"
  ])
])
dnl
dnl Add warning to final summary.
dnl
AC_DEFUN([OCTAVE_CONFIGURE_WARNING], [
  AC_MSG_WARN([$][$1])
  m4_set_add([summary_warning_list], [$1])
])
dnl
dnl Print final summary.
dnl
AC_DEFUN([OCTAVE_CONFIGURE_WARNING_SUMMARY], [
  m4_set_foreach([summary_warning_list], [elt], [
    if test -n "[$]elt"; then
      AC_MSG_WARN([$]elt)
      warn_msg_printed=true
    fi])
])
dnl
dnl Check if the C++ library has the bit_and, bit_or, and bit_xor
dnl templates defined.
dnl
AC_DEFUN([OCTAVE_CXX_BITWISE_OP_TEMPLATES], [
  AC_CACHE_CHECK([whether bit_and, bit_or, bit_xor are defined in the C++ library],
    [octave_cv_cxx_bitwise_op_templates],
    [AC_LANG_PUSH(C++)
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
        #include <functional>
        ]], [[
        int x = 0;
        int y = 1;
        int z1 = std::bit_and<int>() (x, y);
        int z2 = std::bit_or<int>() (x, y);
        int z3 = std::bit_xor<int>() (x, y);
      ]])],
      octave_cv_cxx_bitwise_op_templates=yes,
      octave_cv_cxx_bitwise_op_templates=no)
    AC_LANG_POP(C++)
  ])
  if test $octave_cv_cxx_bitwise_op_templates = yes; then
    AC_DEFINE(HAVE_CXX_BITWISE_OP_TEMPLATES, 1,
      [Define to 1 if C++ library has templated bitwise operators.])
  fi
])
dnl
dnl Check if C++ reinterpret cast works for function pointers.
dnl
AC_DEFUN([OCTAVE_CXX_BROKEN_REINTERPRET_CAST],
  [AC_REQUIRE([AC_PROG_CXX])
  AC_CACHE_CHECK([for broken C++ reinterpret_cast],
    [octave_cv_cxx_broken_reinterpret_cast],
    [AC_LANG_PUSH(C++)
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
        #include <cmath> ]], [[
        typedef double (*fptr) (double);
        fptr psin = sin;
        void *vptr = reinterpret_cast<void *> (psin);
        psin = reinterpret_cast<fptr> (vptr);
      ]])],
      octave_cv_cxx_broken_reinterpret_cast=no,
      octave_cv_cxx_broken_reinterpret_cast=yes)
    AC_LANG_POP(C++)
  ])
  if test $octave_cv_cxx_broken_reinterpret_cast = yes; then
    AC_DEFINE(CXX_BROKEN_REINTERPRET_CAST, 1,
      [Define to 1 if C++ reinterpret_cast fails for function pointers.])
  fi
])
dnl
dnl Check if the C++ library has functions to access real and imaginary
dnl parts of complex numbers independently via references.
dnl
AC_DEFUN([OCTAVE_CXX_COMPLEX_REFERENCE_ACCESSORS], [
  AC_CACHE_CHECK([whether complex class can reference components independently],
    [octave_cv_cxx_complex_reference_accessors],
    [AC_LANG_PUSH(C++)
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
        #include <complex>
        ]], [[
        std::complex<double> x;
        x.real () = 1.0;
        x.imag () = 1.0;
      ]])],
      octave_cv_cxx_complex_reference_accessors=yes,
      octave_cv_cxx_complex_reference_accessors=no)
    AC_LANG_POP(C++)
  ])
  if test $octave_cv_cxx_complex_reference_accessors = yes; then
    AC_DEFINE(HAVE_CXX_COMPLEX_REFERENCE_ACCESSORS, 1,
      [Define to 1 if C++ complex class has T& real (void) and T& imag (void) methods.])
  fi
])
dnl
dnl Check if the C++ library has functions to set real and imaginary
dnl parts of complex numbers independently.
dnl
AC_DEFUN([OCTAVE_CXX_COMPLEX_SETTERS], [
  AC_CACHE_CHECK([whether complex class can set components independently],
    [octave_cv_cxx_complex_setters],
    [AC_LANG_PUSH(C++)
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
        #include <complex>
        ]], [[
        std::complex<double> x;
        x.real (1.0);
        x.imag (2.0);
      ]])],
      octave_cv_cxx_complex_setters=yes, octave_cv_cxx_complex_setters=no)
    AC_LANG_POP(C++)
  ])
  if test $octave_cv_cxx_complex_setters = yes; then
    AC_DEFINE(HAVE_CXX_COMPLEX_SETTERS, 1,
      [Define to 1 if C++ complex class has void real (T) and void imag (T) methods.])
  fi
])
dnl
dnl Check if the compiler supports dynamic auto arrays.
dnl
AC_DEFUN([OCTAVE_CXX_DYNAMIC_AUTO_ARRAYS], [
  AC_CACHE_CHECK([whether C++ supports dynamic auto arrays],
    [octave_cv_cxx_dynamic_auto_arrays],
    [AC_LANG_PUSH(C++)
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([], [[
        void test(char *);
        int length();
        char x[length()];
        test(x);
      ]])],
      octave_cv_cxx_dynamic_auto_arrays=yes,
      octave_cv_cxx_dynamic_auto_arrays=no)
    AC_LANG_POP(C++)
  ])
  if test $octave_cv_cxx_dynamic_auto_arrays = yes; then
    AC_DEFINE(HAVE_DYNAMIC_AUTO_ARRAYS, 1,
      [Define to 1 if C++ supports dynamic auto arrays.])
  fi
])
dnl
dnl Check if C++ compiler handles FLAG command line option.  If two
dnl arguments are specified, execute the second arg as shell commands.
dnl Otherwise, add FLAG to CXXFLAGS if the compiler accepts the flag.
dnl
AC_DEFUN([OCTAVE_CXX_FLAG], [
  ac_safe=`echo "$1" | $SED 'y%./+-:=%__p___%'`
  AC_MSG_CHECKING([whether ${CXX-g++} accepts $1])
  AC_CACHE_VAL([octave_cv_cxx_flag_$ac_safe],
    [AC_LANG_PUSH(C++)
    ac_octave_save_CXXFLAGS="$CXXFLAGS"
    CXXFLAGS="$CXXFLAGS $1"
    AC_LINK_IFELSE([AC_LANG_PROGRAM([], [])],
      eval "octave_cv_cxx_flag_$ac_safe=yes",
      eval "octave_cv_cxx_flag_$ac_safe=no")
    CXXFLAGS="$ac_octave_save_CXXFLAGS"
    AC_LANG_POP(C++)
  ])
  if eval "test \"`echo '$octave_cv_cxx_flag_'$ac_safe`\" = yes"; then
    AC_MSG_RESULT([yes])
    ifelse([$2], ,
      [CXXFLAGS="$CXXFLAGS $1"
      AC_MSG_RESULT([adding $1 to CXXFLAGS])], [$2])
  else
    AC_MSG_RESULT([no])
    ifelse([$3], , , [$3])
  fi
])
dnl
dnl Check if the C++ library is ISO compliant.
dnl FIXME: This is obviously very simplistic, and trivially fooled.
dnl
AC_DEFUN([OCTAVE_CXX_ISO_COMPLIANT_LIBRARY], [
  AC_REQUIRE([AC_PROG_CXX])
  AC_MSG_CHECKING([if C++ library is ISO compliant])
  AC_CACHE_VAL([octave_cv_cxx_iso_compliant_library],
    [AC_LANG_PUSH(C++)
    rm -f conftest.h
    ## Omitting cwctype for now, since it is broken with gcc-3.0.x and
    ## possibly other versions...
    for inc in algorithm bitset cassert cctype cerrno cfloat ciso646 \
        climits clocale cmath complex csetjmp csignal cstdarg cstddef \
        cstdio cstdlib cstring ctime cwchar deque exception \
        fstream functional iomanip ios iosfwd iostream istream iterator \
        limits list locale map memory new numeric ostream queue set \
        sstream stack stdexcept streambuf string strstream typeinfo \
        utility valarray vector; do
      echo "#include <$inc>" >> conftest.h
    done
    AC_LINK_IFELSE([AC_LANG_PROGRAM([[
        #include "conftest.h"
        ]], [[
        std::bitset<50> flags;
        flags.set();
        int digits = std::numeric_limits<unsigned long>::digits;
        digits = 0;
      ]])],
      octave_cv_cxx_iso_compliant_library=yes,
      octave_cv_cxx_iso_compliant_library=no)
    AC_LANG_POP(C++)
  ])
  AC_MSG_RESULT([$octave_cv_cxx_iso_compliant_library])
  if test $octave_cv_cxx_iso_compliant_library = yes; then
    AC_DEFINE(CXX_ISO_COMPLIANT_LIBRARY, 1,
      [Define to 1 if your C++ runtime library is ISO compliant.])
  fi
])
dnl
dnl Check if C++ compiler needs the new friend template declaration
dnl syntax.
dnl
AC_DEFUN([OCTAVE_CXX_NEW_FRIEND_TEMPLATE_DECL], [
  AC_REQUIRE([AC_PROG_CXX])
  AC_MSG_CHECKING([for C++ support for new friend template declaration])
  AC_CACHE_VAL([octave_cv_cxx_new_friend_template_decl],
    [AC_LANG_PUSH(C++)
    rm -f conftest.h
    cat > conftest.h <<EOB
       struct A {
         friend int operator== (const A&, const A&);
         A (int) { }
       };

       template <class T> int
       operator== (const T&, const T&)
       {
         return 0;
       }
EOB
    AC_LINK_IFELSE([AC_LANG_PROGRAM([[
        #include "conftest.h"
        ]], [[
        A a (1);
        return a == A(1);
      ]])],
      octave_cv_cxx_new_friend_template_decl=no,
      octave_cv_cxx_new_friend_template_decl=yes)
    AC_LANG_POP(C++)
  ])
  AC_MSG_RESULT([$octave_cv_cxx_new_friend_template_decl])
  if test $octave_cv_cxx_new_friend_template_decl = yes; then
    AC_DEFINE(CXX_NEW_FRIEND_TEMPLATE_DECL, 1,
      [Define to 1 if your compiler supports `<>' stuff for template friends.])
  fi
])
dnl
dnl Check if the compiler supports placement delete.
dnl
AC_DEFUN([OCTAVE_CXX_PLACEMENT_DELETE], [
  AC_CACHE_CHECK([whether <new> defines placement delete operator],
    [octave_cv_cxx_placement_delete],
    [AC_LANG_PUSH(C++)
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
        #include <new>
        ]], [[
        operator delete((void *)0, (void *)0);
      ]])],
      octave_cv_cxx_placement_delete=yes,
      octave_cv_cxx_placement_delete=no)
    AC_LANG_POP(C++)
  ])
  if test $octave_cv_cxx_placement_delete = yes; then
    AC_DEFINE(HAVE_PLACEMENT_DELETE, 1,
      [Define to 1 if C++ supports operator delete(void *, void *).])
  fi
])
dnl
dnl Allow the user disable support for command line editing using GNU
dnl readline.
dnl
AC_DEFUN([OCTAVE_ENABLE_READLINE], [
  USE_READLINE=yes
  READLINE_LIBS=
  AC_ARG_ENABLE([readline],
    [AS_HELP_STRING([--disable-readline],
      [do not use readline library])],
    [if test "$enableval" = no; then
       USE_READLINE=no
       warn_readline="command editing and history features require GNU Readline"
     fi])
  if test $USE_READLINE = yes; then
    dnl RHEL 5 and older systems require termlib set before enabling readline
    AC_REQUIRE([OCTAVE_CHECK_LIB_TERMLIB])
    ac_octave_save_LIBS="$LIBS"
    LIBS="$TERM_LIBS"
    AC_CHECK_LIB([readline], [rl_set_keyboard_input_timeout],
      [READLINE_LIBS="-lreadline"
      AC_DEFINE(USE_READLINE, 1, [Define to 1 to use the readline library.])
      ],
      [AC_MSG_WARN([I need GNU Readline 4.2 or later])
      AC_MSG_ERROR([this is fatal unless you specify --disable-readline])
    ])
    LIBS="$ac_octave_save_LIBS"
  fi
  AC_SUBST(READLINE_LIBS)
])
dnl
dnl Check if Fortran compiler handles FLAG command line option.  If
dnl two arguments are specified, execute the second arg as shell
dnl commands.  Otherwise, add FLAG to FFLAGS if the compiler accepts
dnl the flag.
dnl
AC_DEFUN([OCTAVE_F77_FLAG], [
  ac_safe=`echo "$1" | $SED 'y%./+-:=%__p___%'`
  AC_MSG_CHECKING([whether ${F77-g77} accepts $1])
  AC_CACHE_VAL([octave_cv_f77_flag_$ac_safe], [
    AC_LANG_PUSH(Fortran 77)
    ac_octave_save_FFLAGS="$FFLAGS"
    FFLAGS="$FFLAGS $1"
    AC_LINK_IFELSE([AC_LANG_PROGRAM([], [])],
      eval "octave_cv_f77_flag_$ac_safe=yes",
      eval "octave_cv_f77_flag_$ac_safe=no")
    FFLAGS="$ac_octave_save_FFLAGS"
    AC_LANG_POP(Fortran 77)
  ])
  if eval "test \"`echo '$octave_cv_f77_flag_'$ac_safe`\" = yes"; then
    AC_MSG_RESULT([yes])
    ifelse([$2], ,
      [FFLAGS="$FFLAGS $1"
      AC_MSG_RESULT([adding $1 to FFLAGS])], [$2])
  else
    AC_MSG_RESULT([no])
    ifelse([$3], , , [$3])
  fi
])
dnl
dnl Check whether fast signed integer arithmetics using bit tricks
dnl can be used in oct-inttypes.h.  Defines HAVE_FAST_INT_OPS if
dnl the following conditions hold:
dnl 1. Signed numbers are represented by twos complement
dnl    (see <http://en.wikipedia.org/wiki/Two%27s_complement>)
dnl 2. static_cast to unsigned int counterpart works like interpreting
dnl    the signed bit pattern as unsigned (and is thus zero-cost).
dnl 3. Signed addition and subtraction yield the same bit results as unsigned.
dnl    (We use casts to prevent optimization interference, so there is no
dnl     need for things like -ftrapv).
dnl 4. Bit operations on signed integers work like on unsigned integers,
dnl    except for the shifts.  Shifts are arithmetic.
dnl
AC_DEFUN([OCTAVE_FAST_INT_OPS], [
  AC_CACHE_CHECK([whether fast integer arithmetics is usable],
    [octave_cv_fast_int_ops],
    [AC_LANG_PUSH(C++)
    AC_RUN_IFELSE([AC_LANG_PROGRAM([[
        #include <limits>
        template<class UT, class ST>
        static bool
        do_test (UT, ST)
        {
          volatile ST s = std::numeric_limits<ST>::min () / 3;
          volatile UT u = static_cast<UT> (s);
          if (*(reinterpret_cast<volatile ST *> (&u)) != s) return true;

          u = 0; u = ~u;
          if (*(reinterpret_cast<volatile ST *> (&u)) != -1) return true;

          ST sx, sy;
          sx = std::numeric_limits<ST>::max () / 2 + 1;
          sy = std::numeric_limits<ST>::max () / 2 + 2;
          if (static_cast<ST> (static_cast<UT> (sx) + static_cast<UT> (sy))
              != std::numeric_limits<ST>::min () + 1) return true;
          if (static_cast<ST> (static_cast<UT> (sx) - static_cast<UT> (sy))
              != -1) return true;

          if ((sx & sy) != (static_cast<UT> (sx) & static_cast<UT> (sy)))
            return true;
          if ((sx | sy) != (static_cast<UT> (sx) | static_cast<UT> (sy)))
            return true;
          if ((sx ^ sy) != (static_cast<UT> (sx) ^ static_cast<UT> (sy)))
            return true;
          if ((-1 >> 1) != -1) return true;
          return false;
        }

        #define DO_TEST(T) \
          if (do_test (static_cast<unsigned T> (0), static_cast<signed T> (0)))\
            return sizeof (T);

        ]],[[

        DO_TEST(char)
        DO_TEST(short)
        DO_TEST(int)
        DO_TEST(long)
        #if (defined(HAVE_LONG_LONG_INT) && defined(HAVE_UNSIGNED_LONG_LONG_INT))
          DO_TEST(long long)
        #endif
      ]])],
      octave_cv_fast_int_ops=yes,
      octave_cv_fast_int_ops=no,
      octave_cv_fast_int_ops=yes)
    AC_LANG_POP(C++)
  ])
  if test $octave_cv_fast_int_ops = yes; then
    AC_DEFINE(HAVE_FAST_INT_OPS, 1,
      [Define to 1 if signed integers use two's complement.])
  fi
])
dnl
dnl Check to see if the compiler and the linker can handle the flags
dnl "-framework $1" for the given prologue $2 and the given body $3 of
dnl a source file.  Arguments 2 and 3 optionally can also be empty.
dnl Add options (lower case letters $1) "--with-framework-$1" and
dnl "--without-framework-$1".  If this test is successful then perform
dnl $4, otherwise do $5.
dnl
AC_DEFUN([OCTAVE_HAVE_FRAMEWORK], [
  AC_MSG_CHECKING([whether ${LD-ld} accepts -framework $1])
  AC_CACHE_VAL([octave_cv_framework_$1],
    [ac_octave_save_LDFLAGS="$LDFLAGS"
    LDFLAGS="$LDFLAGS -framework $1"
    AC_LANG_PUSH(C++)
    AC_LINK_IFELSE([AC_LANG_PROGRAM([$2], [$3])],
      eval "octave_cv_framework_$1=yes",
      eval "octave_cv_framework_$1=no")
    AC_LANG_POP(C++)
    LDFLAGS="$ac_octave_save_LDFLAGS"
  ])
  if test "$octave_cv_framework_$1" = yes; then
    AC_MSG_RESULT([yes])
    AC_ARG_WITH(framework-m4_tolower($1),
      [AS_HELP_STRING([--without-framework-m4_tolower($1)],
        [don't use framework $1])],
         with_have_framework=$withval, with_have_framework=yes)
    if test "$with_have_framework" = yes; then
      [$4]
      :
    else
      AC_MSG_NOTICE([framework rejected by --without-framework-m4_tolower($1)])
      [$5]
    fi
  else
    AC_MSG_RESULT([no])
    [$5]
  fi
])
dnl
dnl Figure out the hardware-vendor-os info.
dnl
AC_DEFUN([OCTAVE_CANONICAL_HOST], [
  AC_CANONICAL_HOST
  if test -z "$host"; then
    host=unknown-unknown-unknown
    AC_MSG_WARN([configuring Octave for unknown system type])
  fi
  canonical_host_type=$host
  AC_SUBST(canonical_host_type)
  if test -z "$host_cpu"; then
    host_cpu=unknown
  fi
  if test -z "$host_vendor"; then
    host_vendor=unknown
  fi
  if test -z "$host_os"; then
    host_os=unknown
  fi
])
dnl
dnl Check for IEEE 754 data format.
dnl
AC_DEFUN([OCTAVE_IEEE754_DATA_FORMAT], [
  AC_MSG_CHECKING([for IEEE 754 data format])
  AC_CACHE_VAL([octave_cv_ieee754_data_format],
    [AC_RUN_IFELSE([AC_LANG_SOURCE([[
        int
        main (void)
        {
          typedef union { unsigned char c[8]; double d; } ieeebytes;

          ieeebytes l = {0x1c, 0xbc, 0x6e, 0xf2, 0x54, 0x8b, 0x11, 0x43};
          ieeebytes b = {0x43, 0x11, 0x8b, 0x54, 0xf2, 0x6e, 0xbc, 0x1c};

          return l.d != 1234567891234567.0 && b.d != 1234567891234567.0;
        }
      ]])],
      octave_cv_ieee754_data_format=yes,
      octave_cv_ieee754_data_format=no,
      octave_cv_ieee754_data_format=yes)
  ])
  if test "$cross_compiling" = yes; then
    AC_MSG_RESULT([$octave_cv_ieee754_data_format assumed for cross compilation])
  else
    AC_MSG_RESULT([$octave_cv_ieee754_data_format])
  fi
  if test $octave_cv_ieee754_data_format = yes; then
    AC_DEFINE(HAVE_IEEE754_DATA_FORMAT, 1,
      [Define to 1 if your system uses IEEE 754 data format.])
  else
    ## If the format is unknown, then you will probably not have a
    ## useful system, so we will abort here.  Anyone wishing to
    ## experiment with building Octave on a system without IEEE
    ## floating point should be capable of removing this check and
    ## the one in the octave_ieee_init function in liboctave/lo-ieee.cc.
    AC_MSG_ERROR([IEEE 754 data format required for building Octave])
  fi
])
dnl
dnl Check for CallInst::addAttribute API
dnl
AC_DEFUN([OCTAVE_LLVM_CALLINST_ADDATTRIBUTE_API], [
  AC_CACHE_CHECK([check LLVM::CallInst::addAttribute arg type is llvm::Attributes],
    [octave_cv_callinst_addattribute_arg_is_attributes],
    [AC_LANG_PUSH(C++)
      AC_COMPILE_IFELSE(
        [AC_LANG_PROGRAM([[
#ifdef HAVE_LLVM_IR_FUNCTION_H
          #include <llvm/IR/Instructions.h>
          #include <llvm/IR/Attributes.h>
#else
          #include <llvm/Instructions.h>
          #include <llvm/Attributes.h>
#endif
          ]], [[
          llvm::CallInst *callinst;
          llvm::AttrBuilder attr_builder;
          attr_builder.addAttribute(llvm::Attributes::StructRet);
          llvm::Attributes attrs = llvm::Attributes::get(llvm::getGlobalContext(), attr_builder);
          callinst->addAttribute (1, attrs);
        ]])],
        octave_cv_callinst_addattribute_arg_is_attributes=yes,
        octave_cv_callinst_addattribute_arg_is_attributes=no)
    AC_LANG_POP(C++)
  ])
  if test $octave_cv_callinst_addattribute_arg_is_attributes = yes; then
    AC_DEFINE(CALLINST_ADDATTRIBUTE_ARG_IS_ATTRIBUTES, 1,
      [Define to 1 if llvm::CallInst:addAttribute arg type is llvm::Attributes.])
  fi
])
dnl
dnl Check for Function::addAttribute API
dnl
AC_DEFUN([OCTAVE_LLVM_FUNCTION_ADDATTRIBUTE_API], [
  AC_CACHE_CHECK([check llvm::Function::addAttribute arg type is llvm::Attributes],
    [octave_cv_function_addattribute_arg_is_attributes],
    [AC_LANG_PUSH(C++)
      AC_COMPILE_IFELSE(
        [AC_LANG_PROGRAM([[
#ifdef HAVE_LLVM_IR_FUNCTION_H
          #include <llvm/IR/Function.h>
          #include <llvm/IR/Attributes.h>
          #include <llvm/IR/LLVMContext.h>
#else
          #include <llvm/Function.h>
          #include <llvm/Attributes.h>
          #include <llvm/LLVMContext.h>
#endif
          ]], [[
          llvm::Function *llvm_function;
          llvm::AttrBuilder attr_builder;
          attr_builder.addAttribute(llvm::Attributes::StructRet);
          llvm::Attributes attrs = llvm::Attributes::get(llvm::getGlobalContext(), attr_builder);
          llvm_function->addAttribute (1, attrs);
        ]])],
        octave_cv_function_addattribute_arg_is_attributes=yes,
        octave_cv_function_addattribute_arg_is_attributes=no)
    AC_LANG_POP(C++)
  ])
  if test $octave_cv_function_addattribute_arg_is_attributes = yes; then
    AC_DEFINE(FUNCTION_ADDATTRIBUTE_ARG_IS_ATTRIBUTES, 1,
      [Define to 1 if llvm::Function:addAttribute arg type is llvm::Attributes.])
  fi
])
dnl
dnl Check for Function::addFnAttr API
dnl
AC_DEFUN([OCTAVE_LLVM_FUNCTION_ADDFNATTR_API], [
  AC_CACHE_CHECK([check LLVM::Function::addFnAttr arg type is llvm::Attributes],
    [octave_cv_function_addfnattr_arg_is_attributes],
    [AC_LANG_PUSH(C++)
      AC_COMPILE_IFELSE(
        [AC_LANG_PROGRAM([[
#ifdef HAVE_LLVM_IR_FUNCTION_H
          #include <llvm/IR/Function.h>
          #include <llvm/IR/Attributes.h>
#else
          #include <llvm/Function.h>
          #include <llvm/Attributes.h>
#endif
          ]], [[
          llvm::Function *llvm_function;
          llvm_function->addFnAttr (llvm::Attributes::AlwaysInline);
        ]])],
        octave_cv_function_addfnattr_arg_is_attributes=yes,
        octave_cv_function_addfnattr_arg_is_attributes=no)
    AC_LANG_POP(C++)
  ])
  if test $octave_cv_function_addfnattr_arg_is_attributes = yes; then
    AC_DEFINE(FUNCTION_ADDFNATTR_ARG_IS_ATTRIBUTES, 1,
      [Define to 1 if llvm::Function:addFnAttr arg type is llvm::Attributes.])
  fi
])
dnl
dnl Check for raw_fd_ostream API
dnl
AC_DEFUN([OCTAVE_LLVM_RAW_FD_OSTREAM_API], [
  AC_CACHE_CHECK([check LLVM::raw_fd_ostream arg type is llvm::sys:fs],
    [octave_cv_raw_fd_ostream_arg_is_llvm_sys_fs],
    [AC_LANG_PUSH(C++)
      AC_COMPILE_IFELSE(
        [AC_LANG_PROGRAM([[
          #include <llvm/Support/raw_os_ostream.h>
          ]], [[
          std::string str;
          llvm::raw_fd_ostream fout ("", str, llvm::sys::fs::F_Binary);
        ]])],
        octave_cv_raw_fd_ostream_arg_is_llvm_sys_fs=yes,
        octave_cv_raw_fd_ostream_arg_is_llvm_sys_fs=no)
    AC_LANG_POP(C++)
  ])
  if test $octave_cv_raw_fd_ostream_arg_is_llvm_sys_fs = yes; then
    AC_DEFINE(RAW_FD_OSTREAM_ARG_IS_LLVM_SYS_FS, 1,
      [Define to 1 if LLVM::raw_fd_ostream arg type is llvm::sys:fs.])
  fi
])
dnl
dnl Check for legacy::PassManager API
dnl
AC_DEFUN([OCTAVE_LLVM_LEGACY_PASSMANAGER_API], [
  AC_CACHE_CHECK([check for LLVM::legacy::PassManager],
    [octave_cv_legacy_passmanager],
    [AC_LANG_PUSH(C++)
      save_LIBS="$LIBS"
      LIBS="$LLVM_LIBS $LIBS"
      AC_LINK_IFELSE(
        [AC_LANG_PROGRAM([[
          #include <llvm/IR/LegacyPassManager.h>
          ]], [[
          llvm::Module *module;
          llvm::legacy::PassManager *module_pass_manager;
          llvm::legacy::FunctionPassManager *pass_manager;
          module_pass_manager = new llvm::legacy::PassManager ();
          pass_manager = new llvm::legacy::FunctionPassManager (module);
        ]])],
        octave_cv_legacy_passmanager=yes,
        octave_cv_legacy_passmanager=no)
      LIBS="$save_LIBS"
    AC_LANG_POP(C++)
  ])
  if test $octave_cv_legacy_passmanager = yes; then
    AC_DEFINE(LEGACY_PASSMANAGER, 1,
      [Define to 1 if LLVM::legacy::PassManager exists.])
  fi
])
dnl
dnl Check for ar.
dnl
AC_DEFUN([OCTAVE_PROG_AR], [
  if test -z "$AR"; then
    AR=ar
  fi
  AC_SUBST(AR)

  if test -z "$ARFLAGS"; then
    ARFLAGS="rc"
  fi
  AC_SUBST(ARFLAGS)
])
dnl
dnl Check for bison.
dnl
AC_DEFUN([OCTAVE_PROG_BISON], [
  AC_PROG_YACC

  case "`$YACC --version`" in
    *bison*) tmp_have_bison=yes ;;
    *) tmp_have_bison=no ;;
  esac

  if test $tmp_have_bison = yes; then
    AC_CACHE_CHECK([syntax of bison api.prefix (or name-prefix) declaration],
                   [octave_cv_bison_api_prefix_decl_style], [
      style="api name"
      quote="quote brace"
      for s in $style; do
        for q in $quote; do
          if test $s = "api"; then
            if test $q = "quote"; then
              def='%define api.prefix "foo_"'
            else
              def='%define api.prefix {foo_}'
            fi
          else
            if test $q = "quote"; then
              def='%name-prefix="foo_"'
            else
              def='%name-prefix {foo_}'
            fi
          fi
          cat << EOF > conftest.yy
$def
%start input
%%
input:;
%%
EOF
          ## Older versions of bison only warn and exit with success.
          octave_bison_output=`$YACC conftest.yy 2>&1`
          ac_status=$?
          if test $ac_status -eq 0 && test -z "$octave_bison_output"; then
            octave_cv_bison_api_prefix_decl_style="$s $q"
            break
          fi
        done
        if test -n "$octave_cv_bison_api_prefix_decl_style"; then
          break
        fi
      done
      rm -f conftest.yy y.tab.h y.tab.c
      ])
  fi

  AC_SUBST(BISON_API_PREFIX_DECL_STYLE, $octave_cv_bison_api_prefix_decl_style)

  if test -z "$octave_cv_bison_api_prefix_decl_style"; then
    tmp_have_bison=no
    warn_bison_api_prefix_decl_style="

I wasn't able to find a suitable style for declaring the api prefix
in a bison input file so I'm disabling bison.
"
    OCTAVE_CONFIGURE_WARNING([warn_bison_api_prefix_decl_style])
  fi

  if test $tmp_have_bison = yes; then
    AC_CACHE_CHECK([syntax of bison push/pull declaration],
                   [octave_cv_bison_push_pull_decl_style], [
      style="dash underscore"
      quote="noquote quote"
      for s in $style; do
        for q in $quote; do
          if test $s = "dash"; then
            def="%define api.push-pull"
          else
            def="%define api.push_pull"
          fi
          if test $q = "quote"; then
            def="$def \"both\""
          else
            def="$def both"
          fi
          cat << EOF > conftest.yy
$def
%start input
%%
input:;
%%
EOF
          octave_bison_output=`$YACC conftest.yy 2>&1`
          ac_status=$?
          if test $ac_status -eq 0 && test -z "$octave_bison_output"; then
            if test $q = noquote; then
              q=
            fi
            octave_cv_bison_push_pull_decl_style="$s $q"
            break
          fi
        done
        if test -n "$octave_cv_bison_push_pull_decl_style"; then
          break
        fi
      done
      rm -f conftest.yy y.tab.h y.tab.c
      ])
  fi

  AC_SUBST(BISON_PUSH_PULL_DECL_STYLE, $octave_cv_bison_push_pull_decl_style)

  if test -z "$octave_cv_bison_push_pull_decl_style"; then
    tmp_have_bison=no
    warn_bison_push_pull_decl_style="

I wasn't able to find a suitable style for declaring a push-pull
parser in a bison input file so I'm disabling bison.
"
    OCTAVE_CONFIGURE_WARNING([warn_bison_push_pull_decl_style])
  fi

  if test $tmp_have_bison = no; then
    YACC='$(top_srcdir)/build-aux/missing bison'
    warn_bison="

I didn't find bison, or the version of bison that I found does not
support all the features that are required, but it's only a problem
if you need to reconstruct parse.cc, which is the case if you're
building from VCS sources.
"
    OCTAVE_CONFIGURE_WARNING([warn_bison])
  fi
])
dnl
dnl Find desktop-file-install program.
dnl
AC_DEFUN([OCTAVE_PROG_DESKTOP_FILE_INSTALL], [
  AC_CHECK_PROG(DESKTOP_FILE_INSTALL, desktop-file-install, desktop-file-install, [])
  AC_SUBST(DESKTOP_FILE_INSTALL)
])
dnl
dnl Find find program.
dnl
## Prefer GNU find if found.
AN_MAKEVAR([FIND],  [OCTAVE_PROG_FIND])
AN_PROGRAM([gfind], [OCTAVE_PROG_FIND])
AN_PROGRAM([find],  [OCTAVE_PROG_FIND])
AC_DEFUN([OCTAVE_PROG_FIND], [
  AC_CHECK_PROGS(FIND, [gfind find])
])
dnl
dnl Check for flex.
dnl
AC_DEFUN([OCTAVE_PROG_FLEX], [
  ## For now, don't define LEXLIB to be -lfl -- we don't use anything in
  ## it, and it might not be installed.
  ##
  ## Also make sure that we generate an interactive scanner if we are
  ## using flex.
  AC_PROG_LEX
  case "`$LEX --version`" in
    *flex*)
      LFLAGS="-I"
      AC_MSG_RESULT([defining LFLAGS to be $LFLAGS])
      LEXLIB=
    ;;
    *)
      LEX='$(top_srcdir)/build-aux/missing flex'
      warn_flex="

I didn't find flex, but it's only a problem if you need to reconstruct
lex.cc, which is the case if you're building from VCS sources.
"
      OCTAVE_CONFIGURE_WARNING([warn_flex])
    ;;
  esac
  AC_SUBST(LFLAGS)
])
dnl
dnl Check for ghostscript.
dnl
AC_DEFUN([OCTAVE_PROG_GHOSTSCRIPT], [
  case "$canonical_host_type" in
    *-*-mingw* | *-*-msdosmsvc)
      ac_octave_gs_names="gswin32c gs mgs"
    ;;
    *)
      ac_octave_gs_names="gs"
    ;;
  esac
  AC_CHECK_PROGS(GHOSTSCRIPT, [$ac_octave_gs_names])
  if test -z "$GHOSTSCRIPT"; then
    GHOSTSCRIPT='$(top_srcdir)/build-aux/missing gs'
    warn_ghostscript="

I didn't find ghostscript, so reconstructing figures for the manual
will fail, and saving graphics in some output formats will fail when
using Octave
"
    OCTAVE_CONFIGURE_WARNING([warn_ghostscript])
  fi
  AC_SUBST(GHOSTSCRIPT)
])
dnl
dnl Check for gnuplot.
dnl
AC_DEFUN([OCTAVE_PROG_GNUPLOT], [
  ac_octave_gp_names="gnuplot"
  ac_octave_gp_default="gnuplot"
  if test "$cross_compiling" = yes; then
    GNUPLOT="$ac_octave_gp_default"
    AC_MSG_RESULT([assuming $GNUPLOT exists on $canonical_host_type host])
  else
    AC_CHECK_PROGS(GNUPLOT, [$ac_octave_gp_names])
    if test -z "$GNUPLOT"; then
      GNUPLOT="$gp_default"
      warn_gnuplot="

gnuplot not found.  It isn't necessary to have gnuplot installed, but
without native graphics or gnuplot you won't be able to use any of
Octave's plotting commands.
"
      OCTAVE_CONFIGURE_WARNING([warn_gnuplot])
    fi
  fi
  AC_SUBST(GNUPLOT)
])
dnl
dnl Check for gperf.
dnl
AC_DEFUN([OCTAVE_PROG_GPERF], [
  AC_CHECK_PROG(GPERF, gperf, gperf, [])
  if test -z "$GPERF"; then
    GPERF='$(top_srcdir)/build-aux/missing gperf'
    warn_gperf="

I didn't find gperf, but it's only a problem if you need to
reconstruct oct-gperf.h
"
    OCTAVE_CONFIGURE_WARNING([warn_gperf])
    GPERF='$(top_srcdir)/build-aux/missing gperf'
  fi
  AC_SUBST(GPERF)
])
dnl
dnl Find icotool program.
dnl
AC_DEFUN([OCTAVE_PROG_ICOTOOL], [
  AC_CHECK_PROG(ICOTOOL, icotool, icotool, [])
  if test -z "$ICOTOOL"; then
    ICOTOOL='$(top_srcdir)/build-aux/missing icotool'
    warn_icotool="

I didn't find icotool, but it's only a problem if you need to
reconstruct octave-logo.ico, which is the case if you're building from
VCS sources.
"
    OCTAVE_CONFIGURE_WARNING([warn_icotool])
  fi
  AC_SUBST(ICOTOOL)
])
dnl
dnl Check for makeinfo.
dnl
AC_DEFUN([OCTAVE_PROG_MAKEINFO], [
  dnl use MKINFO, not MAKEINFO, for variable name because Automake
  dnl automatically defines a value for MAKEINFO even when it does not
  dnl exist which will then fool the 'test -z' line.
  AC_CHECK_PROG(MKINFO, makeinfo, makeinfo, [])
  if test -z "$MKINFO"; then
    warn_makeinfo="

I didn't find makeinfo, which is required for reading documentation.
You may install a copy later for Octave to use.
"
    OCTAVE_CONFIGURE_WARNING([warn_makeinfo])
  fi
])
dnl
dnl What pager should we use?
dnl
AC_DEFUN([OCTAVE_PROG_PAGER], [
  if test "$cross_compiling" = yes; then
    DEFAULT_PAGER=less
    AC_MSG_RESULT([assuming $DEFAULT_PAGER exists on $canonical_host_type host])
    AC_SUBST(DEFAULT_PAGER)
  else
    ac_octave_possible_pagers="less more page pg"
    case "$canonical_host_type" in
      *-*-cygwin* | *-*-mingw32* | *-*-msdosmsvc)
        ac_octave_possible_pagers="$ac_octave_possible_pagers more.com"
      ;;
    esac

    AC_CHECK_PROGS(DEFAULT_PAGER, [$ac_octave_possible_pagers], [])
    if test -z "$DEFAULT_PAGER"; then
      warn_less="I couldn't find \`less', \`more', \`page', or \`pg'"
      OCTAVE_CONFIGURE_WARNING([warn_less])
    fi
  fi
])
dnl
dnl Find Perl program.
dnl
AC_DEFUN([OCTAVE_PROG_PERL], [
  AC_CHECK_PROG(PERL, perl, perl, [])
  AC_SUBST(PERL)
])
dnl
dnl Find Python program.
dnl
AC_DEFUN([OCTAVE_PROG_PYTHON], [
  AC_CHECK_PROG(PYTHON, python, python, [])
  AC_SUBST(PYTHON)
])
dnl
dnl Find rsvg-convert program.
dnl
AC_DEFUN([OCTAVE_PROG_RSVG_CONVERT], [
  AC_CHECK_PROG(RSVG_CONVERT, rsvg-convert, rsvg-convert, [])
  if test -z "$RSVG_CONVERT"; then
    RSVG_CONVERT='$(top_srcdir)/build-aux/missing rsvg-convert'
    warn_rsvg_convert="

I didn't find rsvg-convert, but it's only a problem if you need to
reconstruct octave-logo-*.png, which is the case if you're building
from VCS sources.
"
    OCTAVE_CONFIGURE_WARNING([warn_rsvg_convert])
  fi
  AC_SUBST(RSVG_CONVERT)
])
dnl
dnl Find sed program.
dnl
# Check for a fully-functional sed program, that truncates
# as few characters as possible and that supports "\(X\|Y\)"
# style regular expression alternation.  Prefer GNU sed if found.
AC_DEFUN([OCTAVE_PROG_SED], [
  AC_MSG_CHECKING([for a usable sed])
  if test -z "$SED"; then
    AC_CACHE_VAL([octave_cv_prog_sed],
      [# Loop through the user's path and search for sed and gsed.
      # Next, test potential sed programs in list for truncation.
      _AS_PATH_WALK([$PATH],
        [for ac_prog in sed gsed; do
          for ac_exec_ext in '' $ac_executable_extensions; do
            if AS_EXECUTABLE_P(["$as_dir/$ac_prog$ac_exec_ext"]); then
              _sed_list="$_sed_list $as_dir/$ac_prog$ac_exec_ext"
            fi
          done
        done
      ])
      AS_TMPDIR(sed)
      _max=0
      _count=0
      # Add /usr/xpg4/bin/sed as it is typically found on Solaris
      # along with /bin/sed that truncates output.
      for _sed in $_sed_list /usr/xpg4/bin/sed; do
        test ! -f ${_sed} && break
        cat /dev/null > "$tmp/sed.in"
        _count=0
        echo $ECHO_N "0123456789$ECHO_C" >"$tmp/sed.in"
        # Check for GNU sed and select it if it is found.
        if "${_sed}" --version 2>&1 < /dev/null | egrep '(GNU)' > /dev/null; then
          octave_cv_prog_sed=${_sed}
          break;
        fi
        # Reject if RE alternation is not handled.
        if test "`echo 'this and that' | ${_sed} -n 's/\(this\|that\).*$/\1/p'`" != "this"; then
          continue;
        fi
        while true; do
          cat "$tmp/sed.in" "$tmp/sed.in" >"$tmp/sed.tmp"
          mv "$tmp/sed.tmp" "$tmp/sed.in"
          cp "$tmp/sed.in" "$tmp/sed.nl"
          echo >>"$tmp/sed.nl"
          ${_sed} -e 's/a$//' < "$tmp/sed.nl" >"$tmp/sed.out" || break
          cmp -s "$tmp/sed.out" "$tmp/sed.nl" || break
          # 10000 chars as input seems more than enough
          test $_count -gt 10 && break
          _count=`expr $_count + 1`
          if test $_count -gt $_max; then
            _max=$_count
            octave_cv_prog_sed=$_sed
          fi
        done
      done
      rm -rf "$tmp"
    ])
    SED=$octave_cv_prog_sed
    if test -z "$SED"; then
      AC_MSG_ERROR([no usable version of sed found])
    fi
  fi
  AC_SUBST(SED)
  AC_MSG_RESULT([$SED])
])
dnl
dnl Check for texi2dvi.
dnl
AC_DEFUN([OCTAVE_PROG_TEXI2DVI], [
  AC_CHECK_PROG(TEXI2DVI, texi2dvi, texi2dvi, [])
  if test -z "$TEXI2DVI"; then
    TEXI2DVI='$(top_srcdir)/build-aux/missing texi2dvi'
    warn_texi2dvi="

I didn't find texi2dvi, but it's only a problem if you need to
reconstruct the DVI version of the manual
"
    OCTAVE_CONFIGURE_WARNING([warn_texi2dvi])
  fi
  TEXI2DVI="$TEXI2DVI"' --build-dir=t2d_cache'
  AC_SUBST(TEXI2DVI)
])
dnl
dnl Check for texi2pdf.
dnl
AC_DEFUN([OCTAVE_PROG_TEXI2PDF], [
  AC_REQUIRE([OCTAVE_PROG_TEXI2DVI])
  AC_CHECK_PROG(TEXI2PDF, texi2pdf, texi2pdf, [])
  if test -z "$TEXI2PDF"; then
    ac_octave_texi2pdf_missing=yes;
    if test -n "$TEXI2DVI"; then
      TEXI2PDF="$TEXI2DVI --pdf"
      ac_octave_texi2pdf_missing=no;
    fi
  else
    ac_octave_texi2pdf_missing=no;
  fi
  if test $ac_octave_texi2pdf_missing = yes; then
    TEXI2PDF='$(top_srcdir)/build-aux/missing texi2pdf'
    warn_texi2pdf="

I didn't find texi2pdf, but it's only a problem if you need to
reconstruct the PDF version of the manual
"
    OCTAVE_CONFIGURE_WARNING([warn_texi2pdf])
  fi
  TEXI2PDF="$TEXI2PDF"' --build-dir=t2d_cache'
  AC_SUBST(TEXI2PDF)
])
dnl
dnl Set default value for a variable and substitute it.
dnl
AC_DEFUN([OCTAVE_SET_DEFAULT], [
  ifelse($#, 2, [: ${$1=$2}
])dnl
  AC_MSG_RESULT([defining $1 to be $$1])
  AC_SUBST($1)
])
dnl
dnl Check whether SuiteSparse_time is required by UMFPACK.
dnl UMFPACK >= 4 requires linking in another library for SuiteSparse_time.
dnl
dnl Macro assumes that the check for umfpack has already been performed.
dnl
AC_DEFUN([OCTAVE_UMFPACK_NEED_SUITESPARSE_TIME], [
  AC_CACHE_CHECK([whether UMFPACK needs SuiteSparse_time function],
    [octave_cv_umfpack_need_suitesparse_time],
    [AC_LANG_PUSH(C)
    AC_LINK_IFELSE([AC_LANG_PROGRAM([[
        #if defined (HAVE_SUITESPARSE_UMFPACK_H)
        # include <suitesparse/umfpack.h>
        #elif defined (HAVE_UFSPARSE_UMFPACK_H)
        # include <ufsparse/umfpack.h>
        #elif defined (HAVE_UMFPACK_UMFPACK_H)
        # include <umfpack/umfpack.h>
        #elif defined (HAVE_UMFPACK_H)
        # include <umfpack.h>
        #endif
        ]], [[
        double stats [2];
        umfpack_tic (stats);
      ]])],
      octave_cv_umfpack_need_suitesparse_time=no,
      octave_cv_umfpack_need_suitesparse_time=yes)
    AC_LANG_POP(C)
  ])
])
dnl
dnl Check for UMFPACK separately split complex matrix and RHS.
dnl
dnl Macro assumes that the check for umfpack has already been performed.
dnl
AC_DEFUN([OCTAVE_UMFPACK_SEPARATE_SPLIT], [
  AC_MSG_CHECKING([for UMFPACK separate complex matrix and rhs split])
  AC_CACHE_VAL([octave_cv_umfpack_separate_split],
    [AC_RUN_IFELSE([AC_LANG_SOURCE([[
        #include <stdint.h>
        #include <stdlib.h>
        #include <math.h>
        #if defined (HAVE_SUITESPARSE_UMFPACK_H)
        # include <suitesparse/umfpack.h>
        #elif defined (HAVE_UFSPARSE_UMFPACK_H)
        # include <ufsparse/umfpack.h>
        #elif defined (HAVE_UMFPACK_UMFPACK_H)
        # include <umfpack/umfpack.h>
        #elif defined (HAVE_UMFPACK_H)
        # include <umfpack.h>
        #endif
        #ifdef USE_64_BIT_IDX_T
        typedef uint64_t idx_type;
        #define UMFPACK_NAME(name) umfpack_zl_ ## name
        #else
        typedef int idx_type;
        #define UMFPACK_NAME(name) umfpack_zi_ ## name
        #endif
        idx_type n = 5;
        idx_type Ap[] = {0, 2, 5, 9, 10, 12};
        idx_type Ai[]  = {0, 1, 0, 2, 4, 1, 2, 3, 4, 2, 1, 4};
        double Ax[] = {2., 0., 3., 0., 3., 0., -1., 0., 4., 0., 4., 0.,
                      -3., 0., 1., 0., 2., 0., 2., 0., 6., 0., 1., 0.};
        double br[] = {8., 45., -3., 3., 19.};
        double bi[] = {0., 0., 0., 0., 0.};
        int main (void)
        {
          double *null = (double *) NULL ;
          double *x = (double *)malloc (2 * n * sizeof(double));
          idx_type i ;
          void *Symbolic, *Numeric ;
          (void) UMFPACK_NAME (symbolic) (n, n, Ap, Ai, Ax, null, &Symbolic, null, null) ;
          (void) UMFPACK_NAME (numeric) (Ap, Ai, Ax, null, Symbolic, &Numeric, null, null) ;
          UMFPACK_NAME (free_symbolic) (&Symbolic) ;
          (void) UMFPACK_NAME (solve) (0, Ap, Ai, Ax, null, x, null, br, bi,
                                   Numeric, null, null) ;
          UMFPACK_NAME (free_numeric) (&Numeric) ;
          for (i = 0; i < n; i++, x+=2)
            if (fabs (*x - i - 1.) > 1.e-13)
              return (1);
          return (0) ;
        }
      ]])],
      octave_cv_umfpack_separate_split=yes,
      octave_cv_umfpack_separate_split=no,
      octave_cv_umfpack_separate_split=yes)
  ])
  if test "$cross_compiling" = yes; then
    AC_MSG_RESULT([$octave_cv_umfpack_separate_split assumed for cross compilation])
  else
    AC_MSG_RESULT([$octave_cv_umfpack_separate_split])
  fi
  if test $octave_cv_umfpack_separate_split = yes; then
    AC_DEFINE(UMFPACK_SEPARATE_SPLIT, 1,
      [Define to 1 if the UMFPACK Complex solver allows matrix and RHS to be split independently.])
  fi
])
dnl
dnl Check for unordered map headers and whether tr1 namespace is
dnl required.
dnl
AC_DEFUN([OCTAVE_UNORDERED_MAP_HEADERS], [
  AC_CHECK_HEADERS([unordered_map], [],
    [AC_CHECK_HEADERS([tr1/unordered_map])])
  AC_CACHE_CHECK([whether unordered_map requires tr1 namespace],
    [octave_cv_header_require_tr1_namespace],
    [AC_LANG_PUSH(C++)
    octave_cv_header_require_tr1_namespace=no
    if test $ac_cv_header_unordered_map = yes; then
      ## Have <unordered_map>, but still have to check whether
      ## tr1 namespace is required (like MSVC, for instance).
      AC_COMPILE_IFELSE(
        [AC_LANG_PROGRAM([[
          #include <unordered_map>
          ]], [[
          std::unordered_map<int,int> m;
        ]])],
        octave_cv_header_require_tr1_namespace=no,
        octave_cv_header_require_tr1_namespace=yes)
    elif test $ac_cv_header_tr1_unordered_map = yes; then
      octave_cv_header_require_tr1_namespace=yes
    fi
    AC_LANG_POP(C++)
  ])
  if test $octave_cv_header_require_tr1_namespace = yes; then
    AC_DEFINE(USE_UNORDERED_MAP_WITH_TR1, 1,
      [Define to 1 if unordered_map requires the use of tr1 namespace.])
  fi
])

dnl         End of macros written by Octave developers
dnl ------------------------------------------------------------
dnl

