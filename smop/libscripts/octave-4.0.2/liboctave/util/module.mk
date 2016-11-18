EXTRA_DIST += \
  util/module.mk

UTIL_INC = \
  util/action-container.h \
  util/base-list.h \
  util/byte-swap.h \
  util/caseless-str.h \
  util/cmd-edit.h \
  util/cmd-hist.h \
  util/data-conv.h \
  util/functor.h \
  util/glob-match.h \
  util/lo-array-gripes.h \
  util/lo-cutils.h \
  util/lo-ieee.h \
  util/lo-macros.h \
  util/lo-math.h \
  util/lo-traits.h \
  util/lo-utils.h \
  util/oct-alloc.h \
  util/oct-base64.h \
  util/oct-binmap.h \
  util/oct-cmplx.h \
  util/oct-glob.h \
  util/oct-inttypes.h \
  util/oct-locbuf.h \
  util/oct-md5.h \
  util/oct-mutex.h \
  util/oct-refcount.h \
  util/oct-rl-edit.h \
  util/oct-rl-hist.h \
  util/oct-shlib.h \
  util/oct-sort.h \
  util/oct-sparse.h \
  util/pathsearch.h \
  util/lo-regexp.h \
  util/singleton-cleanup.h \
  util/sparse-sort.h \
  util/sparse-util.h \
  util/statdefs.h \
  util/str-vec.h \
  util/sun-utils.h \
  util/unwind-prot.h \
  util/url-transfer.h

UTIL_C_SRC = \
  util/f2c-main.c \
  util/lo-cutils.c \
  util/oct-rl-edit.c \
  util/oct-rl-hist.c

UTIL_SRC = \
  util/cmd-edit.cc \
  util/cmd-hist.cc \
  util/data-conv.cc \
  util/glob-match.cc \
  util/lo-array-gripes.cc \
  util/lo-ieee.cc \
  util/lo-utils.cc \
  util/oct-base64.cc \
  util/oct-glob.cc \
  util/oct-inttypes.cc \
  util/oct-locbuf.cc \
  util/oct-md5.cc \
  util/oct-mutex.cc \
  util/oct-shlib.cc \
  util/pathsearch.cc \
  util/lo-regexp.cc \
  util/singleton-cleanup.cc \
  util/sparse-sort.cc \
  util/sparse-util.cc \
  util/str-vec.cc \
  util/unwind-prot.cc \
  util/url-transfer.cc \
  $(UTIL_C_SRC)

TEMPLATE_SRC += \
  util/oct-sort.cc

OTHER_INC += \
  util/kpse.cc

noinst_LTLIBRARIES += util/libutil.la

util_libutil_la_SOURCES = $(UTIL_SRC)
util_libutil_la_CPPFLAGS = \
  $(liboctave_la_CPPFLAGS) \
  $(CURL_CPPFLAGS) \
  $(PCRE_CPPFLAGS) \
  $(SPARSE_XCPPFLAGS)

liboctave_la_LIBADD += util/libutil.la
