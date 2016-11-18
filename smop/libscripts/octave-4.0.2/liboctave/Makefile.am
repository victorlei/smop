# Makefile for Octave's liboctave directory
#
# Copyright (C) 1993-2015 John W. Eaton
#
# This file is part of Octave.
#
# Octave is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by the
# Free Software Foundation; either version 3 of the License, or (at
# your option) any later version.
#
# Octave is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
#
# You should have received a copy of the GNU General Public License
# along with Octave; see the file COPYING.  If not, see
# <http://www.gnu.org/licenses/>.

include $(top_srcdir)/build-aux/common.mk

## Search local directories before those specified by the user.
AM_CPPFLAGS = \
  -I$(srcdir)/array \
  -I$(srcdir)/cruft/misc \
  -Inumeric -I$(srcdir)/numeric \
  -Ioperators -I$(srcdir)/operators \
  -I$(srcdir)/system \
  -I$(srcdir)/util \
  -I$(top_builddir)/libgnu -I$(top_srcdir)/libgnu

AM_CFLAGS += $(WARN_CFLAGS)

AM_CXXFLAGS += $(WARN_CXXFLAGS)

octlib_LTLIBRARIES = liboctave.la

EXTRA_DIST =

DISTCLEANFILES =

BUILT_INCS = \
  operators/mx-ops.h \
  $(OPT_INC) \
  $(MX_OP_INC) \
  $(VX_OP_INC) \
  $(SMX_OP_INC)

BUILT_SOURCES = $(BUILT_INCS)

octinclude_HEADERS = \
  $(ARRAY_INC) \
  $(CRUFT_INC) \
  $(NUMERIC_INC) \
  $(OPERATORS_INC) \
  $(SYSTEM_INC) \
  $(UTIL_INC) \
  $(OTHER_INC) \
  $(TEMPLATE_SRC)

nodist_octinclude_HEADERS = \
  $(BUILT_INCS)

noinst_LTLIBRARIES =

## C++ files that are #included, not compiled
OTHER_INC =

## C++ files with templates that are #included, not compiled
TEMPLATE_SRC =

## A list of all files that could include tests

liboctave_la_LIBADD =

include array/module.mk
include cruft/module.mk
include numeric/module.mk
include operators/module.mk
include system/module.mk
include util/module.mk

## liboctave merely collects a bunch of compiled convenience libraries.
## It has no source code itself.
liboctave_la_SOURCES =

liboctave_la_CPPFLAGS = @OCTAVE_DLL_DEFS@ @CRUFT_DLL_DEFS@ $(AM_CPPFLAGS)

include link-deps.mk

# Dummy C++ source to force C++ linking.
nodist_EXTRA_liboctave_la_SOURCES = dummy.cc

liboctave_la_LIBADD += \
  $(top_builddir)/libgnu/libgnu.la \
  $(LIBOCTAVE_LINK_DEPS)

# Increment these as needed and according to the rules in the libtool manual:
liboctave_current = 3
liboctave_revision = 2
liboctave_age = 0

liboctave_version_info = $(liboctave_current):$(liboctave_revision):$(liboctave_age)

liboctave_la_LDFLAGS = \
  -version-info $(liboctave_version_info) \
  $(NO_UNDEFINED_LDFLAG) \
  @XTRA_CRUFT_SH_LDFLAGS@ \
  -bindir $(bindir) \
  $(LIBOCTAVE_LINK_OPTS)

## Rules to build test files

LIBOCTAVE_TST_SRC = \
  $(array_libarray_la_SOURCES) \
  $(numeric_libnumeric_la_SOURCES) \
  $(system_libsystem_la_SOURCES) \
  $(util_libutil_la_SOURCES) \
  $(TEMPLATE_SRC)

TST_FILES_SRC := $(shell $(top_srcdir)/build-aux/find-files-with-tests.sh "$(srcdir)" $(LIBOCTAVE_TST_SRC))

TST_FILES := $(addsuffix -tst,$(TST_FILES_SRC))

liboctavetestsdir := $(octtestsdir)/liboctave

nobase_liboctavetests_DATA = $(TST_FILES)

DISTCLEANFILES += \
  $(BUILT_INCS) \
  $(TST_FILES)

