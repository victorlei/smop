# Makefile for Octave's test directory
#
# Copyright (C) 1994-2015 John W. Eaton
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

FCN_FILES = \
  fntests.m \
  args.tst \
  bug-31371.tst \
  bug-38576.tst \
  colormaps.tst \
  complex.tst \
  diag-perm.tst \
  error.tst \
  eval-catch.tst \
  for.tst \
  func.tst \
  global.tst \
  if.tst \
  index.tst \
  io.tst \
  jit.tst \
  line-continue.tst \
  logical-index.tst \
  null-assign.tst \
  parser.tst \
  prefer.tst \
  range.tst \
  recursion.tst \
  return.tst \
  slice.tst \
  struct.tst \
  switch.tst \
  system.tst \
  transpose.tst \
  try.tst \
  unwind.tst \
  while.tst

include bug-35448/module.mk
include bug-36025/module.mk
include bug-38236/module.mk
include bug-38691/module.mk
include classdef/module.mk
include classes/module.mk
include class-concat/module.mk
include ctor-vs-method/module.mk
include fcn-handle-derived-resolution/module.mk
include nest/module.mk

all-local: .gdbinit

.gdbinit: ../etc/gdbinit
	@$(gdbinit_install_rule)

check: sparse.tst bc-overload-tests.stamp
	$(top_builddir)/run-octave $(RUN_OCTAVE_OPTIONS) --norc --silent --no-history $(srcdir)/fntests.m $(srcdir)

if AMCOND_HAVE_LLVM
check-jit: sparse.tst bc-overload-tests.stamp
	$(top_builddir)/run-octave $(RUN_OCTAVE_OPTIONS) --jit-compiler --norc --silent --no-history $(srcdir)/fntests.m $(srcdir)
endif

sparse.tst: build-sparse-tests.sh
	$(AM_V_GEN)$(srcdir)/build-sparse-tests.sh

GENERATED_BC_OVERLOADS_DIRS := \
  $(shell $(srcdir)/build-bc-overload-tests.sh --list-dirs)

GENERATED_BC_OVERLOADS_FILES := \
  $(shell $(srcdir)/build-bc-overload-tests.sh --list-files)

bc-overload-tests.stamp: build-bc-overload-tests.sh bc-overloads-expected
	$(srcdir)/build-bc-overload-tests.sh $(srcdir)/bc-overloads-expected
	touch $@

BUILT_SOURCES = bc-overload-tests.stamp

EXTRA_DIST = \
  build-sparse-tests.sh \
  build-bc-overload-tests.sh \
  bc-overloads-expected \
  build_bc_overloads_expected.m \
  $(FCN_FILES)

CLEANFILES = \
  sparse.tst \
  bc-overload-tests.stamp \
  $(GENERATED_BC_OVERLOADS_FILES)

DISTCLEANFILES = \
  .gdbinit \
  fntests.log

fixedtestsdir := $(octtestsdir)/fixed

nobase_fixedtests_DATA = \
  sparse.tst \
  $(GENERATED_BC_OVERLOADS_FILES) \
  $(filter-out fntests.m, $(FCN_FILES))

clean-local:
	-rm -rf $(GENERATED_BC_OVERLOADS_DIRS)
