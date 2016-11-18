# Makefile for Octave
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

include build-aux/common.mk

## Avoid making multiple subdirs in parallel which can lead
## to a confusing error message stream
.NOTPARALLEL:

ACLOCAL_AMFLAGS = -I m4

BUILT_DISTFILES = AUTHORS BUGS ChangeLog INSTALL.OCTAVE

EXTRA_DIST = \
  AUTHORS \
  BUGS \
  COPYING \
  ChangeLog \
  INSTALL \
  INSTALL.OCTAVE \
  NEWS \
  README \
  CITATION \
  build-aux/find-files-with-tests.sh \
  build-aux/mk-opts.pl \
  build-aux/move-if-change \
  build-aux/OctJavaQry.class \
  build-aux/stl_algo.h-fixed \
  etc/NEWS.1 \
  etc/NEWS.2 \
  etc/NEWS.3 \
  etc/OLD-ChangeLogs/ChangeLog \
  etc/OLD-ChangeLogs/ChangeLog.1 \
  etc/OLD-ChangeLogs/doc-ChangeLog \
  etc/OLD-ChangeLogs/libcruft-ChangeLog \
  etc/OLD-ChangeLogs/liboctave-ChangeLog \
  etc/OLD-ChangeLogs/scripts-ChangeLog \
  etc/OLD-ChangeLogs/src-ChangeLog \
  etc/OLD-ChangeLogs/test-ChangeLog \
  etc/PROJECTS \
  etc/README.Cygwin \
  etc/README.Linux \
  etc/README.MacOS \
  etc/README.MinGW \
  etc/README.Windows \
  etc/README.gnuplot \
  etc/README.kpathsea \
  etc/gdbinit \
  run-octave.in

include m4/module.mk

# Subdirectories in which to run `make all'.  Including "." before
# @DOCDIR@ is an attempt to force all preceding directories in the list
# to be processed before the current directory so that the
# scripts/DOCSTRINGS libinterp/DOCSTRINGS files are built before
# attempting to build AUTHORS and BUGS.  Including "." again at the end
# of the list ensures that we display the "Octave sucessfully built..."
# message at the very end of the output from Make.  Another fix for
# these problems would be to continue eliminating the recursive make
# invocations so that we have better control over the dependencies and
# the order that things are built.
SUBDIRS = libgnu liboctave libinterp
if AMCOND_BUILD_GUI
SUBDIRS += libgui
endif
SUBDIRS += src scripts . @DOCDIR@ examples etc/icons test .

if ! AMCOND_BUILD_DOCS
dist-hook:
	echo "Documentation disabled.  Cannot package distribution!" ; exit 1;
endif

BUILT_SOURCES = run-octave

if AMCOND_HAVE_BROKEN_STL_ALGO_H
  BUILT_SOURCES += bits/stl_algo.h
else
  BUILT_SOURCES += nonexistent-file
endif

noinst_SCRIPTS = run-octave

INFO_FILES = \
  AUTHORS \
  BUGS \
  INSTALL.OCTAVE

CLEANFILES = $(BUILT_SOURCES)

DISTCLEANFILES = \
  .gdbinit \
  ChangeLog

MAINTAINERCLEANFILES = $(BUILT_DISTFILES)

CONFIG_FILES = @ac_config_headers@ @ac_config_files@

nodist_octinclude_HEADERS = config.h
octinclude_HEADERS = oct-conf-post.h

all-local: $(noinst_SCRIPTS) $(INFO_FILES) .gdbinit
	@echo ""
	@echo "Octave successfully built.  Now choose from the following:"
	@echo ""
	@echo "   ./run-octave    - to run in place to test before installing"
	@echo "   make check      - to run the tests"
	@echo "   make install    - to install (PREFIX=$(prefix))"
	@echo ""

check: all
	$(MAKE) -C test check

if AMCOND_HAVE_LLVM
check-jit: all
	$(MAKE) -C test check-jit
endif

run-octave: run-octave.in Makefile
	$(AM_V_GEN)$(do_subst_script_vals) && \
	chmod a+rx "$@"

bits/stl_algo.h: build-aux/stl_algo.h-fixed
	$(AM_V_GEN)$(MKDIR_P) bits && \
	$(INSTALL_HEADER) $< $@

## If we aren't trying to fix stl_algo.h, then try to ensure that
## there isn't a stray copy sitting in the build tree.

nonexistent-file:
	rm -f bits/stl_algo.h
.PHONY: nonexistent-file

.gdbinit: etc/gdbinit
	@$(gdbinit_install_rule)

if AMCOND_BUILD_DOCS
AUTHORS BUGS INSTALL.OCTAVE:
	$(AM_V_GEN)$(MAKE) -C doc/interpreter ../../$@
endif
.PHONY: AUTHORS BUGS INSTALL.OCTAVE

define changelog-from-hg-log
  rm -f $@-t $@ && \
  ( cd $(srcdir); \
    hg log --style=build-aux/changelog.tmpl --prune=b0e60ad4ae26 --only-branch=`hg branch`; \
    echo ""; \
    echo "See the files in the directory etc/OLD-ChangeLogs for changes before 2011-04-19"; \
  ) > $@-t && \
  mv $@-t $@
endef

ChangeLog:
	$(AM_V_GEN)$(changelog-from-hg-log)
.PHONY: ChangeLog

octetc_DATA = \
  CITATION \
  NEWS

if AMCOND_INSTALL_BUILD_LOGS
octetc_DATA += config.log
endif

doxyhtml:
	$(MAKE) -C doc/doxyhtml doxyhtml
.PHONY: doxyhtml

DIRS_TO_MAKE = \
  $(localfcnfiledir) \
  $(localapifcnfiledir) \
  $(localverfcnfiledir) \
  $(localoctfiledir) \
  $(localapioctfiledir) \
  $(localveroctfiledir) \
  $(localarchlibdir) \
  $(localapiarchlibdir) \
  $(localverarchlibdir)

installdirs-local:
	$(MKDIR_P) $(addprefix $(DESTDIR), $(DIRS_TO_MAKE))

install-data-local: installdirs-local
