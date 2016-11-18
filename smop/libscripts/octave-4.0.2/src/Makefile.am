# Makefile for Octave's src directory
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
  -I$(top_srcdir)/liboctave/array \
  -I$(top_srcdir)/liboctave/cruft/misc \
  -I$(top_srcdir)/liboctave/numeric \
  -I$(top_srcdir)/liboctave/system \
  -I$(top_srcdir)/liboctave/util \
  -I$(top_srcdir)/libinterp \
  -I$(top_builddir)/libinterp/corefcn -I$(top_srcdir)/libinterp/corefcn \
  -I$(top_builddir)/libgnu -I$(top_srcdir)/libgnu

AM_CFLAGS += $(WARN_CFLAGS)

AM_CXXFLAGS += $(WARN_CXXFLAGS)

EXTRA_DIST = \
  main.in.cc \
  mkoctfile.in.cc \
  octave-config.in.cc

DISTCLEANFILES = \
  main.cc \
  mkoctfile.cc \
  octave-config.cc

bin_PROGRAMS = \
  mkoctfile \
  octave \
  octave-cli \
  octave-config

noinst_HEADERS = \
  display-available.h	\
  shared-fcns.h

OCTAVE_VERSION_LINKS = octave-cli-$(version)$(EXEEXT)

archlib_PROGRAMS =

if AMCOND_BUILD_GUI
  archlib_PROGRAMS += octave-gui
  OCTAVE_VERSION_LINKS += octave-gui-$(version)$(EXEEXT)
endif

OCTAVE_CORE_LIBS = \
  $(top_builddir)/libinterp/liboctinterp.la \
  $(top_builddir)/liboctave/liboctave.la

include ../libgui/link-deps.mk

nodist_octave_SOURCES = main.cc
octave_SOURCES = display-available.c

octave_LDADD = \
  $(top_builddir)/libgnu/libgnu.la \
  $(X11_LIBS) \
  $(CARBON_LIBS) \
  $(GNULIB_LINK_DEPS)

octave_LDFLAGS = \
  $(NO_UNDEFINED_LDFLAG) \
  $(OCTAVE_LINK_OPTS)

if AMCOND_BUILD_GUI
  OCTAVE_CPPFLAGS = -DHAVE_OCTAVE_GUI
endif

octave_CPPFLAGS = \
  $(AM_CPPFLAGS) \
  $(OCTAVE_CPPFLAGS)

octave_cli_SOURCES = main-cli.cc

octave_cli_LDADD = \
  $(OCTAVE_CORE_LIBS) \
  $(OCTAVE_LINK_DEPS)

octave_cli_LDFLAGS = \
  $(NO_UNDEFINED_LDFLAG) \
  $(OCTAVE_LINK_OPTS)

if AMCOND_BUILD_GUI
  octave_gui_SOURCES = main-gui.cc
  OCTAVE_GUI_LIBS = $(top_builddir)/libgui/liboctgui.la
  OCTAVE_GUI_CPPFLAGS = -I$(top_srcdir)/libgui/src
endif

octave_gui_CPPFLAGS = \
  $(AM_CPPFLAGS) \
  $(OCTAVE_GUI_CPPFLAGS)

octave_gui_LDADD = \
  $(OCTAVE_GUI_LIBS) \
  $(OCTAVE_CORE_LIBS) \
  $(OCTAVE_GUI_LINK_DEPS)

octave_gui_LDFLAGS = \
  $(NO_UNDEFINED_LDFLAG) \
  $(OCTAVE_GUI_LINK_OPTS)

mkoctfile_SOURCES =
nodist_mkoctfile_SOURCES = mkoctfile.cc
mkoctfile_LDADD = $(top_builddir)/libgnu/libgnu.la $(LIBS)

octave_config_SOURCES =
nodist_octave_config_SOURCES = octave-config.cc
octave_config_LDADD = \
  $(top_builddir)/libinterp/corefcn/libcorefcn.la \
  $(top_builddir)/libgnu/libgnu.la \
  $(LIBS)

OCTAVE_CROSS_TOOLS=
if AMCOND_CROSS_TOOLS
# building cross mkoctfile
OCTAVE_CROSS_TOOLS += $(host_triplet)-mkoctfile$(BUILD_EXEEXT)
$(host_triplet)-mkoctfile$(BUILD_EXEEXT): $(host_triplet)-mkoctfile.cc
	$(BUILD_CXX) -o $(host_triplet)-mkoctfile$(BUILD_EXEEXT) -Dgnulib='' -Doctave_idx_type=int $(DEFAULT_INCLUDES) $(BUILD_CXXFLAGS) $(BUILD_LDFLAGS) $(host_triplet)-mkoctfile.cc
$(host_triplet)-mkoctfile.cc: mkoctfile.in.cc Makefile
	$(AM_V_GEN)$(do_subst_cross_config_vals)

# building cross octave-config
OCTAVE_CROSS_TOOLS += $(host_triplet)-octave-config$(BUILD_EXEEXT)
$(host_triplet)-octave-config$(BUILD_EXEEXT): $(host_triplet)-octave-config.cc
	$(BUILD_CXX) -o $(host_triplet)-octave-config$(BUILD_EXEEXT) -Dgnulib='' -Doctave_idx_type=int $(DEFAULT_INCLUDES) $(BUILD_CXXFLAGS) $(BUILD_LDFLAGS) $(host_triplet)-octave-config.cc
$(host_triplet)-octave-config.cc: octave-config.in.cc Makefile
	$(AM_V_GEN)$(do_subst_default_vals)

mostlyclean-local:
	-rm -f $(OCTAVE_CROSS_TOOLS)

endif

all-local: $(OCTAVE_VERSION_LINKS) $(OCTAVE_CROSS_TOOLS)

octave-config.cc: octave-config.in.cc Makefile
	$(AM_V_GEN)$(do_subst_default_vals)

mkoctfile.cc: mkoctfile.in.cc Makefile
	$(AM_V_GEN)$(do_subst_config_vals)

## main.cc must depend on Makefile.  Calling configure may change
## default/config values.  However, calling configure will also
## regenerate the Makefiles from Makefile.am and trigger the rules below.
main.cc: main.in.cc Makefile
	$(AM_V_GEN)$(do_subst_default_vals)

install-exec-hook: make-version-links

uninstall-local: remove-version-links

make-version-links:
	cd $(DESTDIR)$(bindir) && \
	for f in $(basename $(bin_PROGRAMS)); do \
	  mv $$f$(EXEEXT) $$f-$(version)$(EXEEXT) && \
	    $(LN_S) $$f-$(version)$(EXEEXT) $$f$(EXEEXT); \
	done

remove-version-links:
	for f in $(basename $(bin_PROGRAMS)); do \
	  rm -f $(DESTDIR)$(bindir)/$$f-$(version)$(EXEEXT); \
	done

.PHONY: make-version-links remove-version-links

## We need these file names in the build tree because the wrapper
## program (main.cc) will try to invoke the versioned binaries.

octave-cli-$(version)$(EXEEXT): octave-cli$(EXEEXT)
	$(AM_V_GEN)rm -f $@ && \
	$(LN_S) $< $@

octave-gui-$(version)$(EXEEXT): octave-gui$(EXEEXT)
	$(AM_V_GEN)rm -f $@ && \
	$(LN_S) $< $@

CLEANFILES = \
  $(OCTAVE_VERSION_LINKS)
