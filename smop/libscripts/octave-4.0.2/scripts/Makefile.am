# Makefile for Octave's scripts directory
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

EXTRA_DIST =

CLEANFILES =

DISTCLEANFILES =

FCN_FILE_DIRS =

FCN_FILES =

GEN_FCN_FILES =

PKG_ADD_FILES =

DIRSTAMP_FILES =

IMAGES =

JAR_FILES =

## Read subdirs to set all variables above
include @ftp/module.mk
include audio/module.mk
include deprecated/module.mk
include elfun/module.mk
include general/module.mk
include geometry/module.mk
include gui/module.mk
include help/module.mk
include image/module.mk
include io/module.mk
include java/module.mk
include linear-algebra/module.mk
include miscellaneous/module.mk
include optimization/module.mk
include path/module.mk
include pkg/module.mk
include plot/appearance/module.mk
include plot/draw/module.mk
include plot/util/module.mk
include polynomial/module.mk
include prefs/module.mk
include set/module.mk
include signal/module.mk
include sparse/module.mk
include specfun/module.mk
include special-matrix/module.mk
include startup/module.mk
include statistics/base/module.mk
include statistics/distributions/module.mk
include statistics/models/module.mk
include statistics/tests/module.mk
include strings/module.mk
include testfun/module.mk
include time/module.mk

nobase_fcnfile_DATA = $(FCN_FILES) $(GEN_FCN_FILES) $(JAR_FILES)

image_DATA = $(IMAGES)

FCN_FILES_IN = $(GEN_FCN_FILES:.m=.in)

if AMCOND_BUILD_DOCS
all-local: $(GEN_FCN_FILES) $(PKG_ADD_FILES) $(JAR_FILES) .DOCSTRINGS
else
all-local: $(GEN_FCN_FILES) $(PKG_ADD_FILES) $(JAR_FILES)
endif

octave_dirstamp = $(am__leading_dot)dirstamp

@ftp/PKG_ADD: $(@ftp_FCN_FILES) $(@ftp_GEN_FCN_FILES) @ftp/$(octave_dirstamp) mk-pkg-add
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(srcdir)/mk-pkg-add $(srcdir) $(@ftp_FCN_FILES) -- $(@ftp_GEN_FCN_FILES) > $@-t && \
	mv $@-t $@

audio/PKG_ADD: $(audio_FCN_FILES) $(audio_GEN_FCN_FILES) audio/$(octave_dirstamp) mk-pkg-add
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(srcdir)/mk-pkg-add $(srcdir) $(audio_FCN_FILES) -- $(audio_GEN_FCN_FILES) > $@-t && \
	mv $@-t $@

deprecated/PKG_ADD: $(deprecated_FCN_FILES) $(deprecated_GEN_FCN_FILES) deprecated/$(octave_dirstamp) mk-pkg-add
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(srcdir)/mk-pkg-add $(srcdir) $(deprecated_FCN_FILES) -- $(deprecated_GEN_FCN_FILES) > $@-t && \
	mv $@-t $@

elfun/PKG_ADD: $(elfun_FCN_FILES) $(elfun_GEN_FCN_FILES) elfun/$(octave_dirstamp) mk-pkg-add
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(srcdir)/mk-pkg-add $(srcdir) $(elfun_FCN_FILES) -- $(elfun_GEN_FCN_FILES) > $@-t && \
	mv $@-t $@

general/PKG_ADD: $(general_FCN_FILES) $(general_GEN_FCN_FILES) general/$(octave_dirstamp) mk-pkg-add
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(srcdir)/mk-pkg-add $(srcdir) $(general_FCN_FILES) -- $(general_GEN_FCN_FILES) > $@-t && \
	mv $@-t $@

geometry/PKG_ADD: $(geometry_FCN_FILES) $(geometry_GEN_FCN_FILES) geometry/$(octave_dirstamp) mk-pkg-add
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(srcdir)/mk-pkg-add $(srcdir) $(geometry_FCN_FILES) -- $(geometry_GEN_FCN_FILES) > $@-t && \
	mv $@-t $@

gui/PKG_ADD: $(gui_FCN_FILES) $(gui_GEN_FCN_FILES) gui/$(octave_dirstamp) mk-pkg-add
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(srcdir)/mk-pkg-add $(srcdir) $(gui_FCN_FILES) -- $(gui_GEN_FCN_FILES) > $@-t && \
	mv $@-t $@

help/PKG_ADD: $(help_FCN_FILES) $(help_GEN_FCN_FILES) help/$(octave_dirstamp) mk-pkg-add
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(srcdir)/mk-pkg-add $(srcdir) $(help_FCN_FILES) -- $(help_GEN_FCN_FILES) > $@-t && \
	mv $@-t $@

image/PKG_ADD: $(image_FCN_FILES) $(image_GEN_FCN_FILES) image/$(octave_dirstamp) mk-pkg-add
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(srcdir)/mk-pkg-add $(srcdir) $(image_FCN_FILES) -- $(image_GEN_FCN_FILES) > $@-t && \
	mv $@-t $@

io/PKG_ADD: $(io_FCN_FILES) $(io_GEN_FCN_FILES) io/$(octave_dirstamp) mk-pkg-add
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(srcdir)/mk-pkg-add $(srcdir) $(io_FCN_FILES) -- $(io_GEN_FCN_FILES) > $@-t && \
	mv $@-t $@

java/PKG_ADD: $(java_FCN_FILES) $(java_GEN_FCN_FILES) java/$(octave_dirstamp) mk-pkg-add
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(srcdir)/mk-pkg-add $(srcdir) $(java_FCN_FILES) -- $(java_GEN_FCN_FILES) > $@-t && \
	mv $@-t $@

linear-algebra/PKG_ADD: $(linear_algebra_FCN_FILES) $(linear_algebra_GEN_FCN_FILES) linear-algebra/$(octave_dirstamp) mk-pkg-add
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(srcdir)/mk-pkg-add $(srcdir) $(linear_algebra_FCN_FILES) -- $(linear_algebra_GEN_FCN_FILES) > $@-t && \
	mv $@-t $@

miscellaneous/PKG_ADD: $(miscellaneous_FCN_FILES) $(miscellaneous_GEN_FCN_FILES) miscellaneous/$(octave_dirstamp) mk-pkg-add
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(srcdir)/mk-pkg-add $(srcdir) $(miscellaneous_FCN_FILES) -- $(miscellaneous_GEN_FCN_FILES) > $@-t && \
	mv $@-t $@

optimization/PKG_ADD: $(optimization_FCN_FILES) $(optimization_GEN_FCN_FILES) optimization/$(octave_dirstamp) mk-pkg-add
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(srcdir)/mk-pkg-add "$(srcdir)" $(optimization_FCN_FILES) -- $(optimization_GEN_FCN_FILES) > $@-t && \
	mv $@-t $@

path/PKG_ADD: $(path_FCN_FILES) $(path_GEN_FCN_FILES) path/$(octave_dirstamp) mk-pkg-add
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(srcdir)/mk-pkg-add $(srcdir) $(path_FCN_FILES) -- $(path_GEN_FCN_FILES) > $@-t && \
	mv $@-t $@

pkg/PKG_ADD: $(pkg_FCN_FILES) $(pkg_GEN_FCN_FILES) pkg/$(octave_dirstamp) mk-pkg-add
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(srcdir)/mk-pkg-add $(srcdir) $(pkg_FCN_FILES) -- $(pkg_GEN_FCN_FILES) > $@-t && \
	mv $@-t $@

plot/appearance/PKG_ADD: $(plot_appearance_FCN_FILES) $(plot_appearance_GEN_FCN_FILES) plot/appearance/$(octave_dirstamp) mk-pkg-add
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(srcdir)/mk-pkg-add $(srcdir) $(plot_appearance_FCN_FILES) -- $(plot_appearance_GEN_FCN_FILES) > $@-t && \
	mv $@-t $@

plot/draw/PKG_ADD: $(plot_draw_FCN_FILES) $(plot_draw_GEN_FCN_FILES) plot/draw/$(octave_dirstamp) mk-pkg-add
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(srcdir)/mk-pkg-add $(srcdir) $(plot_draw_FCN_FILES) -- $(plot_draw_GEN_FCN_FILES) > $@-t && \
	mv $@-t $@

plot/util/PKG_ADD: $(plot_util_FCN_FILES) $(plot_util_GEN_FCN_FILES) plot/util/$(octave_dirstamp) mk-pkg-add
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(srcdir)/mk-pkg-add $(srcdir) $(plot_util_FCN_FILES) -- $(plot_util_GEN_FCN_FILES) > $@-t && \
	mv $@-t $@

polynomial/PKG_ADD: $(polynomial_FCN_FILES) $(polynomial_GEN_FCN_FILES) polynomial/$(octave_dirstamp) mk-pkg-add
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(srcdir)/mk-pkg-add $(srcdir) $(polynomial_FCN_FILES) -- $(polynomial_GEN_FCN_FILES) > $@-t && \
	mv $@-t $@

prefs/PKG_ADD: $(prefs_FCN_FILES) $(prefs_GEN_FCN_FILES) prefs/$(octave_dirstamp) mk-pkg-add
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(srcdir)/mk-pkg-add $(srcdir) $(prefs_FCN_FILES) -- $(prefs_GEN_FCN_FILES) > $@-t && \
	mv $@-t $@

set/PKG_ADD: $(set_FCN_FILES) $(set_GEN_FCN_FILES) set/$(octave_dirstamp) mk-pkg-add
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(srcdir)/mk-pkg-add $(srcdir) $(set_FCN_FILES) -- $(set_GEN_FCN_FILES) > $@-t && \
	mv $@-t $@

signal/PKG_ADD: $(signal_FCN_FILES) $(signal_GEN_FCN_FILES) signal/$(octave_dirstamp) mk-pkg-add
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(srcdir)/mk-pkg-add $(srcdir) $(signal_FCN_FILES) -- $(signal_GEN_FCN_FILES) > $@-t && \
	mv $@-t $@

sparse/PKG_ADD: $(sparse_FCN_FILES) $(sparse_GEN_FCN_FILES) sparse/$(octave_dirstamp) mk-pkg-add
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(srcdir)/mk-pkg-add $(srcdir) $(sparse_FCN_FILES) -- $(sparse_GEN_FCN_FILES) > $@-t && \
	mv $@-t $@

specfun/PKG_ADD: $(specfun_FCN_FILES) $(specfun_GEN_FCN_FILES) specfun/$(octave_dirstamp) mk-pkg-add
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(srcdir)/mk-pkg-add $(srcdir) $(specfun_FCN_FILES) -- $(specfun_GEN_FCN_FILES) > $@-t && \
	mv $@-t $@

special-matrix/PKG_ADD: $(special_matrix_FCN_FILES) $(special_matrix_GEN_FCN_FILES) special-matrix/$(octave_dirstamp) mk-pkg-add
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(srcdir)/mk-pkg-add $(srcdir) $(special_matrix_FCN_FILES) -- $(special_matrix_GEN_FCN_FILES) > $@-t && \
	mv $@-t $@

startup/PKG_ADD: $(startup_FCN_FILES) $(startup_GEN_FCN_FILES) startup/$(octave_dirstamp) mk-pkg-add
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(srcdir)/mk-pkg-add $(srcdir) $(startup_FCN_FILES) -- $(startup_GEN_FCN_FILES) > $@-t && \
	mv $@-t $@

statistics/base/PKG_ADD: $(statistics_base_FCN_FILES) $(statistics_base_GEN_FCN_FILES) statistics/base/$(octave_dirstamp) mk-pkg-add
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(srcdir)/mk-pkg-add $(srcdir) $(statistics_base_FCN_FILES) -- $(statistics_base_GEN_FCN_FILES) > $@-t && \
	mv $@-t $@

statistics/distributions/PKG_ADD: $(statistics_distributions_FCN_FILES) $(statistics_distributions_GEN_FCN_FILES) statistics/distributions/$(octave_dirstamp) mk-pkg-add
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(srcdir)/mk-pkg-add $(srcdir) $(statistics_distributions_FCN_FILES) -- $(statistics_distributions_GEN_FCN_FILES) > $@-t && \
	mv $@-t $@

statistics/models/PKG_ADD: $(statistics_models_FCN_FILES) $(statistics_models_GEN_FCN_FILES) statistics/models/$(octave_dirstamp) mk-pkg-add
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(srcdir)/mk-pkg-add $(srcdir) $(statistics_models_FCN_FILES) -- $(statistics_models_GEN_FCN_FILES) > $@-t && \
	mv $@-t $@

statistics/tests/PKG_ADD: $(statistics_tests_FCN_FILES) $(statistics_tests_GEN_FCN_FILES) statistics/tests/$(octave_dirstamp) mk-pkg-add
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(srcdir)/mk-pkg-add $(srcdir) $(statistics_tests_FCN_FILES) -- $(statistics_tests_GEN_FCN_FILES) > $@-t && \
	mv $@-t $@

strings/PKG_ADD: $(strings_FCN_FILES) $(strings_GEN_FCN_FILES) strings/$(octave_dirstamp) mk-pkg-add
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(srcdir)/mk-pkg-add $(srcdir) $(strings_FCN_FILES) -- $(strings_GEN_FCN_FILES) > $@-t && \
	mv $@-t $@

testfun/PKG_ADD: $(testfun_FCN_FILES) $(testfun_GEN_FCN_FILES) testfun/$(octave_dirstamp) mk-pkg-add
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(srcdir)/mk-pkg-add $(srcdir) $(testfun_FCN_FILES) -- $(testfun_GEN_FCN_FILES) > $@-t && \
	mv $@-t $@

time/PKG_ADD: $(time_FCN_FILES) $(time_GEN_FCN_FILES) time/$(octave_dirstamp) mk-pkg-add
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(srcdir)/mk-pkg-add $(srcdir) $(time_FCN_FILES) -- $(time_GEN_FCN_FILES) > $@-t && \
	mv $@-t $@

ui/PKG_ADD: $(ui_FCN_FILES) $(ui_GEN_FCN_FILES) ui/$(octave_dirstamp) mk-pkg-add
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(srcdir)/mk-pkg-add $(srcdir) $(ui_FCN_FILES) -- $(ui_GEN_FCN_FILES) > $@-t && \
	mv $@-t $@

$(@ftp_GEN_FCN_FILES): @ftp/$(octave_dirstamp)
$(audio_GEN_FCN_FILES): audio/$(octave_dirstamp)
$(deprecated_GEN_FCN_FILES): deprecated/$(octave_dirstamp)
$(elfun_GEN_FCN_FILES): elfun/$(octave_dirstamp)
$(general_GEN_FCN_FILES): general/$(octave_dirstamp)
$(geometry_GEN_FCN_FILES): geometry/$(octave_dirstamp)
$(gui_GEN_FCN_FILES): gui/$(octave_dirstamp)
$(help_GEN_FCN_FILES): help/$(octave_dirstamp)
$(image_GEN_FCN_FILES): image/$(octave_dirstamp)
$(io_GEN_FCN_FILES): io/$(octave_dirstamp)
$(java_GEN_FCN_FILES): java/$(octave_dirstamp)
$(linear_algebra_GEN_FCN_FILES): linear-algebra/$(octave_dirstamp)
$(miscellaneous_GEN_FCN_FILES): miscellaneous/$(octave_dirstamp)
$(optimization_GEN_FCN_FILES): optimization/$(octave_dirstamp)
$(path_GEN_FCN_FILES): path/$(octave_dirstamp)
$(pkg_GEN_FCN_FILES): pkg/$(octave_dirstamp)
$(plot_appearance_GEN_FCN_FILES): plot/appearance/$(octave_dirstamp)
$(plot_draw_GEN_FCN_FILES): plot/draw/$(octave_dirstamp)
$(plot_util_GEN_FCN_FILES): plot/util/$(octave_dirstamp)
$(polynomial_GEN_FCN_FILES): polynomial/$(octave_dirstamp)
$(prefs_GEN_FCN_FILES): prefs/$(octave_dirstamp)
$(set_GEN_FCN_FILES): set/$(octave_dirstamp)
$(signal_GEN_FCN_FILES): signal/$(octave_dirstamp)
$(sparse_GEN_FCN_FILES): sparse/$(octave_dirstamp)
$(specfun_GEN_FCN_FILES): specfun/$(octave_dirstamp)
$(special_matrix_GEN_FCN_FILES): special-matrix/$(octave_dirstamp)
$(startup_GEN_FCN_FILES): startup/$(octave_dirstamp)
$(statistics_base_GEN_FCN_FILES): statistics/base/$(octave_dirstamp)
$(statistics_distributions_GEN_FCN_FILES): statistics/distributions/$(octave_dirstamp)
$(statistics_models_GEN_FCN_FILES): statistics/models/$(octave_dirstamp)
$(statistics_tests_GEN_FCN_FILES): statistics/tests/$(octave_dirstamp)
$(strings_GEN_FCN_FILES): strings/$(octave_dirstamp)
$(testfun_GEN_FCN_FILES): testfun/$(octave_dirstamp)
$(time_GEN_FCN_FILES): time/$(octave_dirstamp)
$(ui_GEN_FCN_FILES): ui/$(octave_dirstamp)

@ftp/$(octave_dirstamp):
	$(AM_V_GEN)$(MKDIR_P) @ftp && \
	: > @ftp/$(octave_dirstamp)
audio/$(octave_dirstamp):
	$(AM_V_GEN)$(MKDIR_P) audio && \
	: > audio/$(octave_dirstamp)
deprecated/$(octave_dirstamp):
	$(AM_V_GEN)$(MKDIR_P) deprecated && \
	: > deprecated/$(octave_dirstamp)
elfun/$(octave_dirstamp):
	$(AM_V_GEN)$(MKDIR_P) elfun && \
	: > elfun/$(octave_dirstamp)
general/$(octave_dirstamp):
	$(AM_V_GEN)$(MKDIR_P) general && \
	: > general/$(octave_dirstamp)
geometry/$(octave_dirstamp):
	$(AM_V_GEN)$(MKDIR_P) geometry && \
	: > geometry/$(octave_dirstamp)
gui/$(octave_dirstamp):
	$(AM_V_GEN)$(MKDIR_P) gui && \
	: > gui/$(octave_dirstamp)
help/$(octave_dirstamp):
	$(AM_V_GEN)$(MKDIR_P) help && \
	: > help/$(octave_dirstamp)
image/$(octave_dirstamp):
	$(AM_V_GEN)$(MKDIR_P) image && \
	: > image/$(octave_dirstamp)
io/$(octave_dirstamp):
	$(AM_V_GEN)$(MKDIR_P) io && \
	: > io/$(octave_dirstamp)
java/$(octave_dirstamp):
	$(AM_V_GEN)$(MKDIR_P) java && \
	: > java/$(octave_dirstamp)
linear-algebra/$(octave_dirstamp):
	$(AM_V_GEN)$(MKDIR_P) linear-algebra && \
	: > linear-algebra/$(octave_dirstamp)
miscellaneous/$(octave_dirstamp):
	$(AM_V_GEN)$(MKDIR_P) miscellaneous && \
	: > miscellaneous/$(octave_dirstamp)
optimization/$(octave_dirstamp):
	$(AM_V_GEN)$(MKDIR_P) optimization && \
	: > optimization/$(octave_dirstamp)
path/$(octave_dirstamp):
	$(AM_V_GEN)$(MKDIR_P) path && \
	: > path/$(octave_dirstamp)
pkg/$(octave_dirstamp):
	$(AM_V_GEN)$(MKDIR_P) pkg && \
	: > pkg/$(octave_dirstamp)
plot/appearance/$(octave_dirstamp):
	$(AM_V_GEN)$(MKDIR_P) plot/appearance && \
	: > plot/appearance/$(octave_dirstamp)
plot/draw/$(octave_dirstamp):
	$(AM_V_GEN)$(MKDIR_P) plot/draw && \
	: > plot/draw/$(octave_dirstamp)
plot/util/$(octave_dirstamp):
	$(AM_V_GEN)$(MKDIR_P) plot/util && \
	: > plot/util/$(octave_dirstamp)
polynomial/$(octave_dirstamp):
	$(AM_V_GEN)$(MKDIR_P) polynomial && \
	: > polynomial/$(octave_dirstamp)
prefs/$(octave_dirstamp):
	$(AM_V_GEN)$(MKDIR_P) prefs && \
	: > prefs/$(octave_dirstamp)
set/$(octave_dirstamp):
	$(AM_V_GEN)$(MKDIR_P) set && \
	: > set/$(octave_dirstamp)
signal/$(octave_dirstamp):
	$(AM_V_GEN)$(MKDIR_P) signal && \
	: > signal/$(octave_dirstamp)
sparse/$(octave_dirstamp):
	$(AM_V_GEN)$(MKDIR_P) sparse && \
	: > sparse/$(octave_dirstamp)
specfun/$(octave_dirstamp):
	$(AM_V_GEN)$(MKDIR_P) specfun && \
	: > specfun/$(octave_dirstamp)
special-matrix/$(octave_dirstamp):
	$(AM_V_GEN)$(MKDIR_P) special-matrix && \
	: > special-matrix/$(octave_dirstamp)
startup/$(octave_dirstamp):
	$(AM_V_GEN)$(MKDIR_P) startup && \
	: > startup/$(octave_dirstamp)
statistics/base/$(octave_dirstamp):
	$(AM_V_GEN)$(MKDIR_P) statistics/base && \
	: > statistics/base/$(octave_dirstamp)
statistics/distributions/$(octave_dirstamp):
	$(AM_V_GEN)$(MKDIR_P) statistics/distributions && \
	: > statistics/distributions/$(octave_dirstamp)
statistics/models/$(octave_dirstamp):
	$(AM_V_GEN)$(MKDIR_P) statistics/models && \
	: > statistics/models/$(octave_dirstamp)
statistics/tests/$(octave_dirstamp):
	$(AM_V_GEN)$(MKDIR_P) statistics/tests && \
	: > statistics/tests/$(octave_dirstamp)
strings/$(octave_dirstamp):
	$(AM_V_GEN)$(MKDIR_P) strings && \
	: > strings/$(octave_dirstamp)
testfun/$(octave_dirstamp):
	$(AM_V_GEN)$(MKDIR_P) testfun && \
	: > testfun/$(octave_dirstamp)
time/$(octave_dirstamp):
	$(AM_V_GEN)$(MKDIR_P) time && \
	: > time/$(octave_dirstamp)
ui/$(octave_dirstamp):
	$(AM_V_GEN)$(MKDIR_P) ui && \
	: > ui/$(octave_dirstamp)

if AMCOND_BUILD_DOCS

.DOCSTRINGS: $(FCN_FILES) $(GEN_FCN_FILES) mkdoc.pl Makefile
	$(AM_V_GEN)rm -f $@-t $@ && \
	if [ "x$(srcdir)" != "x." ] && [ -f $(srcdir)/DOCSTRINGS ] && [ ! -f DOCSTRINGS ]; then \
		cp $(srcdir)/DOCSTRINGS DOCSTRINGS; \
		touch -r $(srcdir)/DOCSTRINGS DOCSTRINGS; \
	fi && \
	$(PERL) $(srcdir)/mkdoc.pl "$(srcdir)" $(FCN_FILES) -- $(GEN_FCN_FILES) > $@-t && \
	mv $@-t $@ && \
	$(top_srcdir)/build-aux/move-if-change $@ DOCSTRINGS && \
	touch $@

endif

$(GEN_FCN_FILES) : %.m : %.in Makefile
	$(AM_V_GEN)$(do_subst_config_vals)

check-m-sources:
	@echo "checking whether files in source tree are listed in module.mk files..."; \
	for f in $$(find $(srcdir) -name '*.m'); do \
	  found=false; \
	  for m in $(FCN_FILES) $(GEN_FCN_FILES); do \
	    if [ "$$f" = $(srcdir)/"$$m" ]; then \
	      found=true; \
	      break; \
	    fi; \
	  done; \
	  if $$found; then \
	    true; \
	  else \
	    missing=$$(echo $$f | $(SED) "s|^$(srcdir)||"); \
	    echo "$$missing: not listed in SOURCES"; \
	  fi; \
	done; \
	if test -z "$$missing"; then \
	  echo "yes"; \
	fi
.PHONY: check-m-sources

check-missing-semicolon:
	@echo "checking for missing semicolons in .m files..."
	( echo "warning on Octave:missing-semicolon;"; \
	  for m in $(addprefix $(srcdir)/, $(FCN_FILES)) $(GEN_FCN_FILES); do \
	    echo "source ('$$m');"; \
	  done ) | ../run-octave -qf
.PHONY: check-missing-semicolon

## Add rule to generate ctags.
## Automake would normally generate such a rule, but only if there is a
## xxx_SOURCES target
ctags:
	ctags $(addprefix $(srcdir)/, $(FCN_FILES)) $(GEN_FCN_FILES)

install-data-local: install-startup-files install-pkg-add

uninstall-local: uninstall-startup-files uninstall-pkg-add

install-startup-files:
	$(MKDIR_P) $(DESTDIR)$(fcnfiledir)/startup
	if test -f $(DESTDIR)$(fcnfiledir)/startup/octaverc; then true; \
	else \
	  $(INSTALL_DATA) $(srcdir)/$(SYSTEM_STARTUP_FILE_SRC) \
	    $(DESTDIR)$(fcnfiledir)/startup/octaverc; \
	fi
	if test -f $(DESTDIR)$(fcnfiledir)/startup/inputrc; then true; \
	else \
	  $(INSTALL_DATA) $(srcdir)/$(SYSTEM_INPUTRC_FILE_SRC) \
	    $(DESTDIR)$(fcnfiledir)/startup/inputrc; \
	fi
	$(MKDIR_P) $(DESTDIR)$(localfcnfiledir)/startup
	if test -f $(DESTDIR)$(localfcnfiledir)/startup/octaverc; \
	then true; \
	else \
	  $(INSTALL_DATA) $(srcdir)/$(LOCAL_STARTUP_FILE_SRC) \
	    $(DESTDIR)$(localfcnfiledir)/startup/octaverc; \
	fi
.PHONY: install-startup-files

uninstall-startup-files:
	rm -f $(DESTDIR)$(fcnfiledir)/startup/octaverc
	rm -f $(DESTDIR)$(fcnfiledir)/startup/inputrc
	rm -f $(DESTDIR)$(localfcnfiledir)/startup/octaverc
.PHONY: uninstall-startup-files

install-pkg-add:
	for f in $(PKG_ADD_FILES); do \
	  if [ -n "`cat $$f`" ]; then \
	    $(MKDIR_P) $(DESTDIR)$(fcnfiledir)/`echo $$f | $(SED) 's,/[^/]*$$,,'`; \
	    $(INSTALL_DATA) $$f $(DESTDIR)$(fcnfiledir)/$$f; \
	  fi \
	done
.PHONY: install-pkg-add

uninstall-pkg-add:
	for f in $(PKG_ADD_FILES); do \
	  rm -f $(DESTDIR)$(fcnfiledir)/$$f; \
	done
.PHONY: uninstall-pkg-add

if AMCOND_HAVE_JAVA
else
dist-hook:
	@echo "Packaging distribution requires Java." ; exit 1;
endif

EXTRA_DIST += \
  $(IMAGES) \
  $(FCN_FILES) \
  $(FCN_FILES_IN) \
  $(GEN_FCN_FILES) \
  DOCSTRINGS \
  mkdoc.pl \
  mk-pkg-add

DISTCLEANFILES += \
  .DOCSTRINGS \
  DOCSTRINGS \
  $(PKG_ADD_FILES) \
  $(DIRSTAMP_FILES) \
  $(GEN_FCN_FILES)

distclean-local:
	if [ "x$(srcdir)" != "x." ]; then \
	  rm -f $(java_JAVA_IMAGES); \
	fi
