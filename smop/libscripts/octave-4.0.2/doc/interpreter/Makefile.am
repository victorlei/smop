# Makefile for Octave's doc/interpreter directory
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

## Leading PATH_SEPARATOR required due to weak parsing by dvips (12/04/09)
TEXINPUTS := "$(PATH_SEPARATOR)$(srcdir)$(PATH_SEPARATOR)$(TEXINPUTS)$(PATH_SEPARATOR)"
export TEXINPUTS

## Include custom texmf.cnf necessary to run @seealso macro
TEXMFCNF := "..$(PATH_SEPARATOR)$(srcdir)/..$(PATH_SEPARATOR)$(TEXMFCNF)$(PATH_SEPARATOR)"
export TEXMFCNF

dist_man_MANS = \
  mkoctfile.1 \
  octave-cli.1 \
  octave-config.1 \
  octave.1

## The following files are included in the manual via the @EXAMPLEFILE macro.
## They are dependencies for the documentation.
## They must not be distributed from this directory.
EXAMPLE_FILES = \
  $(top_srcdir)/examples/code/@FIRfilter/FIRfilter.m \
  $(top_srcdir)/examples/code/@FIRfilter/FIRfilter_aggregation.m \
  $(top_srcdir)/examples/code/@FIRfilter/display.m \
  $(top_srcdir)/examples/code/@FIRfilter/subsasgn.m \
  $(top_srcdir)/examples/code/@FIRfilter/subsref.m \
  $(top_srcdir)/examples/code/@polynomial/display.m \
  $(top_srcdir)/examples/code/@polynomial/double.m \
  $(top_srcdir)/examples/code/@polynomial/end.m \
  $(top_srcdir)/examples/code/@polynomial/get.m \
  $(top_srcdir)/examples/code/@polynomial/mtimes.m \
  $(top_srcdir)/examples/code/@polynomial/plot.m \
  $(top_srcdir)/examples/code/@polynomial/polynomial.m \
  $(top_srcdir)/examples/code/@polynomial/polynomial_superiorto.m \
  $(top_srcdir)/examples/code/@polynomial/polyval.m \
  $(top_srcdir)/examples/code/@polynomial/set.m \
  $(top_srcdir)/examples/code/@polynomial/subsref.m \
  $(top_srcdir)/examples/code/addtwomatrices.cc \
  $(top_srcdir)/examples/code/celldemo.cc \
  $(top_srcdir)/examples/code/embedded.cc \
  $(top_srcdir)/examples/code/fortrandemo.cc \
  $(top_srcdir)/examples/code/fortransub.f \
  $(top_srcdir)/examples/code/funcdemo.cc \
  $(top_srcdir)/examples/code/globaldemo.cc \
  $(top_srcdir)/examples/code/helloworld.cc \
  $(top_srcdir)/examples/code/mycell.c \
  $(top_srcdir)/examples/code/myfeval.c \
  $(top_srcdir)/examples/code/myfunc.c \
  $(top_srcdir)/examples/code/myhello.c \
  $(top_srcdir)/examples/code/mypow2.c \
  $(top_srcdir)/examples/code/mysparse.c \
  $(top_srcdir)/examples/code/mystring.c \
  $(top_srcdir)/examples/code/mystruct.c \
  $(top_srcdir)/examples/code/paramdemo.cc \
  $(top_srcdir)/examples/code/standalone.cc \
  $(top_srcdir)/examples/code/stringdemo.cc \
  $(top_srcdir)/examples/code/structdemo.cc \
  $(top_srcdir)/examples/code/unwinddemo.cc


include images.mk

$(srcdir)/images.mk: $(srcdir)/config-images.sh $(srcdir)/images.awk $(srcdir)/images
	$(srcdir)/config-images.sh $(top_srcdir)

JAVA_IMAGES = \
  java-images/image001.png \
  java-images/image002.png \
  java-images/image003.png \
  java-images/image004.png \
  java-images/image005.png \
  java-images/image006.png \
  java-images/image007.png \
  java-images/image008.png \
  java-images/image009.png

BUILT_IMAGES = \
  $(IMAGES_EPS) \
  $(IMAGES_PDF) \
  $(IMAGES_PNG) \
  $(IMAGES_TXT)

IMAGES = \
  $(BUILT_IMAGES) \
  $(JAVA_IMAGES)

## FIXME: JAVA_IMAGES will eventually need to be added to the HTML build.
##        It will require a different Makefile rule later because
##        JAVA_IMAGES live in a subdir rather than in the current directory.
HTMLDIR_IMAGES = $(addprefix octave.html/, $(IMAGES_PNG))

LOGOS = \
  octave_logo.eps \
  octave_logo.pdf

MUNGED_TEXI_SRC = \
  arith.texi \
  audio.texi \
  basics.texi \
  bugs.texi \
  container.texi \
  contrib.texi \
  cp-idx.texi \
  data.texi \
  debug.texi \
  diffeq.texi \
  diagperm.texi \
  external.texi \
  emacs.texi \
  errors.texi \
  eval.texi \
  expr.texi \
  fn-idx.texi \
  func.texi \
  geometry.texi \
  gui.texi \
  gpl.texi \
  grammar.texi \
  image.texi \
  install.texi \
  interp.texi \
  intro.texi \
  io.texi \
  java.texi \
  linalg.texi \
  matrix.texi \
  nonlin.texi \
  numbers.texi \
  obsolete.texi \
  oop.texi \
  op-idx.texi \
  optim.texi \
  package.texi \
  plot.texi \
  poly.texi \
  preface.texi \
  quad.texi \
  set.texi \
  signal.texi \
  sparse.texi \
  stats.texi \
  stmt.texi \
  strings.texi \
  system.texi \
  testfun.texi \
  tips.texi \
  var.texi \
  vectorize.texi

TXI_SRC = $(MUNGED_TEXI_SRC:.texi=.txi)

include graphics_properties.mk

BUILT_TEXINFOS = \
  contributors.texi \
  $(GRAPH_PROP_TEXI_SRC) \
  $(MUNGED_TEXI_SRC)

info_TEXINFOS = octave.texi

octave_TEXINFOS = \
  $(BUILT_TEXINFOS) \
  $(EXAMPLE_FILES) \
  $(IMAGES) \
  $(LOGOS) \
  $(srcdir)/images.mk

all-local: dvi html pdf ps doc-cache

html: $(HTMLDIR_IMAGES)

$(HTMLDIR_IMAGES): $(IMAGES_PNG) | octave.html
	cp $(@F) octave.html

## The texi2dvi script (used to create both PDF and DVI output formats)
## uses some fixed temporary file names.  In order to avoid a race condition
## the DVI and PDF builds are forced to run serially through a Makefile rule.
octave.pdf: octave.dvi

# Prevent packaging of distribution unless all libraries
# necessary to create documentation are present
dist-hook:
	@$(GREP) '#define HAVE_COLAMD 1' $(top_builddir)/config.h > /dev/null || { echo "Documentation creation requires missing COLAMD library.  Cannot package distribution!" ; exit 1; }
	@$(GREP) '#define HAVE_CHOLMOD 1' $(top_builddir)/config.h > /dev/null || { echo "Documentation creation requires missing CHOLMOD library.  Cannot package distribution!" ; exit 1; }
	@$(GREP) '#define HAVE_UMFPACK 1' $(top_builddir)/config.h > /dev/null || { echo "Documentation creation requires missing UMFPACK library.  Cannot package distribution!" ; exit 1; }
	@$(GREP) '#define HAVE_QHULL 1' $(top_builddir)/config.h > /dev/null || { echo "Documentation creation requires missing QHULL library.  Cannot package distribution!" ; exit 1; }

octetc_DATA = doc-cache macros.texi

DOCSTRING_FILES = $(shell $(srcdir)/find-docstring-files.sh "$(top_srcdir)")

doc-cache: $(DOCSTRING_FILES) mk_doc_cache.m
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(top_builddir)/run-octave -f -q -H $(srcdir)/mk_doc_cache.m - $(srcdir)/macros.texi $(DOCSTRING_FILES) >$@-t && \
	mv $@-t $@

$(MUNGED_TEXI_SRC): $(DOCSTRING_FILES) $(munge_texi_SOURCES)

%.texi: %.txi munge-texi.pl
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(PERL) $(srcdir)/munge-texi.pl $(top_srcdir) $(DOCSTRING_FILES) < $< > $@-t && \
	mv $@-t $@

contributors.texi: contributors.in
	$(AM_V_GEN)rm -f $@-t $@ && \
	$(AWK) -f $(srcdir)/mkcontrib.awk $(srcdir)/contributors.in > $@-t && \
	mv $@-t $@

../../AUTHORS: preface.texi contributors.texi
	$(AM_V_MAKEINFO)rm -f AUTHORS && \
	if [ "x$(srcdir)" != "x." ] && [ -f $(srcdir)/contributors.texi ] && [ ! -f contributors.texi ]; then \
		cp $(srcdir)/contributors.texi contributors.texi; \
		touch -r $(srcdir)/contributors.texi contributors.texi; \
	fi && \
	$(MAKEINFO) -D AUTHORSONLY -I $(srcdir) \
	  --no-validate --no-headers --no-split --output AUTHORS $< && \
	mv AUTHORS ../../AUTHORS

../../BUGS: bugs.texi
	$(AM_V_MAKEINFO)rm -f BUGS && \
	$(MAKEINFO) -D BUGSONLY -I $(srcdir) \
	  --no-validate --no-headers --no-split --output BUGS $< && \
	mv BUGS ../../BUGS

../../INSTALL.OCTAVE: install.texi
	$(AM_V_MAKEINFO)rm -f INSTALL && \
	$(MAKEINFO) -D INSTALLONLY -I $(srcdir) \
	  --no-validate --no-headers --no-split --output INSTALL $< && \
	mv INSTALL ../../INSTALL.OCTAVE


undocumented_list:
	rm -f $@
	-$(PERL) $(srcdir)/doccheck/mk_undocumented_list > $@
.PHONY: undocumented_list

SPELLCHECK_FILES = $(MUNGED_TEXI_SRC:.texi=.scheck)

%.scheck: %.texi
	$(srcdir)/doccheck/spellcheck $< > $@-t
	mv $@-t $@
	[ -s $@ ] || rm -f $@

spellcheck: $(SPELLCHECK_FILES)
	@if ls *.scheck >/dev/null 2>&1 ; then \
		echo "Spellcheck failed"; \
		echo "Review the following files:"; \
		ls *.scheck ; \
		exit 1 ; \
	else \
		echo "Spellcheck passed"; \
	fi
.PHONY: spellcheck

EXTRA_DIST = \
  config-images.sh \
  contributors.in \
  doc-cache \
  find-docstring-files.sh \
  genpropdoc.m \
  graphics_properties.mk \
  images \
  images.awk \
  images.mk \
  macros.texi \
  mk_doc_cache.m \
  mkcontrib.awk \
  munge-texi.pl \
  octave.dvi \
  octave.html \
  octave.pdf \
  octave.ps \
  $(IMAGES) \
  $(IMAGES_SRC) \
  $(LOGOS) \
  $(TXI_SRC)

clean-local:
	rm -rf t2d_cache

DISTCLEANFILES = $(BUILT_TEXINFOS)

MAINTAINERCLEANFILES = $(BUILT_IMAGES) doc-cache

