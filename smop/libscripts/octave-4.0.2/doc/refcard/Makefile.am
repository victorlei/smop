# Makefile for Octave's doc/refcard directory
#
# Copyright (C) 1996-2015 John W. Eaton
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

DVIPS = dvips
TEX = tex
PDFTEX = pdftex

AM_V_TEX = $(am__v_TEX_$(V))
am__v_TEX_ = $(am__v_TEX_$(AM_DEFAULT_VERBOSITY))
am__v_TEX_0 = @echo "  TEX     " $@;
am__v_TEX_1 = 

AM_V_PDFTEX = $(am__v_PDFTEX_$(V))
am__v_PDFTEX_ = $(am__v_PDFTEX_$(AM_DEFAULT_VERBOSITY))
am__v_PDFTEX_0 = @echo "  PDFTEX  " $@;
am__v_PDFTEX_1 = 

## The following three blocks of variable definitions can be eliminated
## if we merge the makefiles in teh doc directory.

AM_V_texidevnull = $(am__v_texidevnull_$(V))
am__v_texidevnull_ = $(am__v_texidevnull_$(AM_DEFAULT_VERBOSITY))
am__v_texidevnull_0 = > /dev/null
am__v_texidevnull_1 = 

AM_V_DVIPS = $(am__v_DVIPS_$(V))
am__v_DVIPS_ = $(am__v_DVIPS_$(AM_DEFAULT_VERBOSITY))
am__v_DVIPS_0 = @echo "  DVIPS   " $@;
am__v_DVIPS_1 =

AM_V_texinfo = $(am__v_texinfo_$(V))
am__v_texinfo_ = $(am__v_texinfo_$(AM_DEFAULT_VERBOSITY))
am__v_texinfo_0 = -q
am__v_texinfo_1 = 

%.dvi : %.tex
	-$(AM_V_TEX)TEXINPUTS="$(srcdir):$(TEXINPUTS):" \
	$(TEX) $< $(AM_V_texidevnull)

%.pdf : %.tex
	-$(AM_V_PDFTEX)TEXINPUTS="$(srcdir):$(TEXINPUTS):" \
	$(PDFTEX) $< $(AM_V_texidevnull)

TEX_SRC = \
  refcard.tex \
  refcard-a4.tex \
  refcard-legal.tex \
  refcard-letter.tex

FORMATTED = \
  refcard-a4.dvi \
  refcard-a4.ps \
  refcard-a4.pdf \
  refcard-legal.dvi \
  refcard-legal.ps \
  refcard-legal.pdf \
  refcard-letter.dvi \
  refcard-letter.ps \
  refcard-letter.pdf

all-local: $(FORMATTED)

refcard-a4.pdf: refcard-a4.tex refcard.tex
refcard-a4.dvi: refcard-a4.tex refcard.tex
refcard-a4.ps: refcard-a4.dvi
	-$(AM_V_DVIPS)$(DVIPS) $(AM_V_texinfo) -T 297mm,210mm -o $@ $<

refcard-legal.pdf: refcard-legal.tex refcard.tex
refcard-legal.dvi: refcard-legal.tex refcard.tex
refcard-legal.ps: refcard-legal.dvi
	-$(AM_V_DVIPS)$(DVIPS) $(AM_V_texinfo) -T 14in,8.5in -o $@ $<

refcard-letter.pdf: refcard-letter.tex refcard.tex
refcard-letter.dvi: refcard-letter.tex refcard.tex
refcard-letter.ps: refcard-letter.dvi
	-$(AM_V_DVIPS)$(DVIPS) $(AM_V_texinfo) -T 11in,8.5in -o $@ $<

EXTRA_DIST = \
  $(TEX_SRC) \
  $(FORMATTED)

CLEANFILES = refcard-a4.log refcard-legal.log refcard-letter.log


MAINTAINERCLEANFILES = $(FORMATTED)

