# Makefile for Octave's doc/doxyhtml directory
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

doxyhtml:
	doxygen Doxyfile

EXTRA_DIST = \
  Doxyfile.in \
  Makefile.am \
  README

maintainer-clean-local:
	rm -rf `ls | $(GREP) -v Doxyfile | $(GREP) -v Makefile.am | $(GREP) -v Makefile.in | $(GREP) -v README`
