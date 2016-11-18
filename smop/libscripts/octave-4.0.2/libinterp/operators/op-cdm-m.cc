/*

Copyright (C) 2008-2015 Jaroslav Hajek

This file is part of Octave.

Octave is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

Octave is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Octave; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.

*/

#define LINCLUDE "ov-cx-diag.h"
#define RINCLUDE "ov-re-mat.h"

#define LMATRIX complex_diag_matrix
#define LDMATRIX complex_matrix
#define RMATRIX matrix
#define RDMATRIX complex_matrix

#define LSHORT cdm
#define RSHORT m

#define DEFINELDIV
#define DEFINENULLASSIGNCONV

#include "op-dm-template.cc"

