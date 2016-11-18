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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "gripes.h"
#include "oct-obj.h"
#include "ov.h"
#include "ov-flt-cx-diag.h"
#include "ov-flt-re-diag.h"
#include "ov-typeinfo.h"
#include "ops.h"
#include "xdiv.h"
#include "xpow.h"

#define LINCLUDE "ov-flt-cx-diag.h"
#define RINCLUDE "ov-flt-re-diag.h"

#define LMATRIX float_complex_diag_matrix
#define RMATRIX float_diag_matrix
#define RDMATRIX LMATRIX

#define LSHORT fcdm
#define RSHORT fdm

#define DEFINEDIV
#define DEFINELDIV

#include "op-dm-template.cc"

