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

// FIXME: it might be nice to only include the declarations of the
// operators that are actually needed instead of including all of them.
#include "mx-ops.h"

#include "ov-perm.h"
#include MINCLUDE
#include "ops.h"
#ifdef DEFINENULLASSIGNCONV
#include "ov-null-mat.h"
#endif

#ifndef LDMATRIX
#define LDMATRIX LMATRIX
#endif

#define OCTAVE_LMATRIX CONCAT2(octave_, LMATRIX)
#define OCTAVE_LDMATRIX CONCAT2(octave_, LDMATRIX)
#define OCTAVE_RMATRIX CONCAT2(octave_, RMATRIX)
#ifdef LEFT
#define LMATRIX_VALUE perm_matrix_value
#define RMATRIX_VALUE CONCAT2(RMATRIX, _value)
#else
#define LMATRIX_VALUE CONCAT2(LMATRIX, _value)
#define RMATRIX_VALUE perm_matrix_value
#endif

DEFBINOP (mul, LMATRIX, RMATRIX)
{
  CAST_BINOP_ARGS (const OCTAVE_LMATRIX&, const OCTAVE_RMATRIX&);

  return v1.LMATRIX_VALUE () * v2.RMATRIX_VALUE ();
}

#ifdef LEFT
DEFBINOP (ldiv, LMATRIX, RMATRIX)
{
  CAST_BINOP_ARGS (const OCTAVE_LMATRIX&, const OCTAVE_RMATRIX&);

  return v1.perm_matrix_value ().inverse () * v2.RMATRIX_VALUE ();
}
#else
DEFBINOP (div, LMATRIX, RMATRIX)
{
  CAST_BINOP_ARGS (const OCTAVE_LMATRIX&, const OCTAVE_RMATRIX&);

  return v1.LMATRIX_VALUE () * v2.perm_matrix_value ().inverse ();
}
#endif


#define SHORT_NAME CONCAT3(LSHORT, _, RSHORT)
#define INST_NAME CONCAT3(install_, SHORT_NAME, _ops)

void
INST_NAME (void)
{
  INSTALL_BINOP (op_mul, OCTAVE_LMATRIX, OCTAVE_RMATRIX, mul);
#ifdef LEFT
  INSTALL_BINOP (op_ldiv, OCTAVE_LMATRIX, OCTAVE_RMATRIX, ldiv);
#else
  INSTALL_BINOP (op_div, OCTAVE_LMATRIX, OCTAVE_RMATRIX, div);
#endif
#ifdef DEFINENULLASSIGNCONV
  INSTALL_ASSIGNCONV (OCTAVE_LMATRIX, octave_null_matrix, OCTAVE_LDMATRIX);
  INSTALL_ASSIGNCONV (OCTAVE_LMATRIX, octave_null_str, OCTAVE_LDMATRIX);
  INSTALL_ASSIGNCONV (OCTAVE_LMATRIX, octave_null_sq_str, OCTAVE_LDMATRIX);
#endif
}
