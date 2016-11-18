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

#include "ops.h"
#include "xdiv.h"
#include LINCLUDE
#include RINCLUDE
#ifdef DEFINENULLASSIGNCONV
#include "ov-null-mat.h"
#endif

// matrix by diag matrix ops.

DEFBINOP_OP (add, LMATRIX, RMATRIX, +)
DEFBINOP_OP (sub, LMATRIX, RMATRIX, -)
DEFBINOP_OP (mul, LMATRIX, RMATRIX, *)

#ifndef LDMATRIX
#define LDMATRIX LMATRIX
#endif

#ifndef RDMATRIX
#define RDMATRIX RMATRIX
#endif

#define OCTAVE_LMATRIX CONCAT2(octave_, LMATRIX)
#define OCTAVE_LDMATRIX CONCAT2(octave_, LDMATRIX)
#define OCTAVE_RMATRIX CONCAT2(octave_, RMATRIX)
#define LMATRIX_VALUE CONCAT2(LMATRIX, _value)
#define RMATRIX_VALUE CONCAT2(RMATRIX, _value)
#define LDMATRIX_VALUE CONCAT2(LDMATRIX, _value)
#define RDMATRIX_VALUE CONCAT2(RDMATRIX, _value)

#ifdef DEFINEDIV
DEFBINOP (div, LMATRIX, RMATRIX)
{
  CAST_BINOP_ARGS (const OCTAVE_LMATRIX&, const OCTAVE_RMATRIX&);

  return xdiv (v1.LDMATRIX_VALUE (), v2.RMATRIX_VALUE ());
}
#endif

#ifdef DEFINELDIV
DEFBINOP (ldiv, LMATRIX, RMATRIX)
{
  CAST_BINOP_ARGS (const OCTAVE_LMATRIX&, const OCTAVE_RMATRIX&);

  return xleftdiv (v1.LMATRIX_VALUE (), v2.RDMATRIX_VALUE ());
}
#endif

#define SHORT_NAME CONCAT3(LSHORT, _, RSHORT)
#define INST_NAME CONCAT3(install_, SHORT_NAME, _ops)

void
INST_NAME (void)
{
  INSTALL_BINOP (op_add, OCTAVE_LMATRIX, OCTAVE_RMATRIX, add);
  INSTALL_BINOP (op_sub, OCTAVE_LMATRIX, OCTAVE_RMATRIX, sub);
  INSTALL_BINOP (op_mul, OCTAVE_LMATRIX, OCTAVE_RMATRIX, mul);
#ifdef DEFINEDIV
  INSTALL_BINOP (op_div, OCTAVE_LMATRIX, OCTAVE_RMATRIX, div);
#endif
#ifdef DEFINELDIV
  INSTALL_BINOP (op_ldiv, OCTAVE_LMATRIX, OCTAVE_RMATRIX, ldiv);
#endif
#ifdef DEFINENULLASSIGNCONV
  INSTALL_ASSIGNCONV (OCTAVE_LMATRIX, octave_null_matrix, OCTAVE_LDMATRIX);
  INSTALL_ASSIGNCONV (OCTAVE_LMATRIX, octave_null_str, OCTAVE_LDMATRIX);
  INSTALL_ASSIGNCONV (OCTAVE_LMATRIX, octave_null_sq_str, OCTAVE_LDMATRIX);
#endif
}
