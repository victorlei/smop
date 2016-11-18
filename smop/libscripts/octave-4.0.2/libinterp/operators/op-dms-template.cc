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

#include "ops.h"
#include "gripes.h"
#include "xpow.h"
#include SINCLUDE
#include MINCLUDE

// matrix by diag matrix ops.

#ifndef SCALARV
#define SCALARV SCALAR
#endif

#ifndef MATRIXV
#define MATRIXV MATRIX
#endif

DEFNDBINOP_OP (sdmmul, SCALAR, MATRIX, SCALARV, MATRIXV, *)
DEFNDBINOP_OP (dmsmul, MATRIX, SCALAR, MATRIXV, SCALARV, *)

#define OCTAVE_MATRIX CONCAT2(octave_, MATRIX)
#define OCTAVE_SCALAR CONCAT2(octave_, SCALAR)
#define MATRIX_VALUE CONCAT2(MATRIXV, _value)
#define SCALAR_VALUE CONCAT2(SCALARV, _value)

template <class T>
static T
gripe_if_zero (T x)
{
  if (x == T ())
    gripe_divide_by_zero ();
  return x;
}

DEFBINOP (dmsdiv, MATRIX, SCALAR)
{
  CAST_BINOP_ARGS (const OCTAVE_MATRIX&, const OCTAVE_SCALAR&);

  return v1.MATRIX_VALUE () / gripe_if_zero (v2.SCALAR_VALUE ());
}

DEFBINOP (sdmldiv, SCALAR, MATRIX)
{
  CAST_BINOP_ARGS (const OCTAVE_SCALAR&, const OCTAVE_MATRIX&);

  return v2.MATRIX_VALUE () / gripe_if_zero (v1.SCALAR_VALUE ());
}

DEFBINOP (dmspow, MATRIX, SCALAR)
{
  CAST_BINOP_ARGS (const OCTAVE_MATRIX&, const OCTAVE_SCALAR&);

  return xpow (v1.MATRIX_VALUE (), v2.SCALAR_VALUE ());
}

#define SHORT_NAME CONCAT3(MSHORT, _, SSHORT)
#define INST_NAME CONCAT3(install_, SHORT_NAME, _ops)

void
INST_NAME (void)
{
  INSTALL_BINOP (op_mul, OCTAVE_MATRIX, OCTAVE_SCALAR, dmsmul);
  INSTALL_BINOP (op_div, OCTAVE_MATRIX, OCTAVE_SCALAR, dmsdiv);
  INSTALL_BINOP (op_mul, OCTAVE_SCALAR, OCTAVE_MATRIX, sdmmul);
  INSTALL_BINOP (op_ldiv, OCTAVE_SCALAR, OCTAVE_MATRIX, sdmldiv);
  INSTALL_BINOP (op_pow, OCTAVE_MATRIX, OCTAVE_SCALAR, dmspow);
}
