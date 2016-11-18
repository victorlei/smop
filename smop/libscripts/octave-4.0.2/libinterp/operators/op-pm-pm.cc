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
#include "ov-perm.h"
#include "ov-re-mat.h"
#include "ov-scalar.h"
#include "ov-typeinfo.h"
#include "ops.h"
#include "xpow.h"

DEFUNOP (transpose, perm_matrix)
{
  CAST_UNOP_ARG (const octave_perm_matrix&);
  return octave_value (v.perm_matrix_value ().transpose ());
}

DEFBINOP_OP (mul, perm_matrix, perm_matrix, *)

DEFBINOP (div, perm_matrix, perm_matrix)
{
  CAST_BINOP_ARGS (const octave_perm_matrix&, const octave_perm_matrix&);

  return (v1.perm_matrix_value () * v2.perm_matrix_value ().inverse ());
}

DEFBINOP (ldiv, perm_matrix, perm_matrix)
{
  CAST_BINOP_ARGS (const octave_perm_matrix&, const octave_perm_matrix&);

  return (v1.perm_matrix_value ().inverse () * v2.perm_matrix_value ());
}

DEFBINOP (pow, perm_matrix, scalar)
{
  CAST_BINOP_ARGS (const octave_perm_matrix&, const octave_scalar&);

  return xpow (v1.perm_matrix_value (), v2.scalar_value ());
}

CONVDECL (perm_matrix_to_matrix)
{
  CAST_CONV_ARG (const octave_perm_matrix&);

  return new octave_matrix (v.matrix_value ());
}

void
install_pm_pm_ops (void)
{
  INSTALL_UNOP (op_transpose, octave_perm_matrix, transpose);
  INSTALL_UNOP (op_hermitian, octave_perm_matrix, transpose);

  INSTALL_BINOP (op_mul, octave_perm_matrix, octave_perm_matrix, mul);
  INSTALL_BINOP (op_div, octave_perm_matrix, octave_perm_matrix, div);
  INSTALL_BINOP (op_ldiv, octave_perm_matrix, octave_perm_matrix, ldiv);
  INSTALL_BINOP (op_pow, octave_perm_matrix, octave_scalar, pow);

  INSTALL_CONVOP (octave_perm_matrix, octave_matrix, perm_matrix_to_matrix);
  INSTALL_ASSIGNCONV (octave_perm_matrix, octave_matrix, octave_matrix);
  INSTALL_WIDENOP (octave_perm_matrix, octave_matrix, perm_matrix_to_matrix);
}
