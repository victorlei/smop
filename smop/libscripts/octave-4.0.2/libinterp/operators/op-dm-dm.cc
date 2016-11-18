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
#include "ov-re-mat.h"
#include "ov-re-diag.h"
#include "ov-flt-re-diag.h"
#include "ov-typeinfo.h"
#include "ops.h"
#include "xdiv.h"
#include "xpow.h"

// matrix unary ops.

DEFUNOP_OP (uplus, diag_matrix, /* no-op */)
DEFUNOP_OP (uminus, diag_matrix, -)

DEFUNOP (transpose, diag_matrix)
{
  CAST_UNOP_ARG (const octave_diag_matrix&);
  return octave_value (v.diag_matrix_value ().transpose ());
}

// matrix by matrix ops.

DEFBINOP_OP (add, diag_matrix, diag_matrix, +)
DEFBINOP_OP (sub, diag_matrix, diag_matrix, -)
DEFBINOP_OP (mul, diag_matrix, diag_matrix, *)

DEFBINOP (div, diag_matrix, diag_matrix)
{
  CAST_BINOP_ARGS (const octave_diag_matrix&, const octave_diag_matrix&);

  return xdiv (v1.diag_matrix_value (),
               v2.diag_matrix_value ());
}

DEFBINOP (ldiv, diag_matrix, diag_matrix)
{
  CAST_BINOP_ARGS (const octave_diag_matrix&, const octave_diag_matrix&);

  return xleftdiv (v1.diag_matrix_value (),
                   v2.diag_matrix_value ());
}

CONVDECL (diag_matrix_to_matrix)
{
  CAST_CONV_ARG (const octave_diag_matrix&);

  return new octave_matrix (v.matrix_value ());
}

CONVDECL (diag_matrix_to_float_diag_matrix)
{
  CAST_CONV_ARG (const octave_diag_matrix&);

  return new octave_float_diag_matrix (v.float_diag_matrix_value ());
}

void
install_dm_dm_ops (void)
{
  INSTALL_UNOP (op_uplus, octave_diag_matrix, uplus);
  INSTALL_UNOP (op_uminus, octave_diag_matrix, uminus);
  INSTALL_UNOP (op_transpose, octave_diag_matrix, transpose);
  INSTALL_UNOP (op_hermitian, octave_diag_matrix, transpose);

  INSTALL_BINOP (op_add, octave_diag_matrix, octave_diag_matrix, add);
  INSTALL_BINOP (op_sub, octave_diag_matrix, octave_diag_matrix, sub);
  INSTALL_BINOP (op_mul, octave_diag_matrix, octave_diag_matrix, mul);
  INSTALL_BINOP (op_div, octave_diag_matrix, octave_diag_matrix, div);
  INSTALL_BINOP (op_ldiv, octave_diag_matrix, octave_diag_matrix, ldiv);

  INSTALL_CONVOP (octave_diag_matrix, octave_matrix, diag_matrix_to_matrix);
  INSTALL_CONVOP (octave_diag_matrix, octave_float_diag_matrix,
                  diag_matrix_to_float_diag_matrix);
  INSTALL_ASSIGNCONV (octave_diag_matrix, octave_matrix, octave_matrix);
  INSTALL_WIDENOP (octave_diag_matrix, octave_matrix, diag_matrix_to_matrix);
}
