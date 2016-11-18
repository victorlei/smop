/*

Copyright (C) 1996-2015 John W. Eaton

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
#include "ov-bool.h"
#include "ov-bool-mat.h"
#include "ov-scalar.h"
#include "ov-float.h"
#include "ov-re-mat.h"
#include "ov-typeinfo.h"
#include "ov-null-mat.h"
#include "ops.h"
#include "xdiv.h"
#include "xpow.h"

// bool unary ops.

// scalar unary ops.

DEFUNOP_OP (not, bool, !)

UNOPDECL (uplus, a)
{
  CAST_UNOP_ARG (const octave_bool&);
  return octave_value (v.double_value ());
}

UNOPDECL (uminus, a)
{
  CAST_UNOP_ARG (const octave_bool&);
  return octave_value (- v.double_value ());
}

DEFUNOP_OP (transpose, bool, /* no-op */)
DEFUNOP_OP (hermitian, bool, /* no-op */)

// bool by bool ops.

DEFBINOP_OP (eq, bool, bool, ==)
DEFBINOP_OP (ne, bool, bool, !=)
DEFBINOP_OP (el_and, bool, bool, &&)
DEFBINOP_OP (el_or, bool, bool, ||)

DEFNDCATOP_FN (b_b, bool, bool, bool_array, bool_array, concat)
DEFNDCATOP_FN (b_s, bool, scalar, array, array, concat)
DEFNDCATOP_FN (s_b, scalar, bool, array, array, concat)
DEFNDCATOP_FN (b_f, bool, float_scalar, float_array, float_array, concat)
DEFNDCATOP_FN (f_b, float_scalar, bool, float_array, float_array, concat)

void
install_b_b_ops (void)
{
  INSTALL_UNOP (op_not, octave_bool, not);
  INSTALL_UNOP (op_uplus, octave_bool, uplus);
  INSTALL_UNOP (op_uminus, octave_bool, uminus);
  INSTALL_UNOP (op_transpose, octave_bool, transpose);
  INSTALL_UNOP (op_hermitian, octave_bool, hermitian);

  INSTALL_BINOP (op_eq, octave_bool, octave_bool, eq);
  INSTALL_BINOP (op_ne, octave_bool, octave_bool, ne);
  INSTALL_BINOP (op_el_and, octave_bool, octave_bool, el_and);
  INSTALL_BINOP (op_el_or, octave_bool, octave_bool, el_or);

  INSTALL_CATOP (octave_bool, octave_bool, b_b);
  INSTALL_CATOP (octave_bool, octave_scalar, b_s);
  INSTALL_CATOP (octave_scalar, octave_bool, s_b);
  INSTALL_CATOP (octave_bool, octave_float_scalar, b_f);
  INSTALL_CATOP (octave_float_scalar, octave_bool, f_b);

  INSTALL_ASSIGNCONV (octave_bool, octave_bool, octave_bool_matrix);

  INSTALL_ASSIGNCONV (octave_bool, octave_null_matrix, octave_bool_matrix);
  INSTALL_ASSIGNCONV (octave_bool, octave_null_str, octave_bool_matrix);
  INSTALL_ASSIGNCONV (octave_bool, octave_null_sq_str, octave_bool_matrix);
}
