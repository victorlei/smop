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
#include "ov-cx-mat.h"
#include "ov-cx-diag.h"
#include "ov-flt-cx-diag.h"
#include "ov-typeinfo.h"
#include "ops.h"
#include "xdiv.h"
#include "xpow.h"

// matrix unary ops.

DEFUNOP_OP (uplus, complex_diag_matrix, /* no-op */)
DEFUNOP_OP (uminus, complex_diag_matrix, -)

DEFUNOP (transpose, complex_diag_matrix)
{
  CAST_UNOP_ARG (const octave_complex_diag_matrix&);
  return octave_value (v.complex_diag_matrix_value ().transpose ());
}

DEFUNOP (hermitian, complex_diag_matrix)
{
  CAST_UNOP_ARG (const octave_complex_diag_matrix&);
  return octave_value (v.complex_diag_matrix_value ().hermitian ());
}

// matrix by matrix ops.

DEFBINOP_OP (add, complex_diag_matrix, complex_diag_matrix, +)
DEFBINOP_OP (sub, complex_diag_matrix, complex_diag_matrix, -)
DEFBINOP_OP (mul, complex_diag_matrix, complex_diag_matrix, *)

DEFBINOP (div, complex_diag_matrix, complex_diag_matrix)
{
  CAST_BINOP_ARGS (const octave_complex_diag_matrix&,
                   const octave_complex_diag_matrix&);

  return xdiv (v1.complex_diag_matrix_value (),
               v2.complex_diag_matrix_value ());
}

DEFBINOP (ldiv, complex_diag_matrix, complex_diag_matrix)
{
  CAST_BINOP_ARGS (const octave_complex_diag_matrix&,
                   const octave_complex_diag_matrix&);

  return xleftdiv (v1.complex_diag_matrix_value (),
                   v2.complex_diag_matrix_value ());
}

CONVDECL (complex_diag_matrix_to_complex_matrix)
{
  CAST_CONV_ARG (const octave_complex_diag_matrix&);

  return new octave_complex_matrix (v.complex_matrix_value ());
}

CONVDECL (complex_diag_matrix_to_float_complex_diag_matrix)
{
  CAST_CONV_ARG (const octave_complex_diag_matrix&);

  return
    new octave_float_complex_diag_matrix (v.float_complex_diag_matrix_value ());
}

void
install_cdm_cdm_ops (void)
{
  INSTALL_UNOP (op_uplus, octave_complex_diag_matrix, uplus);
  INSTALL_UNOP (op_uminus, octave_complex_diag_matrix, uminus);
  INSTALL_UNOP (op_transpose, octave_complex_diag_matrix, transpose);
  INSTALL_UNOP (op_hermitian, octave_complex_diag_matrix, hermitian);

  INSTALL_BINOP (op_add, octave_complex_diag_matrix, octave_complex_diag_matrix,
                 add);
  INSTALL_BINOP (op_sub, octave_complex_diag_matrix, octave_complex_diag_matrix,
                 sub);
  INSTALL_BINOP (op_mul, octave_complex_diag_matrix, octave_complex_diag_matrix,
                 mul);
  INSTALL_BINOP (op_div, octave_complex_diag_matrix, octave_complex_diag_matrix,
                 div);
  INSTALL_BINOP (op_ldiv, octave_complex_diag_matrix,
                 octave_complex_diag_matrix, ldiv);

  INSTALL_CONVOP (octave_complex_diag_matrix, octave_complex_matrix,
                  complex_diag_matrix_to_complex_matrix);
  INSTALL_CONVOP (octave_complex_diag_matrix, octave_float_complex_diag_matrix,
                  complex_diag_matrix_to_float_complex_diag_matrix);
  INSTALL_ASSIGNCONV (octave_complex_diag_matrix, octave_complex_matrix,
                      octave_complex_matrix);
  INSTALL_WIDENOP (octave_complex_diag_matrix, octave_complex_matrix,
                   complex_diag_matrix_to_complex_matrix);
}
