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
#include "ov-str-mat.h"
#include "ov-typeinfo.h"
#include "ov-null-mat.h"
#include "ops.h"

// string unary ops.

DEFUNOP (transpose, char_matrix_str)
{
  CAST_UNOP_ARG (const octave_char_matrix_str&);

  if (v.ndims () > 2)
    {
      error ("transpose not defined for N-d objects");
      return octave_value ();
    }
  else
    return octave_value (v.char_matrix_value ().transpose (),
                         a.is_sq_string () ? '\'' : '"');
}

// string by string ops.

#define DEFCHARNDBINOP_FN(name, op, t1, t2, e1, e2, f)  \
  BINOPDECL (name, a1, a2) \
  { \
    dim_vector a1_dims = a1.dims (); \
    dim_vector a2_dims = a2.dims (); \
 \
    bool a1_is_scalar = a1_dims.all_ones (); \
    bool a2_is_scalar = a2_dims.all_ones (); \
 \
    CAST_BINOP_ARGS (const octave_ ## t1&, const octave_ ## t2&); \
 \
    if (a1_is_scalar) \
      { \
        if (a2_is_scalar) \
          return octave_value ((v1.e1 ## _value ())(0) op (v2.e2 ## _value ())(0)); \
        else \
          return octave_value (f ((v1.e1 ## _value ())(0), v2.e2 ## _value ())); \
      } \
    else \
      { \
        if (a2_is_scalar) \
          return octave_value (f (v1.e1 ## _value (), (v2.e2 ## _value ())(0))); \
        else \
          return octave_value (f (v1.e1 ## _value (), v2.e2 ## _value ())); \
      } \
  }

DEFCHARNDBINOP_FN (lt, <, char_matrix_str, char_matrix_str, char_array,
                   char_array, mx_el_lt)
DEFCHARNDBINOP_FN (le, <=, char_matrix_str, char_matrix_str, char_array,
                   char_array, mx_el_le)
DEFCHARNDBINOP_FN (eq, ==, char_matrix_str, char_matrix_str, char_array,
                   char_array, mx_el_eq)
DEFCHARNDBINOP_FN (ge, >=, char_matrix_str, char_matrix_str, char_array,
                   char_array, mx_el_ge)
DEFCHARNDBINOP_FN (gt, >, char_matrix_str, char_matrix_str, char_array,
                   char_array, mx_el_gt)
DEFCHARNDBINOP_FN (ne, !=, char_matrix_str, char_matrix_str, char_array,
                   char_array, mx_el_ne)

DEFASSIGNOP (assign, char_matrix_str, char_matrix_str)
{
  CAST_BINOP_ARGS (octave_char_matrix_str&, const octave_char_matrix_str&);

  v1.assign (idx, v2.char_array_value ());
  return octave_value ();
}

DEFNULLASSIGNOP_FN (null_assign, char_matrix_str, delete_elements)

DEFNDCHARCATOP_FN (str_str, char_matrix_str, char_matrix_str, concat)

void
install_str_str_ops (void)
{
  INSTALL_UNOP (op_transpose, octave_char_matrix_str, transpose);
  INSTALL_UNOP (op_transpose, octave_char_matrix_sq_str, transpose);

  INSTALL_UNOP (op_hermitian, octave_char_matrix_str, transpose);
  INSTALL_UNOP (op_hermitian, octave_char_matrix_sq_str, transpose);

  INSTALL_BINOP (op_lt, octave_char_matrix_str, octave_char_matrix_str, lt);
  INSTALL_BINOP (op_lt, octave_char_matrix_str, octave_char_matrix_sq_str, lt);
  INSTALL_BINOP (op_lt, octave_char_matrix_sq_str, octave_char_matrix_str, lt);
  INSTALL_BINOP (op_lt, octave_char_matrix_sq_str, octave_char_matrix_sq_str,
                 lt);

  INSTALL_BINOP (op_le, octave_char_matrix_str, octave_char_matrix_str, le);
  INSTALL_BINOP (op_le, octave_char_matrix_str, octave_char_matrix_sq_str, le);
  INSTALL_BINOP (op_le, octave_char_matrix_sq_str, octave_char_matrix_str, le);
  INSTALL_BINOP (op_le, octave_char_matrix_sq_str, octave_char_matrix_sq_str,
                 le);

  INSTALL_BINOP (op_eq, octave_char_matrix_str, octave_char_matrix_str, eq);
  INSTALL_BINOP (op_eq, octave_char_matrix_str, octave_char_matrix_sq_str, eq);
  INSTALL_BINOP (op_eq, octave_char_matrix_sq_str, octave_char_matrix_str, eq);
  INSTALL_BINOP (op_eq, octave_char_matrix_sq_str, octave_char_matrix_sq_str,
                 eq);

  INSTALL_BINOP (op_ge, octave_char_matrix_str, octave_char_matrix_str, ge);
  INSTALL_BINOP (op_ge, octave_char_matrix_str, octave_char_matrix_sq_str, ge);
  INSTALL_BINOP (op_ge, octave_char_matrix_sq_str, octave_char_matrix_str, ge);
  INSTALL_BINOP (op_ge, octave_char_matrix_sq_str, octave_char_matrix_sq_str,
                 ge);

  INSTALL_BINOP (op_gt, octave_char_matrix_str, octave_char_matrix_str, gt);
  INSTALL_BINOP (op_gt, octave_char_matrix_str, octave_char_matrix_sq_str, gt);
  INSTALL_BINOP (op_gt, octave_char_matrix_sq_str, octave_char_matrix_str, gt);
  INSTALL_BINOP (op_gt, octave_char_matrix_sq_str, octave_char_matrix_sq_str,
                 gt);

  INSTALL_BINOP (op_ne, octave_char_matrix_str, octave_char_matrix_str, ne);
  INSTALL_BINOP (op_ne, octave_char_matrix_str, octave_char_matrix_sq_str, ne);
  INSTALL_BINOP (op_ne, octave_char_matrix_sq_str, octave_char_matrix_str, ne);
  INSTALL_BINOP (op_ne, octave_char_matrix_sq_str, octave_char_matrix_sq_str,
                 ne);

  INSTALL_CATOP (octave_char_matrix_str, octave_char_matrix_str, str_str);
  INSTALL_CATOP (octave_char_matrix_str, octave_char_matrix_sq_str, str_str);
  INSTALL_CATOP (octave_char_matrix_sq_str, octave_char_matrix_str, str_str);
  INSTALL_CATOP (octave_char_matrix_sq_str, octave_char_matrix_sq_str, str_str);

  INSTALL_ASSIGNOP (op_asn_eq, octave_char_matrix_str, octave_char_matrix_str,
                    assign);
  INSTALL_ASSIGNOP (op_asn_eq, octave_char_matrix_str,
                    octave_char_matrix_sq_str,
                    assign);
  INSTALL_ASSIGNOP (op_asn_eq, octave_char_matrix_sq_str,
                    octave_char_matrix_str,
                    assign);
  INSTALL_ASSIGNOP (op_asn_eq, octave_char_matrix_sq_str,
                    octave_char_matrix_sq_str, assign);

  INSTALL_ASSIGNOP (op_asn_eq, octave_char_matrix_str, octave_null_matrix,
                    null_assign);
  INSTALL_ASSIGNOP (op_asn_eq, octave_char_matrix_str, octave_null_str,
                    null_assign);
  INSTALL_ASSIGNOP (op_asn_eq, octave_char_matrix_str, octave_null_sq_str,
                    null_assign);
  INSTALL_ASSIGNOP (op_asn_eq, octave_char_matrix_sq_str, octave_null_matrix,
                    null_assign);
  INSTALL_ASSIGNOP (op_asn_eq, octave_char_matrix_sq_str, octave_null_str,
                    null_assign);
  INSTALL_ASSIGNOP (op_asn_eq, octave_char_matrix_sq_str, octave_null_sq_str,
                    null_assign);

}
