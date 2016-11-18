/*

Copyright (C) 2003-2015 John W. Eaton

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
#include "ov-scalar.h"
#include "ov-str-mat.h"
#include "ov-typeinfo.h"
#include "ops.h"

DEFASSIGNOP (assign, char_matrix_str, octave_scalar)
{
  CAST_BINOP_ARGS (octave_char_matrix_str&, const octave_scalar&);

  octave_value tmp
    = v2.convert_to_str_internal (false, false,
                                  a1.is_sq_string () ? '\'' : '"');

  if (! error_state)
    v1.assign (idx, tmp.char_array_value ());

  return octave_value ();
}

DEFNDCHARCATOP_FN (str_s, char_matrix_str, scalar, concat)

DEFNDCHARCATOP_FN (s_str, scalar, char_matrix_str, concat)

void
install_str_s_ops (void)
{
  INSTALL_ASSIGNOP (op_asn_eq, octave_char_matrix_str, octave_scalar, assign);
  INSTALL_ASSIGNOP (op_asn_eq, octave_char_matrix_sq_str, octave_scalar,
                    assign);

  INSTALL_CATOP (octave_char_matrix_str, octave_scalar, str_s);
  INSTALL_CATOP (octave_char_matrix_sq_str, octave_scalar, str_s);

  INSTALL_CATOP (octave_scalar, octave_char_matrix_str, s_str);
  INSTALL_CATOP (octave_scalar, octave_char_matrix_sq_str, s_str);
}
