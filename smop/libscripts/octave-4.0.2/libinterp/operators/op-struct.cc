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
#include "ov-re-mat.h"
#include "ov-struct.h"
#include "ov-typeinfo.h"
#include "ops.h"

// struct ops.

DEFUNOP (transpose, struct)
{
  CAST_UNOP_ARG (const octave_struct&);

  if (v.ndims () > 2)
    {
      error ("transpose not defined for N-d objects");
      return octave_value ();
    }
  else
    return octave_value (v.map_value ().transpose ());
}

DEFUNOP (scalar_transpose, scalar_struct)
{
  CAST_UNOP_ARG (const octave_scalar_struct&);

  return octave_value (v.scalar_map_value ());
}

DEFNDCATOP_FN (s_s_concat, struct, struct, map, map, concat)
DEFNDCATOP_FN (s_ss_concat, struct, scalar_struct, map, map, concat)
DEFNDCATOP_FN (ss_s_concat, scalar_struct, struct, map, map, concat)
DEFNDCATOP_FN (ss_ss_concat, scalar_struct, scalar_struct, map, map, concat)

static octave_value
oct_catop_struct_matrix (octave_base_value& a1, const octave_base_value& a2,
                         const Array<octave_idx_type>&)
{
  octave_value retval;
  CAST_BINOP_ARGS (const octave_struct&, const octave_matrix&);
  NDArray tmp = v2.array_value ();
  dim_vector dv = tmp.dims ();
  if (dv.all_zero ())
    retval = octave_value (v1.map_value ());
  else
    error ("invalid concatenation of structure with matrix");
  return retval;
}

static octave_value
oct_catop_matrix_struct (octave_base_value& a1, const octave_base_value& a2,
                         const Array<octave_idx_type>&)
{
  octave_value retval;
  CAST_BINOP_ARGS (const octave_matrix&, const octave_struct&);
  NDArray tmp = v1.array_value ();
  dim_vector dv = tmp.dims ();
  if (dv.all_zero ())
    retval = octave_value (v2.map_value ());
  else
    error ("invalid concatenation of structure with matrix");
  return retval;
}

void
install_struct_ops (void)
{
  INSTALL_UNOP (op_transpose, octave_struct, transpose);
  INSTALL_UNOP (op_hermitian, octave_struct, transpose);

  INSTALL_UNOP (op_transpose, octave_scalar_struct, scalar_transpose);
  INSTALL_UNOP (op_hermitian, octave_scalar_struct, scalar_transpose);

  INSTALL_CATOP (octave_struct, octave_struct, s_s_concat);
  INSTALL_CATOP (octave_struct, octave_scalar_struct, s_ss_concat)
  INSTALL_CATOP (octave_scalar_struct, octave_struct, ss_s_concat)
  INSTALL_CATOP (octave_scalar_struct, octave_scalar_struct, ss_ss_concat)

  INSTALL_CATOP (octave_struct, octave_matrix, struct_matrix);
  INSTALL_CATOP (octave_matrix, octave_struct, matrix_struct);
}
