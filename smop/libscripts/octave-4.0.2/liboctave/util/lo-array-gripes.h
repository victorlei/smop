/*

Copyright (C) 2000-2015 John W. Eaton

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

#if !defined (octave_lo_array_gripes_h)
#define octave_lo_array_gripes_h 1

#include "dim-vector.h"

extern OCTAVE_API const char *error_id_nonconformant_args;

extern OCTAVE_API const char *error_id_index_out_of_bounds;

extern OCTAVE_API const char *error_id_invalid_index;

extern OCTAVE_API const char *warning_id_nearly_singular_matrix;

extern OCTAVE_API const char *warning_id_singular_matrix;

extern void OCTAVE_API
gripe_nan_to_logical_conversion (void);

extern void OCTAVE_API
gripe_nan_to_character_conversion (void);

extern void OCTAVE_API
gripe_nonconformant (const char *op,
                     octave_idx_type op1_len, octave_idx_type op2_len);

extern void OCTAVE_API
gripe_nonconformant (const char *op,
                     octave_idx_type op1_nr, octave_idx_type op1_nc,
                     octave_idx_type op2_nr, octave_idx_type op2_nc);


extern void OCTAVE_API
gripe_nonconformant (const char *op, const dim_vector& op1_dims,
                     const dim_vector& op2_dims);

extern void OCTAVE_API
gripe_index_out_of_range (int nd, int dim,
                          octave_idx_type iext, octave_idx_type ext);

extern void OCTAVE_API
gripe_del_index_out_of_range (bool is1d, octave_idx_type iext,
                              octave_idx_type ext);

extern void OCTAVE_API
gripe_invalid_index (void);

extern void OCTAVE_API
gripe_invalid_resize (void);

extern void OCTAVE_API
gripe_invalid_assignment_size (void);

extern void OCTAVE_API
gripe_assignment_dimension_mismatch (void);

extern void OCTAVE_API
gripe_singular_matrix (double rcond = 0.0);

#endif
