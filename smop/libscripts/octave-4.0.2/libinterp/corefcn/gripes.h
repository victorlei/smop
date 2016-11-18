/*

Copyright (C) 1993-2015 John W. Eaton

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

#if !defined (octave_gripes_h)
#define octave_gripes_h 1

#include <string>

#include "lo-array-gripes.h"

class octave_value;

extern OCTINTERP_API void
gripe_not_supported (const char *);

extern OCTINTERP_API void
gripe_not_implemented (const char *);

extern OCTINTERP_API void
gripe_string_invalid (void);

extern OCTINTERP_API void
gripe_range_invalid (void);

extern OCTINTERP_API void
gripe_nonconformant (void);

extern OCTINTERP_API void
gripe_nonconformant (octave_idx_type r1, octave_idx_type c1,
                     octave_idx_type r2, octave_idx_type c2);

extern OCTINTERP_API void
gripe_empty_arg (const char *name, bool is_error);

extern OCTINTERP_API void
gripe_square_matrix_required (const char *name);

extern OCTINTERP_API void
gripe_user_supplied_eval (const char *name);

extern OCTINTERP_API void
gripe_user_returned_invalid (const char *name);

extern OCTINTERP_API void
gripe_invalid_conversion (const std::string& from, const std::string& to);

extern OCTINTERP_API void
gripe_invalid_value_specified (const char *name);

extern OCTINTERP_API void
gripe_2_or_3_dim_plot (void);

extern OCTINTERP_API void
gripe_unrecognized_float_fmt (void);

extern OCTINTERP_API void
gripe_unrecognized_data_fmt (const char *warn_for);

extern OCTINTERP_API void
gripe_data_conversion (const char *from, const char *to);

extern OCTINTERP_API void
gripe_wrong_type_arg (const char *name, const char *s,
                      bool is_error = true);

extern OCTINTERP_API void
gripe_wrong_type_arg (const char *name, const std::string& s,
                      bool is_error = true);

extern OCTINTERP_API void
gripe_wrong_type_arg (const char *name, const octave_value& tc,
                      bool is_error = true);

extern OCTINTERP_API void
gripe_wrong_type_arg (const std::string& name, const octave_value& tc,
                      bool is_error = true);

extern OCTINTERP_API void
gripe_wrong_type_arg_for_unary_op (const octave_value& op);

extern OCTINTERP_API void
gripe_wrong_type_arg_for_binary_op (const octave_value& op);

extern OCTINTERP_API void
gripe_implicit_conversion (const char *id, const char *from, const char *to);

extern OCTINTERP_API void
gripe_implicit_conversion (const std::string& id, const std::string& from,
                           const std::string& to);

extern OCTINTERP_API void
gripe_divide_by_zero (void);

extern OCTINTERP_API void
gripe_logical_conversion (void);

extern OCTINTERP_API void
gripe_library_execution_error (void);

extern OCTINTERP_API void
gripe_invalid_inquiry_subscript (void);

extern OCTINTERP_API void
gripe_indexed_cs_list (void);

extern OCTINTERP_API void
gripe_nonbraced_cs_list_assignment (void);

extern OCTINTERP_API void
gripe_warn_complex_cmp (void);

extern OCTINTERP_API void
gripe_disabled_feature (const std::string& func, const std::string& feature,
                        const std::string& pkg="Octave");

extern OCTINTERP_API void
gripe_data_file_in_path (const std::string& fcn, const std::string& file);

#endif
