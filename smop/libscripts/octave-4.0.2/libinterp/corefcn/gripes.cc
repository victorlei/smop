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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "defun.h"
#include "error.h"
#include "gripes.h"
#include "oct-obj.h"
#include "utils.h"

void
gripe_not_supported (const char *fcn)
{
  error ("%s: not supported on this system", fcn);
}

void
gripe_not_implemented (const char *fcn)
{
  error ("%s: not implemented", fcn);
}

void
gripe_string_invalid (void)
{
  error ("std::string constant used in invalid context");
}

void
gripe_range_invalid (void)
{
  error ("range constant used in invalid context");
}

void
gripe_nonconformant (void)
{
  error ("nonconformant matrices");
}

void
gripe_nonconformant (octave_idx_type r1, octave_idx_type c1,
                     octave_idx_type r2, octave_idx_type c2)
{
  error ("nonconformant matrices (op1 is %dx%d, op2 is %dx%d)",
         r1, c1, r2, c2);
}

void
gripe_empty_arg (const char *name, bool is_error)
{
  if (is_error)
    error ("%s: empty matrix is invalid as an argument", name);
  else
    warning ("%s: argument is empty matrix", name);
}

void
gripe_square_matrix_required (const char *name)
{
  error ("%s: argument must be a square matrix", name);
}

void
gripe_user_supplied_eval (const char *name)
{
  error ("%s: evaluation of user-supplied function failed", name);
}

void
gripe_user_returned_invalid (const char *name)
{
  error ("%s: user-supplied function returned invalid value", name);
}

void
gripe_invalid_conversion (const std::string& from, const std::string& to)
{
  error ("invalid conversion from %s to %s", from.c_str (), to.c_str ());
}

void
gripe_invalid_value_specified (const char *name)
{
  warning ("invalid value specified for '%s'", name);
}

void
gripe_2_or_3_dim_plot (void)
{
  error ("plot: can only plot in 2 or 3 dimensions");
}

void
gripe_unrecognized_float_fmt (void)
{
  error ("unrecognized floating point format requested");
}

void
gripe_unrecognized_data_fmt (const char *warn_for)
{
  error ("%s: unrecognized data format requested", warn_for);
}

void
gripe_data_conversion (const char *from, const char *to)
{
  error ("unable to convert from %s to %s format", from, to);
}

void
gripe_wrong_type_arg (const char *name, const char *s, bool is_error)
{
  if (is_error)
    error ("%s: wrong type argument '%s'", name, s);
  else
    warning ("%s: wrong type argument '%s'", name, s);
}

void
gripe_wrong_type_arg (const char *name, const std::string& s, bool is_error)
{
  gripe_wrong_type_arg (name, s.c_str (), is_error);
}

void
gripe_wrong_type_arg (const char *name, const octave_value& tc,
                      bool is_error)
{
  std::string type = tc.type_name ();

  gripe_wrong_type_arg (name, type, is_error);
}

void
gripe_wrong_type_arg (const std::string& name, const octave_value& tc,
                      bool is_error)
{
  gripe_wrong_type_arg (name.c_str (), tc, is_error);
}

void
gripe_wrong_type_arg_for_unary_op (const octave_value& op)
{
  std::string type = op.type_name ();
  error ("invalid operand '%s' for unary operator", type.c_str ());
}

void
gripe_wrong_type_arg_for_binary_op (const octave_value& op)
{
  std::string type = op.type_name ();
  error ("invalid operand '%s' for binary operator", type.c_str ());
}

void
gripe_implicit_conversion (const char *id, const char *from, const char *to)
{
  warning_with_id (id, "implicit conversion from %s to %s", from, to);
}

void
gripe_implicit_conversion (const std::string& id,
                           const std::string& from, const std::string& to)
{
  warning_with_id (id.c_str (),
                   "implicit conversion from %s to %s",
                   from.c_str (), to.c_str ());
}

void
gripe_divide_by_zero (void)
{
  warning_with_id ("Octave:divide-by-zero", "division by zero");
}

void
gripe_logical_conversion (void)
{
  warning_with_id ("Octave:logical-conversion",
                   "value not equal to 1 or 0 converted to logical 1");
}

void
gripe_library_execution_error (void)
{
  octave_exception_state = octave_no_exception;

  if (! error_state)
    error ("caught execution error in library function");
}

void
gripe_invalid_inquiry_subscript (void)
{
  error ("invalid dimension inquiry of a non-existent value");
}

void
gripe_indexed_cs_list (void)
{
  error ("a cs-list cannot be further indexed");
}

void
gripe_nonbraced_cs_list_assignment (void)
{
  error ("invalid assignment to cs-list outside multiple assignment");
}

void
gripe_warn_complex_cmp (void)
{
  warning_with_id ("Octave:language-extension",
                   "comparing complex numbers is not supported in Matlab");
}

void
gripe_disabled_feature (const std::string& func, const std::string& feature,
                        const std::string& pkg /*="Octave"*/)
{
  error ("%s: support for %s was disabled when %s was built",
         func.c_str (), feature.c_str (), pkg.c_str ());
}

void
gripe_data_file_in_path (const std::string& fcn, const std::string& file)
{
  warning_with_id ("Octave:data-file-in-path",
                   "%s: '%s' found by searching load path",
                   fcn.c_str (), file.c_str ());
}
