/*

Copyright (C) 2003-2015 John W. Eaton
Copyright (C) 2009 VZLU Prague

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

#include "lo-array-gripes.h"
#include "lo-error.h"

const char *error_id_nonconformant_args = "Octave:nonconformant-args";

const char *error_id_index_out_of_bounds = "Octave:index-out-of-bounds";

const char *error_id_invalid_index = "Octave:invalid-index";

const char *warning_id_nearly_singular_matrix = "Octave:nearly-singular-matrix";

const char *warning_id_singular_matrix = "Octave:singular-matrix";

void
gripe_nan_to_logical_conversion (void)
{
  (*current_liboctave_error_handler)
    ("invalid conversion from NaN to logical");
}

void
gripe_nan_to_character_conversion (void)
{
  (*current_liboctave_error_handler)
    ("invalid conversion from NaN to character");
}

void
gripe_nonconformant (const char *op, octave_idx_type op1_len,
                     octave_idx_type op2_len)
{
  const char *err_id = error_id_nonconformant_args;

  (*current_liboctave_error_with_id_handler)
    (err_id, "%s: nonconformant arguments (op1 len: %d, op2 len: %d)",
     op, op1_len, op2_len);
}

void
gripe_nonconformant (const char *op,
                     octave_idx_type op1_nr, octave_idx_type op1_nc,
                     octave_idx_type op2_nr, octave_idx_type op2_nc)
{
  const char *err_id = error_id_nonconformant_args;

  (*current_liboctave_error_with_id_handler)
    (err_id, "%s: nonconformant arguments (op1 is %dx%d, op2 is %dx%d)",
     op, op1_nr, op1_nc, op2_nr, op2_nc);
}

void
gripe_nonconformant (const char *op, const dim_vector& op1_dims,
                     const dim_vector& op2_dims)
{
  const char *err_id = error_id_nonconformant_args;

  std::string op1_dims_str = op1_dims.str ();
  std::string op2_dims_str = op2_dims.str ();

  (*current_liboctave_error_with_id_handler)
    (err_id, "%s: nonconformant arguments (op1 is %s, op2 is %s)",
     op, op1_dims_str.c_str (), op2_dims_str.c_str ());
}

void
gripe_index_out_of_range (int nd, int dim, octave_idx_type idx,
                          octave_idx_type ext)
{
  const char *err_id = error_id_index_out_of_bounds;

  switch (nd)
    {
    case 1:
      (*current_liboctave_error_with_id_handler)
        (err_id, "A(I): index out of bounds; value %d out of bound %d",
         idx, ext);
      break;

    case 2:
      (*current_liboctave_error_with_id_handler)
        (err_id, "A(I,J): %s index out of bounds; value %d out of bound %d",
         (dim == 1) ? "row" : "column", idx, ext);
      break;

    default:
      (*current_liboctave_error_with_id_handler)
        (err_id, "A(I,J,...): index to dimension %d out of bounds; value %d out of bound %d",
         dim, idx, ext);
      break;
    }
}

void
gripe_del_index_out_of_range (bool is1d, octave_idx_type idx,
                              octave_idx_type ext)
{
  const char *err_id = error_id_index_out_of_bounds;

  (*current_liboctave_error_with_id_handler)
    (err_id, "A(%s) = []: index out of bounds; value %d out of bound %d",
     is1d ? "I" : "..,I,..", idx, ext);
}

void
gripe_invalid_index (void)
{
  const char *err_id = error_id_invalid_index;

  (*current_liboctave_error_with_id_handler)
#ifdef USE_64_BIT_IDX_T
    (err_id, "subscript indices must be either positive integers less than 2^63 or logicals");
#else
    (err_id, "subscript indices must be either positive integers less than 2^31 or logicals");
#endif
}

// FIXME: the following is a common error message to resize,
// regardless of whether it's called from assign or elsewhere.  It
// seems OK to me, but eventually the gripe can be specialized.
// Anyway, propagating various error messages into procedure is, IMHO,
// a nonsense.  If anything, we should change error handling here (and
// throughout liboctave) to allow custom handling of errors

void
gripe_invalid_resize (void)
{
  (*current_liboctave_error_with_id_handler)
    ("Octave:invalid-resize",
     "Invalid resizing operation or ambiguous assignment to an out-of-bounds array element");
}

void
gripe_invalid_assignment_size (void)
{
  (*current_liboctave_error_handler)
    ("A(I) = X: X must have the same size as I");
}

void
gripe_assignment_dimension_mismatch (void)
{
  (*current_liboctave_error_handler)
    ("A(I,J,...) = X: dimensions mismatch");
}

void
gripe_singular_matrix (double rcond)
{
  if (rcond == 0.0)
    {
      (*current_liboctave_warning_with_id_handler)
        (warning_id_singular_matrix,
         "matrix singular to machine precision");
    }
  else
    {
      (*current_liboctave_warning_with_id_handler)
        (warning_id_nearly_singular_matrix,
         "matrix singular to machine precision, rcond = %g", rcond);
    }
}
