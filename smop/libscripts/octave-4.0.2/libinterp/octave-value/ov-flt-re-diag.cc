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

#include "byte-swap.h"

#include "ov-flt-re-diag.h"
#include "ov-base-diag.cc"
#include "ov-float.h"
#include "ov-flt-re-mat.h"
#include "ls-utils.h"

template class octave_base_diag<FloatDiagMatrix, FloatMatrix>;


DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_float_diag_matrix,
                                     "float diagonal matrix", "single");

static octave_base_value *
default_numeric_conversion_function (const octave_base_value& a)
{
  CAST_CONV_ARG (const octave_float_diag_matrix&);

  return new octave_float_matrix (v.float_matrix_value ());
}

octave_base_value::type_conv_info
octave_float_diag_matrix::numeric_conversion_function (void) const
{
  return octave_base_value::type_conv_info (default_numeric_conversion_function,
                                            octave_float_matrix::static_type_id ());
}

octave_base_value *
octave_float_diag_matrix::try_narrowing_conversion (void)
{
  octave_base_value *retval = 0;

  if (matrix.nelem () == 1)
    retval = new octave_float_scalar (matrix (0, 0));

  return retval;
}

DiagMatrix
octave_float_diag_matrix::diag_matrix_value (bool) const
{
  return DiagMatrix (matrix);
}

FloatDiagMatrix
octave_float_diag_matrix::float_diag_matrix_value (bool) const
{
  return matrix;
}

ComplexDiagMatrix
octave_float_diag_matrix::complex_diag_matrix_value (bool) const
{
  return ComplexDiagMatrix (matrix);
}

FloatComplexDiagMatrix
octave_float_diag_matrix::float_complex_diag_matrix_value (bool) const
{
  return FloatComplexDiagMatrix (matrix);
}

octave_value
octave_float_diag_matrix::map (unary_mapper_t umap) const
{
  switch (umap)
    {
    case umap_abs:
      return matrix.abs ();
    case umap_real:
    case umap_conj:
      return matrix;
    case umap_imag:
      return DiagMatrix (matrix.rows (), matrix.cols (), 0.0);
    case umap_sqrt:
      {
        FloatComplexColumnVector tmp = matrix.extract_diag ().map<FloatComplex>
                                       (rc_sqrt);
        FloatComplexDiagMatrix retval (tmp);
        retval.resize (matrix.rows (), matrix.columns ());
        return retval;
      }
    default:
      return to_dense ().map (umap);
    }
}

bool
octave_float_diag_matrix::save_binary (std::ostream& os,
                                       bool& /* save_as_floats*/)
{

  int32_t r = matrix.rows ();
  int32_t c = matrix.cols ();
  os.write (reinterpret_cast<char *> (&r), 4);
  os.write (reinterpret_cast<char *> (&c), 4);

  FloatMatrix m = FloatMatrix (matrix.extract_diag ());
  save_type st = LS_FLOAT;
  if (matrix.length () > 8192) // FIXME: make this configurable.
    {
      float max_val, min_val;
      if (m.all_integers (max_val, min_val))
        st = get_save_type (max_val, min_val);
    }

  const float *mtmp = m.data ();
  write_floats (os, mtmp, st, m.numel ());

  return true;
}

bool
octave_float_diag_matrix::load_binary (std::istream& is, bool swap,
                                       oct_mach_info::float_format fmt)
{
  int32_t r, c;
  char tmp;
  if (! (is.read (reinterpret_cast<char *> (&r), 4)
         && is.read (reinterpret_cast<char *> (&c), 4)
         && is.read (reinterpret_cast<char *> (&tmp), 1)))
    return false;
  if (swap)
    {
      swap_bytes<4> (&r);
      swap_bytes<4> (&c);
    }

  FloatDiagMatrix m (r, c);
  float *re = m.fortran_vec ();
  octave_idx_type len = m.length ();
  read_floats (is, re, static_cast<save_type> (tmp), len, swap, fmt);
  if (error_state || ! is)
    return false;
  matrix = m;

  return true;
}

bool
octave_float_diag_matrix::chk_valid_scalar (const octave_value& val,
                                            float& x) const
{
  bool retval = val.is_real_scalar ();
  if (retval)
    x = val.float_value ();
  return retval;
}
