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

#include "ov-cx-diag.h"
#include "ov-flt-cx-diag.h"
#include "ov-re-diag.h"
#include "ov-base-diag.cc"
#include "ov-complex.h"
#include "ov-cx-mat.h"
#include "ls-utils.h"

template class octave_base_diag<ComplexDiagMatrix, ComplexMatrix>;


DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_complex_diag_matrix,
                                     "complex diagonal matrix", "double");

static octave_base_value *
default_numeric_conversion_function (const octave_base_value& a)
{
  CAST_CONV_ARG (const octave_complex_diag_matrix&);

  return new octave_complex_matrix (v.complex_matrix_value ());
}

octave_base_value::type_conv_info
octave_complex_diag_matrix::numeric_conversion_function (void) const
{
  return octave_base_value::type_conv_info
           (default_numeric_conversion_function,
            octave_complex_matrix::static_type_id ());
}

static octave_base_value *
default_numeric_demotion_function (const octave_base_value& a)
{
  CAST_CONV_ARG (const octave_complex_diag_matrix&);

  return new octave_float_complex_diag_matrix
               (v.float_complex_diag_matrix_value ());
}

octave_base_value::type_conv_info
octave_complex_diag_matrix::numeric_demotion_function (void) const
{
  return
    octave_base_value::type_conv_info (default_numeric_demotion_function,
                                       octave_float_complex_diag_matrix::static_type_id ());
}

octave_base_value *
octave_complex_diag_matrix::try_narrowing_conversion (void)
{
  octave_base_value *retval = 0;

  if (matrix.nelem () == 1)
    {
      retval = new octave_complex (matrix (0, 0));
      octave_base_value *rv2 = retval->try_narrowing_conversion ();
      if (rv2)
        {
          delete retval;
          retval = rv2;
        }
    }
  else if (matrix.all_elements_are_real ())
    {
      return new octave_diag_matrix (::real (matrix));
    }

  return retval;
}

DiagMatrix
octave_complex_diag_matrix::diag_matrix_value (bool force_conversion) const
{
  DiagMatrix retval;

  if (! force_conversion)
    gripe_implicit_conversion ("Octave:imag-to-real",
                               type_name (), "real matrix");

  retval = ::real (matrix);

  return retval;
}

FloatDiagMatrix
octave_complex_diag_matrix::float_diag_matrix_value (bool force_conversion) const
{
  DiagMatrix retval;

  if (! force_conversion)
    gripe_implicit_conversion ("Octave:imag-to-real",
                               type_name (), "real matrix");

  retval = ::real (matrix);

  return retval;
}

ComplexDiagMatrix
octave_complex_diag_matrix::complex_diag_matrix_value (bool) const
{
  return matrix;
}

FloatComplexDiagMatrix
octave_complex_diag_matrix::float_complex_diag_matrix_value (bool) const
{
  return FloatComplexDiagMatrix (matrix);
}

octave_value
octave_complex_diag_matrix::map (unary_mapper_t umap) const
{
  switch (umap)
    {
    case umap_abs:
      return matrix.abs ();
    case umap_real:
      return ::real (matrix);
    case umap_conj:
      return ::conj (matrix);
    case umap_imag:
      return ::imag (matrix);
    case umap_sqrt:
      {
        ComplexColumnVector tmp =
          matrix.extract_diag ().map<Complex> (std::sqrt);
        ComplexDiagMatrix retval (tmp);
        retval.resize (matrix.rows (), matrix.columns ());
        return retval;
      }
    default:
      return to_dense ().map (umap);
    }
}

bool
octave_complex_diag_matrix::save_binary (std::ostream& os, bool& save_as_floats)
{

  int32_t r = matrix.rows ();
  int32_t c = matrix.cols ();
  os.write (reinterpret_cast<char *> (&r), 4);
  os.write (reinterpret_cast<char *> (&c), 4);

  ComplexMatrix m = ComplexMatrix (matrix.extract_diag ());
  save_type st = LS_DOUBLE;
  if (save_as_floats)
    {
      if (m.too_large_for_float ())
        {
          warning ("save: some values too large to save as floats --");
          warning ("save: saving as doubles instead");
        }
      else
        st = LS_FLOAT;
    }
  else if (matrix.length () > 4096) // FIXME: make this configurable.
    {
      double max_val, min_val;
      if (m.all_integers (max_val, min_val))
        st = get_save_type (max_val, min_val);
    }

  const Complex *mtmp = m.data ();
  write_doubles (os, reinterpret_cast<const double *> (mtmp), st,
                 2 * m.numel ());

  return true;
}

bool
octave_complex_diag_matrix::load_binary (std::istream& is, bool swap,
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

  ComplexDiagMatrix m (r, c);
  Complex *im = m.fortran_vec ();
  octave_idx_type len = m.length ();
  read_doubles (is, reinterpret_cast<double *> (im),
                static_cast<save_type> (tmp), 2 * len, swap, fmt);
  if (error_state || ! is)
    return false;
  matrix = m;

  return true;
}

bool
octave_complex_diag_matrix::chk_valid_scalar (const octave_value& val,
                                              Complex& x) const
{
  bool retval = val.is_complex_scalar () || val.is_real_scalar ();
  if (retval)
    x = val.complex_value ();
  return retval;
}

/*

%% bug #36368
%!assert (diag ([1+i, 1-i])^2 , diag ([2i, -2i]), 4*eps);

*/
