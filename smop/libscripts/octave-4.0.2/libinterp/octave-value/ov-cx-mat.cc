/*

Copyright (C) 1996-2015 John W. Eaton
Copyright (C) 2009-2010 VZLU Prague

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

#include <iostream>
#include <vector>

#include "data-conv.h"
#include "lo-ieee.h"
#include "lo-specfun.h"
#include "lo-mappers.h"
#include "mx-base.h"
#include "mach-info.h"
#include "oct-locbuf.h"

#include "gripes.h"
#include "mxarray.h"
#include "oct-obj.h"
#include "oct-hdf5.h"
#include "oct-stream.h"
#include "ops.h"
#include "ov-base.h"
#include "ov-base-mat.h"
#include "ov-base-mat.cc"
#include "ov-complex.h"
#include "ov-cx-mat.h"
#include "ov-flt-cx-mat.h"
#include "ov-re-mat.h"
#include "ov-scalar.h"
#include "pr-output.h"

#include "byte-swap.h"
#include "ls-oct-ascii.h"
#include "ls-hdf5.h"
#include "ls-utils.h"

template class octave_base_matrix<ComplexNDArray>;


DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_complex_matrix,
                                     "complex matrix", "double");

static octave_base_value *
default_numeric_demotion_function (const octave_base_value& a)
{
  CAST_CONV_ARG (const octave_complex_matrix&);

  return new octave_float_complex_matrix (v.float_complex_array_value ());
}

octave_base_value::type_conv_info
octave_complex_matrix::numeric_demotion_function (void) const
{
  return octave_base_value::type_conv_info
           (default_numeric_demotion_function,
            octave_float_complex_matrix::static_type_id ());
}

octave_base_value *
octave_complex_matrix::try_narrowing_conversion (void)
{
  octave_base_value *retval = 0;

  if (matrix.numel () == 1)
    {
      Complex c = matrix (0);

      if (std::imag (c) == 0.0)
        retval = new octave_scalar (std::real (c));
      else
        retval = new octave_complex (c);
    }
  else if (matrix.all_elements_are_real ())
    retval = new octave_matrix (::real (matrix));

  return retval;
}

double
octave_complex_matrix::double_value (bool force_conversion) const
{
  double retval = lo_ieee_nan_value ();

  if (! force_conversion)
    gripe_implicit_conversion ("Octave:imag-to-real",
                               "complex matrix", "real scalar");

  if (rows () > 0 && columns () > 0)
    {
      gripe_implicit_conversion ("Octave:array-to-scalar",
                                 "complex matrix", "real scalar");

      retval = std::real (matrix (0, 0));
    }
  else
    gripe_invalid_conversion ("complex matrix", "real scalar");

  return retval;
}

float
octave_complex_matrix::float_value (bool force_conversion) const
{
  float retval = lo_ieee_float_nan_value ();

  if (! force_conversion)
    gripe_implicit_conversion ("Octave:imag-to-real",
                               "complex matrix", "real scalar");

  if (rows () > 0 && columns () > 0)
    {
      gripe_implicit_conversion ("Octave:array-to-scalar",
                                 "complex matrix", "real scalar");

      retval = std::real (matrix (0, 0));
    }
  else
    gripe_invalid_conversion ("complex matrix", "real scalar");

  return retval;
}

NDArray
octave_complex_matrix::array_value (bool force_conversion) const
{
  NDArray retval;

  if (! force_conversion)
    gripe_implicit_conversion ("Octave:imag-to-real",
                               "complex matrix", "real matrix");

  retval = ::real (matrix);

  return retval;
}

Matrix
octave_complex_matrix::matrix_value (bool force_conversion) const
{
  Matrix retval;

  if (! force_conversion)
    gripe_implicit_conversion ("Octave:imag-to-real",
                               "complex matrix", "real matrix");

  retval = ::real (ComplexMatrix (matrix));

  return retval;
}

FloatMatrix
octave_complex_matrix::float_matrix_value (bool force_conversion) const
{
  FloatMatrix retval;

  if (! force_conversion)
    gripe_implicit_conversion ("Octave:imag-to-real",
                               "complex matrix", "real matrix");

  retval = ::real (ComplexMatrix (matrix));

  return retval;
}

Complex
octave_complex_matrix::complex_value (bool) const
{
  double tmp = lo_ieee_nan_value ();

  Complex retval (tmp, tmp);

  if (rows () > 0 && columns () > 0)
    {
      gripe_implicit_conversion ("Octave:array-to-scalar",
                                 "complex matrix", "complex scalar");

      retval = matrix (0, 0);
    }
  else
    gripe_invalid_conversion ("complex matrix", "complex scalar");

  return retval;
}

FloatComplex
octave_complex_matrix::float_complex_value (bool) const
{
  float tmp = lo_ieee_float_nan_value ();

  FloatComplex retval (tmp, tmp);

  if (rows () > 0 && columns () > 0)
    {
      gripe_implicit_conversion ("Octave:array-to-scalar",
                                 "complex matrix", "complex scalar");

      retval = matrix (0, 0);
    }
  else
    gripe_invalid_conversion ("complex matrix", "complex scalar");

  return retval;
}

ComplexMatrix
octave_complex_matrix::complex_matrix_value (bool) const
{
  return ComplexMatrix (matrix);
}

FloatComplexMatrix
octave_complex_matrix::float_complex_matrix_value (bool) const
{
  return FloatComplexMatrix (ComplexMatrix (matrix));
}

boolNDArray
octave_complex_matrix::bool_array_value (bool warn) const
{
  if (matrix.any_element_is_nan ())
    gripe_nan_to_logical_conversion ();
  else if (warn && (! matrix.all_elements_are_real ()
                    || real (matrix).any_element_not_one_or_zero ()))
    gripe_logical_conversion ();

  return mx_el_ne (matrix, Complex (0.0));
}

charNDArray
octave_complex_matrix::char_array_value (bool frc_str_conv) const
{
  charNDArray retval;

  if (! frc_str_conv)
    gripe_implicit_conversion ("Octave:num-to-str",
                               "complex matrix", "string");
  else
    {
      retval = charNDArray (dims ());
      octave_idx_type nel = numel ();

      for (octave_idx_type i = 0; i < nel; i++)
        retval.elem (i) = static_cast<char>(std::real (matrix.elem (i)));
    }

  return retval;
}

FloatComplexNDArray
octave_complex_matrix::float_complex_array_value (bool) const
{
  return FloatComplexNDArray (matrix);
}

SparseMatrix
octave_complex_matrix::sparse_matrix_value (bool force_conversion) const
{
  SparseMatrix retval;

  if (! force_conversion)
    gripe_implicit_conversion ("Octave:imag-to-real",
                               "complex matrix", "real matrix");

  retval = SparseMatrix (::real (ComplexMatrix (matrix)));

  return retval;
}

SparseComplexMatrix
octave_complex_matrix::sparse_complex_matrix_value (bool) const
{
  return SparseComplexMatrix (ComplexMatrix (matrix));
}

octave_value
octave_complex_matrix::diag (octave_idx_type k) const
{
  octave_value retval;
  if (k == 0 && matrix.ndims () == 2
      && (matrix.rows () == 1 || matrix.columns () == 1))
    retval = ComplexDiagMatrix (DiagArray2<Complex> (matrix));
  else
    retval = octave_base_matrix<ComplexNDArray>::diag (k);

  return retval;
}

octave_value
octave_complex_matrix::diag (octave_idx_type m, octave_idx_type n) const
{
  octave_value retval;

  if (matrix.ndims () == 2
      && (matrix.rows () == 1 || matrix.columns () == 1))
    {
      ComplexMatrix mat (matrix);

      retval = mat.diag (m, n);
    }
  else
    error ("diag: expecting vector argument");

  return retval;
}

bool
octave_complex_matrix::save_ascii (std::ostream& os)
{
  dim_vector d = dims ();
  if (d.length () > 2)
    {
      ComplexNDArray tmp = complex_array_value ();

      os << "# ndims: " << d.length () << "\n";

      for (int i = 0; i < d.length (); i++)
        os << " " << d (i);

      os << "\n" << tmp;
    }
  else
    {
      // Keep this case, rather than use generic code above for backward
      // compatiability. Makes load_ascii much more complex!!
      os << "# rows: " << rows () << "\n"
         << "# columns: " << columns () << "\n";

      os << complex_matrix_value ();
    }

  return true;
}

bool
octave_complex_matrix::load_ascii (std::istream& is)
{
  bool success = true;

  string_vector keywords(2);

  keywords[0] = "ndims";
  keywords[1] = "rows";

  std::string kw;
  octave_idx_type val = 0;

  if (extract_keyword (is, keywords, kw, val, true))
    {
      if (kw == "ndims")
        {
          int mdims = static_cast<int> (val);

          if (mdims >= 0)
            {
              dim_vector dv;
              dv.resize (mdims);

              for (int i = 0; i < mdims; i++)
                is >> dv(i);

              if (is)
                {
                  ComplexNDArray tmp(dv);

                  is >> tmp;

                  if (is)
                    matrix = tmp;
                  else
                    {
                      error ("load: failed to load matrix constant");
                      success = false;
                    }
                }
              else
                {
                  error ("load: failed to read dimensions");
                  success = false;
                }
            }
          else
            {
              error ("load: failed to extract number of dimensions");
              success = false;
            }
        }
      else if (kw == "rows")
        {
          octave_idx_type nr = val;
          octave_idx_type nc = 0;

          if (nr >= 0 && extract_keyword (is, "columns", nc) && nc >= 0)
            {
              if (nr > 0 && nc > 0)
                {
                  ComplexMatrix tmp (nr, nc);
                  is >> tmp;
                  if (is)
                    matrix = tmp;
                  else
                    {
                      error ("load: failed to load matrix constant");
                      success = false;
                    }
                }
              else if (nr == 0 || nc == 0)
                matrix = ComplexMatrix (nr, nc);
              else
                panic_impossible ();
            }
          else
            {
              error ("load: failed to extract number of rows and columns");
              success = false;
            }
        }
      else
        panic_impossible ();
    }
  else
    {
      error ("load: failed to extract number of rows and columns");
      success = false;
    }

  return success;
}

bool
octave_complex_matrix::save_binary (std::ostream& os, bool& save_as_floats)
{
  dim_vector d = dims ();
  if (d.length () < 1)
    return false;

  // Use negative value for ndims to differentiate with old format!!
  int32_t tmp = - d.length ();
  os.write (reinterpret_cast<char *> (&tmp), 4);
  for (int i = 0; i < d.length (); i++)
    {
      tmp = d(i);
      os.write (reinterpret_cast<char *> (&tmp), 4);
    }

  ComplexNDArray m = complex_array_value ();
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
  else if (d.numel () > 4096) // FIXME: make this configurable.
    {
      double max_val, min_val;
      if (m.all_integers (max_val, min_val))
        st = get_save_type (max_val, min_val);
    }


  const Complex *mtmp = m.data ();
  write_doubles (os, reinterpret_cast<const double *> (mtmp), st,
                 2 * d.numel ());

  return true;
}

bool
octave_complex_matrix::load_binary (std::istream& is, bool swap,
                                    oct_mach_info::float_format fmt)
{
  char tmp;
  int32_t mdims;
  if (! is.read (reinterpret_cast<char *> (&mdims), 4))
    return false;
  if (swap)
    swap_bytes<4> (&mdims);
  if (mdims < 0)
    {
      mdims = - mdims;
      int32_t di;
      dim_vector dv;
      dv.resize (mdims);

      for (int i = 0; i < mdims; i++)
        {
          if (! is.read (reinterpret_cast<char *> (&di), 4))
            return false;
          if (swap)
            swap_bytes<4> (&di);
          dv(i) = di;
        }

      // Convert an array with a single dimension to be a row vector.
      // Octave should never write files like this, other software
      // might.

      if (mdims == 1)
        {
          mdims = 2;
          dv.resize (mdims);
          dv(1) = dv(0);
          dv(0) = 1;
        }

      if (! is.read (reinterpret_cast<char *> (&tmp), 1))
        return false;

      ComplexNDArray m(dv);
      Complex *im = m.fortran_vec ();
      read_doubles (is, reinterpret_cast<double *> (im),
                    static_cast<save_type> (tmp), 2 * dv.numel (), swap, fmt);
      if (error_state || ! is)
        return false;
      matrix = m;
    }
  else
    {
      int32_t nr, nc;
      nr = mdims;
      if (! is.read (reinterpret_cast<char *> (&nc), 4))
        return false;
      if (swap)
        swap_bytes<4> (&nc);
      if (! is.read (reinterpret_cast<char *> (&tmp), 1))
        return false;
      ComplexMatrix m (nr, nc);
      Complex *im = m.fortran_vec ();
      octave_idx_type len = nr * nc;
      read_doubles (is, reinterpret_cast<double *> (im),
                    static_cast<save_type> (tmp), 2*len, swap, fmt);
      if (error_state || ! is)
        return false;
      matrix = m;
    }
  return true;
}

bool
octave_complex_matrix::save_hdf5 (octave_hdf5_id loc_id, const char *name,
                                  bool save_as_floats)
{
#if defined (HAVE_HDF5)

  dim_vector dv = dims ();
  int empty = save_hdf5_empty (loc_id, name, dv);
  if (empty)
    return (empty > 0);

  int rank = dv.length ();
  hid_t space_hid, data_hid, type_hid;
  space_hid = data_hid = type_hid = -1;
  bool retval = true;
  ComplexNDArray m = complex_array_value ();

  OCTAVE_LOCAL_BUFFER (hsize_t, hdims, rank);

  // Octave uses column-major, while HDF5 uses row-major ordering
  for (int i = 0; i < rank; i++)
    hdims[i] = dv (rank-i-1);

  space_hid = H5Screate_simple (rank, hdims, 0);
  if (space_hid < 0) return false;

  hid_t save_type_hid = H5T_NATIVE_DOUBLE;

  if (save_as_floats)
    {
      if (m.too_large_for_float ())
        {
          warning ("save: some values too large to save as floats --");
          warning ("save: saving as doubles instead");
        }
      else
        save_type_hid = H5T_NATIVE_FLOAT;
    }
#if HAVE_HDF5_INT2FLOAT_CONVERSIONS
  // hdf5 currently doesn't support float/integer conversions
  else
    {
      double max_val, min_val;

      if (m.all_integers (max_val, min_val))
        save_type_hid
          = save_type_to_hdf5 (get_save_type (max_val, min_val));
    }
#endif /* HAVE_HDF5_INT2FLOAT_CONVERSIONS */

  type_hid = hdf5_make_complex_type (save_type_hid);
  if (type_hid < 0)
    {
      H5Sclose (space_hid);
      return false;
    }
#if HAVE_HDF5_18
  data_hid = H5Dcreate (loc_id, name, type_hid, space_hid,
                        H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
#else
  data_hid = H5Dcreate (loc_id, name, type_hid, space_hid, H5P_DEFAULT);
#endif
  if (data_hid < 0)
    {
      H5Sclose (space_hid);
      H5Tclose (type_hid);
      return false;
    }

  hid_t complex_type_hid = hdf5_make_complex_type (H5T_NATIVE_DOUBLE);
  if (complex_type_hid < 0) retval = false;

  if (retval)
    {
      Complex *mtmp = m.fortran_vec ();
      if (H5Dwrite (data_hid, complex_type_hid, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                    mtmp) < 0)
        {
          H5Tclose (complex_type_hid);
          retval = false;
        }
    }

  H5Tclose (complex_type_hid);
  H5Dclose (data_hid);
  H5Tclose (type_hid);
  H5Sclose (space_hid);

  return retval;

#else
  gripe_save ("hdf5");
  return false;
#endif
}

bool
octave_complex_matrix::load_hdf5 (octave_hdf5_id loc_id, const char *name)
{
  bool retval = false;

#if defined (HAVE_HDF5)

  dim_vector dv;
  int empty = load_hdf5_empty (loc_id, name, dv);
  if (empty > 0)
    matrix.resize (dv);
  if (empty)
    return (empty > 0);

#if HAVE_HDF5_18
  hid_t data_hid = H5Dopen (loc_id, name, H5P_DEFAULT);
#else
  hid_t data_hid = H5Dopen (loc_id, name);
#endif
  hid_t type_hid = H5Dget_type (data_hid);

  hid_t complex_type = hdf5_make_complex_type (H5T_NATIVE_DOUBLE);

  if (! hdf5_types_compatible (type_hid, complex_type))
    {
      H5Tclose (complex_type);
      H5Dclose (data_hid);
      return false;
    }

  hid_t space_id = H5Dget_space (data_hid);

  hsize_t rank = H5Sget_simple_extent_ndims (space_id);

  if (rank < 1)
    {
      H5Tclose (complex_type);
      H5Sclose (space_id);
      H5Dclose (data_hid);
      return false;
    }

  OCTAVE_LOCAL_BUFFER (hsize_t, hdims, rank);
  OCTAVE_LOCAL_BUFFER (hsize_t, maxdims, rank);

  H5Sget_simple_extent_dims (space_id, hdims, maxdims);

  // Octave uses column-major, while HDF5 uses row-major ordering
  if (rank == 1)
    {
      dv.resize (2);
      dv(0) = 1;
      dv(1) = hdims[0];
    }
  else
    {
      dv.resize (rank);
      for (hsize_t i = 0, j = rank - 1; i < rank; i++, j--)
        dv(j) = hdims[i];
    }

  ComplexNDArray m (dv);
  Complex *reim = m.fortran_vec ();
  if (H5Dread (data_hid, complex_type, H5S_ALL, H5S_ALL, H5P_DEFAULT,
               reim) >= 0)
    {
      retval = true;
      matrix = m;
    }

  H5Tclose (complex_type);
  H5Sclose (space_id);
  H5Dclose (data_hid);

#else
  gripe_load ("hdf5");
#endif

  return retval;
}

void
octave_complex_matrix::print_raw (std::ostream& os,
                                  bool pr_as_read_syntax) const
{
  octave_print_internal (os, matrix, pr_as_read_syntax,
                         current_print_indent_level ());
}

mxArray *
octave_complex_matrix::as_mxArray (void) const
{
  mxArray *retval = new mxArray (mxDOUBLE_CLASS, dims (), mxCOMPLEX);

  double *pr = static_cast<double *> (retval->get_data ());
  double *pi = static_cast<double *> (retval->get_imag_data ());

  mwSize nel = numel ();

  const Complex *p = matrix.data ();

  for (mwIndex i = 0; i < nel; i++)
    {
      pr[i] = std::real (p[i]);
      pi[i] = std::imag (p[i]);
    }

  return retval;
}

octave_value
octave_complex_matrix::map (unary_mapper_t umap) const
{
  switch (umap)
    {
    // Mappers handled specially.
    case umap_real:
      return ::real (matrix);
    case umap_imag:
      return ::imag (matrix);
    case umap_conj:
      return ::conj (matrix);

#define ARRAY_METHOD_MAPPER(UMAP, FCN) \
    case umap_ ## UMAP: \
      return octave_value (matrix.FCN ())

      ARRAY_METHOD_MAPPER (abs, abs);
      ARRAY_METHOD_MAPPER (isnan, isnan);
      ARRAY_METHOD_MAPPER (isinf, isinf);
      ARRAY_METHOD_MAPPER (finite, isfinite);

#define ARRAY_MAPPER(UMAP, TYPE, FCN) \
    case umap_ ## UMAP: \
      return octave_value (matrix.map<TYPE> (FCN))

      ARRAY_MAPPER (acos, Complex, ::acos);
      ARRAY_MAPPER (acosh, Complex, ::acosh);
      ARRAY_MAPPER (angle, double, std::arg);
      ARRAY_MAPPER (arg, double, std::arg);
      ARRAY_MAPPER (asin, Complex, ::asin);
      ARRAY_MAPPER (asinh, Complex, ::asinh);
      ARRAY_MAPPER (atan, Complex, ::atan);
      ARRAY_MAPPER (atanh, Complex, ::atanh);
      ARRAY_MAPPER (erf, Complex, ::erf);
      ARRAY_MAPPER (erfc, Complex, ::erfc);
      ARRAY_MAPPER (erfcx, Complex, ::erfcx);
      ARRAY_MAPPER (erfi, Complex, ::erfi);
      ARRAY_MAPPER (dawson, Complex, ::dawson);
      ARRAY_MAPPER (ceil, Complex, ::ceil);
      ARRAY_MAPPER (cos, Complex, std::cos);
      ARRAY_MAPPER (cosh, Complex, std::cosh);
      ARRAY_MAPPER (exp, Complex, std::exp);
      ARRAY_MAPPER (expm1, Complex, ::expm1);
      ARRAY_MAPPER (fix, Complex, ::fix);
      ARRAY_MAPPER (floor, Complex, ::floor);
      ARRAY_MAPPER (log, Complex, std::log);
      ARRAY_MAPPER (log2, Complex, xlog2);
      ARRAY_MAPPER (log10, Complex, std::log10);
      ARRAY_MAPPER (log1p, Complex, ::log1p);
      ARRAY_MAPPER (round, Complex, xround);
      ARRAY_MAPPER (roundb, Complex, xroundb);
      ARRAY_MAPPER (signum, Complex, ::signum);
      ARRAY_MAPPER (sin, Complex, std::sin);
      ARRAY_MAPPER (sinh, Complex, std::sinh);
      ARRAY_MAPPER (sqrt, Complex, std::sqrt);
      ARRAY_MAPPER (tan, Complex, std::tan);
      ARRAY_MAPPER (tanh, Complex, std::tanh);
      ARRAY_MAPPER (isna, bool, octave_is_NA);

    default:
      return octave_base_value::map (umap);
    }
}
