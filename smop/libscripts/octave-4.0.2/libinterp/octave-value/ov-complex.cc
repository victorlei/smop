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

#include <iostream>

#include "lo-ieee.h"
#include "lo-specfun.h"
#include "lo-mappers.h"

#include "mxarray.h"
#include "oct-obj.h"
#include "oct-hdf5.h"
#include "oct-stream.h"
#include "ops.h"
#include "ov-complex.h"
#include "ov-flt-complex.h"
#include "ov-base.h"
#include "ov-base-scalar.h"
#include "ov-base-scalar.cc"
#include "ov-cx-mat.h"
#include "ov-scalar.h"
#include "gripes.h"
#include "pr-output.h"
#include "ops.h"

#include "ls-oct-ascii.h"
#include "ls-hdf5.h"

// Prevent implicit instantiations on some systems (Windows, others?)
// that can lead to duplicate definitions of static data members.

extern template class OCTINTERP_API octave_base_scalar<double>;
extern template class OCTINTERP_API octave_base_scalar<FloatComplex>;


template class octave_base_scalar<Complex>;


DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_complex,
                                     "complex scalar", "double");

static octave_base_value *
default_numeric_demotion_function (const octave_base_value& a)
{
  CAST_CONV_ARG (const octave_complex&);

  return new octave_float_complex (v.float_complex_value ());
}

octave_base_value::type_conv_info
octave_complex::numeric_demotion_function (void) const
{
  return
    octave_base_value::type_conv_info (default_numeric_demotion_function,
                                       octave_float_complex::static_type_id ());
}

octave_base_value *
octave_complex::try_narrowing_conversion (void)
{
  octave_base_value *retval = 0;

  double im = std::imag (scalar);

  if (im == 0.0)
    retval = new octave_scalar (std::real (scalar));

  return retval;
}

octave_value
octave_complex::do_index_op (const octave_value_list& idx, bool resize_ok)
{
  // FIXME: this doesn't solve the problem of
  //
  //   a = i; a([1,1], [1,1], [1,1])
  //
  // and similar constructions.  Hmm...

  // FIXME: using this constructor avoids narrowing the
  // 1x1 matrix back to a scalar value.  Need a better solution
  // to this problem.

  octave_value tmp (new octave_complex_matrix (complex_matrix_value ()));

  return tmp.do_index_op (idx, resize_ok);
}

double
octave_complex::double_value (bool force_conversion) const
{
  double retval;

  if (! force_conversion)
    gripe_implicit_conversion ("Octave:imag-to-real",
                               "complex scalar", "real scalar");

  retval = std::real (scalar);

  return retval;
}

float
octave_complex::float_value (bool force_conversion) const
{
  float retval;

  if (! force_conversion)
    gripe_implicit_conversion ("Octave:imag-to-real",
                               "complex scalar", "real scalar");

  retval = std::real (scalar);

  return retval;
}

Matrix
octave_complex::matrix_value (bool force_conversion) const
{
  Matrix retval;

  if (! force_conversion)
    gripe_implicit_conversion ("Octave:imag-to-real",
                               "complex scalar", "real matrix");

  retval = Matrix (1, 1, std::real (scalar));

  return retval;
}

FloatMatrix
octave_complex::float_matrix_value (bool force_conversion) const
{
  FloatMatrix retval;

  if (! force_conversion)
    gripe_implicit_conversion ("Octave:imag-to-real",
                               "complex scalar", "real matrix");

  retval = FloatMatrix (1, 1, std::real (scalar));

  return retval;
}

NDArray
octave_complex::array_value (bool force_conversion) const
{
  NDArray retval;

  if (! force_conversion)
    gripe_implicit_conversion ("Octave:imag-to-real",
                               "complex scalar", "real matrix");

  retval = NDArray (dim_vector (1, 1), std::real (scalar));

  return retval;
}

FloatNDArray
octave_complex::float_array_value (bool force_conversion) const
{
  FloatNDArray retval;

  if (! force_conversion)
    gripe_implicit_conversion ("Octave:imag-to-real",
                               "complex scalar", "real matrix");

  retval = FloatNDArray (dim_vector (1, 1), std::real (scalar));

  return retval;
}

Complex
octave_complex::complex_value (bool) const
{
  return scalar;
}

FloatComplex
octave_complex::float_complex_value (bool) const
{
  return static_cast<FloatComplex> (scalar);
}

ComplexMatrix
octave_complex::complex_matrix_value (bool) const
{
  return ComplexMatrix (1, 1, scalar);
}

FloatComplexMatrix
octave_complex::float_complex_matrix_value (bool) const
{
  return FloatComplexMatrix (1, 1, static_cast<FloatComplex> (scalar));
}

ComplexNDArray
octave_complex::complex_array_value (bool /* force_conversion */) const
{
  return ComplexNDArray (dim_vector (1, 1), scalar);
}

FloatComplexNDArray
octave_complex::float_complex_array_value (bool /* force_conversion */) const
{
  return FloatComplexNDArray (dim_vector (1, 1),
                              static_cast<FloatComplex> (scalar));
}

octave_value
octave_complex::resize (const dim_vector& dv, bool fill) const
{
  if (fill)
    {
      ComplexNDArray retval (dv, Complex (0));

      if (dv.numel ())
        retval(0) = scalar;

      return retval;
    }
  else
    {
      ComplexNDArray retval (dv);

      if (dv.numel ())
        retval(0) = scalar;

      return retval;
    }
}

octave_value
octave_complex::diag (octave_idx_type m, octave_idx_type n) const
{
  return ComplexDiagMatrix (Array<Complex> (dim_vector (1, 1), scalar), m, n);
}

bool
octave_complex::save_ascii (std::ostream& os)
{
  Complex c = complex_value ();

  octave_write_complex (os, c);

  os << "\n";

  return true;
}

bool
octave_complex::load_ascii (std::istream& is)
{
  scalar = octave_read_value<Complex> (is);

  if (!is)
    {
      error ("load: failed to load complex scalar constant");
      return false;
    }

  return true;
}


bool
octave_complex::save_binary (std::ostream& os, bool& /* save_as_floats */)
{
  char tmp = static_cast<char> (LS_DOUBLE);
  os.write (reinterpret_cast<char *> (&tmp), 1);
  Complex ctmp = complex_value ();
  os.write (reinterpret_cast<char *> (&ctmp), 16);

  return true;
}

bool
octave_complex::load_binary (std::istream& is, bool swap,
                             oct_mach_info::float_format fmt)
{
  char tmp;
  if (! is.read (reinterpret_cast<char *> (&tmp), 1))
    return false;

  Complex ctmp;
  read_doubles (is, reinterpret_cast<double *> (&ctmp),
                static_cast<save_type> (tmp), 2, swap, fmt);
  if (error_state || ! is)
    return false;

  scalar = ctmp;
  return true;
}

bool
octave_complex::save_hdf5 (octave_hdf5_id loc_id, const char *name,
                           bool /* save_as_floats */)
{
  bool retval = false;

#if defined (HAVE_HDF5)

  hsize_t dimens[3];
  hid_t space_hid, type_hid, data_hid;
  space_hid = type_hid = data_hid = -1;

  space_hid = H5Screate_simple (0, dimens, 0);
  if (space_hid < 0)
    return false;

  type_hid = hdf5_make_complex_type (H5T_NATIVE_DOUBLE);
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

  Complex tmp = complex_value ();
  retval = H5Dwrite (data_hid, type_hid, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                     &tmp) >= 0;

  H5Dclose (data_hid);
  H5Tclose (type_hid);
  H5Sclose (space_hid);

#else
  gripe_save ("hdf5");
#endif

  return retval;
}

bool
octave_complex::load_hdf5 (octave_hdf5_id loc_id, const char *name)
{
  bool retval = false;

#if defined (HAVE_HDF5)

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

  if (rank != 0)
    {
      H5Tclose (complex_type);
      H5Sclose (space_id);
      H5Dclose (data_hid);
      return false;
    }

  // complex scalar:
  Complex ctmp;
  if (H5Dread (data_hid, complex_type, H5S_ALL, H5S_ALL, H5P_DEFAULT,
               &ctmp) >= 0)
    {
      retval = true;
      scalar = ctmp;
    }

  H5Tclose (complex_type);
  H5Sclose (space_id);
  H5Dclose (data_hid);

#else
  gripe_load ("hdf5");
#endif

  return retval;
}

mxArray *
octave_complex::as_mxArray (void) const
{
  mxArray *retval = new mxArray (mxDOUBLE_CLASS, 1, 1, mxCOMPLEX);

  double *pr = static_cast<double *> (retval->get_data ());
  double *pi = static_cast<double *> (retval->get_imag_data ());

  pr[0] = std::real (scalar);
  pi[0] = std::imag (scalar);

  return retval;
}

octave_value
octave_complex::map (unary_mapper_t umap) const
{
  switch (umap)
    {
#define SCALAR_MAPPER(UMAP, FCN) \
    case umap_ ## UMAP: \
      return octave_value (FCN (scalar))

      SCALAR_MAPPER (abs, std::abs);
      SCALAR_MAPPER (acos, ::acos);
      SCALAR_MAPPER (acosh, ::acosh);
      SCALAR_MAPPER (angle, std::arg);
      SCALAR_MAPPER (arg, std::arg);
      SCALAR_MAPPER (asin, ::asin);
      SCALAR_MAPPER (asinh, ::asinh);
      SCALAR_MAPPER (atan, ::atan);
      SCALAR_MAPPER (atanh, ::atanh);
      SCALAR_MAPPER (erf, ::erf);
      SCALAR_MAPPER (erfc, ::erfc);
      SCALAR_MAPPER (erfcx, ::erfcx);
      SCALAR_MAPPER (erfi, ::erfi);
      SCALAR_MAPPER (dawson, ::dawson);
      SCALAR_MAPPER (ceil, ::ceil);
      SCALAR_MAPPER (conj, std::conj);
      SCALAR_MAPPER (cos, std::cos);
      SCALAR_MAPPER (cosh, std::cosh);
      SCALAR_MAPPER (exp, std::exp);
      SCALAR_MAPPER (expm1, ::expm1);
      SCALAR_MAPPER (fix, ::fix);
      SCALAR_MAPPER (floor, ::floor);
      SCALAR_MAPPER (imag, std::imag);
      SCALAR_MAPPER (log, std::log);
      SCALAR_MAPPER (log2, xlog2);
      SCALAR_MAPPER (log10, std::log10);
      SCALAR_MAPPER (log1p, ::log1p);
      SCALAR_MAPPER (real, std::real);
      SCALAR_MAPPER (round, xround);
      SCALAR_MAPPER (roundb, xroundb);
      SCALAR_MAPPER (signum, ::signum);
      SCALAR_MAPPER (sin, std::sin);
      SCALAR_MAPPER (sinh, std::sinh);
      SCALAR_MAPPER (sqrt, std::sqrt);
      SCALAR_MAPPER (tan, std::tan);
      SCALAR_MAPPER (tanh, std::tanh);
      SCALAR_MAPPER (finite, xfinite);
      SCALAR_MAPPER (isinf, xisinf);
      SCALAR_MAPPER (isna, octave_is_NA);
      SCALAR_MAPPER (isnan, xisnan);

    default:
      return octave_base_value::map (umap);
    }
}
