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

#include "data-conv.h"
#include "mach-info.h"
#include "lo-specfun.h"
#include "lo-mappers.h"

#include "defun.h"
#include "gripes.h"
#include "mxarray.h"
#include "oct-obj.h"
#include "oct-hdf5.h"
#include "oct-stream.h"
#include "ov-scalar.h"
#include "ov-float.h"
#include "ov-base.h"
#include "ov-base-scalar.h"
#include "ov-base-scalar.cc"
#include "ov-flt-re-mat.h"
#include "ov-typeinfo.h"
#include "pr-output.h"
#include "xdiv.h"
#include "xpow.h"
#include "ops.h"

#include "ls-oct-ascii.h"
#include "ls-hdf5.h"

template class octave_base_scalar<float>;


DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_float_scalar, "float scalar",
                                     "single");

octave_value
octave_float_scalar::do_index_op (const octave_value_list& idx, bool resize_ok)
{
  // FIXME: this doesn't solve the problem of
  //
  //   a = 1; a([1,1], [1,1], [1,1])
  //
  // and similar constructions.  Hmm...

  // FIXME: using this constructor avoids narrowing the
  // 1x1 matrix back to a scalar value.  Need a better solution
  // to this problem.

  octave_value tmp (new octave_float_matrix (float_matrix_value ()));

  return tmp.do_index_op (idx, resize_ok);
}

octave_value
octave_float_scalar::resize (const dim_vector& dv, bool fill) const
{
  if (fill)
    {
      FloatNDArray retval (dv, 0);

      if (dv.numel ())
        retval(0) = scalar;

      return retval;
    }
  else
    {
      FloatNDArray retval (dv);

      if (dv.numel ())
        retval(0) = scalar;

      return retval;
    }
}

octave_value
octave_float_scalar::diag (octave_idx_type m, octave_idx_type n) const
{
  return FloatDiagMatrix (Array<float> (dim_vector (1, 1), scalar), m, n);
}

octave_value
octave_float_scalar::convert_to_str_internal (bool, bool, char type) const
{
  octave_value retval;

  if (xisnan (scalar))
    gripe_nan_to_character_conversion ();
  else
    {
      int ival = NINT (scalar);

      if (ival < 0 || ival > std::numeric_limits<unsigned char>::max ())
        {
          // FIXME: is there something better we could do?

          ival = 0;

          ::warning ("range error for conversion to character value");
        }

      retval = octave_value (std::string (1, static_cast<char> (ival)), type);
    }

  return retval;
}

bool
octave_float_scalar::save_ascii (std::ostream& os)
{
  float d = float_value ();

  octave_write_float (os, d);

  os << "\n";

  return true;
}

bool
octave_float_scalar::load_ascii (std::istream& is)
{
  scalar = octave_read_value<float> (is);
  if (!is)
    {
      error ("load: failed to load scalar constant");
      return false;
    }

  return true;
}

bool
octave_float_scalar::save_binary (std::ostream& os, bool& /* save_as_floats */)
{
  char tmp = LS_FLOAT;
  os.write (reinterpret_cast<char *> (&tmp), 1);
  float dtmp = float_value ();
  os.write (reinterpret_cast<char *> (&dtmp), 4);

  return true;
}

bool
octave_float_scalar::load_binary (std::istream& is, bool swap,
                                  oct_mach_info::float_format fmt)
{
  char tmp;
  if (! is.read (reinterpret_cast<char *> (&tmp), 1))
    return false;

  float dtmp;
  read_floats (is, &dtmp, static_cast<save_type> (tmp), 1, swap, fmt);
  if (error_state || ! is)
    return false;

  scalar = dtmp;
  return true;
}

bool
octave_float_scalar::save_hdf5 (octave_hdf5_id loc_id, const char *name,
                                bool /* save_as_floats */)
{
  bool retval = false;

#if defined (HAVE_HDF5)

  hsize_t dimens[3];
  hid_t space_hid, data_hid;
  space_hid = data_hid = -1;

  space_hid = H5Screate_simple (0, dimens, 0);
  if (space_hid < 0) return false;
#if HAVE_HDF5_18
  data_hid = H5Dcreate (loc_id, name, H5T_NATIVE_FLOAT, space_hid,
                        H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
#else
  data_hid = H5Dcreate (loc_id, name, H5T_NATIVE_FLOAT, space_hid,
                        H5P_DEFAULT);
#endif
  if (data_hid < 0)
    {
      H5Sclose (space_hid);
      return false;
    }

  float tmp = float_value ();
  retval = H5Dwrite (data_hid, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL,
                     H5P_DEFAULT, &tmp) >= 0;

  H5Dclose (data_hid);
  H5Sclose (space_hid);

#else
  gripe_save ("hdf5");
#endif

  return retval;
}

bool
octave_float_scalar::load_hdf5 (octave_hdf5_id loc_id, const char *name)
{
#if defined (HAVE_HDF5)

#if HAVE_HDF5_18
  hid_t data_hid = H5Dopen (loc_id, name, H5P_DEFAULT);
#else
  hid_t data_hid = H5Dopen (loc_id, name);
#endif
  hid_t space_id = H5Dget_space (data_hid);

  hsize_t rank = H5Sget_simple_extent_ndims (space_id);

  if (rank != 0)
    {
      H5Dclose (data_hid);
      return false;
    }

  float dtmp;
  if (H5Dread (data_hid, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL,
               H5P_DEFAULT, &dtmp) < 0)
    {
      H5Dclose (data_hid);
      return false;
    }

  scalar = dtmp;

  H5Dclose (data_hid);

  return true;

#else
  gripe_load ("hdf5");
  return false;
#endif
}

mxArray *
octave_float_scalar::as_mxArray (void) const
{
  mxArray *retval = new mxArray (mxSINGLE_CLASS, 1, 1, mxREAL);

  float *pr = static_cast<float *> (retval->get_data ());

  pr[0] = scalar;

  return retval;
}

octave_value
octave_float_scalar::map (unary_mapper_t umap) const
{
  switch (umap)
    {
    case umap_imag:
      return 0.0f;

    case umap_real:
    case umap_conj:
      return scalar;

#define SCALAR_MAPPER(UMAP, FCN) \
    case umap_ ## UMAP: \
      return octave_value (FCN (scalar))

      SCALAR_MAPPER (abs, ::fabsf);
      SCALAR_MAPPER (acos, rc_acos);
      SCALAR_MAPPER (acosh, rc_acosh);
      SCALAR_MAPPER (angle, ::arg);
      SCALAR_MAPPER (arg, ::arg);
      SCALAR_MAPPER (asin, rc_asin);
      SCALAR_MAPPER (asinh, ::asinhf);
      SCALAR_MAPPER (atan, ::atanf);
      SCALAR_MAPPER (atanh, rc_atanh);
      SCALAR_MAPPER (erf, ::erff);
      SCALAR_MAPPER (erfinv, ::erfinv);
      SCALAR_MAPPER (erfcinv, ::erfcinv);
      SCALAR_MAPPER (erfc, ::erfcf);
      SCALAR_MAPPER (erfcx, ::erfcx);
      SCALAR_MAPPER (erfi, ::erfi);
      SCALAR_MAPPER (dawson, ::dawson);
      SCALAR_MAPPER (gamma, xgamma);
      SCALAR_MAPPER (lgamma, rc_lgamma);
      SCALAR_MAPPER (cbrt, ::cbrtf);
      SCALAR_MAPPER (ceil, ::ceilf);
      SCALAR_MAPPER (cos, ::cosf);
      SCALAR_MAPPER (cosh, ::coshf);
      SCALAR_MAPPER (exp, ::expf);
      SCALAR_MAPPER (expm1, ::expm1f);
      SCALAR_MAPPER (fix, ::fix);
      SCALAR_MAPPER (floor, gnulib::floorf);
      SCALAR_MAPPER (log, rc_log);
      SCALAR_MAPPER (log2, rc_log2);
      SCALAR_MAPPER (log10, rc_log10);
      SCALAR_MAPPER (log1p, rc_log1p);
      SCALAR_MAPPER (round, xround);
      SCALAR_MAPPER (roundb, xroundb);
      SCALAR_MAPPER (signum, ::signum);
      SCALAR_MAPPER (sin, ::sinf);
      SCALAR_MAPPER (sinh, ::sinhf);
      SCALAR_MAPPER (sqrt, rc_sqrt);
      SCALAR_MAPPER (tan, ::tanf);
      SCALAR_MAPPER (tanh, ::tanhf);
      SCALAR_MAPPER (finite, xfinite);
      SCALAR_MAPPER (isinf, xisinf);
      SCALAR_MAPPER (isna, octave_is_NA);
      SCALAR_MAPPER (isnan, xisnan);
      SCALAR_MAPPER (xsignbit, xsignbit);

    // Special cases for Matlab compatibility.
    case umap_xtolower:
    case umap_xtoupper:
      return scalar;

    case umap_xisalnum:
    case umap_xisalpha:
    case umap_xisascii:
    case umap_xiscntrl:
    case umap_xisdigit:
    case umap_xisgraph:
    case umap_xislower:
    case umap_xisprint:
    case umap_xispunct:
    case umap_xisspace:
    case umap_xisupper:
    case umap_xisxdigit:
    case umap_xtoascii:
      {
        octave_value str_conv = convert_to_str (true, true);
        return error_state ? octave_value () : str_conv.map (umap);
      }

    default:
      return octave_base_value::map (umap);
    }
}

bool
octave_float_scalar::fast_elem_insert_self (void *where,
                                            builtin_type_t btyp) const
{

  // Support inline real->complex conversion.
  if (btyp == btyp_float)
    {
      *(reinterpret_cast<float *>(where)) = scalar;
      return true;
    }
  else if (btyp == btyp_float_complex)
    {
      *(reinterpret_cast<FloatComplex *>(where)) = scalar;
      return true;
    }
  else
    return false;
}
