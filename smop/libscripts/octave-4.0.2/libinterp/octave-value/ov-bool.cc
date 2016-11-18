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

#include "mx-base.h"

#include "gripes.h"
#include "mxarray.h"
#include "oct-hdf5.h"
#include "oct-obj.h"
#include "ops.h"
#include "ov-bool.h"
#include "ov-bool-mat.h"
#include "ov-base.h"
#include "ov-base-scalar.h"
#include "ov-base-scalar.cc"
#include "ov-re-mat.h"
#include "ov-scalar.h"
#include "pr-output.h"

#include "ls-oct-ascii.h"
#include "ls-hdf5.h"

// Prevent implicit instantiations on some systems (Windows, others?)
// that can lead to duplicate definitions of static data members.

extern template class OCTINTERP_API octave_base_scalar<double>;


template class octave_base_scalar<bool>;


DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_bool, "bool", "logical");

static octave_base_value *
default_numeric_conversion_function (const octave_base_value& a)
{
  CAST_CONV_ARG (const octave_bool&);

  return new octave_scalar (v.bool_value ());
}

octave_base_value::type_conv_info
octave_bool::numeric_conversion_function (void) const
{
  return octave_base_value::type_conv_info (default_numeric_conversion_function,
                                            octave_scalar::static_type_id ());

}

octave_value
octave_bool::do_index_op (const octave_value_list& idx, bool resize_ok)
{
  // FIXME: this doesn't solve the problem of
  //
  //   a = 1; a([1,1], [1,1], [1,1])
  //
  // and similar constructions.  Hmm...

  // FIXME: using this constructor avoids narrowing the
  // 1x1 matrix back to a scalar value.  Need a better solution
  // to this problem.

  octave_value tmp (new octave_bool_matrix (bool_matrix_value ()));

  return tmp.do_index_op (idx, resize_ok);
}

octave_value
octave_bool::resize (const dim_vector& dv, bool fill) const
{
  if (fill)
    {
      boolNDArray retval (dv, false);
      if (dv.numel ())
        retval(0) = scalar;
      return retval;
    }
  else
    {
      boolNDArray retval (dv);
      if (dv.numel ())
        retval(0) = scalar;
      return retval;
    }
}

octave_value
octave_bool::convert_to_str_internal (bool, bool, char type) const
{
  char s[2];
  s[0] = static_cast<char> (scalar);
  s[1] = '\0';

  return octave_value (s, type);
}

bool
octave_bool::save_ascii (std::ostream& os)
{
  double d = double_value ();

  octave_write_double (os, d);
  os << "\n";

  return true;
}

bool
octave_bool::load_ascii (std::istream& is)
{
  scalar = (octave_read_value<double> (is) != 0.);

  if (!is)
    {
      error ("load: failed to load scalar constant");
      return false;
    }

  return true;
}

bool
octave_bool::save_binary (std::ostream& os, bool& /* save_as_floats */)
{
  char tmp = (scalar ? 1 : 0);
  os.write (reinterpret_cast<char *> (&tmp), 1);

  return true;
}

bool
octave_bool::load_binary (std::istream& is, bool /* swap */,
                          oct_mach_info::float_format /* fmt */)
{
  char tmp;
  if (! is.read (reinterpret_cast<char *> (&tmp), 1))
    return false;
  scalar = (tmp ? 1 : 0);
  return true;
}

bool
octave_bool::save_hdf5 (octave_hdf5_id loc_id, const char *name,
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
  data_hid = H5Dcreate (loc_id, name, H5T_NATIVE_DOUBLE, space_hid,
                        H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
#else
  data_hid = H5Dcreate (loc_id, name, H5T_NATIVE_DOUBLE, space_hid,
                        H5P_DEFAULT);
#endif
  if (data_hid < 0)
    {
      H5Sclose (space_hid);
      return false;
    }

  double tmp = double_value ();
  retval = H5Dwrite (data_hid, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL,
                     H5P_DEFAULT, &tmp) >= 0;

  H5Dclose (data_hid);
  H5Sclose (space_hid);

#else
  gripe_save ("hdf5");
#endif

  return retval;
}

bool
octave_bool::load_hdf5 (octave_hdf5_id loc_id, const char *name)
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

  double dtmp;
  if (H5Dread (data_hid, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL,
               H5P_DEFAULT, &dtmp) < 0)
    {
      H5Dclose (data_hid);
      return false;
    }

  scalar = (dtmp != 0.);

  H5Dclose (data_hid);

#else
  gripe_load ("hdf5");
#endif

  return true;
}

mxArray *
octave_bool::as_mxArray (void) const
{
  mxArray *retval = new mxArray (mxLOGICAL_CLASS, 1, 1, mxREAL);

  bool *pr = static_cast<bool *> (retval->get_data ());

  pr[0] = scalar;

  return retval;
}
