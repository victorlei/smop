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

#include "lo-ieee.h"
#include "mx-base.h"
#include "oct-locbuf.h"

#include "defun.h"
#include "gripes.h"
#include "mxarray.h"
#include "oct-obj.h"
#include "oct-hdf5.h"
#include "ops.h"
#include "ov-base.h"
#include "ov-base-mat.h"
#include "ov-base-mat.cc"
#include "ov-bool.h"
#include "ov-bool-mat.h"
#include "ov-re-mat.h"
#include "pr-output.h"

#include "byte-swap.h"
#include "ls-oct-ascii.h"
#include "ls-hdf5.h"
#include "ls-utils.h"

template class octave_base_matrix<boolNDArray>;


DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_bool_matrix,
                                     "bool matrix", "logical");

static octave_base_value *
default_numeric_conversion_function (const octave_base_value& a)
{
  CAST_CONV_ARG (const octave_bool_matrix&);

  return new octave_matrix (NDArray (v.bool_array_value ()));
}

octave_base_value::type_conv_info
octave_bool_matrix::numeric_conversion_function (void) const
{
  return octave_base_value::type_conv_info (default_numeric_conversion_function,
                                            octave_matrix::static_type_id ());
}

octave_base_value *
octave_bool_matrix::try_narrowing_conversion (void)
{
  octave_base_value *retval = 0;

  if (matrix.ndims () == 2)
    {
      boolMatrix bm (matrix);

      octave_idx_type nr = bm.rows ();
      octave_idx_type nc = bm.cols ();

      if (nr == 1 && nc == 1)
        retval = new octave_bool (bm (0, 0));
    }

  return retval;
}

double
octave_bool_matrix::double_value (bool) const
{
  double retval = lo_ieee_nan_value ();

  if (rows () > 0 && columns () > 0)
    {
      gripe_implicit_conversion ("Octave:array-to-scalar",
                                 "bool matrix", "real scalar");

      retval = matrix (0, 0);
    }
  else
    gripe_invalid_conversion ("bool matrix", "real scalar");

  return retval;
}

float
octave_bool_matrix::float_value (bool) const
{
  float retval = lo_ieee_float_nan_value ();

  if (rows () > 0 && columns () > 0)
    {
      gripe_implicit_conversion ("Octave:array-to-scalar",
                                 "bool matrix", "real scalar");

      retval = matrix (0, 0);
    }
  else
    gripe_invalid_conversion ("bool matrix", "real scalar");

  return retval;
}

Complex
octave_bool_matrix::complex_value (bool) const
{
  double tmp = lo_ieee_nan_value ();

  Complex retval (tmp, tmp);

  if (rows () > 0 && columns () > 0)
    {
      gripe_implicit_conversion ("Octave:array-to-scalar",
                                 "bool matrix", "complex scalar");

      retval = matrix (0, 0);
    }
  else
    gripe_invalid_conversion ("bool matrix", "complex scalar");

  return retval;
}

FloatComplex
octave_bool_matrix::float_complex_value (bool) const
{
  float tmp = lo_ieee_float_nan_value ();

  FloatComplex retval (tmp, tmp);

  if (rows () > 0 && columns () > 0)
    {
      gripe_implicit_conversion ("Octave:array-to-scalar",
                                 "bool matrix", "complex scalar");

      retval = matrix (0, 0);
    }
  else
    gripe_invalid_conversion ("bool matrix", "complex scalar");

  return retval;
}

octave_value
octave_bool_matrix::convert_to_str_internal (bool pad, bool force,
                                             char type) const
{
  octave_value tmp = octave_value (array_value ());
  return tmp.convert_to_str (pad, force, type);
}

void
octave_bool_matrix::print_raw (std::ostream& os,
                               bool pr_as_read_syntax) const
{
  octave_print_internal (os, matrix, pr_as_read_syntax,
                         current_print_indent_level ());
}

bool
octave_bool_matrix::save_ascii (std::ostream& os)
{
  dim_vector d = dims ();
  if (d.length () > 2)
    {
      NDArray tmp = array_value ();
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

      Matrix tmp = matrix_value ();

      os << tmp;
    }

  return true;
}

bool
octave_bool_matrix::load_ascii (std::istream& is)
{
  bool success = true;

  string_vector keywords (2);

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
                  boolNDArray btmp (dv);

                  if (btmp.is_empty ())
                    matrix = btmp;
                  else
                    {
                      NDArray tmp(dv);
                      is >> tmp;

                      if (is)
                        {
                          for (octave_idx_type i = 0; i < btmp.nelem (); i++)
                            btmp.elem (i) = (tmp.elem (i) != 0.);

                          matrix = btmp;
                        }
                      else
                        {
                          error ("load: failed to load matrix constant");
                          success = false;
                        }
                    }
                }
              else
                {
                  error ("load: failed to extract dimensions");
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
                  Matrix tmp (nr, nc);
                  is >> tmp;
                  if (is)
                    {
                      boolMatrix btmp (nr, nc);
                      for (octave_idx_type j = 0; j < nc; j++)
                        for (octave_idx_type i = 0; i < nr; i++)
                          btmp.elem (i,j) = (tmp.elem (i, j) != 0.);

                      matrix = btmp;
                    }
                  else
                    {
                      error ("load: failed to load matrix constant");
                      success = false;
                    }
                }
              else if (nr == 0 || nc == 0)
                matrix = boolMatrix (nr, nc);
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
octave_bool_matrix::save_binary (std::ostream& os, bool& /* save_as_floats */)
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

  boolNDArray m = bool_array_value ();
  bool *mtmp = m.fortran_vec ();
  octave_idx_type nel = m.nelem ();
  OCTAVE_LOCAL_BUFFER (char, htmp, nel);

  for (octave_idx_type i = 0; i < nel; i++)
    htmp[i] = (mtmp[i] ? 1 : 0);

  os.write (htmp, nel);

  return true;
}

bool
octave_bool_matrix::load_binary (std::istream& is, bool swap,
                                 oct_mach_info::float_format /* fmt */)
{
  int32_t mdims;
  if (! is.read (reinterpret_cast<char *> (&mdims), 4))
    return false;
  if (swap)
    swap_bytes<4> (&mdims);
  if (mdims >= 0)
    return false;

  // mdims is negative for consistency with other matrices, where it is
  // negative to allow the positive value to be used for rows/cols for
  // backward compatibility
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

  octave_idx_type nel = dv.numel ();
  OCTAVE_LOCAL_BUFFER (char, htmp, nel);
  if (! is.read (htmp, nel))
    return false;
  boolNDArray m(dv);
  bool *mtmp = m.fortran_vec ();
  for (octave_idx_type i = 0; i < nel; i++)
    mtmp[i] = (htmp[i] ? 1 : 0);
  matrix = m;

  return true;
}

bool
octave_bool_matrix::save_hdf5 (octave_hdf5_id loc_id, const char *name,
                               bool /* save_as_floats */)
{
  bool retval = true;

#if defined (HAVE_HDF5)

  dim_vector dv = dims ();
  int empty = save_hdf5_empty (loc_id, name, dv);
  if (empty)
    return (empty > 0);

  int rank = dv.length ();
  hid_t space_hid, data_hid;
  space_hid = data_hid = -1;
  boolNDArray m = bool_array_value ();

  OCTAVE_LOCAL_BUFFER (hsize_t, hdims, rank);

  // Octave uses column-major, while HDF5 uses row-major ordering
  for (int i = 0; i < rank; i++)
    hdims[i] = dv (rank-i-1);

  space_hid = H5Screate_simple (rank, hdims, 0);
  if (space_hid < 0) return false;
#if HAVE_HDF5_18
  data_hid = H5Dcreate (loc_id, name, H5T_NATIVE_HBOOL, space_hid,
                        H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
#else
  data_hid = H5Dcreate (loc_id, name, H5T_NATIVE_HBOOL, space_hid,
                        H5P_DEFAULT);
#endif
  if (data_hid < 0)
    {
      H5Sclose (space_hid);
      return false;
    }

  octave_idx_type nel = m.nelem ();
  bool *mtmp = m.fortran_vec ();
  OCTAVE_LOCAL_BUFFER (hbool_t, htmp, nel);

  for (octave_idx_type i = 0; i < nel; i++)
    htmp[i] = mtmp[i];

  retval = H5Dwrite (data_hid, H5T_NATIVE_HBOOL, H5S_ALL, H5S_ALL,
                     H5P_DEFAULT, htmp) >= 0;

  H5Dclose (data_hid);
  H5Sclose (space_hid);

#else
  gripe_save ("hdf5");
#endif

  return retval;
}

bool
octave_bool_matrix::load_hdf5 (octave_hdf5_id loc_id, const char *name)
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
  hid_t space_id = H5Dget_space (data_hid);

  hsize_t rank = H5Sget_simple_extent_ndims (space_id);

  if (rank < 1)
    {
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

  octave_idx_type nel = dv.numel ();
  OCTAVE_LOCAL_BUFFER (hbool_t, htmp, nel);
  if (H5Dread (data_hid, H5T_NATIVE_HBOOL, H5S_ALL, H5S_ALL, H5P_DEFAULT, htmp)
      >= 0)
    {
      retval = true;

      boolNDArray btmp (dv);
      for (octave_idx_type i = 0; i < nel; i++)
        btmp.elem (i) = htmp[i];

      matrix = btmp;
    }

  H5Dclose (data_hid);

#else
  gripe_load ("hdf5");
#endif

  return retval;
}

mxArray *
octave_bool_matrix::as_mxArray (void) const
{
  mxArray *retval = new mxArray (mxLOGICAL_CLASS, dims (), mxREAL);

  bool *pr = static_cast<bool *> (retval->get_data ());

  mwSize nel = numel ();

  const bool *p = matrix.data ();

  for (mwIndex i = 0; i < nel; i++)
    pr[i] = p[i];

  return retval;
}

DEFUN (logical, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} logical (@var{x})\n\
Convert the numeric object @var{x} to logical type.\n\
\n\
Any nonzero values will be converted to true (1) while zero values will be\n\
converted to false (0).  The non-numeric value NaN cannot be converted and\n\
will produce an error.\n\
\n\
Compatibility Note: Octave accepts complex values as input, whereas\n\
@sc{matlab} issues an error.\n\
@seealso{double, single, char}\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 1)
    {
      octave_value arg = args(0);
      if (arg.is_bool_type ())
        retval = arg;
      else if (arg.is_numeric_type ())
        {
          if (arg.is_sparse_type ())
            retval = arg.sparse_bool_matrix_value ();
          else if (arg.is_scalar_type ())
            retval = arg.bool_value ();
          else
            retval = arg.bool_array_value ();
        }
      else
        gripe_wrong_type_arg ("logical", arg);
    }
  else
    print_usage ();

  return retval;
}

/*
%!test
%! m = eye (2) != 0;
%! s = !0;
%! c = {"double", "single", "int8", "int16", "int32", "int64", "uint8", "uint16", "uint32", "uint64", "logical"};
%! for i = 1:numel (c)
%!   assert (logical (eye (2, c{i})), m)
%!   assert (logical (eye (1, c{i})), s)
%! endfor
*/
