/*

Copyright (C) 2004-2015 David Bateman
Copyright (C) 1998-2004 Andy Adler

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
#include <limits>
#include <vector>

#include "dim-vector.h"

#include "mxarray.h"
#include "ov-base.h"
#include "ov-scalar.h"
#include "ov-bool.h"
#include "ov-bool-mat.h"
#include "gripes.h"
#include "ops.h"
#include "oct-locbuf.h"

#include "oct-hdf5.h"

#include "ov-re-sparse.h"
#include "ov-cx-sparse.h"
#include "ov-bool-sparse.h"

#include "ov-base-sparse.h"
#include "ov-base-sparse.cc"

template class OCTINTERP_API octave_base_sparse<SparseBoolMatrix>;


DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_sparse_bool_matrix,
                                     "sparse bool matrix", "logical");

static octave_base_value *
default_numeric_conversion_function (const octave_base_value& a)
{
  CAST_CONV_ARG (const octave_sparse_bool_matrix&);

  return
    new octave_sparse_matrix (SparseMatrix (v.sparse_bool_matrix_value ()));
}

octave_base_value::type_conv_info
octave_sparse_bool_matrix::numeric_conversion_function (void) const
{
  return octave_base_value::type_conv_info (default_numeric_conversion_function,
                                            octave_sparse_matrix::static_type_id ());
}

octave_base_value *
octave_sparse_bool_matrix::try_narrowing_conversion (void)
{
  octave_base_value *retval = 0;

  if (Vsparse_auto_mutate)
    {
      // Don't use numel, since it can overflow for very large matrices
      // Note that for the second test, this means it becomes approximative
      // since it involves a cast to double to avoid issues of overflow
      if (matrix.rows () == 1 && matrix.cols () == 1)
        {
          // Const copy of the matrix, so the right version of () operator used
          const SparseBoolMatrix tmp (matrix);

          retval = new octave_bool (tmp (0));
        }
      else if (matrix.cols () > 0 && matrix.rows () > 0
               && (double (matrix.byte_size ()) > double (matrix.rows ())
                   * double (matrix.cols ()) * sizeof (bool)))
        retval = new octave_bool_matrix (matrix.matrix_value ());
    }

  return retval;
}

double
octave_sparse_bool_matrix::double_value (bool) const
{
  double retval = lo_ieee_nan_value ();

  if (numel () > 0)
    {
      if (numel () > 1)
        gripe_implicit_conversion ("Octave:array-to-scalar",
                                   "bool sparse matrix", "real scalar");

      retval = matrix (0, 0);
    }
  else
    gripe_invalid_conversion ("bool sparse matrix", "real scalar");

  return retval;
}

Complex
octave_sparse_bool_matrix::complex_value (bool) const
{
  double tmp = lo_ieee_nan_value ();

  Complex retval (tmp, tmp);

  if (rows () > 0 && columns () > 0)
    {
      if (numel () > 1)
        gripe_implicit_conversion ("Octave:array-to-scalar",
                                   "bool sparse matrix", "complex scalar");

      retval = matrix (0, 0);
    }
  else
    gripe_invalid_conversion ("bool sparse matrix", "complex scalar");

  return retval;
}

octave_value
octave_sparse_bool_matrix::convert_to_str_internal (bool pad, bool force,
                                                    char type) const
{
  octave_value tmp = octave_value (array_value ());
  return tmp.convert_to_str (pad, force, type);
}

// FIXME: These are inefficient ways of creating full matrices

Matrix
octave_sparse_bool_matrix::matrix_value (bool) const
{
  return Matrix (matrix.matrix_value ());
}

ComplexMatrix
octave_sparse_bool_matrix::complex_matrix_value (bool) const
{
  return ComplexMatrix (matrix.matrix_value ());
}

ComplexNDArray
octave_sparse_bool_matrix::complex_array_value (bool) const
{
  return ComplexNDArray (ComplexMatrix (matrix.matrix_value ()));
}

NDArray
octave_sparse_bool_matrix::array_value (bool) const
{
  return NDArray (Matrix (matrix.matrix_value ()));
}

charNDArray
octave_sparse_bool_matrix::char_array_value (bool) const
{
  charNDArray retval (dims (), 0);
  octave_idx_type nc = matrix.cols ();
  octave_idx_type nr = matrix.rows ();

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = matrix.cidx (j); i < matrix.cidx (j+1); i++)
      retval(matrix.ridx (i) + nr * j) = static_cast<char>(matrix.data (i));

  return retval;
}

boolMatrix
octave_sparse_bool_matrix::bool_matrix_value (bool) const
{
  return matrix.matrix_value ();
}

boolNDArray
octave_sparse_bool_matrix::bool_array_value (bool) const
{
  return boolNDArray (matrix.matrix_value ());
}


SparseMatrix
octave_sparse_bool_matrix::sparse_matrix_value (bool) const
{
  return SparseMatrix (this->matrix);
}

SparseComplexMatrix
octave_sparse_bool_matrix::sparse_complex_matrix_value (bool) const
{
  return SparseComplexMatrix (this->matrix);
}

bool
octave_sparse_bool_matrix::save_binary (std::ostream& os, bool&)
{
  dim_vector d = this->dims ();
  if (d.length () < 1)
    return false;

  // Ensure that additional memory is deallocated
  matrix.maybe_compress ();

  int nr = d(0);
  int nc = d(1);
  int nz = nnz ();

  int32_t itmp;
  // Use negative value for ndims to be consistent with other formats
  itmp = -2;
  os.write (reinterpret_cast<char *> (&itmp), 4);

  itmp = nr;
  os.write (reinterpret_cast<char *> (&itmp), 4);

  itmp = nc;
  os.write (reinterpret_cast<char *> (&itmp), 4);

  itmp = nz;
  os.write (reinterpret_cast<char *> (&itmp), 4);

  // add one to the printed indices to go from
  // zero-based to one-based arrays
  for (int i = 0; i < nc+1; i++)
    {
      octave_quit ();
      itmp = matrix.cidx (i);
      os.write (reinterpret_cast<char *> (&itmp), 4);
    }

  for (int i = 0; i < nz; i++)
    {
      octave_quit ();
      itmp = matrix.ridx (i);
      os.write (reinterpret_cast<char *> (&itmp), 4);
    }

  OCTAVE_LOCAL_BUFFER (char, htmp, nz);

  for (int i = 0; i < nz; i++)
    htmp[i] = (matrix.data (i) ? 1 : 0);

  os.write (htmp, nz);

  return true;
}

bool
octave_sparse_bool_matrix::load_binary (std::istream& is, bool swap,
                                        oct_mach_info::float_format /* fmt */)
{
  int32_t nz, nc, nr, tmp;
  if (! is.read (reinterpret_cast<char *> (&tmp), 4))
    return false;

  if (swap)
    swap_bytes<4> (&tmp);

  if (tmp != -2)
    {
      error ("load: only 2-D sparse matrices are supported");
      return false;
    }

  if (! is.read (reinterpret_cast<char *> (&nr), 4))
    return false;
  if (! is.read (reinterpret_cast<char *> (&nc), 4))
    return false;
  if (! is.read (reinterpret_cast<char *> (&nz), 4))
    return false;

  if (swap)
    {
      swap_bytes<4> (&nr);
      swap_bytes<4> (&nc);
      swap_bytes<4> (&nz);
    }

  SparseBoolMatrix m (static_cast<octave_idx_type> (nr),
                      static_cast<octave_idx_type> (nc),
                      static_cast<octave_idx_type> (nz));

  for (int i = 0; i < nc+1; i++)
    {
      octave_quit ();
      if (! is.read (reinterpret_cast<char *> (&tmp), 4))
        return false;
      if (swap)
        swap_bytes<4> (&tmp);
      m.cidx (i) = tmp;
    }

  for (int i = 0; i < nz; i++)
    {
      octave_quit ();
      if (! is.read (reinterpret_cast<char *> (&tmp), 4))
        return false;
      if (swap)
        swap_bytes<4> (&tmp);
      m.ridx (i) = tmp;
    }

  if (error_state || ! is)
    return false;

  OCTAVE_LOCAL_BUFFER (char, htmp, nz);

  if (! is.read (htmp, nz))
    return false;

  for (int i = 0; i < nz; i++)
    m.data(i) = (htmp[i] ? 1 : 0);

  if (! m.indices_ok ())
    return false;

  matrix = m;

  return true;
}

bool
octave_sparse_bool_matrix::save_hdf5 (octave_hdf5_id loc_id, const char *name, bool)
{
  bool retval = false;

#if defined (HAVE_HDF5)

  dim_vector dv = dims ();
  int empty = save_hdf5_empty (loc_id, name, dv);
  if (empty)
    return (empty > 0);

  // Ensure that additional memory is deallocated
  matrix.maybe_compress ();
#if HAVE_HDF5_18
  hid_t group_hid = H5Gcreate (loc_id, name, H5P_DEFAULT, H5P_DEFAULT,
                               H5P_DEFAULT);
#else
  hid_t group_hid = H5Gcreate (loc_id, name, 0);
#endif
  if (group_hid < 0)
    return false;

  hid_t space_hid, data_hid;
  space_hid = data_hid = -1;
  SparseBoolMatrix m = sparse_bool_matrix_value ();
  octave_idx_type tmp;
  hsize_t hdims[2];

  space_hid = H5Screate_simple (0, hdims, 0);
  if (space_hid < 0)
    {
      H5Gclose (group_hid);
      return false;
    }
#if HAVE_HDF5_18
  data_hid = H5Dcreate (group_hid, "nr", H5T_NATIVE_IDX, space_hid,
                        H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
#else
  data_hid = H5Dcreate (group_hid, "nr", H5T_NATIVE_IDX, space_hid,
                        H5P_DEFAULT);
#endif
  if (data_hid < 0)
    {
      H5Sclose (space_hid);
      H5Gclose (group_hid);
      return false;
    }

  tmp = m.rows ();
  retval = H5Dwrite (data_hid, H5T_NATIVE_IDX, H5S_ALL,
                     H5S_ALL, H5P_DEFAULT, &tmp) >= 0;
  H5Dclose (data_hid);
  if (!retval)
    {
      H5Sclose (space_hid);
      H5Gclose (group_hid);
      return false;
    }

#if HAVE_HDF5_18
  data_hid = H5Dcreate (group_hid, "nc", H5T_NATIVE_IDX, space_hid,
                        H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
#else
  data_hid = H5Dcreate (group_hid, "nc", H5T_NATIVE_IDX, space_hid,
                        H5P_DEFAULT);
#endif
  if (data_hid < 0)
    {
      H5Sclose (space_hid);
      H5Gclose (group_hid);
      return false;
    }

  tmp = m.cols ();
  retval = H5Dwrite (data_hid, H5T_NATIVE_IDX, H5S_ALL, H5S_ALL,
                     H5P_DEFAULT, &tmp) >= 0;
  H5Dclose (data_hid);
  if (!retval)
    {
      H5Sclose (space_hid);
      H5Gclose (group_hid);
      return false;
    }

#if HAVE_HDF5_18
  data_hid = H5Dcreate (group_hid, "nz", H5T_NATIVE_IDX, space_hid,
                        H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
#else
  data_hid = H5Dcreate (group_hid, "nz", H5T_NATIVE_IDX, space_hid,
                        H5P_DEFAULT);
#endif
  if (data_hid < 0)
    {
      H5Sclose (space_hid);
      H5Gclose (group_hid);
      return false;
    }

  tmp = m.nnz ();
  retval = H5Dwrite (data_hid, H5T_NATIVE_IDX, H5S_ALL, H5S_ALL,
                     H5P_DEFAULT, &tmp) >= 0;
  H5Dclose (data_hid);
  if (!retval)
    {
      H5Sclose (space_hid);
      H5Gclose (group_hid);
      return false;
    }

  H5Sclose (space_hid);

  hdims[0] = m.cols () + 1;
  hdims[1] = 1;

  space_hid = H5Screate_simple (2, hdims, 0);

  if (space_hid < 0)
    {
      H5Gclose (group_hid);
      return false;
    }

#if HAVE_HDF5_18
  data_hid = H5Dcreate (group_hid, "cidx", H5T_NATIVE_IDX, space_hid,
                        H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
#else
  data_hid = H5Dcreate (group_hid, "cidx", H5T_NATIVE_IDX, space_hid,
                        H5P_DEFAULT);
#endif
  if (data_hid < 0)
    {
      H5Sclose (space_hid);
      H5Gclose (group_hid);
      return false;
    }

  octave_idx_type * itmp = m.xcidx ();
  retval = H5Dwrite (data_hid, H5T_NATIVE_IDX, H5S_ALL, H5S_ALL,
                     H5P_DEFAULT, itmp) >= 0;
  H5Dclose (data_hid);
  if (!retval)
    {
      H5Sclose (space_hid);
      H5Gclose (group_hid);
      return false;
    }

  H5Sclose (space_hid);

  hdims[0] = m.nnz ();
  hdims[1] = 1;

  space_hid = H5Screate_simple (2, hdims, 0);

  if (space_hid < 0)
    {
      H5Gclose (group_hid);
      return false;
    }

#if HAVE_HDF5_18
  data_hid = H5Dcreate (group_hid, "ridx", H5T_NATIVE_IDX, space_hid,
                        H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
#else
  data_hid = H5Dcreate (group_hid, "ridx", H5T_NATIVE_IDX, space_hid,
                        H5P_DEFAULT);
#endif
  if (data_hid < 0)
    {
      H5Sclose (space_hid);
      H5Gclose (group_hid);
      return false;
    }

  itmp = m.xridx ();
  retval = H5Dwrite (data_hid, H5T_NATIVE_IDX, H5S_ALL, H5S_ALL,
                     H5P_DEFAULT, itmp) >= 0;
  H5Dclose (data_hid);
  if (!retval)
    {
      H5Sclose (space_hid);
      H5Gclose (group_hid);
      return false;
    }

#if HAVE_HDF5_18
  data_hid = H5Dcreate (group_hid, "data", H5T_NATIVE_HBOOL, space_hid,
                        H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
#else
  data_hid = H5Dcreate (group_hid, "data", H5T_NATIVE_HBOOL, space_hid,
                        H5P_DEFAULT);
#endif
  if (data_hid < 0)
    {
      H5Sclose (space_hid);
      H5Gclose (group_hid);
      return false;
    }

  OCTAVE_LOCAL_BUFFER (hbool_t, htmp, m.nnz ());
  for (int i = 0; i < m.nnz (); i++)
    htmp[i] = m.xdata(i);

  retval = H5Dwrite (data_hid, H5T_NATIVE_HBOOL, H5S_ALL, H5S_ALL,
                     H5P_DEFAULT, htmp) >= 0;
  H5Dclose (data_hid);
  H5Sclose (space_hid);
  H5Gclose (group_hid);

#else
  gripe_save ("hdf5");
#endif

  return retval;
}

bool
octave_sparse_bool_matrix::load_hdf5 (octave_hdf5_id loc_id, const char *name)
{
  bool retval = false;

#if defined (HAVE_HDF5)

  octave_idx_type nr, nc, nz;
  hid_t group_hid, data_hid, space_hid;
  hsize_t rank;

  dim_vector dv;
  int empty = load_hdf5_empty (loc_id, name, dv);
  if (empty > 0)
    matrix.resize (dv);
  if (empty)
    return (empty > 0);

#if HAVE_HDF5_18
  group_hid = H5Gopen (loc_id, name, H5P_DEFAULT);
#else
  group_hid = H5Gopen (loc_id, name);
#endif
  if (group_hid < 0) return false;

#if HAVE_HDF5_18
  data_hid = H5Dopen (group_hid, "nr", H5P_DEFAULT);
#else
  data_hid = H5Dopen (group_hid, "nr");
#endif
  space_hid = H5Dget_space (data_hid);
  rank = H5Sget_simple_extent_ndims (space_hid);

  if (rank != 0)
    {
      H5Dclose (data_hid);
      H5Gclose (group_hid);
      return false;
    }

  if (H5Dread (data_hid, H5T_NATIVE_IDX, H5S_ALL, H5S_ALL, H5P_DEFAULT, &nr)
      < 0)
    {
      H5Dclose (data_hid);
      H5Gclose (group_hid);
      return false;
    }

  H5Dclose (data_hid);

#if HAVE_HDF5_18
  data_hid = H5Dopen (group_hid, "nc", H5P_DEFAULT);
#else
  data_hid = H5Dopen (group_hid, "nc");
#endif
  space_hid = H5Dget_space (data_hid);
  rank = H5Sget_simple_extent_ndims (space_hid);

  if (rank != 0)
    {
      H5Dclose (data_hid);
      H5Gclose (group_hid);
      return false;
    }

  if (H5Dread (data_hid, H5T_NATIVE_IDX, H5S_ALL, H5S_ALL, H5P_DEFAULT, &nc)
      < 0)
    {
      H5Dclose (data_hid);
      H5Gclose (group_hid);
      return false;
    }

  H5Dclose (data_hid);

#if HAVE_HDF5_18
  data_hid = H5Dopen (group_hid, "nz", H5P_DEFAULT);
#else
  data_hid = H5Dopen (group_hid, "nz");
#endif
  space_hid = H5Dget_space (data_hid);
  rank = H5Sget_simple_extent_ndims (space_hid);

  if (rank != 0)
    {
      H5Dclose (data_hid);
      H5Gclose (group_hid);
      return false;
    }

  if (H5Dread (data_hid, H5T_NATIVE_IDX, H5S_ALL, H5S_ALL, H5P_DEFAULT, &nz)
      < 0)
    {
      H5Dclose (data_hid);
      H5Gclose (group_hid);
      return false;
    }

  H5Dclose (data_hid);

  SparseBoolMatrix m (static_cast<octave_idx_type> (nr),
                      static_cast<octave_idx_type> (nc),
                      static_cast<octave_idx_type> (nz));

#if HAVE_HDF5_18
  data_hid = H5Dopen (group_hid, "cidx", H5P_DEFAULT);
#else
  data_hid = H5Dopen (group_hid, "cidx");
#endif
  space_hid = H5Dget_space (data_hid);
  rank = H5Sget_simple_extent_ndims (space_hid);

  if (rank != 2)
    {
      H5Sclose (space_hid);
      H5Dclose (data_hid);
      H5Gclose (group_hid);
      return false;
    }

  OCTAVE_LOCAL_BUFFER (hsize_t, hdims, rank);
  OCTAVE_LOCAL_BUFFER (hsize_t, maxdims, rank);

  H5Sget_simple_extent_dims (space_hid, hdims, maxdims);

  if (static_cast<int> (hdims[0]) != nc + 1
      || static_cast<int> (hdims[1]) != 1)
    {
      H5Sclose (space_hid);
      H5Dclose (data_hid);
      H5Gclose (group_hid);
      return false;
    }

  octave_idx_type *itmp = m.xcidx ();
  if (H5Dread (data_hid, H5T_NATIVE_IDX, H5S_ALL, H5S_ALL, H5P_DEFAULT, itmp)
      < 0)
    {
      H5Sclose (space_hid);
      H5Dclose (data_hid);
      H5Gclose (group_hid);
      return false;
    }

  H5Sclose (space_hid);
  H5Dclose (data_hid);

#if HAVE_HDF5_18
  data_hid = H5Dopen (group_hid, "ridx", H5P_DEFAULT);
#else
  data_hid = H5Dopen (group_hid, "ridx");
#endif
  space_hid = H5Dget_space (data_hid);
  rank = H5Sget_simple_extent_ndims (space_hid);

  if (rank != 2)
    {
      H5Sclose (space_hid);
      H5Dclose (data_hid);
      H5Gclose (group_hid);
      return false;
    }

  H5Sget_simple_extent_dims (space_hid, hdims, maxdims);

  if (static_cast<int> (hdims[0]) != nz
      || static_cast<int> (hdims[1]) != 1)
    {
      H5Sclose (space_hid);
      H5Dclose (data_hid);
      H5Gclose (group_hid);
      return false;
    }

  itmp = m.xridx ();
  if (H5Dread (data_hid, H5T_NATIVE_IDX, H5S_ALL, H5S_ALL,
               H5P_DEFAULT, itmp) < 0)
    {
      H5Sclose (space_hid);
      H5Dclose (data_hid);
      H5Gclose (group_hid);
      return false;
    }

  H5Sclose (space_hid);
  H5Dclose (data_hid);

#if HAVE_HDF5_18
  data_hid = H5Dopen (group_hid, "data", H5P_DEFAULT);
#else
  data_hid = H5Dopen (group_hid, "data");
#endif
  space_hid = H5Dget_space (data_hid);
  rank = H5Sget_simple_extent_ndims (space_hid);

  if (rank != 2)
    {
      H5Sclose (space_hid);
      H5Dclose (data_hid);
      H5Gclose (group_hid);
      return false;
    }

  H5Sget_simple_extent_dims (space_hid, hdims, maxdims);

  if (static_cast<int> (hdims[0]) != nz
      || static_cast<int> (hdims[1]) != 1)
    {
      H5Sclose (space_hid);
      H5Dclose (data_hid);
      H5Gclose (group_hid);
      return false;
    }

  OCTAVE_LOCAL_BUFFER (hbool_t, htmp, nz);

  if (H5Dread (data_hid, H5T_NATIVE_HBOOL, H5S_ALL, H5S_ALL,
               H5P_DEFAULT, htmp) >= 0
      && m.indices_ok ())
    {
      retval = true;

      for (int i = 0; i < nz; i++)
        m.xdata(i) = htmp[i];

      matrix = m;
    }

  H5Sclose (space_hid);
  H5Dclose (data_hid);
  H5Gclose (group_hid);

#else
  gripe_load ("hdf5");
#endif

  return retval;
}

mxArray *
octave_sparse_bool_matrix::as_mxArray (void) const
{
  mwSize nz = nzmax ();
  mxArray *retval = new mxArray (mxLOGICAL_CLASS, rows (), columns (),
                                 nz, mxREAL);
  bool *pr = static_cast<bool *> (retval->get_data ());
  mwIndex *ir = retval->get_ir ();
  mwIndex *jc = retval->get_jc ();

  for (mwIndex i = 0; i < nz; i++)
    {
      pr[i] = matrix.data (i);
      ir[i] = matrix.ridx (i);
    }

  for (mwIndex i = 0; i < columns () + 1; i++)
    jc[i] = matrix.cidx (i);

  return retval;
}
