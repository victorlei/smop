/*

Copyright (C) 2004-2015 John W. Eaton

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

#include "lo-ieee.h"
#include "lo-utils.h"
#include "mx-base.h"
#include "quit.h"
#include "oct-locbuf.h"

#include "defun.h"
#include "gripes.h"
#include "oct-obj.h"
#include "oct-lvalue.h"
#include "oct-hdf5.h"
#include "oct-stream.h"
#include "ops.h"
#include "ov-base.h"
#include "ov-base-mat.h"
#include "ov-base-mat.cc"
#include "ov-base-scalar.h"
#include "ov-base-scalar.cc"
#include "ov-base-int.h"
#include "ov-int-traits.h"
#include "pr-output.h"
#include "variables.h"

#include "byte-swap.h"
#include "ls-oct-ascii.h"
#include "ls-utils.h"
#include "ls-hdf5.h"

// We have all the machinery below (octave_base_int_helper and
// octave_base_int_helper_traits) to avoid a few warnings from GCC
// about comparisons always false due to limited range of data types.
// Ugh.  The cure may be worse than the disease.

template <class T, bool is_signed = true, bool can_be_too_big = true>
struct octave_base_int_helper
{
  static bool
  char_value_out_of_range (T val)
  {
    return val < 0 || val > std::numeric_limits<unsigned char>::max ();
  }
};

template <class T>
struct octave_base_int_helper<T, false, false>
{
  static bool char_value_out_of_range (T) { return false; }
};

template <class T>
struct octave_base_int_helper<T, false, true>
{
  static bool char_value_out_of_range (T val)
  {
    return val > std::numeric_limits<unsigned char>::max ();
  }
};

template <class T>
struct octave_base_int_helper<T, true, false>
{
  static bool char_value_out_of_range (T val) { return val < 0; }
};

// For all types other than char, signed char, and unsigned char, we
// assume that the upper limit for the range of allowable values is
// larger than the range for unsigned char.  If that's not true, we
// are still OK, but will see the warnings again for any other types
// that do not meet this assumption.

template <class T>
struct octave_base_int_helper_traits
{
  static const bool can_be_larger_than_uchar_max = true;
};

template <>
struct octave_base_int_helper_traits<char>
{
  static const bool can_be_larger_than_uchar_max = false;
};

template <>
struct octave_base_int_helper_traits<signed char>
{
  static const bool can_be_larger_than_uchar_max = false;
};

template <>
struct octave_base_int_helper_traits<unsigned char>
{
  static const bool can_be_larger_than_uchar_max = false;
};


template <class T>
octave_base_value *
octave_base_int_matrix<T>::try_narrowing_conversion (void)
{
  octave_base_value *retval = 0;

  if (this->matrix.nelem () == 1)
    retval = new typename octave_value_int_traits<T>::scalar_type
               (this->matrix (0));

  return retval;
}

template <class T>
octave_value
octave_base_int_matrix<T>::convert_to_str_internal (bool, bool, char type) const
{
  octave_value retval;
  dim_vector dv = this->dims ();
  octave_idx_type nel = dv.numel ();

  charNDArray chm (dv);

  bool warned = false;

  for (octave_idx_type i = 0; i < nel; i++)
    {
      octave_quit ();

      typename T::element_type tmp = this->matrix(i);

      typedef typename T::element_type::val_type val_type;

      val_type ival = tmp.value ();

      static const bool is_signed = std::numeric_limits<val_type>::is_signed;
      static const bool can_be_larger_than_uchar_max
        = octave_base_int_helper_traits<val_type>::can_be_larger_than_uchar_max;

      if (octave_base_int_helper<val_type, is_signed,
          can_be_larger_than_uchar_max>::char_value_out_of_range (ival))
        {
          // FIXME: is there something better we could do?

          ival = 0;

          if (! warned)
            {
              ::warning ("range error for conversion to character value");
              warned = true;
            }
        }
      else
        chm (i) = static_cast<char> (ival);
    }

  retval = octave_value (chm, type);

  return retval;
}

template <class T>
bool
octave_base_int_matrix<T>::save_ascii (std::ostream& os)
{
  dim_vector d = this->dims ();

  os << "# ndims: " << d.length () << "\n";

  for (int i = 0; i < d.length (); i++)
    os << " " << d (i);

  os << "\n" << this->matrix;

  return true;
}

template <class T>
bool
octave_base_int_matrix<T>::load_ascii (std::istream& is)
{
  int mdims = 0;
  bool success = true;

  if (extract_keyword (is, "ndims", mdims, true))
    {
      if (mdims >= 0)
        {
          dim_vector dv;
          dv.resize (mdims);

          for (int i = 0; i < mdims; i++)
            is >> dv(i);

          T tmp(dv);

          is >> tmp;

          if (!is)
            {
              error ("load: failed to load matrix constant");
              success = false;
            }

          this->matrix = tmp;
        }
      else
        {
          error ("load: failed to extract number of rows and columns");
          success = false;
        }
    }
  else
    error ("load: failed to extract number of dimensions");

  return success;
}

template <class T>
bool
octave_base_int_matrix<T>::save_binary (std::ostream& os, bool&)
{
  dim_vector d = this->dims ();
  if (d.length () < 1)
    return false;

  // Use negative value for ndims to differentiate with old format!!
  int32_t tmp = - d.length ();
  os.write (reinterpret_cast<char *> (&tmp), 4);
  for (int i=0; i < d.length (); i++)
    {
      tmp = d(i);
      os.write (reinterpret_cast<char *> (&tmp), 4);
    }

  os.write (reinterpret_cast<const char *> (this->matrix.data ()),
            this->byte_size ());

  return true;
}

template <class T>
bool
octave_base_int_matrix<T>::load_binary (std::istream& is, bool swap,
                                        oct_mach_info::float_format)
{
  int32_t mdims;
  if (! is.read (reinterpret_cast<char *> (&mdims), 4))
    return false;
  if (swap)
    swap_bytes<4> (&mdims);
  if (mdims >= 0)
    return false;

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

  T m (dv);

  if (! is.read (reinterpret_cast<char *> (m.fortran_vec ()), m.byte_size ()))
    return false;

  if (swap)
    {
      int nel = dv.numel ();
      int bytes = nel / m.byte_size ();
      for (int i = 0; i < nel; i++)
        switch (bytes)
          {
          case 8:
            swap_bytes<8> (&m(i));
            break;
          case 4:
            swap_bytes<4> (&m(i));
            break;
          case 2:
            swap_bytes<2> (&m(i));
            break;
          case 1:
          default:
            break;
          }
    }

  this->matrix = m;
  return true;
}

template <class T>
bool
octave_base_int_matrix<T>::save_hdf5 (octave_hdf5_id loc_id, const char *name, bool)
{
  bool retval = false;

#if defined (HAVE_HDF5)

  hid_t save_type_hid = HDF5_SAVE_TYPE;
  dim_vector dv = this->dims ();
  int empty = save_hdf5_empty (loc_id, name, dv);
  if (empty)
    return (empty > 0);

  int rank = dv.length ();
  hid_t space_hid, data_hid;
  space_hid = data_hid = -1;
  OCTAVE_LOCAL_BUFFER (hsize_t, hdims, rank);

  // Octave uses column-major, while HDF5 uses row-major ordering
  for (int i = 0; i < rank; i++)
    hdims[i] = dv (rank-i-1);

  space_hid = H5Screate_simple (rank, hdims, 0);

  if (space_hid < 0) return false;
#if HAVE_HDF5_18
  data_hid = H5Dcreate (loc_id, name, save_type_hid, space_hid,
                        H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
#else
  data_hid = H5Dcreate (loc_id, name, save_type_hid, space_hid,
                        H5P_DEFAULT);
#endif
  if (data_hid < 0)
    {
      H5Sclose (space_hid);
      return false;
    }

  retval = H5Dwrite (data_hid, save_type_hid, H5S_ALL, H5S_ALL,
                     H5P_DEFAULT, this->matrix.data ()) >= 0;

  H5Dclose (data_hid);
  H5Sclose (space_hid);

#else
  this->gripe_save ("hdf5");
#endif

  return retval;
}

template <class T>
bool
octave_base_int_matrix<T>::load_hdf5 (octave_hdf5_id loc_id, const char *name)
{
  bool retval = false;

#if defined (HAVE_HDF5)

  hid_t save_type_hid = HDF5_SAVE_TYPE;
  dim_vector dv;
  int empty = load_hdf5_empty (loc_id, name, dv);
  if (empty > 0)
    this->matrix.resize (dv);
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

  T m (dv);
  if (H5Dread (data_hid, save_type_hid, H5S_ALL, H5S_ALL,
               H5P_DEFAULT, m.fortran_vec ()) >= 0)
    {
      retval = true;
      this->matrix = m;
    }

  H5Sclose (space_id);
  H5Dclose (data_hid);

#else
  this->gripe_load ("hdf5");
#endif

  return retval;
}

template <class T>
void
octave_base_int_matrix<T>::print_raw (std::ostream& os,
                                      bool pr_as_read_syntax) const
{
  octave_print_internal (os, this->matrix, pr_as_read_syntax,
                         this->current_print_indent_level ());
}

template <class T>
octave_value
octave_base_int_scalar<T>::convert_to_str_internal (bool, bool, char type) const
{
  octave_value retval;

  T tmp = this->scalar;

  typedef typename T::val_type val_type;

  val_type ival = tmp.value ();

  static const bool is_signed = std::numeric_limits<val_type>::is_signed;
  static const bool can_be_larger_than_uchar_max
    = octave_base_int_helper_traits<val_type>::can_be_larger_than_uchar_max;

  if (octave_base_int_helper<val_type, is_signed,
      can_be_larger_than_uchar_max>::char_value_out_of_range (ival))
    {
      // FIXME: is there something better we could do?

      ival = 0;

      ::warning ("range error for conversion to character value");
    }
  else
    retval = octave_value (std::string (1, static_cast<char> (ival)), type);

  return retval;
}

template <class T>
bool
octave_base_int_scalar<T>::save_ascii (std::ostream& os)
{
  os << this->scalar << "\n";
  return true;
}

template <class T>
bool
octave_base_int_scalar<T>::load_ascii (std::istream& is)
{
  is >> this->scalar;
  if (!is)
    {
      error ("load: failed to load scalar constant");
      return false;
    }
  return true;
}

template <class T>
bool
octave_base_int_scalar<T>::save_binary (std::ostream& os, bool&)
{
  os.write (reinterpret_cast<char *> (&(this->scalar)), this->byte_size ());
  return true;
}

template <class T>
bool
octave_base_int_scalar<T>::load_binary (std::istream& is, bool swap,
                                        oct_mach_info::float_format)
{
  T tmp;
  if (! is.read (reinterpret_cast<char *> (&tmp), this->byte_size ()))
    return false;

  if (swap)
    switch (this->byte_size ())
      {
      case 8:
        swap_bytes<8> (&tmp);
        break;
      case 4:
        swap_bytes<4> (&tmp);
        break;
      case 2:
        swap_bytes<2> (&tmp);
        break;
      case 1:
      default:
        break;
      }
  this->scalar = tmp;
  return true;
}

template <class T>
bool
octave_base_int_scalar<T>::save_hdf5 (octave_hdf5_id loc_id, const char *name, bool)
{
  bool retval = false;

#if defined (HAVE_HDF5)

  hid_t save_type_hid = HDF5_SAVE_TYPE;
  hsize_t dimens[3];
  hid_t space_hid, data_hid;
  space_hid = data_hid = -1;

  space_hid = H5Screate_simple (0, dimens, 0);
  if (space_hid < 0) return false;

#if HAVE_HDF5_18
  data_hid = H5Dcreate (loc_id, name, save_type_hid, space_hid,
                        H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
#else
  data_hid = H5Dcreate (loc_id, name, save_type_hid, space_hid,
                        H5P_DEFAULT);
#endif
  if (data_hid < 0)
    {
      H5Sclose (space_hid);
      return false;
    }

  retval = H5Dwrite (data_hid, save_type_hid, H5S_ALL, H5S_ALL,
                     H5P_DEFAULT, &(this->scalar)) >= 0;

  H5Dclose (data_hid);
  H5Sclose (space_hid);

#else
  this->gripe_save ("hdf5");
#endif

  return retval;
}

template <class T>
bool
octave_base_int_scalar<T>::load_hdf5 (octave_hdf5_id loc_id, const char *name)
{
#if defined (HAVE_HDF5)

  hid_t save_type_hid = HDF5_SAVE_TYPE;
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

  T tmp;
  if (H5Dread (data_hid, save_type_hid, H5S_ALL, H5S_ALL,
               H5P_DEFAULT, &tmp) < 0)
    {
      H5Dclose (data_hid);
      return false;
    }

  this->scalar = tmp;

  H5Dclose (data_hid);

  return true;

#else
  this->gripe_load ("hdf5");
  return false;
#endif
}

