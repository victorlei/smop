/*

Copyright (C) 2003-2015 John W. Eaton

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

#if !defined (octave_ls_hdf5_h)
#define octave_ls_hdf5_h 1

#if defined (HAVE_HDF5)

#include "oct-hdf5.h"

// first, we need to define our own dummy stream subclass, since
// HDF5 needs to do its own file i/o

// hdf5_fstreambase is used for both input and output streams, modeled
// on the fstreambase class in <fstream.h>

class hdf5_fstreambase : virtual public std::ios
{
public:

  // HDF5 uses an "id" to refer to an open file
  hid_t file_id;

  // keep track of current item index in the file
  int current_item;

  hdf5_fstreambase () : file_id (-1), current_item () { }

  ~hdf5_fstreambase () { close (); }

  hdf5_fstreambase (const char *name, int mode, int /* prot */ = 0)
    : file_id (-1), current_item (-1)
  {
    if (mode & std::ios::in)
      file_id = H5Fopen (name, H5F_ACC_RDONLY, H5P_DEFAULT);
    else if (mode & std::ios::out)
      {
        if (mode & std::ios::app && H5Fis_hdf5 (name) > 0)
          file_id = H5Fopen (name, H5F_ACC_RDWR, H5P_DEFAULT);
        else
          file_id = H5Fcreate (name, H5F_ACC_TRUNC, H5P_DEFAULT,
                               H5P_DEFAULT);
      }
    if (file_id < 0)
      std::ios::setstate (std::ios::badbit);

    current_item = 0;
  }

  void close ()
  {
    if (file_id >= 0)
      {
        if (H5Fclose (file_id) < 0)
          std::ios::setstate (std::ios::badbit);
        file_id = -1;
      }
  }

  void open (const char *name, int mode, int)
  {
    clear ();

    if (mode & std::ios::in)
      file_id = H5Fopen (name, H5F_ACC_RDONLY, H5P_DEFAULT);
    else if (mode & std::ios::out)
      {
        if (mode & std::ios::app && H5Fis_hdf5 (name) > 0)
          file_id = H5Fopen (name, H5F_ACC_RDWR, H5P_DEFAULT);
        else
          file_id = H5Fcreate (name, H5F_ACC_TRUNC, H5P_DEFAULT,
                               H5P_DEFAULT);
      }
    if (file_id < 0)
      std::ios::setstate (std::ios::badbit);

    current_item = 0;
  }
};

// input and output streams, subclassing istream and ostream
// so that we can pass them for stream parameters in the functions below.

class hdf5_ifstream : public hdf5_fstreambase, public std::istream
{
public:

  hdf5_ifstream () : hdf5_fstreambase (), std::istream (0) { }

  hdf5_ifstream (const char *name, int mode = std::ios::in|std::ios::binary,
                 int prot = 0)
    : hdf5_fstreambase (name, mode, prot), std::istream (0) { }

  void open (const char *name, int mode = std::ios::in|std::ios::binary,
             int prot = 0)
  { hdf5_fstreambase::open (name, mode, prot); }
};

class hdf5_ofstream : public hdf5_fstreambase, public std::ostream
{
public:

  hdf5_ofstream () : hdf5_fstreambase (), std::ostream (0) { }

  hdf5_ofstream (const char *name, int mode = std::ios::out|std::ios::binary,
                 int prot = 0)
    : hdf5_fstreambase (name, mode, prot), std::ostream (0) { }

  void open (const char *name, int mode = std::ios::out|std::ios::binary,
             int prot = 0)
  { hdf5_fstreambase::open (name, mode, prot); }
};

// Callback data structure for passing data to hdf5_read_next_data, below.

struct
hdf5_callback_data
{
  hdf5_callback_data (void)
    : name (), global (false), tc (), doc () { }

  // the following fields are set by hdf5_read_data on successful return:

  // the name of the variable
  std::string name;

  // whether it is global
  bool global;

  // the value of the variable, in Octave form
  octave_value tc;

  // a documentation string (NULL if none)
  std::string doc;
};

#if HAVE_HDF5_INT2FLOAT_CONVERSIONS
extern OCTINTERP_API hid_t
save_type_to_hdf5 (save_type st)
#endif

extern OCTINTERP_API hid_t
hdf5_make_complex_type (hid_t num_type);

extern OCTINTERP_API bool
hdf5_types_compatible (hid_t t1, hid_t t2);

extern OCTINTERP_API herr_t
hdf5_read_next_data (hid_t group_id, const char *name, void *dv);

extern OCTINTERP_API bool
add_hdf5_data (hid_t loc_id, const octave_value& tc,
               const std::string& name, const std::string& doc,
               bool mark_as_global, bool save_as_floats);

extern OCTINTERP_API int
save_hdf5_empty (hid_t loc_id, const char *name, const dim_vector d);

extern OCTINTERP_API int
load_hdf5_empty (hid_t loc_id, const char *name, dim_vector &d);

extern OCTINTERP_API std::string
read_hdf5_data (std::istream& is,  const std::string& filename, bool& global,
                octave_value& tc, std::string& doc,
                const string_vector& argv, int argv_idx, int argc);

extern OCTINTERP_API bool
save_hdf5_data (std::ostream& os, const octave_value& tc,
                const std::string& name, const std::string& doc,
                bool mark_as_global, bool save_as_floats);

extern OCTINTERP_API bool
hdf5_check_attr (hid_t loc_id, const char *attr_name);

extern OCTINTERP_API bool
hdf5_get_scalar_attr (hid_t loc_id, hid_t type_id, const char *attr_name,
                      void *buf);

extern OCTINTERP_API herr_t
hdf5_add_attr (hid_t loc_id, const char *attr_name);


extern OCTINTERP_API herr_t
hdf5_add_scalar_attr (hid_t loc_id, hid_t type_id,
                      const char *attr_name, void *buf);

#ifdef USE_64_BIT_IDX_T
#define H5T_NATIVE_IDX H5T_NATIVE_INT64
#else
#define H5T_NATIVE_IDX H5T_NATIVE_INT
#endif

#endif

#endif
