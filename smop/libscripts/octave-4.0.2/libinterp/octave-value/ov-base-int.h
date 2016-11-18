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

#if !defined (octave_ov_base_int_h)
#define octave_ov_base_int_h 1

#include <cstdlib>

#include <iosfwd>
#include <string>

#include "mx-base.h"
#include "str-vec.h"

#include "error.h"
#include "ov-base.h"
#include "ov-base-mat.h"
#include "ov-base-scalar.h"
#include "ov-typeinfo.h"

// base int matrix values.

template <class T>
class
octave_base_int_matrix : public octave_base_matrix<T>
{
public:

  octave_base_int_matrix (void) : octave_base_matrix<T> () { }

  octave_base_int_matrix (const T& nda) : octave_base_matrix<T> (nda) { }

  ~octave_base_int_matrix (void) { }

  octave_base_value *clone (void) const
  { return new octave_base_int_matrix (*this); }

  octave_base_value *empty_clone (void) const
  { return new octave_base_int_matrix (); }

  octave_base_value *try_narrowing_conversion (void);

  bool is_real_type (void) const { return true; }

  //  void increment (void) { matrix += 1; }

  //  void decrement (void) { matrix -= 1; }

  void print_raw (std::ostream& os, bool pr_as_read_syntax = false) const;

  octave_value convert_to_str_internal (bool, bool, char type) const;

  bool save_ascii (std::ostream& os);

  bool load_ascii (std::istream& is);

  bool save_binary (std::ostream& os, bool&);

  bool load_binary (std::istream& is, bool swap,
                    oct_mach_info::float_format);

  bool save_hdf5 (octave_hdf5_id loc_id, const char *name, bool);

  bool load_hdf5 (octave_hdf5_id loc_id, const char *name);
};

// base int scalar values.

template <class T>
class
octave_base_int_scalar : public octave_base_scalar<T>
{
public:

  octave_base_int_scalar (void) : octave_base_scalar<T> () { }

  octave_base_int_scalar (const T& s) : octave_base_scalar<T> (s) { }

  ~octave_base_int_scalar (void) { }

  octave_base_value *clone (void) const
  { return new octave_base_int_scalar (*this); }
  octave_base_value *empty_clone (void) const
  { return new octave_base_int_scalar (); }

  octave_base_value *try_narrowing_conversion (void) { return 0; }

  bool is_real_type (void) const { return true; }

  bool is_real_scalar (void) const { return true; }

  //  void increment (void) { scalar += 1; }

  //  void decrement (void) { scalar -= 1; }

  octave_value convert_to_str_internal (bool, bool, char type) const;

  bool save_ascii (std::ostream& os);

  bool load_ascii (std::istream& is);

  bool save_binary (std::ostream& os, bool&);

  bool load_binary (std::istream& is, bool swap,
                    oct_mach_info::float_format);

  bool save_hdf5 (octave_hdf5_id loc_id, const char *name, bool);

  bool load_hdf5 (octave_hdf5_id loc_id, const char *name);
};

#endif
