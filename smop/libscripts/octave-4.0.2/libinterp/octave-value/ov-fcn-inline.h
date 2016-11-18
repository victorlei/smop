/*

Copyright (C) 2004-2015 David Bateman

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

#if !defined (octave_ov_fcn_inline_h)
#define octave_ov_fcn_inline_h 1

#include <iosfwd>
#include <string>


#include "ov-base.h"
#include "ov-base-mat.h"
#include "ov-fcn.h"
#include "ov-typeinfo.h"
#include "symtab.h"
#include "ov-fcn-handle.h"

// Inline functions.

class
OCTINTERP_API
octave_fcn_inline : public octave_fcn_handle
{
public:

  octave_fcn_inline (void)
    : octave_fcn_handle (), iftext (), ifargs () { }

  octave_fcn_inline (const std::string& f, const string_vector& a,
                     const std::string& n = std::string ());

  octave_fcn_inline (const octave_fcn_inline& fi)
    : octave_fcn_handle (fi), iftext (fi.iftext), ifargs (fi.ifargs) { }

  ~octave_fcn_inline (void) { }

  octave_base_value *clone (void) const
  { return new octave_fcn_inline (*this); }
  octave_base_value *empty_clone (void) const
  { return new octave_fcn_inline (); }

  bool is_inline_function (void) const { return true; }

  octave_fcn_inline *fcn_inline_value (bool = false) { return this; }

  std::string fcn_text (void) const { return iftext; }

  string_vector fcn_arg_names (void) const { return ifargs; }

  octave_value convert_to_str_internal (bool, bool, char) const;

  octave_map map_value (void) const;

  bool save_ascii (std::ostream& os);

  bool load_ascii (std::istream& is);

  bool save_binary (std::ostream& os, bool& save_as_floats);

  bool load_binary (std::istream& is, bool swap,
                    oct_mach_info::float_format fmt);

  bool save_hdf5 (octave_hdf5_id loc_id, const char *name, bool save_as_floats);

  bool load_hdf5 (octave_hdf5_id loc_id, const char *name);

  void print (std::ostream& os, bool pr_as_read_syntax = false);

  void print_raw (std::ostream& os, bool pr_as_read_syntax = false) const;

private:


  DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA

  // The expression of an inline function.
  std::string iftext;

  // The args of an inline function.
  string_vector ifargs;
};

#endif
