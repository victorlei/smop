/*

Copyright (C) 2003-2015 John W. Eaton
Copyright (C) 2009 VZLU Prague

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

#if !defined (octave_ov_fcn_handle_h)
#define octave_ov_fcn_handle_h 1

#include <iosfwd>
#include <string>
#include <memory>


#include "ov-base.h"
#include "ov-base-mat.h"
#include "ov-fcn.h"
#include "ov-typeinfo.h"

// Function handles.

class
OCTINTERP_API
octave_fcn_handle : public octave_base_value
{
private:

  typedef std::map<std::string, octave_value> str_ov_map;

public:

  static const std::string anonymous;

  octave_fcn_handle (void)
    : fcn (), nm (), has_overloads (false), overloads () { }

  octave_fcn_handle (const std::string& n)
    : fcn (), nm (n), has_overloads (false), overloads () { }

  octave_fcn_handle (const octave_value& f,  const std::string& n = anonymous);

  octave_fcn_handle (const octave_fcn_handle& fh)
    : octave_base_value (fh), fcn (fh.fcn), nm (fh.nm),
      has_overloads (fh.has_overloads), overloads ()
  {
    for (int i = 0; i < btyp_num_types; i++)
      builtin_overloads[i] = fh.builtin_overloads[i];

    overloads = fh.overloads;
  }

  ~octave_fcn_handle (void) { }

  octave_base_value *clone (void) const
  { return new octave_fcn_handle (*this); }
  octave_base_value *empty_clone (void) const
  { return new octave_fcn_handle (); }

  octave_value subsref (const std::string& type,
                        const std::list<octave_value_list>& idx)
  {
    octave_value_list tmp = subsref (type, idx, 1);
    return tmp.length () > 0 ? tmp(0) : octave_value ();
  }

  octave_value_list subsref (const std::string& type,
                             const std::list<octave_value_list>& idx,
                             int nargout);

  octave_value_list subsref (const std::string& type,
                             const std::list<octave_value_list>& idx,
                             int nargout,
                             const std::list<octave_lvalue>* lvalue_list);

  octave_value_list
  do_multi_index_op (int nargout, const octave_value_list& args);

  octave_value_list
  do_multi_index_op (int nargout, const octave_value_list& args,
                     const std::list<octave_lvalue>* lvalue_list);

  bool is_defined (void) const { return true; }

  bool is_function_handle (void) const { return true; }

  builtin_type_t builtin_type (void) const { return btyp_func_handle; }

  bool is_overloaded (void) const { return has_overloads; }

  dim_vector dims (void) const;

  octave_function *function_value (bool = false)
  { return fcn.function_value (); }

  octave_user_function *user_function_value (bool = false)
  { return fcn.user_function_value (); }

  octave_fcn_handle *fcn_handle_value (bool = false) { return this; }

  octave_value fcn_val (void) const { return fcn; }

  std::string fcn_name (void) const { return nm; }

  void set_overload (builtin_type_t btyp, const octave_value& ov_fcn)
  {
    if (btyp != btyp_unknown)
      {
        has_overloads = true;
        builtin_overloads[btyp] = ov_fcn;
      }

  }

  void set_overload (const std::string& dispatch_type,
                     const octave_value& ov_fcn)
  {
    has_overloads = true;
    overloads[dispatch_type] = ov_fcn;
  }

  bool is_equal_to (const octave_fcn_handle&) const;

  bool save_ascii (std::ostream& os);

  bool load_ascii (std::istream& is);

  bool save_binary (std::ostream& os, bool& save_as_floats);

  bool load_binary (std::istream& is, bool swap,
                    oct_mach_info::float_format fmt);

  bool save_hdf5 (octave_hdf5_id loc_id, const char *name, bool save_as_floats);

  bool load_hdf5 (octave_hdf5_id loc_id, const char *name);

  void print (std::ostream& os, bool pr_as_read_syntax = false);

  void print_raw (std::ostream& os, bool pr_as_read_syntax = false) const;

  // Simple function handles are printed without a newline.
  bool print_as_scalar (void) const { return nm != anonymous; }

private:

  bool set_fcn (const std::string &octaveroot, const std::string& fpath);


  DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA

protected:

  // The function we are handling.
  octave_value fcn;

  // The name of the handle, including the "@".
  std::string nm;

  // Whether the function is overloaded at all.
  bool has_overloads;

  // Overloads for builtin types. We use array to make lookup faster.
  octave_value builtin_overloads[btyp_num_types];

  // Overloads for other classes.
  str_ov_map overloads;

  friend octave_value make_fcn_handle (const std::string &, bool);
};

extern octave_value make_fcn_handle (const std::string& nm,
                                     bool local_funcs = true);

class
OCTINTERP_API
octave_fcn_binder : public octave_fcn_handle
{
private:
  // Private ctor.
  octave_fcn_binder (const octave_value& f, const octave_value& root,
                     const octave_value_list& templ,
                     const std::vector<int>& mask, int exp_nargin);

public:

  // Factory method.
  static octave_fcn_handle *maybe_binder (const octave_value& f);

  octave_value_list
  do_multi_index_op (int nargout, const octave_value_list& args);

  octave_value_list
  do_multi_index_op (int nargout, const octave_value_list& args,
                     const std::list<octave_lvalue>* lvalue_list);

protected:

  octave_value root_handle;
  octave_value_list arg_template;
  std::vector<int> arg_mask;
  int expected_nargin;
};
#endif
