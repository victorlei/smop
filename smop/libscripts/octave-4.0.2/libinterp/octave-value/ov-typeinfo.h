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

#if !defined (octave_ov_typeinfo_h)
#define octave_ov_typeinfo_h 1

#include <string>

#include "Array.h"

#include "ov.h"

class string_vector;

class
OCTINTERP_API
octave_value_typeinfo
{
public:

  typedef octave_value (*unary_class_op_fcn) (const octave_value&);

  typedef octave_value (*unary_op_fcn) (const octave_base_value&);

  typedef void (*non_const_unary_op_fcn) (octave_base_value&);

  typedef octave_value (*binary_class_op_fcn)
    (const octave_value&, const octave_value&);

  typedef octave_value (*binary_op_fcn)
    (const octave_base_value&, const octave_base_value&);

  typedef octave_value (*cat_op_fcn)
    (octave_base_value&, const octave_base_value&,
     const Array<octave_idx_type>& ra_idx);

  typedef octave_value (*assign_op_fcn)
    (octave_base_value&, const octave_value_list&, const octave_base_value&);

  typedef octave_value (*assignany_op_fcn)
    (octave_base_value&, const octave_value_list&, const octave_value&);

  static bool instance_ok (void);

  static int register_type (const std::string&, const std::string&,
                            const octave_value&);

  static bool register_unary_class_op (octave_value::unary_op,
                                       unary_class_op_fcn);

  static bool register_unary_op (octave_value::unary_op, int, unary_op_fcn);

  static bool register_non_const_unary_op (octave_value::unary_op, int,
                                           non_const_unary_op_fcn);

  static bool register_binary_class_op (octave_value::binary_op,
                                        binary_class_op_fcn);

  static bool register_binary_op (octave_value::binary_op, int, int,
                                  binary_op_fcn);

  static bool register_binary_class_op (octave_value::compound_binary_op,
                                        binary_class_op_fcn);

  static bool register_binary_op (octave_value::compound_binary_op, int, int,
                                  binary_op_fcn);

  static bool register_cat_op (int, int, cat_op_fcn);

  static bool register_assign_op (octave_value::assign_op, int, int,
                                  assign_op_fcn);

  static bool register_assignany_op (octave_value::assign_op, int,
                                     assignany_op_fcn);

  static bool register_pref_assign_conv (int, int, int);

  static bool
  register_type_conv_op (int, int, octave_base_value::type_conv_fcn);

  static bool
  register_widening_op (int, int, octave_base_value::type_conv_fcn);

  static octave_value
  lookup_type (const std::string& nm)
  {
    return instance->do_lookup_type (nm);
  }

  static unary_class_op_fcn
  lookup_unary_class_op (octave_value::unary_op op)
  {
    return instance->do_lookup_unary_class_op (op);
  }

  static unary_op_fcn
  lookup_unary_op (octave_value::unary_op op, int t)
  {
    return instance->do_lookup_unary_op (op, t);
  }

  static non_const_unary_op_fcn
  lookup_non_const_unary_op (octave_value::unary_op op, int t)
  {
    return instance->do_lookup_non_const_unary_op (op, t);
  }

  static binary_class_op_fcn
  lookup_binary_class_op (octave_value::binary_op op)
  {
    return instance->do_lookup_binary_class_op (op);
  }

  static binary_op_fcn
  lookup_binary_op (octave_value::binary_op op, int t1, int t2)
  {
    return instance->do_lookup_binary_op (op, t1, t2);
  }

  static binary_class_op_fcn
  lookup_binary_class_op (octave_value::compound_binary_op op)
  {
    return instance->do_lookup_binary_class_op (op);
  }

  static binary_op_fcn
  lookup_binary_op (octave_value::compound_binary_op op, int t1, int t2)
  {
    return instance->do_lookup_binary_op (op, t1, t2);
  }

  static cat_op_fcn
  lookup_cat_op (int t1, int t2)
  {
    return instance->do_lookup_cat_op (t1, t2);
  }

  static assign_op_fcn
  lookup_assign_op (octave_value::assign_op op, int t_lhs, int t_rhs)
  {
    return instance->do_lookup_assign_op (op, t_lhs, t_rhs);
  }

  static assignany_op_fcn
  lookup_assignany_op (octave_value::assign_op op, int t_lhs)
  {
    return instance->do_lookup_assignany_op (op, t_lhs);
  }

  static int
  lookup_pref_assign_conv (int t_lhs, int t_rhs)
  {
    return instance->do_lookup_pref_assign_conv (t_lhs, t_rhs);
  }

  static octave_base_value::type_conv_fcn
  lookup_type_conv_op (int t, int t_result)
  {
    return instance->do_lookup_type_conv_op (t, t_result);
  }

  static octave_base_value::type_conv_fcn
  lookup_widening_op (int t, int t_result)
  {
    return instance->do_lookup_widening_op (t, t_result);
  }

  static string_vector installed_type_names (void)
  {
    return instance->do_installed_type_names ();
  }

protected:

  octave_value_typeinfo (void)
    : num_types (0), types (dim_vector (init_tab_sz, 1), std::string ()),
      vals (dim_vector (init_tab_sz, 1)),
      unary_class_ops (dim_vector (octave_value::num_unary_ops, 1), 0),
      unary_ops (dim_vector (octave_value::num_unary_ops, init_tab_sz), 0),
      non_const_unary_ops (dim_vector (octave_value::num_unary_ops, init_tab_sz), 0),
      binary_class_ops (dim_vector (octave_value::num_binary_ops, 1), 0),
      binary_ops (dim_vector (octave_value::num_binary_ops, init_tab_sz, init_tab_sz), 0),
      compound_binary_class_ops (dim_vector (octave_value::num_compound_binary_ops, 1), 0),
      compound_binary_ops (dim_vector (octave_value::num_compound_binary_ops, init_tab_sz, init_tab_sz), 0),
      cat_ops (dim_vector (init_tab_sz, init_tab_sz), 0),
      assign_ops (dim_vector (octave_value::num_assign_ops, init_tab_sz, init_tab_sz), 0),
      assignany_ops (dim_vector (octave_value::num_assign_ops, init_tab_sz), 0),
      pref_assign_conv (dim_vector (init_tab_sz, init_tab_sz), -1),
      type_conv_ops (dim_vector (init_tab_sz, init_tab_sz), 0),
      widening_ops (dim_vector (init_tab_sz, init_tab_sz), 0)  { }

  ~octave_value_typeinfo (void) { }

private:

  static const int init_tab_sz;

  static octave_value_typeinfo *instance;

  static void cleanup_instance (void) { delete instance; instance = 0; }

  int num_types;

  Array<std::string> types;

  Array<octave_value> vals;

  Array<void *> unary_class_ops;

  Array<void *> unary_ops;

  Array<void *> non_const_unary_ops;

  Array<void *> binary_class_ops;

  Array<void *> binary_ops;

  Array<void *> compound_binary_class_ops;

  Array<void *> compound_binary_ops;

  Array<void *> cat_ops;

  Array<void *> assign_ops;

  Array<void *> assignany_ops;

  Array<int> pref_assign_conv;

  Array<void *> type_conv_ops;

  Array<void *> widening_ops;

  int do_register_type (const std::string&, const std::string&,
                        const octave_value&);

  bool do_register_unary_class_op (octave_value::unary_op, unary_class_op_fcn);

  bool do_register_unary_op (octave_value::unary_op, int, unary_op_fcn);

  bool do_register_non_const_unary_op (octave_value::unary_op, int,
                                       non_const_unary_op_fcn);

  bool do_register_binary_class_op (octave_value::binary_op,
                                    binary_class_op_fcn);

  bool do_register_binary_op (octave_value::binary_op, int, int,
                              binary_op_fcn);

  bool do_register_binary_class_op (octave_value::compound_binary_op,
                                    binary_class_op_fcn);

  bool do_register_binary_op (octave_value::compound_binary_op, int, int,
                              binary_op_fcn);

  bool do_register_cat_op (int, int, cat_op_fcn);

  bool do_register_assign_op (octave_value::assign_op, int, int,
                              assign_op_fcn);

  bool do_register_assignany_op (octave_value::assign_op, int,
                                 assignany_op_fcn);

  bool do_register_pref_assign_conv (int, int, int);

  bool do_register_type_conv_op (int, int, octave_base_value::type_conv_fcn);

  bool do_register_widening_op (int, int, octave_base_value::type_conv_fcn);

  octave_value do_lookup_type (const std::string& nm);

  unary_class_op_fcn do_lookup_unary_class_op (octave_value::unary_op);

  unary_op_fcn do_lookup_unary_op (octave_value::unary_op, int);

  non_const_unary_op_fcn do_lookup_non_const_unary_op
    (octave_value::unary_op, int);

  binary_class_op_fcn do_lookup_binary_class_op (octave_value::binary_op);

  binary_op_fcn do_lookup_binary_op (octave_value::binary_op, int, int);

  binary_class_op_fcn do_lookup_binary_class_op (octave_value::compound_binary_op);

  binary_op_fcn do_lookup_binary_op (octave_value::compound_binary_op,
                                     int, int);

  cat_op_fcn do_lookup_cat_op (int, int);

  assign_op_fcn do_lookup_assign_op (octave_value::assign_op, int, int);

  assignany_op_fcn do_lookup_assignany_op (octave_value::assign_op, int);

  int do_lookup_pref_assign_conv (int, int);

  octave_base_value::type_conv_fcn do_lookup_type_conv_op (int, int);

  octave_base_value::type_conv_fcn do_lookup_widening_op (int, int);

  string_vector do_installed_type_names (void);

  // No copying!

  octave_value_typeinfo (const octave_value_typeinfo&);

  octave_value_typeinfo& operator = (const octave_value_typeinfo&);
};

#endif
