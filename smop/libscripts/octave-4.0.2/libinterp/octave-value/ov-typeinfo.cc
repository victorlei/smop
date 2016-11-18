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

#include "Array.h"
#include "singleton-cleanup.h"

#include "defun.h"
#include "error.h"
#include "ov-typeinfo.h"

const int
octave_value_typeinfo::init_tab_sz (16);

octave_value_typeinfo *
octave_value_typeinfo::instance (0);

bool
octave_value_typeinfo::instance_ok (void)
{
  bool retval = true;

  if (! instance)
    {
      instance = new octave_value_typeinfo ();

      if (instance)
        singleton_cleanup_list::add (cleanup_instance);
    }

  if (! instance)
    {
      ::error ("unable to create value type info object!");

      retval = false;
    }

  return retval;
}

int
octave_value_typeinfo::register_type (const std::string& t_name,
                                      const std::string& c_name,
                                      const octave_value& val)
{
  return (instance_ok ())
         ? instance->do_register_type (t_name, c_name, val) : -1;
}

bool
octave_value_typeinfo::register_unary_class_op (octave_value::unary_op op,
                                                octave_value_typeinfo::unary_class_op_fcn f)
{
  return (instance_ok ())
         ? instance->do_register_unary_class_op (op, f) : false;
}

bool
octave_value_typeinfo::register_unary_op (octave_value::unary_op op,
                                          int t,
                                          octave_value_typeinfo::unary_op_fcn f)
{
  return (instance_ok ())
         ? instance->do_register_unary_op (op, t, f) : false;
}

bool
octave_value_typeinfo::register_non_const_unary_op (octave_value::unary_op op,
                                                    int t,
                                                    octave_value_typeinfo::non_const_unary_op_fcn f)
{
  return (instance_ok ())
         ? instance->do_register_non_const_unary_op (op, t, f) : false;
}

bool
octave_value_typeinfo::register_binary_class_op (octave_value::binary_op op,
                                                 octave_value_typeinfo::binary_class_op_fcn f)
{
  return (instance_ok ())
         ? instance->do_register_binary_class_op (op, f) : false;
}

bool
octave_value_typeinfo::register_binary_op (octave_value::binary_op op,
                                           int t1, int t2,
                                           octave_value_typeinfo::binary_op_fcn f)
{
  return (instance_ok ())
         ? instance->do_register_binary_op (op, t1, t2, f) : false;
}

bool
octave_value_typeinfo::register_binary_class_op (octave_value::compound_binary_op op,
                                                 octave_value_typeinfo::binary_class_op_fcn f)
{
  return (instance_ok ())
         ? instance->do_register_binary_class_op (op, f) : false;
}

bool
octave_value_typeinfo::register_binary_op (octave_value::compound_binary_op op,
                                           int t1, int t2,
                                           octave_value_typeinfo::binary_op_fcn f)
{
  return (instance_ok ())
         ? instance->do_register_binary_op (op, t1, t2, f) : false;
}

bool
octave_value_typeinfo::register_cat_op (int t1, int t2,
                                        octave_value_typeinfo::cat_op_fcn f)
{
  return (instance_ok ())
         ? instance->do_register_cat_op (t1, t2, f) : false;
}

bool
octave_value_typeinfo::register_assign_op (octave_value::assign_op op,
                                           int t_lhs, int t_rhs,
                                           octave_value_typeinfo::assign_op_fcn f)
{
  return (instance_ok ())
         ? instance->do_register_assign_op (op, t_lhs, t_rhs, f) : -1;
}

bool
octave_value_typeinfo::register_assignany_op (octave_value::assign_op op,
                                              int t_lhs, octave_value_typeinfo::assignany_op_fcn f)
{
  return (instance_ok ())
         ? instance->do_register_assignany_op (op, t_lhs, f) : -1;
}

bool
octave_value_typeinfo::register_pref_assign_conv (int t_lhs, int t_rhs,
                                                  int t_result)
{
  return (instance_ok ())
         ? instance->do_register_pref_assign_conv (t_lhs, t_rhs, t_result)
         : false;
}

bool
octave_value_typeinfo::register_type_conv_op (int t, int t_result,
                                              octave_base_value::type_conv_fcn f)
{
  return (instance_ok ())
         ? instance->do_register_type_conv_op (t, t_result, f) : false;
}

bool
octave_value_typeinfo::register_widening_op (int t, int t_result,
                                             octave_base_value::type_conv_fcn f)
{
  return (instance_ok ())
         ? instance->do_register_widening_op (t, t_result, f) : false;
}

// FIXME: we should also store all class names and provide a
// way to list them (calling class with nargin == 0?).

int
octave_value_typeinfo::do_register_type (const std::string& t_name,
                                         const std::string& /* c_name */,
                                         const octave_value& val)
{
  int i = 0;

  for (i = 0; i < num_types; i++)
    if (t_name == types (i))
      return i;

  int len = types.length ();

  if (i == len)
    {
      len *= 2;

      types.resize (dim_vector (len, 1), std::string ());

      vals.resize (dim_vector (len, 1), octave_value ());

      unary_ops.resize (dim_vector (octave_value::num_unary_ops, len), 0);

      non_const_unary_ops.resize
        (dim_vector (octave_value::num_unary_ops, len), 0);

      binary_ops.resize
        (dim_vector (octave_value::num_binary_ops, len, len), 0);

      compound_binary_ops.resize
        (dim_vector (octave_value::num_compound_binary_ops, len, len), 0);

      cat_ops.resize (dim_vector (len, len), 0);

      assign_ops.resize
        (dim_vector (octave_value::num_assign_ops, len, len), 0);

      assignany_ops.resize
        (dim_vector (octave_value::num_assign_ops, len), 0);

      pref_assign_conv.resize (dim_vector (len, len), -1);

      type_conv_ops.resize (dim_vector (len, len), 0);

      widening_ops.resize (dim_vector (len, len), 0);
    }

  types (i) = t_name;

  vals (i) = val;

  num_types++;

  return i;
}

bool
octave_value_typeinfo::do_register_unary_class_op (octave_value::unary_op op,
                                                   octave_value_typeinfo::unary_class_op_fcn f)
{
  if (lookup_unary_class_op (op))
    {
      std::string op_name = octave_value::unary_op_as_string (op);

      warning ("duplicate unary operator '%s' for class dispatch",
               op_name.c_str ());
    }

  unary_class_ops.checkelem (static_cast<int> (op))
    = reinterpret_cast<void *> (f);

  return false;
}

bool
octave_value_typeinfo::do_register_unary_op (octave_value::unary_op op,
                                             int t,
                                             octave_value_typeinfo::unary_op_fcn f)
{
  if (lookup_unary_op (op, t))
    {
      std::string op_name = octave_value::unary_op_as_string (op);
      std::string type_name = types(t);

      warning ("duplicate unary operator '%s' for type '%s'",
               op_name.c_str (), type_name.c_str ());
    }

  unary_ops.checkelem (static_cast<int> (op), t) = reinterpret_cast<void *> (f);

  return false;
}

bool
octave_value_typeinfo::do_register_non_const_unary_op
  (octave_value::unary_op op, int t,
   octave_value_typeinfo::non_const_unary_op_fcn f)
{
  if (lookup_non_const_unary_op (op, t))
    {
      std::string op_name = octave_value::unary_op_as_string (op);
      std::string type_name = types(t);

      warning ("duplicate unary operator '%s' for type '%s'",
               op_name.c_str (), type_name.c_str ());
    }

  non_const_unary_ops.checkelem (static_cast<int> (op), t)
    = reinterpret_cast<void *> (f);

  return false;
}

bool
octave_value_typeinfo::do_register_binary_class_op (octave_value::binary_op op,
                                                    octave_value_typeinfo::binary_class_op_fcn f)
{
  if (lookup_binary_class_op (op))
    {
      std::string op_name = octave_value::binary_op_as_string (op);

      warning ("duplicate binary operator '%s' for class dispatch",
               op_name.c_str ());
    }

  binary_class_ops.checkelem (static_cast<int> (op))
    = reinterpret_cast<void *> (f);

  return false;
}

bool
octave_value_typeinfo::do_register_binary_op (octave_value::binary_op op,
                                              int t1, int t2,
                                              octave_value_typeinfo::binary_op_fcn f)
{
  if (lookup_binary_op (op, t1, t2))
    {
      std::string op_name = octave_value::binary_op_as_string (op);
      std::string t1_name = types(t1);
      std::string t2_name = types(t2);

      warning ("duplicate binary operator '%s' for types '%s' and '%s'",
               op_name.c_str (), t1_name.c_str (), t1_name.c_str ());
    }

  binary_ops.checkelem (static_cast<int> (op), t1, t2)
    = reinterpret_cast<void *> (f);

  return false;
}

bool
octave_value_typeinfo::do_register_binary_class_op (octave_value::compound_binary_op op,
                                                    octave_value_typeinfo::binary_class_op_fcn f)
{
  if (lookup_binary_class_op (op))
    {
      std::string op_name = octave_value::binary_op_fcn_name (op);

      warning ("duplicate compound binary operator '%s' for class dispatch",
               op_name.c_str ());
    }

  compound_binary_class_ops.checkelem (static_cast<int> (op))
    = reinterpret_cast<void *> (f);

  return false;
}

bool
octave_value_typeinfo::do_register_binary_op (octave_value::compound_binary_op op,
                                              int t1, int t2,
                                              octave_value_typeinfo::binary_op_fcn f)
{
  if (lookup_binary_op (op, t1, t2))
    {
      std::string op_name = octave_value::binary_op_fcn_name (op);
      std::string t1_name = types(t1);
      std::string t2_name = types(t2);

      warning ("duplicate compound binary operator '%s' for types '%s' and '%s'",
               op_name.c_str (), t1_name.c_str (), t1_name.c_str ());
    }

  compound_binary_ops.checkelem (static_cast<int> (op), t1, t2)
    = reinterpret_cast<void *> (f);

  return false;
}

bool
octave_value_typeinfo::do_register_cat_op (int t1, int t2,
                                           octave_value_typeinfo::cat_op_fcn f)
{
  if (lookup_cat_op (t1, t2))
    {
      std::string t1_name = types(t1);
      std::string t2_name = types(t2);

      warning ("duplicate concatenation operator for types '%s' and '%s'",
               t1_name.c_str (), t1_name.c_str ());
    }

  cat_ops.checkelem (t1, t2) = reinterpret_cast<void *> (f);

  return false;
}

bool
octave_value_typeinfo::do_register_assign_op (octave_value::assign_op op,
                                              int t_lhs, int t_rhs,
                                              octave_value_typeinfo::assign_op_fcn f)
{
  if (lookup_assign_op (op, t_lhs, t_rhs))
    {
      std::string op_name = octave_value::assign_op_as_string (op);
      std::string t_lhs_name = types(t_lhs);
      std::string t_rhs_name = types(t_rhs);

      warning ("duplicate assignment operator '%s' for types '%s' and '%s'",
               op_name.c_str (), t_lhs_name.c_str (), t_rhs_name.c_str ());
    }

  assign_ops.checkelem (static_cast<int> (op), t_lhs, t_rhs)
    = reinterpret_cast<void *> (f);

  return false;
}

bool
octave_value_typeinfo::do_register_assignany_op (octave_value::assign_op op,
                                                 int t_lhs, octave_value_typeinfo::assignany_op_fcn f)
{
  if (lookup_assignany_op (op, t_lhs))
    {
      std::string op_name = octave_value::assign_op_as_string (op);
      std::string t_lhs_name = types(t_lhs);

      warning ("duplicate assignment operator '%s' for types '%s'",
               op_name.c_str (), t_lhs_name.c_str ());
    }

  assignany_ops.checkelem (static_cast<int> (op), t_lhs)
    = reinterpret_cast<void *> (f);

  return false;
}

bool
octave_value_typeinfo::do_register_pref_assign_conv (int t_lhs, int t_rhs,
                                                     int t_result)
{
  if (lookup_pref_assign_conv (t_lhs, t_rhs) >= 0)
    {
      std::string t_lhs_name = types(t_lhs);
      std::string t_rhs_name = types(t_rhs);

      warning ("overriding assignment conversion for types '%s' and '%s'",
               t_lhs_name.c_str (), t_rhs_name.c_str ());
    }

  pref_assign_conv.checkelem (t_lhs, t_rhs) = t_result;

  return false;
}

bool
octave_value_typeinfo::do_register_type_conv_op
  (int t, int t_result, octave_base_value::type_conv_fcn f)
{
  if (lookup_type_conv_op (t, t_result))
    {
      std::string t_name = types(t);
      std::string t_result_name = types(t_result);

      warning ("overriding type conversion op for '%s' to '%s'",
               t_name.c_str (), t_result_name.c_str ());
    }

  type_conv_ops.checkelem (t, t_result) = reinterpret_cast<void *> (f);

  return false;
}

bool
octave_value_typeinfo::do_register_widening_op
  (int t, int t_result, octave_base_value::type_conv_fcn f)
{
  if (lookup_widening_op (t, t_result))
    {
      std::string t_name = types(t);
      std::string t_result_name = types(t_result);

      warning ("overriding widening op for '%s' to '%s'",
               t_name.c_str (), t_result_name.c_str ());
    }

  widening_ops.checkelem (t, t_result) = reinterpret_cast<void *> (f);

  return false;
}

octave_value
octave_value_typeinfo::do_lookup_type (const std::string& nm)
{
  octave_value retval;

  for (int i = 0; i < num_types; i++)
    {
      if (nm == types(i))
        {
          retval = vals(i);
          retval.make_unique ();
          break;
        }
    }

  return retval;
}

octave_value_typeinfo::unary_class_op_fcn
octave_value_typeinfo::do_lookup_unary_class_op (octave_value::unary_op op)
{
  void *f = unary_class_ops.checkelem (static_cast<int> (op));
  return reinterpret_cast<octave_value_typeinfo::unary_class_op_fcn> (f);
}

octave_value_typeinfo::unary_op_fcn
octave_value_typeinfo::do_lookup_unary_op (octave_value::unary_op op, int t)
{
  void *f = unary_ops.checkelem (static_cast<int> (op), t);
  return reinterpret_cast<octave_value_typeinfo::unary_op_fcn> (f);
}

octave_value_typeinfo::non_const_unary_op_fcn
octave_value_typeinfo::do_lookup_non_const_unary_op
  (octave_value::unary_op op, int t)
{
  void *f = non_const_unary_ops.checkelem (static_cast<int> (op), t);
  return reinterpret_cast<octave_value_typeinfo::non_const_unary_op_fcn> (f);
}

octave_value_typeinfo::binary_class_op_fcn
octave_value_typeinfo::do_lookup_binary_class_op (octave_value::binary_op op)
{
  void *f = binary_class_ops.checkelem (static_cast<int> (op));
  return reinterpret_cast<octave_value_typeinfo::binary_class_op_fcn> (f);
}

octave_value_typeinfo::binary_op_fcn
octave_value_typeinfo::do_lookup_binary_op (octave_value::binary_op op,
                                            int t1, int t2)
{
  void *f = binary_ops.checkelem (static_cast<int> (op), t1, t2);
  return reinterpret_cast<octave_value_typeinfo::binary_op_fcn> (f);
}

octave_value_typeinfo::binary_class_op_fcn
octave_value_typeinfo::do_lookup_binary_class_op (octave_value::compound_binary_op op)
{
  void *f = compound_binary_class_ops.checkelem (static_cast<int> (op));
  return reinterpret_cast<octave_value_typeinfo::binary_class_op_fcn> (f);
}

octave_value_typeinfo::binary_op_fcn
octave_value_typeinfo::do_lookup_binary_op (octave_value::compound_binary_op op,
                                            int t1, int t2)
{
  void *f = compound_binary_ops.checkelem (static_cast<int> (op), t1, t2);
  return reinterpret_cast<octave_value_typeinfo::binary_op_fcn> (f);
}

octave_value_typeinfo::cat_op_fcn
octave_value_typeinfo::do_lookup_cat_op (int t1, int t2)
{
  void *f = cat_ops.checkelem (t1, t2);
  return reinterpret_cast<octave_value_typeinfo::cat_op_fcn> (f);
}

octave_value_typeinfo::assign_op_fcn
octave_value_typeinfo::do_lookup_assign_op (octave_value::assign_op op,
                                            int t_lhs, int t_rhs)
{
  void *f = assign_ops.checkelem (static_cast<int> (op), t_lhs, t_rhs);
  return reinterpret_cast<octave_value_typeinfo::assign_op_fcn> (f);
}

octave_value_typeinfo::assignany_op_fcn
octave_value_typeinfo::do_lookup_assignany_op (octave_value::assign_op op,
                                               int t_lhs)
{
  void *f = assignany_ops.checkelem (static_cast<int> (op), t_lhs);
  return reinterpret_cast<octave_value_typeinfo::assignany_op_fcn> (f);
}

int
octave_value_typeinfo::do_lookup_pref_assign_conv (int t_lhs, int t_rhs)
{
  return pref_assign_conv.checkelem (t_lhs, t_rhs);
}

octave_base_value::type_conv_fcn
octave_value_typeinfo::do_lookup_type_conv_op (int t, int t_result)
{
  void *f = type_conv_ops.checkelem (t, t_result);
  return reinterpret_cast<octave_base_value::type_conv_fcn> (f);
}

octave_base_value::type_conv_fcn
octave_value_typeinfo::do_lookup_widening_op (int t, int t_result)
{
  void *f = widening_ops.checkelem (t, t_result);
  return reinterpret_cast<octave_base_value::type_conv_fcn> (f);
}

string_vector
octave_value_typeinfo::do_installed_type_names (void)
{
  string_vector retval (num_types);

  for (int i = 0; i < num_types; i++)
    retval(i) = types(i);

  return retval;
}

DEFUN (typeinfo, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} typeinfo ()\n\
@deftypefnx {Built-in Function} {} typeinfo (@var{expr})\n\
\n\
Return the type of the expression @var{expr}, as a string.\n\
\n\
If @var{expr} is omitted, return a cell array of strings containing all the\n\
currently installed data types.\n\
@seealso{class, isa}\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 0)
    retval = Cell (octave_value_typeinfo::installed_type_names ());
  else if (nargin == 1)
    retval = args(0).type_name ();
  else
    print_usage ();

  return retval;
}

/*
%!assert (iscellstr (typeinfo ()))

%!assert (typeinfo ({"cell"}), "cell")

%!assert (typeinfo (1), "scalar")
%!assert (typeinfo (double (1)), "scalar")
%!assert (typeinfo (i), "complex scalar")

%!assert (typeinfo ([1, 2]), "matrix")
%!assert (typeinfo (double ([1, 2])), "matrix")
%!assert (typeinfo (diag ([1, 2])), "diagonal matrix")
%!assert (typeinfo ([i, 2]), "complex matrix")
%!assert (typeinfo (diag ([i, 2])), "complex diagonal matrix")

%!assert (typeinfo (1:2), "range")

%!assert (typeinfo (false), "bool")
%!assert (typeinfo ([true, false]), "bool matrix")

%!assert (typeinfo ("string"), "string")
%!assert (typeinfo ('string'), "sq_string")

%!assert (typeinfo (int8 (1)), "int8 scalar")
%!assert (typeinfo (int16 (1)), "int16 scalar")
%!assert (typeinfo (int32 (1)), "int32 scalar")
%!assert (typeinfo (int64 (1)), "int64 scalar")
%!assert (typeinfo (uint8 (1)), "uint8 scalar")
%!assert (typeinfo (uint16 (1)), "uint16 scalar")
%!assert (typeinfo (uint32 (1)), "uint32 scalar")
%!assert (typeinfo (uint64 (1)), "uint64 scalar")

%!assert (typeinfo (int8 ([1,2])), "int8 matrix")
%!assert (typeinfo (int16 ([1,2])), "int16 matrix")
%!assert (typeinfo (int32 ([1,2])), "int32 matrix")
%!assert (typeinfo (int64 ([1,2])), "int64 matrix")
%!assert (typeinfo (uint8 ([1,2])), "uint8 matrix")
%!assert (typeinfo (uint16 ([1,2])), "uint16 matrix")
%!assert (typeinfo (uint32 ([1,2])), "uint32 matrix")
%!assert (typeinfo (uint64 ([1,2])), "uint64 matrix")

%!assert (typeinfo (sparse ([true, false])), "sparse bool matrix")
%!assert (typeinfo (logical (sparse (i * eye (10)))), "sparse bool matrix")
%!assert (typeinfo (sparse ([1,2])), "sparse matrix")
%!assert (typeinfo (sparse (eye (10))), "sparse matrix")
%!assert (typeinfo (sparse ([i,2])), "sparse complex matrix")
%!assert (typeinfo (sparse (i * eye (10))), "sparse complex matrix")

%!test
%! s(2).a = 1;
%! assert (typeinfo (s), "struct");

%!test
%! s.a = 1;
%! assert (typeinfo (s), "scalar struct");

## FIXME: This doesn't work as a test for comma-separated list
%!#test
%! clist = {1, 2, 3};
%! assert (typeinfo (clist{:}), "cs-list");

%!assert (typeinfo (@sin), "function handle")
%!assert (typeinfo (@(x) x), "function handle")

%!assert (typeinfo (inline ("x^2")), "inline function")

%!assert (typeinfo (single (1)), "float scalar")
%!assert (typeinfo (single (i)), "float complex scalar")
%!assert (typeinfo (single ([1, 2])), "float matrix")

%!assert (typeinfo (single (diag ([1, 2]))), "float diagonal matrix")
%!assert (typeinfo (diag (single ([1, 2]))), "float diagonal matrix")
%!assert (typeinfo (single (diag ([i, 2]))), "float complex diagonal matrix")
%!assert (typeinfo (diag (single ([i, 2]))), "float complex diagonal matrix")

%!assert (typeinfo (eye(3)(:,[1 3 2])), "permutation matrix")
%!test
%! [l, u, p] = lu (rand (3));
%! assert (typeinfo (p), "permutation matrix");

%!assert (typeinfo ([]), "null_matrix")
%!assert (typeinfo (""), "null_string")
%!assert (typeinfo (''), "null_sq_string")

%!test
%! cvar = onCleanup (@() "");
%! assert (typeinfo (cvar), "onCleanup");

%!testif HAVE_JAVA
%! x = javaObject ("java.lang.StringBuffer");
%! assert (typeinfo (x), "octave_java");

## Test input validation
%!error typeinfo ("foo", 1)
*/
