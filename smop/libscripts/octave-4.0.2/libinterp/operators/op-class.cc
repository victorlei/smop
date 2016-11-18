/*

Copyright (C) 2007-2015 John W. Eaton

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

#include "oct-time.h"

#include "gripes.h"
#include "load-path.h"
#include "oct-obj.h"
#include "ov.h"
#include "ov-class.h"
#include "ov-typeinfo.h"
#include "ops.h"
#include "symtab.h"
#include "parse.h"

// class ops.

#define DEF_CLASS_UNOP(name) \
  static octave_value \
  oct_unop_ ## name (const octave_value& a) \
  { \
    octave_value retval; \
 \
    std::string class_name = a.class_name (); \
 \
    octave_value meth = symbol_table::find_method (#name, class_name); \
 \
    if (meth.is_defined ()) \
      { \
        octave_value_list args; \
 \
        args(0) = a; \
 \
        octave_value_list tmp = feval (meth.function_value (), args, 1); \
 \
        if (tmp.length () > 0) \
          retval = tmp(0); \
      } \
    else \
      error ("%s method not defined for %s class", \
             #name, class_name.c_str ()); \
 \
    return retval; \
  }

DEF_CLASS_UNOP (not)
DEF_CLASS_UNOP (uplus)
DEF_CLASS_UNOP (uminus)
DEF_CLASS_UNOP (transpose)
DEF_CLASS_UNOP (ctranspose)

// FIXME: we need to handle precedence in the binop function.

#define DEF_CLASS_BINOP(name) \
  static octave_value \
  oct_binop_ ## name (const octave_value& a1, const octave_value& a2) \
  { \
    octave_value retval; \
 \
    std::string dispatch_type \
      = a1.is_object () ? a1.class_name () : a2.class_name (); \
 \
    octave_value meth = symbol_table::find_method (#name, dispatch_type); \
 \
    if (meth.is_defined ()) \
      { \
        octave_value_list args; \
 \
        args(1) = a2; \
        args(0) = a1; \
 \
        octave_value_list tmp = feval (meth.function_value (), args, 1); \
 \
        if (tmp.length () > 0) \
          retval = tmp(0); \
      } \
    else \
      error ("%s method not defined for %s class", \
             #name, dispatch_type.c_str ()); \
 \
    return retval; \
  }

DEF_CLASS_BINOP (plus)
DEF_CLASS_BINOP (minus)
DEF_CLASS_BINOP (mtimes)
DEF_CLASS_BINOP (mrdivide)
DEF_CLASS_BINOP (mpower)
DEF_CLASS_BINOP (mldivide)
DEF_CLASS_BINOP (lt)
DEF_CLASS_BINOP (le)
DEF_CLASS_BINOP (eq)
DEF_CLASS_BINOP (ge)
DEF_CLASS_BINOP (gt)
DEF_CLASS_BINOP (ne)
DEF_CLASS_BINOP (times)
DEF_CLASS_BINOP (rdivide)
DEF_CLASS_BINOP (power)
DEF_CLASS_BINOP (ldivide)
DEF_CLASS_BINOP (and)
DEF_CLASS_BINOP (or)

#define INSTALL_CLASS_UNOP(op, f) \
  octave_value_typeinfo::register_unary_class_op \
    (octave_value::op, oct_unop_ ## f)

#define INSTALL_CLASS_BINOP(op, f) \
  octave_value_typeinfo::register_binary_class_op \
    (octave_value::op, oct_binop_ ## f)

void
install_class_ops (void)
{
  INSTALL_CLASS_UNOP (op_not, not);
  INSTALL_CLASS_UNOP (op_uplus, uplus);
  INSTALL_CLASS_UNOP (op_uminus, uminus);
  INSTALL_CLASS_UNOP (op_transpose, transpose);
  INSTALL_CLASS_UNOP (op_hermitian, ctranspose);

  INSTALL_CLASS_BINOP (op_add, plus);
  INSTALL_CLASS_BINOP (op_sub, minus);
  INSTALL_CLASS_BINOP (op_mul, mtimes);
  INSTALL_CLASS_BINOP (op_div, mrdivide);
  INSTALL_CLASS_BINOP (op_pow, mpower);
  INSTALL_CLASS_BINOP (op_ldiv, mldivide);
  INSTALL_CLASS_BINOP (op_lt, lt);
  INSTALL_CLASS_BINOP (op_le, le);
  INSTALL_CLASS_BINOP (op_eq, eq);
  INSTALL_CLASS_BINOP (op_ge, ge);
  INSTALL_CLASS_BINOP (op_gt, gt);
  INSTALL_CLASS_BINOP (op_ne, ne);
  INSTALL_CLASS_BINOP (op_el_mul, times);
  INSTALL_CLASS_BINOP (op_el_div, rdivide);
  INSTALL_CLASS_BINOP (op_el_pow, power);
  INSTALL_CLASS_BINOP (op_el_ldiv, ldivide);
  INSTALL_CLASS_BINOP (op_el_and, and);
  INSTALL_CLASS_BINOP (op_el_or, or);
}
