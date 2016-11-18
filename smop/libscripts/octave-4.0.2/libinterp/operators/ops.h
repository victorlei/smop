/*

Copyright (C) 1996-2015 John W. Eaton
Copyright (C) 2009 VZLU Prague, a.s.

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

#if !defined (octave_ops_h)
#define octave_ops_h 1

#include "Array-util.h"

// Concatenation macros that enforce argument prescan
#define CONCAT2X(x,y) x ## y
#define CONCAT2(x,y) CONCAT2X(x,y)

#define CONCAT3X(x,y,z) x ## y ## z
#define CONCAT3(x,y,z) CONCAT3X(x,y,z)

extern void install_ops (void);

#define INSTALL_UNOP(op, t, f) \
  octave_value_typeinfo::register_unary_op \
    (octave_value::op, t::static_type_id (), CONCAT2(oct_unop_, f));

#define INSTALL_NCUNOP(op, t, f) \
  octave_value_typeinfo::register_non_const_unary_op \
    (octave_value::op, t::static_type_id (), CONCAT2(oct_unop_, f));

#define INSTALL_BINOP(op, t1, t2, f) \
  octave_value_typeinfo::register_binary_op \
    (octave_value::op, t1::static_type_id (), t2::static_type_id (), \
     CONCAT2(oct_binop_, f));

#define INSTALL_CATOP(t1, t2, f) \
  octave_value_typeinfo::register_cat_op \
    (t1::static_type_id (), t2::static_type_id (), CONCAT2(oct_catop_, f));

#define INSTALL_ASSIGNOP(op, t1, t2, f) \
  octave_value_typeinfo::register_assign_op \
    (octave_value::op, t1::static_type_id (), t2::static_type_id (), \
     CONCAT2(oct_assignop_, f));

#define INSTALL_ASSIGNANYOP(op, t1, f) \
  octave_value_typeinfo::register_assignany_op \
    (octave_value::op, t1::static_type_id (), CONCAT2(oct_assignop_, f));

#define INSTALL_ASSIGNCONV(t1, t2, tr) \
  octave_value_typeinfo::register_pref_assign_conv \
    (t1::static_type_id (), t2::static_type_id (), tr::static_type_id ());

#define INSTALL_CONVOP(t1, t2, f) \
  octave_value_typeinfo::register_type_conv_op \
    (t1::static_type_id (), t2::static_type_id (), CONCAT2(oct_conv_, f));

#define INSTALL_WIDENOP(t1, t2, f) \
  octave_value_typeinfo::register_widening_op \
    (t1::static_type_id (), t2::static_type_id (), CONCAT2(oct_conv_, f));

#define CAST_UNOP_ARG(t) \
  t v = dynamic_cast<t> (a)

#define CAST_BINOP_ARGS(t1, t2) \
  t1 v1 = dynamic_cast<t1> (a1);                \
  t2 v2 = dynamic_cast<t2> (a2)

#define CAST_CONV_ARG(t) \
  t v = dynamic_cast<t> (a)

#define ASSIGNOPDECL(name) \
  static octave_value \
  CONCAT2(oct_assignop_, name) (octave_base_value& a1, \
                         const octave_value_list& idx, \
                         const octave_base_value& a2)

#define NULLASSIGNOPDECL(name) \
  static octave_value \
  CONCAT2(oct_assignop_, name) (octave_base_value& a, \
                         const octave_value_list& idx, \
                         const octave_base_value&)

#define ASSIGNANYOPDECL(name) \
  static octave_value \
  CONCAT2(oct_assignop_, name) (octave_base_value& a1, \
                         const octave_value_list& idx, \
                         const octave_value& a2)

#define DEFASSIGNOP(name, t1, t2) \
  ASSIGNOPDECL (name)

#define DEFASSIGNOP_FN(name, t1, t2, f) \
  ASSIGNOPDECL (name) \
  { \
    CAST_BINOP_ARGS (CONCAT2(octave_, t1)&, const CONCAT2(octave_, t2)&); \
 \
    v1.f (idx, v2.CONCAT2(t1, _value) ()); \
    return octave_value (); \
  }

#define DEFNULLASSIGNOP_FN(name, t, f) \
  NULLASSIGNOPDECL (name) \
  { \
    CAST_UNOP_ARG (CONCAT2(octave_, t)&); \
 \
    v.f (idx); \
    return octave_value (); \
  }

#define DEFNDASSIGNOP_FN(name, t1, t2, e, f) \
  ASSIGNOPDECL (name) \
  { \
    CAST_BINOP_ARGS (CONCAT2(octave_, t1)&, const CONCAT2(octave_, t2)&); \
 \
    v1.f (idx, v2.CONCAT2(e, _value) ()); \
    return octave_value (); \
  }

// FIXME: the following currently don't handle index.
#define DEFNDASSIGNOP_OP(name, t1, t2, f, op) \
  ASSIGNOPDECL (name) \
  { \
    CAST_BINOP_ARGS (CONCAT2(octave_, t1)&, const CONCAT2(octave_, t2)&); \
 \
    assert (idx.empty ()); \
    v1.matrix_ref () op v2.CONCAT2(f, _value) (); \
 \
    return octave_value (); \
  }

#define DEFNDASSIGNOP_FNOP(name, t1, t2, f, fnop) \
  ASSIGNOPDECL (name) \
  { \
    CAST_BINOP_ARGS (CONCAT2(octave_, t1)&, const CONCAT2(octave_, t2)&); \
 \
    assert (idx.empty ()); \
    fnop (v1.matrix_ref (), v2.CONCAT2(f, _value) ()); \
 \
    return octave_value (); \
  }

#define DEFASSIGNANYOP_FN(name, t1, f) \
  ASSIGNANYOPDECL (name) \
  { \
    CONCAT2(octave_, t1)& v1 = dynamic_cast<CONCAT2(octave_, t1)&> (a1); \
 \
    v1.f (idx, a2); \
    return octave_value (); \
  }

#define CONVDECL(name) \
  static octave_base_value * \
  CONCAT2(oct_conv_, name) (const octave_base_value& a)

#define CONVDECLX(name) \
  static octave_base_value * \
  CONCAT2(oct_conv_, name) (const octave_base_value&)

#define DEFCONV(name, a_dummy, b_dummy) \
  CONVDECL (name)

#define DEFCONVFNX(name, tfrom, ovtto, tto, e) \
  CONVDECL (name) \
  { \
    CAST_CONV_ARG (const CONCAT2(octave_, tfrom)&); \
 \
    return new CONCAT2(octave_, ovtto) (CONCAT2(tto, NDArray) (v.CONCAT2(e, array_value) ())); \
  }

#define DEFCONVFNX2(name, tfrom, ovtto, e) \
  CONVDECL (name) \
  { \
    CAST_CONV_ARG (const CONCAT2(octave_, tfrom)&); \
 \
    return new CONCAT2(octave_, ovtto) (v.CONCAT2(e, array_value) ()); \
  }

#define DEFDBLCONVFN(name, ovtfrom, e) \
  CONVDECL (name) \
  { \
    CAST_CONV_ARG (const CONCAT2(octave_, ovtfrom)&); \
 \
    return new octave_matrix (NDArray (v.CONCAT2(e, _value) ())); \
  }

#define DEFFLTCONVFN(name, ovtfrom, e) \
  CONVDECL (name) \
  { \
    CAST_CONV_ARG (const CONCAT2(octave_, ovtfrom)&); \
 \
    return new octave_float_matrix (FloatNDArray (v.CONCAT2(e, _value) ())); \
  }

#define DEFSTRINTCONVFN(name, tto) \
  DEFCONVFNX(name, char_matrix_str, CONCAT2(tto, _matrix), tto, char_)

#define DEFSTRDBLCONVFN(name, tfrom) \
  DEFCONVFNX(name, tfrom, matrix, , char_)

#define DEFSTRFLTCONVFN(name, tfrom) \
  DEFCONVFNX(name, tfrom, float_matrix, Float, char_)

#define DEFCONVFN(name, tfrom, tto) \
  DEFCONVFNX2 (name, tfrom, CONCAT2(tto, _matrix), CONCAT2(tto, _))

#define DEFCONVFN2(name, tfrom, sm, tto) \
  DEFCONVFNX2 (name, CONCAT3(tfrom, _, sm), CONCAT2(tto, _matrix), CONCAT2(tto, _))

#define UNOPDECL(name, a) \
  static octave_value \
  CONCAT2(oct_unop_, name) (const octave_base_value& a)

#define DEFUNOPX(name, t) \
  UNOPDECL (name, , )

#define DEFUNOP(name, t) \
  UNOPDECL (name, a)

#define DEFUNOP_OP(name, t, op) \
  UNOPDECL (name, a) \
  { \
    CAST_UNOP_ARG (const CONCAT2(octave_, t)&); \
    return octave_value (op v.CONCAT2(t, _value) ()); \
  }

#define DEFNDUNOP_OP(name, t, e, op) \
  UNOPDECL (name, a) \
  { \
    CAST_UNOP_ARG (const CONCAT2(octave_, t)&); \
    return octave_value (op v.CONCAT2(e, _value) ()); \
  }

// FIXME: in some cases, the constructor isn't necessary.

#define DEFUNOP_FN(name, t, f) \
  UNOPDECL (name, a) \
  { \
    CAST_UNOP_ARG (const CONCAT2(octave_, t)&); \
    return octave_value (f (v.CONCAT2(t, _value) ())); \
  }

#define DEFNDUNOP_FN(name, t, e, f) \
  UNOPDECL (name, a) \
  { \
    CAST_UNOP_ARG (const CONCAT2(octave_, t)&); \
    return octave_value (f (v.CONCAT2(e, _value) ())); \
  }

#define DEFNCUNOP_METHOD(name, t, method) \
  static void \
  CONCAT2(oct_unop_, name) (octave_base_value& a) \
  { \
    CAST_UNOP_ARG (CONCAT2(octave_, t)&); \
    v.method (); \
  }

#define BINOPDECL(name, a1, a2) \
  static octave_value \
  CONCAT2(oct_binop_, name) (const octave_base_value& a1, const octave_base_value& a2)

#define DEFBINOPX(name, t1, t2) \
  BINOPDECL (name, , )

#define DEFBINOP(name, t1, t2) \
  BINOPDECL (name, a1, a2)

#define DEFBINOP_OP(name, t1, t2, op) \
  BINOPDECL (name, a1, a2) \
  { \
    CAST_BINOP_ARGS (const CONCAT2(octave_, t1)&, const CONCAT2(octave_, t2)&); \
    return octave_value \
      (v1.CONCAT2(t1, _value) () op v2.CONCAT2(t2, _value) ()); \
  }

#define DEFCMPLXCMPOP_OP(name, t1, t2, op) \
  BINOPDECL (name, a1, a2) \
  { \
    CAST_BINOP_ARGS (const CONCAT2(octave_, t1)&, const CONCAT2(octave_, t2)&); \
    gripe_warn_complex_cmp (); \
    return octave_value \
      (v1.CONCAT2(t1, _value) () op v2.CONCAT2(t2, _value) ()); \
  }

#define DEFSCALARBOOLOP_OP(name, t1, t2, op) \
  BINOPDECL (name, a1, a2) \
  { \
    CAST_BINOP_ARGS (const CONCAT2(octave_, t1)&, const CONCAT2(octave_, t2)&); \
    if (xisnan (v1.CONCAT2(t1, _value) ()) || xisnan (v2.CONCAT2(t2, _value) ())) \
      { \
        gripe_nan_to_logical_conversion (); \
        return octave_value (); \
      } \
    else \
      return octave_value \
        (v1.CONCAT2(t1, _value) () op v2.CONCAT2(t2, _value) ()); \
  }

#define DEFNDBINOP_OP(name, t1, t2, e1, e2, op) \
  BINOPDECL (name, a1, a2) \
  { \
    CAST_BINOP_ARGS (const CONCAT2(octave_, t1)&, const CONCAT2(octave_, t2)&); \
    return octave_value \
      (v1.CONCAT2(e1, _value) () op v2.CONCAT2(e2, _value) ()); \
  }

// FIXME: in some cases, the constructor isn't necessary.

#define DEFBINOP_FN(name, t1, t2, f) \
  BINOPDECL (name, a1, a2) \
  { \
    CAST_BINOP_ARGS (const CONCAT2(octave_, t1)&, const CONCAT2(octave_, t2)&); \
    return octave_value (f (v1.CONCAT2(t1, _value) (), v2.CONCAT2(t2, _value) ())); \
  }

#define DEFNDBINOP_FN(name, t1, t2, e1, e2, f) \
  BINOPDECL (name, a1, a2) \
  { \
    CAST_BINOP_ARGS (const CONCAT2(octave_, t1)&, const CONCAT2(octave_, t2)&); \
    return octave_value (f (v1.CONCAT2(e1, _value) (), v2.CONCAT2(e2, _value) ())); \
  }

#define DEFNDCMPLXCMPOP_FN(name, t1, t2, e1, e2, f) \
  BINOPDECL (name, a1, a2) \
  { \
    CAST_BINOP_ARGS (const CONCAT2(octave_, t1)&, const CONCAT2(octave_, t2)&); \
    return octave_value (f (v1.CONCAT2(e1, _value) (), v2.CONCAT2(e2, _value) ())); \
  }

#define CATOPDECL(name, a1, a2) \
  static octave_value \
  CONCAT2(oct_catop_, name) (octave_base_value& a1, const octave_base_value& a2, \
                      const Array<octave_idx_type>& ra_idx)

#define DEFCATOPX(name, t1, t2) \
  CATOPDECL (name, , )

#define DEFCATOP(name, t1, t2)  \
  CATOPDECL (name, a1, a2)

// FIXME: in some cases, the constructor isn't necessary.

#define DEFCATOP_FN(name, t1, t2, f) \
  CATOPDECL (name, a1, a2) \
  { \
    CAST_BINOP_ARGS (CONCAT2(octave_, t1)&, const CONCAT2(octave_, t2)&); \
    return octave_value (v1.CONCAT2(t1, _value) () . f (v2.CONCAT2(t2, _value) (), ra_idx)); \
  }

#define DEFNDCATOP_FN(name, t1, t2, e1, e2, f) \
  CATOPDECL (name, a1, a2) \
  { \
    CAST_BINOP_ARGS (CONCAT2(octave_, t1)&, const CONCAT2(octave_, t2)&); \
    return octave_value (v1.CONCAT2(e1, _value) () . f (v2.CONCAT2(e2, _value) (), ra_idx)); \
  }

#define DEFNDCHARCATOP_FN(name, t1, t2, f) \
  CATOPDECL (name, a1, a2) \
  { \
    CAST_BINOP_ARGS (CONCAT2(octave_, t1)&, const CONCAT2(octave_, t2)&); \
 \
    return octave_value (v1.char_array_value () . f (v2.char_array_value (), ra_idx), \
                         ((a1.is_sq_string () || a2.is_sq_string ()) \
                          ? '\'' : '"')); \
  }

// For compatibility, the second arg is always converted to the type
// of the first.  Hmm.

#define DEFNDCATOP_FN2(name, t1, t2, tc1, tc2, e1, e2, f) \
  CATOPDECL (name, a1, a2) \
  { \
    CAST_BINOP_ARGS (CONCAT2(octave_, t1)&, const CONCAT2(octave_, t2)&); \
    return octave_value (tc1 (v1.CONCAT2(e1, _value) ()) . f (tc2 (v2.CONCAT2(e2, _value) ()), ra_idx)); \
  }

#define CATOP_NONCONFORMANT(msg) \
  gripe_nonconformant (msg, \
                       a1.rows (), a1.columns (), \
                       a2.rows (), a2.columns ()); \
  return octave_value ()

#endif
