/*

Copyright (C) 1996-2015 John W. Eaton
Copyright (C) 2009-2010 VZLU Prague

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

#include "data-conv.h"
#include "quit.h"
#include "str-vec.h"

#include "oct-obj.h"
#include "oct-stream.h"
#include "ov.h"
#include "ov-base.h"
#include "ov-bool.h"
#include "ov-bool-mat.h"
#include "ov-cell.h"
#include "ov-scalar.h"
#include "ov-float.h"
#include "ov-re-mat.h"
#include "ov-flt-re-mat.h"
#include "ov-re-diag.h"
#include "ov-flt-re-diag.h"
#include "ov-perm.h"
#include "ov-bool-sparse.h"
#include "ov-cx-sparse.h"
#include "ov-re-sparse.h"
#include "ov-int8.h"
#include "ov-int16.h"
#include "ov-int32.h"
#include "ov-int64.h"
#include "ov-uint8.h"
#include "ov-uint16.h"
#include "ov-uint32.h"
#include "ov-uint64.h"
#include "ov-complex.h"
#include "ov-flt-complex.h"
#include "ov-cx-mat.h"
#include "ov-flt-cx-mat.h"
#include "ov-cx-diag.h"
#include "ov-flt-cx-diag.h"
#include "ov-ch-mat.h"
#include "ov-str-mat.h"
#include "ov-range.h"
#include "ov-struct.h"
#include "ov-class.h"
#include "ov-classdef.h"
#include "ov-oncleanup.h"
#include "ov-cs-list.h"
#include "ov-colon.h"
#include "ov-builtin.h"
#include "ov-dld-fcn.h"
#include "ov-usr-fcn.h"
#include "ov-fcn-handle.h"
#include "ov-fcn-inline.h"
#include "ov-typeinfo.h"
#include "ov-null-mat.h"
#include "ov-lazy-idx.h"
#ifdef HAVE_JAVA
#include "ov-java.h"
#endif

#include "defun.h"
#include "error.h"
#include "gripes.h"
#include "pager.h"
#include "parse.h"
#include "pr-output.h"
#include "symtab.h"
#include "utils.h"
#include "variables.h"

// We are likely to have a lot of octave_value objects to allocate, so
// make the grow_size large.

// If TRUE, don't create special diagonal matrix objects.

static bool Vdisable_diagonal_matrix = false;

// If TRUE, don't create special permutation matrix objects.

static bool Vdisable_permutation_matrix = false;

// If TRUE, don't create special range objects.

static bool Vdisable_range = false;

// FIXME

// Octave's value type.

octave_base_value *
octave_value::nil_rep (void)
{
  static octave_base_value nr;
  return &nr;
}

std::string
octave_value::unary_op_as_string (unary_op op)
{
  std::string retval;

  switch (op)
    {
    case op_not:
      retval = "!";
      break;

    case op_uplus:
      retval = "+";
      break;

    case op_uminus:
      retval = "-";
      break;

    case op_transpose:
      retval = ".'";
      break;

    case op_hermitian:
      retval = "'";
      break;

    case op_incr:
      retval = "++";
      break;

    case op_decr:
      retval = "--";
      break;

    default:
      retval = "<unknown>";
    }

  return retval;
}

std::string
octave_value::unary_op_fcn_name (unary_op op)
{
  std::string retval;

  switch (op)
    {
    case op_not:
      retval = "not";
      break;

    case op_uplus:
      retval = "uplus";
      break;

    case op_uminus:
      retval = "uminus";
      break;

    case op_transpose:
      retval = "transpose";
      break;

    case op_hermitian:
      retval = "ctranspose";
      break;

    default:
      break;
    }

  return retval;
}

std::string
octave_value::binary_op_as_string (binary_op op)
{
  std::string retval;

  switch (op)
    {
    case op_add:
      retval = "+";
      break;

    case op_sub:
      retval = "-";
      break;

    case op_mul:
      retval = "*";
      break;

    case op_div:
      retval = "/";
      break;

    case op_pow:
      retval = "^";
      break;

    case op_ldiv:
      retval = "\\";
      break;

    case op_lshift:
      retval = "<<";
      break;

    case op_rshift:
      retval = ">>";
      break;

    case op_lt:
      retval = "<";
      break;

    case op_le:
      retval = "<=";
      break;

    case op_eq:
      retval = "==";
      break;

    case op_ge:
      retval = ">=";
      break;

    case op_gt:
      retval = ">";
      break;

    case op_ne:
      retval = "!=";
      break;

    case op_el_mul:
      retval = ".*";
      break;

    case op_el_div:
      retval = "./";
      break;

    case op_el_pow:
      retval = ".^";
      break;

    case op_el_ldiv:
      retval = ".\\";
      break;

    case op_el_and:
      retval = "&";
      break;

    case op_el_or:
      retval = "|";
      break;

    case op_struct_ref:
      retval = ".";
      break;

    default:
      retval = "<unknown>";
    }

  return retval;
}

std::string
octave_value::binary_op_fcn_name (binary_op op)
{
  std::string retval;

  switch (op)
    {
    case op_add:
      retval = "plus";
      break;

    case op_sub:
      retval = "minus";
      break;

    case op_mul:
      retval = "mtimes";
      break;

    case op_div:
      retval = "mrdivide";
      break;

    case op_pow:
      retval = "mpower";
      break;

    case op_ldiv:
      retval = "mldivide";
      break;

    case op_lt:
      retval = "lt";
      break;

    case op_le:
      retval = "le";
      break;

    case op_eq:
      retval = "eq";
      break;

    case op_ge:
      retval = "ge";
      break;

    case op_gt:
      retval = "gt";
      break;

    case op_ne:
      retval = "ne";
      break;

    case op_el_mul:
      retval = "times";
      break;

    case op_el_div:
      retval = "rdivide";
      break;

    case op_el_pow:
      retval = "power";
      break;

    case op_el_ldiv:
      retval = "ldivide";
      break;

    case op_el_and:
      retval = "and";
      break;

    case op_el_or:
      retval = "or";
      break;

    default:
      break;
    }

  return retval;
}

std::string
octave_value::binary_op_fcn_name (compound_binary_op op)
{
  std::string retval;

  switch (op)
    {
    case op_trans_mul:
      retval = "transtimes";
      break;

    case op_mul_trans:
      retval = "timestrans";
      break;

    case op_herm_mul:
      retval = "hermtimes";
      break;

    case op_mul_herm:
      retval = "timesherm";
      break;

    case op_trans_ldiv:
      retval = "transldiv";
      break;

    case op_herm_ldiv:
      retval = "hermldiv";
      break;

    case op_el_and_not:
      retval = "andnot";
      break;

    case op_el_or_not:
      retval = "ornot";
      break;

    case op_el_not_and:
      retval = "notand";
      break;

    case op_el_not_or:
      retval = "notor";
      break;

    default:
      break;
    }

  return retval;
}

std::string
octave_value::assign_op_as_string (assign_op op)
{
  std::string retval;

  switch (op)
    {
    case op_asn_eq:
      retval = "=";
      break;

    case op_add_eq:
      retval = "+=";
      break;

    case op_sub_eq:
      retval = "-=";
      break;

    case op_mul_eq:
      retval = "*=";
      break;

    case op_div_eq:
      retval = "/=";
      break;

    case op_ldiv_eq:
      retval = "\\=";
      break;

    case op_pow_eq:
      retval = "^=";
      break;

    case op_lshift_eq:
      retval = "<<=";
      break;

    case op_rshift_eq:
      retval = ">>=";
      break;

    case op_el_mul_eq:
      retval = ".*=";
      break;

    case op_el_div_eq:
      retval = "./=";
      break;

    case op_el_ldiv_eq:
      retval = ".\\=";
      break;

    case op_el_pow_eq:
      retval = ".^=";
      break;

    case op_el_and_eq:
      retval = "&=";
      break;

    case op_el_or_eq:
      retval = "|=";
      break;

    default:
      retval = "<unknown>";
    }

  return retval;
}

octave_value::binary_op
octave_value::assign_op_to_binary_op (assign_op op)
{
  switch (op)
    {
    case op_add_eq:
      return op_add;
    case op_sub_eq:
      return op_sub;
    case op_mul_eq:
      return op_mul;
    case op_div_eq:
      return op_div;
    case op_ldiv_eq:
      return op_ldiv;
    case op_pow_eq:
      return op_pow;
    case op_lshift_eq:
      return op_lshift;
    case op_rshift_eq:
      return op_rshift;
    case op_el_mul_eq:
      return op_el_mul;
    case op_el_div_eq:
      return op_el_div;
    case op_el_ldiv_eq:
      return op_el_ldiv;
    case op_el_pow_eq:
      return op_el_pow;
    case op_el_and_eq:
      return op_el_and;
    case op_el_or_eq:
      return op_el_or;
    default:
      return unknown_binary_op;
    }

}

octave_value::assign_op
octave_value::binary_op_to_assign_op (binary_op op)
{
  assign_op retval;

  switch (op)
    {
    case op_add:
      retval = op_add_eq;
      break;
    case op_sub:
      retval = op_sub_eq;
      break;
    case op_mul:
      retval = op_mul_eq;
      break;
    case op_div:
      retval = op_div_eq;
      break;
    case op_el_mul:
      retval = op_el_mul_eq;
      break;
    case op_el_div:
      retval = op_el_div_eq;
      break;
    case op_el_and:
      retval = op_el_and_eq;
      break;
    case op_el_or:
      retval = op_el_or_eq;
      break;
    default:
      retval = unknown_assign_op;
    }

  return retval;
}

octave_value::octave_value (short int i)
  : rep (new octave_scalar (i))
{
}

octave_value::octave_value (unsigned short int i)
  : rep (new octave_scalar (i))
{
}

octave_value::octave_value (int i)
  : rep (new octave_scalar (i))
{
}

octave_value::octave_value (unsigned int i)
  : rep (new octave_scalar (i))
{
}

octave_value::octave_value (long int i)
  : rep (new octave_scalar (i))
{
}

octave_value::octave_value (unsigned long int i)
  : rep (new octave_scalar (i))
{
}

#if defined (HAVE_LONG_LONG_INT)
octave_value::octave_value (long long int i)
  : rep (new octave_scalar (i))
{
}
#endif

#if defined (HAVE_UNSIGNED_LONG_LONG_INT)
octave_value::octave_value (unsigned long long int i)
  : rep (new octave_scalar (i))
{
}
#endif

octave_value::octave_value (octave_time t)
  : rep (new octave_scalar (t.double_value ()))
{
}

octave_value::octave_value (double d)
  : rep (new octave_scalar (d))
{
}

octave_value::octave_value (float d)
  : rep (new octave_float_scalar (d))
{
}

octave_value::octave_value (const Cell& c, bool is_csl)
  : rep (is_csl
         ? dynamic_cast<octave_base_value *> (new octave_cs_list (c))
         : dynamic_cast<octave_base_value *> (new octave_cell (c)))
{
}

octave_value::octave_value (const Array<octave_value>& a, bool is_csl)
  : rep (is_csl
         ? dynamic_cast<octave_base_value *> (new octave_cs_list (Cell (a)))
         : dynamic_cast<octave_base_value *> (new octave_cell (Cell (a))))
{
}

octave_value::octave_value (const Matrix& m, const MatrixType& t)
  : rep (new octave_matrix (m, t))
{
  maybe_mutate ();
}

octave_value::octave_value (const FloatMatrix& m, const MatrixType& t)
  : rep (new octave_float_matrix (m, t))
{
  maybe_mutate ();
}

octave_value::octave_value (const NDArray& a)
  : rep (new octave_matrix (a))
{
  maybe_mutate ();
}

octave_value::octave_value (const FloatNDArray& a)
  : rep (new octave_float_matrix (a))
{
  maybe_mutate ();
}

octave_value::octave_value (const Array<double>& a)
  : rep (new octave_matrix (a))
{
  maybe_mutate ();
}

octave_value::octave_value (const Array<float>& a)
  : rep (new octave_float_matrix (a))
{
  maybe_mutate ();
}

octave_value::octave_value (const DiagArray2<double>& d)
  : rep (Vdisable_diagonal_matrix
         ? dynamic_cast<octave_base_value *> (new octave_matrix (Matrix (d)))
         : dynamic_cast<octave_base_value *> (new octave_diag_matrix (d)))
{
  maybe_mutate ();
}

octave_value::octave_value (const DiagArray2<float>& d)
  : rep (Vdisable_diagonal_matrix
         ? dynamic_cast<octave_base_value *> (new octave_float_matrix (FloatMatrix (d)))
         : dynamic_cast<octave_base_value *> (new octave_float_diag_matrix (d)))
{
  maybe_mutate ();
}

octave_value::octave_value (const DiagArray2<Complex>& d)
  : rep (Vdisable_diagonal_matrix
         ? dynamic_cast<octave_base_value *> (new octave_complex_matrix (ComplexMatrix (d)))
         : dynamic_cast<octave_base_value *> (new octave_complex_diag_matrix (d)))
{
  maybe_mutate ();
}

octave_value::octave_value (const DiagArray2<FloatComplex>& d)
  : rep (Vdisable_diagonal_matrix
         ? dynamic_cast<octave_base_value *> (new octave_float_complex_matrix (FloatComplexMatrix (d)))
         : dynamic_cast<octave_base_value *> (new octave_float_complex_diag_matrix (d)))
{
  maybe_mutate ();
}

octave_value::octave_value (const DiagMatrix& d)
  : rep (Vdisable_diagonal_matrix
         ? dynamic_cast<octave_base_value *> (new octave_matrix (Matrix (d)))
         : dynamic_cast<octave_base_value *> (new octave_diag_matrix (d)))
{
  maybe_mutate ();
}

octave_value::octave_value (const FloatDiagMatrix& d)
  : rep (Vdisable_diagonal_matrix
         ? dynamic_cast<octave_base_value *> (new octave_float_matrix (FloatMatrix (d)))
         : dynamic_cast<octave_base_value *> (new octave_float_diag_matrix (d)))
{
  maybe_mutate ();
}

octave_value::octave_value (const RowVector& v)
  : rep (new octave_matrix (v))
{
  maybe_mutate ();
}

octave_value::octave_value (const FloatRowVector& v)
  : rep (new octave_float_matrix (v))
{
  maybe_mutate ();
}

octave_value::octave_value (const ColumnVector& v)
  : rep (new octave_matrix (v))
{
  maybe_mutate ();
}

octave_value::octave_value (const FloatColumnVector& v)
  : rep (new octave_float_matrix (v))
{
  maybe_mutate ();
}

octave_value::octave_value (const Complex& C)
  : rep (new octave_complex (C))
{
  maybe_mutate ();
}

octave_value::octave_value (const FloatComplex& C)
  : rep (new octave_float_complex (C))
{
  maybe_mutate ();
}

octave_value::octave_value (const ComplexMatrix& m, const MatrixType& t)
  : rep (new octave_complex_matrix (m, t))
{
  maybe_mutate ();
}

octave_value::octave_value (const FloatComplexMatrix& m, const MatrixType& t)
  : rep (new octave_float_complex_matrix (m, t))
{
  maybe_mutate ();
}

octave_value::octave_value (const ComplexNDArray& a)
  : rep (new octave_complex_matrix (a))
{
  maybe_mutate ();
}

octave_value::octave_value (const FloatComplexNDArray& a)
  : rep (new octave_float_complex_matrix (a))
{
  maybe_mutate ();
}

octave_value::octave_value (const Array<Complex>& a)
  : rep (new octave_complex_matrix (a))
{
  maybe_mutate ();
}

octave_value::octave_value (const Array<FloatComplex>& a)
  : rep (new octave_float_complex_matrix (a))
{
  maybe_mutate ();
}

octave_value::octave_value (const ComplexDiagMatrix& d)
  : rep (Vdisable_diagonal_matrix
         ? dynamic_cast<octave_base_value *> (new octave_complex_matrix (ComplexMatrix (d)))
         : dynamic_cast<octave_base_value *> (new octave_complex_diag_matrix (d)))
{
  maybe_mutate ();
}

octave_value::octave_value (const FloatComplexDiagMatrix& d)
  : rep (Vdisable_diagonal_matrix
         ? dynamic_cast<octave_base_value *> (new octave_float_complex_matrix (FloatComplexMatrix (d)))
         : dynamic_cast<octave_base_value *> (new octave_float_complex_diag_matrix (d)))
{
  maybe_mutate ();
}

octave_value::octave_value (const ComplexRowVector& v)
  : rep (new octave_complex_matrix (v))
{
  maybe_mutate ();
}

octave_value::octave_value (const FloatComplexRowVector& v)
  : rep (new octave_float_complex_matrix (v))
{
  maybe_mutate ();
}

octave_value::octave_value (const ComplexColumnVector& v)
  : rep (new octave_complex_matrix (v))
{
  maybe_mutate ();
}

octave_value::octave_value (const FloatComplexColumnVector& v)
  : rep (new octave_float_complex_matrix (v))
{
  maybe_mutate ();
}

octave_value::octave_value (const PermMatrix& p)
  : rep (Vdisable_permutation_matrix
         ? dynamic_cast<octave_base_value *> (new octave_matrix (Matrix (p)))
         : dynamic_cast<octave_base_value *> (new octave_perm_matrix (p)))
{
  maybe_mutate ();
}

octave_value::octave_value (bool b)
  : rep (new octave_bool (b))
{
}

octave_value::octave_value (const boolMatrix& bm, const MatrixType& t)
  : rep (new octave_bool_matrix (bm, t))
{
  maybe_mutate ();
}

octave_value::octave_value (const boolNDArray& bnda)
  : rep (new octave_bool_matrix (bnda))
{
  maybe_mutate ();
}

octave_value::octave_value (const Array<bool>& bnda)
  : rep (new octave_bool_matrix (bnda))
{
  maybe_mutate ();
}

octave_value::octave_value (char c, char type)
  : rep (type == '"'
         ? new octave_char_matrix_dq_str (c)
         : new octave_char_matrix_sq_str (c))
{
  maybe_mutate ();
}

octave_value::octave_value (const char *s, char type)
  : rep (type == '"'
         ? new octave_char_matrix_dq_str (s)
         : new octave_char_matrix_sq_str (s))
{
  maybe_mutate ();
}

octave_value::octave_value (const std::string& s, char type)
  : rep (type == '"'
         ? new octave_char_matrix_dq_str (s)
         : new octave_char_matrix_sq_str (s))
{
  maybe_mutate ();
}

octave_value::octave_value (const string_vector& s, char type)
  : rep (type == '"'
         ? new octave_char_matrix_dq_str (s)
         : new octave_char_matrix_sq_str (s))
{
  maybe_mutate ();
}

octave_value::octave_value (const charMatrix& chm, char type)
  : rep (type == '"'
         ? new octave_char_matrix_dq_str (chm)
         : new octave_char_matrix_sq_str (chm))
{
  maybe_mutate ();
}

octave_value::octave_value (const charNDArray& chm, char type)
  : rep (type == '"'
         ? new octave_char_matrix_dq_str (chm)
         : new octave_char_matrix_sq_str (chm))
{
  maybe_mutate ();
}

octave_value::octave_value (const Array<char>& chm, char type)
  : rep (type == '"'
         ? new octave_char_matrix_dq_str (chm)
         : new octave_char_matrix_sq_str (chm))
{
  maybe_mutate ();
}

octave_value::octave_value (const charMatrix& chm, bool, char type)
  : rep (type == '"'
         ? new octave_char_matrix_dq_str (chm)
         : new octave_char_matrix_sq_str (chm))
{
  maybe_mutate ();
}

octave_value::octave_value (const charNDArray& chm, bool, char type)
  : rep (type == '"'
         ? new octave_char_matrix_dq_str (chm)
         : new octave_char_matrix_sq_str (chm))
{
  maybe_mutate ();
}

octave_value::octave_value (const Array<char>& chm, bool, char type)
  : rep (type == '"'
         ? new octave_char_matrix_dq_str (chm)
         : new octave_char_matrix_sq_str (chm))
{
  maybe_mutate ();
}

octave_value::octave_value (const SparseMatrix& m, const MatrixType &t)
  : rep (new octave_sparse_matrix (m, t))
{
  maybe_mutate ();
}

octave_value::octave_value (const Sparse<double>& m, const MatrixType &t)
  : rep (new octave_sparse_matrix (m, t))
{
  maybe_mutate ();
}

octave_value::octave_value (const SparseComplexMatrix& m, const MatrixType &t)
  : rep (new octave_sparse_complex_matrix (m, t))
{
  maybe_mutate ();
}

octave_value::octave_value (const Sparse<Complex>& m, const MatrixType &t)
  : rep (new octave_sparse_complex_matrix (m, t))
{
  maybe_mutate ();
}

octave_value::octave_value (const SparseBoolMatrix& bm, const MatrixType &t)
  : rep (new octave_sparse_bool_matrix (bm, t))
{
  maybe_mutate ();
}

octave_value::octave_value (const Sparse<bool>& bm, const MatrixType &t)
  : rep (new octave_sparse_bool_matrix (bm, t))
{
  maybe_mutate ();
}

octave_value::octave_value (const octave_int8& i)
  : rep (new octave_int8_scalar (i))
{
  maybe_mutate ();
}

octave_value::octave_value (const octave_uint8& i)
  : rep (new octave_uint8_scalar (i))
{
  maybe_mutate ();
}

octave_value::octave_value (const octave_int16& i)
  : rep (new octave_int16_scalar (i))
{
  maybe_mutate ();
}

octave_value::octave_value (const octave_uint16& i)
  : rep (new octave_uint16_scalar (i))
{
  maybe_mutate ();
}

octave_value::octave_value (const octave_int32& i)
  : rep (new octave_int32_scalar (i))
{
  maybe_mutate ();
}

octave_value::octave_value (const octave_uint32& i)
  : rep (new octave_uint32_scalar (i))
{
  maybe_mutate ();
}

octave_value::octave_value (const octave_int64& i)
  : rep (new octave_int64_scalar (i))
{
  maybe_mutate ();
}

octave_value::octave_value (const octave_uint64& i)
  : rep (new octave_uint64_scalar (i))
{
  maybe_mutate ();
}

octave_value::octave_value (const int8NDArray& inda)
  : rep (new octave_int8_matrix (inda))
{
  maybe_mutate ();
}

octave_value::octave_value (const Array<octave_int8>& inda)
  : rep (new octave_int8_matrix (inda))
{
  maybe_mutate ();
}

octave_value::octave_value (const uint8NDArray& inda)
  : rep (new octave_uint8_matrix (inda))
{
  maybe_mutate ();
}

octave_value::octave_value (const Array<octave_uint8>& inda)
  : rep (new octave_uint8_matrix (inda))
{
  maybe_mutate ();
}

octave_value::octave_value (const int16NDArray& inda)
  : rep (new octave_int16_matrix (inda))
{
  maybe_mutate ();
}

octave_value::octave_value (const Array<octave_int16>& inda)
  : rep (new octave_int16_matrix (inda))
{
  maybe_mutate ();
}

octave_value::octave_value (const uint16NDArray& inda)
  : rep (new octave_uint16_matrix (inda))
{
  maybe_mutate ();
}

octave_value::octave_value (const Array<octave_uint16>& inda)
  : rep (new octave_uint16_matrix (inda))
{
  maybe_mutate ();
}

octave_value::octave_value (const int32NDArray& inda)
  : rep (new octave_int32_matrix (inda))
{
  maybe_mutate ();
}

octave_value::octave_value (const Array<octave_int32>& inda)
  : rep (new octave_int32_matrix (inda))
{
  maybe_mutate ();
}

octave_value::octave_value (const uint32NDArray& inda)
  : rep (new octave_uint32_matrix (inda))
{
  maybe_mutate ();
}

octave_value::octave_value (const Array<octave_uint32>& inda)
  : rep (new octave_uint32_matrix (inda))
{
  maybe_mutate ();
}

octave_value::octave_value (const int64NDArray& inda)
  : rep (new octave_int64_matrix (inda))
{
  maybe_mutate ();
}

octave_value::octave_value (const Array<octave_int64>& inda)
  : rep (new octave_int64_matrix (inda))
{
  maybe_mutate ();
}

octave_value::octave_value (const uint64NDArray& inda)
  : rep (new octave_uint64_matrix (inda))
{
  maybe_mutate ();
}

octave_value::octave_value (const Array<octave_uint64>& inda)
  : rep (new octave_uint64_matrix (inda))
{
  maybe_mutate ();
}

octave_value::octave_value (const Array<octave_idx_type>& inda, bool zero_based,
                            bool cache_index)
  : rep (new octave_matrix (inda, zero_based, cache_index))
{
  maybe_mutate ();
}

octave_value::octave_value (const idx_vector& idx, bool lazy)
  : rep ()
{
  double scalar;
  Range range;
  NDArray array;
  boolNDArray mask;
  idx_vector::idx_class_type idx_class;

  if (lazy)
    {
      // Only make lazy indices out of ranges and index vectors.
      switch (idx.idx_class ())
        {
        case idx_vector::class_range:
        case idx_vector::class_vector:
          rep = new octave_lazy_index (idx);
          maybe_mutate ();
          return;
        default:
          break;
        }
    }

  idx.unconvert (idx_class, scalar, range, array, mask);

  switch (idx_class)
    {
    case idx_vector::class_colon:
      rep = new octave_magic_colon ();
      break;
    case idx_vector::class_range:
      rep = new octave_range (range, idx);
      break;
    case idx_vector::class_scalar:
      rep = new octave_scalar (scalar);
      break;
    case idx_vector::class_vector:
      rep = new octave_matrix (array, idx);
      break;
    case idx_vector::class_mask:
      rep = new octave_bool_matrix (mask, idx);
      break;
    default:
      assert (false);
      break;
    }

  // FIXME: needed?
  maybe_mutate ();
}

octave_value::octave_value (const Array<std::string>& cellstr)
  : rep (new octave_cell (cellstr))
{
  maybe_mutate ();
}

octave_value::octave_value (double base, double limit, double inc)
  : rep (new octave_range (base, limit, inc))
{
  maybe_mutate ();
}

octave_value::octave_value (const Range& r, bool force_range)
  : rep (force_range || ! Vdisable_range
         ? dynamic_cast<octave_base_value *> (new octave_range (r))
         : dynamic_cast<octave_base_value *> (new octave_matrix (r.matrix_value ())))
{
  maybe_mutate ();
}

octave_value::octave_value (const octave_map& m)
  : rep (new octave_struct (m))
{
  maybe_mutate ();
}

octave_value::octave_value (const octave_scalar_map& m)
  : rep (new octave_scalar_struct (m))
{
}

octave_value::octave_value (const octave_map& m, const std::string& id,
                            const std::list<std::string>& plist)
  : rep (new octave_class (m, id, plist))
{
  maybe_mutate ();
}

octave_value::octave_value (const octave_scalar_map& m, const std::string& id,
                            const std::list<std::string>& plist)
  : rep (new octave_class (m, id, plist))
{
}

octave_value::octave_value (const octave_value_list& l, bool)
  : rep (new octave_cs_list (l))
{
}

octave_value::octave_value (octave_value::magic_colon)
  : rep (new octave_magic_colon ())
{
}

octave_value::octave_value (octave_base_value *new_rep, bool borrow)
  : rep (new_rep)
{
  if (borrow)
    rep->count++;
}

octave_value::octave_value (octave_base_value *new_rep, int xcount)
  : rep (new_rep)
{
  rep->count = xcount;
}

octave_base_value *
octave_value::clone (void) const
{
  return rep->clone ();
}

void
octave_value::maybe_mutate (void)
{
  octave_base_value *tmp = rep->try_narrowing_conversion ();

  if (tmp && tmp != rep)
    {
      if (--rep->count == 0)
        delete rep;

      rep = tmp;
    }
}

octave_value
octave_value::single_subsref (const std::string& type,
                              const octave_value_list& idx)
{
  std::list<octave_value_list> i;

  i.push_back (idx);

  return rep->subsref (type, i);
}

octave_value_list
octave_value::subsref (const std::string& type,
                       const std::list<octave_value_list>& idx, int nargout)
{
  if (nargout == 1)
    return rep->subsref (type, idx);
  else
    return rep->subsref (type, idx, nargout);
}

octave_value_list
octave_value::subsref (const std::string& type,
                       const std::list<octave_value_list>& idx, int nargout,
                       const std::list<octave_lvalue> *lvalue_list)
{
  if (lvalue_list)
    return rep->subsref (type, idx, nargout, lvalue_list);
  else
    return subsref (type, idx, nargout);
}

octave_value
octave_value::next_subsref (const std::string& type,
                            const std::list<octave_value_list>& idx,
                            size_t skip)
{
  if (! error_state && idx.size () > skip)
    {
      std::list<octave_value_list> new_idx (idx);
      for (size_t i = 0; i < skip; i++)
        new_idx.erase (new_idx.begin ());
      return subsref (type.substr (skip), new_idx);
    }
  else
    return *this;
}

octave_value_list
octave_value::next_subsref (int nargout, const std::string& type,
                            const std::list<octave_value_list>& idx,
                            size_t skip)
{
  if (! error_state && idx.size () > skip)
    {
      std::list<octave_value_list> new_idx (idx);
      for (size_t i = 0; i < skip; i++)
        new_idx.erase (new_idx.begin ());
      return subsref (type.substr (skip), new_idx, nargout);
    }
  else
    return *this;
}

octave_value_list
octave_value::next_subsref (int nargout, const std::string& type,
                            const std::list<octave_value_list>& idx,
                            const std::list<octave_lvalue> *lvalue_list,
                            size_t skip)
{
  if (! error_state && idx.size () > skip)
    {
      std::list<octave_value_list> new_idx (idx);
      for (size_t i = 0; i < skip; i++)
        new_idx.erase (new_idx.begin ());
      return subsref (type.substr (skip), new_idx, nargout, lvalue_list);
    }
  else
    return *this;
}

octave_value
octave_value::next_subsref (bool auto_add, const std::string& type,
                            const std::list<octave_value_list>& idx,
                            size_t skip)
{
  if (! error_state && idx.size () > skip)
    {
      std::list<octave_value_list> new_idx (idx);
      for (size_t i = 0; i < skip; i++)
        new_idx.erase (new_idx.begin ());
      return subsref (type.substr (skip), new_idx, auto_add);
    }
  else
    return *this;
}

octave_value_list
octave_value::do_multi_index_op (int nargout, const octave_value_list& idx)
{
  return rep->do_multi_index_op (nargout, idx);
}

octave_value_list
octave_value::do_multi_index_op (int nargout, const octave_value_list& idx,
                                 const std::list<octave_lvalue> *lvalue_list)
{
  return rep->do_multi_index_op (nargout, idx, lvalue_list);
}

#if 0
static void
gripe_assign_failed (const std::string& on, const std::string& tn1,
                     const std::string& tn2)
{
  error ("assignment failed for '%s %s %s'",
         tn1.c_str (), on.c_str (), tn2.c_str ());
}
#endif

static void
gripe_assign_failed_or_no_method (const std::string& on,
                                  const std::string& tn1,
                                  const std::string& tn2)
{
  error ("assignment failed, or no method for '%s %s %s'",
         tn1.c_str (), on.c_str (), tn2.c_str ());
}

octave_value
octave_value::subsasgn (const std::string& type,
                        const std::list<octave_value_list>& idx,
                        const octave_value& rhs)
{
  return rep->subsasgn (type, idx, rhs);
}

octave_value
octave_value::undef_subsasgn (const std::string& type,
                              const std::list<octave_value_list>& idx,
                              const octave_value& rhs)
{
  return rep->undef_subsasgn (type, idx, rhs);
}

octave_value&
octave_value::assign (assign_op op, const std::string& type,
                      const std::list<octave_value_list>& idx,
                      const octave_value& rhs)
{
  octave_value retval;

  make_unique ();

  octave_value t_rhs = rhs;

  if (op != op_asn_eq)
    {
      if (is_defined ())
        {
          octave_value t = subsref (type, idx);

          if (! error_state)
            {
              binary_op binop = op_eq_to_binary_op (op);

              if (! error_state)
                t_rhs = do_binary_op (binop, t, rhs);
            }
        }
      else
        error ("in computed assignment A(index) OP= X, A must be defined first");
    }

  if (! error_state)
    {
      octave_value tmp = subsasgn (type, idx, t_rhs);

      if (error_state)
        gripe_assign_failed_or_no_method (assign_op_as_string (op_asn_eq),
                                          type_name (), rhs.type_name ());
      else
        *this = tmp;
    }

  return *this;
}

octave_value&
octave_value::assign (assign_op op, const octave_value& rhs)
{
  if (op == op_asn_eq)
    // Regularize a null matrix if stored into a variable.
    operator = (rhs.storable_value ());
  else if (is_defined ())
    {
      octave_value_typeinfo::assign_op_fcn f = 0;

      // Only attempt to operate in-place if this variable is unshared.
      if (rep->count == 1)
        {
          int tthis = this->type_id ();
          int trhs = rhs.type_id ();

          f = octave_value_typeinfo::lookup_assign_op (op, tthis, trhs);
        }

      if (f)
        {
          try
            {
              f (*rep, octave_value_list (), *rhs.rep);
              // Usually unnecessary, but may be needed (complex arrays).
              maybe_mutate ();
            }
          catch (octave_execution_exception)
            {
              gripe_library_execution_error ();
            }
        }
      else
        {

          binary_op binop = op_eq_to_binary_op (op);

          if (! error_state)
            {
              octave_value t = do_binary_op (binop, *this, rhs);

              if (! error_state)
                operator = (t);
            }
        }
    }
  else
    error ("in computed assignment A OP= X, A must be defined first");

  return *this;
}

octave_idx_type
octave_value::length (void) const
{
  octave_idx_type retval = 0;

  const dim_vector dv = dims ();

  for (int i = 0; i < dv.length (); i++)
    {
      if (dv(i) == 0)
        {
          retval = 0;
          break;
        }

      if (dv(i) > retval)
        retval = dv(i);
    }

  return retval;
}

bool
octave_value::is_equal (const octave_value& test) const
{
  bool retval = false;

  // If there is no op_eq for these types, we can't compare values.

  if (rows () == test.rows () && columns () == test.columns ())
    {
      octave_value tmp = do_binary_op (octave_value::op_eq, *this, test);

      // Empty array also means a match.
      if (! error_state && tmp.is_defined ())
        retval = tmp.is_true () || tmp.is_empty ();
    }

  return retval;
}

Cell
octave_value::cell_value (void) const
{
  return rep->cell_value ();
}

// Define the idx_type_value function here instead of in ov.h to avoid
// needing definitions for the SIZEOF_X macros in ov.h.

octave_idx_type
octave_value::idx_type_value (bool req_int, bool frc_str_conv) const
{
#if defined (USE_64_BIT_IDX_T)
  return int64_value (req_int, frc_str_conv);
#else
  return int_value (req_int, frc_str_conv);
#endif
}

octave_map
octave_value::map_value (void) const
{
  return rep->map_value ();
}

octave_scalar_map
octave_value::scalar_map_value (void) const
{
  return rep->scalar_map_value ();
}

octave_function *
octave_value::function_value (bool silent) const
{
  return rep->function_value (silent);
}

octave_user_function *
octave_value::user_function_value (bool silent) const
{
  return rep->user_function_value (silent);
}

octave_user_script *
octave_value::user_script_value (bool silent) const
{
  return rep->user_script_value (silent);
}

octave_user_code *
octave_value::user_code_value (bool silent) const
{
  return rep->user_code_value (silent);
}

octave_fcn_handle *
octave_value::fcn_handle_value (bool silent) const
{
  return rep->fcn_handle_value (silent);
}

octave_fcn_inline *
octave_value::fcn_inline_value (bool silent) const
{
  return rep->fcn_inline_value (silent);
}

octave_value_list
octave_value::list_value (void) const
{
  return rep->list_value ();
}

static dim_vector
make_vector_dims (const dim_vector& dv, bool force_vector_conversion,
                  const std::string& my_type, const std::string& wanted_type)
{
  dim_vector retval (dv);
  retval.chop_trailing_singletons ();
  octave_idx_type nel = dv.numel ();

  if (retval.length () > 2 || (retval(0) != 1 && retval(1) != 1))
    {
      if (!force_vector_conversion)
        gripe_implicit_conversion ("Octave:array-to-vector",
                                   my_type.c_str (), wanted_type.c_str ());
      retval = dim_vector (nel, 1);
    }

  return retval;
}

ColumnVector
octave_value::column_vector_value (bool force_string_conv,
                                   bool frc_vec_conv) const
{
  return ColumnVector (vector_value (force_string_conv,
                                     frc_vec_conv));
}

ComplexColumnVector
octave_value::complex_column_vector_value (bool force_string_conv,
                                           bool frc_vec_conv) const
{
  return ComplexColumnVector (complex_vector_value (force_string_conv,
                                                    frc_vec_conv));
}

RowVector
octave_value::row_vector_value (bool force_string_conv,
                                bool frc_vec_conv) const
{
  return RowVector (vector_value (force_string_conv,
                                  frc_vec_conv));
}

ComplexRowVector
octave_value::complex_row_vector_value (bool force_string_conv,
                                        bool frc_vec_conv) const
{
  return ComplexRowVector (complex_vector_value (force_string_conv,
                                                 frc_vec_conv));
}

Array<double>
octave_value::vector_value (bool force_string_conv,
                            bool force_vector_conversion) const
{
  Array<double> retval = array_value (force_string_conv);

  if (error_state)
    return retval;
  else
    return retval.reshape (make_vector_dims (retval.dims (),
                                             force_vector_conversion,
                                             type_name (), "real vector"));
}

template <class T>
static Array<int>
convert_to_int_array (const Array<octave_int<T> >& A)
{
  Array<int> retval (A.dims ());
  octave_idx_type n = A.numel ();

  for (octave_idx_type i = 0; i < n; i++)
    retval.xelem (i) = octave_int<int> (A.xelem (i));

  return retval;
}

Array<int>
octave_value::int_vector_value (bool force_string_conv, bool require_int,
                                bool force_vector_conversion) const
{
  Array<int> retval;

  if (is_integer_type ())
    {
      if (is_int32_type ())
        retval = convert_to_int_array (int32_array_value ());
      else if (is_int64_type ())
        retval = convert_to_int_array (int64_array_value ());
      else if (is_int16_type ())
        retval = convert_to_int_array (int16_array_value ());
      else if (is_int8_type ())
        retval = convert_to_int_array (int8_array_value ());
      else if (is_uint32_type ())
        retval = convert_to_int_array (uint32_array_value ());
      else if (is_uint64_type ())
        retval = convert_to_int_array (uint64_array_value ());
      else if (is_uint16_type ())
        retval = convert_to_int_array (uint16_array_value ());
      else if (is_uint8_type ())
        retval = convert_to_int_array (uint8_array_value ());
      else
        retval = array_value (force_string_conv);
    }
  else
    {
      const NDArray a = array_value (force_string_conv);
      if (! error_state)
        {
          if (require_int)
            {
              retval.resize (a.dims ());
              for (octave_idx_type i = 0; i < a.numel (); i++)
                {
                  double ai = a.elem (i);
                  int v = static_cast<int> (ai);
                  if (ai == v)
                    retval.xelem (i) = v;
                  else
                    {
                      error_with_cfn ("conversion to integer value failed");
                      break;
                    }
                }
            }
          else
            retval = Array<int> (a);
        }
    }


  if (error_state)
    return retval;
  else
    return retval.reshape (make_vector_dims (retval.dims (),
                                             force_vector_conversion,
                                             type_name (), "integer vector"));
}

template <class T>
static Array<octave_idx_type>
convert_to_octave_idx_type_array (const Array<octave_int<T> >& A)
{
  Array<octave_idx_type> retval (A.dims ());
  octave_idx_type n = A.numel ();

  for (octave_idx_type i = 0; i < n; i++)
    retval.xelem (i) = octave_int<octave_idx_type> (A.xelem (i));

  return retval;
}

Array<octave_idx_type>
octave_value::octave_idx_type_vector_value (bool require_int,
                                            bool force_string_conv,
                                            bool force_vector_conversion) const
{
  Array<octave_idx_type> retval;

  if (is_integer_type ())
    {
      if (is_int32_type ())
        retval = convert_to_octave_idx_type_array (int32_array_value ());
      else if (is_int64_type ())
        retval = convert_to_octave_idx_type_array (int64_array_value ());
      else if (is_int16_type ())
        retval = convert_to_octave_idx_type_array (int16_array_value ());
      else if (is_int8_type ())
        retval = convert_to_octave_idx_type_array (int8_array_value ());
      else if (is_uint32_type ())
        retval = convert_to_octave_idx_type_array (uint32_array_value ());
      else if (is_uint64_type ())
        retval = convert_to_octave_idx_type_array (uint64_array_value ());
      else if (is_uint16_type ())
        retval = convert_to_octave_idx_type_array (uint16_array_value ());
      else if (is_uint8_type ())
        retval = convert_to_octave_idx_type_array (uint8_array_value ());
      else
        retval = array_value (force_string_conv);
    }
  else
    {
      const NDArray a = array_value (force_string_conv);
      if (! error_state)
        {
          if (require_int)
            {
              retval.resize (a.dims ());
              for (octave_idx_type i = 0; i < a.numel (); i++)
                {
                  double ai = a.elem (i);
                  octave_idx_type v = static_cast<octave_idx_type> (ai);
                  if (ai == v)
                    retval.xelem (i) = v;
                  else
                    {
                      error_with_cfn ("conversion to integer value failed");
                      break;
                    }
                }
            }
          else
            retval = Array<octave_idx_type> (a);
        }
    }


  if (error_state)
    return retval;
  else
    return retval.reshape (make_vector_dims (retval.dims (),
                                             force_vector_conversion,
                                             type_name (), "integer vector"));
}

Array<Complex>
octave_value::complex_vector_value (bool force_string_conv,
                                    bool force_vector_conversion) const
{
  Array<Complex> retval = complex_array_value (force_string_conv);

  if (error_state)
    return retval;
  else
    return retval.reshape (make_vector_dims (retval.dims (),
                                             force_vector_conversion,
                                             type_name (), "complex vector"));
}

FloatColumnVector
octave_value::float_column_vector_value (bool force_string_conv,
                                         bool frc_vec_conv) const
{
  return FloatColumnVector (float_vector_value (force_string_conv,
                                                frc_vec_conv));
}

FloatComplexColumnVector
octave_value::float_complex_column_vector_value (bool force_string_conv,
                                                 bool frc_vec_conv) const
{
  return
    FloatComplexColumnVector (float_complex_vector_value (force_string_conv,
                                                          frc_vec_conv));
}

FloatRowVector
octave_value::float_row_vector_value (bool force_string_conv,
                                      bool frc_vec_conv) const
{
  return FloatRowVector (float_vector_value (force_string_conv,
                                             frc_vec_conv));
}

FloatComplexRowVector
octave_value::float_complex_row_vector_value (bool force_string_conv,
                                              bool frc_vec_conv) const
{
  return FloatComplexRowVector (float_complex_vector_value (force_string_conv,
                                                           frc_vec_conv));
}

Array<float>
octave_value::float_vector_value (bool force_string_conv,
                                  bool force_vector_conversion) const
{
  Array<float> retval = float_array_value (force_string_conv);

  if (error_state)
    return retval;
  else
    return retval.reshape (make_vector_dims (retval.dims (),
                                             force_vector_conversion,
                                             type_name (), "real vector"));
}

Array<FloatComplex>
octave_value::float_complex_vector_value (bool force_string_conv,
                                          bool force_vector_conversion) const
{
  Array<FloatComplex> retval = float_complex_array_value (force_string_conv);

  if (error_state)
    return retval;
  else
    return retval.reshape (make_vector_dims (retval.dims (),
                                             force_vector_conversion,
                                             type_name (), "complex vector"));
}

octave_value
octave_value::storable_value (void) const
{
  octave_value retval = *this;
  if (is_null_value ())
    retval = octave_value (rep->empty_clone ());
  else
    retval.maybe_economize ();

  return retval;
}

void
octave_value::make_storable_value (void)
{
  if (is_null_value ())
    {
      octave_base_value *rc = rep->empty_clone ();
      if (--rep->count == 0)
        delete rep;
      rep = rc;
    }
  else
    maybe_economize ();
}

int
octave_value::write (octave_stream& os, int block_size,
                     oct_data_conv::data_type output_type, int skip,
                     oct_mach_info::float_format flt_fmt) const
{
  return rep->write (os, block_size, output_type, skip, flt_fmt);
}

static void
gripe_binary_op (const std::string& on, const std::string& tn1,
                 const std::string& tn2)
{
  error ("binary operator '%s' not implemented for '%s' by '%s' operations",
         on.c_str (), tn1.c_str (), tn2.c_str ());
}

static void
gripe_binary_op_conv (const std::string& on)
{
  error ("type conversion failed for binary operator '%s'", on.c_str ());
}

octave_value
do_binary_op (octave_value::binary_op op,
              const octave_value& v1, const octave_value& v2)
{
  octave_value retval;

  int t1 = v1.type_id ();
  int t2 = v2.type_id ();

  if (t1 == octave_class::static_type_id ()
      || t2 == octave_class::static_type_id ()
      || t1 == octave_classdef::static_type_id ()
      || t2 == octave_classdef::static_type_id ())
    {
      octave_value_typeinfo::binary_class_op_fcn f
        = octave_value_typeinfo::lookup_binary_class_op (op);

      if (f)
        {
          try
            {
              retval = f (v1, v2);
            }
          catch (octave_execution_exception)
            {
              gripe_library_execution_error ();
            }
        }
      else
        gripe_binary_op (octave_value::binary_op_as_string (op),
                         v1.class_name (), v2.class_name ());
    }
  else
    {
      // FIXME: we need to handle overloading operators for built-in
      // classes (double, char, int8, etc.)

      octave_value_typeinfo::binary_op_fcn f
        = octave_value_typeinfo::lookup_binary_op (op, t1, t2);

      if (f)
        {
          try
            {
              retval = f (*v1.rep, *v2.rep);
            }
          catch (octave_execution_exception)
            {
              gripe_library_execution_error ();
            }
        }
      else
        {
          octave_value tv1;
          octave_base_value::type_conv_info cf1 =
            v1.numeric_conversion_function ();

          octave_value tv2;
          octave_base_value::type_conv_info cf2 =
            v2.numeric_conversion_function ();

          // Try biased (one-sided) conversions first.
          if (cf2.type_id () >= 0
              && octave_value_typeinfo::lookup_binary_op (op, t1,
                                                          cf2.type_id ()))
            cf1 = 0;
          else if (cf1.type_id () >= 0
                   && octave_value_typeinfo::lookup_binary_op (op,
                                                               cf1.type_id (),
                                                               t2))
            cf2 = 0;

          if (cf1)
            {
              octave_base_value *tmp = cf1 (*v1.rep);

              if (tmp)
                {
                  tv1 = octave_value (tmp);
                  t1 = tv1.type_id ();
                }
              else
                {
                  gripe_binary_op_conv (octave_value::binary_op_as_string (op));
                  return retval;
                }
            }
          else
            tv1 = v1;

          if (cf2)
            {
              octave_base_value *tmp = cf2 (*v2.rep);

              if (tmp)
                {
                  tv2 = octave_value (tmp);
                  t2 = tv2.type_id ();
                }
              else
                {
                  gripe_binary_op_conv (octave_value::binary_op_as_string (op));
                  return retval;
                }
            }
          else
            tv2 = v2;

          if (cf1 || cf2)
            {
              retval = do_binary_op (op, tv1, tv2);
            }
          else
            {
              //demote double -> single and try again
              cf1 = tv1.numeric_demotion_function ();

              cf2 = tv2.numeric_demotion_function ();

              // Try biased (one-sided) conversions first.
              if (cf2.type_id () >= 0
                  && octave_value_typeinfo::lookup_binary_op (op, t1,
                                                              cf2.type_id ()))
                cf1 = 0;
              else if (cf1.type_id () >= 0
                       && octave_value_typeinfo::lookup_binary_op (op,
                                                                   cf1.type_id (),
                                                                   t2))
                cf2 = 0;

              if (cf1)
                {
                  octave_base_value *tmp = cf1 (*tv1.rep);

                  if (tmp)
                    {
                      tv1 = octave_value (tmp);
                      t1 = tv1.type_id ();
                    }
                  else
                    {
                      gripe_binary_op_conv
                        (octave_value::binary_op_as_string (op));
                      return retval;
                    }
                }

              if (cf2)
                {
                  octave_base_value *tmp = cf2 (*tv2.rep);

                  if (tmp)
                    {
                      tv2 = octave_value (tmp);
                      t2 = tv2.type_id ();
                    }
                  else
                    {
                      gripe_binary_op_conv
                        (octave_value::binary_op_as_string (op));
                      return retval;
                    }
                }

              if (cf1 || cf2)
                {
                  f = octave_value_typeinfo::lookup_binary_op (op, t1, t2);

                  if (f)
                    {
                      try
                        {
                          retval = f (*tv1.rep, *tv2.rep);
                        }
                      catch (octave_execution_exception)
                        {
                          gripe_library_execution_error ();
                        }
                    }
                  else
                    gripe_binary_op (octave_value::binary_op_as_string (op),
                                     v1.type_name (), v2.type_name ());
                }
              else
                gripe_binary_op (octave_value::binary_op_as_string (op),
                                 v1.type_name (), v2.type_name ());
            }
        }
    }

  return retval;
}

static octave_value
decompose_binary_op (octave_value::compound_binary_op op,
                     const octave_value& v1, const octave_value& v2)
{
  octave_value retval;

  switch (op)
    {
    case octave_value::op_trans_mul:
      retval = do_binary_op (octave_value::op_mul,
                             do_unary_op (octave_value::op_transpose, v1),
                             v2);
      break;
    case octave_value::op_mul_trans:
      retval = do_binary_op (octave_value::op_mul,
                             v1,
                             do_unary_op (octave_value::op_transpose, v2));
      break;
    case octave_value::op_herm_mul:
      retval = do_binary_op (octave_value::op_mul,
                             do_unary_op (octave_value::op_hermitian, v1),
                             v2);
      break;
    case octave_value::op_mul_herm:
      retval = do_binary_op (octave_value::op_mul,
                             v1,
                             do_unary_op (octave_value::op_hermitian, v2));
      break;
    case octave_value::op_trans_ldiv:
      retval = do_binary_op (octave_value::op_ldiv,
                             do_unary_op (octave_value::op_transpose, v1),
                             v2);
      break;
    case octave_value::op_herm_ldiv:
      retval = do_binary_op (octave_value::op_ldiv,
                             do_unary_op (octave_value::op_hermitian, v1),
                             v2);
      break;
    case octave_value::op_el_not_and:
      retval = do_binary_op (octave_value::op_el_and,
                             do_unary_op (octave_value::op_not, v1),
                             v2);
      break;
    case octave_value::op_el_not_or:
      retval = do_binary_op (octave_value::op_el_or,
                             do_unary_op (octave_value::op_not, v1),
                             v2);
      break;
    case octave_value::op_el_and_not:
      retval = do_binary_op (octave_value::op_el_and,
                             v1,
                             do_unary_op (octave_value::op_not, v2));
      break;
    case octave_value::op_el_or_not:
      retval = do_binary_op (octave_value::op_el_or,
                             v1,
                             do_unary_op (octave_value::op_not, v2));
      break;
    default:
      error ("invalid compound operator");
      break;
    }

  return retval;
}

octave_value
do_binary_op (octave_value::compound_binary_op op,
              const octave_value& v1, const octave_value& v2)
{
  octave_value retval;

  int t1 = v1.type_id ();
  int t2 = v2.type_id ();

  if (t1 == octave_class::static_type_id ()
      || t2 == octave_class::static_type_id ()
      || t1 == octave_classdef::static_type_id ()
      || t2 == octave_classdef::static_type_id ())
    {
      octave_value_typeinfo::binary_class_op_fcn f
        = octave_value_typeinfo::lookup_binary_class_op (op);

      if (f)
        {
          try
            {
              retval = f (v1, v2);
            }
          catch (octave_execution_exception)
            {
              gripe_library_execution_error ();
            }
        }
      else
        retval = decompose_binary_op (op, v1, v2);
    }
  else
    {
      octave_value_typeinfo::binary_op_fcn f
        = octave_value_typeinfo::lookup_binary_op (op, t1, t2);

      if (f)
        {
          try
            {
              retval = f (*v1.rep, *v2.rep);
            }
          catch (octave_execution_exception)
            {
              gripe_library_execution_error ();
            }
        }
      else
        retval = decompose_binary_op (op, v1, v2);
    }

  return retval;
}

static void
gripe_cat_op (const std::string& tn1, const std::string& tn2)
{
  error ("concatenation operator not implemented for '%s' by '%s' operations",
         tn1.c_str (), tn2.c_str ());
}

static void
gripe_cat_op_conv (void)
{
  error ("type conversion failed for concatenation operator");
}

octave_value
do_cat_op (const octave_value& v1, const octave_value& v2,
           const Array<octave_idx_type>& ra_idx)
{
  octave_value retval;

  // Can't rapid return for concatenation with an empty object here as
  // something like cat(1,[],single([]) must return the correct type.

  int t1 = v1.type_id ();
  int t2 = v2.type_id ();

  octave_value_typeinfo::cat_op_fcn f
    = octave_value_typeinfo::lookup_cat_op (t1, t2);

  if (f)
    {
      try
        {
          retval = f (*v1.rep, *v2.rep, ra_idx);
        }
      catch (octave_execution_exception)
        {
          gripe_library_execution_error ();
        }
    }
  else
    {
      octave_value tv1;
      octave_base_value::type_conv_info cf1 = v1.numeric_conversion_function ();

      octave_value tv2;
      octave_base_value::type_conv_info cf2 = v2.numeric_conversion_function ();

      // Try biased (one-sided) conversions first.
      if (cf2.type_id () >= 0
          && octave_value_typeinfo::lookup_cat_op (t1, cf2.type_id ()))
        cf1 = 0;
      else if (cf1.type_id () >= 0
               && octave_value_typeinfo::lookup_cat_op (cf1.type_id (), t2))
        cf2 = 0;

      if (cf1)
        {
          octave_base_value *tmp = cf1 (*v1.rep);

          if (tmp)
            {
              tv1 = octave_value (tmp);
              t1 = tv1.type_id ();
            }
          else
            {
              gripe_cat_op_conv ();
              return retval;
            }
        }
      else
        tv1 = v1;

      if (cf2)
        {
          octave_base_value *tmp = cf2 (*v2.rep);

          if (tmp)
            {
              tv2 = octave_value (tmp);
              t2 = tv2.type_id ();
            }
          else
            {
              gripe_cat_op_conv ();
              return retval;
            }
        }
      else
        tv2 = v2;

      if (cf1 || cf2)
        {
          retval = do_cat_op (tv1, tv2, ra_idx);
        }
      else
        gripe_cat_op (v1.type_name (), v2.type_name ());
    }

  return retval;
}

octave_value
do_colon_op (const octave_value& base, const octave_value& increment,
             const octave_value& limit, bool is_for_cmd_expr)
{
  octave_value retval;

  if (base.is_object () || increment.is_object () || limit.is_object ())
    {
      std::string dispatch_type;

      if (base.is_object ())
        dispatch_type = base.class_name ();
      else if (increment.is_defined () && increment.is_object ())
        dispatch_type = increment.class_name ();
      else
        dispatch_type = limit.class_name ();

      octave_value meth = symbol_table::find_method ("colon", dispatch_type);

      if (meth.is_defined ())
        {
          octave_value_list args;

          if (increment.is_defined ())
            {
              args(2) = limit;
              args(1) = increment;
            }
          else
            args(1) = limit;

          args(0) = base;

          octave_value_list tmp = feval (meth.function_value (), args, 1);

          if (tmp.length () > 0)
            retval = tmp(0);
        }
      else
        error ("colon method not defined for %s class", dispatch_type.c_str ());
    }
  else
    {
      bool result_is_str = (base.is_string () && limit.is_string ());
      bool dq_str = (base.is_dq_string () || limit.is_dq_string ());

      Matrix m_base = base.matrix_value (true);

      if (error_state)
        {
          error ("invalid base value in colon expression");
          return retval;
        }

      Matrix m_limit = limit.matrix_value (true);

      if (error_state)
        {
          error ("invalid limit value in colon expression");
          return retval;
        }

      Matrix m_increment = (increment.is_defined ()
                            ? increment.matrix_value (true)
                            : Matrix (1, 1, 1.0));

      if (error_state)
        {
          error ("invalid increment value in colon expression");
          return retval;
        }

      bool base_empty = m_base.is_empty ();
      bool limit_empty = m_limit.is_empty ();
      bool increment_empty = m_increment.is_empty ();

      if (base_empty || limit_empty || increment_empty)
        retval = Range ();
      else
        {
          Range r (m_base(0), m_limit(0), m_increment(0));

          // For compatibility with Matlab, don't allow the range used in
          // a FOR loop expression to be converted to a Matrix.

          retval = octave_value (r, is_for_cmd_expr);

          if (result_is_str)
            retval = retval.convert_to_str (false, true, dq_str ? '"' : '\'');
        }
    }

  return retval;
}

void
octave_value::print_info (std::ostream& os, const std::string& prefix) const
{
  os << prefix << "type_name: " << type_name () << "\n"
     << prefix << "count:     " << get_count () << "\n"
     << prefix << "rep info:  ";

  rep->print_info (os, prefix + " ");
}

static void
gripe_unary_op (const std::string& on, const std::string& tn)
{
  error ("unary operator '%s' not implemented for '%s' operands",
         on.c_str (), tn.c_str ());
}

static void
gripe_unary_op_conv (const std::string& on)
{
  error ("type conversion failed for unary operator '%s'", on.c_str ());
}

octave_value
do_unary_op (octave_value::unary_op op, const octave_value& v)
{
  octave_value retval;

  int t = v.type_id ();

  if (t == octave_class::static_type_id ()
      || t == octave_classdef::static_type_id ())
    {
      octave_value_typeinfo::unary_class_op_fcn f
        = octave_value_typeinfo::lookup_unary_class_op (op);

      if (f)
        {
          try
            {
              retval = f (v);
            }
          catch (octave_execution_exception)
            {
              gripe_library_execution_error ();
            }
        }
      else
        gripe_unary_op (octave_value::unary_op_as_string (op),
                        v.class_name ());
    }
  else
    {
      // FIXME: we need to handle overloading operators for built-in
      // classes (double, char, int8, etc.)

      octave_value_typeinfo::unary_op_fcn f
        = octave_value_typeinfo::lookup_unary_op (op, t);

      if (f)
        {
          try
            {
              retval = f (*v.rep);
            }
          catch (octave_execution_exception)
            {
              gripe_library_execution_error ();
            }
        }
      else
        {
          octave_value tv;
          octave_base_value::type_conv_fcn cf
            = v.numeric_conversion_function ();

          if (cf)
            {
              octave_base_value *tmp = cf (*v.rep);

              if (tmp)
                {
                  tv = octave_value (tmp);
                  retval = do_unary_op (op, tv);
                }
              else
                gripe_unary_op_conv (octave_value::unary_op_as_string (op));
            }
          else
            gripe_unary_op (octave_value::unary_op_as_string (op),
                            v.type_name ());
        }
    }

  return retval;
}

static void
gripe_unary_op_conversion_failed (const std::string& op,
                                  const std::string& tn)
{
  error ("operator %s: type conversion for '%s' failed",
         op.c_str (), tn.c_str ());
}

octave_value&
octave_value::do_non_const_unary_op (unary_op op)
{
  if (op == op_incr || op == op_decr)
    {
      // We want the gripe just here, because in the other branch this should
      // not happen, and if it did anyway (internal error), the message would
      // be confusing.
      if (is_undefined ())
        {
          std::string op_str = unary_op_as_string (op);
          error ("in x%s or %sx, x must be defined first",
                 op_str.c_str (), op_str.c_str ());
          return *this;
        }

      // Genuine.
      int t = type_id ();

      octave_value_typeinfo::non_const_unary_op_fcn f
        = octave_value_typeinfo::lookup_non_const_unary_op (op, t);

      if (f)
        {
          make_unique ();

          try
            {
              f (*rep);
            }
          catch (octave_execution_exception)
            {
              gripe_library_execution_error ();
            }
        }
      else
        {
          octave_base_value::type_conv_fcn cf = numeric_conversion_function ();

          if (cf)
            {
              octave_base_value *tmp = cf (*rep);

              if (tmp)
                {
                  octave_base_value *old_rep = rep;
                  rep = tmp;

                  t = type_id ();

                  f = octave_value_typeinfo::lookup_non_const_unary_op (op, t);

                  if (f)
                    {
                      try
                        {
                          f (*rep);
                        }
                      catch (octave_execution_exception)
                        {
                          gripe_library_execution_error ();
                        }

                      if (old_rep && --old_rep->count == 0)
                        delete old_rep;
                    }
                  else
                    {
                      if (old_rep)
                        {
                          if (--rep->count == 0)
                            delete rep;

                          rep = old_rep;
                        }

                      gripe_unary_op (octave_value::unary_op_as_string (op),
                                      type_name ());
                    }
                }
              else
                gripe_unary_op_conversion_failed
                  (octave_value::unary_op_as_string (op), type_name ());
            }
          else
            gripe_unary_op (octave_value::unary_op_as_string (op),
                            type_name ());
        }
    }
  else
    {
      // Non-genuine.
      int t = type_id ();

      octave_value_typeinfo::non_const_unary_op_fcn f = 0;

      // Only attempt to operate in-place if this variable is unshared.
      if (rep->count == 1)
        f = octave_value_typeinfo::lookup_non_const_unary_op (op, t);

      if (f)
        {
          try
            {
              f (*rep);
            }
          catch (octave_execution_exception)
            {
              gripe_library_execution_error ();
            }
        }
      else
        *this = do_unary_op (op, *this);
    }

  return *this;
}

octave_value&
octave_value::do_non_const_unary_op (unary_op op, const std::string& type,
                                     const std::list<octave_value_list>& idx)
{
  if (idx.empty ())
    do_non_const_unary_op (op);
  else
    {
      // FIXME: only do the following stuff if we can't find a
      // specific function to call to handle the op= operation for the
      // types we have.

      assign_op assop = unary_op_to_assign_op (op);

      assign (assop, type, idx, 1.0);
    }

  return *this;
}

octave_value::assign_op
octave_value::unary_op_to_assign_op (unary_op op)
{
  assign_op binop = unknown_assign_op;

  switch (op)
    {
    case op_incr:
      binop = op_add_eq;
      break;

    case op_decr:
      binop = op_sub_eq;
      break;

    default:
      {
        std::string on = unary_op_as_string (op);
        error ("operator %s: no assign operator found", on.c_str ());
      }
    }

  return binop;
}

octave_value::binary_op
octave_value::op_eq_to_binary_op (assign_op op)
{
  binary_op binop = unknown_binary_op;

  switch (op)
    {
    case op_add_eq:
      binop = op_add;
      break;

    case op_sub_eq:
      binop = op_sub;
      break;

    case op_mul_eq:
      binop = op_mul;
      break;

    case op_div_eq:
      binop = op_div;
      break;

    case op_ldiv_eq:
      binop = op_ldiv;
      break;

    case op_pow_eq:
      binop = op_pow;
      break;

    case op_lshift_eq:
      binop = op_lshift;
      break;

    case op_rshift_eq:
      binop = op_rshift;
      break;

    case op_el_mul_eq:
      binop = op_el_mul;
      break;

    case op_el_div_eq:
      binop = op_el_div;
      break;

    case op_el_ldiv_eq:
      binop = op_el_ldiv;
      break;

    case op_el_pow_eq:
      binop = op_el_pow;
      break;

    case op_el_and_eq:
      binop = op_el_and;
      break;

    case op_el_or_eq:
      binop = op_el_or;
      break;

    default:
      {
        std::string on = assign_op_as_string (op);
        error ("operator %s: no binary operator found", on.c_str ());
      }
    }

  return binop;
}

octave_value
octave_value::empty_conv (const std::string& type, const octave_value& rhs)
{
  octave_value retval;

  if (type.length () > 0)
    {
      switch (type[0])
        {
        case '(':
          {
            if (type.length () > 1 && type[1] == '.')
              retval = octave_map ();
            else
              retval = octave_value (rhs.empty_clone ());
          }
          break;

        case '{':
          retval = Cell ();
          break;

        case '.':
          retval = octave_scalar_map ();
          break;

        default:
          panic_impossible ();
        }
    }
  else
    retval = octave_value (rhs.empty_clone ());

  return retval;
}

void
install_types (void)
{
  octave_base_value::register_type ();
  octave_cell::register_type ();
  octave_scalar::register_type ();
  octave_complex::register_type ();
  octave_matrix::register_type ();
  octave_diag_matrix::register_type ();
  octave_complex_matrix::register_type ();
  octave_complex_diag_matrix::register_type ();
  octave_range::register_type ();
  octave_bool::register_type ();
  octave_bool_matrix::register_type ();
  octave_char_matrix_str::register_type ();
  octave_char_matrix_sq_str::register_type ();
  octave_int8_scalar::register_type ();
  octave_int16_scalar::register_type ();
  octave_int32_scalar::register_type ();
  octave_int64_scalar::register_type ();
  octave_uint8_scalar::register_type ();
  octave_uint16_scalar::register_type ();
  octave_uint32_scalar::register_type ();
  octave_uint64_scalar::register_type ();
  octave_int8_matrix::register_type ();
  octave_int16_matrix::register_type ();
  octave_int32_matrix::register_type ();
  octave_int64_matrix::register_type ();
  octave_uint8_matrix::register_type ();
  octave_uint16_matrix::register_type ();
  octave_uint32_matrix::register_type ();
  octave_uint64_matrix::register_type ();
  octave_sparse_bool_matrix::register_type ();
  octave_sparse_matrix::register_type ();
  octave_sparse_complex_matrix::register_type ();
  octave_struct::register_type ();
  octave_scalar_struct::register_type ();
  octave_class::register_type ();
  octave_cs_list::register_type ();
  octave_magic_colon::register_type ();
  octave_builtin::register_type ();
  octave_user_function::register_type ();
  octave_dld_function::register_type ();
  octave_fcn_handle::register_type ();
  octave_fcn_inline::register_type ();
  octave_float_scalar::register_type ();
  octave_float_complex::register_type ();
  octave_float_matrix::register_type ();
  octave_float_diag_matrix::register_type ();
  octave_float_complex_matrix::register_type ();
  octave_float_complex_diag_matrix::register_type ();
  octave_perm_matrix::register_type ();
  octave_null_matrix::register_type ();
  octave_null_str::register_type ();
  octave_null_sq_str::register_type ();
  octave_lazy_index::register_type ();
  octave_oncleanup::register_type ();
#ifdef HAVE_JAVA
  octave_java::register_type ();
#endif
}

DEFUN (sizeof, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} sizeof (@var{val})\n\
Return the size of @var{val} in bytes.\n\
@seealso{whos}\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 1)
    retval = args(0).byte_size ();
  else
    print_usage ();

  return retval;
}

/*
%!assert (sizeof (uint64 (ones (3))), 72)
%!assert (sizeof (double (zeros (2,4))), 64)
%!assert (sizeof ({"foo", "bar", "baaz"}), 10)
*/

static void
decode_subscripts (const char* name, const octave_value& arg,
                   std::string& type_string,
                   std::list<octave_value_list>& idx)
{
  const octave_map m = arg.map_value ();

  if (! error_state
      && m.nfields () == 2 && m.contains ("type") && m.contains ("subs"))
    {
      octave_idx_type nel = m.numel ();

      type_string = std::string (nel, '\0');
      idx = std::list<octave_value_list> ();

      if (nel == 0)
        return;

      const Cell type = m.contents ("type");
      const Cell subs = m.contents ("subs");

      for (int k = 0; k < nel; k++)
        {
          if (type(k).is_string ())
            {
              std::string item = type(k).string_value ();
              if (item == "{}")
                type_string[k] = '{';
              else if (item == "()")
                type_string[k] = '(';
              else if (item == ".")
                type_string[k] = '.';
              else
                {
                  error ("%s: invalid indexing type '%s'", name, item.c_str ());
                  return;
                }
            }
          else
            {
              error ("%s: type(%d) must be a string", name, k+1);
              return;
            }

          octave_value_list idx_item;

          if (subs(k).is_string ())
            idx_item(0) = subs(k);
          else if (subs(k).is_cell ())
            {
              Cell subs_cell = subs(k).cell_value ();

              for (int n = 0; n < subs_cell.length (); n++)
                {
                  if (subs_cell(n).is_string ()
                      && subs_cell(n).string_value () == ":")
                    idx_item(n) = octave_value(octave_value::magic_colon_t);
                  else
                    idx_item(n) = subs_cell(n);
                }
            }
          else
            {
              error ("%s: subs(%d) must be a string or cell array", name, k+1);
              return;
            }

          idx.push_back (idx_item);
        }
    }
  else
    error ("%s: second argument must be a structure with fields 'type' and 'subs'",
           name);
}

DEFUN (subsref, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} subsref (@var{val}, @var{idx})\n\
Perform the subscripted element selection operation according to the\n\
subscript specified by @var{idx}.\n\
\n\
The subscript @var{idx} is expected to be a structure array with fields\n\
@samp{type} and @samp{subs}.  Valid values for @samp{type} are\n\
@samp{\"()\"}, @samp{\"@{@}\"}, and @samp{\".\"}.  The @samp{subs} field may\n\
be either @samp{\":\"} or a cell array of index values.\n\
\n\
The following example shows how to extract the first two columns of a matrix\n\
\n\
@example\n\
@group\n\
val = magic (3)\n\
    @result{} val = [ 8   1   6\n\
               3   5   7\n\
               4   9   2 ]\n\
idx.type = \"()\";\n\
idx.subs = @{\":\", 1:2@};\n\
subsref (val, idx)\n\
     @result{} [ 8   1\n\
          3   5\n\
          4   9 ]\n\
@end group\n\
@end example\n\
\n\
@noindent\n\
Note that this is the same as writing @code{val(:,1:2)}.\n\
\n\
If @var{idx} is an empty structure array with fields @samp{type} and\n\
@samp{subs}, return @var{val}.\n\
@seealso{subsasgn, substruct}\n\
@end deftypefn")
{
  octave_value_list retval;

  if (args.length () == 2)
    {
      std::string type;
      std::list<octave_value_list> idx;

      decode_subscripts ("subsref", args(1), type, idx);

      if (! error_state)
        {
          octave_value arg0 = args(0);

          if (type.empty ())
            retval = arg0;
          else
            retval = arg0.subsref (type, idx, nargout);
        }
    }
  else
    print_usage ();

  return retval;
}

DEFUN (subsasgn, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} subsasgn (@var{val}, @var{idx}, @var{rhs})\n\
Perform the subscripted assignment operation according to the subscript\n\
specified by @var{idx}.\n\
\n\
The subscript @var{idx} is expected to be a structure array with fields\n\
@samp{type} and @samp{subs}.  Valid values for @samp{type} are\n\
@samp{\"()\"}, @samp{\"@{@}\"}, and @samp{\".\"}.  The @samp{subs} field may\n\
be either @samp{\":\"} or a cell array of index values.\n\
\n\
The following example shows how to set the two first columns of a 3-by-3\n\
matrix to zero.\n\
\n\
@example\n\
@group\n\
val = magic (3);\n\
idx.type = \"()\";\n\
idx.subs = @{\":\", 1:2@};\n\
subsasgn (val, idx, 0)\n\
     @result{}  [ 0   0   6\n\
           0   0   7\n\
           0   0   2 ]\n\
@end group\n\
@end example\n\
\n\
Note that this is the same as writing @code{val(:,1:2) = 0}.\n\
\n\
If @var{idx} is an empty structure array with fields @samp{type} and\n\
@samp{subs}, return @var{rhs}.\n\
@seealso{subsref, substruct}\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 3)
    {
      std::string type;
      std::list<octave_value_list> idx;

      decode_subscripts ("subsasgn", args(1), type, idx);

      if (! error_state)
        {
          if (type.empty ())
            {
              // Regularize a null matrix if stored into a variable.

              retval = args(2).storable_value ();
            }
          else
            {
              octave_value arg0 = args(0);

              arg0.make_unique ();

              if (! error_state)
                retval= arg0.subsasgn (type, idx, args(2));
            }
        }
    }
  else
    print_usage ();

  return retval;
}

/*
%!test
%! a = reshape ([1:25], 5,5);
%! idx1 = substruct ("()", {3, 3});
%! idx2 = substruct ("()", {2:2:5, 2:2:5});
%! idx3 = substruct ("()", {":", [1,5]});
%! idx4 = struct ("type", {}, "subs", {});
%! assert (subsref (a, idx1), 13);
%! assert (subsref (a, idx2), [7 17; 9 19]);
%! assert (subsref (a, idx3), [1:5; 21:25]');
%! assert (subsref (a, idx4), a);
%! a = subsasgn (a, idx1, 0);
%! a = subsasgn (a, idx2, 0);
%! a = subsasgn (a, idx3, 0);
%!# a = subsasgn (a, idx4, 0);
%! b = [0    6   11   16    0
%!      0    0   12    0    0
%!      0    8    0   18    0
%!      0    0   14    0    0
%!      0   10   15   20    0];
%! assert (a, b);

%!test
%! c = num2cell (reshape ([1:25],5,5));
%! idx1 = substruct  ("{}", {3, 3});
%! idx2 = substruct  ("()", {2:2:5, 2:2:5});
%! idx3 = substruct  ("()", {":", [1,5]});
%! idx2p = substruct ("{}", {2:2:5, 2:2:5});
%! idx3p = substruct ("{}", {":", [1,5]});
%! idx4 = struct ("type", {}, "subs", {});
%! assert ({ subsref(c, idx1) }, {13});
%! assert ({ subsref(c, idx2p) }, {7 9 17 19});
%! assert ({ subsref(c, idx3p) }, num2cell ([1:5, 21:25]));
%! assert (subsref (c, idx4), c);
%! c = subsasgn (c, idx1, 0);
%! c = subsasgn (c, idx2, 0);
%! c = subsasgn (c, idx3, 0);
%!# c = subsasgn (c, idx4, 0);
%! d = {0    6   11   16    0
%!      0    0   12    0    0
%!      0    8    0   18    0
%!      0    0   14    0    0
%!      0   10   15   20    0};
%! assert (c, d);

%!test
%! s.a = "ohai";
%! s.b = "dere";
%! s.c = 42;
%! idx1 = substruct (".", "a");
%! idx2 = substruct (".", "b");
%! idx3 = substruct (".", "c");
%! idx4 = struct ("type", {}, "subs", {});
%! assert (subsref (s, idx1), "ohai");
%! assert (subsref (s, idx2), "dere");
%! assert (subsref (s, idx3), 42);
%! assert (subsref (s, idx4), s);
%! s = subsasgn (s, idx1, "Hello");
%! s = subsasgn (s, idx2, "There");
%! s = subsasgn (s, idx3, 163);
%!# s = subsasgn (s, idx4, 163);
%! t.a = "Hello";
%! t.b = "There";
%! t.c = 163;
%! assert (s, t);
*/

DEFUN (is_sq_string, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} is_sq_string (@var{x})\n\
Return true if @var{x} is a single-quoted character string.\n\
@seealso{is_dq_string, ischar}\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 1)
    retval = args(0).is_sq_string ();
  else
    print_usage ();

  return retval;
}

/*
%!assert (is_sq_string ('foo'), true)
%!assert (is_sq_string ("foo"), false)
%!assert (is_sq_string (1.0), false)
%!assert (is_sq_string ({2.0}), false)

%!error is_sq_string ()
%!error is_sq_string ('foo', 2)
*/

DEFUN (is_dq_string, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} is_dq_string (@var{x})\n\
Return true if @var{x} is a double-quoted character string.\n\
@seealso{is_sq_string, ischar}\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 1)
    retval = args(0).is_dq_string ();
  else
    print_usage ();

  return retval;
}

/*
%!assert (is_dq_string ("foo"), true)
%!assert (is_dq_string ('foo'), false)
%!assert (is_dq_string (1.0), false)
%!assert (is_dq_string ({2.0}), false)

%!error is_dq_string ()
%!error is_dq_string ("foo", 2)
*/

DEFUN (disable_permutation_matrix, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{val} =} disable_permutation_matrix ()\n\
@deftypefnx {Built-in Function} {@var{old_val} =} disable_permutation_matrix (@var{new_val})\n\
@deftypefnx {Built-in Function} {} disable_permutation_matrix (@var{new_val}, \"local\")\n\
Query or set the internal variable that controls whether permutation\n\
matrices are stored in a special space-efficient format.\n\
\n\
The default value is true.  If this option is disabled Octave will store\n\
permutation matrices as full matrices.\n\
\n\
When called from inside a function with the @qcode{\"local\"} option, the\n\
variable is changed locally for the function and any subroutines it calls.\n\
The original variable value is restored when exiting the function.\n\
@seealso{disable_range, disable_diagonal_matrix}\n\
@end deftypefn")
{
  return SET_INTERNAL_VARIABLE (disable_permutation_matrix);
}

/*
%!function p = __test_dpm__ (dpm)
%!  disable_permutation_matrix (dpm, "local");
%!  [~, ~, p] = lu ([1,2;3,4]);
%!endfunction

%!assert (typeinfo (__test_dpm__ (false)), "permutation matrix");
%!assert (typeinfo (__test_dpm__ (true)), "matrix");
*/

DEFUN (disable_diagonal_matrix, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{val} =} disable_diagonal_matrix ()\n\
@deftypefnx {Built-in Function} {@var{old_val} =} disable_diagonal_matrix (@var{new_val})\n\
@deftypefnx {Built-in Function} {} disable_diagonal_matrix (@var{new_val}, \"local\")\n\
Query or set the internal variable that controls whether diagonal\n\
matrices are stored in a special space-efficient format.\n\
\n\
The default value is true.  If this option is disabled Octave will store\n\
diagonal matrices as full matrices.\n\
\n\
When called from inside a function with the @qcode{\"local\"} option, the\n\
variable is changed locally for the function and any subroutines it calls.\n\
The original variable value is restored when exiting the function.\n\
@seealso{disable_range, disable_permutation_matrix}\n\
@end deftypefn")
{
  return SET_INTERNAL_VARIABLE (disable_diagonal_matrix);
}

/*
%!function [x, xi, fx, fxi] = __test_ddm__ (ddm)
%!  disable_diagonal_matrix (ddm, "local");
%!  x = eye (2);
%!  xi = x*i;
%!  fx = single (x);
%!  fxi = single (xi);
%!endfunction

%!shared x, xi, fx, fxi
%!  [x, xi, fx, fxi] = __test_ddm__ (false);
%!assert (typeinfo (x), "diagonal matrix");
%!assert (typeinfo (xi), "complex diagonal matrix");
%!assert (typeinfo (fx), "float diagonal matrix");
%!assert (typeinfo (fxi), "float complex diagonal matrix");

%!shared x, xi, fx, fxi
%!  [x, xi, fx, fxi] = __test_ddm__ (true);
%!assert (typeinfo (x), "matrix");
%!assert (typeinfo (xi), "complex matrix");
%!assert (typeinfo (fx), "float matrix");
%!assert (typeinfo (fxi), "float complex matrix");
*/

DEFUN (disable_range, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{val} =} disable_range ()\n\
@deftypefnx {Built-in Function} {@var{old_val} =} disable_range (@var{new_val})\n\
@deftypefnx {Built-in Function} {} disable_range (@var{new_val}, \"local\")\n\
Query or set the internal variable that controls whether ranges are stored\n\
in a special space-efficient format.\n\
\n\
The default value is true.  If this option is disabled Octave will store\n\
ranges as full matrices.\n\
\n\
When called from inside a function with the @qcode{\"local\"} option, the\n\
variable is changed locally for the function and any subroutines it calls.\n\
The original variable value is restored when exiting the function.\n\
@seealso{disable_diagonal_matrix, disable_permutation_matrix}\n\
@end deftypefn")
{
  return SET_INTERNAL_VARIABLE (disable_range);
}

/*
%!function r = __test_dr__ (dr)
%!  disable_range (dr, "local");
%!  ## Constant folding will produce range for 1:13.
%!  base = 1;
%!  limit = 13;
%!  r = base:limit;
%!endfunction

%!assert (typeinfo (__test_dr__ (false)), "range");
%!assert (typeinfo (__test_dr__ (true)), "matrix");
*/

