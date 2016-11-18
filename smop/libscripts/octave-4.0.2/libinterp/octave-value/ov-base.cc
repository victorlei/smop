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

#include <iostream>
#include <limits>

#include "lo-ieee.h"
#include "lo-mappers.h"

#include "defun.h"
#include "gripes.h"
#include "mxarray.h"
#include "oct-map.h"
#include "oct-obj.h"
#include "oct-hdf5.h"
#include "oct-lvalue.h"
#include "oct-stream.h"
#include "ops.h"
#include "ov-base.h"
#include "ov-cell.h"
#include "ov-ch-mat.h"
#include "ov-complex.h"
#include "ov-cx-mat.h"
#include "ov-range.h"
#include "ov-re-mat.h"
#include "ov-scalar.h"
#include "ov-str-mat.h"
#include "ov-fcn-handle.h"
#include "parse.h"
#include "pr-output.h"
#include "utils.h"
#include "toplev.h"
#include "variables.h"

builtin_type_t btyp_mixed_numeric (builtin_type_t x, builtin_type_t y)
{
  builtin_type_t retval = btyp_unknown;

  if (x == btyp_bool)
    x = btyp_double;
  if (y == btyp_bool)
    y = btyp_double;

  if (x <= btyp_float_complex && y <= btyp_float_complex)
    retval = static_cast<builtin_type_t> (x | y);
  else if (x <= btyp_uint64 && y <= btyp_float)
    retval = x;
  else if (x <= btyp_float && y <= btyp_uint64)
    retval = y;
  else if ((x >= btyp_int8 && x <= btyp_int64
            && y >= btyp_int8 && y <= btyp_int64)
           || (x >= btyp_uint8 && x <= btyp_uint64
               && y >= btyp_uint8 && y <= btyp_uint64))
    retval = (x > y) ? x : y;

  return retval;
}

std::string btyp_class_name[btyp_num_types] =
{
  "double", "single", "double", "single",
  "int8", "int16", "int32", "int64",
  "uint8", "uint16", "uint32", "uint64",
  "logical", "char",
  "struct", "cell", "function_handle"
};

string_vector
get_builtin_classes (void)
{
  static string_vector retval;

  if (retval.is_empty ())
    {
      int n = btyp_num_types - 2;
      retval = string_vector (n);
      int j = 0;
      for (int i = 0; i < btyp_num_types; i++)
        {
          builtin_type_t ityp = static_cast<builtin_type_t> (i);
          if (ityp != btyp_complex && ityp != btyp_float_complex)
            retval(j++) = btyp_class_name[i];
        }
    }

  return retval;
}

DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_base_value,
                                     "<unknown type>", "unknown");

// TRUE means to perform automatic sparse to real mutation if there
// is memory to be saved
bool Vsparse_auto_mutate = false;

octave_base_value *
octave_base_value::empty_clone (void) const
{
  return resize (dim_vector ()).clone ();
}

octave_value
octave_base_value::squeeze (void) const
{
  std::string nm = type_name ();
  error ("squeeze: invalid operation for %s type", nm.c_str ());
  return octave_value ();
}

octave_value
octave_base_value::full_value (void) const
{
  gripe_wrong_type_arg ("full: invalid operation for %s type", type_name ());
  return octave_value ();
}

Matrix
octave_base_value::size (void)
{
  const dim_vector dv = dims ();
  Matrix mdv (1, dv.length ());
  for (octave_idx_type i = 0; i < dv.length (); i++)
    mdv(i) = dv(i);
  return mdv;
}

octave_idx_type
octave_base_value::numel (const octave_value_list& idx)
{
  return dims_to_numel (dims (), idx);
}

octave_value
octave_base_value::subsref (const std::string&,
                            const std::list<octave_value_list>&)
{
  std::string nm = type_name ();
  error ("can't perform indexing operations for %s type", nm.c_str ());
  return octave_value ();
}

octave_value_list
octave_base_value::subsref (const std::string&,
                            const std::list<octave_value_list>&, int)
{
  std::string nm = type_name ();
  error ("can't perform indexing operations for %s type", nm.c_str ());
  return octave_value ();
}

octave_value
octave_base_value::subsref (const std::string& type,
                            const std::list<octave_value_list>& idx,
                            bool /* auto_add */)
{
  // This way we may get a more meaningful error message.
  return subsref (type, idx);
}

octave_value_list
octave_base_value::subsref (const std::string& type,
                            const std::list<octave_value_list>& idx,
                            int nargout,
                            const std::list<octave_lvalue> *)
{
  // Fall back to call without passing lvalue list.
  return subsref (type, idx, nargout);
}

octave_value
octave_base_value::do_index_op (const octave_value_list&, bool)
{
  std::string nm = type_name ();
  error ("can't perform indexing operations for %s type", nm.c_str ());
  return octave_value ();
}

octave_value_list
octave_base_value::do_multi_index_op (int, const octave_value_list&)
{
  std::string nm = type_name ();
  error ("can't perform indexing operations for %s type", nm.c_str ());
  return octave_value ();
}

octave_value_list
octave_base_value::do_multi_index_op (int nargout, const octave_value_list& idx,
                                      const std::list<octave_lvalue> *)
{
  // Fall back.
  return do_multi_index_op (nargout, idx);
}

idx_vector
octave_base_value::index_vector (bool /* require_integers */) const
{
  std::string nm = type_name ();
  error ("%s type invalid as index value", nm.c_str ());
  return idx_vector ();
}

octave_value
octave_base_value::subsasgn (const std::string& type,
                             const std::list<octave_value_list>& idx,
                             const octave_value& rhs)
{
  octave_value retval;

  if (is_defined ())
    {
      if (is_numeric_type ())
        {
          switch (type[0])
            {
            case '(':
              {
                if (type.length () == 1)
                  retval = numeric_assign (type, idx, rhs);
                else if (is_empty ())
                  {
                    // Allow conversion of empty matrix to some other
                    // type in cases like
                    //
                    //  x = []; x(i).f = rhs

                    octave_value tmp = octave_value::empty_conv (type, rhs);

                    retval = tmp.subsasgn (type, idx, rhs);
                  }
                else
                  {
                    std::string nm = type_name ();
                    error ("in indexed assignment of %s, last rhs index must be ()",
                           nm.c_str ());
                  }
              }
              break;

            case '{':
            case '.':
              {
                std::string nm = type_name ();
                error ("%s cannot be indexed with %c", nm.c_str (), type[0]);
              }
              break;

            default:
              panic_impossible ();
            }
        }
      else
        {
          std::string nm = type_name ();
          error ("can't perform indexed assignment for %s type", nm.c_str ());
        }
    }
  else
    {
      // Create new object of appropriate type for given index and rhs
      // types and then call undef_subsasgn for that object.

      octave_value tmp = octave_value::empty_conv (type, rhs);

      retval = tmp.undef_subsasgn (type, idx, rhs);
    }

  return retval;
}

octave_value
octave_base_value::undef_subsasgn (const std::string& type,
                                   const std::list<octave_value_list>& idx,
                                   const octave_value& rhs)
{
  // In most cases, undef_subsasgn is handled the sams as subsasgn.  One
  // exception is octave_class objects.

  return subsasgn (type, idx, rhs);
}

octave_idx_type
octave_base_value::nnz (void) const
{
  gripe_wrong_type_arg ("octave_base_value::nnz ()", type_name ());
  return -1;
}

octave_idx_type
octave_base_value::nzmax (void) const
{
  return numel ();
}

octave_idx_type
octave_base_value::nfields (void) const
{
  gripe_wrong_type_arg ("octave_base_value::nfields ()", type_name ());
  return -1;
}

octave_value
octave_base_value::reshape (const dim_vector&) const
{
  gripe_wrong_type_arg ("octave_base_value::reshape ()", type_name ());
  return octave_value ();
}

octave_value
octave_base_value::permute (const Array<int>&, bool) const
{
  gripe_wrong_type_arg ("octave_base_value::permute ()", type_name ());
  return octave_value ();
}

octave_value
octave_base_value::resize (const dim_vector&, bool) const
{
  gripe_wrong_type_arg ("octave_base_value::resize ()", type_name ());
  return octave_value ();
}

MatrixType
octave_base_value::matrix_type (void) const
{
  gripe_wrong_type_arg ("octave_base_value::matrix_type ()", type_name ());
  return MatrixType ();
}

MatrixType
octave_base_value::matrix_type (const MatrixType&) const
{
  gripe_wrong_type_arg ("octave_base_value::matrix_type ()", type_name ());
  return MatrixType ();
}

octave_value
octave_base_value::all (int) const
{
  return 0.0;
}

octave_value
octave_base_value::any (int) const
{
  return 0.0;
}

octave_value
octave_base_value::convert_to_str (bool pad, bool force, char type) const
{
  octave_value retval = convert_to_str_internal (pad, force, type);

  if (! force && is_numeric_type ())
    gripe_implicit_conversion ("Octave:num-to-str",
                               type_name (), retval.type_name ());

  return retval;
}

octave_value
octave_base_value::convert_to_str_internal (bool, bool, char) const
{
  gripe_wrong_type_arg ("octave_base_value::convert_to_str_internal ()",
                        type_name ());
  return octave_value ();
}

void
octave_base_value::convert_to_row_or_column_vector (void)
{
  gripe_wrong_type_arg
    ("octave_base_value::convert_to_row_or_column_vector ()",
     type_name ());
}

void
octave_base_value::print (std::ostream&, bool)
{
  gripe_wrong_type_arg ("octave_base_value::print ()", type_name ());
}

void
octave_base_value::print_raw (std::ostream&, bool) const
{
  gripe_wrong_type_arg ("octave_base_value::print_raw ()", type_name ());
}

bool
octave_base_value::print_name_tag (std::ostream& os,
                                   const std::string& name) const
{
  bool retval = false;

  indent (os);

  if (print_as_scalar ())
    os << name << " = ";
  else
    {
      os << name << " =";
      newline (os);
      if (! Vcompact_format)
        newline (os);

      retval = true;
    }

  return retval;
}

void
octave_base_value::print_with_name (std::ostream& output_buf,
                                    const std::string& name,
                                    bool print_padding)
{
  bool pad_after = print_name_tag (output_buf, name);

  print (output_buf);

  if (print_padding  && pad_after && ! Vcompact_format)
    newline (output_buf);
}

void
octave_base_value::print_info (std::ostream& os,
                               const std::string& /* prefix */) const
{
  os << "no info for type: " << type_name () << "\n";
}

#define INT_CONV_METHOD(T, F) \
  T \
  octave_base_value::F ## _value (bool require_int, bool frc_str_conv) const \
  { \
    T retval = 0; \
 \
    double d = double_value (frc_str_conv); \
 \
    if (! error_state) \
      { \
        if (require_int && D_NINT (d) != d) \
          error_with_cfn ("conversion of %g to " #T " value failed", d); \
        else if (d < std::numeric_limits<T>::min ()) \
          retval = std::numeric_limits<T>::min (); \
        else if (d > std::numeric_limits<T>::max ()) \
          retval = std::numeric_limits<T>::max (); \
        else \
          retval = static_cast<T> (::fix (d));  \
      } \
    else \
      gripe_wrong_type_arg ("octave_base_value::" #F "_value ()", \
                            type_name ()); \
 \
    return retval; \
  }

INT_CONV_METHOD (short int, short)
INT_CONV_METHOD (unsigned short int, ushort)

INT_CONV_METHOD (int, int)
INT_CONV_METHOD (unsigned int, uint)

INT_CONV_METHOD (long int, long)
INT_CONV_METHOD (unsigned long int, ulong)

INT_CONV_METHOD (int64_t, int64)
INT_CONV_METHOD (uint64_t, uint64)

int
octave_base_value::nint_value (bool frc_str_conv) const
{
  int retval = 0;

  double d = double_value (frc_str_conv);

  if (! error_state)
    {
      if (xisnan (d))
        {
          error ("conversion of NaN to integer value failed");
          return retval;
        }

      retval = static_cast<int> (::fix (d));
    }
  else
    gripe_wrong_type_arg ("octave_base_value::nint_value ()", type_name ());

  return retval;
}

double
octave_base_value::double_value (bool) const
{
  double retval = lo_ieee_nan_value ();
  gripe_wrong_type_arg ("octave_base_value::double_value ()", type_name ());
  return retval;
}

float
octave_base_value::float_value (bool) const
{
  float retval = lo_ieee_float_nan_value ();
  gripe_wrong_type_arg ("octave_base_value::float_value ()", type_name ());
  return retval;
}

Cell
octave_base_value::cell_value () const
{
  Cell retval;
  gripe_wrong_type_arg ("octave_base_value::cell_value()", type_name ());
  return retval;
}

Matrix
octave_base_value::matrix_value (bool) const
{
  Matrix retval;
  gripe_wrong_type_arg ("octave_base_value::matrix_value()", type_name ());
  return retval;
}

FloatMatrix
octave_base_value::float_matrix_value (bool) const
{
  FloatMatrix retval;
  gripe_wrong_type_arg ("octave_base_value::float_matrix_value()",
                        type_name ());
  return retval;
}

NDArray
octave_base_value::array_value (bool) const
{
  FloatNDArray retval;
  gripe_wrong_type_arg ("octave_base_value::array_value()", type_name ());
  return retval;
}

FloatNDArray
octave_base_value::float_array_value (bool) const
{
  FloatNDArray retval;
  gripe_wrong_type_arg ("octave_base_value::float_array_value()", type_name ());
  return retval;
}

Complex
octave_base_value::complex_value (bool) const
{
  double tmp = lo_ieee_nan_value ();
  Complex retval (tmp, tmp);
  gripe_wrong_type_arg ("octave_base_value::complex_value()", type_name ());
  return retval;
}

FloatComplex
octave_base_value::float_complex_value (bool) const
{
  float tmp = lo_ieee_float_nan_value ();
  FloatComplex retval (tmp, tmp);
  gripe_wrong_type_arg ("octave_base_value::float_complex_value()",
                        type_name ());
  return retval;
}

ComplexMatrix
octave_base_value::complex_matrix_value (bool) const
{
  ComplexMatrix retval;
  gripe_wrong_type_arg ("octave_base_value::complex_matrix_value()",
                        type_name ());
  return retval;
}

FloatComplexMatrix
octave_base_value::float_complex_matrix_value (bool) const
{
  FloatComplexMatrix retval;
  gripe_wrong_type_arg ("octave_base_value::float_complex_matrix_value()",
                        type_name ());
  return retval;
}

ComplexNDArray
octave_base_value::complex_array_value (bool) const
{
  ComplexNDArray retval;
  gripe_wrong_type_arg ("octave_base_value::complex_array_value()",
                        type_name ());
  return retval;
}

FloatComplexNDArray
octave_base_value::float_complex_array_value (bool) const
{
  FloatComplexNDArray retval;
  gripe_wrong_type_arg ("octave_base_value::float_complex_array_value()",
                        type_name ());
  return retval;
}

bool
octave_base_value::bool_value (bool) const
{
  bool retval = false;
  gripe_wrong_type_arg ("octave_base_value::bool_value()", type_name ());
  return retval;
}

boolMatrix
octave_base_value::bool_matrix_value (bool) const
{
  boolMatrix retval;
  gripe_wrong_type_arg ("octave_base_value::bool_matrix_value()",
                        type_name ());
  return retval;
}

boolNDArray
octave_base_value::bool_array_value (bool) const
{
  boolNDArray retval;
  gripe_wrong_type_arg ("octave_base_value::bool_array_value()",
                        type_name ());
  return retval;
}

charMatrix
octave_base_value::char_matrix_value (bool force) const
{
  charMatrix retval;

  octave_value tmp = convert_to_str (false, force);

  if (! error_state)
    retval = tmp.char_matrix_value ();

  return retval;
}

charNDArray
octave_base_value::char_array_value (bool) const
{
  charNDArray retval;
  gripe_wrong_type_arg ("octave_base_value::char_array_value()",
                        type_name ());
  return retval;
}

SparseMatrix
octave_base_value::sparse_matrix_value (bool) const
{
  SparseMatrix retval;
  gripe_wrong_type_arg ("octave_base_value::sparse_matrix_value()",
                        type_name ());
  return retval;
}

SparseComplexMatrix
octave_base_value::sparse_complex_matrix_value (bool) const
{
  SparseComplexMatrix retval;
  gripe_wrong_type_arg ("octave_base_value::sparse_complex_matrix_value()",
                        type_name ());
  return retval;
}

SparseBoolMatrix
octave_base_value::sparse_bool_matrix_value (bool) const
{
  SparseBoolMatrix retval;
  gripe_wrong_type_arg ("octave_base_value::sparse_bool_matrix_value()",
                        type_name ());
  return retval;
}

DiagMatrix
octave_base_value::diag_matrix_value (bool) const
{
  DiagMatrix retval;
  gripe_wrong_type_arg ("octave_base_value::diag_matrix_value()", type_name ());
  return retval;
}

FloatDiagMatrix
octave_base_value::float_diag_matrix_value (bool) const
{
  FloatDiagMatrix retval;
  gripe_wrong_type_arg ("octave_base_value::float_diag_matrix_value()",
                        type_name ());
  return retval;
}

ComplexDiagMatrix
octave_base_value::complex_diag_matrix_value (bool) const
{
  ComplexDiagMatrix retval;
  gripe_wrong_type_arg ("octave_base_value::complex_diag_matrix_value()",
                        type_name ());
  return retval;
}

FloatComplexDiagMatrix
octave_base_value::float_complex_diag_matrix_value (bool) const
{
  FloatComplexDiagMatrix retval;
  gripe_wrong_type_arg ("octave_base_value::float_complex_diag_matrix_value()",
                        type_name ());
  return retval;
}

PermMatrix
octave_base_value::perm_matrix_value (void) const
{
  PermMatrix retval;
  gripe_wrong_type_arg ("octave_base_value::perm_matrix_value()", type_name ());
  return retval;
}

octave_int8
octave_base_value::int8_scalar_value (void) const
{
  octave_int8 retval;
  gripe_wrong_type_arg ("octave_base_value::int8_scalar_value()",
                        type_name ());
  return retval;
}

octave_int16
octave_base_value::int16_scalar_value (void) const
{
  octave_int16 retval;
  gripe_wrong_type_arg ("octave_base_value::int16_scalar_value()",
                        type_name ());
  return retval;
}

octave_int32
octave_base_value::int32_scalar_value (void) const
{
  octave_int32 retval;
  gripe_wrong_type_arg ("octave_base_value::int32_scalar_value()",
                        type_name ());
  return retval;
}

octave_int64
octave_base_value::int64_scalar_value (void) const
{
  octave_int64 retval;
  gripe_wrong_type_arg ("octave_base_value::int64_scalar_value()",
                        type_name ());
  return retval;
}

octave_uint8
octave_base_value::uint8_scalar_value (void) const
{
  octave_uint8 retval;
  gripe_wrong_type_arg ("octave_base_value::uint8_scalar_value()",
                        type_name ());
  return retval;
}

octave_uint16
octave_base_value::uint16_scalar_value (void) const
{
  octave_uint16 retval;
  gripe_wrong_type_arg ("octave_base_value::uint16_scalar_value()",
                        type_name ());
  return retval;
}

octave_uint32
octave_base_value::uint32_scalar_value (void) const
{
  octave_uint32 retval;
  gripe_wrong_type_arg ("octave_base_value::uint32_scalar_value()",
                        type_name ());
  return retval;
}

octave_uint64
octave_base_value::uint64_scalar_value (void) const
{
  octave_uint64 retval;
  gripe_wrong_type_arg ("octave_base_value::uint64_scalar_value()",
                        type_name ());
  return retval;
}

int8NDArray
octave_base_value::int8_array_value (void) const
{
  int8NDArray retval;
  gripe_wrong_type_arg ("octave_base_value::int8_array_value()",
                        type_name ());
  return retval;
}

int16NDArray
octave_base_value::int16_array_value (void) const
{
  int16NDArray retval;
  gripe_wrong_type_arg ("octave_base_value::int16_array_value()",
                        type_name ());
  return retval;
}

int32NDArray
octave_base_value::int32_array_value (void) const
{
  int32NDArray retval;
  gripe_wrong_type_arg ("octave_base_value::int32_array_value()",
                        type_name ());
  return retval;
}

int64NDArray
octave_base_value::int64_array_value (void) const
{
  int64NDArray retval;
  gripe_wrong_type_arg ("octave_base_value::int64_array_value()",
                        type_name ());
  return retval;
}

uint8NDArray
octave_base_value::uint8_array_value (void) const
{
  uint8NDArray retval;
  gripe_wrong_type_arg ("octave_base_value::uint8_array_value()",
                        type_name ());
  return retval;
}

uint16NDArray
octave_base_value::uint16_array_value (void) const
{
  uint16NDArray retval;
  gripe_wrong_type_arg ("octave_base_value::uint16_array_value()",
                        type_name ());
  return retval;
}

uint32NDArray
octave_base_value::uint32_array_value (void) const
{
  uint32NDArray retval;
  gripe_wrong_type_arg ("octave_base_value::uint32_array_value()",
                        type_name ());
  return retval;
}

uint64NDArray
octave_base_value::uint64_array_value (void) const
{
  uint64NDArray retval;
  gripe_wrong_type_arg ("octave_base_value::uint64_array_value()",
                        type_name ());
  return retval;
}

string_vector
octave_base_value::all_strings (bool pad) const
{
  string_vector retval;

  octave_value tmp = convert_to_str (pad, true);

  if (! error_state)
    retval = tmp.all_strings ();

  return retval;
}

std::string
octave_base_value::string_value (bool force) const
{
  std::string retval;

  octave_value tmp = convert_to_str (force);

  if (! error_state)
    retval = tmp.string_value ();

  return retval;
}

Array<std::string>
octave_base_value::cellstr_value (void) const
{
  Array<std::string> retval;
  gripe_wrong_type_arg ("octave_base_value::cellstry_value()",
                        type_name ());
  return retval;
}

Range
octave_base_value::range_value (void) const
{
  Range retval;
  gripe_wrong_type_arg ("octave_base_value::range_value()", type_name ());
  return retval;
}

octave_map
octave_base_value::map_value (void) const
{
  octave_map retval;
  gripe_wrong_type_arg ("octave_base_value::map_value()", type_name ());
  return retval;
}

octave_scalar_map
octave_base_value::scalar_map_value (void) const
{
  octave_map tmp = map_value ();

  if (tmp.numel () == 1)
    return tmp.checkelem (0);
  else
    {
      if (! error_state)
        error ("invalid conversion of multi-dimensional struct to scalar struct");

      return octave_scalar_map ();
    }
}

string_vector
octave_base_value::map_keys (void) const
{
  string_vector retval;
  gripe_wrong_type_arg ("octave_base_value::map_keys()", type_name ());
  return retval;
}

size_t
octave_base_value::nparents (void) const
{
  size_t retval = 0;
  gripe_wrong_type_arg ("octave_base_value::nparents()", type_name ());
  return retval;
}

std::list<std::string>
octave_base_value::parent_class_name_list (void) const
{
  std::list<std::string> retval;
  gripe_wrong_type_arg ("octave_base_value::parent_class_name_list()",
                        type_name ());
  return retval;
}

string_vector
octave_base_value::parent_class_names (void) const
{
  string_vector retval;
  gripe_wrong_type_arg ("octave_base_value::parent_class_names()",
                        type_name ());
  return retval;
}

octave_function *
octave_base_value::function_value (bool silent)
{
  octave_function *retval = 0;

  if (! silent)
    gripe_wrong_type_arg ("octave_base_value::function_value()",
                          type_name ());
  return retval;
}

octave_user_function *
octave_base_value::user_function_value (bool silent)
{
  octave_user_function *retval = 0;

  if (! silent)
    gripe_wrong_type_arg ("octave_base_value::user_function_value()",
                          type_name ());
  return retval;
}

octave_user_script *
octave_base_value::user_script_value (bool silent)
{
  octave_user_script *retval = 0;

  if (! silent)
    gripe_wrong_type_arg ("octave_base_value::user_script_value()",
                          type_name ());
  return retval;
}

octave_user_code *
octave_base_value::user_code_value (bool silent)
{
  octave_user_code *retval = 0;

  if (! silent)
    gripe_wrong_type_arg ("octave_base_value::user_code_value()",
                          type_name ());
  return retval;
}

octave_fcn_handle *
octave_base_value::fcn_handle_value (bool silent)
{
  octave_fcn_handle *retval = 0;

  if (! silent)
    gripe_wrong_type_arg ("octave_base_value::fcn_handle_value()",
                          type_name ());
  return retval;
}

octave_fcn_inline *
octave_base_value::fcn_inline_value (bool silent)
{
  octave_fcn_inline *retval = 0;

  if (! silent)
    gripe_wrong_type_arg ("octave_base_value::fcn_inline_value()",
                          type_name ());
  return retval;
}

octave_value_list
octave_base_value::list_value (void) const
{
  octave_value_list retval;
  gripe_wrong_type_arg ("octave_base_value::list_value()", type_name ());
  return retval;
}

bool
octave_base_value::save_ascii (std::ostream&)
{
  gripe_wrong_type_arg ("octave_base_value::save_ascii()", type_name ());
  return false;
}

bool
octave_base_value::load_ascii (std::istream&)
{
  gripe_wrong_type_arg ("octave_base_value::load_ascii()", type_name ());
  return false;
}

bool
octave_base_value::save_binary (std::ostream&, bool&)
{
  gripe_wrong_type_arg ("octave_base_value::save_binary()", type_name ());
  return false;
}

bool
octave_base_value::load_binary (std::istream&, bool,
                                oct_mach_info::float_format)
{
  gripe_wrong_type_arg ("octave_base_value::load_binary()", type_name ());
  return false;
}

bool
octave_base_value::save_hdf5 (octave_hdf5_id, const char *, bool)
{
  gripe_wrong_type_arg ("octave_base_value::save_binary()", type_name ());

  return false;
}

bool
octave_base_value::load_hdf5 (octave_hdf5_id, const char *)
{
  gripe_wrong_type_arg ("octave_base_value::load_binary()", type_name ());

  return false;
}

int
octave_base_value::write (octave_stream&, int, oct_data_conv::data_type,
                          int, oct_mach_info::float_format) const
{
  gripe_wrong_type_arg ("octave_base_value::write()", type_name ());

  return false;
}

mxArray *
octave_base_value::as_mxArray (void) const
{
  return 0;
}

octave_value
octave_base_value::diag (octave_idx_type) const
{
  gripe_wrong_type_arg ("octave_base_value::diag ()", type_name ());

  return octave_value ();
}

octave_value
octave_base_value::diag (octave_idx_type, octave_idx_type) const
{
  gripe_wrong_type_arg ("octave_base_value::diag ()", type_name ());

  return octave_value ();
}

octave_value
octave_base_value::sort (octave_idx_type, sortmode) const
{
  gripe_wrong_type_arg ("octave_base_value::sort ()", type_name ());

  return octave_value ();
}

octave_value
octave_base_value::sort (Array<octave_idx_type> &,
                         octave_idx_type, sortmode) const
{
  gripe_wrong_type_arg ("octave_base_value::sort ()", type_name ());

  return octave_value ();
}

sortmode
octave_base_value::is_sorted (sortmode) const
{
  gripe_wrong_type_arg ("octave_base_value::is_sorted ()", type_name ());

  return UNSORTED;
}

Array<octave_idx_type>
octave_base_value::sort_rows_idx (sortmode) const
{
  gripe_wrong_type_arg ("octave_base_value::sort_rows_idx ()", type_name ());

  return Array<octave_idx_type> ();
}

sortmode
octave_base_value::is_sorted_rows (sortmode) const
{
  gripe_wrong_type_arg ("octave_base_value::is_sorted_rows ()", type_name ());

  return UNSORTED;
}


const char *
octave_base_value::get_umap_name (unary_mapper_t umap)
{
  static const char *names[num_unary_mappers] =
  {
    "abs",
    "acos",
    "acosh",
    "angle",
    "arg",
    "asin",
    "asinh",
    "atan",
    "atanh",
    "cbrt",
    "ceil",
    "conj",
    "cos",
    "cosh",
    "erf",
    "erfinv",
    "erfcinv",
    "erfc",
    "erfcx",
    "erfi",
    "dawson",
    "exp",
    "expm1",
    "finite",
    "fix",
    "floor",
    "gamma",
    "imag",
    "isinf",
    "isna",
    "isnan",
    "lgamma",
    "log",
    "log2",
    "log10",
    "log1p",
    "real",
    "round",
    "roundb",
    "signum",
    "sin",
    "sinh",
    "sqrt",
    "tan",
    "tanh",
    "isalnum",
    "isalpha",
    "isascii",
    "iscntrl",
    "isdigit",
    "isgraph",
    "islower",
    "isprint",
    "ispunct",
    "isspace",
    "isupper",
    "isxdigit",
    "signbit",
    "toascii",
    "tolower",
    "toupper"
  };

  if (umap < 0 || umap >= num_unary_mappers)
    return "unknown";
  else
    return names[umap];
}

void
octave_base_value::gripe_load (const char *type) const
{
  warning_with_id
    ("Octave:load-save-unavailable",
     "%s: loading %s files not available in this version of Octave",
     t_name.c_str (), type);
}

void
octave_base_value::gripe_save (const char *type) const
{
  warning_with_id
    ("Octave:load-save-unavailable",
     "%s: saving %s files not available in this version of Octave",
     t_name.c_str (), type);
}

octave_value
octave_base_value::map (unary_mapper_t umap) const
{
  error ("%s: not defined for %s", get_umap_name (umap), type_name ().c_str ());
  return octave_value ();
}

void
octave_base_value::lock (void)
{
  gripe_wrong_type_arg ("octave_base_value::lock ()", type_name ());
}

void
octave_base_value::unlock (void)
{
  gripe_wrong_type_arg ("octave_base_value::unlock ()", type_name ());
}

void
octave_base_value::dump (std::ostream& os) const
{
  dim_vector dv = this->dims ();

  os << "class: " << this->class_name ()
     << " type: " << this->type_name ()
     << " dims: " << dv.str ();
}

static void
gripe_indexed_assignment (const std::string& tn1, const std::string& tn2)
{
  error ("assignment of '%s' to indexed '%s' not implemented",
         tn2.c_str (), tn1.c_str ());
}

static void
gripe_assign_conversion_failed (const std::string& tn1,
                                const std::string& tn2)
{
  error ("type conversion for assignment of '%s' to indexed '%s' failed",
         tn2.c_str (), tn1.c_str ());
}

static void
gripe_no_conversion (const std::string& on, const std::string& tn1,
                     const std::string& tn2)
{
  error ("operator %s: no conversion for assignment of '%s' to indexed '%s'",
         on.c_str (), tn2.c_str (), tn1.c_str ());
}

octave_value
octave_base_value::numeric_assign (const std::string& type,
                                   const std::list<octave_value_list>& idx,
                                   const octave_value& rhs)
{
  octave_value retval;

  if (idx.front ().empty ())
    {
      error ("missing index in indexed assignment");
      return retval;
    }

  int t_lhs = type_id ();
  int t_rhs = rhs.type_id ();

  octave_value_typeinfo::assign_op_fcn f
    = octave_value_typeinfo::lookup_assign_op (octave_value::op_asn_eq,
                                               t_lhs, t_rhs);

  bool done = false;

  if (f)
    {
      f (*this, idx.front (), rhs.get_rep ());

      done = (! error_state);
    }

  if (done)
    {
      count++;
      retval = octave_value (this);
    }
  else
    {
      int t_result
        = octave_value_typeinfo::lookup_pref_assign_conv (t_lhs, t_rhs);

      if (t_result >= 0)
        {
          octave_base_value::type_conv_fcn cf
            = octave_value_typeinfo::lookup_widening_op (t_lhs, t_result);

          if (cf)
            {
              octave_base_value *tmp = cf (*this);

              if (tmp)
                {
                  octave_value val (tmp);

                  retval = val.subsasgn (type, idx, rhs);

                  done = (! error_state);
                }
              else
                gripe_assign_conversion_failed (type_name (),
                                                rhs.type_name ());
            }
          else
            gripe_indexed_assignment (type_name (), rhs.type_name ());
        }

      if (! (done || error_state))
        {
          octave_value tmp_rhs;

          octave_base_value::type_conv_info cf_rhs
            = rhs.numeric_conversion_function ();

          octave_base_value::type_conv_info cf_this
            = numeric_conversion_function ();

          // Try biased (one-sided) conversions first.
          if (cf_rhs.type_id () >= 0
              && (octave_value_typeinfo::lookup_assign_op (octave_value::op_asn_eq,
                                                           t_lhs,
                                                           cf_rhs.type_id ())
                  || octave_value_typeinfo::lookup_pref_assign_conv (t_lhs,
                                                                     cf_rhs.type_id ()) >= 0))
            cf_this = 0;
          else if (cf_this.type_id () >= 0
                   && (octave_value_typeinfo::lookup_assign_op (octave_value::op_asn_eq,
                                                                cf_this.type_id (), t_rhs)
                       || octave_value_typeinfo::lookup_pref_assign_conv (cf_this.type_id (),
                                                                          t_rhs) >= 0))
            cf_rhs = 0;

          if (cf_rhs)
            {
              octave_base_value *tmp = cf_rhs (rhs.get_rep ());

              if (tmp)
                tmp_rhs = octave_value (tmp);
              else
                {
                  gripe_assign_conversion_failed (type_name (),
                                                  rhs.type_name ());
                  return octave_value ();
                }
            }
          else
            tmp_rhs = rhs;

          count++;
          octave_value tmp_lhs = octave_value (this);

          if (cf_this)
            {
              octave_base_value *tmp = cf_this (*this);

              if (tmp)
                tmp_lhs = octave_value (tmp);
              else
                {
                  gripe_assign_conversion_failed (type_name (),
                                                  rhs.type_name ());
                  return octave_value ();
                }
            }

          if (cf_this || cf_rhs)
            {
              retval = tmp_lhs.subsasgn (type, idx, tmp_rhs);

              done = (! error_state);
            }
          else
            gripe_no_conversion (octave_value::assign_op_as_string
                                   (octave_value::op_asn_eq),
                                 type_name (), rhs.type_name ());
        }
    }

  // The assignment may have converted to a type that is wider than
  // necessary.

  retval.maybe_mutate ();

  return retval;
}

// Current indentation.
int octave_base_value::curr_print_indent_level = 0;

// TRUE means we are at the beginning of a line.
bool octave_base_value::beginning_of_line = true;

// Each print() function should call this before printing anything.
//
// This doesn't need to be fast, but isn't there a better way?

void
octave_base_value::indent (std::ostream& os) const
{
  assert (curr_print_indent_level >= 0);

  if (beginning_of_line)
    {
      // FIXME: do we need this?
      // os << prefix;

      for (int i = 0; i < curr_print_indent_level; i++)
        os << " ";

      beginning_of_line = false;
    }
}

// All print() functions should use this to print new lines.

void
octave_base_value::newline (std::ostream& os) const
{
  os << "\n";

  beginning_of_line = true;
}

// For ressetting print state.

void
octave_base_value::reset (void) const
{
  beginning_of_line = true;
  curr_print_indent_level = 0;
}


octave_value
octave_base_value::fast_elem_extract (octave_idx_type) const
{
  return octave_value ();
}

bool
octave_base_value::fast_elem_insert (octave_idx_type, const octave_value&)
{
  return false;
}

bool
octave_base_value::fast_elem_insert_self (void *, builtin_type_t) const
{
  return false;
}

CONVDECLX (matrix_conv)
{
  return new octave_matrix ();
}

CONVDECLX (complex_matrix_conv)
{
  return new octave_complex_matrix ();
}

CONVDECLX (string_conv)
{
  return new octave_char_matrix_str ();
}

CONVDECLX (cell_conv)
{
  return new octave_cell ();
}

static inline octave_value_list
sanitize (const octave_value_list& ovl)
{
  octave_value_list retval = ovl;

  for (octave_idx_type i = 0; i < ovl.length (); i++)
    {
      if (retval(i).is_magic_colon ())
        retval(i) = ":";
    }

  return retval;
}

octave_value
make_idx_args (const std::string& type,
               const std::list<octave_value_list>& idx,
               const std::string& who)
{
  octave_value retval;

  size_t len = type.length ();

  if (len == idx.size ())
    {
      Cell type_field (1, len);
      Cell subs_field (1, len);

      std::list<octave_value_list>::const_iterator p = idx.begin ();

      for (size_t i = 0; i < len; i++)
        {
          char t = type[i];

          switch (t)
            {
            case '(':
              type_field(i) = "()";
              subs_field(i) = Cell (sanitize (*p++));
              break;

            case '{':
              type_field(i) = "{}";
              subs_field(i) = Cell (sanitize (*p++));
              break;

            case '.':
              {
                type_field(i) = ".";

                octave_value_list vlist = *p++;

                if (vlist.length () == 1)
                  {
                    octave_value val = vlist(0);

                    if (val.is_string ())
                      subs_field(i) = val;
                    else
                      {
                        error ("string argument required for '.' index");
                        return retval;
                      }
                  }
                else
                  {
                    error ("only single argument permitted for '.' index");
                    return retval;
                  }
              }
              break;

            default:
              panic_impossible ();
              break;
            }
        }

      octave_map m;

      m.assign ("type", type_field);
      m.assign ("subs", subs_field);

      retval = m;
    }
  else
    error ("invalid index for %s", who.c_str ());

  return retval;
}

bool
called_from_builtin (void)
{
  octave_function *fcn = octave_call_stack::caller ();

  // FIXME: we probably need a better check here, or some other
  // mechanism to avoid overloaded functions when builtin is used.
  // For example, what if someone overloads the builtin function?
  // Also, are there other places where using builtin is not properly
  // avoiding dispatch?

  return (fcn && fcn->name () == "builtin");
}

void
install_base_type_conversions (void)
{
  INSTALL_ASSIGNCONV (octave_base_value, octave_scalar, octave_matrix);
  INSTALL_ASSIGNCONV (octave_base_value, octave_matrix, octave_matrix);
  INSTALL_ASSIGNCONV (octave_base_value, octave_complex, octave_complex_matrix);
  INSTALL_ASSIGNCONV (octave_base_value, octave_complex_matrix,
                      octave_complex_matrix);
  INSTALL_ASSIGNCONV (octave_base_value, octave_range, octave_matrix);
  INSTALL_ASSIGNCONV (octave_base_value, octave_char_matrix_str,
                      octave_char_matrix_str);
  INSTALL_ASSIGNCONV (octave_base_value, octave_cell, octave_cell);

  INSTALL_WIDENOP (octave_base_value, octave_matrix, matrix_conv);
  INSTALL_WIDENOP (octave_base_value, octave_complex_matrix,
                   complex_matrix_conv);
  INSTALL_WIDENOP (octave_base_value, octave_char_matrix_str, string_conv);
  INSTALL_WIDENOP (octave_base_value, octave_cell, cell_conv);
}

DEFUN (sparse_auto_mutate, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{val} =} sparse_auto_mutate ()\n\
@deftypefnx {Built-in Function} {@var{old_val} =} sparse_auto_mutate (@var{new_val})\n\
@deftypefnx {Built-in Function} {} sparse_auto_mutate (@var{new_val}, \"local\")\n\
Query or set the internal variable that controls whether Octave will\n\
automatically mutate sparse matrices to full matrices to save memory.\n\
\n\
For example:\n\
\n\
@example\n\
@group\n\
s = speye (3);\n\
sparse_auto_mutate (false);\n\
s(:, 1) = 1;\n\
typeinfo (s)\n\
@result{} sparse matrix\n\
sparse_auto_mutate (true);\n\
s(1, :) = 1;\n\
typeinfo (s)\n\
@result{} matrix\n\
@end group\n\
@end example\n\
\n\
When called from inside a function with the @qcode{\"local\"} option, the\n\
variable is changed locally for the function and any subroutines it calls.\n\
The original variable value is restored when exiting the function.\n\
@end deftypefn")
{
  return SET_INTERNAL_VARIABLE (sparse_auto_mutate);
}

/*
%!test
%! s = speye (3);
%! sparse_auto_mutate (false);
%! s(:, 1) = 1;
%! assert (typeinfo (s), "sparse matrix");
%! sparse_auto_mutate (true);
%! s(1, :) = 1;
%! assert (typeinfo (s), "matrix");
%! sparse_auto_mutate (false);
*/
