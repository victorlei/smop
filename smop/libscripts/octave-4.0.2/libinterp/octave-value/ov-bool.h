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

#if !defined (octave_ov_bool_h)
#define octave_ov_bool_h 1

#include <cstdlib>

#include <iosfwd>
#include <string>

#include "lo-utils.h"
#include "mx-base.h"
#include "str-vec.h"

#include "oct-stream.h"
#include "ov-base.h"
#include "ov-base-scalar.h"
#include "ov-bool-mat.h"
#include "ov-scalar.h"
#include "ov-typeinfo.h"

class octave_value_list;

class tree_walker;

// Real scalar values.

class
OCTINTERP_API
octave_bool : public octave_base_scalar<bool>
{
public:

  octave_bool (void)
    : octave_base_scalar<bool> (false) { }

  octave_bool (bool b)
    : octave_base_scalar<bool> (b) { }

  octave_bool (const octave_bool& s)
    : octave_base_scalar<bool> (s) { }

  ~octave_bool (void) { }

  octave_base_value *clone (void) const { return new octave_bool (*this); }
  octave_base_value *empty_clone (void) const
  { return new octave_bool_matrix (); }

  type_conv_info numeric_conversion_function (void) const;

  octave_value do_index_op (const octave_value_list& idx,
                            bool resize_ok = false);

  idx_vector index_vector (bool /* require_integers */ = false) const { return idx_vector (scalar); }

  builtin_type_t builtin_type (void) const { return btyp_bool; }

  bool is_real_scalar (void) const { return true; }

  bool is_bool_scalar (void) const { return true; }

  bool is_bool_type (void) const { return true; }

  bool is_real_type (void) const { return true; }

  bool is_numeric_type (void) const { return false; }

  bool is_true (void) const { return scalar; }

  int8NDArray
  int8_array_value (void) const
  { return int8NDArray (dim_vector (1, 1), scalar); }

  int16NDArray
  int16_array_value (void) const
  { return int16NDArray (dim_vector (1, 1), scalar); }

  int32NDArray
  int32_array_value (void) const
  { return int32NDArray (dim_vector (1, 1), scalar); }

  int64NDArray
  int64_array_value (void) const
  { return int64NDArray (dim_vector (1, 1), scalar); }

  uint8NDArray
  uint8_array_value (void) const
  { return uint8NDArray (dim_vector (1, 1), scalar); }

  uint16NDArray
  uint16_array_value (void) const
  { return uint16NDArray (dim_vector (1, 1), scalar); }

  uint32NDArray
  uint32_array_value (void) const
  { return uint32NDArray (dim_vector (1, 1), scalar); }

  uint64NDArray
  uint64_array_value (void) const
  { return uint64NDArray (dim_vector (1, 1), scalar); }

  octave_int8
  int8_scalar_value (void) const { return octave_int8 (scalar); }

  octave_int16
  int16_scalar_value (void) const { return octave_int16 (scalar); }

  octave_int32
  int32_scalar_value (void) const { return octave_int32 (scalar); }

  octave_int64
  int64_scalar_value (void) const { return octave_int64 (scalar); }

  octave_uint8
  uint8_scalar_value (void) const { return octave_uint8 (scalar); }

  octave_uint16
  uint16_scalar_value (void) const { return octave_uint16 (scalar); }

  octave_uint32
  uint32_scalar_value (void) const { return octave_uint32 (scalar); }

  octave_uint64
  uint64_scalar_value (void) const { return octave_uint64 (scalar); }

  double double_value (bool = false) const { return scalar; }

  float float_value (bool = false) const { return scalar; }

  double scalar_value (bool = false) const { return scalar; }

  float float_scalar_value (bool = false) const { return scalar; }

  Matrix matrix_value (bool = false) const
  { return Matrix (1, 1, scalar); }

  FloatMatrix float_matrix_value (bool = false) const
  { return FloatMatrix (1, 1, scalar); }

  NDArray array_value (bool = false) const
  { return NDArray (dim_vector (1, 1), static_cast<double> (scalar)); }

  FloatNDArray float_array_value (bool = false) const
  { return FloatNDArray (dim_vector (1, 1), static_cast<double> (scalar)); }

  Complex complex_value (bool = false) const { return scalar; }

  FloatComplex float_complex_value (bool = false) const { return scalar; }

  ComplexMatrix complex_matrix_value (bool = false) const
  { return ComplexMatrix (1, 1, Complex (scalar)); }

  FloatComplexMatrix float_complex_matrix_value (bool = false) const
  { return FloatComplexMatrix (1, 1, FloatComplex (scalar)); }

  ComplexNDArray complex_array_value (bool = false) const
  { return ComplexNDArray (dim_vector (1, 1), Complex (scalar)); }

  FloatComplexNDArray float_complex_array_value (bool = false) const
  { return FloatComplexNDArray (dim_vector (1, 1), FloatComplex (scalar)); }

  SparseMatrix sparse_matrix_value (bool = false) const
  { return SparseMatrix (Matrix (1, 1, scalar)); }

  // FIXME Need SparseComplexMatrix (Matrix) constructor!!!
  SparseComplexMatrix sparse_complex_matrix_value (bool = false) const
  { return SparseComplexMatrix (sparse_matrix_value ()); }

  SparseBoolMatrix sparse_bool_matrix_value (bool = false) const
  { return SparseBoolMatrix (boolMatrix (1, 1, scalar)); }

  charNDArray
  char_array_value (bool = false) const
  {
    charNDArray retval (dim_vector (1, 1));
    retval(0) = static_cast<char> (scalar);
    return retval;
  }

  bool bool_value (bool = false) const { return scalar; }

  boolMatrix bool_matrix_value (bool = false) const
  { return boolMatrix (1, 1, scalar); }

  boolNDArray bool_array_value (bool = false) const
  { return boolNDArray (dim_vector (1, 1), scalar); }

  octave_value resize (const dim_vector& dv, bool fill = false) const;

  octave_value convert_to_str_internal (bool pad, bool force, char type) const;

  bool save_ascii (std::ostream& os);

  bool load_ascii (std::istream& is);

  bool save_binary (std::ostream& os, bool& save_as_floats);

  bool load_binary (std::istream& is, bool swap,
                    oct_mach_info::float_format fmt);

  bool save_hdf5 (octave_hdf5_id loc_id, const char *name, bool save_as_floats);

  bool load_hdf5 (octave_hdf5_id loc_id, const char *name);

  int write (octave_stream& os, int block_size,
             oct_data_conv::data_type output_type, int skip,
             oct_mach_info::float_format flt_fmt) const
  {
    return os.write (bool_array_value (), block_size, output_type,
                     skip, flt_fmt);
  }

  mxArray *as_mxArray (void) const;

  // Mapper functions are converted to double for treatment
  octave_value map (unary_mapper_t umap) const
  {
    octave_scalar m (scalar_value ());
    return m.map (umap);
  }

private:


  DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA
};

#endif
