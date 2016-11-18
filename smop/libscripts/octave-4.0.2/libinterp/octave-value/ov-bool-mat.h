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

#if !defined (octave_ov_bool_mat_h)
#define octave_ov_bool_mat_h 1

#include <cstdlib>

#include <iosfwd>
#include <string>

#include "mx-base.h"

#include "error.h"
#include "oct-stream.h"
#include "ov-base.h"
#include "ov-base-mat.h"
#include "ov-re-mat.h"
#include "ov-typeinfo.h"

#include "MatrixType.h"

class octave_value_list;

class tree_walker;

// Character matrix values.

class
octave_bool_matrix : public octave_base_matrix<boolNDArray>
{
public:

  octave_bool_matrix (void)
    : octave_base_matrix<boolNDArray> () { }

  octave_bool_matrix (const boolNDArray& bnda)
    : octave_base_matrix<boolNDArray> (bnda) { }

  octave_bool_matrix (const Array<bool>& bnda)
    : octave_base_matrix<boolNDArray> (bnda) { }

  octave_bool_matrix (const boolMatrix& bm)
    : octave_base_matrix<boolNDArray> (bm) { }

  octave_bool_matrix (const boolMatrix& bm, const MatrixType& t)
    : octave_base_matrix<boolNDArray> (bm, t) { }

  octave_bool_matrix (const boolNDArray& bm, const idx_vector& cache)
    : octave_base_matrix<boolNDArray> (bm)
  {
    set_idx_cache (cache);
  }

  octave_bool_matrix (const octave_bool_matrix& bm)
    : octave_base_matrix<boolNDArray> (bm) { }

  ~octave_bool_matrix (void) { }

  octave_base_value *clone (void) const
  { return new octave_bool_matrix (*this); }

  octave_base_value *empty_clone (void) const
  { return new octave_bool_matrix (); }

  type_conv_info numeric_conversion_function (void) const;

  octave_base_value *try_narrowing_conversion (void);

  idx_vector index_vector (bool /* require_integers */ = false) const
  {
    return idx_cache ? *idx_cache : set_idx_cache (idx_vector (matrix));
  }

  builtin_type_t builtin_type (void) const { return btyp_bool; }

  bool is_bool_matrix (void) const { return true; }

  bool is_bool_type (void) const { return true; }

  bool is_real_type (void) const { return true; }

  bool is_numeric_type (void) const { return false; }

  int8NDArray
  int8_array_value (void) const { return int8NDArray (matrix); }

  int16NDArray
  int16_array_value (void) const { return int16NDArray (matrix); }

  int32NDArray
  int32_array_value (void) const { return int32NDArray (matrix); }

  int64NDArray
  int64_array_value (void) const { return int64NDArray (matrix); }

  uint8NDArray
  uint8_array_value (void) const { return uint8NDArray (matrix); }

  uint16NDArray
  uint16_array_value (void) const { return uint16NDArray (matrix); }

  uint32NDArray
  uint32_array_value (void) const { return uint32NDArray (matrix); }

  uint64NDArray
  uint64_array_value (void) const { return uint64NDArray (matrix); }

  double double_value (bool = false) const;

  float float_value (bool = false) const;

  double scalar_value (bool frc_str_conv = false) const
  { return double_value (frc_str_conv); }

  Matrix matrix_value (bool = false) const
  { return Matrix (boolMatrix (matrix)); }

  FloatMatrix float_matrix_value (bool = false) const
  { return FloatMatrix (boolMatrix (matrix)); }

  NDArray array_value (bool = false) const
  { return NDArray (matrix); }

  FloatNDArray float_array_value (bool = false) const
  { return FloatNDArray (matrix); }

  Complex complex_value (bool = false) const;

  FloatComplex float_complex_value (bool = false) const;

  ComplexMatrix complex_matrix_value (bool = false) const
  { return ComplexMatrix (boolMatrix (matrix)); }

  FloatComplexMatrix float_complex_matrix_value (bool = false) const
  { return FloatComplexMatrix (boolMatrix (matrix)); }

  ComplexNDArray complex_array_value (bool = false) const
  { return ComplexNDArray (matrix); }

  FloatComplexNDArray float_complex_array_value (bool = false) const
  { return FloatComplexNDArray (matrix); }

  charNDArray
  char_array_value (bool = false) const
  {
    charNDArray retval (dims ());

    octave_idx_type nel = numel ();

    for (octave_idx_type i = 0; i < nel; i++)
      retval(i) = static_cast<char>(matrix(i));

    return retval;
  }

  boolMatrix bool_matrix_value (bool = false) const
  { return boolMatrix (matrix); }

  boolNDArray bool_array_value (bool = false) const
  { return matrix; }

  SparseMatrix sparse_matrix_value (bool = false) const
  { return SparseMatrix (Matrix (boolMatrix (matrix))); }

  SparseComplexMatrix sparse_complex_matrix_value (bool = false) const
  { return SparseComplexMatrix (ComplexMatrix (boolMatrix (matrix))); }

  SparseBoolMatrix sparse_bool_matrix_value (bool = false) const
  { return SparseBoolMatrix (boolMatrix (matrix)); }

  octave_value convert_to_str_internal (bool pad, bool force, char type) const;

  // Use matrix_ref here to clear index cache.
  void invert (void) { matrix_ref ().invert (); }

  void print_raw (std::ostream& os, bool pr_as_read_syntax = false) const;

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
  { return os.write (matrix, block_size, output_type, skip, flt_fmt); }

  // Unsafe.  This function exists to support the MEX interface.
  // You should not use it anywhere else.
  void *mex_get_data (void) const { return matrix.mex_get_data (); }

  mxArray *as_mxArray (void) const;

  // Mapper functions are converted to double for treatment
  octave_value map (unary_mapper_t umap) const
  {
    octave_matrix m (array_value ());
    return m.map (umap);
  }

protected:


  DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA
};

#endif
