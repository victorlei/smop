/*

Copyright (C) 2010-2015 VZLU Prague

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

#if !defined (octave_ov_lazy_idx_h)
#define octave_ov_lazy_idx_h 1

#include "ov-re-mat.h"

// Lazy indices that stay in idx_vector form until the conversion to NDArray is
// actually needed.

class
OCTINTERP_API
octave_lazy_index : public octave_base_value
{
public:

  octave_lazy_index (void)
    : octave_base_value (), index (), value () { }

  octave_lazy_index (const idx_vector& idx)
    : octave_base_value (), index (idx), value () { }

  octave_lazy_index (const octave_lazy_index& i)
    : octave_base_value (), index (i.index), value (i.value) { }

  ~octave_lazy_index (void) { }

  octave_base_value *clone (void) const
  { return new octave_lazy_index (*this); }
  octave_base_value *empty_clone (void) const { return new octave_matrix (); }

  type_conv_info numeric_conversion_function (void) const;

  octave_base_value *try_narrowing_conversion (void);

  octave_value fast_elem_extract (octave_idx_type n) const;

  size_t byte_size (void) const { return numel () * sizeof (octave_idx_type); }

  octave_value squeeze (void) const;

  octave_value full_value (void) const { return make_value (); }

  idx_vector index_vector (bool /* require_integers */ = false) const { return index; }

  builtin_type_t builtin_type (void) const { return btyp_double; }

  bool is_real_matrix (void) const { return true; }

  bool is_real_type (void) const { return true; }

  bool is_double_type (void) const { return true; }

  bool is_float_type (void) const { return true; }

  octave_value subsref (const std::string& type,
                        const std::list<octave_value_list>& idx)
  { return make_value ().subsref (type, idx); }

  octave_value_list subsref (const std::string& type,
                             const std::list<octave_value_list>& idx, int)
  { return subsref (type, idx); }

  octave_value do_index_op (const octave_value_list& idx,
                            bool resize_ok = false)
  { return make_value ().do_index_op (idx, resize_ok); }

  dim_vector dims (void) const { return index.orig_dimensions (); }

  octave_idx_type numel (void) const { return index.length (0); }

  octave_idx_type nnz (void) const { return numel (); }

  octave_value reshape (const dim_vector& new_dims) const;

  octave_value permute (const Array<int>& vec, bool inv = false) const;

  octave_value resize (const dim_vector& dv, bool fill = false) const
  { return make_value ().resize (dv, fill); }

  octave_value all (int dim = 0) const { return make_value ().all (dim); }
  octave_value any (int dim = 0) const { return make_value ().any (dim); }

  MatrixType matrix_type (void) const { return make_value ().matrix_type (); }
  MatrixType matrix_type (const MatrixType& _typ) const
  { return make_value ().matrix_type (_typ); }

  octave_value sort (octave_idx_type dim = 0, sortmode mode = ASCENDING) const;

  octave_value sort (Array<octave_idx_type> &sidx, octave_idx_type dim = 0,
                     sortmode mode = ASCENDING) const;

  sortmode is_sorted (sortmode mode = UNSORTED) const;

  Array<octave_idx_type> sort_rows_idx (sortmode mode = ASCENDING) const;

  sortmode is_sorted_rows (sortmode mode = UNSORTED) const;

  bool is_matrix_type (void) const { return true; }

  bool is_numeric_type (void) const { return true; }

  bool is_defined (void) const { return true; }

  bool is_constant (void) const { return true; }

  bool is_true (void) const
  { return make_value ().is_true (); }

  bool print_as_scalar (void) const
  { return make_value ().print_as_scalar (); }

  void print (std::ostream& os, bool pr_as_read_syntax = false)
  { make_value ().print (os, pr_as_read_syntax); }

  void print_info (std::ostream& os, const std::string& prefix) const
  { make_value ().print_info (os, prefix); }

#define FORWARD_VALUE_QUERY(TYPE,NAME) \
  TYPE \
  NAME (void) const { return make_value ().NAME (); }

  FORWARD_VALUE_QUERY (int8NDArray,  int8_array_value)
  FORWARD_VALUE_QUERY (int16NDArray, int16_array_value)
  FORWARD_VALUE_QUERY (int32NDArray, int32_array_value)
  FORWARD_VALUE_QUERY (int64NDArray, int64_array_value)
  FORWARD_VALUE_QUERY (uint8NDArray,  uint8_array_value)
  FORWARD_VALUE_QUERY (uint16NDArray, uint16_array_value)
  FORWARD_VALUE_QUERY (uint32NDArray, uint32_array_value)
  FORWARD_VALUE_QUERY (uint64NDArray, uint64_array_value)

#define FORWARD_VALUE_QUERY1(TYPE,NAME) \
  TYPE \
  NAME (bool flag = false) const { return make_value ().NAME (flag); }

  FORWARD_VALUE_QUERY1 (double, double_value)

  FORWARD_VALUE_QUERY1 (float, float_value)

  FORWARD_VALUE_QUERY1 (double, scalar_value)

  FORWARD_VALUE_QUERY1 (Matrix, matrix_value)

  FORWARD_VALUE_QUERY1 (FloatMatrix, float_matrix_value)

  FORWARD_VALUE_QUERY1 (Complex, complex_value)

  FORWARD_VALUE_QUERY1 (FloatComplex, float_complex_value)

  FORWARD_VALUE_QUERY1 (ComplexMatrix, complex_matrix_value)

  FORWARD_VALUE_QUERY1 (FloatComplexMatrix, float_complex_matrix_value)

  FORWARD_VALUE_QUERY1 (ComplexNDArray, complex_array_value)

  FORWARD_VALUE_QUERY1 (FloatComplexNDArray, float_complex_array_value)

  FORWARD_VALUE_QUERY1 (boolNDArray, bool_array_value)

  FORWARD_VALUE_QUERY1 (charNDArray, char_array_value)

  FORWARD_VALUE_QUERY1 (NDArray, array_value)

  FORWARD_VALUE_QUERY1 (FloatNDArray, float_array_value)

  FORWARD_VALUE_QUERY1 (SparseMatrix, sparse_matrix_value)

  FORWARD_VALUE_QUERY1 (SparseComplexMatrix, sparse_complex_matrix_value)

  octave_value diag (octave_idx_type k = 0) const
  { return make_value ().diag (k); }

  octave_value convert_to_str_internal (bool pad, bool force, char type) const
  { return make_value ().convert_to_str_internal (pad, force, type); }

  void print_raw (std::ostream& os, bool pr_as_read_syntax = false) const
  { return make_value ().print_raw (os, pr_as_read_syntax); }

  bool save_ascii (std::ostream& os);

  bool load_ascii (std::istream& is);

  bool save_binary (std::ostream& os, bool& save_as_floats);

  bool load_binary (std::istream& is, bool swap,
                    oct_mach_info::float_format fmt);


  int write (octave_stream& os, int block_size,
             oct_data_conv::data_type output_type, int skip,
             oct_mach_info::float_format flt_fmt) const
  { return make_value ().write (os, block_size, output_type, skip, flt_fmt); }

  // Unsafe.  This function exists to support the MEX interface.
  // You should not use it anywhere else.
  void *mex_get_data (void) const
  { return make_value ().mex_get_data (); }

  mxArray *as_mxArray (void) const
  { return make_value ().as_mxArray (); }

  octave_value map (unary_mapper_t umap) const
  { return make_value ().map (umap); }

private:
  const octave_value& make_value (void) const
  {
    if (value.is_undefined ())
      value = octave_value (index, false);

    return value;
  }

  octave_value& make_value (void)
  {
    if (value.is_undefined ())
      value = octave_value (index, false);

    return value;
  }

  idx_vector index;
  mutable octave_value value;

  static octave_base_value *
  numeric_conversion_function (const octave_base_value&);

  DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA
};

#endif
