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

#if !defined (octave_ov_range_h)
#define octave_ov_range_h 1

#include <cstdlib>

#include <iosfwd>
#include <string>

#include "Range.h"

#include "lo-mappers.h"
#include "lo-utils.h"
#include "mx-base.h"
#include "str-vec.h"

#include "error.h"
#include "oct-stream.h"
#include "ov-base.h"
#include "ov-re-mat.h"
#include "ov-typeinfo.h"

class octave_value_list;

class tree_walker;

// Range values.

class
octave_range : public octave_base_value
{
public:

  octave_range (void)
    : octave_base_value (), range (), idx_cache () { }

  octave_range (double base, double limit, double inc)
    : octave_base_value (), range (base, limit, inc), idx_cache ()
  {
    if (range.nelem () < 0)
      ::error ("invalid range");
  }

  octave_range (const Range& r)
    : octave_base_value (), range (r), idx_cache ()
  {
    if (range.nelem () < 0 && range.nelem () != -2)
      ::error ("invalid range");
  }

  octave_range (const octave_range& r)
    : octave_base_value (), range (r.range),
      idx_cache (r.idx_cache ? new idx_vector (*r.idx_cache) : 0)
  { }

  octave_range (const Range& r, const idx_vector& cache)
    : octave_base_value (), range (r), idx_cache ()
  {
    set_idx_cache (cache);
  }

  ~octave_range (void) { clear_cached_info (); }

  octave_base_value *clone (void) const { return new octave_range (*this); }

  // A range is really just a special kind of real matrix object.  In
  // the places where we need to call empty_clone, it makes more sense
  // to create an empty matrix (0x0) instead of an empty range (1x0).
  octave_base_value *empty_clone (void) const { return new octave_matrix (); }

  type_conv_info numeric_conversion_function (void) const;

  octave_base_value *try_narrowing_conversion (void);

  octave_value subsref (const std::string& type,
                        const std::list<octave_value_list>& idx);

  octave_value_list subsref (const std::string& type,
                             const std::list<octave_value_list>& idx, int)
  { return subsref (type, idx); }

  octave_value do_index_op (const octave_value_list& idx,
                            bool resize_ok = false);

  idx_vector index_vector (bool require_integers = false) const;

  dim_vector dims (void) const
  {
    octave_idx_type n = range.nelem ();
    return dim_vector (n > 0, n);
  }

  octave_value resize (const dim_vector& dv, bool fill = false) const;


  size_t byte_size (void) const { return 3 * sizeof (double); }

  octave_value reshape (const dim_vector& new_dims) const
  { return NDArray (array_value ().reshape (new_dims)); }

  octave_value permute (const Array<int>& vec, bool inv = false) const
  { return NDArray (array_value ().permute (vec, inv)); }

  octave_value squeeze (void) const { return range; }

  octave_value full_value (void) const { return range.matrix_value (); }

  bool is_defined (void) const { return true; }

  bool is_constant (void) const { return true; }

  bool is_range (void) const { return true; }

  octave_value all (int dim = 0) const;

  octave_value any (int dim = 0) const;

  octave_value diag (octave_idx_type k = 0) const;

  octave_value diag (octave_idx_type m, octave_idx_type n) const;

  octave_value sort (octave_idx_type dim = 0, sortmode mode = ASCENDING) const
  { return range.sort (dim, mode); }

  octave_value sort (Array<octave_idx_type>& sidx, octave_idx_type dim = 0,
                     sortmode mode = ASCENDING) const
  { return range.sort (sidx, dim, mode); }

  sortmode is_sorted (sortmode mode = UNSORTED) const
  { return range.is_sorted (mode); }

  Array<octave_idx_type> sort_rows_idx (sortmode) const
  { return Array<octave_idx_type> (dim_vector (1, 0)); }

  sortmode is_sorted_rows (sortmode mode = UNSORTED) const
  { return mode ? mode : ASCENDING; }

  builtin_type_t builtin_type (void) const { return btyp_double; }

  bool is_real_type (void) const { return true; }

  bool is_double_type (void) const { return true; }

  bool is_float_type (void) const { return true; }

  bool is_numeric_type (void) const { return true; }

  bool is_true (void) const;

  double double_value (bool = false) const;

  float float_value (bool = false) const;

  double scalar_value (bool frc_str_conv = false) const
  { return double_value (frc_str_conv); }

  float float_scalar_value (bool frc_str_conv = false) const
  { return float_value (frc_str_conv); }

  Matrix matrix_value (bool = false) const
  { return range.matrix_value (); }

  FloatMatrix float_matrix_value (bool = false) const
  { return range.matrix_value (); }

  NDArray array_value (bool = false) const
  { return range.matrix_value (); }

  FloatNDArray float_array_value (bool = false) const
  { return FloatMatrix (range.matrix_value ()); }

  charNDArray char_array_value (bool = false) const;

  // FIXME: it would be better to have Range::intXNDArray_value
  // functions to avoid the intermediate conversion to a matrix
  // object.

  int8NDArray
  int8_array_value (void) const { return int8NDArray (array_value ()); }

  int16NDArray
  int16_array_value (void) const { return int16NDArray (array_value ()); }

  int32NDArray
  int32_array_value (void) const { return int32NDArray (array_value ()); }

  int64NDArray
  int64_array_value (void) const { return int64NDArray (array_value ()); }

  uint8NDArray
  uint8_array_value (void) const { return uint8NDArray (array_value ()); }

  uint16NDArray
  uint16_array_value (void) const { return uint16NDArray (array_value ()); }

  uint32NDArray
  uint32_array_value (void) const { return uint32NDArray (array_value ()); }

  uint64NDArray
  uint64_array_value (void) const { return uint64NDArray (array_value ()); }

  SparseMatrix sparse_matrix_value (bool = false) const
  { return SparseMatrix (range.matrix_value ()); }

  SparseComplexMatrix sparse_complex_matrix_value (bool = false) const
  { return SparseComplexMatrix (sparse_matrix_value ()); }

  Complex complex_value (bool = false) const;

  FloatComplex float_complex_value (bool = false) const;

  boolNDArray bool_array_value (bool warn = false) const;

  ComplexMatrix complex_matrix_value (bool = false) const
  { return ComplexMatrix (range.matrix_value ()); }

  FloatComplexMatrix float_complex_matrix_value (bool = false) const
  { return FloatComplexMatrix (range.matrix_value ()); }

  ComplexNDArray complex_array_value (bool = false) const
  { return ComplexMatrix (range.matrix_value ()); }

  FloatComplexNDArray float_complex_array_value (bool = false) const
  { return FloatComplexMatrix (range.matrix_value ()); }

  Range range_value (void) const { return range; }

  octave_value convert_to_str_internal (bool pad, bool force, char type) const;

  void print (std::ostream& os, bool pr_as_read_syntax = false);

  void print_raw (std::ostream& os, bool pr_as_read_syntax = false) const;

  bool print_name_tag (std::ostream& os, const std::string& name) const;

  void short_disp (std::ostream& os) const;

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
    // FIXME: could be more memory efficient by having a
    // special case of the octave_stream::write method for ranges.

    return os.write (matrix_value (), block_size, output_type, skip, flt_fmt);
  }

  mxArray *as_mxArray (void) const;

  octave_value map (unary_mapper_t umap) const
  {
    octave_matrix m (matrix_value ());
    return m.map (umap);
  }

  octave_value fast_elem_extract (octave_idx_type n) const;

private:

  Range range;

  idx_vector set_idx_cache (const idx_vector& idx) const
  {
    delete idx_cache;
    idx_cache = idx ? new idx_vector (idx) : 0;
    return idx;
  }

  void clear_cached_info (void) const
  {
    delete idx_cache; idx_cache = 0;
  }

  mutable idx_vector *idx_cache;

  // No assignment.

  octave_range& operator = (const octave_range&);


  DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA
};

#endif
