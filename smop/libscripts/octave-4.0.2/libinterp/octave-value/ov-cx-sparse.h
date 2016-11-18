/*

Copyright (C) 2004-2015 David Bateman
Copyright (C) 1998-2004 Andy Adler

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

#if !defined (octave_ov_cx_sparse_h)
#define octave_ov_cx_sparse_h 1

#include <cstdlib>

#include <iosfwd>
#include <string>

#include "mx-base.h"
#include "str-vec.h"

#include "error.h"
#include "oct-stream.h"
#include "ov-base.h"
#include "ov-typeinfo.h"

#include "CSparse.h"
#include "ov-base-sparse.h"
#include "ov-re-sparse.h"

class octave_value_list;

class tree_walker;

class
OCTINTERP_API
octave_sparse_complex_matrix : public octave_base_sparse<SparseComplexMatrix>
{
public:

  octave_sparse_complex_matrix (void)
    : octave_base_sparse<SparseComplexMatrix> () { }

  octave_sparse_complex_matrix (const ComplexNDArray& m)
    : octave_base_sparse<SparseComplexMatrix> (SparseComplexMatrix (m)) { }

  octave_sparse_complex_matrix (const ComplexMatrix& m)
    : octave_base_sparse<SparseComplexMatrix> (SparseComplexMatrix (m)) { }

  octave_sparse_complex_matrix (const SparseComplexMatrix& m)
    : octave_base_sparse<SparseComplexMatrix> (m) { }

  octave_sparse_complex_matrix (const SparseComplexMatrix& m,
                                const MatrixType &t)
    : octave_base_sparse<SparseComplexMatrix> (m, t) { }

  octave_sparse_complex_matrix (const MSparse<Complex>& m)
    : octave_base_sparse<SparseComplexMatrix> (m) { }

  octave_sparse_complex_matrix (const MSparse<Complex>& m,
                                const MatrixType &t)
    : octave_base_sparse<SparseComplexMatrix> (m, t) { }

  octave_sparse_complex_matrix (const Sparse<Complex>& m,
                                const MatrixType &t)
    : octave_base_sparse<SparseComplexMatrix> (SparseComplexMatrix (m), t) { }

  octave_sparse_complex_matrix (const Sparse<Complex>& m)
    : octave_base_sparse<SparseComplexMatrix> (SparseComplexMatrix (m)) { }

  octave_sparse_complex_matrix (const octave_sparse_complex_matrix& cm)
    : octave_base_sparse<SparseComplexMatrix> (cm) { }

  ~octave_sparse_complex_matrix (void) { }

  octave_base_value *clone (void) const
  { return new octave_sparse_complex_matrix (*this); }
  octave_base_value *empty_clone (void) const
  { return new octave_sparse_complex_matrix (); }

  octave_base_value *try_narrowing_conversion (void);

  builtin_type_t builtin_type (void) const { return btyp_complex; }

  bool is_complex_matrix (void) const { return true; }

  bool is_complex_type (void) const { return true; }

  bool is_double_type (void) const { return true; }

  bool is_float_type (void) const { return true; }

  double double_value (bool = false) const;

  double scalar_value (bool frc_str_conv = false) const
  { return double_value (frc_str_conv); }

  Matrix matrix_value (bool = false) const;

  Complex complex_value (bool = false) const;

  ComplexMatrix complex_matrix_value (bool = false) const;

  ComplexNDArray complex_array_value (bool = false) const;

  charNDArray char_array_value (bool frc_str_conv = false) const;

  SparseMatrix sparse_matrix_value (bool = false) const;

  SparseComplexMatrix sparse_complex_matrix_value (bool = false) const
  { return matrix; }

  SparseBoolMatrix sparse_bool_matrix_value (bool warn = false) const;

#if 0
  int write (octave_stream& os, int block_size,
             oct_data_conv::data_type output_type, int skip,
             oct_mach_info::float_format flt_fmt) const
  {
    // Yes, for compatibility, we drop the imaginary part here.
    return os.write (matrix_value (true), block_size, output_type,
                     skip, flt_fmt);
  }
#endif

  bool save_binary (std::ostream& os, bool& save_as_floats);

  bool load_binary (std::istream& is, bool swap,
                    oct_mach_info::float_format fmt);

  bool save_hdf5 (octave_hdf5_id loc_id, const char *name, bool save_as_floats);

  bool load_hdf5 (octave_hdf5_id loc_id, const char *name);

  mxArray *as_mxArray (void) const;

  octave_value map (unary_mapper_t umap) const;

private:


  DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA
};

#endif
