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

#if !defined (octave_ov_ch_mat_h)
#define octave_ov_ch_mat_h 1

#include <cstdlib>

#include <iosfwd>
#include <string>

#include "mx-base.h"
#include "str-vec.h"

#include "error.h"
#include "ov.h"
#include "ov-base.h"
#include "ov-base-mat.h"
#include "ov-int64.h"
#include "ov-re-mat.h"
#include "ov-typeinfo.h"

class octave_value_list;

class tree_walker;

// Character matrix values.

class
octave_char_matrix : public octave_base_matrix<charNDArray>
{
protected:

  octave_char_matrix (void)
    : octave_base_matrix<charNDArray> () { }

  octave_char_matrix (const charMatrix& chm)
    : octave_base_matrix<charNDArray> (chm) { }

  octave_char_matrix (const charNDArray& chm)
    : octave_base_matrix<charNDArray> (chm) { }

  octave_char_matrix (const Array<char>& chm)
    : octave_base_matrix<charNDArray> (chm) { }

  octave_char_matrix (char c)
    : octave_base_matrix<charNDArray> (c) { }

  octave_char_matrix (const char *s)
    : octave_base_matrix<charNDArray> (s) { }

  octave_char_matrix (const std::string& s)
    : octave_base_matrix<charNDArray> (s) { }

  octave_char_matrix (const string_vector& s)
    : octave_base_matrix<charNDArray> (s) { }

  octave_char_matrix (const octave_char_matrix& chm)
    : octave_base_matrix<charNDArray> (chm) { }

public:

  ~octave_char_matrix (void) { }

  octave_base_value *clone (void) const
  { return new octave_char_matrix (*this); }
  octave_base_value *empty_clone (void) const
  { return new octave_char_matrix (); }

  idx_vector index_vector (bool require_integers = false) const;

  builtin_type_t builtin_type (void) const { return btyp_char; }

  bool is_char_matrix (void) const { return true; }
  bool is_real_matrix (void) const { return true; }

  bool is_real_type (void) const { return true; }

  double double_value (bool = false) const;

  float float_value (bool = false) const;

  double scalar_value (bool frc_str_conv = false) const
  { return double_value (frc_str_conv); }

  float float_scalar_value (bool frc_str_conv = false) const
  { return float_value (frc_str_conv); }

  octave_int64 int64_scalar_value () const;
  octave_uint64 uint64_scalar_value () const;

  Matrix matrix_value (bool = false) const
  { return Matrix (charMatrix (matrix)); }

  FloatMatrix float_matrix_value (bool = false) const
  { return FloatMatrix (charMatrix (matrix)); }

  NDArray array_value (bool = false) const
  { return NDArray (matrix); }

  FloatNDArray float_array_value (bool = false) const
  { return FloatNDArray (matrix); }

  Complex complex_value (bool = false) const;

  FloatComplex float_complex_value (bool = false) const;

  ComplexMatrix complex_matrix_value (bool = false) const
  { return ComplexMatrix (charMatrix (matrix)); }

  FloatComplexMatrix float_complex_matrix_value (bool = false) const
  { return FloatComplexMatrix (charMatrix (matrix)); }

  ComplexNDArray complex_array_value (bool = false) const
  { return ComplexNDArray (matrix); }

  FloatComplexNDArray float_complex_array_value (bool = false) const
  { return FloatComplexNDArray (matrix); }

  charMatrix char_matrix_value (bool = false) const
  { return charMatrix (matrix); }

  charNDArray char_array_value (bool = false) const
  { return matrix; }

  octave_value convert_to_str_internal (bool, bool, char type) const
  { return octave_value (matrix, type); }

  void print_raw (std::ostream& os, bool pr_as_read_syntax = false) const;

  // Unsafe.  This function exists to support the MEX interface.
  // You should not use it anywhere else.
  void *mex_get_data (void) const { return matrix.mex_get_data (); }

  mxArray *as_mxArray (void) const;

  octave_value map (unary_mapper_t umap) const;
};

#endif
