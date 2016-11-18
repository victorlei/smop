/*

Copyright (C) 2008-2015 Jaroslav Hajek

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

#if !defined (octave_ov_re_diag_h)
#define octave_ov_re_diag_h 1

#include "ov-base.h"
#include "ov-base-diag.h"
#include "ov-re-mat.h"
#include "ov-typeinfo.h"

// Real diagonal matrix values.

class
OCTINTERP_API
octave_diag_matrix
  : public octave_base_diag<DiagMatrix, Matrix>
{
public:

  octave_diag_matrix (void)
    : octave_base_diag<DiagMatrix, Matrix> () { }

  octave_diag_matrix (const DiagMatrix& m)
    : octave_base_diag<DiagMatrix, Matrix> (m) { }

  octave_diag_matrix (const octave_diag_matrix& m)
    : octave_base_diag<DiagMatrix, Matrix> (m) { }

  ~octave_diag_matrix (void) { }

  octave_base_value *clone (void) const
  { return new octave_diag_matrix (*this); }
  octave_base_value *empty_clone (void) const
  { return new octave_diag_matrix (); }

  type_conv_info numeric_conversion_function (void) const;

  type_conv_info numeric_demotion_function (void) const;

  octave_base_value *try_narrowing_conversion (void);

  octave_value do_index_op (const octave_value_list& idx,
                            bool resize_ok = false);

  builtin_type_t builtin_type (void) const { return btyp_double; }

  bool is_real_matrix (void) const { return true; }

  bool is_real_type (void) const { return true; }

  bool is_double_type (void) const { return true; }

  bool is_float_type (void) const { return true; }

  DiagMatrix diag_matrix_value (bool = false) const;

  FloatDiagMatrix float_diag_matrix_value (bool = false) const;

  ComplexDiagMatrix complex_diag_matrix_value (bool = false) const;

  FloatComplexDiagMatrix float_complex_diag_matrix_value (bool = false) const;

  bool save_binary (std::ostream& os, bool& save_as_floats);

  bool load_binary (std::istream& is, bool swap,
                    oct_mach_info::float_format fmt);

  octave_value map (unary_mapper_t umap) const;

private:

  bool chk_valid_scalar (const octave_value&,
                         double&) const;


  DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA
};

#endif
