/*

Copyright (C) 2004-2015 John W. Eaton
Copyright (C) 2010 VZLU Prague

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

#include <cstdlib>

#include <iosfwd>
#include <string>

#include "mx-base.h"
#include "str-vec.h"

#include "error.h"
#include "mxarray.h"
#include "oct-stream.h"
#include "ov-base.h"
#include "ov-base-int.h"
#include "ov-typeinfo.h"
#include "gripes.h"

#include "ov-re-mat.h"
#include "ov-scalar.h"

class
OCTINTERP_API
OCTAVE_VALUE_INT_MATRIX_T
  : public octave_base_int_matrix<intNDArray<OCTAVE_INT_T> >
{
public:

  OCTAVE_VALUE_INT_MATRIX_T (void)
    : octave_base_int_matrix<intNDArray<OCTAVE_INT_T> > () { }

  OCTAVE_VALUE_INT_MATRIX_T (const intNDArray<OCTAVE_INT_T>& nda)
    : octave_base_int_matrix<intNDArray<OCTAVE_INT_T> > (nda) { }

  OCTAVE_VALUE_INT_MATRIX_T (const Array<OCTAVE_INT_T>& nda)
    : octave_base_int_matrix<intNDArray<OCTAVE_INT_T> >
        (intNDArray<OCTAVE_INT_T> (nda)) { }

  ~OCTAVE_VALUE_INT_MATRIX_T (void) { }

  octave_base_value *clone (void) const
  { return new OCTAVE_VALUE_INT_MATRIX_T (*this); }

  octave_base_value *empty_clone (void) const
  { return new OCTAVE_VALUE_INT_MATRIX_T (); }

  bool OCTAVE_TYPE_PREDICATE_FUNCTION (void) const { return true; }

  bool is_integer_type (void) const { return true; }

  builtin_type_t builtin_type (void) const { return OCTAVE_INT_BTYP; }

public:

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

  double
  double_value (bool = false) const
  {
    double retval = lo_ieee_nan_value ();

    if (numel () > 0)
      {
        gripe_implicit_conversion ("Octave:array-to-scalar",
                                   type_name (), "real scalar");

        retval = matrix(0).double_value ();
      }
    else
      gripe_invalid_conversion (type_name (), "real scalar");

    return retval;

  }

  float
  float_value (bool = false) const
  {
    float retval = lo_ieee_float_nan_value ();

    if (numel () > 0)
      {
        gripe_implicit_conversion ("Octave:array-to-scalar",
                                   type_name (), "real scalar");

        retval = matrix(0).float_value ();
      }
    else
      gripe_invalid_conversion (type_name (), "real scalar");

    return retval;

  }

  double scalar_value (bool = false) const { return double_value (); }

  float float_scalar_value (bool = false) const { return float_value (); }

  Matrix
  matrix_value (bool = false) const
  {
    Matrix retval;
    dim_vector dv = dims ();
    if (dv.length () > 2)
      error ("invalid conversion of %s to Matrix", type_name ().c_str ());
    else
      {
        retval = Matrix (dv(0), dv(1));
        double *vec = retval.fortran_vec ();
        octave_idx_type nel = matrix.numel ();
        for (octave_idx_type i = 0; i < nel; i++)
          vec[i] = matrix(i).double_value ();
      }
    return retval;
  }

  FloatMatrix
  float_matrix_value (bool = false) const
  {
    FloatMatrix retval;
    dim_vector dv = dims ();
    if (dv.length () > 2)
      error ("invalid conversion of %s to FloatMatrix", type_name ().c_str ());
    else
      {
        retval = FloatMatrix (dv(0), dv(1));
        float *vec = retval.fortran_vec ();
        octave_idx_type nel = matrix.numel ();
        for (octave_idx_type i = 0; i < nel; i++)
          vec[i] = matrix(i).float_value ();
      }
    return retval;
  }

  ComplexMatrix
  complex_matrix_value (bool = false) const
  {
    ComplexMatrix retval;
    dim_vector dv = dims ();
    if (dv.length () > 2)
      error ("invalid conversion of %s to Matrix", type_name ().c_str ());
    else
      {
        retval = ComplexMatrix (dv(0), dv(1));
        Complex *vec = retval.fortran_vec ();
        octave_idx_type nel = matrix.numel ();
        for (octave_idx_type i = 0; i < nel; i++)
          vec[i] = Complex (matrix(i).double_value ());
      }
    return retval;
  }

  FloatComplexMatrix
  float_complex_matrix_value (bool = false) const
  {
    FloatComplexMatrix retval;
    dim_vector dv = dims ();
    if (dv.length () > 2)
      error ("invalid conversion of %s to FloatMatrix", type_name ().c_str ());
    else
      {
        retval = FloatComplexMatrix (dv(0), dv(1));
        FloatComplex *vec = retval.fortran_vec ();
        octave_idx_type nel = matrix.numel ();
        for (octave_idx_type i = 0; i < nel; i++)
          vec[i] = FloatComplex (matrix(i).float_value ());
      }
    return retval;
  }

  NDArray
  array_value (bool = false) const
  {
    NDArray retval (matrix.dims ());
    double *vec = retval.fortran_vec ();
    octave_idx_type nel = matrix.numel ();
    for (octave_idx_type i = 0; i < nel; i++)
      vec[i] = matrix(i).double_value ();
    return retval;
  }

  FloatNDArray
  float_array_value (bool = false) const
  {
    FloatNDArray retval (matrix.dims ());
    float *vec = retval.fortran_vec ();
    octave_idx_type nel = matrix.numel ();
    for (octave_idx_type i = 0; i < nel; i++)
      vec[i] = matrix(i).float_value ();
    return retval;
  }

  ComplexNDArray
  complex_array_value (bool = false) const
  {
    ComplexNDArray retval (matrix.dims ());
    Complex *vec = retval.fortran_vec ();
    octave_idx_type nel = matrix.numel ();
    for (octave_idx_type i = 0; i < nel; i++)
      vec[i] = Complex (matrix(i).double_value ());
    return retval;
  }

  FloatComplexNDArray
  float_complex_array_value (bool = false) const
  {
    FloatComplexNDArray retval (matrix.dims ());
    FloatComplex *vec = retval.fortran_vec ();
    octave_idx_type nel = matrix.numel ();
    for (octave_idx_type i = 0; i < nel; i++)
      vec[i] = FloatComplex (matrix(i).float_value ());
    return retval;
  }

  boolNDArray
  bool_array_value (bool warn = false) const
  {
    boolNDArray retval (dims ());

    octave_idx_type nel = numel ();

    if (warn && matrix.any_element_not_one_or_zero ())
      gripe_logical_conversion ();

    bool *vec = retval.fortran_vec ();
    for (octave_idx_type i = 0; i < nel; i++)
      vec[i] = matrix(i).bool_value ();

    return retval;
  }

  charNDArray
  char_array_value (bool = false) const
  {
    charNDArray retval (dims ());

    octave_idx_type nel = numel ();

    char *vec = retval.fortran_vec ();
    for (octave_idx_type i = 0; i < nel; i++)
      vec[i] = matrix(i).char_value ();

    return retval;
  }

  // Use matrix_ref here to clear index cache.
  void increment (void)
  {
    matrix_ref () += OCTAVE_INT_T (1);
  }

  void decrement (void)
  {
    matrix_ref () -= OCTAVE_INT_T (1);
  }

  void changesign (void)
  {
    matrix_ref ().changesign ();
  }

  idx_vector index_vector (bool /* require_integers */ = false) const
  {
    return idx_cache ? *idx_cache : set_idx_cache (idx_vector (matrix));
  }

  int write (octave_stream& os, int block_size,
             oct_data_conv::data_type output_type, int skip,
             oct_mach_info::float_format flt_fmt) const
  { return os.write (matrix, block_size, output_type, skip, flt_fmt); }

  // Unsafe.  This function exists to support the MEX interface.
  // You should not use it anywhere else.
  void *mex_get_data (void) const { return matrix.mex_get_data (); }

  mxArray *as_mxArray (void) const
  {
    mxArray *retval = new mxArray (OCTAVE_INT_MX_CLASS, dims (), mxREAL);

    OCTAVE_INT_T::val_type *pr = static_cast<OCTAVE_INT_T::val_type *>
                                 (retval->get_data ());

    mwSize nel = numel ();

    const OCTAVE_INT_T *p = matrix.data ();

    for (mwIndex i = 0; i < nel; i++)
      pr[i] = p[i].value ();

    return retval;
  }

  octave_value map (unary_mapper_t umap) const
  {
    switch (umap)
      {
      case umap_abs:
        return matrix.abs ();
      case umap_signum:
        return matrix.signum ();
      case umap_ceil:
      case umap_conj:
      case umap_fix:
      case umap_floor:
      case umap_real:
      case umap_round:
        return matrix;
      case umap_imag:
        return intNDArray<OCTAVE_INT_T> (matrix.dims (), OCTAVE_INT_T ());
      case umap_isnan:
      case umap_isna:
      case umap_isinf:
        return boolNDArray (matrix.dims (), false);
      case umap_finite:
        return boolNDArray (matrix.dims (), true);

      // Special cases for Matlab compatibility.
      case umap_xtolower:
      case umap_xtoupper:
        return matrix;

      default:
        {
          // FIXME: we should be able to do better than converting to
          // double here.
          octave_matrix m (array_value ());
          return m.map (umap);
        }
      }
  }

private:


  DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA
};

class
OCTINTERP_API
OCTAVE_VALUE_INT_SCALAR_T
  : public octave_base_int_scalar<OCTAVE_INT_T>
{
public:

  OCTAVE_VALUE_INT_SCALAR_T (void)
    : octave_base_int_scalar<OCTAVE_INT_T> () { }

  OCTAVE_VALUE_INT_SCALAR_T (const OCTAVE_INT_T& nda)
    : octave_base_int_scalar<OCTAVE_INT_T> (nda) { }

  ~OCTAVE_VALUE_INT_SCALAR_T (void) { }

  octave_base_value *clone (void) const
  { return new OCTAVE_VALUE_INT_SCALAR_T (*this); }

  octave_base_value *empty_clone (void) const
  { return new OCTAVE_VALUE_INT_MATRIX_T (); }

  octave_value do_index_op (const octave_value_list& idx,
                            bool resize_ok = false)
  {
    // FIXME: this doesn't solve the problem of
    //
    //   a = 1; a([1,1], [1,1], [1,1])
    //
    // and similar constructions.  Hmm...

    // FIXME: using this constructor avoids narrowing the
    // 1x1 matrix back to a scalar value.  Need a better solution
    // to this problem.

    octave_value tmp
    (new OCTAVE_VALUE_INT_MATRIX_T
     (OCTAVE_VALUE_INT_NDARRAY_EXTRACTOR_FUNCTION ()));

    return tmp.do_index_op (idx, resize_ok);
  }

  bool OCTAVE_TYPE_PREDICATE_FUNCTION (void) const { return true; }

  bool is_integer_type (void) const { return true; }

  builtin_type_t builtin_type (void) const { return OCTAVE_INT_BTYP; }

public:

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

  int8NDArray
  int8_array_value (void) const
  { return int8NDArray (dim_vector (1, 1), int8_scalar_value ()); }

  int16NDArray
  int16_array_value (void) const
  { return int16NDArray (dim_vector (1, 1), int16_scalar_value ()); }

  int32NDArray
  int32_array_value (void) const
  { return int32NDArray (dim_vector (1, 1), int32_scalar_value ()); }

  int64NDArray
  int64_array_value (void) const
  { return int64NDArray (dim_vector (1, 1), int64_scalar_value ()); }

  uint8NDArray
  uint8_array_value (void) const
  { return uint8NDArray (dim_vector (1, 1), uint8_scalar_value ()); }

  uint16NDArray
  uint16_array_value (void) const
  { return uint16NDArray (dim_vector (1, 1), uint16_scalar_value ()); }

  uint32NDArray
  uint32_array_value (void) const
  { return uint32NDArray (dim_vector (1, 1), uint32_scalar_value ()); }

  uint64NDArray
  uint64_array_value (void) const
  { return uint64NDArray (dim_vector (1, 1), uint64_scalar_value ()); }

  octave_value resize (const dim_vector& dv, bool fill = false) const
  {
    if (fill)
      {
        intNDArray<OCTAVE_INT_T> retval (dv, 0);
        if (dv.numel ())
          retval(0) = scalar;
        return retval;
      }
    else
      {
        intNDArray<OCTAVE_INT_T> retval (dv);
        if (dv.numel ())
          retval(0) = scalar;
        return retval;
      }
  }

  double double_value (bool = false) const { return scalar.double_value (); }

  float float_value (bool = false) const { return scalar.float_value (); }

  double scalar_value (bool = false) const { return scalar.double_value (); }

  float float_scalar_value (bool = false) const
  { return scalar.float_value (); }

  Matrix
  matrix_value (bool = false) const
  {
    Matrix retval (1, 1);
    retval(0,0) = scalar.double_value ();
    return retval;
  }

  FloatMatrix
  float_matrix_value (bool = false) const
  {
    FloatMatrix retval (1, 1);
    retval(0,0) = scalar.float_value ();
    return retval;
  }

  ComplexMatrix
  complex_matrix_value (bool = false) const
  {
    ComplexMatrix retval (1, 1);
    retval(0,0) = Complex (scalar.double_value ());
    return retval;
  }

  FloatComplexMatrix
  float_complex_matrix_value (bool = false) const
  {
    FloatComplexMatrix retval (1, 1);
    retval(0,0) = FloatComplex (scalar.float_value ());
    return retval;
  }

  NDArray
  array_value (bool = false) const
  {
    NDArray retval (dim_vector (1, 1));
    retval(0) = scalar.double_value ();
    return retval;
  }

  FloatNDArray
  float_array_value (bool = false) const
  {
    FloatNDArray retval (dim_vector (1, 1));
    retval(0) = scalar.float_value ();
    return retval;
  }

  ComplexNDArray
  complex_array_value (bool = false) const
  {
    ComplexNDArray retval (dim_vector (1, 1));
    retval(0) = FloatComplex (scalar.double_value ());
    return retval;
  }

  FloatComplexNDArray
  float_complex_array_value (bool = false) const
  {
    FloatComplexNDArray retval (dim_vector (1, 1));
    retval(0) = FloatComplex (scalar.float_value ());
    return retval;
  }

  bool bool_value (bool warn = false) const
  {
    if (warn && scalar != 0.0 && scalar != 1.0)
      gripe_logical_conversion ();

    return scalar.bool_value ();
  }

  boolNDArray
  bool_array_value (bool warn = false) const
  {
    boolNDArray retval (dim_vector (1, 1));

    if (warn && scalar != 0.0 && scalar != 1.0)
      gripe_logical_conversion ();

    retval(0) = scalar.bool_value ();

    return retval;
  }

  charNDArray
  char_array_value (bool = false) const
  {
    charNDArray retval (dim_vector (1, 1));
    retval(0) = scalar.char_value ();
    return retval;
  }

  void increment (void)
  {
    scalar += OCTAVE_INT_T (1);
  }

  void decrement (void)
  {
    scalar -= OCTAVE_INT_T (1);
  }

  idx_vector index_vector (bool /* require_integers */ = false) const { return idx_vector (scalar); }

  int write (octave_stream& os, int block_size,
             oct_data_conv::data_type output_type, octave_idx_type skip,
             oct_mach_info::float_format flt_fmt) const
  {
    return os.write (OCTAVE_VALUE_INT_NDARRAY_EXTRACTOR_FUNCTION (),
                     block_size, output_type, skip, flt_fmt);
  }

  // Unsafe.  This function exists to support the MEX interface.
  // You should not use it anywhere else.
  void *mex_get_data (void) const { return scalar.mex_get_data (); }

  mxArray *as_mxArray (void) const
  {
    mxArray *retval = new mxArray (OCTAVE_INT_MX_CLASS, 1, 1, mxREAL);

    OCTAVE_INT_T::val_type *pr = static_cast<OCTAVE_INT_T::val_type *>
                                 (retval->get_data ());

    pr[0] = scalar.value ();

    return retval;
  }

  octave_value map (unary_mapper_t umap) const
  {
    switch (umap)
      {
      case umap_abs:
        return scalar.abs ();
      case umap_signum:
        return scalar.signum ();
      case umap_ceil:
      case umap_conj:
      case umap_fix:
      case umap_floor:
      case umap_real:
      case umap_round:
        return scalar;
      case umap_imag:
        return OCTAVE_INT_T ();
      case umap_isnan:
      case umap_isna:
      case umap_isinf:
        return false;
      case umap_finite:
        return true;

      // Special cases for Matlab compatibility.
      case umap_xtolower:
      case umap_xtoupper:
        return scalar;

      default:
        {
          octave_scalar m (scalar_value ());
          return m.map (umap);
        }
      }
  }

private:


  DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA
};
