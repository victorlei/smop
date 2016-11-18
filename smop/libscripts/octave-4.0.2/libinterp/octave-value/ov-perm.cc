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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "byte-swap.h"
#include "dim-vector.h"

#include "mxarray.h"
#include "ov-perm.h"
#include "ov-re-mat.h"
#include "ov-scalar.h"
#include "error.h"
#include "gripes.h"
#include "ops.h"
#include "pr-output.h"

#include "ls-oct-ascii.h"

octave_value
octave_perm_matrix::subsref (const std::string& type,
                             const std::list<octave_value_list>& idx)
{
  octave_value retval;

  switch (type[0])
    {
    case '(':
      retval = do_index_op (idx.front ());
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

  return retval.next_subsref (type, idx);
}

octave_value
octave_perm_matrix::do_index_op (const octave_value_list& idx,
                                 bool resize_ok)
{
  octave_value retval;
  octave_idx_type nidx = idx.length ();
  idx_vector idx0, idx1;
  if (nidx == 2)
    {
      idx0 = idx(0).index_vector ();
      idx1 = idx(1).index_vector ();
    }

  // This hack is to allow constructing permutation matrices using
  // eye(n)(p,:), eye(n)(:,q) && eye(n)(p,q) where p & q are permutation
  // vectors.
  // Note that, for better consistency, eye(n)(:,:) still converts to a full
  // matrix.
  if (! error_state && nidx == 2)
    {
      bool left = idx0.is_permutation (matrix.rows ());
      bool right = idx1.is_permutation (matrix.cols ());

      if (left && right)
        {
          if (idx0.is_colon ()) left = false;
          if (idx1.is_colon ()) right = false;
          if (left || right)
            {
              PermMatrix p = matrix;
              if (left)
                p = PermMatrix (idx0, false) * p;
              if (right)
                p = p * PermMatrix (idx1, true);
              retval = p;
            }
          else
            {
              retval = this;
              this->count++;
            }
        }
    }

  // if error_state is set, we've already griped.
  if (! error_state && ! retval.is_defined ())
    {
      if (nidx == 2 && ! resize_ok && idx0.is_scalar () && idx1.is_scalar ())
        retval = matrix.checkelem (idx0(0), idx1(0));
      else
        retval = to_dense ().do_index_op (idx, resize_ok);
    }

  return retval;
}

bool
octave_perm_matrix::is_true (void) const
{
  return to_dense ().is_true ();
}

double
octave_perm_matrix::double_value (bool) const
{
  double retval = lo_ieee_nan_value ();

  if (numel () > 0)
    {
      gripe_implicit_conversion ("Octave:array-to-scalar",
                                 type_name (), "real scalar");

      retval = matrix (0, 0);
    }
  else
    gripe_invalid_conversion (type_name (), "real scalar");

  return retval;
}

float
octave_perm_matrix::float_value (bool) const
{
  float retval = lo_ieee_float_nan_value ();

  if (numel () > 0)
    {
      gripe_implicit_conversion ("Octave:array-to-scalar",
                                 type_name (), "real scalar");

      retval = matrix (0, 0);
    }
  else
    gripe_invalid_conversion (type_name (), "real scalar");

  return retval;
}

Complex
octave_perm_matrix::complex_value (bool) const
{
  double tmp = lo_ieee_nan_value ();

  Complex retval (tmp, tmp);

  if (rows () > 0 && columns () > 0)
    {
      gripe_implicit_conversion ("Octave:array-to-scalar",
                                 type_name (), "complex scalar");

      retval = matrix (0, 0);
    }
  else
    gripe_invalid_conversion (type_name (), "complex scalar");

  return retval;
}

FloatComplex
octave_perm_matrix::float_complex_value (bool) const
{
  float tmp = lo_ieee_float_nan_value ();

  FloatComplex retval (tmp, tmp);

  if (rows () > 0 && columns () > 0)
    {
      gripe_implicit_conversion ("Octave:array-to-scalar",
                                 type_name (), "complex scalar");

      retval = matrix (0, 0);
    }
  else
    gripe_invalid_conversion (type_name (), "complex scalar");

  return retval;
}

#define FORWARD_MATRIX_VALUE(TYPE, PREFIX) \
TYPE \
octave_perm_matrix::PREFIX ## _value (bool frc_str_conv) const \
{ \
  return to_dense ().PREFIX ## _value (frc_str_conv); \
}

SparseMatrix
octave_perm_matrix::sparse_matrix_value (bool) const
{
  return SparseMatrix (matrix);
}

SparseBoolMatrix
octave_perm_matrix::sparse_bool_matrix_value (bool) const
{
  return SparseBoolMatrix (matrix);
}

SparseComplexMatrix
octave_perm_matrix::sparse_complex_matrix_value (bool) const
{
  return SparseComplexMatrix (sparse_matrix_value ());
}

FORWARD_MATRIX_VALUE (Matrix, matrix)
FORWARD_MATRIX_VALUE (FloatMatrix, float_matrix)
FORWARD_MATRIX_VALUE (ComplexMatrix, complex_matrix)
FORWARD_MATRIX_VALUE (FloatComplexMatrix, float_complex_matrix)

FORWARD_MATRIX_VALUE (NDArray, array)
FORWARD_MATRIX_VALUE (FloatNDArray, float_array)
FORWARD_MATRIX_VALUE (ComplexNDArray, complex_array)
FORWARD_MATRIX_VALUE (FloatComplexNDArray, float_complex_array)

FORWARD_MATRIX_VALUE (boolNDArray, bool_array)
FORWARD_MATRIX_VALUE (charNDArray, char_array)

idx_vector
octave_perm_matrix::index_vector (bool require_integers) const
{
  return to_dense ().index_vector (require_integers);
}

octave_value
octave_perm_matrix::convert_to_str_internal (bool pad, bool force,
                                             char type) const
{
  return to_dense ().convert_to_str_internal (pad, force, type);
}

bool
octave_perm_matrix::save_ascii (std::ostream& os)
{
  os << "# size: " << matrix.rows () << "\n";
  os << "# orient: c\n";

  Array<octave_idx_type> pvec = matrix.col_perm_vec ();
  octave_idx_type n = pvec.length ();
  ColumnVector tmp (n);
  for (octave_idx_type i = 0; i < n; i++) tmp(i) = pvec(i) + 1;
  os << tmp;

  return true;
}

bool
octave_perm_matrix::load_ascii (std::istream& is)
{
  octave_idx_type n;
  bool success = true;
  char orient;

  if (extract_keyword (is, "size", n, true)
      && extract_keyword (is, "orient", orient, true))
    {
      bool colp = orient == 'c';
      ColumnVector tmp (n);
      is >> tmp;
      if (!is)
        {
          error ("load: failed to load permutation matrix constant");
          success = false;
        }
      else
        {
          Array<octave_idx_type> pvec (dim_vector (n, 1));
          for (octave_idx_type i = 0; i < n; i++) pvec(i) = tmp(i) - 1;
          matrix = PermMatrix (pvec, colp);

          // Invalidate cache. Probably not necessary, but safe.
          dense_cache = octave_value ();
        }
    }
  else
    {
      error ("load: failed to extract size & orientation");
      success = false;
    }

  return success;
}

bool
octave_perm_matrix::save_binary (std::ostream& os, bool&)
{

  int32_t sz = matrix.rows ();
  bool colp = true;
  os.write (reinterpret_cast<char *> (&sz), 4);
  os.write (reinterpret_cast<char *> (&colp), 1);
  const Array<octave_idx_type>& col_perm = matrix.col_perm_vec ();
  os.write (reinterpret_cast<const char *> (col_perm.data ()),
                                            col_perm.byte_size ());

  return true;
}

bool
octave_perm_matrix::load_binary (std::istream& is, bool swap,
                                 oct_mach_info::float_format)
{
  int32_t sz;
  bool colp;
  if (! (is.read (reinterpret_cast<char *> (&sz), 4)
         && is.read (reinterpret_cast<char *> (&colp), 1)))
    return false;

  MArray<octave_idx_type> m (dim_vector (sz, 1));

  if (! is.read (reinterpret_cast<char *> (m.fortran_vec ()), m.byte_size ()))
    return false;

  if (swap)
    {
      int nel = m.numel ();
      for (int i = 0; i < nel; i++)
        switch (sizeof (octave_idx_type))
          {
          case 8:
            swap_bytes<8> (&m(i));
            break;
          case 4:
            swap_bytes<4> (&m(i));
            break;
          case 2:
            swap_bytes<2> (&m(i));
            break;
          case 1:
          default:
            break;
          }
    }

  matrix = PermMatrix (m, colp);
  return true;
}

void
octave_perm_matrix::print_raw (std::ostream& os,
                               bool pr_as_read_syntax) const
{
  return octave_print_internal (os, matrix, pr_as_read_syntax,
                                current_print_indent_level ());
}

mxArray *
octave_perm_matrix::as_mxArray (void) const
{
  return to_dense ().as_mxArray ();
}

bool
octave_perm_matrix::print_as_scalar (void) const
{
  dim_vector dv = dims ();

  return (dv.all_ones () || dv.any_zero ());
}

void
octave_perm_matrix::print (std::ostream& os, bool pr_as_read_syntax)
{
  print_raw (os, pr_as_read_syntax);
  newline (os);
}

int
octave_perm_matrix::write (octave_stream& os, int block_size,
                           oct_data_conv::data_type output_type, int skip,
                           oct_mach_info::float_format flt_fmt) const
{
  return to_dense ().write (os, block_size, output_type, skip, flt_fmt);
}

void
octave_perm_matrix::print_info (std::ostream& os,
                                const std::string& prefix) const
{
  matrix.print_info (os, prefix);
}


octave_value
octave_perm_matrix::to_dense (void) const
{
  if (! dense_cache.is_defined ())
    dense_cache = Matrix (matrix);

  return dense_cache;
}


DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_perm_matrix,
                                     "permutation matrix", "double");

static octave_base_value *
default_numeric_conversion_function (const octave_base_value& a)
{
  CAST_CONV_ARG (const octave_perm_matrix&);

  return new octave_matrix (v.matrix_value ());
}

octave_base_value::type_conv_info
octave_perm_matrix::numeric_conversion_function (void) const
{
  return octave_base_value::type_conv_info (default_numeric_conversion_function,
                                            octave_matrix::static_type_id ());
}

octave_base_value *
octave_perm_matrix::try_narrowing_conversion (void)
{
  octave_base_value *retval = 0;

  if (matrix.nelem () == 1)
    retval = new octave_scalar (matrix (0, 0));

  return retval;
}

octave_value
octave_perm_matrix::fast_elem_extract (octave_idx_type n) const
{
  if (n < matrix.numel ())
    {
      octave_idx_type nr = matrix.rows ();

      octave_idx_type r = n % nr;
      octave_idx_type c = n / nr;

      return octave_value (matrix.elem (r, c));
    }
  else
    return octave_value ();
}
