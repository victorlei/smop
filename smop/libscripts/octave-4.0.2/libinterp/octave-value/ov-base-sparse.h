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

#if !defined (octave_ov_base_sparse_h)
#define octave_ov_base_sparse_h 1

#include <cstdlib>

#include <iosfwd>
#include <string>

#include "str-vec.h"

#include "error.h"
#include "oct-obj.h"
#include "ov-base.h"
#include "ov-typeinfo.h"

#include "boolSparse.h"
#include "MatrixType.h"

class tree_walker;

class octave_sparse_bool_matrix;

template <class T>
class
octave_base_sparse : public octave_base_value
{
public:

  octave_base_sparse (void)
    : octave_base_value (), matrix (), typ (MatrixType ())
  { }

  octave_base_sparse (const T& a)
    : octave_base_value (), matrix (a), typ (MatrixType ())
  {
    if (matrix.ndims () == 0)
      matrix.resize (dim_vector (0, 0));
  }

  octave_base_sparse (const T& a, const MatrixType& t)
    : octave_base_value (), matrix (a), typ (t)
  {
    if (matrix.ndims () == 0)
      matrix.resize (dim_vector (0, 0));
  }

  octave_base_sparse (const octave_base_sparse& a)
    : octave_base_value (), matrix (a.matrix), typ (a.typ) { }

  ~octave_base_sparse (void) { }

  octave_idx_type numel (void) const { return dims ().safe_numel (); }

  octave_idx_type nnz (void) const { return matrix.nnz (); }

  octave_idx_type nzmax (void) const { return matrix.nzmax (); }

  size_t byte_size (void) const { return matrix.byte_size (); }

  octave_value squeeze (void) const { return matrix.squeeze (); }

  octave_value full_value (void) const { return matrix.matrix_value (); }

  octave_value subsref (const std::string& type,
                        const std::list<octave_value_list>& idx);

  octave_value_list subsref (const std::string& type,
                             const std::list<octave_value_list>& idx, int)
  { return subsref (type, idx); }

  octave_value subsasgn (const std::string& type,
                         const std::list<octave_value_list>& idx,
                         const octave_value& rhs);

  void assign (const octave_value_list& idx, const T& rhs);

  void delete_elements (const octave_value_list& idx);

  dim_vector dims (void) const { return matrix.dims (); }

  octave_value do_index_op (const octave_value_list& idx,
                            bool resize_ok = false);

  octave_value reshape (const dim_vector& new_dims) const
  { return T (matrix.reshape (new_dims)); }

  octave_value permute (const Array<int>& vec, bool inv = false) const
  { return T (matrix.permute (vec, inv)); }

  octave_value resize (const dim_vector& dv, bool = false) const;

  octave_value all (int dim = 0) const { return matrix.all (dim); }
  octave_value any (int dim = 0) const { return matrix.any (dim); }

  octave_value diag (octave_idx_type k = 0) const
  { return octave_value (matrix.diag (k)); }

  octave_value sort (octave_idx_type dim = 0, sortmode mode = ASCENDING) const
  { return octave_value (matrix.sort (dim, mode)); }
  octave_value sort (Array<octave_idx_type> &sidx, octave_idx_type dim = 0,
                     sortmode mode = ASCENDING) const
  { return octave_value (matrix.sort (sidx, dim, mode)); }

  sortmode is_sorted (sortmode mode = UNSORTED) const
  { return full_value ().is_sorted (mode); }

  MatrixType matrix_type (void) const { return typ; }
  MatrixType matrix_type (const MatrixType& _typ) const
  { MatrixType ret = typ; typ = _typ; return ret; }

  bool is_matrix_type (void) const { return true; }

  bool is_numeric_type (void) const { return true; }

  bool is_sparse_type (void) const { return true; }

  bool is_defined (void) const { return true; }

  bool is_constant (void) const { return true; }

  bool is_true (void) const;

  octave_idx_type capacity (void) const { return matrix.capacity (); }

  bool print_as_scalar (void) const;

  void print (std::ostream& os, bool pr_as_read_syntax = false);

  void print_info (std::ostream& os, const std::string& prefix) const;

  void print_raw (std::ostream& os, bool pr_as_read_syntax = false) const;

  bool save_ascii (std::ostream& os);

  bool load_ascii (std::istream& is);

  // Unsafe.  These functions exists to support the MEX interface.
  // You should not use them anywhere else.
  void *mex_get_data (void) const { return matrix.mex_get_data (); }

  octave_idx_type *mex_get_ir (void) const { return matrix.mex_get_ir (); }

  octave_idx_type *mex_get_jc (void) const { return matrix.mex_get_jc (); }

  octave_value fast_elem_extract (octave_idx_type n) const;

protected:

  octave_value map (octave_base_value::unary_mapper_t umap) const;

  T matrix;

  mutable MatrixType typ;
};

#endif
