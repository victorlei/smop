/*

Copyright (C) 2000-2015 John W. Eaton

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

#if !defined (octave_Array_util_h)
#define octave_Array_util_h 1

#include "Array.h"
#include "dim-vector.h"
#include "idx-vector.h"
#include "lo-array-gripes.h"

extern OCTAVE_API bool index_in_bounds (const Array<octave_idx_type>& ra_idx,
                                        const dim_vector& dimensions);

extern OCTAVE_API void increment_index (Array<octave_idx_type>& ra_idx,
                                        const dim_vector& dimensions,
                                        int start_dimension = 0);

extern OCTAVE_API octave_idx_type get_scalar_idx (Array<octave_idx_type>& idx,
                                                  dim_vector& dims);

extern OCTAVE_API octave_idx_type num_ones (const Array<octave_idx_type>&
                                            ra_idx);

extern OCTAVE_API bool is_scalar (const dim_vector& dim);

extern OCTAVE_API bool is_vector (const dim_vector& dim);

extern OCTAVE_API bool any_ones (const Array<octave_idx_type>& arr);

// These four compute a linear index for given dimensions, throwing
// exceptions on invalid indices.
extern OCTAVE_API octave_idx_type
compute_index (octave_idx_type n, const dim_vector& dims);

extern OCTAVE_API octave_idx_type
compute_index (octave_idx_type i, octave_idx_type j, const dim_vector& dims);

extern OCTAVE_API octave_idx_type
compute_index (octave_idx_type i, octave_idx_type j, octave_idx_type k,
               const dim_vector& dims);

extern OCTAVE_API octave_idx_type
compute_index (const Array<octave_idx_type>& ra_idx, const dim_vector& dims);

extern OCTAVE_API Array<octave_idx_type>
conv_to_int_array (const Array<idx_vector>& a);

extern OCTAVE_API Array<idx_vector> conv_to_array (const idx_vector *tmp,
                                                   const octave_idx_type len);

extern OCTAVE_API dim_vector freeze (Array<idx_vector>& ra_idx,
                                     const dim_vector& dimensions,
                                     int resize_ok);

extern OCTAVE_API bool vector_equivalent (const dim_vector& dv);

extern OCTAVE_API bool all_ok (const Array<idx_vector>& ra_idx);

extern OCTAVE_API bool any_orig_empty (const Array<idx_vector>& ra_idx);

extern OCTAVE_API bool all_colon_equiv (const Array<idx_vector>& ra_idx,
                                        const dim_vector& frozen_lengths);

extern OCTAVE_API bool all_ones (const Array<octave_idx_type>& arr);

extern OCTAVE_API Array<octave_idx_type>
get_elt_idx (const Array<idx_vector>& ra_idx,
             const Array<octave_idx_type>& result_idx);

extern OCTAVE_API Array<octave_idx_type> get_ra_idx (octave_idx_type idx,
                                                     const dim_vector& dims);

extern OCTAVE_API dim_vector zero_dims_inquire (const Array<idx_vector>& ia,
                                                const dim_vector& rhdv);

extern OCTAVE_API dim_vector zero_dims_inquire (const idx_vector& i,
                                                const idx_vector& j,
                                                const dim_vector& rhdv);

extern OCTAVE_API idx_vector sub2ind (const dim_vector& dv,
                                      const Array<idx_vector>& idxa);

extern OCTAVE_API Array<idx_vector> ind2sub (const dim_vector& dv,
                                             const idx_vector& idx);

struct
permute_vector
{
  octave_idx_type pidx;
  octave_idx_type iidx;
};

extern int OCTAVE_API permute_vector_compare (const void *a, const void *b);

#endif
