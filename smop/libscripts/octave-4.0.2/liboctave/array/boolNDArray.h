/*

Copyright (C) 2003-2015 John W. Eaton

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

#if !defined (octave_boolNDArray_h)
#define octave_boolNDArray_h 1

#include "Array.h"

#include "mx-defs.h"
#include "mx-op-decl.h"
#include "bsxfun-decl.h"

class
OCTAVE_API
boolNDArray : public Array<bool>
{
public:

  typedef boolMatrix matrix_type;

  boolNDArray (void) : Array<bool> () { }

  boolNDArray (const dim_vector& dv) : Array<bool> (dv) { }

  boolNDArray (const dim_vector& dv, const bool& val)
    : Array<bool> (dv, val) { }

  boolNDArray (const boolNDArray& a) : Array<bool> (a) { }

  boolNDArray (const Array<bool>& a) : Array<bool> (a) { }

  boolNDArray& operator = (const boolNDArray& a)
  {
    Array<bool>::operator = (a);
    return *this;
  }

  // unary operations

  boolNDArray operator ! (void) const;

  boolNDArray& invert (void);

  bool any_element_is_nan (void) const { return false; }

  // FIXME: this is not quite the right thing.

  boolNDArray all (int dim = -1) const;
  boolNDArray any (int dim = -1) const;

  NDArray sum (int dim = -1) const;
  NDArray cumsum (int dim = -1) const;

  boolNDArray concat (const boolNDArray& rb,
                      const Array<octave_idx_type>& ra_idx);

  boolNDArray& insert (const boolNDArray& a, octave_idx_type r,
                       octave_idx_type c);
  boolNDArray& insert (const boolNDArray& a,
                       const Array<octave_idx_type>& ra_idx);

  boolNDArray squeeze (void) const { return Array<bool>::squeeze (); }

  static void increment_index (Array<octave_idx_type>& ra_idx,
                               const dim_vector& dimensions,
                               int start_dimension = 0);

  static octave_idx_type compute_index (Array<octave_idx_type>& ra_idx,
                                        const dim_vector& dimensions);

  // i/o

  // friend std::ostream& operator << (std::ostream& os, const NDArray& a);
  // friend std::istream& operator >> (std::istream& is, NDArray& a);

  //  bool all_elements_are_real (void) const;
  //  bool all_integers (double& max_val, double& min_val) const;

  boolNDArray diag (octave_idx_type k = 0) const;

  boolNDArray diag (octave_idx_type m, octave_idx_type n) const;
};

NDND_BOOL_OP_DECLS (boolNDArray, boolNDArray, OCTAVE_API)
NDND_CMP_OP_DECLS (boolNDArray, boolNDArray, OCTAVE_API)

NDS_BOOL_OP_DECLS (boolNDArray, bool, OCTAVE_API)
NDS_CMP_OP_DECLS (boolNDArray, bool, OCTAVE_API)

SND_BOOL_OP_DECLS (bool, boolNDArray, OCTAVE_API)
SND_CMP_OP_DECLS (bool, boolNDArray, OCTAVE_API)

extern OCTAVE_API boolNDArray&
mx_el_and_assign (boolNDArray& m, const boolNDArray& a);
extern OCTAVE_API boolNDArray&
mx_el_or_assign (boolNDArray& m, const boolNDArray& a);

BSXFUN_OP_DECL (and, boolNDArray, OCTAVE_API);
BSXFUN_OP_DECL (or, boolNDArray, OCTAVE_API);

#endif
