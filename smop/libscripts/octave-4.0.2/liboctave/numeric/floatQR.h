/*

Copyright (C) 1994-2015 John W. Eaton
Copyright (C) 2008-2009 Jaroslav Hajek

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

#if !defined (octave_floatQR_h)
#define octave_floatQR_h 1

#include <iosfwd>

#include "fMatrix.h"
#include "fColVector.h"
#include "fRowVector.h"
#include "base-qr.h"

class
OCTAVE_API
FloatQR : public base_qr<FloatMatrix>
{
public:

  FloatQR (void) : base_qr<FloatMatrix> () { }

  FloatQR (const FloatMatrix&, qr_type_t = qr_type_std);

  FloatQR (const FloatMatrix& qx, const FloatMatrix& rx)
    : base_qr<FloatMatrix> (qx, rx) { }

  FloatQR (const FloatQR& a) : base_qr<FloatMatrix> (a) { }

  void init (const FloatMatrix&, qr_type_t);

  void update (const FloatColumnVector& u, const FloatColumnVector& v);

  void update (const FloatMatrix& u, const FloatMatrix& v);

  void insert_col (const FloatColumnVector& u, octave_idx_type j);

  void insert_col (const FloatMatrix& u, const Array<octave_idx_type>& j);

  void delete_col (octave_idx_type j);

  void delete_col (const Array<octave_idx_type>& j);

  void insert_row (const FloatRowVector& u, octave_idx_type j);

  void delete_row (octave_idx_type j);

  void shift_cols (octave_idx_type i, octave_idx_type j);

protected:

  void form (octave_idx_type n, FloatMatrix& afact,
             float *tau, qr_type_t qr_type);
};

#endif
