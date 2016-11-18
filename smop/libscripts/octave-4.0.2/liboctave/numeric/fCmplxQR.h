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

// updating/downdating by Jaroslav Hajek 2008

#if !defined (octave_fCmplxQR_h)
#define octave_fCmplxQR_h 1

#include <iosfwd>

#include "fCMatrix.h"
#include "fCColVector.h"
#include "fCRowVector.h"
#include "base-qr.h"

class
OCTAVE_API
FloatComplexQR : public base_qr<FloatComplexMatrix>
{
public:

  FloatComplexQR (void) : base_qr<FloatComplexMatrix> () { }

  FloatComplexQR (const FloatComplexMatrix&, qr_type_t = qr_type_std);

  FloatComplexQR (const FloatComplexMatrix& qx, const FloatComplexMatrix& rx)
    : base_qr<FloatComplexMatrix> (qx, rx) { }

  FloatComplexQR (const FloatComplexQR& a) : base_qr<FloatComplexMatrix> (a) { }

  void init (const FloatComplexMatrix&, qr_type_t = qr_type_std);

  void update (const FloatComplexColumnVector& u,
               const FloatComplexColumnVector& v);

  void update (const FloatComplexMatrix& u, const FloatComplexMatrix& v);

  void insert_col (const FloatComplexColumnVector& u, octave_idx_type j);

  void insert_col (const FloatComplexMatrix& u,
                   const Array<octave_idx_type>& j);

  void delete_col (octave_idx_type j);

  void delete_col (const Array<octave_idx_type>& j);

  void insert_row (const FloatComplexRowVector& u, octave_idx_type j);

  void delete_row (octave_idx_type j);

  void shift_cols (octave_idx_type i, octave_idx_type j);

protected:

  void form (octave_idx_type n, FloatComplexMatrix& afact,
             FloatComplex *tau, qr_type_t qr_type);
};

#endif
