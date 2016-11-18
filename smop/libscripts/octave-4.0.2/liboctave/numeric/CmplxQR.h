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

#if !defined (octave_CmplxQR_h)
#define octave_CmplxQR_h 1

#include <iosfwd>

#include "CMatrix.h"
#include "CColVector.h"
#include "CRowVector.h"
#include "base-qr.h"


class
OCTAVE_API
ComplexQR : public base_qr<ComplexMatrix>
{
public:

  ComplexQR (void) : base_qr<ComplexMatrix> () { }

  ComplexQR (const ComplexMatrix&, qr_type_t = qr_type_std);

  ComplexQR (const ComplexMatrix& qx, const ComplexMatrix& rx)
    : base_qr<ComplexMatrix> (qx, rx) { }

  ComplexQR (const ComplexQR& a) : base_qr<ComplexMatrix> (a) { }

  void init (const ComplexMatrix&, qr_type_t = qr_type_std);

  void update (const ComplexColumnVector& u, const ComplexColumnVector& v);

  void update (const ComplexMatrix& u, const ComplexMatrix& v);

  void insert_col (const ComplexColumnVector& u, octave_idx_type j);

  void insert_col (const ComplexMatrix& u, const Array<octave_idx_type>& j);

  void delete_col (octave_idx_type j);

  void delete_col (const Array<octave_idx_type>& j);

  void insert_row (const ComplexRowVector& u, octave_idx_type j);

  void delete_row (octave_idx_type j);

  void shift_cols (octave_idx_type i, octave_idx_type j);

protected:

  void form (octave_idx_type n, ComplexMatrix& afact,
             Complex *tau, qr_type_t qr_type);
};

#endif
