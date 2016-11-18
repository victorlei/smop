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

#if !defined (octave_dbleQR_h)
#define octave_dbleQR_h 1

#include <iosfwd>

#include "dMatrix.h"
#include "dColVector.h"
#include "dRowVector.h"
#include "base-qr.h"

class
OCTAVE_API
QR : public base_qr<Matrix>
{
public:

  // Import them here to allow the QR:: prefix.
  typedef qr_type_t type;

  static const type std = qr_type_std;
  static const type raw = qr_type_raw;
  static const type economy = qr_type_economy;

  QR (void) : base_qr<Matrix> () { }

  QR (const Matrix&, qr_type_t = qr_type_std);

  QR (const Matrix& qx, const Matrix& rx)
    : base_qr<Matrix> (qx, rx) { }

  QR (const QR& a) : base_qr<Matrix> (a) { }

  void init (const Matrix&, qr_type_t);

  void update (const ColumnVector& u, const ColumnVector& v);

  void update (const Matrix& u, const Matrix& v);

  void insert_col (const ColumnVector& u, octave_idx_type j);

  void insert_col (const Matrix& u, const Array<octave_idx_type>& j);

  void delete_col (octave_idx_type j);

  void delete_col (const Array<octave_idx_type>& j);

  void insert_row (const RowVector& u, octave_idx_type j);

  void delete_row (octave_idx_type j);

  void shift_cols (octave_idx_type i, octave_idx_type j);

protected:

  void form (octave_idx_type n, Matrix& afact,
             double *tau, qr_type_t qr_type);
};

#endif
