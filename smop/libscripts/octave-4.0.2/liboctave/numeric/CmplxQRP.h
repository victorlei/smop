/*

Copyright (C) 1994-2015 John W. Eaton

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

#if !defined (octave_CmplxQRP_h)
#define octave_CmplxQRP_h 1

#include <iosfwd>

#include "CmplxQR.h"
#include "PermMatrix.h"
#include "dColVector.h"

class
OCTAVE_API
ComplexQRP : public ComplexQR
{
public:

  ComplexQRP (void) : ComplexQR (), p () { }

  ComplexQRP (const ComplexMatrix&, qr_type_t = qr_type_std);

  ComplexQRP (const ComplexQRP& a) : ComplexQR (a), p (a.p) { }

  ComplexQRP& operator = (const ComplexQRP& a)
  {
    if (this != &a)
      {
        ComplexQR::operator = (a);
        p = a.p;
      }
    return *this;
  }

  ~ComplexQRP (void) { }

  void init (const ComplexMatrix&, qr_type_t = qr_type_std);

  PermMatrix P (void) const { return p; }

  RowVector Pvec (void) const;

  friend std::ostream&  operator << (std::ostream&, const ComplexQRP&);

private:

  PermMatrix p;
};

#endif
