/*

Copyright (C) 2009-2015 Jaroslav Hajek

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

#if !defined (octave_base_qr_h)
#define octave_base_qr_h 1

#include "MArray.h"
#include "dColVector.h"
#include "PermMatrix.h"

enum qr_type_t
{
  qr_type_std,
  qr_type_raw,
  qr_type_economy
};

template <class qr_type>
class
base_qr
{
public:

  typedef typename qr_type::element_type qr_elt_type;

  base_qr (void) : q (), r () { }

  base_qr (const qr_type& q, const qr_type& r);

  base_qr (const base_qr& a) : q (a.q), r (a.r) { }

  base_qr& operator = (const base_qr& a)
  {
    if (this != &a)
      {
        q = a.q;
        r = a.r;
      }
    return *this;
  }

  virtual ~base_qr (void) { }

  qr_type Q (void) const { return q; }

  qr_type R (void) const { return r; }

  qr_type_t get_type (void) const;

  bool regular (void) const;

protected:

  qr_type q;
  qr_type r;
};

#ifndef HAVE_QRUPDATE
void warn_qrupdate_once (void);
#endif

#endif
