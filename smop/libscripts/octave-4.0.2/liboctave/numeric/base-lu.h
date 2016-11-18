/*

Copyright (C) 1996-2015 John W. Eaton
Copyright (C) 2009 VZLU Prague

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

#if !defined (octave_base_lu_h)
#define octave_base_lu_h 1

#include "MArray.h"
#include "dColVector.h"
#include "PermMatrix.h"

template <class lu_type>
class
base_lu
{
public:

  typedef typename lu_type::element_type lu_elt_type;

  base_lu (void)
    : a_fact (), l_fact (), ipvt () { }

  base_lu (const base_lu& a)
    : a_fact (a.a_fact), l_fact (a.l_fact), ipvt (a.ipvt) { }

  base_lu (const lu_type& l, const lu_type& u,
           const PermMatrix& p);

  base_lu& operator = (const base_lu& a)
  {
    if (this != &a)
      {
        a_fact = a.a_fact;
        l_fact = a.l_fact;
        ipvt = a.ipvt;
      }
    return *this;
  }

  virtual ~base_lu (void) { }

  bool packed (void) const;

  void unpack (void);

  lu_type L (void) const;

  lu_type U (void) const;

  lu_type Y (void) const;

  PermMatrix P (void) const;

  ColumnVector P_vec (void) const;

  bool regular (void) const;

protected:

  Array<octave_idx_type> getp (void) const;

  lu_type a_fact;
  lu_type l_fact;

  Array<octave_idx_type> ipvt;
};

#endif
