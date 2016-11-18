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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "base-lu.h"

template <class lu_type>
base_lu<lu_type>::base_lu (const lu_type& l, const lu_type& u,
                           const PermMatrix& p)
  : a_fact (u), l_fact (l), ipvt (p.transpose ().col_perm_vec ())
{
  if (l.columns () != u.rows ())
    (*current_liboctave_error_handler) ("lu: dimension mismatch");
}

template <class lu_type>
bool
base_lu <lu_type> :: packed (void) const
{
  return l_fact.dims () == dim_vector ();
}

template <class lu_type>
void
base_lu <lu_type> :: unpack (void)
{
  if (packed ())
    {
      l_fact = L ();
      a_fact = U (); // FIXME: sub-optimal
      ipvt = getp ();
    }
}

template <class lu_type>
lu_type
base_lu <lu_type> :: L (void) const
{
  if (packed ())
    {
      octave_idx_type a_nr = a_fact.rows ();
      octave_idx_type a_nc = a_fact.cols ();
      octave_idx_type mn = (a_nr < a_nc ? a_nr : a_nc);

      lu_type l (a_nr, mn, lu_elt_type (0.0));

      for (octave_idx_type i = 0; i < a_nr; i++)
        {
          if (i < a_nc)
            l.xelem (i, i) = 1.0;

          for (octave_idx_type j = 0; j < (i < a_nc ? i : a_nc); j++)
            l.xelem (i, j) = a_fact.xelem (i, j);
        }

      return l;
    }
  else
    return l_fact;
}

template <class lu_type>
lu_type
base_lu <lu_type> :: U (void) const
{
  if (packed ())
    {
      octave_idx_type a_nr = a_fact.rows ();
      octave_idx_type a_nc = a_fact.cols ();
      octave_idx_type mn = (a_nr < a_nc ? a_nr : a_nc);

      lu_type u (mn, a_nc, lu_elt_type (0.0));

      for (octave_idx_type i = 0; i < mn; i++)
        {
          for (octave_idx_type j = i; j < a_nc; j++)
            u.xelem (i, j) = a_fact.xelem (i, j);
        }

      return u;
    }
  else
    return a_fact;
}

template <class lu_type>
lu_type
base_lu <lu_type> :: Y (void) const
{
  if (! packed ())
    (*current_liboctave_error_handler)
      ("lu: Y () not implemented for unpacked form");
  return a_fact;
}

template <class lu_type>
Array<octave_idx_type>
base_lu <lu_type> :: getp (void) const
{
  if (packed ())
    {
      octave_idx_type a_nr = a_fact.rows ();

      Array<octave_idx_type> pvt (dim_vector (a_nr, 1));

      for (octave_idx_type i = 0; i < a_nr; i++)
        pvt.xelem (i) = i;

      for (octave_idx_type i = 0; i < ipvt.length (); i++)
        {
          octave_idx_type k = ipvt.xelem (i);

          if (k != i)
            {
              octave_idx_type tmp = pvt.xelem (k);
              pvt.xelem (k) = pvt.xelem (i);
              pvt.xelem (i) = tmp;
            }
        }

      return pvt;
    }
  else
    return ipvt;
}

template <class lu_type>
PermMatrix
base_lu <lu_type> :: P (void) const
{
  return PermMatrix (getp (), false);
}

template <class lu_type>
ColumnVector
base_lu <lu_type> :: P_vec (void) const
{
  octave_idx_type a_nr = a_fact.rows ();

  ColumnVector p (a_nr);

  Array<octave_idx_type> pvt = getp ();

  for (octave_idx_type i = 0; i < a_nr; i++)
    p.xelem (i) = static_cast<double> (pvt.xelem (i) + 1);

  return p;
}

template <class lu_type>
bool
base_lu<lu_type>::regular (void) const
{
  bool retval = true;

  octave_idx_type k = std::min (a_fact.rows (), a_fact.columns ());

  for (octave_idx_type i = 0; i < k; i++)
    {
      if (a_fact(i, i) == lu_elt_type ())
        {
          retval = false;
          break;
        }
    }

  return retval;
}
