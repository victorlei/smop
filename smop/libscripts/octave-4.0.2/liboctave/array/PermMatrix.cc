/*

Copyright (C) 2008-2015 Jaroslav Hajek

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

#include "PermMatrix.h"
#include "idx-vector.h"
#include "Array-util.h"
#include "oct-locbuf.h"

static void
gripe_invalid_permutation (void)
{
  (*current_liboctave_error_handler)
    ("PermMatrix: invalid permutation vector");
}

void
PermMatrix::setup (const Array<octave_idx_type>& p, bool colp, bool check)
{
  if (check)
    {
      if (! idx_vector (p).is_permutation (p.length ()))
        {
          gripe_invalid_permutation ();
          Array<octave_idx_type>::operator = (Array<octave_idx_type> ());
        }
    }

  if (!colp)
    *this = this->transpose ();
}

PermMatrix::PermMatrix (const Array<octave_idx_type>& p)
  : Array<octave_idx_type> (p)
{
  setup (p, false, true);
}

PermMatrix::PermMatrix (const Array<octave_idx_type>& p, bool colp, bool check)
  : Array<octave_idx_type> (p)
{
  setup (p, colp, check);
}

void
PermMatrix::setup (const idx_vector& idx, bool colp, octave_idx_type n)
{
  octave_idx_type len = idx.length (n);

  if (! idx.is_permutation (len))
    gripe_invalid_permutation ();
  else
    {
      Array<octave_idx_type> idxa (dim_vector (len, 1));
      for (octave_idx_type i = 0; i < len; i++) idxa(i) = idx(i);
      Array<octave_idx_type>::operator = (idxa);
    }

  if (!colp)
    *this = this->transpose ();
}

PermMatrix::PermMatrix (const idx_vector& idx)
  : Array<octave_idx_type> ()
{
  setup (idx, false, 0);
}

PermMatrix::PermMatrix (const idx_vector& idx, bool colp, octave_idx_type n)
  : Array<octave_idx_type> ()
{
  setup (idx, colp, n);
}

PermMatrix::PermMatrix (octave_idx_type n)
  : Array<octave_idx_type> (dim_vector (n, 1))
{
  for (octave_idx_type i = 0; i < n; i++)
    xelem (i) = i;
}

octave_idx_type
PermMatrix::checkelem (octave_idx_type i, octave_idx_type j) const
{
  octave_idx_type len = Array<octave_idx_type>::length ();
  if (i < 0 || j < 0 || i > len || j > len)
    {
      (*current_liboctave_error_handler) ("index out of range");
      return 0;
    }
  else
    return elem (i, j);
}


PermMatrix
PermMatrix::transpose (void) const
{
  octave_idx_type len = Array<octave_idx_type>::length ();

  PermMatrix retval (len);

  for (octave_idx_type i = 0; i < len; ++i)
    retval.xelem (xelem (i)) = i;

  return retval;
}

PermMatrix
PermMatrix::inverse (void) const
{
  return transpose ();
}

octave_idx_type
PermMatrix::determinant (void) const
{
  // Determine the sign of a permutation in linear time.
  // Is this widely known?

  octave_idx_type len = perm_length ();
  const octave_idx_type *pa = data ();

  OCTAVE_LOCAL_BUFFER (octave_idx_type, p, len);
  OCTAVE_LOCAL_BUFFER (octave_idx_type, q, len);

  for (octave_idx_type i = 0; i < len; i++)
    {
      p[i] = pa[i];
      q[p[i]] = i;
    }

  bool neg = false;

  for (octave_idx_type i = 0; i < len; i++)
    {
      octave_idx_type j = p[i];
      octave_idx_type k = q[i];
      if (j != i)
        {
          p[k] = p[i];
          q[j] = q[i];
          neg = ! neg;
        }
    }

  return neg ? -1 : 1;
}

PermMatrix
PermMatrix::power(octave_idx_type m) const
{
  if (m < 0)
    return inverse ().pos_power (-m);
  else if (m > 0)
    return pos_power (m);
  else
    return PermMatrix (rows ());
}

PermMatrix
PermMatrix::pos_power (octave_idx_type m) const
{
  octave_idx_type n = rows ();

  const octave_idx_type *p = data ();
  Array<octave_idx_type> res_pvec (dim_vector (n, 1), -1);
  octave_idx_type *q = res_pvec.fortran_vec ();

  for (octave_idx_type ics = 0; ics < n; ics++)
    {
      if (q[ics] > 0)
        continue;

      // go forward m steps or until a cycle is found.
      octave_idx_type ic, j;
      for (j = 1, ic = p[ics]; j != m && ic != ics; j++, ic = p[ic]) ;
      if (ic == ics)
        {
          // reduce power.
          octave_idx_type mm = m % j;
          // go forward mm steps.
          for (j = 0, ic = ics; j != mm; j++, ic = p[ic]) ;
        }

      // now ic = p^m[ics]. Loop through the whole cycle.
      octave_idx_type jcs = ics;
      do
        {
          q[jcs] = ic;
          jcs = p[jcs]; ic = p[ic];
        }
      while (jcs != ics);

    }

  return PermMatrix (res_pvec, true, false);
}

PermMatrix
PermMatrix::eye (octave_idx_type n)
{
  return PermMatrix (n);
}

PermMatrix
operator *(const PermMatrix& a, const PermMatrix& b)
{
  PermMatrix r;

  const Array<octave_idx_type> ia = a.col_perm_vec ();
  const Array<octave_idx_type> ib = b.col_perm_vec ();

  octave_idx_type n = a.columns ();

  if (n != b.rows ())
    gripe_nonconformant ("operator *", n, n, b.rows (), b.rows ());
  else
    r = PermMatrix (ia.index (idx_vector (ib)), true, false);

  return r;
}
