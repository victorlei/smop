/*

Copyright (C) 2003-2015 John W. Eaton
Copyirght (C) 2009, 2010 VZLU Prague

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

#include <iostream>

#include "dim-vector.h"

octave_idx_type *
dim_vector::nil_rep (void)
{
  static dim_vector zv (0, 0);
  return zv.rep;
}

// The maximum allowed value for a dimension extent. This will normally be a
// tiny bit off the maximum value of octave_idx_type.
// Currently 1 is subtracted to allow safe conversion of any 2D Array into
// Sparse, but this offset may change in the future.
octave_idx_type
dim_vector::dim_max (void)
{
  return std::numeric_limits<octave_idx_type>::max () - 1;
}

void
dim_vector::chop_all_singletons (void)
{
  make_unique ();

  int j = 0;
  int l = ndims ();

  for (int i = 0; i < l; i++)
    {
      if (rep[i] != 1)
        rep[j++] = rep[i];
    }

  if (j == 1)
    rep[1] = 1;

  ndims () = j > 2 ? j : 2;
}

std::string
dim_vector::str (char sep) const
{
  std::ostringstream buf;

  for (int i = 0; i < length (); i++)
    {
      buf << elem (i);

      if (i < length () - 1)
        buf << sep;
    }

  std::string retval = buf.str ();

  return retval;
}

int
dim_vector::num_ones (void) const
{
  int retval = 0;

  for (int i = 0; i < length (); i++)
    if (elem (i) == 1)
      retval++;

  return retval;
}

octave_idx_type
dim_vector::safe_numel (void) const
{
  octave_idx_type idx_max = dim_max ();
  octave_idx_type n = 1;
  int n_dims = length ();

  for (int i = 0; i < n_dims; i++)
    {
      n *= rep[i];
      if (rep[i] != 0)
        idx_max /= rep[i];
      if (idx_max <= 0)
        throw std::bad_alloc ();
    }

  return n;
}

dim_vector
dim_vector::squeeze (void) const
{
  dim_vector new_dims = *this;

  bool dims_changed = 1;

  int k = 0;

  for (int i = 0; i < length (); i++)
    {
      if (elem (i) == 1)
        dims_changed = true;
      else
        new_dims(k++) = elem (i);
    }

  if (dims_changed)
    {
      if (k == 0)
        new_dims = dim_vector (1, 1);
      else if (k == 1)
        {
          // There is one non-singleton dimension, so we need
          // to decide the correct orientation.

          if (elem (0) == 1)
            {
              // The original dimension vector had a leading
              // singleton dimension.

              octave_idx_type tmp = new_dims(0);

              new_dims.resize (2);

              new_dims(0) = 1;
              new_dims(1) = tmp;
            }
          else
            {
              // The first element of the original dimension vector
              // was not a singleton dimension.

              new_dims.resize (2);

              new_dims(1) = 1;
            }
        }
      else
        new_dims.resize (k);
    }

  return new_dims;
}

// This is the rule for cat(). cat (dim, A, B) works if one
// of the following holds, in this order:
//
// 1. size (A, k) == size (B, k) for all k != dim.
// In this case, size (C, dim) = size (A, dim) + size (B, dim) and
// other sizes remain intact.
//
// 2. A is 0x0, in which case B is the result
// 3. B is 0x0, in which case A is the result

bool
dim_vector::concat (const dim_vector& dvb, int dim)
{
  int orig_nd = ndims ();
  int ndb = dvb.ndims ();
  int new_nd = dim < ndb ? ndb : dim + 1;
  if (new_nd > orig_nd)
    resize (new_nd, 1);
  else
    new_nd = orig_nd;

  make_unique ();

  bool match = true;

  for (int i = 0; i < ndb; i++)
    {
      if (i != dim && rep[i] != dvb(i))
        {
          match = false;
          break;
        }
    }

  for (int i = ndb; i < new_nd; i++)
    {
      if (i != dim && rep[i] != 1)
        {
          match = false;
          break;
        }
    }

  if (match)
    rep[dim] += (dim < ndb ? dvb(dim) : 1);
  else
    {
      // Dimensions don't match. The only allowed fix is
      // to omit 0x0.
      if (ndb == 2 && dvb(0) == 0 && dvb(1) == 0)
        match = true;
      else if (orig_nd == 2 && rep[0] == 0 && rep[1] == 0)
        {
          *this = dvb;
          match = true;
        }
    }

  chop_trailing_singletons ();

  return match;
}

// Rules for horzcat/vertcat are yet looser.
// two arrays A, B can be concatenated
// horizontally (dim = 2) or vertically (dim = 1) if one of the
// following holds, in this order:
//
// 1. cat (dim, A, B) works
//
// 2. A, B are 2D and one of them is an empty vector, in which
// case the result is the other one except if both of them
// are empty vectors, in which case the result is 0x0.

bool
dim_vector::hvcat (const dim_vector& dvb, int dim)
{
  if (concat (dvb, dim))
    return true;
  else if (length () == 2 && dvb.length () == 2)
    {
      bool e2dv = rep[0] + rep[1] == 1;
      bool e2dvb = dvb(0) + dvb(1) == 1;
      if (e2dvb)
        {
          if (e2dv)
            *this = dim_vector ();
          return true;
        }
      else if (e2dv)
        {
          *this = dvb;
          return true;
        }
    }

  return false;
}

dim_vector
dim_vector::redim (int n) const
{
  int n_dims = length ();

  if (n_dims == n)
    return *this;
  else if (n_dims < n)
    {
      dim_vector retval = alloc (n);

      for (int i = 0; i < n_dims; i++)
        retval.rep[i] = rep[i];

      for (int i = n_dims; i < n; i++)
        retval.rep[i] = 1;

      return retval;
    }
  else
    {
      if (n < 1) n = 1;

      dim_vector retval = alloc (n);

      retval.rep[1] = 1;

      for (int i = 0; i < n-1; i++)
        retval.rep[i] = rep[i];

      int k = rep[n-1];
      for (int i = n; i < n_dims; i++)
        k *= rep[i];

      retval.rep[n-1] = k;

      return retval;
    }
}
