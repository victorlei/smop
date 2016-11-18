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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "base-qr.h"

template <class qr_type>
base_qr<qr_type>::base_qr (const qr_type& q_arg, const qr_type& r_arg)
  : q (q_arg), r (r_arg)
{
  octave_idx_type q_nr = q.rows ();
  octave_idx_type q_nc = q.columns ();
  octave_idx_type r_nr = r.rows ();
  octave_idx_type r_nc = r.columns ();

  if (! (q_nc == r_nr && (q_nr == q_nc || (q_nr > q_nc && r_nr == r_nc))))
    {
      q = qr_type ();
      r = qr_type ();

      (*current_liboctave_error_handler) ("QR dimensions mismatch");
    }
}

template <class qr_type>
qr_type_t
base_qr<qr_type>::get_type (void) const
{
  qr_type_t retval;

  if (!q.is_empty () && q.is_square ())
    retval = qr_type_std;
  else if (q.rows () > q.columns () && r.is_square ())
    retval = qr_type_economy;
  else
    retval = qr_type_raw;

  return retval;
}

template <class qr_type>
bool
base_qr<qr_type>::regular (void) const
{
  bool retval = true;

  octave_idx_type k = std::min (r.rows (), r.columns ());

  for (octave_idx_type i = 0; i < k; i++)
    {
      if (r(i, i) == qr_elt_type ())
        {
          retval = false;
          break;
        }
    }

  return retval;
}

