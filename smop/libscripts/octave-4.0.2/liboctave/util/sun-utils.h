/*

Copyright (C) 1993-2015 John W. Eaton

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

#if !defined (octave_sun_utils_h)
#define octave_sun_utils_h 1

// This is only needed to dereference pointers to doubles if mixing
// GCC and Sun SPARC f77/cc compiled code.  See the GCC manual (where the
// function access_double() is described) and the Sun f77 manual,
// which explains that doubles are not always aligned on 8 byte
// boundaries.

#if defined (__sparc) && defined (__GNUC__)

inline double
access_double (double *unaligned_ptr)
{
  union d2i { double d; int i[2]; };

  union d2i *p = (union d2i *) unaligned_ptr;
  union d2i u;

  u.i[0] = p->i[0];
  u.i[1] = p->i[1];

  return u.d;
}

inline void
assign_double (double *unaligned_ptr, double value)
{
  union d2i { double d; int i[2]; };

  double *ptr = &value;
  union d2i *v = (union d2i *) ptr;
  union d2i *p = (union d2i *) unaligned_ptr;

  p->i[0] = v->i[0];
  p->i[1] = v->i[1];
}

#endif
#endif
