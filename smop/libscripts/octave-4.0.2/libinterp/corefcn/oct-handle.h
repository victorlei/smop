/*

Copyright (C) 2007-2015 John W. Eaton

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

#if !defined (octave_oct_handle_h)
#define octave_oct_handle_h 1

#include "dMatrix.h"
#include "lo-ieee.h"

#include "ov.h"

// ---------------------------------------------------------------------

class octave_handle
{
public:
  octave_handle (void) : val (octave_NaN) { }

  octave_handle (const octave_value& a)
    : val (octave_NaN)
  {
    if (a.is_empty ())
      /* do nothing */;
    else
      {
        double tval = a.double_value ();

        if (! error_state)
          val = tval;
        else
          error ("invalid handle");
      }
  }

  octave_handle (int a) : val (a) { }

  octave_handle (double a) : val (a) { }

  octave_handle (const octave_handle& a) : val (a.val) { }

  octave_handle& operator = (const octave_handle& a)
  {
    if (&a != this)
      val = a.val;

    return *this;
  }

  ~octave_handle (void) { }

  double value (void) const { return val; }

  octave_value as_octave_value (void) const
  {
    return ok () ? octave_value (val) : octave_value (Matrix ());
  }

  // Prefix increment/decrement operators.
  octave_handle& operator ++ (void)
  {
    ++val;
    return *this;
  }

  octave_handle& operator -- (void)
  {
    --val;
    return *this;
  }

  // Postfix increment/decrement operators.
  const octave_handle operator ++ (int)
  {
    octave_handle old_value = *this;
    ++(*this);
    return old_value;
  }

  const octave_handle operator -- (int)
  {
    octave_handle old_value = *this;
    --(*this);
    return old_value;
  }

  bool ok (void) const { return ! xisnan (val); }

private:
  double val;
};

inline bool
operator == (const octave_handle& a, const octave_handle& b)
{
  return a.value () == b.value ();
}

inline bool
operator != (const octave_handle& a, const octave_handle& b)
{
  return a.value () != b.value ();
}

inline bool
operator < (const octave_handle& a, const octave_handle& b)
{
  return a.value () < b.value ();
}

inline bool
operator <= (const octave_handle& a, const octave_handle& b)
{
  return a.value () <= b.value ();
}

inline bool
operator >= (const octave_handle& a, const octave_handle& b)
{
  return a.value () >= b.value ();
}

inline bool
operator > (const octave_handle& a, const octave_handle& b)
{
  return a.value () > b.value ();
}

#endif
