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

#if !defined (octave_CollocWt_h)
#define octave_CollocWt_h 1

#include <iosfwd>

#include "dMatrix.h"
#include "dColVector.h"

class
OCTAVE_API
CollocWt
{
public:

  CollocWt (void)
    : n (0), inc_left (0), inc_right (0), lb (0.0), rb (1.0),
      Alpha (0.0), Beta (0.0), r (), q (), A (), B (), initialized (false) { }

  CollocWt (octave_idx_type nc, octave_idx_type il, octave_idx_type ir)
    : n (nc), inc_left (il), inc_right (ir), lb (0.0), rb (1.0),
      Alpha (0.0), Beta (0.0), r (), q (), A (), B (), initialized (false) { }

  CollocWt (octave_idx_type nc, octave_idx_type il, octave_idx_type ir,
            double l, double rr)
    : n (nc), inc_left (il), inc_right (ir), lb (l), rb (rr),
      Alpha (0.0), Beta (0.0), r (), q (), A (), B (), initialized (false) { }

  CollocWt (octave_idx_type nc, double a, double b, octave_idx_type il,
            octave_idx_type ir)
    : n (nc), inc_left (il), inc_right (ir), lb (0.0), rb (1.0),
      Alpha (a), Beta (b), r (), q (), A (), B (), initialized (false) { }

  CollocWt (octave_idx_type nc, double a, double b, octave_idx_type il,
            octave_idx_type ir,
            double ll, double rr)
    : n (nc), inc_left (il), inc_right (ir), lb (ll), rb (rr),
      Alpha (a), Beta (b), r (), q (), A (), B (), initialized (false) { }

  CollocWt (const CollocWt& a)
    : n (a.n), inc_left (a.inc_left), inc_right (a.inc_right),
      lb (a.lb), rb (a.rb), Alpha (a.Alpha), Beta (a.Beta),
      r (a.r), q (a.q), A (a.A), B (a.B),
      initialized (a.initialized) { }

  CollocWt& operator = (const CollocWt& a)
  {
    if (this != &a)
      {
        n = a.n;
        inc_left = a.inc_left;
        inc_right = a.inc_right;
        lb = a.lb;
        rb = a.rb;
        r = a.r;
        q = a.q;
        A = a.A;
        B = a.B;
        initialized = a.initialized;
      }
    return *this;
  }

  ~CollocWt (void) { }

  CollocWt& resize (octave_idx_type nc)
  {
    n = nc;
    initialized = false;
    return *this;
  }

  CollocWt& add_left (void)
  {
    inc_left = 1;
    initialized = false;
    return *this;
  }

  CollocWt& delete_left (void)
  {
    inc_left = 0;
    initialized = false;
    return *this;
  }

  CollocWt& set_left (double val);

  CollocWt& add_right (void)
  {
    inc_right = 1;
    initialized = false;
    return *this;
  }

  CollocWt& delete_right (void)
  {
    inc_right = 0;
    initialized = false;
    return *this;
  }

  CollocWt& set_right (double val);

  CollocWt& set_alpha (double val)
  {
    Alpha = val;
    initialized = false;
    return *this;
  }

  CollocWt& set_beta (double val)
  {
    Beta = val;
    initialized = false;
    return *this;
  }

  octave_idx_type ncol (void) const { return n; }

  octave_idx_type left_included (void) const { return inc_left; }
  octave_idx_type right_included (void) const { return inc_right; }

  double left (void) const { return lb; }
  double right (void) const { return rb; }

  double width (void) const { return rb - lb; }

  double alpha (void) const { return Alpha; }
  double beta (void) const { return Beta; }

  ColumnVector roots (void) { if (!initialized) init (); return r; }
  ColumnVector quad (void) { if (!initialized) init (); return q; }

  ColumnVector quad_weights (void) { return quad (); }

  Matrix first (void) { if (!initialized) init (); return A; }

  Matrix second (void) { if (!initialized) init (); return B; }

  friend std::ostream& operator << (std::ostream&, const CollocWt&);

protected:

  octave_idx_type n;

  octave_idx_type inc_left;
  octave_idx_type inc_right;

  double lb;
  double rb;

  double Alpha;
  double Beta;

  ColumnVector r;
  ColumnVector q;

  Matrix A;
  Matrix B;

  bool initialized;

  void init (void);

  void error (const char *msg);
};

#endif
