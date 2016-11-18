/*

Copyright (C) 1995-2015 John W. Eaton

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

#if !defined (octave_base_min_h)
#define octave_base_min_h 1

#include "dColVector.h"

class
base_minimizer
{
public:

  base_minimizer (void) : x () { }

  base_minimizer (const ColumnVector& xx) : x (xx) { }

  base_minimizer (const base_minimizer& a) : x (a.x) { }

  virtual ~base_minimizer (void) { }

  base_minimizer& operator = (const base_minimizer& a)
  {
    if (this != &a)
      x = a.x;

    return *this;
  }

  // Derived classes must provide a function to actually do the
  // minimization.

  virtual ColumnVector do_minimize (double& objf, octave_idx_type& inform,
                                    ColumnVector& lambda) = 0;

  // Lots of ways to call the single function and optionally set and
  // get additional information.

  virtual ColumnVector minimize (void)
  {
    double objf;
    octave_idx_type inform;
    ColumnVector lambda;
    return do_minimize (objf, inform, lambda);
  }

  virtual ColumnVector minimize (double& objf)
  {
    octave_idx_type inform;
    ColumnVector lambda;
    return do_minimize (objf, inform, lambda);
  }

  virtual ColumnVector minimize (double& objf, octave_idx_type& inform)
  {
    ColumnVector lambda;
    return do_minimize (objf, inform, lambda);
  }

  virtual ColumnVector minimize (double& objf, octave_idx_type& inform,
                                 ColumnVector& lambda)
  {
    return do_minimize (objf, inform, lambda);
  }

  virtual ColumnVector minimize (const ColumnVector& x0)
  {
    x = x0;
    double objf;
    octave_idx_type inform;
    ColumnVector lambda;
    return do_minimize (objf, inform, lambda);
  }

  virtual ColumnVector minimize (const ColumnVector& x0, double& objf)
  {
    x = x0;
    octave_idx_type inform;
    ColumnVector lambda;
    return do_minimize (objf, inform, lambda);
  }

  virtual ColumnVector minimize (const ColumnVector& x0, double& objf,
                                 octave_idx_type& inform)
  {
    x = x0;
    ColumnVector lambda;
    return do_minimize (objf, inform, lambda);
  }

  virtual ColumnVector minimize (const ColumnVector& x0, double& objf,
                                 octave_idx_type& inform, ColumnVector& lambda)
  {
    x = x0;
    return do_minimize (objf, inform, lambda);
  }

  octave_idx_type size (void) const { return x.capacity (); }

protected:

  ColumnVector x;
};

#endif
