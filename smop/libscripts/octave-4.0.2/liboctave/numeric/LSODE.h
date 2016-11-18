/*

Copyright (C) 1996-2015 John W. Eaton

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

#if !defined (octave_LSODE_h)
#define octave_LSODE_h 1

#include <cfloat>

#include "LSODE-opts.h"
#include "lo-math.h"

class
OCTAVE_API
LSODE : public ODE, public LSODE_options
{
public:

  LSODE (void)
    : ODE (), LSODE_options (), initialized (false), method_flag (0),
      maxord (0), itask (0), iopt (0), itol (0), liw (0), lrw (0),
      iwork (), rwork (), rel_tol (0.0), abs_tol () { }

  LSODE (const ColumnVector& s, double tm, const ODEFunc& f)
    : ODE (s, tm, f), LSODE_options (), initialized (false), method_flag (0),
      maxord (0), itask (0), iopt (0), itol (0), liw (0), lrw (0),
      iwork (), rwork (), rel_tol (0.0), abs_tol () { }

  ~LSODE (void) { }

  ColumnVector do_integrate (double t);

  Matrix do_integrate (const ColumnVector& tout);

  Matrix do_integrate (const ColumnVector& tout, const ColumnVector& tcrit);

  std::string error_message (void) const;

private:

  bool initialized;

  octave_idx_type method_flag;
  octave_idx_type maxord;
  octave_idx_type itask;
  octave_idx_type iopt;
  octave_idx_type itol;

  octave_idx_type liw;
  octave_idx_type lrw;

  Array<octave_idx_type> iwork;
  Array<double> rwork;

  double rel_tol;

  Array<double> abs_tol;
};

#endif
