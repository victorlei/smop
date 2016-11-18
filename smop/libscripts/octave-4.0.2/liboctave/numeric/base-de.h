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

#if !defined (octave_base_de_h)
#define octave_base_de_h 1

#include <string>

#include "dColVector.h"
#include "dMatrix.h"

class
base_diff_eqn
{
public:

  base_diff_eqn (void)
    : x (), t (0.0), stop_time (0.0), stop_time_set (false),
      restart (true), integration_error (false), istate (0) { }

  base_diff_eqn (const ColumnVector& xx, double tt)
    : x (xx), t (tt), stop_time (0.0), stop_time_set (false),
      restart (true), integration_error (false), istate (0) { }

  base_diff_eqn (const base_diff_eqn& a)
    : x (a.x), t (a.t), stop_time (0.0), stop_time_set (false),
      restart (true), integration_error (false), istate (0) { }

  virtual ~base_diff_eqn (void) { }

  base_diff_eqn& operator = (const base_diff_eqn& a)
  {
    if (this != &a)
      {
        x = a.x;
        t = a.t;
        stop_time = a.stop_time;
        stop_time_set = a.stop_time_set;
        restart = a.restart;
        integration_error = a.integration_error;
        istate = a.istate;
      }

    return *this;
  }

  void initialize (const ColumnVector& x0, double t0)
  {
    x = x0;
    t = t0;
    integration_error = false;
    istate = 0;
    force_restart ();
  }

  octave_idx_type size (void) const { return x.capacity (); }

  ColumnVector state (void) const { return x; }

  double time (void) const { return t; }

  void set_stop_time (double tt)
  {
    stop_time_set = true;
    stop_time = tt;
    force_restart ();
  }

  void clear_stop_time (void)
  {
    stop_time_set = false;
    force_restart ();
  }

  virtual void force_restart (void) { restart = true; }

  bool integration_ok (void) const { return ! integration_error; }

  octave_idx_type integration_state (void) const { return istate; }

  virtual std::string error_message (void) const = 0;

protected:

  ColumnVector x;

  double t;

  double stop_time;

  bool stop_time_set;

  bool restart;

  bool integration_error;

  octave_idx_type istate;
};

#endif
