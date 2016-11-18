/*

Copyright (C) 2004-2015 David Bateman
Copyright (C) 1998-2004 Andy Adler

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

#include "lo-error.h"
#include "lo-ieee.h"

#include "oct-spparms.h"
#include "singleton-cleanup.h"

octave_sparse_params *octave_sparse_params::instance = 0;

bool
octave_sparse_params::instance_ok (void)
{
  bool retval = true;

  if (! instance)
    {
      instance = new octave_sparse_params ();

      if (instance)
        singleton_cleanup_list::add (cleanup_instance);
    }

  if (! instance)
    {
      (*current_liboctave_error_handler)
        ("unable to create octave_sparse_params object!");

      retval = false;
    }

  return retval;
}

void
octave_sparse_params::defaults (void)
{
  if (instance_ok ())
    instance->do_defaults ();
}

void
octave_sparse_params::tight (void)
{
  if (instance_ok ())
    instance->do_tight ();
}

string_vector
octave_sparse_params::get_keys (void)
{
  return instance_ok () ? instance->do_get_keys () : string_vector ();
}

ColumnVector
octave_sparse_params::get_vals (void)
{
  return instance_ok () ? instance->do_get_vals () : ColumnVector ();
}

bool
octave_sparse_params::set_vals (const NDArray& vals)
{
  return instance_ok () ? instance->do_set_vals (vals) : false;
}

bool
octave_sparse_params::set_key (const std::string& key, const double& val)
{
  return instance_ok () ? instance->do_set_key (key, val) : false;
}

double
octave_sparse_params::get_key (const std::string& key)
{
  return instance_ok () ? instance->do_get_key (key) : octave_NaN;
}

double
octave_sparse_params::get_bandden (void)
{
  return instance_ok () ? instance->do_get_bandden () : 0.0;
}

void
octave_sparse_params::print_info (std::ostream& os, const std::string& prefix)
{
  if (instance_ok ())
    instance->do_print_info (os, prefix);
}

void
octave_sparse_params::do_defaults (void)
{
  params(0) = 0;      // spumoni
  params(1) = 1;      // ths_rel
  params(2) = 1;      // ths_abs
  params(3) = 0;      // exact_d
  params(4) = 3;      // supernd
  params(5) = 3;      // rreduce
  params(6) = 0.5;    // wh_frac
  params(7) = 1;      // autommd
  params(8) = 1;      // autoamd
  params(9) = 0.1;    // piv_tol
  params(10) = 0.5;   // bandden
  params(11) = 1;     // umfpack
  params(12) = 0.001; // sym_tol
}

void
octave_sparse_params::do_tight (void)
{
  params(0) = 0;      // spumoni
  params(1) = 1;      // ths_rel
  params(2) = 0;      // ths_abs
  params(3) = 1;      // exact_d
  params(4) = 1;      // supernd
  params(5) = 1;      // rreduce
  params(6) = 0.5;    // wh_frac
  params(7) = 1;      // autommd
  params(8) = 1;      // autoamd
  params(9) = 0.1;    // piv_tol
  params(10) = 0.5;   // bandden
  params(11) = 1;     // umfpack
  params(12) = 0.001; // sym_tol
}

void
octave_sparse_params::init_keys (void)
{
  keys(0) = "spumoni";
  keys(1) = "ths_rel";
  keys(2) = "ths_abs";
  keys(3) = "exact_d";
  keys(4) = "supernd";
  keys(5) = "rreduce";
  keys(6) = "wh_frac";
  keys(7) = "autommd";
  keys(8) = "autoamd";
  keys(9) = "piv_tol";
  keys(10) = "bandden";
  keys(11) = "umfpack";
  keys(12) = "sym_tol";
}

double
octave_sparse_params::do_get_bandden (void)
{
  return params(10);
}

bool
octave_sparse_params::do_set_vals (const NDArray& vals)
{
  octave_idx_type len = vals.length ();

  if (len > OCTAVE_SPARSE_CONTROLS_SIZE)
    {
      (*current_liboctave_error_handler)
        ("octave_sparse_params::do_set_vals: too many values");

      return false;
    }
  else
    {
      for (int i = 0; i < len; i++)
        params(i) = vals(i);

      return true;
    }
}

bool
octave_sparse_params::do_set_key (const std::string& key, const double& val)
{
  for (int i = 0; i < OCTAVE_SPARSE_CONTROLS_SIZE; i++)
    {
      if (keys (i) == key)
        {
          params(i) = val;
          return true;
        }
    }

  return false;
}

double
octave_sparse_params::do_get_key (const std::string& key)
{
  for (int i = 0; i < OCTAVE_SPARSE_CONTROLS_SIZE; i++)
    {
      if (keys (i) == key)
        return params(i);
    }

  return octave_NaN;
}

void
octave_sparse_params::do_print_info (std::ostream& os,
                                     const std::string& prefix) const
{
  for (int i = 0; i < OCTAVE_SPARSE_CONTROLS_SIZE; i++)
    os << prefix << keys(i) << ": " << params(i) << "\n";
}
