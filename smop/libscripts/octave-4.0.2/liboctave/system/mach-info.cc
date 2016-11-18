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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "f77-fcn.h"
#include "lo-error.h"
#include "mach-info.h"
#include "singleton-cleanup.h"

extern "C"
{
  double F77_FUNC (d1mach, D1MACH) (const octave_idx_type&);
}

oct_mach_info *oct_mach_info::instance = 0;

union equiv
{
  double d;
  int i[2];
};

struct
float_params
{
  oct_mach_info::float_format fp_fmt;
  equiv fp_par[4];
};

#define INIT_FLT_PAR(fp, fmt, sm1, sm2, lrg1, lrg2, rt1, rt2, dv1, dv2) \
  do \
    { \
      fp.fp_fmt = (fmt); \
      fp.fp_par[0].i[0] = (sm1);  fp.fp_par[0].i[1] = (sm2); \
      fp.fp_par[1].i[0] = (lrg1); fp.fp_par[1].i[1] = (lrg2); \
      fp.fp_par[2].i[0] = (rt1);  fp.fp_par[2].i[1] = (rt2); \
      fp.fp_par[3].i[0] = (dv1);  fp.fp_par[3].i[1] = (dv2); \
    } \
  while (0)

static int
equiv_compare (const equiv *std, const equiv *v, int len)
{
  int i;
  for (i = 0; i < len; i++)
    if (v[i].i[0] != std[i].i[0] || v[i].i[1] != std[i].i[1])
      return 0;
  return 1;
}

static oct_mach_info::float_format
get_float_format (void)
{
  oct_mach_info::float_format retval = oct_mach_info::flt_fmt_unknown;

  float_params fp[5];

  INIT_FLT_PAR (fp[0], oct_mach_info::flt_fmt_ieee_big_endian,
                   1048576,  0,
                2146435071, -1,
                1017118720,  0,
                1018167296,  0);

  INIT_FLT_PAR (fp[1], oct_mach_info::flt_fmt_ieee_little_endian,
                 0,    1048576,
                -1, 2146435071,
                 0, 1017118720,
                 0, 1018167296);

  INIT_FLT_PAR (fp[4], oct_mach_info::flt_fmt_unknown,
                0, 0,
                0, 0,
                0, 0,
                0, 0);

  equiv mach_fp_par[4];

  mach_fp_par[0].d = F77_FUNC (d1mach, D1MACH) (1);
  mach_fp_par[1].d = F77_FUNC (d1mach, D1MACH) (2);
  mach_fp_par[2].d = F77_FUNC (d1mach, D1MACH) (3);
  mach_fp_par[3].d = F77_FUNC (d1mach, D1MACH) (4);

  int i = 0;
  do
    {
      if (equiv_compare (fp[i].fp_par, mach_fp_par, 4))
        {
          retval = fp[i].fp_fmt;
          break;
        }
    }
  while (fp[++i].fp_fmt != oct_mach_info::flt_fmt_unknown);

  return retval;
}

static bool
ten_little_endians (void)
{
  // Are we little or big endian?  From Harbison & Steele.

  union
  {
    long l;
    char c[sizeof (long)];
  } u;

  u.l = 1;

  return (u.c[sizeof (long) - 1] == 1);
}

oct_mach_info::oct_mach_info (void)
  : native_float_fmt (get_float_format ()),
    big_chief (ten_little_endians ()) { }

bool
oct_mach_info::instance_ok (void)
{
  bool retval = true;

  if (! instance)
    {
      instance = new oct_mach_info ();

      if (instance)
        singleton_cleanup_list::add (cleanup_instance);
    }

  if (! instance)
    {
      (*current_liboctave_error_handler)
        ("unable to create command history object!");

      retval = false;
    }

  return retval;
}

oct_mach_info::float_format
oct_mach_info::native_float_format (void)
{
  return (instance_ok ())
         ? instance->native_float_fmt : oct_mach_info::flt_fmt_unknown;
}

bool
oct_mach_info::words_big_endian (void)
{
  return (instance_ok ())
         ? instance->big_chief : false;
}

bool
oct_mach_info::words_little_endian (void)
{
  return (instance_ok ())
         ? (! instance->big_chief) : false;
}

oct_mach_info::float_format
oct_mach_info::string_to_float_format (const std::string& s)
{
  oct_mach_info::float_format retval = oct_mach_info::flt_fmt_unknown;

  if (s == "native" || s == "n")
    retval = oct_mach_info::native_float_format ();
  else if (s == "ieee-be" || s == "b")
    retval = oct_mach_info::flt_fmt_ieee_big_endian;
  else if (s == "ieee-le" || s == "l")
    retval = oct_mach_info::flt_fmt_ieee_little_endian;
  else if (s == "unknown")
    retval = oct_mach_info::flt_fmt_unknown;
  else
    (*current_liboctave_error_handler)
      ("invalid architecture type specified");

  return retval;
}

std::string
oct_mach_info::float_format_as_string (float_format flt_fmt)
{
  std::string retval = "unknown";

  switch (flt_fmt)
    {
    case flt_fmt_ieee_big_endian:
      retval = "ieee-be";
      break;

    case flt_fmt_ieee_little_endian:
      retval = "ieee-le";
      break;

    default:
      break;
    }

  return retval;
}
