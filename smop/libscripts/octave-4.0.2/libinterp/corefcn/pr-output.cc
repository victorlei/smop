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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cfloat>
#include <cstdio>
#include <cstring>

#include <iomanip>
#include <iostream>
#include <sstream>
#include <string>

#include "Array-util.h"
#include "CMatrix.h"
#include "Range.h"
#include "cmd-edit.h"
#include "dMatrix.h"
#include "lo-mappers.h"
#include "lo-math.h"
#include "mach-info.h"
#include "oct-cmplx.h"
#include "quit.h"
#include "str-vec.h"

#include "Cell.h"
#include "defun.h"
#include "error.h"
#include "gripes.h"
#include "oct-obj.h"
#include "oct-stream.h"
#include "pager.h"
#include "pr-output.h"
#include "sysdep.h"
#include "unwind-prot.h"
#include "utils.h"
#include "variables.h"

// TRUE means use a scaled fixed point format for 'format long' and
// 'format short'.
static bool Vfixed_point_format = false;

// The maximum field width for a number printed by the default output
// routines.
static int Voutput_max_field_width = 10;

// The precision of the numbers printed by the default output
// routines.
static int Voutput_precision = 5;

// TRUE means that the dimensions of empty objects should be printed
// like this: x = [](2x0).
bool Vprint_empty_dimensions = true;

// TRUE means that the rows of big matrices should be split into
// smaller slices that fit on the screen.
static bool Vsplit_long_rows = true;

// TRUE means don't do any fancy formatting.
static bool free_format = false;

// TRUE means print plus sign for nonzero, blank for zero.
static bool plus_format = false;

// First char for > 0, second for < 0, third for == 0.
static std::string plus_format_chars = "+- ";

// TRUE means always print in a rational approximation
static bool rat_format = false;

// Used to force the length of the rational approximation string for Frats
static int rat_string_len = -1;

// TRUE means always print like dollars and cents.
static bool bank_format = false;

// TRUE means print data in hexadecimal format.
static int hex_format = 0;

// TRUE means print data in binary-bit-pattern format.
static int bit_format = 0;

// TRUE means don't put newlines around the column number headers.
bool Vcompact_format = false;

// TRUE means use an e format.
static bool print_e = false;

// TRUE means use a g format.
static bool print_g = false;

// TRUE means print E instead of e for exponent field.
static bool print_big_e = false;

// TRUE means use an engineering format.
static bool print_eng = false;

class pr_engineering_float;
class pr_formatted_float;
class pr_rational_float;

static int
current_output_max_field_width (void)
{
  return Voutput_max_field_width;
}

static int
current_output_precision (void)
{
  return Voutput_precision;
}

class
float_format
{
public:

  float_format (int w = current_output_max_field_width (),
                int p = current_output_precision (), int f = 0)
    : fw (w), ex (0), prec (p), fmt (f), up (0), sp (0) { }

  float_format (int w, int e, int p, int f)
    : fw (w), ex (e), prec (p), fmt (f), up (0), sp (0) { }

  float_format (const float_format& ff)
    : fw (ff.fw), ex (ff.ex), prec (ff.prec), fmt (ff.fmt), up (ff.up),
      sp (ff.sp) { }

  float_format& operator = (const float_format& ff)
  {
    if (&ff != this)
      {
        fw = ff.fw;
        ex = ff.ex;
        prec = ff.prec;
        fmt = ff.fmt;
        up = ff.up;
        sp = ff.sp;
      }

    return *this;
  }

  ~float_format (void) { }

  float_format& scientific (void) { fmt = std::ios::scientific; return *this; }
  float_format& fixed (void) { fmt = std::ios::fixed; return *this; }
  float_format& general (void) { fmt = 0; return *this; }

  float_format& uppercase (void) { up = std::ios::uppercase; return *this; }
  float_format& lowercase (void) { up = 0; return *this; }

  float_format& precision (int p) { prec = p; return *this; }

  float_format& width (int w) { fw = w; return *this; }

  float_format& trailing_zeros (bool tz = true)
  { sp = tz ? std::ios::showpoint : 0; return *this; }

  friend std::ostream& operator << (std::ostream& os,
                                    const pr_engineering_float& pef);

  friend std::ostream& operator << (std::ostream& os,
                                    const pr_formatted_float& pff);

  friend std::ostream& operator << (std::ostream& os,
                                    const pr_rational_float& prf);

private:

  // Field width.  Zero means as wide as necessary.
  int fw;

  // Exponent Field width.  Zero means as wide as necessary.
  int ex;

  // Precision.
  int prec;

  // Format.
  int fmt;

  // E or e.
  int up;

  // Show trailing zeros.
  int sp;
};

static int
calc_scale_exp (const int& x)
{
  if (! print_eng)
    return x;
  else
    return x - 3*static_cast<int> (x/3);
  /* The expression above is equivalent to x - (x % 3).
   * According to the ISO specification for C++ the modulo operator is
   * compiler dependent if any of the arguments are negative.  Since this
   * function will need to work on negative arguments, and we want to avoid
   * portability issues, we re-implement the modulo function to the desired
   * behavior (truncation).  There may be a gnulib replacement.
   *
   * ISO/IEC 14882:2003 : Programming languages -- C++. 5.6.4: ISO, IEC. 2003 .
   * "the binary % operator yields the remainder from the division of the first
   * expression by the second. .... If both operands are nonnegative then the
   * remainder is nonnegative; if not, the sign of the remainder is
   * implementation-defined".  */
}

static int
engineering_exponent (const double& x)
{
  int ex = 0;
  if (x != 0)
    {
      double absval = (x < 0.0 ? -x : x);
      int logabsval = static_cast<int> (gnulib::floor (log10 (absval)));
      /* Avoid using modulo function with negative arguments for portability.
       * See extended comment at calc_scale_exp */
      if (logabsval < 0.0)
        ex = logabsval - 2 + ((-logabsval + 2) % 3);
      else
        ex = logabsval - (logabsval % 3);
    }
  return ex;
}

static int
num_digits (const double& x)
{
  return 1 + (print_eng
              ? engineering_exponent (x)
              : static_cast<int> (gnulib::floor (log10 (x))));
}

class
pr_engineering_float
{
public:

  const float_format& f;

  double val;

  int exponent (void) const
  {
    return engineering_exponent (val);
  }

  double mantissa (void) const
  {
    return val / std::pow (10.0, exponent ());
  }

  pr_engineering_float (const float_format& f_arg, double val_arg)
    : f (f_arg), val (val_arg) { }
};

std::ostream&
operator << (std::ostream& os, const pr_engineering_float& pef)
{
  octave_preserve_stream_state stream_state (os);

  if (pef.f.fw >= 0)
    os << std::setw (pef.f.fw - pef.f.ex);

  if (pef.f.prec >= 0)
    os << std::setprecision (pef.f.prec);

  os.flags (static_cast<std::ios::fmtflags>
            (pef.f.fmt | pef.f.up | pef.f.sp));

  os << pef.mantissa ();

  int ex = pef.exponent ();
  if (ex < 0)
    {
      os << std::setw (0) << "e-";
      ex = -ex;
    }
  else
    os << std::setw (0) << "e+";

  os << std::setw (pef.f.ex - 2) << std::setfill ('0') << ex;

  return os;
}

class
pr_formatted_float
{
public:

  const float_format& f;

  double val;

  pr_formatted_float (const float_format& f_arg, double val_arg)
    : f (f_arg), val (val_arg) { }
};

std::ostream&
operator << (std::ostream& os, const pr_formatted_float& pff)
{
  octave_preserve_stream_state stream_state (os);

  if (pff.f.fw >= 0)
    os << std::setw (pff.f.fw);

  if (pff.f.prec >= 0)
    os << std::setprecision (pff.f.prec);

  os.flags (static_cast<std::ios::fmtflags>
            (pff.f.fmt | pff.f.up | pff.f.sp));

  os << pff.val;

  return os;
}

static inline std::string
rational_approx (double val, int len)
{
  std::string s;

  if (len <= 0)
    len = 10;

  if (xisinf (val))
    s = "1/0";
  else if (xisnan (val))
    s = "0/0";
  else if (val < std::numeric_limits<int>::min ()
           || val > std::numeric_limits<int>::max ()
           || D_NINT (val) == val)
    {
      std::ostringstream buf;
      buf.flags (std::ios::fixed);
      buf << std::setprecision (0) << xround (val);
      s = buf.str ();
    }
  else
    {
      double lastn = 1.;
      double lastd = 0.;
      double n = xround (val);
      double d = 1.;
      double frac = val - n;
      int m = 0;

      std::ostringstream buf2;
      buf2.flags (std::ios::fixed);
      buf2 << std::setprecision (0) << static_cast<int>(n);
      s = buf2.str ();

      while (1)
        {
          double flip = 1. / frac;
          double step = xround (flip);
          double nextn = n;
          double nextd = d;

          // Have we converged to 1/intmax ?
          if (fabs (frac) < 1 / static_cast<double> (std::numeric_limits<int>::max ()))
            {
              lastn = n;
              lastd = d;
              break;
            }

          frac = flip - step;
          n = n * step + lastn;
          d = d * step + lastd;
          lastn = nextn;
          lastd = nextd;

          std::ostringstream buf;
          buf.flags (std::ios::fixed);
          buf << std::setprecision (0) << static_cast<int>(n)
              << "/" << static_cast<int>(d);
          m++;

          if (n < 0 && d < 0)
            {
              // Double negative, string can be two characters longer..
              if (buf.str ().length () > static_cast<unsigned int>(len + 2))
                break;
            }
          else if (buf.str ().length () > static_cast<unsigned int>(len))
            break;

          if (fabs (n) > std::numeric_limits<int>::max ()
              || fabs (d) > std::numeric_limits<int>::max ())
            break;

          s = buf.str ();
        }

      if (lastd < 0.)
        {
          // Move sign to the top
          lastd = - lastd;
          lastn = - lastn;
          std::ostringstream buf;
          buf.flags (std::ios::fixed);
          buf << std::setprecision (0) << static_cast<int>(lastn)
              << "/" << static_cast<int>(lastd);
          s = buf.str ();
        }
    }

  return s;
}

/*
%!assert (rats (2.0005, 9), "4001/2000")
%!assert (rats (-2.0005, 10), "-4001/2000")
%!assert (strtrim (rats (2.0005, 30)), "4001/2000")
%!assert (pi - str2num (rats (pi, 30)), 0, 4 * eps)
%!assert (e - str2num (rats (e, 30)), 0, 4 * eps)
%!assert (rats (123, 2), " *")

%!test
%! v = 1 / double (intmax);
%! err = v - str2num (rats(v, 12));
%! assert (err, 0, 4 * eps);
*/

class
pr_rational_float
{
public:

  const float_format& f;

  double val;

  pr_rational_float (const float_format& f_arg, double val_arg)
    : f (f_arg), val (val_arg) { }
};

std::ostream&
operator << (std::ostream& os, const pr_rational_float& prf)
{
  octave_preserve_stream_state stream_state (os);

  int fw = (rat_string_len > 0 ? rat_string_len : prf.f.fw);
  std::string s = rational_approx (prf.val, fw);

  if (fw >= 0)
    os << std::setw (fw);

  os.flags (static_cast<std::ios::fmtflags>
            (prf.f.fmt | prf.f.up | prf.f.sp));

  if (fw > 0 && s.length () > static_cast<unsigned int>(fw))
    os << "*";
  else
    os << s;

  return os;
}

// Current format for real numbers and the real part of complex
// numbers.
static float_format *curr_real_fmt = 0;

// Current format for the imaginary part of complex numbers.
static float_format *curr_imag_fmt = 0;

static double
pr_max_internal (const Matrix& m)
{
  octave_idx_type nr = m.rows ();
  octave_idx_type nc = m.columns ();

  double result = -std::numeric_limits<double>::max ();

  bool all_inf_or_nan = true;

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      {
        double val = m(i,j);
        if (! xfinite (val))
          continue;

        all_inf_or_nan = false;

        if (val > result)
          result = val;
      }

  if (all_inf_or_nan)
    result = 0.0;

  return result;
}

static double
pr_min_internal (const Matrix& m)
{
  octave_idx_type nr = m.rows ();
  octave_idx_type nc = m.columns ();

  double result = std::numeric_limits<double>::max ();

  bool all_inf_or_nan = true;

  for (octave_idx_type j = 0; j < nc; j++)
    for (octave_idx_type i = 0; i < nr; i++)
      {
        double val = m(i,j);
        if (! xfinite (val))
          continue;

        all_inf_or_nan = false;

        if (val < result)
          result = val;
      }

  if (all_inf_or_nan)
    result = 0.0;

  return result;
}

// FIXME: it would be nice to share more code among these functions,..

static void
set_real_format (int digits, bool inf_or_nan, bool int_only, int &fw)
{
  static float_format fmt;

  int prec = Voutput_precision;

  int ld, rd;

  if (rat_format)
    {
      fw = 0;
      rd = 0;
    }
  else if (bank_format)
    {
      fw = digits < 0 ? 5 : digits + 4;
      if (inf_or_nan && fw < 5)
        fw = 5;
      rd = 2;
    }
  else if (hex_format)
    {
      fw = 2 * sizeof (double);
      rd = 0;
    }
  else if (bit_format)
    {
      fw = 8 * sizeof (double);
      rd = 0;
    }
  else if (inf_or_nan || int_only)
    {
      fw = 1 + digits;
      if (inf_or_nan && fw < 4)
        fw = 4;
      rd = fw;
    }
  else
    {
      if (digits > 0)
        {
          ld = digits;
          rd = prec > digits ? prec - digits : prec;
        }
      else
        {
          ld = 1;
          rd = prec > digits ? prec - digits : prec;
        }

      fw = 1 + ld + 1 + rd;
      if (inf_or_nan && fw < 4)
        fw = 4;
    }

  if (! (rat_format || bank_format || hex_format || bit_format)
      && (fw > Voutput_max_field_width || print_e || print_g || print_eng))
    {
      if (print_g)
        fmt = float_format ();
      else
        {
          // e+ddd
          int ex = 5;

          if (print_eng)
            {
              // -ddd.
              fw = 5 + prec + ex;
              if (inf_or_nan && fw < 6)
                fw = 6;
              fmt = float_format (fw, ex, prec - 1, std::ios::fixed);
            }
          else
            {
              // -d.
              fw = 3 + prec + ex;
              if (inf_or_nan && fw < 4)
                fw = 4;
              fmt = float_format (fw, ex, prec - 1, std::ios::scientific);
            }
        }

      if (print_big_e)
        fmt.uppercase ();
    }
  else if (! bank_format && (inf_or_nan || int_only))
    fmt = float_format (fw, rd);
  else
    fmt = float_format (fw, rd, std::ios::fixed);

  curr_real_fmt = &fmt;
}

static void
set_format (double d, int& fw)
{
  curr_real_fmt = 0;
  curr_imag_fmt = 0;

  if (free_format)
    return;

  bool inf_or_nan = (xisinf (d) || xisnan (d));

  bool int_only = (! inf_or_nan && D_NINT (d) == d);

  double d_abs = d < 0.0 ? -d : d;

  int digits = (inf_or_nan || d_abs == 0.0) ? 0 : num_digits (d_abs);

  set_real_format (digits, inf_or_nan, int_only, fw);
}

static inline void
set_format (double d)
{
  int fw;
  set_format (d, fw);
}

static void
set_real_matrix_format (int x_max, int x_min, bool inf_or_nan,
                        int int_or_inf_or_nan, int& fw)
{
  static float_format fmt;

  int prec = Voutput_precision;

  int ld, rd;

  if (rat_format)
    {
      fw = 9;
      rd = 0;
    }
  else if (bank_format)
    {
      int digits = x_max > x_min ? x_max : x_min;
      fw = digits <= 0 ? 5 : digits + 4;
      if (inf_or_nan && fw < 5)
        fw = 5;
      rd = 2;
    }
  else if (hex_format)
    {
      fw = 2 * sizeof (double);
      rd = 0;
    }
  else if (bit_format)
    {
      fw = 8 * sizeof (double);
      rd = 0;
    }
  else if (Vfixed_point_format && ! print_g)
    {
      rd = prec;
      fw = rd + 2;
      if (inf_or_nan && fw < 4)
        fw = 4;
    }
  else if (int_or_inf_or_nan)
    {
      int digits = x_max > x_min ? x_max : x_min;
      fw = digits <= 0 ? 2 : digits + 1;
      if (inf_or_nan && fw < 4)
        fw = 4;
      rd = fw;
    }
  else
    {
      int ld_max, rd_max;
      if (x_max > 0)
        {
          ld_max = x_max;
          rd_max = prec > x_max ? prec - x_max : prec;
          x_max++;
        }
      else
        {
          ld_max = 1;
          rd_max = prec > x_max ? prec - x_max : prec;
          x_max = -x_max + 1;
        }

      int ld_min, rd_min;
      if (x_min > 0)
        {
          ld_min = x_min;
          rd_min = prec > x_min ? prec - x_min : prec;
          x_min++;
        }
      else
        {
          ld_min = 1;
          rd_min = prec > x_min ? prec - x_min : prec;
          x_min = -x_min + 1;
        }

      ld = ld_max > ld_min ? ld_max : ld_min;
      rd = rd_max > rd_min ? rd_max : rd_min;

      fw = 1 + ld + 1 + rd;
      if (inf_or_nan && fw < 4)
        fw = 4;
    }

  if (! (rat_format || bank_format || hex_format || bit_format)
      && (print_e
          || print_eng || print_g
          || (! Vfixed_point_format && fw > Voutput_max_field_width)))
    {
      if (print_g)
        fmt = float_format ();
      else
        {
          int ex = 4;
          if (x_max > 100 || x_min > 100)
            ex++;

          if (print_eng)
            {
              fw = 4 + prec + ex;
              if (inf_or_nan && fw < 6)
                fw = 6;
              fmt = float_format (fw, ex, prec - 1, std::ios::fixed);
            }
          else
            {
              fw = 2 + prec + ex;
              if (inf_or_nan && fw < 4)
                fw = 4;
              fmt = float_format (fw, prec - 1, std::ios::scientific);
            }
        }

      if (print_big_e)
        fmt.uppercase ();
    }
  else if (! bank_format && int_or_inf_or_nan)
    fmt = float_format (fw, rd);
  else
    fmt = float_format (fw, rd, std::ios::fixed);

  curr_real_fmt = &fmt;
}

static void
set_format (const Matrix& m, int& fw, double& scale)
{
  curr_real_fmt = 0;
  curr_imag_fmt = 0;

  if (free_format)
    return;

  bool inf_or_nan = m.any_element_is_inf_or_nan ();

  bool int_or_inf_or_nan = m.all_elements_are_int_or_inf_or_nan ();

  Matrix m_abs = m.abs ();
  double max_abs = pr_max_internal (m_abs);
  double min_abs = pr_min_internal (m_abs);

  int x_max = max_abs == 0.0 ? 0 : num_digits (max_abs);

  int x_min = min_abs == 0.0 ? 0 : num_digits (min_abs);

  scale = (x_max == 0 || int_or_inf_or_nan)
          ? 1.0 : std::pow (10.0, calc_scale_exp (x_max - 1));

  set_real_matrix_format (x_max, x_min, inf_or_nan, int_or_inf_or_nan, fw);
}

static inline void
set_format (const Matrix& m)
{
  int fw;
  double scale;
  set_format (m, fw, scale);
}

static void
set_complex_format (int x_max, int x_min, int r_x, bool inf_or_nan,
                    int int_only, int& r_fw, int& i_fw)
{
  static float_format r_fmt;
  static float_format i_fmt;

  int prec = Voutput_precision;

  int ld, rd;

  if (rat_format)
    {
      i_fw = 0;
      r_fw = 0;
      rd = 0;
    }
  else if (bank_format)
    {
      int digits = r_x;
      i_fw = 0;
      r_fw = digits <= 0 ? 5 : digits + 4;
      if (inf_or_nan && r_fw < 5)
        r_fw = 5;
      rd = 2;
    }
  else if (hex_format)
    {
      r_fw = 2 * sizeof (double);
      i_fw = 2 * sizeof (double);
      rd = 0;
    }
  else if (bit_format)
    {
      r_fw = 8 * sizeof (double);
      i_fw = 8 * sizeof (double);
      rd = 0;
    }
  else if (inf_or_nan || int_only)
    {
      int digits = x_max > x_min ? x_max : x_min;
      i_fw = digits <= 0 ? 1 : digits;
      r_fw = i_fw + 1;
      if (inf_or_nan && i_fw < 3)
        {
          i_fw = 3;
          r_fw = 4;
        }
      rd = r_fw;
    }
  else
    {
      int ld_max, rd_max;
      if (x_max > 0)
        {
          ld_max = x_max;
          rd_max = prec > x_max ? prec - x_max : prec;
          x_max++;
        }
      else
        {
          ld_max = 1;
          rd_max = prec > x_max ? prec - x_max : prec;
          x_max = -x_max + 1;
        }

      int ld_min, rd_min;
      if (x_min > 0)
        {
          ld_min = x_min;
          rd_min = prec > x_min ? prec - x_min : prec;
          x_min++;
        }
      else
        {
          ld_min = 1;
          rd_min = prec > x_min ? prec - x_min : prec;
          x_min = -x_min + 1;
        }

      ld = ld_max > ld_min ? ld_max : ld_min;
      rd = rd_max > rd_min ? rd_max : rd_min;

      i_fw = ld + 1 + rd;
      r_fw = i_fw + 1;
      if (inf_or_nan && i_fw < 3)
        {
          i_fw = 3;
          r_fw = 4;
        }
    }

  if (! (rat_format || bank_format || hex_format || bit_format)
      && (r_fw > Voutput_max_field_width || print_e || print_eng || print_g))
    {
      if (print_g)
        {
          r_fmt = float_format ();
          i_fmt = float_format ();
        }
      else
        {
          int ex = 4;
          if (x_max > 100 || x_min > 100)
            ex++;

          if (print_eng)
            {
              i_fw =  3 + prec + ex;
              r_fw = i_fw + 1;
              if (inf_or_nan && i_fw < 5)
                {
                  i_fw = 5;
                  r_fw = 6;
                }
              r_fmt = float_format (r_fw, ex, prec - 1, std::ios::fixed);
              i_fmt = float_format (i_fw, ex, prec - 1, std::ios::fixed);
            }
          else
            {
              i_fw = 1 + prec + ex;
              r_fw = i_fw + 1;
              if (inf_or_nan && i_fw < 3)
                {
                  i_fw = 3;
                  r_fw = 4;
                }
              r_fmt = float_format (r_fw, prec - 1, std::ios::scientific);
              i_fmt = float_format (i_fw, prec - 1, std::ios::scientific);
            }
        }

      if (print_big_e)
        {
          r_fmt.uppercase ();
          i_fmt.uppercase ();
        }
    }
  else if (! bank_format && (inf_or_nan || int_only))
    {
      r_fmt = float_format (r_fw, rd);
      i_fmt = float_format (i_fw, rd);
    }
  else
    {
      r_fmt = float_format (r_fw, rd, std::ios::fixed);
      i_fmt = float_format (i_fw, rd, std::ios::fixed);
    }

  curr_real_fmt = &r_fmt;
  curr_imag_fmt = &i_fmt;
}

static void
set_format (const Complex& c, int& r_fw, int& i_fw)
{
  curr_real_fmt = 0;
  curr_imag_fmt = 0;

  if (free_format)
    return;

  double rp = c.real ();
  double ip = c.imag ();

  bool inf_or_nan = (xisinf (c) || xisnan (c));

  bool int_only = (D_NINT (rp) == rp && D_NINT (ip) == ip);

  double r_abs = rp < 0.0 ? -rp : rp;
  double i_abs = ip < 0.0 ? -ip : ip;

  int r_x = (! xfinite (rp) || r_abs == 0.0) ? 0 : num_digits (r_abs);

  int i_x = (! xfinite (ip) || i_abs == 0.0) ? 0 : num_digits (i_abs);

  int x_max, x_min;

  if (r_x > i_x)
    {
      x_max = r_x;
      x_min = i_x;
    }
  else
    {
      x_max = i_x;
      x_min = r_x;
    }

  set_complex_format (x_max, x_min, r_x, inf_or_nan, int_only, r_fw, i_fw);
}

static inline void
set_format (const Complex& c)
{
  int r_fw, i_fw;
  set_format (c, r_fw, i_fw);
}

static void
set_complex_matrix_format (int x_max, int x_min, int r_x_max,
                           int r_x_min, bool inf_or_nan,
                           int int_or_inf_or_nan, int& r_fw, int& i_fw)
{
  static float_format r_fmt;
  static float_format i_fmt;

  int prec = Voutput_precision;

  int ld, rd;

  if (rat_format)
    {
      i_fw = 9;
      r_fw = 9;
      rd = 0;
    }
  else if (bank_format)
    {
      int digits = r_x_max > r_x_min ? r_x_max : r_x_min;
      i_fw = 0;
      r_fw = digits <= 0 ? 5 : digits + 4;
      if (inf_or_nan && r_fw < 5)
        r_fw = 5;
      rd = 2;
    }
  else if (hex_format)
    {
      r_fw = 2 * sizeof (double);
      i_fw = 2 * sizeof (double);
      rd = 0;
    }
  else if (bit_format)
    {
      r_fw = 8 * sizeof (double);
      i_fw = 8 * sizeof (double);
      rd = 0;
    }
  else if (Vfixed_point_format && ! print_g)
    {
      rd = prec;
      i_fw = rd + 1;
      r_fw = i_fw + 1;
      if (inf_or_nan && i_fw < 3)
        {
          i_fw = 3;
          r_fw = 4;
        }
    }
  else if (int_or_inf_or_nan)
    {
      int digits = x_max > x_min ? x_max : x_min;
      i_fw = digits <= 0 ? 1 : digits;
      r_fw = i_fw + 1;
      if (inf_or_nan && i_fw < 3)
        {
          i_fw = 3;
          r_fw = 4;
        }
      rd = r_fw;
    }
  else
    {
      int ld_max, rd_max;
      if (x_max > 0)
        {
          ld_max = x_max;
          rd_max = prec > x_max ? prec - x_max : prec;
          x_max++;
        }
      else
        {
          ld_max = 1;
          rd_max = prec > x_max ? prec - x_max : prec;
          x_max = -x_max + 1;
        }

      int ld_min, rd_min;
      if (x_min > 0)
        {
          ld_min = x_min;
          rd_min = prec > x_min ? prec - x_min : prec;
          x_min++;
        }
      else
        {
          ld_min = 1;
          rd_min = prec > x_min ? prec - x_min : prec;
          x_min = -x_min + 1;
        }

      ld = ld_max > ld_min ? ld_max : ld_min;
      rd = rd_max > rd_min ? rd_max : rd_min;

      i_fw = ld + 1 + rd;
      r_fw = i_fw + 1;
      if (inf_or_nan && i_fw < 3)
        {
          i_fw = 3;
          r_fw = 4;
        }
    }

  if (! (rat_format || bank_format || hex_format || bit_format)
      && (print_e
          || print_eng || print_g
          || (! Vfixed_point_format && r_fw > Voutput_max_field_width)))
    {
      if (print_g)
        {
          r_fmt = float_format ();
          i_fmt = float_format ();
        }
      else
        {
          int ex = 4;
          if (x_max > 100 || x_min > 100)
            ex++;

          if (print_eng)
            {
              i_fw = 3 + prec + ex;
              r_fw = i_fw + 1;
              if (inf_or_nan && i_fw < 5)
                {
                  i_fw = 5;
                  r_fw = 6;
                }
              r_fmt = float_format (r_fw, ex, prec - 1, std::ios::fixed);
              i_fmt = float_format (i_fw, ex, prec - 1, std::ios::fixed);
            }
          else
            {
              i_fw = 1 + prec + ex;
              r_fw = i_fw + 1;
              if (inf_or_nan && i_fw < 3)
                {
                  i_fw = 3;
                  r_fw = 4;
                }
              r_fmt = float_format (r_fw, prec - 1, std::ios::scientific);
              i_fmt = float_format (i_fw, prec - 1, std::ios::scientific);
            }
        }

      if (print_big_e)
        {
          r_fmt.uppercase ();
          i_fmt.uppercase ();
        }
    }
  else if (! bank_format && int_or_inf_or_nan)
    {
      r_fmt = float_format (r_fw, rd);
      i_fmt = float_format (i_fw, rd);
    }
  else
    {
      r_fmt = float_format (r_fw, rd, std::ios::fixed);
      i_fmt = float_format (i_fw, rd, std::ios::fixed);
    }

  curr_real_fmt = &r_fmt;
  curr_imag_fmt = &i_fmt;
}

static void
set_format (const ComplexMatrix& cm, int& r_fw, int& i_fw, double& scale)
{
  curr_real_fmt = 0;
  curr_imag_fmt = 0;

  if (free_format)
    return;

  Matrix rp = real (cm);
  Matrix ip = imag (cm);

  bool inf_or_nan = cm.any_element_is_inf_or_nan ();

  bool int_or_inf_or_nan = (rp.all_elements_are_int_or_inf_or_nan ()
                            && ip.all_elements_are_int_or_inf_or_nan ());

  Matrix r_m_abs = rp.abs ();
  double r_max_abs = pr_max_internal (r_m_abs);
  double r_min_abs = pr_min_internal (r_m_abs);

  Matrix i_m_abs = ip.abs ();
  double i_max_abs = pr_max_internal (i_m_abs);
  double i_min_abs = pr_min_internal (i_m_abs);

  int r_x_max = r_max_abs == 0.0 ? 0 : num_digits (r_max_abs);

  int r_x_min = r_min_abs == 0.0 ? 0 : num_digits (r_min_abs);

  int i_x_max = i_max_abs == 0.0 ? 0 : num_digits (i_max_abs);

  int i_x_min = i_min_abs == 0.0 ? 0 : num_digits (i_min_abs);

  int x_max = r_x_max > i_x_max ? r_x_max : i_x_max;
  int x_min = r_x_min > i_x_min ? r_x_min : i_x_min;

  scale = (x_max == 0 || int_or_inf_or_nan)
          ? 1.0 : std::pow (10.0, calc_scale_exp (x_max - 1));

  set_complex_matrix_format (x_max, x_min, r_x_max, r_x_min, inf_or_nan,
                             int_or_inf_or_nan, r_fw, i_fw);
}

static inline void
set_format (const ComplexMatrix& cm)
{
  int r_fw, i_fw;
  double scale;
  set_format (cm, r_fw, i_fw, scale);
}

static void
set_range_format (int x_max, int x_min, int all_ints, int& fw)
{
  static float_format fmt;

  int prec = Voutput_precision;

  int ld, rd;

  if (rat_format)
    {
      fw = 9;
      rd = 0;
    }
  else if (bank_format)
    {
      int digits = x_max > x_min ? x_max : x_min;
      fw = digits < 0 ? 5 : digits + 4;
      rd = 2;
    }
  else if (hex_format)
    {
      fw = 2 * sizeof (double);
      rd = 0;
    }
  else if (bit_format)
    {
      fw = 8 * sizeof (double);
      rd = 0;
    }
  else if (all_ints)
    {
      int digits = x_max > x_min ? x_max : x_min;
      fw = digits + 1;
      rd = fw;
    }
  else if (Vfixed_point_format && ! print_g)
    {
      rd = prec;
      fw = rd + 3;
    }
  else
    {
      int ld_max, rd_max;
      if (x_max > 0)
        {
          ld_max = x_max;
          rd_max = prec > x_max ? prec - x_max : prec;
          x_max++;
        }
      else
        {
          ld_max = 1;
          rd_max = prec > x_max ? prec - x_max : prec;
          x_max = -x_max + 1;
        }

      int ld_min, rd_min;
      if (x_min > 0)
        {
          ld_min = x_min;
          rd_min = prec > x_min ? prec - x_min : prec;
          x_min++;
        }
      else
        {
          ld_min = 1;
          rd_min = prec > x_min ? prec - x_min : prec;
          x_min = -x_min + 1;
        }

      ld = ld_max > ld_min ? ld_max : ld_min;
      rd = rd_max > rd_min ? rd_max : rd_min;

      fw = ld + rd + 3;
    }

  if (! (rat_format || bank_format || hex_format || bit_format)
      && (print_e
          || print_eng || print_g
          || (! Vfixed_point_format && fw > Voutput_max_field_width)))
    {
      if (print_g)
        fmt = float_format ();
      else
        {
          int ex = 4;
          if (x_max > 100 || x_min > 100)
            ex++;

          if (print_eng)
            {
              fw = 5 + prec + ex;
              fmt = float_format (fw, ex, prec - 1, std::ios::fixed);
            }
          else
            {
              fw = 3 + prec + ex;
              fmt = float_format (fw, prec - 1, std::ios::scientific);
            }
        }

      if (print_big_e)
        fmt.uppercase ();
    }
  else if (! bank_format && all_ints)
    fmt = float_format (fw, rd);
  else
    fmt = float_format (fw, rd, std::ios::fixed);

  curr_real_fmt = &fmt;
}

static void
set_format (const Range& r, int& fw, double& scale)
{
  curr_real_fmt = 0;
  curr_imag_fmt = 0;

  if (free_format)
    return;

  double r_min = r.base ();
  double r_max = r.limit ();

  if (r_max < r_min)
    {
      double tmp = r_max;
      r_max = r_min;
      r_min = tmp;
    }

  bool all_ints = r.all_elements_are_ints ();

  double max_abs = r_max < 0.0 ? -r_max : r_max;
  double min_abs = r_min < 0.0 ? -r_min : r_min;

  int x_max = max_abs == 0.0 ? 0 : num_digits (max_abs);

  int x_min = min_abs == 0.0 ? 0 : num_digits (min_abs);

  scale = (x_max == 0 || all_ints)
          ? 1.0 : std::pow (10.0, calc_scale_exp (x_max - 1));

  set_range_format (x_max, x_min, all_ints, fw);
}

static inline void
set_format (const Range& r)
{
  int fw;
  double scale;
  set_format (r, fw, scale);
}

union equiv
{
  double d;
  unsigned char i[sizeof (double)];
};

#define PRINT_CHAR_BITS(os, c) \
  do \
    { \
      unsigned char ctmp = c; \
      char stmp[9]; \
      stmp[0] = (ctmp & 0x80) ? '1' : '0'; \
      stmp[1] = (ctmp & 0x40) ? '1' : '0'; \
      stmp[2] = (ctmp & 0x20) ? '1' : '0'; \
      stmp[3] = (ctmp & 0x10) ? '1' : '0'; \
      stmp[4] = (ctmp & 0x08) ? '1' : '0'; \
      stmp[5] = (ctmp & 0x04) ? '1' : '0'; \
      stmp[6] = (ctmp & 0x02) ? '1' : '0'; \
      stmp[7] = (ctmp & 0x01) ? '1' : '0'; \
      stmp[8] = '\0'; \
      os << stmp; \
    } \
  while (0)

#define PRINT_CHAR_BITS_SWAPPED(os, c) \
  do \
    { \
      unsigned char ctmp = c; \
      char stmp[9]; \
      stmp[0] = (ctmp & 0x01) ? '1' : '0'; \
      stmp[1] = (ctmp & 0x02) ? '1' : '0'; \
      stmp[2] = (ctmp & 0x04) ? '1' : '0'; \
      stmp[3] = (ctmp & 0x08) ? '1' : '0'; \
      stmp[4] = (ctmp & 0x10) ? '1' : '0'; \
      stmp[5] = (ctmp & 0x20) ? '1' : '0'; \
      stmp[6] = (ctmp & 0x40) ? '1' : '0'; \
      stmp[7] = (ctmp & 0x80) ? '1' : '0'; \
      stmp[8] = '\0'; \
      os << stmp; \
    } \
  while (0)

static void
pr_any_float (const float_format *fmt, std::ostream& os, double d, int fw = 0)
{
  if (fmt)
    {
      // Unless explicitly asked for, always print in big-endian format
      // for hex and bit formats.
      //
      //   {bit,hex}_format == 1: print big-endian
      //   {bit,hex}_format == 2: print native

      if (hex_format)
        {
          octave_preserve_stream_state stream_state (os);

          equiv tmp;
          tmp.d = d;

          // Unless explicitly asked for, always print in big-endian format.

          // FIXME: will bad things happen if we are
          // interrupted before resetting the format flags and fill
          // character?

          oct_mach_info::float_format flt_fmt =
            oct_mach_info::native_float_format ();

          os.fill ('0');
          os.flags (std::ios::right | std::ios::hex);

          if (hex_format > 1
              || flt_fmt == oct_mach_info::flt_fmt_ieee_big_endian)
            {
              for (size_t i = 0; i < sizeof (double); i++)
                os << std::setw (2) << static_cast<int> (tmp.i[i]);
            }
          else
            {
              for (int i = sizeof (double) - 1; i >= 0; i--)
                os << std::setw (2) << static_cast<int> (tmp.i[i]);
            }
        }
      else if (bit_format)
        {
          equiv tmp;
          tmp.d = d;

          oct_mach_info::float_format flt_fmt =
            oct_mach_info::native_float_format ();

          if (flt_fmt == oct_mach_info::flt_fmt_ieee_big_endian)
            {
              for (size_t i = 0; i < sizeof (double); i++)
                PRINT_CHAR_BITS (os, tmp.i[i]);
            }
          else
            {
              if (bit_format > 1)
                {
                  for (size_t i = 0; i < sizeof (double); i++)
                    PRINT_CHAR_BITS_SWAPPED (os, tmp.i[i]);
                }
              else
                {
                  for (int i = sizeof (double) - 1; i >= 0; i--)
                    PRINT_CHAR_BITS (os, tmp.i[i]);
                }
            }
        }
      else if (octave_is_NA (d))
        {
          octave_preserve_stream_state stream_state (os);

          if (fw > 0)
            os << std::setw (fw) << "NA";
          else
            os << "NA";
        }
      else if (rat_format)
        os << pr_rational_float (*fmt, d);
      else if (xisinf (d))
        {
          octave_preserve_stream_state stream_state (os);

          const char *s;
          if (d < 0.0)
            s = "-Inf";
          else
            s = "Inf";

          if (fw > 0)
            os << std::setw (fw) << s;
          else
            os << s;
        }
      else if (xisnan (d))
        {
          octave_preserve_stream_state stream_state (os);

          if (fw > 0)
            os << std::setw (fw) << "NaN";
          else
            os << "NaN";
        }
      else if (print_eng)
        os << pr_engineering_float (*fmt, d);
      else
        os << pr_formatted_float (*fmt, d);
    }
  else
    os << d;
}

static inline void
pr_float (std::ostream& os, double d, int fw = 0, double scale = 1.0)
{
  if (Vfixed_point_format && ! print_g && scale != 1.0)
    d /= scale;

  pr_any_float (curr_real_fmt, os, d, fw);
}

static inline void
pr_imag_float (std::ostream& os, double d, int fw = 0)
{
  pr_any_float (curr_imag_fmt, os, d, fw);
}

static void
pr_complex (std::ostream& os, const Complex& c, int r_fw = 0,
            int i_fw = 0, double scale = 1.0)
{
  Complex tmp
    = (Vfixed_point_format && ! print_g && scale != 1.0) ? c / scale : c;

  double r = tmp.real ();

  pr_float (os, r, r_fw);

  if (! bank_format)
    {
      double i = tmp.imag ();
      if (! (hex_format || bit_format) && lo_ieee_signbit (i))
        {
          os << " - ";
          i = -i;
          pr_imag_float (os, i, i_fw);
        }
      else
        {
          if (hex_format || bit_format)
            os << "  ";
          else
            os << " + ";

          pr_imag_float (os, i, i_fw);
        }
      os << "i";
    }
}

static void
print_empty_matrix (std::ostream& os, octave_idx_type nr, octave_idx_type nc,
                    bool pr_as_read_syntax)
{
  assert (nr == 0 || nc == 0);

  if (pr_as_read_syntax)
    {
      if (nr == 0 && nc == 0)
        os << "[]";
      else
        os << "zeros (" << nr << ", " << nc << ")";
    }
  else
    {
      os << "[]";

      if (Vprint_empty_dimensions)
        os << "(" << nr << "x" << nc << ")";
    }
}

static void
print_empty_nd_array (std::ostream& os, const dim_vector& dims,
                      bool pr_as_read_syntax)
{
  assert (dims.any_zero ());

  if (pr_as_read_syntax)
    os << "zeros (" << dims.str (',') << ")";
  else
    {
      os << "[]";

      if (Vprint_empty_dimensions)
        os << "(" << dims.str () << ")";
    }
}

static void
pr_scale_header (std::ostream& os, double scale)
{
  if (Vfixed_point_format && ! print_g && scale != 1.0)
    {
      octave_preserve_stream_state stream_state (os);

      os << "  "
         << std::setw (8) << std::setprecision (1)
         << std::setiosflags (std::ios::scientific|std::ios::left)
         << scale
         << " *\n";

      if (! Vcompact_format)
        os << "\n";
    }
}

static void
pr_col_num_header (std::ostream& os, octave_idx_type total_width, int max_width,
                   octave_idx_type lim, octave_idx_type col, int extra_indent)
{
  if (total_width > max_width && Vsplit_long_rows)
    {
      octave_preserve_stream_state stream_state (os);

      if (col != 0)
        {
          if (Vcompact_format)
            os << "\n";
          else
            os << "\n\n";
        }

      octave_idx_type num_cols = lim - col;

      os << std::setw (extra_indent) << "";

      if (num_cols == 1)
        os << " Column " << col + 1 << ":\n";
      else if (num_cols == 2)
        os << " Columns " << col + 1 << " and " << lim << ":\n";
      else
        os << " Columns " << col + 1 << " through " << lim << ":\n";

      if (! Vcompact_format)
        os << "\n";
    }
}

template <class T>
/* static */ inline void
pr_plus_format (std::ostream& os, const T& val)
{
  if (val > T (0))
    os << plus_format_chars[0];
  else if (val < T (0))
    os << plus_format_chars[1];
  else
    os << plus_format_chars[2];
}

void
octave_print_internal (std::ostream&, char, bool)
{
  panic_impossible ();
}

void
octave_print_internal (std::ostream& os, double d,
                       bool pr_as_read_syntax)
{
  if (pr_as_read_syntax)
    os << d;
  else if (plus_format)
    pr_plus_format (os, d);
  else
    {
      set_format (d);
      if (free_format)
        os << d;
      else
        pr_float (os, d);
    }
}

void
octave_print_internal (std::ostream& os, const Matrix& m,
                       bool pr_as_read_syntax, int extra_indent)
{
  octave_idx_type nr = m.rows ();
  octave_idx_type nc = m.columns ();

  if (nr == 0 || nc == 0)
    print_empty_matrix (os, nr, nc, pr_as_read_syntax);
  else if (plus_format && ! pr_as_read_syntax)
    {
      for (octave_idx_type i = 0; i < nr; i++)
        {
          for (octave_idx_type j = 0; j < nc; j++)
            {
              octave_quit ();

              pr_plus_format (os, m(i,j));
            }

          if (i < nr - 1)
            os << "\n";
        }
    }
  else
    {
      int fw;
      double scale = 1.0;
      set_format (m, fw, scale);
      int column_width = fw + 2;
      octave_idx_type total_width = nc * column_width;
      octave_idx_type max_width = command_editor::terminal_cols ();

      if (pr_as_read_syntax)
        max_width -= 4;
      else
        max_width -= extra_indent;

      if (max_width < 0)
        max_width = 0;

      if (free_format)
        {
          if (pr_as_read_syntax)
            os << "[\n";

          os << m;

          if (pr_as_read_syntax)
            os << "]";

          return;
        }

      octave_idx_type inc = nc;
      if (total_width > max_width && Vsplit_long_rows)
        {
          inc = max_width / column_width;
          if (inc == 0)
            inc++;
        }

      if (pr_as_read_syntax)
        {
          for (octave_idx_type i = 0; i < nr; i++)
            {
              octave_idx_type col = 0;
              while (col < nc)
                {
                  octave_idx_type lim = col + inc < nc ? col + inc : nc;

                  for (octave_idx_type j = col; j < lim; j++)
                    {
                      octave_quit ();

                      if (i == 0 && j == 0)
                        os << "[ ";
                      else
                        {
                          if (j > col && j < lim)
                            os << ", ";
                          else
                            os << "  ";
                        }

                      pr_float (os, m(i,j));
                    }

                  col += inc;

                  if (col >= nc)
                    {
                      if (i == nr - 1)
                        os << " ]";
                      else
                        os << ";\n";
                    }
                  else
                    os << " ...\n";
                }
            }
        }
      else
        {
          octave_preserve_stream_state stream_state (os);

          pr_scale_header (os, scale);

          for (octave_idx_type col = 0; col < nc; col += inc)
            {
              octave_idx_type lim = col + inc < nc ? col + inc : nc;

              pr_col_num_header (os, total_width, max_width, lim, col,
                                 extra_indent);

              for (octave_idx_type i = 0; i < nr; i++)
                {
                  os << std::setw (extra_indent) << "";

                  for (octave_idx_type j = col; j < lim; j++)
                    {
                      octave_quit ();

                      os << "  ";

                      pr_float (os, m(i,j), fw, scale);
                    }

                  if (i < nr - 1)
                    os << "\n";
                }
            }
        }
    }
}

void
octave_print_internal (std::ostream& os, const DiagMatrix& m,
                       bool pr_as_read_syntax, int extra_indent)
{
  octave_idx_type nr = m.rows ();
  octave_idx_type nc = m.columns ();

  if (nr == 0 || nc == 0)
    print_empty_matrix (os, nr, nc, pr_as_read_syntax);
  else if (plus_format && ! pr_as_read_syntax)
    {
      for (octave_idx_type i = 0; i < nr; i++)
        {
          for (octave_idx_type j = 0; j < nc; j++)
            {
              octave_quit ();

              pr_plus_format (os, m(i,j));
            }

          if (i < nr - 1)
            os << "\n";
        }
    }
  else
    {
      int fw;
      double scale = 1.0;
      set_format (Matrix (m.diag ()), fw, scale);
      int column_width = fw + 2;
      octave_idx_type total_width = nc * column_width;
      octave_idx_type max_width = command_editor::terminal_cols ();

      if (pr_as_read_syntax)
        max_width -= 4;
      else
        max_width -= extra_indent;

      if (max_width < 0)
        max_width = 0;

      if (free_format)
        {
          if (pr_as_read_syntax)
            os << "[\n";

          os << Matrix (m);

          if (pr_as_read_syntax)
            os << "]";

          return;
        }

      octave_idx_type inc = nc;
      if (total_width > max_width && Vsplit_long_rows)
        {
          inc = max_width / column_width;
          if (inc == 0)
            inc++;
        }

      if (pr_as_read_syntax)
        {
          os << "diag (";

          octave_idx_type col = 0;
          while (col < nc)
            {
              octave_idx_type lim = col + inc < nc ? col + inc : nc;

              for (octave_idx_type j = col; j < lim; j++)
                {
                  octave_quit ();

                  if (j == 0)
                    os << "[ ";
                  else
                    {
                      if (j > col && j < lim)
                        os << ", ";
                      else
                        os << "  ";
                    }

                  pr_float (os, m(j,j));
                }

              col += inc;

              if (col >= nc)
                os << " ]";
              else
                os << " ...\n";
            }
          os << ")";
        }
      else
        {
          octave_preserve_stream_state stream_state (os);

          os << "Diagonal Matrix\n";
          if (! Vcompact_format)
            os << "\n";

          pr_scale_header (os, scale);

          // kluge. Get the true width of a number.
          int zero_fw;

          {
            std::ostringstream tmp_oss;
            pr_float (tmp_oss, 0.0, fw, scale);
            zero_fw = tmp_oss.str ().length ();
          }

          for (octave_idx_type col = 0; col < nc; col += inc)
            {
              octave_idx_type lim = col + inc < nc ? col + inc : nc;

              pr_col_num_header (os, total_width, max_width, lim, col,
                                 extra_indent);

              for (octave_idx_type i = 0; i < nr; i++)
                {
                  os << std::setw (extra_indent) << "";

                  for (octave_idx_type j = col; j < lim; j++)
                    {
                      octave_quit ();

                      os << "  ";

                      if (i == j)
                        pr_float (os, m(i,j), fw, scale);
                      else
                        os << std::setw (zero_fw) << '0';

                    }

                  if (i < nr - 1)
                    os << "\n";
                }
            }
        }
    }
}

template <typename NDA_T, typename ELT_T, typename MAT_T>
void print_nd_array (std::ostream& os, const NDA_T& nda,
                     bool pr_as_read_syntax)
{

  if (nda.is_empty ())
    print_empty_nd_array (os, nda.dims (), pr_as_read_syntax);
  else
    {

      int ndims = nda.ndims ();

      dim_vector dims = nda.dims ();

      Array<octave_idx_type> ra_idx (dim_vector (ndims, 1), 0);

      octave_idx_type m = 1;

      for (int i = 2; i < ndims; i++)
        m *= dims(i);

      octave_idx_type nr = dims(0);
      octave_idx_type nc = dims(1);

      for (octave_idx_type i = 0; i < m; i++)
        {
          octave_quit ();

          std::string nm = "ans";

          if (m > 1)
            {
              nm += "(:,:,";

              std::ostringstream buf;

              for (int k = 2; k < ndims; k++)
                {
                  buf << ra_idx(k) + 1;

                  if (k < ndims - 1)
                    buf << ",";
                  else
                    buf << ")";
                }

              nm += buf.str ();
            }

          Array<idx_vector> idx (dim_vector (ndims, 1));

          idx(0) = idx_vector (':');
          idx(1) = idx_vector (':');

          for (int k = 2; k < ndims; k++)
            idx(k) = idx_vector (ra_idx(k));

          octave_value page
            = MAT_T (Array<ELT_T> (nda.index (idx), dim_vector (nr, nc)));

          if (i != m - 1)
            {
              page.print_with_name (os, nm);
            }
          else
            {
              page.print_name_tag (os, nm);
              page.print_raw (os);
            }

          if (i < m)
            NDA_T::increment_index (ra_idx, dims, 2);
        }
    }
}

void
octave_print_internal (std::ostream& os, const NDArray& nda,
                       bool pr_as_read_syntax, int extra_indent)
{
  switch (nda.ndims ())
    {
    case 1:
    case 2:
      octave_print_internal (os, Matrix (nda),
                             pr_as_read_syntax, extra_indent);
      break;

    default:
      print_nd_array <NDArray, double, Matrix> (os, nda, pr_as_read_syntax);
      break;
    }
}

template <>
/* static */ inline void
pr_plus_format<> (std::ostream& os, const Complex& c)
{
  double rp = c.real ();
  double ip = c.imag ();

  if (rp == 0.0)
    {
      if (ip == 0.0)
        os << " ";
      else
        os << "i";
    }
  else if (ip == 0.0)
    pr_plus_format (os, rp);
  else
    os << "c";
}

void
octave_print_internal (std::ostream& os, const Complex& c,
                       bool pr_as_read_syntax)
{
  if (pr_as_read_syntax)
    os << c;
  else if (plus_format)
    pr_plus_format (os, c);
  else
    {
      set_format (c);
      if (free_format)
        os << c;
      else
        pr_complex (os, c);
    }
}

void
octave_print_internal (std::ostream& os, const ComplexMatrix& cm,
                       bool pr_as_read_syntax, int extra_indent)
{
  octave_idx_type nr = cm.rows ();
  octave_idx_type nc = cm.columns ();

  if (nr == 0 || nc == 0)
    print_empty_matrix (os, nr, nc, pr_as_read_syntax);
  else if (plus_format && ! pr_as_read_syntax)
    {
      for (octave_idx_type i = 0; i < nr; i++)
        {
          for (octave_idx_type j = 0; j < nc; j++)
            {
              octave_quit ();

              pr_plus_format (os, cm(i,j));
            }

          if (i < nr - 1)
            os << "\n";
        }
    }
  else
    {
      int r_fw, i_fw;
      double scale = 1.0;
      set_format (cm, r_fw, i_fw, scale);
      int column_width = i_fw + r_fw;
      column_width += (rat_format || bank_format || hex_format
                       || bit_format) ? 2 : 7;
      octave_idx_type total_width = nc * column_width;
      octave_idx_type max_width = command_editor::terminal_cols ();

      if (pr_as_read_syntax)
        max_width -= 4;
      else
        max_width -= extra_indent;

      if (max_width < 0)
        max_width = 0;

      if (free_format)
        {
          if (pr_as_read_syntax)
            os << "[\n";

          os << cm;

          if (pr_as_read_syntax)
            os << "]";

          return;
        }

      octave_idx_type inc = nc;
      if (total_width > max_width && Vsplit_long_rows)
        {
          inc = max_width / column_width;
          if (inc == 0)
            inc++;
        }

      if (pr_as_read_syntax)
        {
          for (octave_idx_type i = 0; i < nr; i++)
            {
              octave_idx_type col = 0;
              while (col < nc)
                {
                  octave_idx_type lim = col + inc < nc ? col + inc : nc;

                  for (octave_idx_type j = col; j < lim; j++)
                    {
                      octave_quit ();

                      if (i == 0 && j == 0)
                        os << "[ ";
                      else
                        {
                          if (j > col && j < lim)
                            os << ", ";
                          else
                            os << "  ";
                        }

                      pr_complex (os, cm(i,j));
                    }

                  col += inc;

                  if (col >= nc)
                    {
                      if (i == nr - 1)
                        os << " ]";
                      else
                        os << ";\n";
                    }
                  else
                    os << " ...\n";
                }
            }
        }
      else
        {
          octave_preserve_stream_state stream_state (os);

          pr_scale_header (os, scale);

          for (octave_idx_type col = 0; col < nc; col += inc)
            {
              octave_idx_type lim = col + inc < nc ? col + inc : nc;

              pr_col_num_header (os, total_width, max_width, lim, col,
                                 extra_indent);

              for (octave_idx_type i = 0; i < nr; i++)
                {
                  os << std::setw (extra_indent) << "";

                  for (octave_idx_type j = col; j < lim; j++)
                    {
                      octave_quit ();

                      os << "  ";

                      pr_complex (os, cm(i,j), r_fw, i_fw, scale);
                    }

                  if (i < nr - 1)
                    os << "\n";
                }
            }
        }
    }
}

void
octave_print_internal (std::ostream& os, const ComplexDiagMatrix& cm,
                       bool pr_as_read_syntax, int extra_indent)
{
  octave_idx_type nr = cm.rows ();
  octave_idx_type nc = cm.columns ();

  if (nr == 0 || nc == 0)
    print_empty_matrix (os, nr, nc, pr_as_read_syntax);
  else if (plus_format && ! pr_as_read_syntax)
    {
      for (octave_idx_type i = 0; i < nr; i++)
        {
          for (octave_idx_type j = 0; j < nc; j++)
            {
              octave_quit ();

              pr_plus_format (os, cm(i,j));
            }

          if (i < nr - 1)
            os << "\n";
        }
    }
  else
    {
      int r_fw, i_fw;
      double scale = 1.0;
      set_format (ComplexMatrix (cm.diag ()), r_fw, i_fw, scale);
      int column_width = i_fw + r_fw;
      column_width += (rat_format || bank_format || hex_format
                       || bit_format) ? 2 : 7;
      octave_idx_type total_width = nc * column_width;
      octave_idx_type max_width = command_editor::terminal_cols ();

      if (pr_as_read_syntax)
        max_width -= 4;
      else
        max_width -= extra_indent;

      if (max_width < 0)
        max_width = 0;

      if (free_format)
        {
          if (pr_as_read_syntax)
            os << "[\n";

          os << ComplexMatrix (cm);

          if (pr_as_read_syntax)
            os << "]";

          return;
        }

      octave_idx_type inc = nc;
      if (total_width > max_width && Vsplit_long_rows)
        {
          inc = max_width / column_width;
          if (inc == 0)
            inc++;
        }

      if (pr_as_read_syntax)
        {
          os << "diag (";

          octave_idx_type col = 0;
          while (col < nc)
            {
              octave_idx_type lim = col + inc < nc ? col + inc : nc;

              for (octave_idx_type j = col; j < lim; j++)
                {
                  octave_quit ();

                  if (j == 0)
                    os << "[ ";
                  else
                    {
                      if (j > col && j < lim)
                        os << ", ";
                      else
                        os << "  ";
                    }

                  pr_complex (os, cm(j,j));
                }

              col += inc;

              if (col >= nc)
                os << " ]";
              else
                os << " ...\n";
            }
          os << ")";
        }
      else
        {
          octave_preserve_stream_state stream_state (os);

          os << "Diagonal Matrix\n";
          if (! Vcompact_format)
            os << "\n";

          pr_scale_header (os, scale);

          // kluge. Get the true width of a number.
          int zero_fw;

          {
            std::ostringstream tmp_oss;
            pr_complex (tmp_oss, Complex (0.0), r_fw, i_fw, scale);
            zero_fw = tmp_oss.str ().length ();
          }

          for (octave_idx_type col = 0; col < nc; col += inc)
            {
              octave_idx_type lim = col + inc < nc ? col + inc : nc;

              pr_col_num_header (os, total_width, max_width, lim, col,
                                 extra_indent);

              for (octave_idx_type i = 0; i < nr; i++)
                {
                  os << std::setw (extra_indent) << "";

                  for (octave_idx_type j = col; j < lim; j++)
                    {
                      octave_quit ();

                      os << "  ";

                      if (i == j)
                        pr_complex (os, cm(i,j), r_fw, i_fw, scale);
                      else
                        os << std::setw (zero_fw) << '0';
                    }

                  if (i < nr - 1)
                    os << "\n";
                }
            }
        }
    }
}

void
octave_print_internal (std::ostream& os, const PermMatrix& m,
                       bool pr_as_read_syntax, int extra_indent)
{
  octave_idx_type nr = m.rows ();
  octave_idx_type nc = m.columns ();

  if (nr == 0 || nc == 0)
    print_empty_matrix (os, nr, nc, pr_as_read_syntax);
  else if (plus_format && ! pr_as_read_syntax)
    {
      for (octave_idx_type i = 0; i < nr; i++)
        {
          for (octave_idx_type j = 0; j < nc; j++)
            {
              octave_quit ();

              pr_plus_format (os, m(i,j));
            }

          if (i < nr - 1)
            os << "\n";
        }
    }
  else
    {
      int fw = 2;
      int column_width = fw + 2;
      octave_idx_type total_width = nc * column_width;
      octave_idx_type max_width = command_editor::terminal_cols ();

      if (pr_as_read_syntax)
        max_width -= 4;
      else
        max_width -= extra_indent;

      if (max_width < 0)
        max_width = 0;

      if (free_format)
        {
          if (pr_as_read_syntax)
            os << "[\n";

          os << Matrix (m);

          if (pr_as_read_syntax)
            os << "]";

          return;
        }

      octave_idx_type inc = nc;
      if (total_width > max_width && Vsplit_long_rows)
        {
          inc = max_width / column_width;
          if (inc == 0)
            inc++;
        }

      if (pr_as_read_syntax)
        {
          Array<octave_idx_type> pvec = m.col_perm_vec ();

          os << "eye (";
          os << ":, ";

          octave_idx_type col = 0;
          while (col < nc)
            {
              octave_idx_type lim = col + inc < nc ? col + inc : nc;

              for (octave_idx_type j = col; j < lim; j++)
                {
                  octave_quit ();

                  if (j == 0)
                    os << "[ ";
                  else
                    {
                      if (j > col && j < lim)
                        os << ", ";
                      else
                        os << "  ";
                    }

                  os << pvec (j);
                }

              col += inc;

              if (col >= nc)
                os << " ]";
              else
                os << " ...\n";
            }
          os << ")";
        }
      else
        {
          octave_preserve_stream_state stream_state (os);

          os << "Permutation Matrix\n";
          if (! Vcompact_format)
            os << "\n";

          for (octave_idx_type col = 0; col < nc; col += inc)
            {
              octave_idx_type lim = col + inc < nc ? col + inc : nc;

              pr_col_num_header (os, total_width, max_width, lim, col,
                                 extra_indent);

              for (octave_idx_type i = 0; i < nr; i++)
                {
                  os << std::setw (extra_indent) << "";

                  for (octave_idx_type j = col; j < lim; j++)
                    {
                      octave_quit ();

                      os << "  ";

                      os << std::setw (fw) << m(i,j);
                    }

                  if (i < nr - 1)
                    os << "\n";
                }
            }
        }
    }
}

void
octave_print_internal (std::ostream& os, const ComplexNDArray& nda,
                       bool pr_as_read_syntax, int extra_indent)
{
  switch (nda.ndims ())
    {
    case 1:
    case 2:
      octave_print_internal (os, ComplexMatrix (nda),
                             pr_as_read_syntax, extra_indent);
      break;

    default:
      print_nd_array <ComplexNDArray, Complex, ComplexMatrix>
                     (os, nda, pr_as_read_syntax);
      break;
    }
}

void
octave_print_internal (std::ostream& os, bool d, bool pr_as_read_syntax)
{
  octave_print_internal (os, double (d), pr_as_read_syntax);
}

// FIXME: write single precision versions of the printing functions.

void
octave_print_internal (std::ostream& os, float d, bool pr_as_read_syntax)
{
  octave_print_internal (os, double (d), pr_as_read_syntax);
}

void
octave_print_internal (std::ostream& os, const FloatMatrix& m,
                       bool pr_as_read_syntax, int extra_indent)
{
  octave_print_internal (os, Matrix (m), pr_as_read_syntax, extra_indent);
}

void
octave_print_internal (std::ostream& os, const FloatDiagMatrix& m,
                       bool pr_as_read_syntax, int extra_indent)
{
  octave_print_internal (os, DiagMatrix (m), pr_as_read_syntax, extra_indent);
}

void
octave_print_internal (std::ostream& os, const FloatNDArray& nda,
                       bool pr_as_read_syntax, int extra_indent)
{
  octave_print_internal (os, NDArray (nda), pr_as_read_syntax, extra_indent);
}

void
octave_print_internal (std::ostream& os, const FloatComplex& c,
                       bool pr_as_read_syntax)
{
  octave_print_internal (os, Complex (c), pr_as_read_syntax);
}

void
octave_print_internal (std::ostream& os, const FloatComplexMatrix& cm,
                       bool pr_as_read_syntax, int extra_indent)
{
  octave_print_internal (os, ComplexMatrix (cm), pr_as_read_syntax,
                         extra_indent);
}

void
octave_print_internal (std::ostream& os, const FloatComplexDiagMatrix& cm,
                       bool pr_as_read_syntax, int extra_indent)
{
  octave_print_internal (os, ComplexDiagMatrix (cm), pr_as_read_syntax,
                         extra_indent);
}

void
octave_print_internal (std::ostream& os, const FloatComplexNDArray& nda,
                       bool pr_as_read_syntax, int extra_indent)
{
  octave_print_internal (os, ComplexNDArray (nda), pr_as_read_syntax,
                         extra_indent);
}

void
octave_print_internal (std::ostream& os, const Range& r,
                       bool pr_as_read_syntax, int extra_indent)
{
  double base = r.base ();
  double increment = r.inc ();
  double limit = r.limit ();
  octave_idx_type num_elem = r.nelem ();

  if (plus_format && ! pr_as_read_syntax)
    {
      for (octave_idx_type i = 0; i < num_elem; i++)
        {
          octave_quit ();

          double val = base + i * increment;

          pr_plus_format (os, val);
        }
    }
  else
    {
      int fw = 0;
      double scale = 1.0;
      set_format (r, fw, scale);

      if (pr_as_read_syntax)
        {
          if (free_format)
            {
              os << base << " : ";
              if (increment != 1.0)
                os << increment << " : ";
              os << limit;
            }
          else
            {
              pr_float (os, base, fw);
              os << " : ";
              if (increment != 1.0)
                {
                  pr_float (os, increment, fw);
                  os << " : ";
                }
              pr_float (os, limit, fw);
            }
        }
      else
        {
          octave_preserve_stream_state stream_state (os);

          int column_width = fw + 2;
          octave_idx_type total_width = num_elem * column_width;
          octave_idx_type max_width = command_editor::terminal_cols ();

          if (free_format)
            {
              os << r;
              return;
            }

          octave_idx_type inc = num_elem;
          if (total_width > max_width && Vsplit_long_rows)
            {
              inc = max_width / column_width;
              if (inc == 0)
                inc++;
            }

          max_width -= extra_indent;

          if (max_width < 0)
            max_width = 0;

          pr_scale_header (os, scale);

          octave_idx_type col = 0;
          while (col < num_elem)
            {
              octave_idx_type lim = col + inc < num_elem ? col + inc : num_elem;

              pr_col_num_header (os, total_width, max_width, lim, col,
                                 extra_indent);

              os << std::setw (extra_indent) << "";

              for (octave_idx_type i = col; i < lim; i++)
                {
                  octave_quit ();

                  double val;
                  if (i == 0)
                    val = base;
                  else
                    val = base + i * increment;

                  if (i == num_elem - 1)
                    {
                      // See the comments in Range::matrix_value.
                      if ((increment > 0 && val >= limit)
                          || (increment < 0 && val <= limit))
                        val = limit;
                    }

                  os << "  ";

                  pr_float (os, val, fw, scale);
                }

              col += inc;
            }
        }
    }
}

void
octave_print_internal (std::ostream& os, const boolMatrix& bm,
                       bool pr_as_read_syntax,
                       int extra_indent)
{
  Matrix tmp (bm);
  octave_print_internal (os, tmp, pr_as_read_syntax, extra_indent);
}

void
octave_print_internal (std::ostream& os, const boolNDArray& nda,
                       bool pr_as_read_syntax,
                       int extra_indent)
{
  switch (nda.ndims ())
    {
    case 1:
    case 2:
      octave_print_internal (os, boolMatrix (nda),
                             pr_as_read_syntax, extra_indent);
      break;

    default:
      print_nd_array<boolNDArray, bool,
                     boolMatrix> (os, nda, pr_as_read_syntax);
      break;
    }
}

void
octave_print_internal (std::ostream& os, const charMatrix& chm,
                       bool pr_as_read_syntax,
                       int /* extra_indent FIXME */,
                       bool pr_as_string)
{
  if (pr_as_string)
    {
      octave_idx_type nstr = chm.rows ();

      if (pr_as_read_syntax && nstr > 1)
        os << "[ ";

      if (nstr != 0)
        {
          for (octave_idx_type i = 0; i < nstr; i++)
            {
              octave_quit ();

              std::string row = chm.row_as_string (i);

              if (pr_as_read_syntax)
                {
                  os << "\"" << undo_string_escapes (row) << "\"";

                  if (i < nstr - 1)
                    os << "; ";
                }
              else
                {
                  os << row;

                  if (i < nstr - 1)
                    os << "\n";
                }
            }
        }

      if (pr_as_read_syntax && nstr > 1)
        os << " ]";
    }
  else
    {
      os << "sorry, printing char matrices not implemented yet\n";
    }
}

void
octave_print_internal (std::ostream& os, const charNDArray& nda,
                       bool pr_as_read_syntax, int extra_indent,
                       bool pr_as_string)
{
  switch (nda.ndims ())
    {
    case 1:
    case 2:
      octave_print_internal (os, charMatrix (nda),
                             pr_as_read_syntax, extra_indent, pr_as_string);
      break;

    default:
      print_nd_array <charNDArray, char, charMatrix> (os, nda,
                                                      pr_as_read_syntax);
      break;
    }
}

void
octave_print_internal (std::ostream& os, const std::string& s,
                       bool pr_as_read_syntax, int extra_indent)
{
  Array<std::string> nda (dim_vector (1, 1), s);

  octave_print_internal (os, nda, pr_as_read_syntax, extra_indent);
}

void
octave_print_internal (std::ostream& os, const Array<std::string>& nda,
                       bool pr_as_read_syntax, int /* extra_indent */)
{
  // FIXME: this mostly duplicates the code in the print_nd_array<>
  // function. Can fix this with std::is_same from C++11.

  if (nda.is_empty ())
    print_empty_nd_array (os, nda.dims (), pr_as_read_syntax);
  else if (nda.length () == 1)
    {
      os << nda(0);
    }
  else
    {
      int ndims = nda.ndims ();

      dim_vector dims = nda.dims ();

      Array<octave_idx_type> ra_idx (dim_vector (ndims, 1), 0);

      octave_idx_type m = 1;

      for (int i = 2; i < ndims; i++)
        m *= dims(i);

      octave_idx_type nr = dims(0);
      octave_idx_type nc = dims(1);

      for (octave_idx_type i = 0; i < m; i++)
        {
          std::string nm = "ans";

          if (m > 1)
            {
              nm += "(:,:,";

              std::ostringstream buf;

              for (int k = 2; k < ndims; k++)
                {
                  buf << ra_idx(k) + 1;

                  if (k < ndims - 1)
                    buf << ",";
                  else
                    buf << ")";
                }

              nm += buf.str ();
            }

          Array<idx_vector> idx (dim_vector (ndims, 1));

          idx(0) = idx_vector (':');
          idx(1) = idx_vector (':');

          for (int k = 2; k < ndims; k++)
            idx(k) = idx_vector (ra_idx(k));

          Array<std::string> page (nda.index (idx), dim_vector (nr, nc));

          // FIXME: need to do some more work to put these
          //        in neatly aligned columns...

          octave_idx_type n_rows = page.rows ();
          octave_idx_type n_cols = page.cols ();

          os << nm << " =\n";
          if (! Vcompact_format)
            os << "\n";

          for (octave_idx_type ii = 0; ii < n_rows; ii++)
            {
              for (octave_idx_type jj = 0; jj < n_cols; jj++)
                os << "  " << page(ii,jj);

              os << "\n";
            }

          if (i < m - 1)
            os << "\n";

          if (i < m)
            increment_index (ra_idx, dims, 2);
        }
    }
}

template <class T>
class
octave_print_conv
{
public:
  typedef T print_conv_type;
};

#define PRINT_CONV(T1, T2) \
  template <> \
  class \
  octave_print_conv<T1> \
  { \
  public: \
    typedef T2 print_conv_type; \
  }

PRINT_CONV (octave_int8, octave_int16);
PRINT_CONV (octave_uint8, octave_uint16);

#undef PRINT_CONV

template <class T>
/* static */ inline void
pr_int (std::ostream& os, const T& d, int fw = 0)
{
  size_t sz = d.byte_size ();
  const unsigned char * tmpi = d.iptr ();

  // Unless explicitly asked for, always print in big-endian
  // format for hex and bit formats.
  //
  //   {bit,hex}_format == 1: print big-endian
  //   {bit,hex}_format == 2: print native

  if (hex_format)
    {
      octave_preserve_stream_state stream_state (os);

      os.flags (std::ios::right | std::ios::hex);

      if (hex_format > 1 || oct_mach_info::words_big_endian ())
        {
          for (size_t i = 0; i < sz; i++)
            os << std::setw (2) << static_cast<int> (tmpi[i]);
        }
      else
        {
          for (int i = sz - 1; i >= 0; i--)
            os << std::setw (2) << static_cast<int> (tmpi[i]);
        }
    }
  else if (bit_format)
    {
      if (oct_mach_info::words_big_endian ())
        {
          for (size_t i = 0; i < sz; i++)
            PRINT_CHAR_BITS (os, tmpi[i]);
        }
      else
        {
          if (bit_format > 1)
            {
              for (size_t i = 0; i < sz; i++)
                PRINT_CHAR_BITS_SWAPPED (os, tmpi[i]);
            }
          else
            {
              for (int i = sz - 1; i >= 0; i--)
                PRINT_CHAR_BITS (os, tmpi[i]);
            }
        }
    }
  else
    {
      octave_preserve_stream_state stream_state (os);

      os << std::setw (fw)
         << typename octave_print_conv<T>::print_conv_type (d);

      if (bank_format)
        os << ".00";
    }
}

// FIXME: all this mess with abs is an attempt to avoid seeing
//
//   warning: comparison of unsigned expression < 0 is always false
//
// from GCC.  Isn't there a better way?

template <class T>
/* static */ inline T
abs (T x)
{
  return x < 0 ? -x : x;
}

#define INSTANTIATE_ABS(T) \
  template /* static */ T abs (T)

INSTANTIATE_ABS(signed char);
INSTANTIATE_ABS(short);
INSTANTIATE_ABS(int);
INSTANTIATE_ABS(long);
INSTANTIATE_ABS(long long);

#define SPECIALIZE_UABS(T) \
  template <> \
  /* static */ inline unsigned T \
  abs (unsigned T x) \
  { \
    return x; \
  }

SPECIALIZE_UABS(char)
SPECIALIZE_UABS(short)
SPECIALIZE_UABS(int)
SPECIALIZE_UABS(long)
SPECIALIZE_UABS(long long)

template void
pr_int (std::ostream&, const octave_int8&, int);

template void
pr_int (std::ostream&, const octave_int16&, int);

template void
pr_int (std::ostream&, const octave_int32&, int);

template void
pr_int (std::ostream&, const octave_int64&, int);

template void
pr_int (std::ostream&, const octave_uint8&, int);

template void
pr_int (std::ostream&, const octave_uint16&, int);

template void
pr_int (std::ostream&, const octave_uint32&, int);

template void
pr_int (std::ostream&, const octave_uint64&, int);

template <class T>
void
octave_print_internal_template (std::ostream& os, const octave_int<T>& val,
                                bool)
{
  if (plus_format)
    {
      pr_plus_format (os, val);
    }
  else
    {
      if (free_format)
        os << typename octave_print_conv<octave_int<T> >::print_conv_type (val);
      else
        pr_int (os, val);
    }
}

#define PRINT_INT_SCALAR_INTERNAL(TYPE) \
  OCTINTERP_API void \
  octave_print_internal (std::ostream& os, const octave_int<TYPE>& val, bool dummy) \
  { \
    octave_print_internal_template (os, val, dummy); \
  }

PRINT_INT_SCALAR_INTERNAL (int8_t)
PRINT_INT_SCALAR_INTERNAL (uint8_t)
PRINT_INT_SCALAR_INTERNAL (int16_t)
PRINT_INT_SCALAR_INTERNAL (uint16_t)
PRINT_INT_SCALAR_INTERNAL (int32_t)
PRINT_INT_SCALAR_INTERNAL (uint32_t)
PRINT_INT_SCALAR_INTERNAL (int64_t)
PRINT_INT_SCALAR_INTERNAL (uint64_t)

template <class T>
/* static */ inline void
octave_print_internal_template (std::ostream& os, const intNDArray<T>& nda,
                                bool pr_as_read_syntax, int extra_indent)
{
  // FIXME: this mostly duplicates the code in the print_nd_array<>
  // function. Can fix this with std::is_same from C++11.

  if (nda.is_empty ())
    print_empty_nd_array (os, nda.dims (), pr_as_read_syntax);
  else if (nda.length () == 1)
    octave_print_internal_template (os, nda(0), pr_as_read_syntax);
  else if (plus_format && ! pr_as_read_syntax)
    {
      int ndims = nda.ndims ();

      Array<octave_idx_type> ra_idx (dim_vector (ndims, 1), 0);

      dim_vector dims = nda.dims ();

      octave_idx_type m = 1;

      for (int i = 2; i < ndims; i++)
        m *= dims(i);

      octave_idx_type nr = dims(0);
      octave_idx_type nc = dims(1);

      for (octave_idx_type i = 0; i < m; i++)
        {
          if (m > 1)
            {
              std::string nm = "ans(:,:,";

              std::ostringstream buf;

              for (int k = 2; k < ndims; k++)
                {
                  buf << ra_idx(k) + 1;

                  if (k < ndims - 1)
                    buf << ",";
                  else
                    buf << ")";
                }

              nm += buf.str ();

              os << nm << " =\n";
              if (! Vcompact_format)
                os << "\n";
            }

          Array<idx_vector> idx (dim_vector (ndims, 1));

          idx(0) = idx_vector (':');
          idx(1) = idx_vector (':');

          for (int k = 2; k < ndims; k++)
            idx(k) = idx_vector (ra_idx(k));

          Array<T> page (nda.index (idx), dim_vector (nr, nc));

          for (octave_idx_type ii = 0; ii < nr; ii++)
            {
              for (octave_idx_type jj = 0; jj < nc; jj++)
                {
                  octave_quit ();

                  pr_plus_format (os, page(ii,jj));
                }

              if ((ii < nr - 1) || (i < m -1))
                os << "\n";
            }

          if (i < m - 1)
            {
              os << "\n";
              increment_index (ra_idx, dims, 2);
            }
        }
    }
  else
    {
      int ndims = nda.ndims ();

      dim_vector dims = nda.dims ();

      Array<octave_idx_type> ra_idx (dim_vector (ndims, 1), 0);

      octave_idx_type m = 1;

      for (int i = 2; i < ndims; i++)
        m *= dims(i);

      octave_idx_type nr = dims(0);
      octave_idx_type nc = dims(1);

      int fw = 0;
      if (hex_format)
        fw = 2 * nda(0).byte_size ();
      else if (bit_format)
        fw = nda(0).nbits ();
      else
        {
          bool isneg = false;
          int digits = 0;

          for (octave_idx_type i = 0; i < dims.numel (); i++)
            {
              int new_digits
                = static_cast<int>
                  (gnulib::floor (log10 (double (abs (nda(i).value ()))) + 1.0));

              if (new_digits > digits)
                digits = new_digits;

              if (! isneg)
                isneg = (abs (nda(i).value ()) != nda(i).value ());
            }

          fw = digits + isneg;
        }

      int column_width = fw + (rat_format ?  0 : (bank_format ? 5 : 2));
      octave_idx_type total_width = nc * column_width;
      int max_width = command_editor::terminal_cols () - extra_indent;
      octave_idx_type inc = nc;
      if (total_width > max_width && Vsplit_long_rows)
        {
          inc = max_width / column_width;
          if (inc == 0)
            inc++;
        }

      for (octave_idx_type i = 0; i < m; i++)
        {
          if (m > 1)
            {
              std::string nm = "ans(:,:,";

              std::ostringstream buf;

              for (int k = 2; k < ndims; k++)
                {
                  buf << ra_idx(k) + 1;

                  if (k < ndims - 1)
                    buf << ",";
                  else
                    buf << ")";
                }

              nm += buf.str ();

              os << nm << " =\n";
              if (! Vcompact_format)
                os << "\n";
            }

          Array<idx_vector> idx (dim_vector (ndims, 1));

          idx(0) = idx_vector (':');
          idx(1) = idx_vector (':');

          for (int k = 2; k < ndims; k++)
            idx(k) = idx_vector (ra_idx(k));

          Array<T> page (nda.index (idx), dim_vector (nr, nc));

          if (free_format)
            {
              if (pr_as_read_syntax)
                os << "[\n";

              for (octave_idx_type ii = 0; ii < nr; ii++)
                {
                  for (octave_idx_type jj = 0; jj < nc; jj++)
                    {
                      octave_quit ();
                      os << "  ";
                      os << typename octave_print_conv<T>::print_conv_type (page(ii,jj));
                    }
                  os << "\n";
                }

              if (pr_as_read_syntax)
                os << "]";
            }
          else
            {
              octave_preserve_stream_state stream_state (os);

              octave_idx_type n_rows = page.rows ();
              octave_idx_type n_cols = page.cols ();

              for (octave_idx_type col = 0; col < n_cols; col += inc)
                {
                  octave_idx_type lim = col + inc < n_cols ? col + inc : n_cols;

                  pr_col_num_header (os, total_width, max_width, lim, col,
                                     extra_indent);

                  for (octave_idx_type ii = 0; ii < n_rows; ii++)
                    {
                      os << std::setw (extra_indent) << "";

                      for (octave_idx_type jj = col; jj < lim; jj++)
                        {
                          octave_quit ();
                          os << "  ";
                          pr_int (os, page(ii,jj), fw);
                        }
                      if ((ii < n_rows - 1) || (i < m -1))
                        os << "\n";
                    }
                }
            }

          if (i < m - 1)
            {
              os << "\n";
              increment_index (ra_idx, dims, 2);
            }
        }
    }
}

#define PRINT_INT_ARRAY_INTERNAL(TYPE) \
  OCTINTERP_API void \
  octave_print_internal (std::ostream& os, const intNDArray<TYPE>& nda, \
                         bool pr_as_read_syntax, int extra_indent) \
  { \
    octave_print_internal_template (os, nda, pr_as_read_syntax, extra_indent); \
  }

PRINT_INT_ARRAY_INTERNAL (octave_int8)
PRINT_INT_ARRAY_INTERNAL (octave_uint8)
PRINT_INT_ARRAY_INTERNAL (octave_int16)
PRINT_INT_ARRAY_INTERNAL (octave_uint16)
PRINT_INT_ARRAY_INTERNAL (octave_int32)
PRINT_INT_ARRAY_INTERNAL (octave_uint32)
PRINT_INT_ARRAY_INTERNAL (octave_int64)
PRINT_INT_ARRAY_INTERNAL (octave_uint64)

void
octave_print_internal (std::ostream&, const Cell&, bool, int, bool)
{
  panic_impossible ();
}

void
octave_print_internal (std::ostream&, const octave_value&, bool)
{
  panic_impossible ();
}

DEFUN (rats, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} rats (@var{x}, @var{len})\n\
Convert @var{x} into a rational approximation represented as a string.\n\
\n\
The string can be converted back into a matrix as follows:\n\
\n\
@example\n\
@group\n\
r = rats (hilb (4));\n\
x = str2num (r)\n\
@end group\n\
@end example\n\
\n\
The optional second argument defines the maximum length of the string\n\
representing the elements of @var{x}.  By default @var{len} is 9.\n\
\n\
If the length of the smallest possible rational approximation exceeds\n\
@var{len}, an asterisk (*) padded with spaces will be returned instead.\n\
@seealso{format, rat}\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin < 1 || nargin > 2 || nargout > 1)
    print_usage ();
  else
    {
      unwind_protect frame;

      frame.protect_var (rat_string_len);

      rat_string_len = 9;

      if (nargin == 2)
        rat_string_len = args(1).nint_value ();

      if (! error_state)
        {
          octave_value arg = args(0);

          if (arg.is_numeric_type ())
            {
              frame.protect_var (rat_format);

              rat_format = true;

              std::ostringstream buf;
              arg.print (buf);
              std::string s = buf.str ();

              std::list<std::string> lst;

              size_t n = 0;
              size_t s_len = s.length ();

              while (n < s_len)
                {
                  size_t m = s.find ('\n',  n);

                  if (m == std::string::npos)
                    {
                      lst.push_back (s.substr (n));
                      break;
                    }
                  else
                    {
                      lst.push_back (s.substr (n, m - n));
                      n = m + 1;
                    }
                }

              retval = string_vector (lst);
            }
          else
            error ("rats: X must be numeric");
        }
    }

  return retval;
}

DEFUN (disp, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} disp (@var{x})\n\
Display the value of @var{x}.\n\
\n\
For example:\n\
\n\
@example\n\
@group\n\
disp (\"The value of pi is:\"), disp (pi)\n\
\n\
     @print{} the value of pi is:\n\
     @print{} 3.1416\n\
@end group\n\
@end example\n\
\n\
@noindent\n\
Note that the output from @code{disp} always ends with a newline.\n\
\n\
If an output value is requested, @code{disp} prints nothing and returns the\n\
formatted output in a string.\n\
@seealso{fdisp}\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin == 1 && nargout < 2)
    {
      octave_value arg = args(0);

      if (nargout == 0)
        arg.print (octave_stdout);
      else
        {
          std::ostringstream buf;
          arg.print (buf);
          retval = octave_value (buf.str (), arg.is_dq_string () ? '"' : '\'');
        }
    }
  else
    print_usage ();

  return retval;
}

DEFUN (fdisp, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} fdisp (@var{fid}, @var{x})\n\
Display the value of @var{x} on the stream @var{fid}.\n\
\n\
For example:\n\
\n\
@example\n\
@group\n\
fdisp (stdout, \"The value of pi is:\"), fdisp (stdout, pi)\n\
\n\
     @print{} the value of pi is:\n\
     @print{} 3.1416\n\
@end group\n\
@end example\n\
\n\
@noindent\n\
Note that the output from @code{fdisp} always ends with a newline.\n\
@seealso{disp}\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin == 2)
    {
      int fid = octave_stream_list::get_file_number (args(0));

      octave_stream os = octave_stream_list::lookup (fid, "fdisp");

      if (! error_state)
        {
          std::ostream *osp = os.output_stream ();

          octave_value arg = args(1);

          if (osp)
            arg.print (*osp);
          else
            error ("fdisp: stream FID not open for writing");
        }
    }
  else
    print_usage ();

  return retval;
}

/*
%!test
%! format short
%! fd = tmpfile ();
%! for r = [0, Inf -Inf, NaN]
%!   for i = [0, Inf -Inf, NaN]
%!     fdisp (fd, complex (r, i));
%!   endfor
%! endfor
%! fclose (fd);

%!test
%! foo.real = pi * ones (3,20,3);
%! foo.complex = pi * ones (3,20,3) + 1i;
%! foo.char = repmat ("- Hello World -", [3, 20]);
%! foo.cell = {foo.real, foo.complex, foo.char};
%! fields = fieldnames (foo);
%! for f = 1:numel (fields)
%!   format loose;
%!   loose = disp (foo.(fields{f}));
%!   format compact;
%!   compact = disp (foo.(fields{f}));
%!   expected = strrep (loose, "\n\n", "\n");
%!   assert (expected, compact);
%! endfor
*/

static void
init_format_state (void)
{
  free_format = false;
  plus_format = false;
  rat_format = false;
  bank_format = false;
  hex_format = 0;
  bit_format = 0;
  Vcompact_format = false;
  print_e = false;
  print_big_e = false;
  print_g = false;
  print_eng = false;
}

static void
set_output_prec_and_fw (int prec, int fw)
{
  Voutput_precision =  prec;
  Voutput_max_field_width = fw;
}

static std::string format_string ("short");

static void
set_format_style (int argc, const string_vector& argv)
{
  int idx = 1;
  std::string format;

  if (--argc > 0)
    {
      std::string arg = argv[idx++];
      format = arg;

      if (arg == "short")
        {
          if (--argc > 0)
            {
              arg = argv[idx++];
              format.append (arg);

              if (arg == "e")
                {
                  init_format_state ();
                  print_e = true;
                }
              else if (arg == "E")
                {
                  init_format_state ();
                  print_e = true;
                  print_big_e = true;
                }
              else if (arg == "g")
                {
                  init_format_state ();
                  print_g = true;
                }
              else if (arg == "G")
                {
                  init_format_state ();
                  print_g = true;
                  print_big_e = true;
                }
              else if (arg == "eng")
                {
                  init_format_state ();
                  print_eng = true;
                }
              else
                {
                  error ("format: unrecognized option 'short %s'",
                         arg.c_str ());
                  return;
                }
            }
          else
            init_format_state ();

          set_output_prec_and_fw (5, 10);
        }
      else if (arg == "shorte")
        {
          init_format_state ();
          print_e = true;
          set_output_prec_and_fw (5, 10);
        }
      else if (arg == "shortE")
        {
          init_format_state ();
          print_e = true;
          print_big_e = true;
          set_output_prec_and_fw (5, 10);
        }
      else if (arg == "shortg")
        {
          init_format_state ();
          print_g = true;
          set_output_prec_and_fw (5, 10);
        }
      else if (arg == "shortG")
        {
          init_format_state ();
          print_g = true;
          print_big_e = true;
          set_output_prec_and_fw (5, 10);
        }
      else if (arg == "shortEng")
        {
          init_format_state ();
          print_eng = true;
          set_output_prec_and_fw (5, 10);
        }
      else if (arg == "long")
        {
          if (--argc > 0)
            {
              arg = argv[idx++];
              format.append (arg);

              if (arg == "e")
                {
                  init_format_state ();
                  print_e = true;
                }
              else if (arg == "E")
                {
                  init_format_state ();
                  print_e = true;
                  print_big_e = true;
                }
              else if (arg == "g")
                {
                  init_format_state ();
                  print_g = true;
                }
              else if (arg == "G")
                {
                  init_format_state ();
                  print_g = true;
                  print_big_e = true;
                }
              else if (arg == "eng")
                {
                  init_format_state ();
                  print_eng = true;
                }
              else
                {
                  error ("format: unrecognized option 'long %s'",
                         arg.c_str ());
                  return;
                }
            }
          else
            init_format_state ();

          set_output_prec_and_fw (15, 20);
        }
      else if (arg == "longe")
        {
          init_format_state ();
          print_e = true;
          set_output_prec_and_fw (15, 20);
        }
      else if (arg == "longE")
        {
          init_format_state ();
          print_e = true;
          print_big_e = true;
          set_output_prec_and_fw (15, 20);
        }
      else if (arg == "longg")
        {
          init_format_state ();
          print_g = true;
          set_output_prec_and_fw (15, 20);
        }
      else if (arg == "longG")
        {
          init_format_state ();
          print_g = true;
          print_big_e = true;
          set_output_prec_and_fw (15, 20);
        }
      else if (arg == "longEng")
        {
          init_format_state ();
          print_eng = true;
          set_output_prec_and_fw (15, 20);
        }
      else if (arg == "hex")
        {
          init_format_state ();
          hex_format = 1;
        }
      else if (arg == "native-hex")
        {
          init_format_state ();
          hex_format = 2;
        }
      else if (arg == "bit")
        {
          init_format_state ();
          bit_format = 1;
        }
      else if (arg == "native-bit")
        {
          init_format_state ();
          bit_format = 2;
        }
      else if (arg == "+" || arg == "plus")
        {
          if (--argc > 0)
            {
              arg = argv[idx++];
              format.append (arg);

              if (arg.length () == 3)
                plus_format_chars = arg;
              else
                {
                  error ("format: invalid option for plus format");
                  return;
                }
            }
          else
            plus_format_chars = "+- ";

          init_format_state ();
          plus_format = true;
        }
      else if (arg == "rat")
        {
          init_format_state ();
          rat_format = true;
        }
      else if (arg == "bank")
        {
          init_format_state ();
          bank_format = true;
        }
      else if (arg == "free")
        {
          init_format_state ();
          free_format = true;
        }
      else if (arg == "none")
        {
          init_format_state ();
          free_format = true;
        }
      else if (arg == "compact")
        {
          Vcompact_format = true;
          return;
        }
      else if (arg == "loose")
        {
          Vcompact_format = false;
          return;
        }
      else
        {
          error ("format: unrecognized format state '%s'", arg.c_str ());
          return;
        }
    }
  else
    {
      init_format_state ();
      set_output_prec_and_fw (5, 10);
      format = std::string ("short");
    }

  format_string = format;
}


DEFUN (format, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Command} {} format\n\
@deftypefnx {Command} {} format options\n\
Reset or specify the format of the output produced by @code{disp} and\n\
Octave's normal echoing mechanism.\n\
\n\
This command only affects the display of numbers but not how they are stored\n\
or computed.  To change the internal representation from the default double\n\
use one of the conversion functions such as @code{single}, @code{uint8},\n\
@code{int64}, etc.\n\
\n\
By default, Octave displays 5 significant digits in a human readable form\n\
(option @samp{short} paired with @samp{loose} format for matrices).\n\
If @code{format} is invoked without any options, this default format\n\
is restored.\n\
\n\
Valid formats for floating point numbers are listed in the following\n\
table.\n\
\n\
@table @code\n\
@item short\n\
Fixed point format with 5 significant figures in a field that is a maximum\n\
of 10 characters wide.  (default).\n\
\n\
If Octave is unable to format a matrix so that columns line up on the\n\
decimal point and all numbers fit within the maximum field width then\n\
it switches to an exponential @samp{e} format.\n\
\n\
@item long\n\
Fixed point format with 15 significant figures in a field that is a maximum\n\
of 20 characters wide.\n\
\n\
As with the @samp{short} format, Octave will switch to an exponential\n\
@samp{e} format if it is unable to format a matrix properly using the\n\
current format.\n\
\n\
@item  short e\n\
@itemx long e\n\
Exponential format.  The number to be represented is split between a mantissa\n\
and an exponent (power of 10).  The mantissa has 5 significant digits in the\n\
short format and 15 digits in the long format.\n\
For example, with the @samp{short e} format, @code{pi} is displayed as\n\
@code{3.1416e+00}.\n\
\n\
@item  short E\n\
@itemx long E\n\
Identical to @samp{short e} or @samp{long e} but displays an uppercase\n\
@samp{E} to indicate the exponent.\n\
For example, with the @samp{long E} format, @code{pi} is displayed as\n\
@code{3.14159265358979E+00}.\n\
\n\
@item  short g\n\
@itemx long g\n\
Optimally choose between fixed point and exponential format based on\n\
the magnitude of the number.\n\
For example, with the @samp{short g} format,\n\
@code{pi .^ [2; 4; 8; 16; 32]} is displayed as\n\
\n\
@example\n\
@group\n\
ans =\n\
\n\
      9.8696\n\
      97.409\n\
      9488.5\n\
  9.0032e+07\n\
  8.1058e+15\n\
@end group\n\
@end example\n\
\n\
@item  short eng\n\
@itemx long eng\n\
Identical to @samp{short e} or @samp{long e} but displays the value\n\
using an engineering format, where the exponent is divisible by 3. For\n\
example, with the @samp{short eng} format, @code{10 * pi} is displayed as\n\
@code{31.4159e+00}.\n\
\n\
@item  long G\n\
@itemx short G\n\
Identical to @samp{short g} or @samp{long g} but displays an uppercase\n\
@samp{E} to indicate the exponent.\n\
\n\
@item  free\n\
@itemx none\n\
Print output in free format, without trying to line up columns of\n\
matrices on the decimal point.  This also causes complex numbers to be\n\
formatted as numeric pairs like this @samp{(0.60419, 0.60709)} instead\n\
of like this @samp{0.60419 + 0.60709i}.\n\
@end table\n\
\n\
The following formats affect all numeric output (floating point and\n\
integer types).\n\
\n\
@table @code\n\
@item  \"+\"\n\
@itemx \"+\" @var{chars}\n\
@itemx plus\n\
@itemx plus @var{chars}\n\
Print a @samp{+} symbol for matrix elements greater than zero, a\n\
@samp{-} symbol for elements less than zero and a space for zero matrix\n\
elements.  This format can be very useful for examining the structure\n\
of a large sparse matrix.\n\
\n\
The optional argument @var{chars} specifies a list of 3 characters to use\n\
for printing values greater than zero, less than zero and equal to zero.\n\
For example, with the @samp{\"+\" \"+-.\"} format,\n\
@code{[1, 0, -1; -1, 0, 1]} is displayed as\n\
\n\
@example\n\
@group\n\
ans =\n\
\n\
+.-\n\
-.+\n\
@end group\n\
@end example\n\
\n\
@item bank\n\
Print in a fixed format with two digits to the right of the decimal\n\
point.\n\
\n\
@item native-hex\n\
Print the hexadecimal representation of numbers as they are stored in\n\
memory.  For example, on a workstation which stores 8 byte real values\n\
in IEEE format with the least significant byte first, the value of\n\
@code{pi} when printed in @code{native-hex} format is\n\
@code{400921fb54442d18}.\n\
\n\
@item hex\n\
The same as @code{native-hex}, but always print the most significant\n\
byte first.\n\
\n\
@item native-bit\n\
Print the bit representation of numbers as stored in memory.\n\
For example, the value of @code{pi} is\n\
\n\
@example\n\
@group\n\
01000000000010010010000111111011\n\
01010100010001000010110100011000\n\
@end group\n\
@end example\n\
\n\
(shown here in two 32 bit sections for typesetting purposes) when\n\
printed in native-bit format on a workstation which stores 8 byte real values\n\
in IEEE format with the least significant byte first.\n\
\n\
@item bit\n\
The same as @code{native-bit}, but always print the most significant\n\
bits first.\n\
\n\
@item rat\n\
Print a rational approximation, i.e., values are approximated\n\
as the ratio of small integers.\n\
For example, with the @samp{rat} format,\n\
@code{pi} is displayed as @code{355/113}.\n\
@end table\n\
\n\
The following two options affect the display of all matrices.\n\
\n\
@table @code\n\
@item compact\n\
Remove blank lines around column number labels and between\n\
matrices producing more compact output with more data per page.\n\
\n\
@item loose\n\
Insert blank lines above and below column number labels and between matrices\n\
to produce a more readable output with less data per page.  (default).\n\
@end table\n\
@seealso{fixed_point_format, output_max_field_width, output_precision, split_long_rows, print_empty_dimensions, rats}\n\
@end deftypefn")
{
  octave_value_list retval;

  int argc = args.length () + 1;

  string_vector argv = args.make_argv ("format");

  if (error_state)
    return retval;

  set_format_style (argc, argv);

  return retval;
}

DEFUN (__compactformat__, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{val} =} __compactformat__ ()\n\
@deftypefnx {Built-in Function} {} __compactformat__ (@var{TRUE|FALSE})\n\
Undocumented internal function\n\
@end deftypefn")
{
  return SET_INTERNAL_VARIABLE (compact_format);
}

DEFUN (__formatstring__, , ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {@var{val} =} __formatstring__ ()\n\
Undocumented internal function\n\
@end deftypefn")
{
  return ovl (format_string);
}

DEFUN (fixed_point_format, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{val} =} fixed_point_format ()\n\
@deftypefnx {Built-in Function} {@var{old_val} =} fixed_point_format (@var{new_val})\n\
@deftypefnx {Built-in Function} {} fixed_point_format (@var{new_val}, \"local\")\n\
Query or set the internal variable that controls whether Octave will\n\
use a scaled format to print matrix values.\n\
\n\
The scaled format prints a scaling factor on the first line of output chosen\n\
such that the largest matrix element can be written with a single leading\n\
digit.  For example:\n\
\n\
@example\n\
@group\n\
logspace (1, 7, 5)'\n\
ans =\n\
\n\
  1.0e+07  *\n\
\n\
  0.00000\n\
  0.00003\n\
  0.00100\n\
  0.03162\n\
  1.00000\n\
@end group\n\
@end example\n\
\n\
@noindent\n\
Notice that the first value appears to be 0 when it is actually 1.  Because\n\
of the possibility for confusion you should be careful about enabling\n\
@code{fixed_point_format}.\n\
\n\
When called from inside a function with the @qcode{\"local\"} option, the\n\
variable is changed locally for the function and any subroutines it calls.\n\
The original variable value is restored when exiting the function.\n\
@seealso{format, output_max_field_width, output_precision}\n\
@end deftypefn")
{
  return SET_INTERNAL_VARIABLE (fixed_point_format);
}

DEFUN (print_empty_dimensions, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{val} =} print_empty_dimensions ()\n\
@deftypefnx {Built-in Function} {@var{old_val} =} print_empty_dimensions (@var{new_val})\n\
@deftypefnx {Built-in Function} {} print_empty_dimensions (@var{new_val}, \"local\")\n\
Query or set the internal variable that controls whether the dimensions of\n\
empty matrices are printed along with the empty matrix symbol, @samp{[]}.\n\
\n\
For example, the expression\n\
\n\
@example\n\
zeros (3, 0)\n\
@end example\n\
\n\
@noindent\n\
will print\n\
\n\
@example\n\
ans = [](3x0)\n\
@end example\n\
\n\
When called from inside a function with the @qcode{\"local\"} option, the\n\
variable is changed locally for the function and any subroutines it calls.\n\
The original variable value is restored when exiting the function.\n\
@seealso{format}\n\
@end deftypefn")
{
  return SET_INTERNAL_VARIABLE (print_empty_dimensions);
}

DEFUN (split_long_rows, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{val} =} split_long_rows ()\n\
@deftypefnx {Built-in Function} {@var{old_val} =} split_long_rows (@var{new_val})\n\
@deftypefnx {Built-in Function} {} split_long_rows (@var{new_val}, \"local\")\n\
Query or set the internal variable that controls whether rows of a matrix\n\
may be split when displayed to a terminal window.\n\
\n\
If the rows are split, Octave will display the matrix in a series of smaller\n\
pieces, each of which can fit within the limits of your terminal width and\n\
each set of rows is labeled so that you can easily see which columns are\n\
currently being displayed.  For example:\n\
\n\
@example\n\
@group\n\
octave:13> rand (2,10)\n\
ans =\n\
\n\
 Columns 1 through 6:\n\
\n\
  0.75883  0.93290  0.40064  0.43818  0.94958  0.16467\n\
  0.75697  0.51942  0.40031  0.61784  0.92309  0.40201\n\
\n\
 Columns 7 through 10:\n\
\n\
  0.90174  0.11854  0.72313  0.73326\n\
  0.44672  0.94303  0.56564  0.82150\n\
@end group\n\
@end example\n\
\n\
When called from inside a function with the @qcode{\"local\"} option, the\n\
variable is changed locally for the function and any subroutines it calls.\n\
The original variable value is restored when exiting the function.\n\
@seealso{format}\n\
@end deftypefn")
{
  return SET_INTERNAL_VARIABLE (split_long_rows);
}

DEFUN (output_max_field_width, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{val} =} output_max_field_width ()\n\
@deftypefnx {Built-in Function} {@var{old_val} =} output_max_field_width (@var{new_val})\n\
@deftypefnx {Built-in Function} {} output_max_field_width (@var{new_val}, \"local\")\n\
Query or set the internal variable that specifies the maximum width\n\
of a numeric output field.\n\
\n\
When called from inside a function with the @qcode{\"local\"} option, the\n\
variable is changed locally for the function and any subroutines it calls.\n\
The original variable value is restored when exiting the function.\n\
@seealso{format, fixed_point_format, output_precision}\n\
@end deftypefn")
{
  return SET_INTERNAL_VARIABLE_WITH_LIMITS (output_max_field_width, 0,
                                            std::numeric_limits<int>::max ());
}

DEFUN (output_precision, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{val} =} output_precision ()\n\
@deftypefnx {Built-in Function} {@var{old_val} =} output_precision (@var{new_val})\n\
@deftypefnx {Built-in Function} {} output_precision (@var{new_val}, \"local\")\n\
Query or set the internal variable that specifies the minimum number of\n\
significant figures to display for numeric output.\n\
\n\
When called from inside a function with the @qcode{\"local\"} option, the\n\
variable is changed locally for the function and any subroutines it calls.\n\
The original variable value is restored when exiting the function.\n\
@seealso{format, fixed_point_format, output_max_field_width}\n\
@end deftypefn")
{
  return SET_INTERNAL_VARIABLE_WITH_LIMITS (output_precision, -1,
                                            std::numeric_limits<int>::max ());
}
