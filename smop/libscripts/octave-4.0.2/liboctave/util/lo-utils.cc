// utils.cc
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

#include <cctype>
#include <cstdlib>
#include <cstdio>
#include <cstring>
#include <cfloat>

#include <limits>
#include <string>

#include <sys/types.h>
#include <unistd.h>

#include "quit.h"

#include "lo-error.h"
#include "lo-ieee.h"
#include "lo-mappers.h"
#include "lo-utils.h"

bool xis_int_or_inf_or_nan (double x)
{ return xisnan (x) || D_NINT (x) == x; }

bool xis_one_or_zero (double x)
{ return x == 0 || x == 1; }

bool xis_zero (double x)
{ return x == 0; }

bool xtoo_large_for_float (double x)
{
  return (xfinite (x) && fabs (x) > std::numeric_limits<float>::max ());
}

bool xtoo_large_for_float (const Complex& x)
{
  return (xtoo_large_for_float (x.real ())
          || xtoo_large_for_float (x.imag ()));
}

bool xis_int_or_inf_or_nan (float x)
{ return xisnan (x) || D_NINT (x) == x; }

bool xis_one_or_zero (float x)
{ return x == 0 || x == 1; }

bool xis_zero (float x)
{ return x == 0; }

// Save a string.

char *
strsave (const char *s)
{
  if (! s)
    return 0;

  int len = strlen (s);
  char *tmp = new char [len+1];
  tmp = strcpy (tmp, s);
  return tmp;
}

// This function was adapted from xputenv from Karl Berry's kpathsearch
// library.

// FIXME: make this do the right thing if we don't have a SMART_PUTENV.

void
octave_putenv (const std::string& name, const std::string& value)
{
  int new_len = name.length () + value.length () + 2;

  char *new_item = static_cast<char*> (gnulib::malloc (new_len));

  sprintf (new_item, "%s=%s", name.c_str (), value.c_str ());

  // As far as I can see there's no way to distinguish between the
  // various errors; putenv doesn't have errno values.

  if (gnulib::putenv (new_item) < 0)
    (*current_liboctave_error_handler) ("putenv (%s) failed", new_item);
}

std::string
octave_fgets (FILE *f)
{
  bool eof;
  return octave_fgets (f, eof);
}

std::string
octave_fgets (FILE *f, bool& eof)
{
  eof = false;

  std::string retval;

  int grow_size = 1024;
  int max_size = grow_size;

  char *buf = static_cast<char *> (gnulib::malloc (max_size));
  char *bufptr = buf;
  int len = 0;

  do
    {
      if (gnulib::fgets (bufptr, grow_size, f))
        {
          len = strlen (bufptr);

          if (len == grow_size - 1)
            {
              int tmp = bufptr - buf + grow_size - 1;
              grow_size *= 2;
              max_size += grow_size;
              buf = static_cast<char *> (gnulib::realloc (buf, max_size));
              bufptr = buf + tmp;

              if (*(bufptr-1) == '\n')
                {
                  *bufptr = '\0';
                  retval = buf;
                }
            }
          else if (bufptr[len-1] != '\n')
            {
              bufptr[len++] = '\n';
              bufptr[len] = '\0';
              retval = buf;
            }
          else
            retval = buf;
        }
      else
        {
          if (len == 0)
            {
              eof = true;

              free (buf);

              buf = 0;
            }

          break;
        }
    }
  while (retval.empty ());

  if (buf)
    free (buf);

  octave_quit ();

  return retval;
}

std::string
octave_fgetl (FILE *f)
{
  bool eof;
  return octave_fgetl (f, eof);
}

std::string
octave_fgetl (FILE *f, bool& eof)
{
  std::string retval = octave_fgets (f, eof);

  size_t len = retval.length ();

  if (retval[len-1] == '\n')
    retval.resize (len-1);

  return retval;
}

// Note that the caller is responsible for repositioning the stream on failure.

template <typename T>
T
read_inf_nan_na (std::istream& is, char c0)
{
  T val = 0.0;

  switch (c0)
    {
    case 'i': case 'I':
      {
        char c1 = is.get ();
        if (c1 == 'n' || c1 == 'N')
          {
            char c2 = is.get ();
            if (c2 == 'f' || c2 == 'F')
              val = std::numeric_limits<T>::infinity ();
            else
              is.setstate (std::ios::failbit);
          }
        else
          is.setstate (std::ios::failbit);
      }
      break;

    case 'n': case 'N':
      {
        char c1 = is.get ();
        if (c1 == 'a' || c1 == 'A')
          {
            char c2 = is.get ();
            if (c2 == 'n' || c2 == 'N')
              val = std::numeric_limits<T>::quiet_NaN ();
            else
              {
                val = octave_numeric_limits<T>::NA ();
                if (c2 != EOF)
                  is.putback (c2);
              }
          }
        else
          is.setstate (std::ios::failbit);
      }
      break;

    default:
      abort ();
    }

  return val;
}

// Read a double value.  Discard any sign on NaN and NA.

template <typename T>
double
octave_read_fp_value (std::istream& is)
{
  T val = 0.0;

  // FIXME: resetting stream position is likely to fail unless we are
  // reading from a file.
  std::ios::streampos pos = is.tellg ();

  char c1 = ' ';

  while (isspace (c1))
    c1 = is.get ();

  bool neg = false;

  switch (c1)
    {
    case '-':
      neg = true;
      // fall through...

    case '+':
      {
        char c2 = 0;
        c2 = is.get ();
        if (c2 == 'i' || c2 == 'I' || c2 == 'n' || c2 == 'N')
          val = read_inf_nan_na<T> (is, c2);
        else
          {
            is.putback (c2);
            is >> val;
          }

        if (neg && ! is.fail ())
          val = -val;
      }
      break;

    case 'i': case 'I':
    case 'n': case 'N':
      val = read_inf_nan_na<T> (is, c1);
      break;

    default:
      is.putback (c1);
      is >> val;
      break;
    }

  std::ios::iostate status = is.rdstate ();
  if (status & std::ios::failbit)
    {
      is.clear ();
      is.seekg (pos);
      is.setstate (status);
    }

  return val;
}

template <typename T>
std::complex<T>
octave_read_cx_fp_value (std::istream& is)
{
  T re = 0.0;
  T im = 0.0;

  std::complex<T> cx = 0.0;

  char ch = ' ';

  while (isspace (ch))
    ch = is.get ();

  if (ch == '(')
    {
      re = octave_read_value<T> (is);
      ch = is.get ();

      if (ch == ',')
        {
          im = octave_read_value<T> (is);
          ch = is.get ();

          if (ch == ')')
            cx = std::complex<T> (re, im);
          else
            is.setstate (std::ios::failbit);
        }
      else if (ch == ')')
        cx = re;
      else
        is.setstate (std::ios::failbit);
    }
  else
    {
      is.putback (ch);
      cx = octave_read_value<double> (is);
    }

  return cx;
}

template <> OCTAVE_API double octave_read_value (std::istream& is)
{
  return octave_read_fp_value<double> (is);
}

template <> OCTAVE_API Complex octave_read_value (std::istream& is)
{
  return octave_read_cx_fp_value<double> (is);
}

template <> OCTAVE_API float octave_read_value (std::istream& is)
{
  return octave_read_fp_value<float> (is);
}

template <> OCTAVE_API FloatComplex octave_read_value (std::istream& is)
{
  return octave_read_cx_fp_value<float> (is);
}

void
octave_write_double (std::ostream& os, double d)
{
  if (lo_ieee_is_NA (d))
    os << "NA";
  else if (lo_ieee_isnan (d))
    os << "NaN";
  else if (lo_ieee_isinf (d))
    os << (d < 0 ? "-Inf" : "Inf");
  else
    os << d;
}

void
octave_write_complex (std::ostream& os, const Complex& c)
{
  os << "(";
  octave_write_double (os, real (c));
  os << ",";
  octave_write_double (os, imag (c));
  os << ")";
}

void
octave_write_float (std::ostream& os, float d)
{
  if (lo_ieee_is_NA (d))
    os << "NA";
  else if (lo_ieee_isnan (d))
    os << "NaN";
  else if (lo_ieee_isinf (d))
    os << (d < 0 ? "-Inf" : "Inf");
  else
    os << d;
}

void
octave_write_float_complex (std::ostream& os, const FloatComplex& c)
{
  os << "(";
  octave_write_float (os, real (c));
  os << ",";
  octave_write_float (os, imag (c));
  os << ")";
}
