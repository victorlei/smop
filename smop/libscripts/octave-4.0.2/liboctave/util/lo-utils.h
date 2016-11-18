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

#if !defined (octave_lo_utils_h)
#define octave_lo_utils_h 1

#include <cstdio>

#include <iostream>
#include <string>

#include "quit.h"

#include "lo-cutils.h"
#include "oct-cmplx.h"

// Generic any/all test functionality with arbitrary predicate.

template <class F, class T, bool zero>
bool
any_all_test (F fcn, const T *m, octave_idx_type len)
{
  octave_idx_type i;

  for (i = 0; i < len - 3; i += 4)
    {
      octave_quit ();

      if (fcn (m[i]) != zero
          || fcn (m[i+1]) != zero
          || fcn (m[i+2]) != zero
          || fcn (m[i+3]) != zero)
        return ! zero;
    }

  octave_quit ();

  for (; i < len; i++)
    if (fcn (m[i]) != zero)
      return ! zero;

  return zero;
}

extern OCTAVE_API bool xis_int_or_inf_or_nan (double x);
extern OCTAVE_API bool xis_one_or_zero (double x);
extern OCTAVE_API bool xis_zero (double x);
extern OCTAVE_API bool xtoo_large_for_float (double x);

extern OCTAVE_API bool xtoo_large_for_float (const Complex&  x);

extern OCTAVE_API bool xis_int_or_inf_or_nan (float x);
extern OCTAVE_API bool xis_one_or_zero (float x);
extern OCTAVE_API bool xis_zero (float x);
extern OCTAVE_API bool xtoo_large_for_float (float x);

extern OCTAVE_API char *strsave (const char *);

extern OCTAVE_API void
octave_putenv (const std::string&, const std::string&);

extern OCTAVE_API std::string octave_fgets (std::FILE *);
extern OCTAVE_API std::string octave_fgetl (std::FILE *);

extern OCTAVE_API std::string octave_fgets (std::FILE *, bool& eof);
extern OCTAVE_API std::string octave_fgetl (std::FILE *, bool& eof);

template <typename T>
T
octave_read_value (std::istream& is)
{
  T retval;
  is >> retval;
  return retval;
}

template <> OCTAVE_API double octave_read_value (std::istream& is);
template <> OCTAVE_API Complex octave_read_value (std::istream& is);
template <> OCTAVE_API float octave_read_value (std::istream& is);
template <> OCTAVE_API FloatComplex octave_read_value (std::istream& is);

// The next four functions are provided for backward compatibility.
inline double
octave_read_double (std::istream& is)
{
  return octave_read_value<double> (is);
}

inline Complex
octave_read_complex (std::istream& is)
{
  return octave_read_value<Complex> (is);
}

inline float
octave_read_float (std::istream& is)
{
  return octave_read_value<float> (is);
}

inline FloatComplex
octave_read_float_complex (std::istream& is)
{
  return octave_read_value<FloatComplex> (is);
}

extern OCTAVE_API void
octave_write_double (std::ostream& os, double dval);

extern OCTAVE_API void
octave_write_complex (std::ostream& os, const Complex& cval);

extern OCTAVE_API void
octave_write_float (std::ostream& os, float dval);

extern OCTAVE_API void
octave_write_float_complex (std::ostream& os, const FloatComplex& cval);

// Maybe this is overkill, but it allos

class
octave_wait
{
public:

  static bool ifexited (int status)
  {
    return octave_wifexited (status);
  }

  static int exitstatus (int status)
  {
    return octave_wexitstatus (status);
  }

  static bool ifsignaled (int status)
  {
    return octave_wifsignaled (status);
  }

  static int termsig (int status)
  {
    return octave_wtermsig (status);
  }

  static bool coredump (int status)
  {
    return octave_wcoredump (status);
  }

  static bool ifstopped (int status)
  {
    return octave_wifstopped (status);
  }

  static int stopsig (int status)
  {
    return octave_wstopsig (status);
  }

  static bool ifcontinued (int status)
  {
    return octave_wifcontinued (status);
  }
};

#endif
