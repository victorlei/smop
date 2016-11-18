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

#if !defined (octave_mach_info_h)
#define octave_mach_info_h 1

#include <string>

class
OCTAVE_API
oct_mach_info
{
protected:

  oct_mach_info (void);

public:

  enum float_format
  {
    flt_fmt_unknown,
    flt_fmt_ieee_little_endian,
    flt_fmt_ieee_big_endian,
  };

  static bool instance_ok (void);

  static float_format native_float_format (void);

  static bool words_big_endian (void);

  static bool words_little_endian (void);

  static float_format string_to_float_format (const std::string&);

  static std::string float_format_as_string (float_format);

private:

  static oct_mach_info *instance;

  static void cleanup_instance (void) { delete instance; instance = 0; }

  // The floating point format for the current machine.
  mutable float_format native_float_fmt;

  // TRUE if the byte order on this system is big endian.
  mutable bool big_chief;

  // No copying!

  oct_mach_info (const oct_mach_info&);

  oct_mach_info& operator = (const oct_mach_info&);
};

#endif
