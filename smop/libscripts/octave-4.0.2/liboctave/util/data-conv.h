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

#if !defined (octave_data_conv_h)
#define octave_data_conv_h 1

#include <limits>

#include "mach-info.h"
#include "oct-inttypes.h"

class
OCTAVE_API
oct_data_conv
{
public:

  enum data_type
  {
    dt_int8      =  0,
    dt_uint8     =  1,
    dt_int16     =  2,
    dt_uint16    =  3,
    dt_int32     =  4,
    dt_uint32    =  5,
    dt_int64     =  6,
    dt_uint64    =  7,
    dt_single    =  8,
    dt_double    =  9,
    dt_char      = 10,
    dt_schar     = 11,
    dt_uchar     = 12,
    dt_logical   = 13,
    dt_short     = 14,
    dt_ushort    = 15,
    dt_int       = 16,
    dt_uint      = 17,
    dt_long      = 18,
    dt_ulong     = 19,
    dt_longlong  = 20,
    dt_ulonglong = 21,
    dt_float     = 22,
    dt_unknown   = 23 // Must be last, have largest value!
  };

  static size_t data_type_size (data_type dt);

  static data_type string_to_data_type (const std::string& s);

  static void string_to_data_type (const std::string& s, int& block_size,
                                   data_type& input_type,
                                   data_type& output_type);

  static void string_to_data_type (const std::string& s, int& block_size,
                                   data_type& output_type);

  static std::string data_type_as_string (data_type dt);
};

// Add new entries to the end of this enum, otherwise Octave will not
// be able to read binary data files stored in Octave's binary data
// format that were created with previous versions of Octave.

enum save_type
{
  LS_U_CHAR  = 0,
  LS_U_SHORT = 1,
  LS_U_INT   = 2,
  LS_CHAR    = 3,
  LS_SHORT   = 4,
  LS_INT     = 5,
  LS_FLOAT   = 6,
  LS_DOUBLE  = 7,
  LS_U_LONG  = 8,
  LS_LONG    = 9
};

extern OCTAVE_API void
do_double_format_conversion (void *data, octave_idx_type len,
                             oct_mach_info::float_format from_fmt,
                             oct_mach_info::float_format to_fmt
                             = oct_mach_info::native_float_format ());

extern OCTAVE_API void
do_float_format_conversion (void *data, octave_idx_type len,
                            oct_mach_info::float_format from_fmt,
                            oct_mach_info::float_format to_fmt
                            = oct_mach_info::native_float_format ());

extern OCTAVE_API void
do_float_format_conversion (void *data, size_t sz, octave_idx_type len,
                            oct_mach_info::float_format from_fmt,
                            oct_mach_info::float_format to_fmt
                            = oct_mach_info::native_float_format ());

extern OCTAVE_API void
read_doubles (std::istream& is, double *data, save_type type,
              octave_idx_type len, bool swap, oct_mach_info::float_format fmt);

extern OCTAVE_API void
write_doubles (std::ostream& os, const double *data, save_type type,
               octave_idx_type len);

extern OCTAVE_API void
read_floats (std::istream& is, float *data, save_type type,
             octave_idx_type len, bool swap, oct_mach_info::float_format fmt);

extern OCTAVE_API void
write_floats (std::ostream& os, const float *data, save_type type,
              octave_idx_type len);

template <typename T>
inline bool
is_equivalent_type (oct_data_conv::data_type)
{
  return false;
}

template <>
inline bool
is_equivalent_type<int8_t> (oct_data_conv::data_type t)
{
  return t == oct_data_conv::dt_int8;
}

template <>
inline bool
is_equivalent_type<int16_t> (oct_data_conv::data_type t)
{
  return t == oct_data_conv::dt_int16;
}

template <>
inline bool
is_equivalent_type<int32_t> (oct_data_conv::data_type t)
{
  return t == oct_data_conv::dt_int32;
}

template <>
inline bool
is_equivalent_type<int64_t> (oct_data_conv::data_type t)
{
  return t == oct_data_conv::dt_int64;
}

template <>
inline bool
is_equivalent_type<uint8_t> (oct_data_conv::data_type t)
{
  return t == oct_data_conv::dt_uint8;
}

template <>
inline bool
is_equivalent_type<uint16_t> (oct_data_conv::data_type t)
{
  return t == oct_data_conv::dt_uint16;
}

template <>
inline bool
is_equivalent_type<uint32_t> (oct_data_conv::data_type t)
{
  return t == oct_data_conv::dt_uint32;
}

template <>
inline bool
is_equivalent_type<uint64_t> (oct_data_conv::data_type t)
{
  return t == oct_data_conv::dt_uint64;
}

template <>
inline bool
is_equivalent_type<octave_int8> (oct_data_conv::data_type t)
{
  return t == oct_data_conv::dt_int8;
}

template <>
inline bool
is_equivalent_type<octave_int16> (oct_data_conv::data_type t)
{
  return t == oct_data_conv::dt_int16;
}

template <>
inline bool
is_equivalent_type<octave_int32> (oct_data_conv::data_type t)
{
  return t == oct_data_conv::dt_int32;
}

template <>
inline bool
is_equivalent_type<octave_int64> (oct_data_conv::data_type t)
{
  return t == oct_data_conv::dt_int64;
}

template <>
inline bool
is_equivalent_type<octave_uint8> (oct_data_conv::data_type t)
{
  return t == oct_data_conv::dt_uint8;
}

template <>
inline bool
is_equivalent_type<octave_uint16> (oct_data_conv::data_type t)
{
  return t == oct_data_conv::dt_uint16;
}

template <>
inline bool
is_equivalent_type<octave_uint32> (oct_data_conv::data_type t)
{
  return t == oct_data_conv::dt_uint32;
}

template <>
inline bool
is_equivalent_type<octave_uint64> (oct_data_conv::data_type t)
{
  return t == oct_data_conv::dt_uint64;
}

template <>
inline bool
is_equivalent_type<double> (oct_data_conv::data_type t)
{
  return t == oct_data_conv::dt_double;
}

template <>
inline bool
is_equivalent_type<float> (oct_data_conv::data_type t)
{
  return t == oct_data_conv::dt_single || t == oct_data_conv::dt_float;
}

#endif
