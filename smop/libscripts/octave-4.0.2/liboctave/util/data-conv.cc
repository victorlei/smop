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

#include <iostream>
#include <limits>
#include <vector>

#include "byte-swap.h"
#include "data-conv.h"
#include "lo-error.h"
#include "lo-ieee.h"
#include "oct-locbuf.h"

#if defined HAVE_LONG_LONG_INT
#define FIND_SIZED_INT_TYPE(VAL, BITS, TQ, Q) \
  do \
    { \
      int sz = BITS / std::numeric_limits<unsigned char>::digits; \
      if (sizeof (TQ char) == sz) \
        VAL = oct_data_conv::dt_ ## Q ## char; \
      else if (sizeof (TQ short) == sz) \
        VAL = oct_data_conv::dt_ ## Q ## short; \
      else if (sizeof (TQ int) == sz) \
        VAL = oct_data_conv::dt_ ## Q ## int; \
      else if (sizeof (TQ long) == sz) \
        VAL = oct_data_conv::dt_ ## Q ## long; \
      else if (sizeof (TQ long long) == sz) \
        VAL = oct_data_conv::dt_ ## Q ## longlong; \
      else \
        VAL = oct_data_conv::dt_unknown; \
    } \
  while (0)
#else
#define FIND_SIZED_INT_TYPE(VAL, BITS, TQ, Q) \
  do \
    { \
      int sz = BITS / std::numeric_limits<unsigned char>::digits; \
      if (sizeof (TQ char) == sz) \
        VAL = oct_data_conv::dt_ ## Q ## char; \
      else if (sizeof (TQ short) == sz) \
        VAL = oct_data_conv::dt_ ## Q ## short; \
      else if (sizeof (TQ int) == sz) \
        VAL = oct_data_conv::dt_ ## Q ## int; \
      else if (sizeof (TQ long) == sz) \
        VAL = oct_data_conv::dt_ ## Q ## long; \
      else \
        VAL = oct_data_conv::dt_unknown; \
    } \
  while (0)
#endif

#define FIND_SIZED_FLOAT_TYPE(VAL, BITS) \
  do \
    { \
      int sz = BITS / std::numeric_limits<unsigned char>::digits; \
      if (sizeof (float) == sz) \
        VAL = oct_data_conv::dt_float; \
      else if (sizeof (double) == sz) \
        VAL = oct_data_conv::dt_double; \
      else \
        VAL = oct_data_conv::dt_unknown; \
    } \
  while (0)

// I'm not sure it is worth the trouble, but let's use a lookup table
// for the types that are supposed to be a specific number of bits
// wide.  Given the macros above, this should work as long as
// std::numeric_limits<unsigned char>::digits is a multiple of 8 and
// there are types with the right sizes.
//
// The sized data type lookup table has the following format:
//
//                            bits
//                    +----+----+----+----+
//                    |  8 | 16 | 32 | 64 |
//                    +----+----+----+----+
//     signed integer |    |    |    |    |
//                    +----+----+----+----+
//   unsigned integer |    |    |    |    |
//                    +----+----+----+----+
//     floating point |    |    |    |    |
//                    +----+----+----+----+
//
// So, the 0,3 element is supposed to contain the oct_data_conv enum
// value corresponding to the correct native data type for a signed
// 32-bit integer.

static void
init_sized_type_lookup_table (oct_data_conv::data_type table[3][4])
{
  int bits = 8;

  for (int i = 0; i < 4; i++)
    {
      FIND_SIZED_INT_TYPE (table[0][i], bits, , );

      FIND_SIZED_INT_TYPE (table[1][i], bits, unsigned, u);

      FIND_SIZED_FLOAT_TYPE (table[2][i], bits);

      bits *= 2;
    }
}

static std::string
strip_spaces (const std::string& str)
{
  size_t n = str.length ();

  size_t k = 0;

  std::string s (n, ' ');

  for (size_t i = 0; i < n; i++)
    if (! isspace (str[i]))
      s[k++] = tolower (str[i]);

  s.resize (k);

  return s;
}

#define GET_SIZED_INT_TYPE(T, U) \
  do \
    { \
      switch (sizeof (T)) \
        { \
        case 1: \
          retval = dt_ ## U ## int8; \
          break; \
 \
        case 2: \
          retval = dt_ ## U ## int16; \
          break; \
 \
        case 4: \
          retval = dt_ ## U ## int32; \
          break; \
 \
        case 8: \
          retval = dt_ ## U ## int64; \
          break; \
 \
        default: \
          retval = dt_unknown; \
          break; \
        } \
    } \
  while (0)

size_t
oct_data_conv::data_type_size (data_type dt)
{
  size_t retval = -1;

  switch (dt)
    {
    case oct_data_conv::dt_int8:
      retval = sizeof (int8_t);
      break;

    case oct_data_conv::dt_uint8:
      retval = sizeof (uint8_t);
      break;

    case oct_data_conv::dt_int16:
      retval = sizeof (int16_t);
      break;

    case oct_data_conv::dt_uint16:
      retval = sizeof (uint16_t);
      break;

    case oct_data_conv::dt_int32:
      retval = sizeof (int32_t);
      break;

    case oct_data_conv::dt_uint32:
      retval = sizeof (uint32_t);
      break;

    case oct_data_conv::dt_int64:
      retval = sizeof (int64_t);
      break;

    case oct_data_conv::dt_uint64:
      retval = sizeof (uint64_t);
      break;

    case oct_data_conv::dt_float:
    case oct_data_conv::dt_single:
      retval = sizeof (float);
      break;

    case oct_data_conv::dt_double:
      retval = sizeof (double);
      break;

    case oct_data_conv::dt_char:
      retval = sizeof (char);
      break;

    case oct_data_conv::dt_schar:
      retval = sizeof (signed char);
      break;

    case oct_data_conv::dt_uchar:
      retval = sizeof (unsigned char);
      break;

    case oct_data_conv::dt_short:
      retval = sizeof (short);
      break;

    case oct_data_conv::dt_ushort:
      retval = sizeof (unsigned short);
      break;

    case oct_data_conv::dt_int:
      retval = sizeof (int);
      break;

    case oct_data_conv::dt_uint:
      retval = sizeof (unsigned int);
      break;

    case oct_data_conv::dt_long:
      retval = sizeof (long);
      break;

    case oct_data_conv::dt_ulong:
      retval = sizeof (unsigned long);
      break;

    case oct_data_conv::dt_longlong:
      retval = sizeof (long long);
      break;

    case oct_data_conv::dt_ulonglong:
      retval = sizeof (unsigned long long);
      break;

    case oct_data_conv::dt_logical:
      retval = sizeof (bool);
      break;

    case oct_data_conv::dt_unknown:
    default:
      abort ();
      break;
    }

  return retval;
}

oct_data_conv::data_type
oct_data_conv::string_to_data_type (const std::string& str)
{
  data_type retval = dt_unknown;

  static bool initialized = false;

  static data_type sized_type_table[3][4];

  if (! initialized)
    {
      init_sized_type_lookup_table (sized_type_table);

      initialized = true;
    }

  std::string s = strip_spaces (str);

  if (s == "int8" || s == "integer*1")
    retval = dt_int8;
  else if (s == "uint8")
    retval = dt_uint8;
  else if (s == "int16" || s == "integer*2")
    retval = dt_int16;
  else if (s == "uint16")
    retval = dt_uint16;
  else if (s == "int32" || s == "integer*4")
    retval = dt_int32;
  else if (s == "uint32")
    retval = dt_uint32;
  else if (s == "int64" || s == "integer*8")
    retval = dt_int64;
  else if (s == "uint64")
    retval = dt_uint64;
  else if (s == "single" || s == "float32" || s == "real*4")
    retval = dt_single;
  else if (s == "double" || s == "float64" || s == "real*8")
    retval = dt_double;
  else if (s == "char" || s == "char*1")
    retval = dt_char;
  else if (s == "schar" || s == "signedchar")
    retval = dt_schar;
  else if (s == "uchar" || s == "unsignedchar")
    retval = dt_uchar;
  else if (s == "short")
    GET_SIZED_INT_TYPE (short, );
  else if (s == "ushort" || s == "unsignedshort")
    GET_SIZED_INT_TYPE (unsigned short, u);
  else if (s == "int")
    GET_SIZED_INT_TYPE (int, );
  else if (s == "uint" || s == "unsignedint")
    GET_SIZED_INT_TYPE (unsigned int, u);
  else if (s == "long")
    GET_SIZED_INT_TYPE (long, );
  else if (s == "ulong" || s == "unsignedlong")
    GET_SIZED_INT_TYPE (unsigned long, u);
  else if (s == "longlong")
    GET_SIZED_INT_TYPE (long long, );
  else if (s == "ulonglong" || s == "unsignedlonglong")
    GET_SIZED_INT_TYPE (unsigned long long, u);
  else if (s == "float")
    {
      if (sizeof (float) == sizeof (double))
        retval = dt_double;
      else
        retval = dt_single;
    }
  else if (s == "logical")
    retval = dt_logical;
  else
    (*current_liboctave_error_handler) ("invalid data type specified");

  if (retval == dt_unknown)
    (*current_liboctave_error_handler)
      ("unable to find matching native data type for %s", s.c_str ());

  return retval;
}

void
oct_data_conv::string_to_data_type (const std::string& str, int& block_size,
                                    oct_data_conv::data_type& input_type,
                                    oct_data_conv::data_type& output_type)
{
  block_size = 1;
  input_type = dt_uchar;
  output_type = dt_double;

  bool input_is_output = false;

  std::string s = strip_spaces (str);

  size_t pos = 0;

  if (s[0] == '*')
    input_is_output = true;
  else
    {
      size_t len = s.length ();

      while (pos < len && isdigit (s[pos]))
        pos++;

      if (pos > 0)
        {
          if (s[pos] == '*')
            {
              block_size = atoi (s.c_str ());
              s = s.substr (pos+1);
            }
          else
            {
              (*current_liboctave_error_handler)
                ("invalid repeat count in '%s'", str.c_str ());

              return;
            }
        }
    }

  pos = s.find ('=');

  if (pos != std::string::npos)
    {
      if (s[pos+1] == '>')
        {
          std::string s1;

          if (input_is_output)
            {
              s1 = s.substr (1, pos-1);

              (*current_liboctave_warning_with_id_handler)
                ("Octave:fread-precision-syntax",
                 "warning: ignoring leading * in fread precision");
            }
          else
            s1 = s.substr (0, pos);

          input_type = string_to_data_type (s1);
          output_type = string_to_data_type (s.substr (pos+2));
        }
      else
        (*current_liboctave_error_handler)
          ("fread: invalid precision specified");
    }
  else
    {
      if (input_is_output)
        s = s.substr (1);

      input_type = string_to_data_type (s);

      if (input_is_output)
        output_type = input_type;
    }
}

void
oct_data_conv::string_to_data_type (const std::string& str, int& block_size,
                                    oct_data_conv::data_type& output_type)
{
  block_size = 1;
  output_type = dt_double;

  std::string s = strip_spaces (str);

  size_t pos = 0;

  size_t len = s.length ();

  while (pos < len && isdigit (s[pos]))
    pos++;

  if (pos > 0)
    {
      if (s[pos] == '*')
        {
          block_size = atoi (s.c_str ());
          s = s.substr (pos+1);
        }
      else
        {
          (*current_liboctave_error_handler)
            ("invalid repeat count in '%s'", str.c_str ());

          return;
        }
    }

  output_type = string_to_data_type (s);
}

std::string
oct_data_conv::data_type_as_string (oct_data_conv::data_type dt)
{
  std::string retval;

  switch (dt)
    {
    case oct_data_conv::dt_int8:
      retval = "int8";
      break;

    case oct_data_conv::dt_uint8:
      retval = "uint8";
      break;

    case oct_data_conv::dt_int16:
      retval = "int16";
      break;

    case oct_data_conv::dt_uint16:
      retval = "uint16";
      break;

    case oct_data_conv::dt_int32:
      retval = "int32";
      break;

    case oct_data_conv::dt_uint32:
      retval = "uint32";
      break;

    case oct_data_conv::dt_int64:
      retval = "int64";
      break;

    case oct_data_conv::dt_uint64:
      retval = "uint64";
      break;

    case oct_data_conv::dt_single:
      retval = "single";
      break;

    case oct_data_conv::dt_double:
      retval = "double";
      break;

    case oct_data_conv::dt_char:
      retval = "char";
      break;

    case oct_data_conv::dt_schar:
      retval = "signed char";
      break;

    case oct_data_conv::dt_uchar:
      retval = "unsigned char";
      break;

    case oct_data_conv::dt_short:
      retval = "short";
      break;

    case oct_data_conv::dt_ushort:
      retval = "unsigned short";
      break;

    case oct_data_conv::dt_int:
      retval = "int";
      break;

    case oct_data_conv::dt_uint:
      retval = "unsigned int";
      break;

    case oct_data_conv::dt_long:
      retval = "long";
      break;

    case oct_data_conv::dt_ulong:
      retval = "unsigned long";
      break;

    case oct_data_conv::dt_longlong:
      retval = "long long";
      break;

    case oct_data_conv::dt_ulonglong:
      retval = "unsigned long long";
      break;

    case oct_data_conv::dt_float:
      retval = "float";
      break;

    case oct_data_conv::dt_logical:
      retval = "logical";
      break;

    case oct_data_conv::dt_unknown:
    default:
      retval = "unknown";
      break;
    }

  return retval;
}

#define LS_DO_READ(TYPE, swap, data, size, len, stream) \
  do \
    { \
      if (len > 0) \
        { \
          OCTAVE_LOCAL_BUFFER (TYPE, ptr, len); \
          std::streamsize n_bytes = size * static_cast<std::streamsize> (len); \
          stream.read (reinterpret_cast<char *> (ptr), n_bytes); \
          if (swap) \
            swap_bytes< size > (ptr, len); \
          for (octave_idx_type i = 0; i < len; i++) \
            data[i] = ptr[i]; \
        } \
    } \
  while (0)

// Have to use copy here to avoid writing over data accessed via
// Matrix::data ().

#define LS_DO_WRITE(TYPE, data, size, len, stream) \
  do \
    { \
      if (len > 0) \
        { \
          char tmp_type = type; \
          stream.write (&tmp_type, 1); \
          OCTAVE_LOCAL_BUFFER (TYPE, ptr, len); \
          for (octave_idx_type i = 0; i < len; i++) \
            ptr[i] = static_cast<TYPE> (data[i]);         \
          std::streamsize n_bytes = size * static_cast<std::streamsize> (len); \
          stream.write (reinterpret_cast<char *> (ptr), n_bytes); \
        } \
    } \
  while (0)

// Loading variables from files.

static void
gripe_unrecognized_float_fmt (void)
{
  (*current_liboctave_error_handler)
    ("unrecognized floating point format requested");
}

// But first, some data conversion routines.

// Currently, we only handle conversions for the IEEE types.  To fix
// that, make more of the following routines work.

// FIXME: assumes sizeof (Complex) == 8
// FIXME: assumes sizeof (double) == 8
// FIXME: assumes sizeof (float) == 4

static void
IEEE_big_double_to_IEEE_little_double (void *d, octave_idx_type len)
{
  swap_bytes<8> (d, len);
}

static void
IEEE_big_float_to_IEEE_little_float (void *d, octave_idx_type len)
{
  swap_bytes<4> (d, len);
}

static void
IEEE_little_double_to_IEEE_big_double (void *d, octave_idx_type len)
{
  swap_bytes<8> (d, len);
}

static void
IEEE_little_float_to_IEEE_big_float (void *d, octave_idx_type len)
{
  swap_bytes<4> (d, len);
}

void
do_double_format_conversion (void *data, octave_idx_type len,
                             oct_mach_info::float_format from_fmt,
                             oct_mach_info::float_format to_fmt)
{
  switch (to_fmt)
    {
    case oct_mach_info::flt_fmt_ieee_little_endian:
      switch (from_fmt)
        {
        case oct_mach_info::flt_fmt_ieee_little_endian:
          break;

        case oct_mach_info::flt_fmt_ieee_big_endian:
          IEEE_big_double_to_IEEE_little_double (data, len);
          break;

        default:
          gripe_unrecognized_float_fmt ();
          break;
        }
      break;

    case oct_mach_info::flt_fmt_ieee_big_endian:
      switch (from_fmt)
        {
        case oct_mach_info::flt_fmt_ieee_little_endian:
          IEEE_little_double_to_IEEE_big_double (data, len);
          break;

        case oct_mach_info::flt_fmt_ieee_big_endian:
          break;

        default:
          gripe_unrecognized_float_fmt ();
          break;
        }
      break;

    default:
      (*current_liboctave_error_handler)
        ("impossible state reached in file '%s' at line %d",
         __FILE__, __LINE__);
      break;
    }
}

void
do_float_format_conversion (void *data, octave_idx_type len,
                            oct_mach_info::float_format from_fmt,
                            oct_mach_info::float_format to_fmt)
{
  switch (to_fmt)
    {
    case oct_mach_info::flt_fmt_ieee_little_endian:
      switch (from_fmt)
        {
        case oct_mach_info::flt_fmt_ieee_little_endian:
          break;

        case oct_mach_info::flt_fmt_ieee_big_endian:
          IEEE_big_float_to_IEEE_little_float (data, len);
          break;

        default:
          gripe_unrecognized_float_fmt ();
          break;
        }
      break;

    case oct_mach_info::flt_fmt_ieee_big_endian:
      switch (from_fmt)
        {
        case oct_mach_info::flt_fmt_ieee_little_endian:
          IEEE_little_float_to_IEEE_big_float (data, len);
          break;

        case oct_mach_info::flt_fmt_ieee_big_endian:
          break;

        default:
          gripe_unrecognized_float_fmt ();
          break;
        }
      break;

    default:
      (*current_liboctave_error_handler)
        ("impossible state reached in file '%s' at line %d",
         __FILE__, __LINE__);
      break;
    }
}

void
do_float_format_conversion (void *data, size_t sz, octave_idx_type len,
                            oct_mach_info::float_format from_fmt,
                            oct_mach_info::float_format to_fmt)
{
  switch (sz)
    {
    case sizeof (float):
      do_float_format_conversion (data, len, from_fmt, to_fmt);
      break;

    case sizeof (double):
      do_double_format_conversion (data, len, from_fmt, to_fmt);
      break;

    default:
      (*current_liboctave_error_handler)
        ("impossible state reached in file '%s' at line %d",
         __FILE__, __LINE__);
      break;
    }
}

void
read_doubles (std::istream& is, double *data, save_type type,
              octave_idx_type len, bool swap,
              oct_mach_info::float_format fmt)
{
  switch (type)
    {
    case LS_U_CHAR:
      LS_DO_READ (uint8_t, swap, data, 1, len, is);
      break;

    case LS_U_SHORT:
      LS_DO_READ (uint16_t, swap, data, 2, len, is);
      break;

    case LS_U_INT:
      LS_DO_READ (uint32_t, swap, data, 4, len, is);
      break;

    case LS_CHAR:
      LS_DO_READ (int8_t, swap, data, 1, len, is);
      break;

    case LS_SHORT:
      LS_DO_READ (int16_t, swap, data, 2, len, is);
      break;

    case LS_INT:
      LS_DO_READ (int32_t, swap, data, 4, len, is);
      break;

    case LS_FLOAT:
      {
        OCTAVE_LOCAL_BUFFER (float, ptr, len);
        std::streamsize n_bytes = 4 * static_cast<std::streamsize> (len);
        is.read (reinterpret_cast<char *> (ptr), n_bytes);
        do_float_format_conversion (ptr, len, fmt);
        for (octave_idx_type i = 0; i < len; i++)
          data[i] = ptr[i];
      }
      break;

    case LS_DOUBLE: // No conversion necessary.
      {
        std::streamsize n_bytes = 8 * static_cast<std::streamsize> (len);
        is.read (reinterpret_cast<char *> (data), n_bytes);
        do_double_format_conversion (data, len, fmt);

        for (int i = 0; i < len; i++)
          data[i] = __lo_ieee_replace_old_NA (data[i]);
      }
      break;

    default:
      is.clear (std::ios::failbit|is.rdstate ());
      break;
    }
}

void
read_floats (std::istream& is, float *data, save_type type,
             octave_idx_type len, bool swap,
             oct_mach_info::float_format fmt)
{
  switch (type)
    {
    case LS_U_CHAR:
      LS_DO_READ (uint8_t, swap, data, 1, len, is);
      break;

    case LS_U_SHORT:
      LS_DO_READ (uint16_t, swap, data, 2, len, is);
      break;

    case LS_U_INT:
      LS_DO_READ (uint32_t, swap, data, 4, len, is);
      break;

    case LS_CHAR:
      LS_DO_READ (int8_t, swap, data, 1, len, is);
      break;

    case LS_SHORT:
      LS_DO_READ (int16_t, swap, data, 2, len, is);
      break;

    case LS_INT:
      LS_DO_READ (int32_t, swap, data, 4, len, is);
      break;

    case LS_FLOAT: // No conversion necessary.
      {
        std::streamsize n_bytes = 4 * static_cast<std::streamsize> (len);
        is.read (reinterpret_cast<char *> (data), n_bytes);
        do_float_format_conversion (data, len, fmt);
      }
      break;

    case LS_DOUBLE:
      {
        OCTAVE_LOCAL_BUFFER (double, ptr, len);
        std::streamsize n_bytes = 8 * static_cast<std::streamsize> (len);
        is.read (reinterpret_cast<char *> (ptr), n_bytes);
        do_double_format_conversion (ptr, len, fmt);
        for (octave_idx_type i = 0; i < len; i++)
          data[i] = ptr[i];
      }
      break;

    default:
      is.clear (std::ios::failbit|is.rdstate ());
      break;
    }
}

void
write_doubles (std::ostream& os, const double *data, save_type type,
               octave_idx_type len)
{
  switch (type)
    {
    case LS_U_CHAR:
      LS_DO_WRITE (uint8_t, data, 1, len, os);
      break;

    case LS_U_SHORT:
      LS_DO_WRITE (uint16_t, data, 2, len, os);
      break;

    case LS_U_INT:
      LS_DO_WRITE (uint32_t, data, 4, len, os);
      break;

    case LS_CHAR:
      LS_DO_WRITE (int8_t, data, 1, len, os);
      break;

    case LS_SHORT:
      LS_DO_WRITE (int16_t, data, 2, len, os);
      break;

    case LS_INT:
      LS_DO_WRITE (int32_t, data, 4, len, os);
      break;

    case LS_FLOAT:
      LS_DO_WRITE (float, data, 4, len, os);
      break;

    case LS_DOUBLE: // No conversion necessary.
      {
        char tmp_type = static_cast<char> (type);
        os.write (&tmp_type, 1);
        std::streamsize n_bytes = 8 * static_cast<std::streamsize> (len);
        os.write (reinterpret_cast <const char *> (data), n_bytes);
      }
      break;

    default:
      (*current_liboctave_error_handler)
        ("unrecognized data format requested");
      break;
    }
}

void
write_floats (std::ostream& os, const float *data, save_type type,
              octave_idx_type len)
{
  switch (type)
    {
    case LS_U_CHAR:
      LS_DO_WRITE (uint8_t, data, 1, len, os);
      break;

    case LS_U_SHORT:
      LS_DO_WRITE (uint16_t, data, 2, len, os);
      break;

    case LS_U_INT:
      LS_DO_WRITE (uint32_t, data, 4, len, os);
      break;

    case LS_CHAR:
      LS_DO_WRITE (int8_t, data, 1, len, os);
      break;

    case LS_SHORT:
      LS_DO_WRITE (int16_t, data, 2, len, os);
      break;

    case LS_INT:
      LS_DO_WRITE (int32_t, data, 4, len, os);
      break;

    case LS_FLOAT: // No conversion necessary.
      {
        char tmp_type = static_cast<char> (type);
        os.write (&tmp_type, 1);
        std::streamsize n_bytes = 4 * static_cast<std::streamsize> (len);
        os.write (reinterpret_cast <const char *> (data), n_bytes);
      }
      break;

    case LS_DOUBLE:
      LS_DO_WRITE (double, data, 8, len, os);
      break;

    default:
      (*current_liboctave_error_handler)
        ("unrecognized data format requested");
      break;
    }
}
