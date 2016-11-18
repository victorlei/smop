/*

Copyright (C) 2007-2015 David Bateman
Copyright (C) 2009 VZLU Prague

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

#include <limits>

#include "mx-base.h"

#include "defun.h"
#include "error.h"
#include "gripes.h"
#include "oct-obj.h"
#include "unwind-prot.h"

static dim_vector
get_vec_dims (const dim_vector& old_dims, octave_idx_type n)
{
  if (old_dims.length () == 2 && old_dims(0) == 1)
    return dim_vector (1, n);
  else if (old_dims.length () == 2 && old_dims (0) == 0 && old_dims (1) == 0)
    return dim_vector ();
  else
    return dim_vector (n, 1);
}

template <class ArrayType>
static void
get_data_and_bytesize (const ArrayType& array,
                       const void *& data,
                       octave_idx_type& byte_size,
                       dim_vector& old_dims,
                       unwind_protect& frame)
{
  // The array given may be a temporary, constructed from a scalar or sparse
  // array. This will ensure the data will be deallocated after we exit.
  frame.add_delete (new ArrayType (array));

  data = reinterpret_cast<const void *> (array.data ());
  byte_size = array.byte_size ();

  old_dims = array.dims ();
}

template <class ArrayType>
static ArrayType
reinterpret_copy (const void *data, octave_idx_type byte_size,
                  const dim_vector& old_dims)
{
  typedef typename ArrayType::element_type T;
  octave_idx_type n = byte_size / sizeof (T);

  if (n * static_cast<int> (sizeof (T)) == byte_size)
    {
      ArrayType retval (get_vec_dims (old_dims, n));
      T *dest = retval.fortran_vec ();
      std::memcpy (dest, data, n * sizeof (T));

      return retval;
    }
  else
    {
      error ("typecast: incorrect number of input values to make output value");
      return ArrayType ();
    }
}


DEFUN (typecast, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {@var{y} =} typecast (@var{x}, \"@var{class}\")\n\
Return a new array @var{y} resulting from interpreting the data of @var{x}\n\
in memory as data of the numeric class @var{class}.\n\
\n\
Both the class of @var{x} and @var{class} must be one of the built-in\n\
numeric classes:\n\
\n\
@example\n\
@group\n\
\"logical\"\n\
\"char\"\n\
\"int8\"\n\
\"int16\"\n\
\"int32\"\n\
\"int64\"\n\
\"uint8\"\n\
\"uint16\"\n\
\"uint32\"\n\
\"uint64\"\n\
\"double\"\n\
\"single\"\n\
\"double complex\"\n\
\"single complex\"\n\
@end group\n\
@end example\n\
\n\
@noindent\n\
the last two are only used with @var{class}; they indicate that a\n\
complex-valued result is requested.  Complex arrays are stored in memory as\n\
consecutive pairs of real numbers.  The sizes of integer types are given by\n\
their bit counts.  Both logical and char are typically one byte wide;\n\
however, this is not guaranteed by C++.  If your system is IEEE conformant,\n\
single and double will be 4 bytes and 8 bytes wide, respectively.\n\
@qcode{\"logical\"} is not allowed for @var{class}.\n\
\n\
If the input is a row vector, the return value is a row vector, otherwise it\n\
is a column vector.\n\
\n\
If the bit length of @var{x} is not divisible by that of @var{class}, an\n\
error occurs.\n\
\n\
An example of the use of typecast on a little-endian machine is\n\
\n\
@example\n\
@group\n\
@var{x} = uint16 ([1, 65535]);\n\
typecast (@var{x}, \"uint8\")\n\
@result{} [   1,   0, 255, 255]\n\
@end group\n\
@end example\n\
@seealso{cast, bitpack, bitunpack, swapbytes}\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 2)
    {
      unwind_protect frame;
      const void *data = 0;
      octave_idx_type byte_size = 0;
      dim_vector old_dims;

      octave_value array = args(0);

      if (array.is_bool_type ())
        get_data_and_bytesize (array.bool_array_value (), data, byte_size,
                               old_dims, frame);
      else if (array.is_string ())
        get_data_and_bytesize (array.char_array_value (), data, byte_size,
                               old_dims, frame);
      else if (array.is_integer_type ())
        {
          if (array.is_int8_type ())
            get_data_and_bytesize (array.int8_array_value (), data, byte_size,
                                   old_dims, frame);
          else if (array.is_int16_type ())
            get_data_and_bytesize (array.int16_array_value (), data, byte_size,
                                   old_dims, frame);
          else if (array.is_int32_type ())
            get_data_and_bytesize (array.int32_array_value (), data, byte_size,
                                   old_dims, frame);
          else if (array.is_int64_type ())
            get_data_and_bytesize (array.int64_array_value (), data, byte_size,
                                   old_dims, frame);
          else if (array.is_uint8_type ())
            get_data_and_bytesize (array.uint8_array_value (), data, byte_size,
                                   old_dims, frame);
          else if (array.is_uint16_type ())
            get_data_and_bytesize (array.uint16_array_value (), data, byte_size,
                                   old_dims, frame);
          else if (array.is_uint32_type ())
            get_data_and_bytesize (array.uint32_array_value (), data, byte_size,
                                   old_dims, frame);
          else if (array.is_uint64_type ())
            get_data_and_bytesize (array.uint64_array_value (), data, byte_size,
                                   old_dims, frame);
          else
            assert (0);
        }
      else if (array.is_complex_type ())
        {
          if (array.is_single_type ())
            get_data_and_bytesize (array.float_complex_array_value (), data,
                                   byte_size, old_dims, frame);
          else
            get_data_and_bytesize (array.complex_array_value (), data,
                                   byte_size, old_dims, frame);
        }
      else if (array.is_real_type ())
        {
          if (array.is_single_type ())
            get_data_and_bytesize (array.float_array_value (), data, byte_size,
                                   old_dims, frame);
          else
            get_data_and_bytesize (array.array_value (), data, byte_size,
                                   old_dims, frame); }
      else
        error ("typecast: invalid input class: %s",
                                                array.class_name ().c_str ());

      std::string numclass = args(1).string_value ();

      if (error_state || numclass.size () == 0)
        ;
      else if (numclass == "char")
        retval = octave_value (reinterpret_copy<charNDArray>
                   (data, byte_size, old_dims), array.is_dq_string () ? '"'
                                                                      : '\'');
      else if (numclass[0] == 'i')
        {
          if (numclass == "int8")
            retval = reinterpret_copy<int8NDArray> (data, byte_size, old_dims);
          else if (numclass == "int16")
            retval = reinterpret_copy<int16NDArray> (data, byte_size, old_dims);
          else if (numclass == "int32")
            retval = reinterpret_copy<int32NDArray> (data, byte_size, old_dims);
          else if (numclass == "int64")
            retval = reinterpret_copy<int64NDArray> (data, byte_size, old_dims);
        }
      else if (numclass[0] == 'u')
        {
          if (numclass == "uint8")
            retval = reinterpret_copy<uint8NDArray> (data, byte_size, old_dims);
          else if (numclass == "uint16")
            retval = reinterpret_copy<uint16NDArray> (data, byte_size,
                                                      old_dims);
          else if (numclass == "uint32")
            retval = reinterpret_copy<uint32NDArray> (data, byte_size,
                                                      old_dims);
          else if (numclass == "uint64")
            retval = reinterpret_copy<uint64NDArray> (data, byte_size,
                                                      old_dims);
        }
      else if (numclass == "single")
        retval = reinterpret_copy<FloatNDArray> (data, byte_size, old_dims);
      else if (numclass == "double")
        retval = reinterpret_copy<NDArray> (data, byte_size, old_dims);
      else if (numclass == "single complex")
        retval = reinterpret_copy<FloatComplexNDArray> (data, byte_size,
                                                        old_dims);
      else if (numclass == "double complex")
        retval = reinterpret_copy<ComplexNDArray> (data, byte_size, old_dims);

      if (! error_state && retval.is_undefined ())
        error ("typecast: cannot convert to %s class", numclass.c_str ());
    }
  else
    print_usage ();

  return retval;
}

template <class ArrayType>
ArrayType
do_bitpack (const boolNDArray& bitp)
{
  typedef typename ArrayType::element_type T;
  octave_idx_type n
    = bitp.numel () / (sizeof (T) * std::numeric_limits<unsigned char>::digits);

  if (n * static_cast<int> (sizeof (T)) * std::numeric_limits<unsigned char>::digits == bitp.numel ())
    {

      ArrayType retval (get_vec_dims (bitp.dims (), n));

      const bool *bits = bitp.fortran_vec ();
      char *packed = reinterpret_cast<char *> (retval.fortran_vec ());

      octave_idx_type m = n * sizeof (T);

      for (octave_idx_type i = 0; i < m; i++)
        {
          char c = bits[0];
          for (int j = 1; j < std::numeric_limits<unsigned char>::digits; j++)
            c |= bits[j] << j;

          packed[i] = c;
          bits += std::numeric_limits<unsigned char>::digits;
        }

      return retval;
    }
  else
    {
      error ("bitpack: incorrect number of bits to make up output value");
      return ArrayType ();
    }
}

DEFUN (bitpack, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {@var{y} =} bitpack (@var{x}, @var{class})\n\
Return a new array @var{y} resulting from interpreting the logical array\n\
@var{x} as raw bit patterns for data of the numeric class @var{class}.\n\
\n\
@var{class} must be one of the built-in numeric classes:\n\
\n\
@example\n\
@group\n\
\"double\"\n\
\"single\"\n\
\"double complex\"\n\
\"single complex\"\n\
\"char\"\n\
\"int8\"\n\
\"int16\"\n\
\"int32\"\n\
\"int64\"\n\
\"uint8\"\n\
\"uint16\"\n\
\"uint32\"\n\
\"uint64\"\n\
@end group\n\
@end example\n\
\n\
The number of elements of @var{x} should be divisible by the bit length of\n\
@var{class}.  If it is not, excess bits are discarded.  Bits come in\n\
increasing order of significance, i.e., @code{x(1)} is bit 0, @code{x(2)} is\n\
bit 1, etc.\n\
\n\
The result is a row vector if @var{x} is a row vector, otherwise it is a\n\
column vector.\n\
@seealso{bitunpack, typecast}\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () != 2)
    print_usage ();
  else if (! args(0).is_bool_type ())
    error ("bitpack: X must be a logical array");
  else
    {
      boolNDArray bitp = args(0).bool_array_value ();

      std::string numclass = args(1).string_value ();

      if (error_state || numclass.size () == 0)
        ;
      else if (numclass == "char")
        retval = octave_value (do_bitpack<charNDArray> (bitp), '\'');
      else if (numclass[0] == 'i')
        {
          if (numclass == "int8")
            retval = do_bitpack<int8NDArray> (bitp);
          else if (numclass == "int16")
            retval = do_bitpack<int16NDArray> (bitp);
          else if (numclass == "int32")
            retval = do_bitpack<int32NDArray> (bitp);
          else if (numclass == "int64")
            retval = do_bitpack<int64NDArray> (bitp);
        }
      else if (numclass[0] == 'u')
        {
          if (numclass == "uint8")
            retval = do_bitpack<uint8NDArray> (bitp);
          else if (numclass == "uint16")
            retval = do_bitpack<uint16NDArray> (bitp);
          else if (numclass == "uint32")
            retval = do_bitpack<uint32NDArray> (bitp);
          else if (numclass == "uint64")
            retval = do_bitpack<uint64NDArray> (bitp);
        }
      else if (numclass == "single")
        retval = do_bitpack<FloatNDArray> (bitp);
      else if (numclass == "double")
        retval = do_bitpack<NDArray> (bitp);
      else if (numclass == "single complex")
        retval = do_bitpack<FloatComplexNDArray> (bitp);
      else if (numclass == "double complex")
        retval = do_bitpack<ComplexNDArray> (bitp);

      if (! error_state && retval.is_undefined ())
        error ("bitpack: cannot pack to %s class", numclass.c_str ());
    }

  return retval;
}

template <class ArrayType>
boolNDArray
do_bitunpack (const ArrayType& array)
{
  typedef typename ArrayType::element_type T;
  octave_idx_type n = array.numel () * sizeof (T)
                      * std::numeric_limits<unsigned char>::digits;

  boolNDArray retval (get_vec_dims (array.dims (), n));

  const char *packed = reinterpret_cast<const char *> (array.fortran_vec ());
  bool *bits = retval.fortran_vec ();

  octave_idx_type m = n / std::numeric_limits<unsigned char>::digits;

  for (octave_idx_type i = 0; i < m; i++)
    {
      char c = packed[i];
      bits[0] = c & 1;
      for (int j = 1; j < std::numeric_limits<unsigned char>::digits; j++)
        bits[j] = (c >>= 1) & 1;
      bits += std::numeric_limits<unsigned char>::digits;
    }

  return retval;
}

DEFUN (bitunpack, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {@var{y} =} bitunpack (@var{x})\n\
Return a logical array @var{y} corresponding to the raw bit patterns of\n\
@var{x}.\n\
\n\
@var{x} must belong to one of the built-in numeric classes:\n\
\n\
@example\n\
@group\n\
\"double\"\n\
\"single\"\n\
\"char\"\n\
\"int8\"\n\
\"int16\"\n\
\"int32\"\n\
\"int64\"\n\
\"uint8\"\n\
\"uint16\"\n\
\"uint32\"\n\
\"uint64\"\n\
@end group\n\
@end example\n\
\n\
The result is a row vector if @var{x} is a row vector; otherwise, it is a\n\
column vector.\n\
@seealso{bitpack, typecast}\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 1
      && (args(0).is_numeric_type () || args(0).is_string ()))
    {
      octave_value array = args(0);

      if (array.is_string ())
        retval = do_bitunpack (array.char_array_value ());
      else if (array.is_integer_type ())
        {
          if (array.is_int8_type ())
            retval = do_bitunpack (array.int8_array_value ());
          else if (array.is_int16_type ())
            retval = do_bitunpack (array.int16_array_value ());
          else if (array.is_int32_type ())
            retval = do_bitunpack (array.int32_array_value ());
          else if (array.is_int64_type ())
            retval = do_bitunpack (array.int64_array_value ());
          else if (array.is_uint8_type ())
            retval = do_bitunpack (array.uint8_array_value ());
          else if (array.is_uint16_type ())
            retval = do_bitunpack (array.uint16_array_value ());
          else if (array.is_uint32_type ())
            retval = do_bitunpack (array.uint32_array_value ());
          else if (array.is_uint64_type ())
            retval = do_bitunpack (array.uint64_array_value ());
          else
            assert (0);
        }
      else if (array.is_complex_type ())
        {
          if (array.is_single_type ())
            retval = do_bitunpack (array.float_complex_array_value ());
          else
            retval = do_bitunpack (array.complex_array_value ());
        }
      else if (array.is_real_type ())
        {
          if (array.is_single_type ())
            retval = do_bitunpack (array.float_array_value ());
          else
            retval = do_bitunpack (array.array_value ());
        }
      else
        error ("bitunpack: invalid input class: %s",
                                                 array.class_name ().c_str ());
    }
  else
    print_usage ();

  return retval;
}
