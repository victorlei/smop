/*

Copyright (C) 2004-2015 John W. Eaton

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

#if !defined (octave_ov_int_traits_h)
#define octave_ov_int_traits_h 1

#include "ov-int8.h"
#include "ov-int16.h"
#include "ov-int32.h"
#include "ov-int64.h"

#include "ov-uint8.h"
#include "ov-uint16.h"
#include "ov-uint32.h"
#include "ov-uint64.h"

template <class T>
class
octave_value_int_traits
{
public:
  typedef T scalar_type;
};

#define OCTAVE_VALUE_INT_TRAITS(MT, ST) \
  template<> \
  class \
  octave_value_int_traits<MT> \
  { \
  public: \
    typedef ST scalar_type; \
  }

OCTAVE_VALUE_INT_TRAITS(int8NDArray, octave_int8_scalar);
OCTAVE_VALUE_INT_TRAITS(int16NDArray, octave_int16_scalar);
OCTAVE_VALUE_INT_TRAITS(int32NDArray, octave_int32_scalar);
OCTAVE_VALUE_INT_TRAITS(int64NDArray, octave_int64_scalar);

OCTAVE_VALUE_INT_TRAITS(uint8NDArray, octave_uint8_scalar);
OCTAVE_VALUE_INT_TRAITS(uint16NDArray, octave_uint16_scalar);
OCTAVE_VALUE_INT_TRAITS(uint32NDArray, octave_uint32_scalar);
OCTAVE_VALUE_INT_TRAITS(uint64NDArray, octave_uint64_scalar);

#endif
