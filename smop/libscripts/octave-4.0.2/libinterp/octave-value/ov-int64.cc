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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <iostream>
#include <limits>

#include "lo-ieee.h"
#include "lo-utils.h"
#include "mx-base.h"
#include "quit.h"

#include "defun.h"
#include "gripes.h"
#include "oct-obj.h"
#include "oct-lvalue.h"
#include "oct-hdf5.h"
#include "ops.h"
#include "ov-base.h"

#if defined (HAVE_HDF5)
#define HDF5_SAVE_TYPE H5T_NATIVE_INT64
#endif

#include "ov-base-int.h"
#include "ov-base-int.cc"
#include "ov-int64.h"
#include "ov-type-conv.h"
#include "pr-output.h"
#include "variables.h"

#include "byte-swap.h"
#include "ls-oct-ascii.h"
#include "ls-utils.h"
#include "ls-hdf5.h"

// Prevent implicit instantiations on some systems (Windows, others?)
// that can lead to duplicate definitions of static data members.

extern template class OCTINTERP_API octave_base_scalar<double>;


template class octave_base_matrix<int64NDArray>;

template class octave_base_int_matrix<int64NDArray>;


DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_int64_matrix,
                                     "int64 matrix", "int64");

template class octave_base_scalar<octave_int64>;

template class octave_base_int_scalar<octave_int64>;


DEFINE_OV_TYPEID_FUNCTIONS_AND_DATA (octave_int64_scalar,
                                     "int64 scalar", "int64");

DEFUN (int64, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} int64 (@var{x})\n\
Convert @var{x} to 64-bit integer type.\n\
@seealso{int8, uint8, int16, uint16, int32, uint32, uint64}\n\
@end deftypefn")
{
  OCTAVE_TYPE_CONV_BODY (int64);
}

/*
%!assert (class (int64 (1)), "int64")
%!assert (int64 (1.25), int64 (1))
%!assert (int64 (1.5), int64 (2))
%!assert (int64 (-1.5), int64 (-2))
%!assert (int64 (2^65), int64 (2^64-1))
%!assert (int64 (-2^65), int64 (-2^64))
*/
