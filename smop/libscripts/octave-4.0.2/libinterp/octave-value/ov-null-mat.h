/*

Copyright (C) 2008-2015 Jaroslav Hajek

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

#if !defined (octave_ov_null_mat_h)
#define octave_ov_null_mat_h 1

#include "ov.h"
#include "ov-re-mat.h"
#include "ov-str-mat.h"

// Design rationale:
// The constructors are hidden.  There is only one null matrix (or null string)
// object, that can have shallow copies.  Cloning the object returns just a
// normal empty matrix, so all the shallow copies are, in fact, read-only. This
// conveniently ensures that any attempt to fiddle with the null matrix
// destroys its special status.

// The special [] value.

class
OCTINTERP_API
octave_null_matrix : public octave_matrix
{
  octave_null_matrix (void) : octave_matrix () { }

public:

  static const octave_value instance;

  bool is_null_value (void) const { return true; }

  type_conv_info numeric_conversion_function (void) const;

private:

  DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA
};

// The special "" value

class
OCTINTERP_API
octave_null_str : public octave_char_matrix_str
{
  octave_null_str (void) : octave_char_matrix_str () { }

public:

  static const octave_value instance;

  bool is_null_value (void) const { return true; }

  type_conv_info numeric_conversion_function (void) const;


private:

  DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA
};

// The special '' value

class
OCTINTERP_API
octave_null_sq_str : public octave_char_matrix_sq_str
{
  octave_null_sq_str (void) : octave_char_matrix_sq_str () { }

public:

  static const octave_value instance;

  bool is_null_value (void) const { return true; }

  type_conv_info numeric_conversion_function (void) const;

private:

  DECLARE_OV_TYPEID_FUNCTIONS_AND_DATA
};

#endif
