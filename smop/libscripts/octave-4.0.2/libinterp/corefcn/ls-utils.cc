/*

Copyright (C) 2003-2015 John W. Eaton

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

#include "data-conv.h"

#include "ls-utils.h"

// MAX_VAL and MIN_VAL are assumed to have integral values even though
// they are stored in doubles.

save_type
get_save_type (double /* max_val */, double /* min_val */)
{
  save_type st = LS_DOUBLE;

  // Matlab doesn't seem to load the UINT32 type correctly, so let's
  // avoid it (and the other unsigned types, even though they may not
  // have the same problem.  And apparently, there are problems with
  // other smaller types as well.  If we avoid them all, then maybe we
  // will avoid problems.  Unfortunately, we won't be able to save
  // space...

  //  if (max_val < 256 && min_val > -1)
  //    st = LS_U_CHAR;
  //  else if (max_val < 65536 && min_val > -1)
  //    st = LS_U_SHORT;
  //  else if (max_val < 4294967295UL && min_val > -1)
  //    st = LS_U_INT;
  //  else if (max_val < 128 && min_val >= -128)
  //    st = LS_CHAR;
  //  else if (max_val < 32768 && min_val >= -32768)
  //    st = LS_SHORT;
  //  else if (max_val <= 2147483647L && min_val >= -2147483647L)
  //    st = LS_INT;

  return st;
}

save_type
get_save_type (float /* max_val */, float /* min_val */)
{
  save_type st = LS_FLOAT;

  // Matlab doesn't seem to load the UINT32 type correctly, so let's
  // avoid it (and the other unsigned types, even though they may not
  // have the same problem.  And apparently, there are problems with
  // other smaller types as well.  If we avoid them all, then maybe we
  // will avoid problems.  Unfortunately, we won't be able to save
  // space...

  //  if (max_val < 256 && min_val > -1)
  //    st = LS_U_CHAR;
  //  else if (max_val < 65536 && min_val > -1)
  //    st = LS_U_SHORT;
  //  else if (max_val < 4294967295UL && min_val > -1)
  //    st = LS_U_INT;
  //  else if (max_val < 128 && min_val >= -128)
  //    st = LS_CHAR;
  //  else if (max_val < 32768 && min_val >= -32768)
  //    st = LS_SHORT;
  //  else if (max_val <= 2147483647L && min_val >= -2147483647L)
  //    st = LS_INT;

  return st;
}
