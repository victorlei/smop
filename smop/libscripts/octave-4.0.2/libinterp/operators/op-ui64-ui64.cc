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

#include "mx-ui64nda-i8.h"
#include "mx-ui64nda-ui8.h"
#include "mx-ui64nda-i16.h"
#include "mx-ui64nda-ui16.h"
#include "mx-ui64nda-i32.h"
#include "mx-ui64nda-ui32.h"
#include "mx-ui64nda-i64.h"

#include "mx-ui64nda-i8nda.h"
#include "mx-ui64nda-ui8nda.h"
#include "mx-ui64nda-i16nda.h"
#include "mx-ui64nda-ui16nda.h"
#include "mx-ui64nda-i32nda.h"
#include "mx-ui64nda-ui32nda.h"
#include "mx-ui64nda-i64nda.h"

#include "mx-ui64-i8nda.h"
#include "mx-ui64-ui8nda.h"
#include "mx-ui64-i16nda.h"
#include "mx-ui64-ui16nda.h"
#include "mx-ui64-i32nda.h"
#include "mx-ui64-ui32nda.h"
#include "mx-ui64-i64nda.h"

#include "mx-ui64nda-s.h"
#include "mx-s-ui64nda.h"

#include "mx-ui64nda-nda.h"
#include "mx-nda-ui64nda.h"

#include "mx-ui64-nda.h"
#include "mx-nda-ui64.h"

#include "mx-ui64nda-fs.h"
#include "mx-fs-ui64nda.h"

#include "mx-ui64nda-fnda.h"
#include "mx-fnda-ui64nda.h"

#include "mx-ui64-fnda.h"
#include "mx-fnda-ui64.h"

#include "gripes.h"
#include "oct-obj.h"
#include "ov.h"
#include "ov-int16.h"
#include "ov-int32.h"
#include "ov-int64.h"
#include "ov-int8.h"
#include "ov-uint16.h"
#include "ov-uint32.h"
#include "ov-uint64.h"
#include "ov-uint8.h"
#include "ov-scalar.h"
#include "ov-float.h"
#include "ov-complex.h"
#include "ov-flt-complex.h"
#include "ov-re-mat.h"
#include "ov-flt-re-mat.h"
#include "ov-cx-mat.h"
#include "ov-flt-cx-mat.h"
#include "ov-typeinfo.h"
#include "ov-null-mat.h"
#include "ops.h"
#include "xdiv.h"
#include "xpow.h"

#include "op-int.h"

OCTAVE_INT_OPS (uint64)

OCTAVE_MS_INT_ASSIGN_OPS (mi8, uint64_, int8_, int8_)
OCTAVE_MS_INT_ASSIGN_OPS (mui8, uint64_, uint8_, uint8_)
OCTAVE_MS_INT_ASSIGN_OPS (mi16, uint64_, int16_, int16_)
OCTAVE_MS_INT_ASSIGN_OPS (mui16, uint64_, uint16_, uint16_)
OCTAVE_MS_INT_ASSIGN_OPS (mi32, uint64_, int32_, int32_)
OCTAVE_MS_INT_ASSIGN_OPS (mui32, uint64_, uint32_, uint32_)
OCTAVE_MS_INT_ASSIGN_OPS (mi64, uint64_, int64_, int64_)

OCTAVE_MM_INT_ASSIGN_OPS (mmi8, uint64_, int8_, int8_)
OCTAVE_MM_INT_ASSIGN_OPS (mmui8, uint64_, uint8_, uint8_)
OCTAVE_MM_INT_ASSIGN_OPS (mmi16, uint64_, int16_, int16_)
OCTAVE_MM_INT_ASSIGN_OPS (mmui16, uint64_, uint16_, uint16_)
OCTAVE_MM_INT_ASSIGN_OPS (mmi32, uint64_, int32_, int32_)
OCTAVE_MM_INT_ASSIGN_OPS (mmui32, uint64_, uint32_, uint32_)
OCTAVE_MM_INT_ASSIGN_OPS (mmi64, uint64_, int64_, int64_)

OCTAVE_MIXED_INT_CMP_OPS (uint64, int8)
OCTAVE_MIXED_INT_CMP_OPS (uint64, uint8)
OCTAVE_MIXED_INT_CMP_OPS (uint64, int16)
OCTAVE_MIXED_INT_CMP_OPS (uint64, uint16)
OCTAVE_MIXED_INT_CMP_OPS (uint64, int32)
OCTAVE_MIXED_INT_CMP_OPS (uint64, uint32)
OCTAVE_MIXED_INT_CMP_OPS (uint64, int64)

void
install_ui64_ui64_ops (void)
{
  OCTAVE_INSTALL_INT_OPS (uint64);

  OCTAVE_INSTALL_MS_INT_ASSIGN_OPS (mi8, uint64_, int8_);
  OCTAVE_INSTALL_MS_INT_ASSIGN_OPS (mui8, uint64_, uint8_);
  OCTAVE_INSTALL_MS_INT_ASSIGN_OPS (mi16, uint64_, int16_);
  OCTAVE_INSTALL_MS_INT_ASSIGN_OPS (mui16, uint64_, uint16_);
  OCTAVE_INSTALL_MS_INT_ASSIGN_OPS (mi32, uint64_, int32_);
  OCTAVE_INSTALL_MS_INT_ASSIGN_OPS (mui32, uint64_, uint32_);
  OCTAVE_INSTALL_MS_INT_ASSIGN_OPS (mi64, uint64_, int64_);

  OCTAVE_INSTALL_MM_INT_ASSIGN_OPS (mmi8, uint64_, int8_);
  OCTAVE_INSTALL_MM_INT_ASSIGN_OPS (mmui8, uint64_, uint8_);
  OCTAVE_INSTALL_MM_INT_ASSIGN_OPS (mmi16, uint64_, int16_);
  OCTAVE_INSTALL_MM_INT_ASSIGN_OPS (mmui16, uint64_, uint16_);
  OCTAVE_INSTALL_MM_INT_ASSIGN_OPS (mmi32, uint64_, int32_);
  OCTAVE_INSTALL_MM_INT_ASSIGN_OPS (mmui32, uint64_, uint32_);
  OCTAVE_INSTALL_MM_INT_ASSIGN_OPS (mmi64, uint64_, int64_);

  OCTAVE_INSTALL_SM_INT_ASSIGNCONV (uint64, int8);
  OCTAVE_INSTALL_SM_INT_ASSIGNCONV (uint64, uint8);
  OCTAVE_INSTALL_SM_INT_ASSIGNCONV (uint64, int16);
  OCTAVE_INSTALL_SM_INT_ASSIGNCONV (uint64, uint16);
  OCTAVE_INSTALL_SM_INT_ASSIGNCONV (uint64, int32);
  OCTAVE_INSTALL_SM_INT_ASSIGNCONV (uint64, uint32);
  OCTAVE_INSTALL_SM_INT_ASSIGNCONV (uint64, int64);

  OCTAVE_INSTALL_MIXED_INT_CMP_OPS (uint64, int8);
  OCTAVE_INSTALL_MIXED_INT_CMP_OPS (uint64, uint8);
  OCTAVE_INSTALL_MIXED_INT_CMP_OPS (uint64, int16);
  OCTAVE_INSTALL_MIXED_INT_CMP_OPS (uint64, uint16);
  OCTAVE_INSTALL_MIXED_INT_CMP_OPS (uint64, int32);
  OCTAVE_INSTALL_MIXED_INT_CMP_OPS (uint64, uint32);
  OCTAVE_INSTALL_MIXED_INT_CMP_OPS (uint64, int64);
}
