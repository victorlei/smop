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

#if !defined (octave_oct_h)
#define octave_oct_h 1

// Things that are often included to create .oct files.

// config.h needs to be first because it includes #defines that can */
// affect other header files.

#include <config.h>

#include "Matrix.h"

#include "oct-locbuf.h"
#include "defun-dld.h"
#include "error.h"
#include "gripes.h"
#include "help.h"
#include "oct-obj.h"
#include "pager.h"
#include "utils.h"
#include "variables.h"

#endif
