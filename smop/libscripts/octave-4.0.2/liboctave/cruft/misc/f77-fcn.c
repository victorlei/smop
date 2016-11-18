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

#include <stdlib.h>
#include <string.h>

#include "f77-fcn.h"
#include "quit.h"
#include "lo-error.h"

/* All the STOP statements in the Fortran routines have been replaced
   with a call to XSTOPX.

   XSTOPX jumps back to the entry point for the Fortran function that
   called us.  Then the calling function should do whatever cleanup
   is necessary.  */

F77_RET_T
#if defined (F77_USES_CRAY_CALLING_CONVENTION)
F77_FUNC (xstopx, XSTOPX) (octave_cray_ftn_ch_dsc desc)
#elif defined (F77_USES_VISUAL_FORTRAN_CALLING_CONVENTION)
F77_FUNC (xstopx, XSTOPX) (const char *s, int slen)
#else
F77_FUNC (xstopx, XSTOPX) (const char *s, long slen)
#endif
{
#if defined (F77_USES_CRAY_CALLING_CONVENTION)
  const char *s = desc.const_ptr = ptr_arg;
  unsigned long slen = desc.mask.len;
#endif

  f77_exception_encountered = 1;

  /* Skip printing message if it is just a single blank character.  */
  if (s && slen > 0 && ! (slen == 1 && *s == ' '))
    (*current_liboctave_error_handler) ("%.*s", slen, s);

  octave_jump_to_enclosing_context ();

  F77_NORETURN (0)
}
