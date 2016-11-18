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

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

#include "lo-error.h"

/* Having this file in this directory is a kluge to avoid unresolved
   symbol errors when creating shared versions of libcruft. */

/* Pointer to the current error handling function. */
liboctave_error_handler current_liboctave_error_handler
  = liboctave_fatal;

/* Pointer to the current error_with_id handling function. */
liboctave_error_with_id_handler current_liboctave_error_with_id_handler
  = liboctave_fatal_with_id;

/* Pointer to the current warning handler. */
liboctave_warning_handler current_liboctave_warning_handler
  = liboctave_warning;

/* Pointer to the current warning_with_id handler. */
liboctave_warning_with_id_handler current_liboctave_warning_with_id_handler
  = liboctave_warning_with_id;

static void
verror (const char *name, const char *fmt, va_list args)
{
  if (name)
    fprintf (stderr, "%s: ", name);

  vfprintf (stderr, fmt, args);
  fprintf (stderr, "\n");
  fflush (stderr);
}

void
set_liboctave_error_handler (liboctave_error_handler f)
{
  if (f)
    current_liboctave_error_handler = f;
  else
    current_liboctave_error_handler = liboctave_fatal;
}

void
set_liboctave_error_with_id_handler (liboctave_error_with_id_handler f)
{
  if (f)
    current_liboctave_error_with_id_handler = f;
  else
    current_liboctave_error_with_id_handler = liboctave_fatal_with_id;
}

void
set_liboctave_warning_handler (liboctave_warning_handler f)
{
  if (f)
    current_liboctave_warning_handler = f;
  else
    current_liboctave_warning_handler = liboctave_warning;
}

void
set_liboctave_warning_with_id_handler (liboctave_warning_with_id_handler f)
{
  if (f)
    current_liboctave_warning_with_id_handler = f;
  else
    current_liboctave_warning_with_id_handler = liboctave_warning_with_id;
}

void
liboctave_fatal (const char *fmt, ...)
{
  va_list args;
  va_start (args, fmt);
  verror ("fatal", fmt, args);
  va_end (args);

  exit (1);
}

void
liboctave_fatal_with_id (const char *id, const char *fmt, ...)
{
  va_list args;
  (void) id; /*unused*/
  va_start (args, fmt);
  verror ("fatal", fmt, args);
  va_end (args);

  exit (1);
}

void
liboctave_warning (const char *fmt, ...)
{
  va_list args;
  va_start (args, fmt);
  verror ("warning", fmt, args);
  va_end (args);
}

void
liboctave_warning_with_id (const char *id, const char *fmt, ...)
{
  va_list args;
  (void) id; /*unused*/
  va_start (args, fmt);
  verror ("warning", fmt, args);
  va_end (args);
}
