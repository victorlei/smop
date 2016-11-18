/*

Copyright (C) 2002-2015 John W. Eaton

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

#include <cstring>

#include <iostream>
#include <new>

#include "quit.h"

void (*octave_signal_hook) (void) = 0;
void (*octave_interrupt_hook) (void) = 0;
void (*octave_bad_alloc_hook) (void) = 0;

void
octave_handle_signal (void)
{
  if (octave_signal_hook)
    octave_signal_hook ();

  if (octave_interrupt_state > 0)
    {
      octave_interrupt_state = -1;
      octave_throw_interrupt_exception ();
    }
}

void
octave_throw_interrupt_exception (void)
{
  if (octave_interrupt_hook)
    octave_interrupt_hook ();

  throw octave_interrupt_exception ();
}

void
octave_throw_execution_exception (void)
{
  // FIXME: would a hook function be useful here?

  octave_exception_state = octave_exec_exception;

  throw octave_execution_exception ();
}

void
octave_throw_bad_alloc (void)
{
  if (octave_bad_alloc_hook)
    octave_bad_alloc_hook ();

  octave_exception_state = octave_alloc_exception;

  throw std::bad_alloc ();
}

void
octave_rethrow_exception (void)
{
  if (octave_interrupt_state)
    {
      octave_interrupt_state = -1;
      octave_throw_interrupt_exception ();
    }
  else
    {
      switch (octave_exception_state)
        {
        case octave_exec_exception:
          octave_throw_execution_exception ();
          break;

        case octave_alloc_exception:
          octave_throw_bad_alloc ();
          break;

        default:
          break;
        }
    }
}
