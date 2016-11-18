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

// This class is based on the procbuf class from libg++, written by
// Per Bothner, Copyright (C) 1993 Free Software Foundation.

#if !defined (octave_oct_procbuf_h)
#define octave_oct_procbuf_h 1

#include <sys/types.h>

#include "c-file-ptr-stream.h"

class
octave_procbuf : public c_file_ptr_buf
{
public:

  octave_procbuf (void)
    : c_file_ptr_buf (0), wstatus (-1), open_p (false), proc_pid (-1),
      next (0) { }

  octave_procbuf (const char *command, int mode)
    : c_file_ptr_buf (0), wstatus (-1), open_p (false), proc_pid (-1),
      next (0) { open (command, mode); }

  ~octave_procbuf (void) { close (); }

  octave_procbuf *open (const char *command, int mode);

  octave_procbuf *close (void);

  int wait_status (void) const { return wstatus; }

  bool is_open (void) const { return open_p; }

  pid_t pid (void) const { return proc_pid; }

protected:

  int wstatus;

  bool open_p;

  pid_t proc_pid;

  octave_procbuf *next;

private:

  // No copying!

  octave_procbuf (const octave_procbuf&);

  octave_procbuf& operator = (const octave_procbuf&);
};

extern void symbols_of_oct_procbuf (void);

#endif
