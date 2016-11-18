/*

Copyright (C) 1993-2015 John W. Eaton

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

/*

The signal blocking macros defined below were adapted from similar
functions from GNU Bash, the Bourne Again SHell, copyright (C) 1994
Free Software Foundation, Inc.

*/

// This file should always be included after config.h!

#if !defined (octave_sighandlers_h)
#define octave_sighandlers_h 1

// Include signal.h, not csignal since the latter might only define
// the ANSI standard C signal interface.

#include <signal.h>

#include "syswait.h"
#include "siglist.h"

#include "base-list.h"

typedef void sig_handler (int);

// FIXME: the data should probably be private...

struct
octave_interrupt_handler
{
#ifdef SIGINT
  sig_handler *int_handler;
#endif

#ifdef SIGBREAK
  sig_handler *brk_handler;
#endif
};

// Nonzero means we have already printed a message for this series of
// SIGPIPES.  We assume that the writer will eventually give up.
extern int pipe_handler_error_count;

// TRUE means we can be interrupted.
extern OCTINTERP_API bool can_interrupt;

extern OCTINTERP_API
sig_handler *octave_set_signal_handler (int, sig_handler *,
                                        bool restart_syscalls = true);

extern OCTINTERP_API void install_signal_handlers (void);

extern OCTINTERP_API void octave_signal_handler (void);

extern OCTINTERP_API octave_interrupt_handler octave_catch_interrupts (void);

extern OCTINTERP_API octave_interrupt_handler octave_ignore_interrupts (void);

extern OCTINTERP_API octave_interrupt_handler
octave_set_interrupt_handler (const volatile octave_interrupt_handler&,
                              bool restart_syscalls = true);

#if defined (__WIN32__) && ! defined (__CYGWIN__)
extern OCTINTERP_API void w32_raise_sigint (void);
#endif

// extern void ignore_sigchld (void);

// Maybe this should be in a separate file?

class
OCTINTERP_API
octave_child
{
public:

  // Do whatever to handle event for child with PID (might not
  // actually be dead, could just be stopped).  Return true if
  // the list element corresponding to PID should be removed from
  // list.  This function should not call any functions that modify
  // the octave_child_list.

  typedef bool (*child_event_handler) (pid_t, int);

  octave_child (pid_t id = -1, child_event_handler f = 0)
    : pid (id), handler (f), have_status (0), status (0) { }

  octave_child (const octave_child& oc)
    : pid (oc.pid), handler (oc.handler),
      have_status (oc.have_status), status (oc.status) { }

  octave_child& operator = (const octave_child& oc)
  {
    if (&oc != this)
      {
        pid = oc.pid;
        handler = oc.handler;
        have_status = oc.have_status;
        status = oc.status;
      }
    return *this;
  }

  ~octave_child (void) { }

  // The process id of this child.
  pid_t pid;

  // The function we call if an event happens for this child.
  child_event_handler handler;

  // Nonzero if this child has stopped or terminated.
  sig_atomic_t have_status;

  // The status of this child; 0 if running, otherwise a status value
  // from waitpid.
  int status;
};

class
OCTINTERP_API
octave_child_list
{
protected:

  octave_child_list (void) { }

  class octave_child_list_rep : public octave_base_list<octave_child>
  {
  public:

    void insert (pid_t pid, octave_child::child_event_handler f);

    void reap (void);

    bool wait (void);
  };

public:

  ~octave_child_list (void) { }

  static void insert (pid_t pid, octave_child::child_event_handler f);

  static void reap (void);

  static bool wait (void);

  static void remove (pid_t pid);

private:

  static bool instance_ok (void);

  static octave_child_list_rep *instance;

  static void cleanup_instance (void) { delete instance; instance = 0; }
};

// TRUE means we should try to enter the debugger on SIGINT.
extern OCTINTERP_API bool Vdebug_on_interrupt;

#endif
