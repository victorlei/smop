/*

Copyright (C) 2013-2015 John W. Eaton

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

#if defined (__WIN32__) && ! defined (__CYGWIN__)
#include <windows.h>
#else
#include <pthread.h>
#endif

#include <sys/types.h>
#include <signal.h>

#include "sighandlers.h"
#include "thread-manager.h"

#if defined (__WIN32__) && ! defined (__CYGWIN__)

class windows_thread_manager : public octave_base_thread_manager
{
public:

  windows_thread_manager (void) : octave_base_thread_manager () { }

  void register_current_thread (void) { }

  void interrupt (void)
  {
    GenerateConsoleCtrlEvent (CTRL_C_EVENT, 0);
  }
};

#else

class pthread_thread_manager : public octave_base_thread_manager
{
public:

  pthread_thread_manager (void)
    : octave_base_thread_manager (), my_thread (), initialized (false)
  { }

  void register_current_thread (void)
  {
    my_thread = pthread_self ();
    initialized = true;
  }

  void interrupt (void)
  {
    if (initialized)
      pthread_kill (my_thread, SIGINT);
  }

private:

  pthread_t my_thread;

  bool initialized;
};

#endif

octave_thread_manager::octave_thread_manager (void)
  : rep (octave_thread_manager::create_rep ())
{ }

// The following is a workaround for an apparent bug in GCC 4.1.2 and
// possibly earlier versions.  See Octave bug report #30685 for details.
#if defined (__GNUC__)
# if ! (__GNUC__ > 4 \
        || (__GNUC__ == 4 && (__GNUC_MINOR__ > 1 \
                              || (__GNUC_MINOR__ == 1 && __GNUC_PATCHLEVEL__ > 2))))
#  undef GNULIB_NAMESPACE
#  define GNULIB_NAMESPACE
#  warning "disabling GNULIB_NAMESPACE for signal functions -- consider upgrading to a current version of GCC"
# endif
#endif

static void
block_or_unblock_signal (int how, int sig)
{
#if ! defined (__WIN32__) || defined (__CYGWIN__)
  // Blocking/unblocking signals at thread level is only supported
  // on platform with fully compliant POSIX threads. This is not
  // supported on Win32. Moreover, we have to make sure that SIGINT
  // handler is not installed before calling AllocConsole: installing
  // a SIGINT handler internally calls SetConsoleCtrlHandler, which
  // must be called after AllocConsole to be effective.

  sigset_t signal_mask;

  GNULIB_NAMESPACE::sigemptyset (&signal_mask);

  GNULIB_NAMESPACE::sigaddset (&signal_mask, sig);

  pthread_sigmask (how, &signal_mask, 0);
#endif
}

void
octave_thread_manager::block_interrupt_signal (void)
{
  block_or_unblock_signal (SIG_BLOCK, SIGINT);
}

void
octave_thread_manager::unblock_interrupt_signal (void)
{
  block_or_unblock_signal (SIG_UNBLOCK, SIGINT);
}

octave_base_thread_manager *
octave_thread_manager::create_rep (void)
{
#if defined (__WIN32__) && ! defined (__CYGWIN__)
  return new windows_thread_manager ();
#else
  return new pthread_thread_manager ();
#endif
}
