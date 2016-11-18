/*

Copyright (C) 2008-2015 Michael Goffioul

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

#include "oct-mutex.h"
#include "lo-error.h"

#if defined (__WIN32__) && ! defined (__CYGWIN__)
#include <windows.h>
#elif defined (HAVE_PTHREAD_H)
#include <pthread.h>
#endif

void
octave_base_mutex::lock (void)
{
  (*current_liboctave_error_handler) ("mutex not supported on this platform");
}

void
octave_base_mutex::unlock (void)
{
  (*current_liboctave_error_handler) ("mutex not supported on this platform");
}

bool
octave_base_mutex::try_lock (void)
{
  (*current_liboctave_error_handler) ("mutex not supported on this platform");

  return false;
}

#if defined (__WIN32__) && ! defined (__CYGWIN__)

class
octave_w32_mutex : public octave_base_mutex
{
public:
  octave_w32_mutex (void)
    : octave_base_mutex ()
  {
    InitializeCriticalSection (&cs);
  }

  ~octave_w32_mutex (void)
  {
    DeleteCriticalSection (&cs);
  }

  void lock (void)
  {
    EnterCriticalSection (&cs);
  }

  void unlock (void)
  {
    LeaveCriticalSection (&cs);
  }

  bool try_lock (void)
  {
    return (TryEnterCriticalSection (&cs) != 0);
  }

private:
  CRITICAL_SECTION cs;
};

static DWORD octave_thread_id = 0;

void
octave_thread::init (void)
{
  octave_thread_id = GetCurrentThreadId ();
}

bool
octave_thread::is_octave_thread (void)
{
  return (GetCurrentThreadId () == octave_thread_id);
}

#elif defined (HAVE_PTHREAD_H)

class
octave_pthread_mutex : public octave_base_mutex
{
public:
  octave_pthread_mutex (void)
    : octave_base_mutex (), pm ()
  {
    pthread_mutexattr_t attr;

    pthread_mutexattr_init (&attr);
    pthread_mutexattr_settype (&attr, PTHREAD_MUTEX_RECURSIVE);
    pthread_mutex_init (&pm, &attr);
    pthread_mutexattr_destroy (&attr);
  }

  ~octave_pthread_mutex (void)
  {
    pthread_mutex_destroy (&pm);
  }

  void lock (void)
  {
    pthread_mutex_lock (&pm);
  }

  void unlock (void)
  {
    pthread_mutex_unlock (&pm);
  }

  bool try_lock (void)
  {
    return (pthread_mutex_trylock (&pm) == 0);
  }

private:
  pthread_mutex_t pm;
};

static pthread_t octave_thread_id = 0;

void
octave_thread::init (void)
{
  octave_thread_id = pthread_self ();
}

bool
octave_thread::is_octave_thread (void)
{
  return (pthread_equal (octave_thread_id, pthread_self ()) != 0);
}

#endif

static octave_base_mutex *
init_rep (void)
{
#if defined (__WIN32__) && ! defined (__CYGWIN__)
  return new octave_w32_mutex ();
#elif defined (HAVE_PTHREAD_H)
  return new octave_pthread_mutex ();
#else
  return new octave_base_mutex ();
#endif
}

octave_mutex::octave_mutex (void) : rep (init_rep ()) { }
