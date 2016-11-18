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

#include <cerrno>

#include <iostream>

#include <sys/types.h>
#include <unistd.h>

#include "lo-mappers.h"
#include "lo-utils.h"
#include "oct-procbuf.h"
#include "oct-syscalls.h"
#include "sysdep.h"
#include "variables.h"

#include "defun.h"
#include "gripes.h"
#include "utils.h"

#ifndef SHELL_PATH
#define SHELL_PATH "/bin/sh"
#endif

// This class is based on the procbuf class from libg++, written by
// Per Bothner, Copyright (C) 1993 Free Software Foundation.

static octave_procbuf *octave_procbuf_list = 0;

#ifndef BUFSIZ
#define BUFSIZ 1024
#endif

octave_procbuf *
octave_procbuf::open (const char *command, int mode)
{
#if defined (__CYGWIN__) || defined (__MINGW32__) || defined (_MSC_VER)

  if (is_open ())
    return 0;

  f = octave_popen (command, (mode & std::ios::in) ? "r" : "w");

  if (! f)
    return 0;

  // Oops... popen doesn't return the associated pid, so fake it for now

  proc_pid = 1;

  open_p = true;

  if (mode & std::ios::out)
    ::setvbuf (f, 0, _IOLBF, BUFSIZ);

  return this;

#elif defined (HAVE_SYS_WAIT_H)

  int pipe_fds[2];

  volatile int child_std_end = (mode & std::ios::in) ? 1 : 0;

  volatile int parent_end, child_end;

  if (is_open ())
    return 0;

  if (pipe (pipe_fds) < 0)
    return 0;

  if (mode & std::ios::in)
    {
      parent_end = pipe_fds[0];
      child_end = pipe_fds[1];
    }
  else
    {
      parent_end = pipe_fds[1];
      child_end = pipe_fds[0];
    }

  proc_pid = ::fork ();

  if (proc_pid == 0)
    {
      gnulib::close (parent_end);

      if (child_end != child_std_end)
        {
          gnulib::dup2 (child_end, child_std_end);
          gnulib::close (child_end);
        }

      while (octave_procbuf_list)
        {
          FILE *fp = octave_procbuf_list->f;

          if (fp)
            {
              gnulib::fclose (fp);
              fp = 0;
            }

          octave_procbuf_list = octave_procbuf_list->next;
        }

      execl (SHELL_PATH, "sh", "-c", command, static_cast<void *> (0));

      exit (127);
    }

  gnulib::close (child_end);

  if (proc_pid < 0)
    {
      gnulib::close (parent_end);
      return 0;
    }

  f = ::fdopen (parent_end, (mode & std::ios::in) ? "r" : "w");

  if (mode & std::ios::out)
    ::setvbuf (f, 0, _IOLBF, BUFSIZ);

  open_p = true;

  next = octave_procbuf_list;
  octave_procbuf_list = this;

  return this;

#else

  return 0;

#endif
}

octave_procbuf *
octave_procbuf::close (void)
{
#if defined (__CYGWIN__) || defined (__MINGW32__) || defined (_MSC_VER)

  if (f)
    {
      wstatus = octave_pclose (f);
      f = 0;
    }

  open_p = false;

  return this;

#elif defined (HAVE_SYS_WAIT_H)

  if (f)
    {
      pid_t wait_pid;

      int status = -1;

      for (octave_procbuf **ptr = &octave_procbuf_list;
           *ptr != 0;
           ptr = &(*ptr)->next)
        {
          if (*ptr == this)
            {
              *ptr = (*ptr)->next;
              status = 0;
              break;
            }
        }

      if (status == 0 && gnulib::fclose (f) == 0)
        {
          using namespace std;

          do
            {
              wait_pid = octave_syscalls::waitpid (proc_pid, &wstatus, 0);
            }
          while (wait_pid == -1 && errno == EINTR);
        }

      f = 0;
    }

  open_p = false;

  return this;

#else

  return 0;

#endif
}
