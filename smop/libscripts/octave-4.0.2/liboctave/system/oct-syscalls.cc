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
#include <cstdlib>

#include <string.h>

#include <sys/types.h>
#include <unistd.h>

#include <fcntl.h>

// We can't use csignal as kill is not in the std namespace, and picky
// compiler runtimes will also exclude it from global scope as well.

#include <signal.h>

#include "lo-utils.h"
#include "lo-sysdep.h"
#include "oct-syscalls.h"
#include "str-vec.h"

#define NOT_SUPPORTED(nm) \
  nm ": not supported on this system"

int
octave_syscalls::dup2 (int old_fd, int new_fd)
{
  std::string msg;
  return dup2 (old_fd, new_fd, msg);
}

int
octave_syscalls::dup2 (int old_fd, int new_fd, std::string& msg)
{
  msg = std::string ();

  int status = -1;

#if defined (HAVE_DUP2)
  status = gnulib::dup2 (old_fd, new_fd);

  if (status < 0)
    msg = gnulib::strerror (errno);
#else
  msg = NOT_SUPPORTED ("dup2");
#endif

  return status;
}

int
octave_syscalls::execvp (const std::string& file, const string_vector& argv)
{
  std::string msg;
  return execvp (file, argv, msg);
}

int
octave_syscalls::execvp (const std::string& file, const string_vector& args,
                         std::string& msg)
{
  msg = std::string ();

  int status = -1;

#if defined (HAVE_EXECVP)
  char **argv = args.c_str_vec ();

  status = ::execvp (file.c_str (), argv);

  string_vector::delete_c_str_vec (argv);

  if (status < 0)
    msg = gnulib::strerror (errno);
#else
  msg = NOT_SUPPORTED ("execvp");
#endif

  return status;
}

pid_t
octave_syscalls::fork (std::string& msg)
{
  pid_t status = -1;

#if defined (HAVE_FORK)
  status = ::fork ();

  if (status < 0)
    msg = gnulib::strerror (errno);
#else
  msg = NOT_SUPPORTED ("fork");
#endif

  return status;
}

pid_t
octave_syscalls::vfork (std::string& msg)
{
  pid_t status = -1;

#if defined (HAVE_VFORK) || defined (HAVE_FORK)
#if defined (HAVE_VFORK)
  status = ::vfork ();
#else
  status = ::fork ();
#endif

  if (status < 0)
    msg = gnulib::strerror (errno);
#else
  msg = NOT_SUPPORTED ("vfork");
#endif

  return status;
}

pid_t
octave_syscalls::getpgrp (std::string& msg)
{
  pid_t status = -1;

#if defined (HAVE_GETPGRP)
  status = ::getpgrp ();

  if (status < 0)
    msg = gnulib::strerror (errno);
#else
  msg = NOT_SUPPORTED ("getpgrp");
#endif

  return status;
}

pid_t
octave_syscalls::getpid (void)
{
#if defined (HAVE_GETPID)
  return ::getpid ();
#else
  return 0;
#endif
}

pid_t
octave_syscalls::getppid (void)
{
#if defined (HAVE_GETPPID)
  return ::getppid ();
#else
  return 0;
#endif
}

gid_t
octave_syscalls::getgid (void)
{
#if defined (HAVE_GETGID)
  return ::getgid ();
#else
  return 0;
#endif
}

gid_t
octave_syscalls::getegid (void)
{
#if defined (HAVE_GETEGID)
  return ::getegid ();
#else
  return 0;
#endif
}

uid_t
octave_syscalls::getuid (void)
{
#if defined (HAVE_GETUID)
  return ::getuid ();
#else
  return 0;
#endif
}

uid_t
octave_syscalls::geteuid (void)
{
#if defined (HAVE_GETEUID)
  return ::geteuid ();
#else
  return 0;
#endif
}

int
octave_syscalls::pipe (int *fildes)
{
  std::string msg;
  return pipe (fildes, msg);
}

int
octave_syscalls::pipe (int *fildes, std::string& msg)
{
  msg = std::string ();

  int status = -1;

  status = gnulib::pipe (fildes);

  if (status < 0)
    msg = gnulib::strerror (errno);

  return status;
}

pid_t
octave_syscalls::waitpid (pid_t pid, int *status, int options)
{
  std::string msg;
  return waitpid (pid, status, options, msg);
}

pid_t
octave_syscalls::waitpid (pid_t pid, int *status, int options,
                          std::string& msg)
{
  pid_t retval = -1;
  msg = std::string ();

#if defined (HAVE_WAITPID)
  retval = ::octave_waitpid (pid, status, options);

  if (retval < 0)
    msg = gnulib::strerror (errno);
#else
  msg = NOT_SUPPORTED ("waitpid");
#endif

  return retval;
}

int
octave_syscalls::kill (pid_t pid, int sig)
{
  std::string msg;
  return kill (pid, sig, msg);
}

int
octave_syscalls::kill (pid_t pid, int sig, std::string& msg)
{
  msg = std::string ();

  int status = -1;

#if defined (HAVE_KILL)
  status = ::kill (pid, sig);

  if (status < 0)
    msg = gnulib::strerror (errno);
#else
  msg = NOT_SUPPORTED ("kill");
#endif

  return status;
}

pid_t
octave_syscalls::popen2 (const std::string& cmd, const string_vector& args,
                         bool sync_mode, int *fildes)
{
  std::string msg;
  bool interactive = false;
  return popen2 (cmd, args, sync_mode, fildes, msg, interactive);
}

pid_t
octave_syscalls::popen2 (const std::string& cmd, const string_vector& args,
                         bool sync_mode, int *fildes, std::string& msg)
{
  bool interactive = false;
  return popen2 (cmd, args, sync_mode, fildes, msg, interactive);
}

pid_t
octave_syscalls::popen2 (const std::string& cmd, const string_vector& args,
                         bool sync_mode, int *fildes, std::string& msg,
                         bool &interactive)
{
#if defined (__WIN32__) && ! defined (__CYGWIN__)
  return ::octave_popen2 (cmd, args, sync_mode, fildes, msg);
#else
  pid_t pid;
  int child_stdin[2], child_stdout[2];

  if (pipe (child_stdin, msg) == 0)
    {
      if (pipe (child_stdout, msg) == 0)
        {
          pid = fork (msg);
          if (pid < 0)
            msg = "popen2: process creation failed -- " + msg;
          else if (pid == 0)
            {
              std::string child_msg;

              interactive = false;

              // Child process
              gnulib::close (child_stdin[1]);
              gnulib::close (child_stdout[0]);

              if (dup2 (child_stdin[0], STDIN_FILENO) >= 0)
                {
                  gnulib::close (child_stdin[0]);
                  if (dup2 (child_stdout[1], STDOUT_FILENO) >= 0)
                    {
                      gnulib::close (child_stdout[1]);
                      if (execvp (cmd, args, child_msg) < 0)
                        child_msg = "popen2 (child): unable to start process -- " + child_msg;
                    }
                  else
                    child_msg = "popen2 (child): file handle duplication failed -- " + child_msg;
                }
              else
                child_msg = "popen2 (child): file handle duplication failed -- " + child_msg;

              (*current_liboctave_error_handler)(child_msg.c_str ());

              exit (0);
            }
          else
            {
              // Parent process
              gnulib::close (child_stdin[0]);
              gnulib::close (child_stdout[1]);

#if defined (F_SETFL) && defined (O_NONBLOCK)
              if (! sync_mode && octave_fcntl (child_stdout[0], F_SETFL,
                                               O_NONBLOCK, msg) < 0)
                msg = "popen2: error setting file mode -- " + msg;
              else
#endif
                {
                  fildes[0] = child_stdin[1];
                  fildes[1] = child_stdout[0];
                  return pid;
                }
            }
          gnulib::close (child_stdout[0]);
          gnulib::close (child_stdout[1]);
        }
      else
        msg = "popen2: pipe creation failed -- " + msg;

      gnulib::close (child_stdin[0]);
      gnulib::close (child_stdin[1]);
    }
  else
    msg = "popen2: pipe creation failed -- " + msg;

  return -1;
#endif
}

int
octave_fcntl (int fd, int cmd, long arg)
{
  std::string msg;
  return octave_fcntl (fd, cmd, arg, msg);
}

int
octave_fcntl (int fd, int cmd, long arg, std::string& msg)
{
  msg = std::string ();

  int status = -1;

  status = gnulib::fcntl (fd, cmd, arg);

  if (status < 0)
    msg = gnulib::strerror (errno);

  return status;
}
