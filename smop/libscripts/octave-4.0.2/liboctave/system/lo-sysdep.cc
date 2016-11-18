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

#include <iostream>
#include <string>
#include <vector>

#include <sys/types.h>
#include <unistd.h>

#include <fcntl.h>

#if defined (__WIN32__) && ! defined (__CYGWIN__)
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#endif

#include "file-ops.h"
#include "lo-error.h"
#include "pathlen.h"
#include "lo-sysdep.h"
#include "str-vec.h"
#include "oct-locbuf.h"

std::string
octave_getcwd (void)
{
  std::string retval;

  // Using the gnulib getcwd module ensures that we have a getcwd that
  // will allocate a buffer as large as necessary if buf and size are
  // both 0.

  char *tmp = gnulib::getcwd (0, 0);

  if (tmp)
    {
      retval = tmp;
      free (tmp);
    }
  else
    (*current_liboctave_error_handler) ("unable to find current directory");

  return retval;
}

int
octave_chdir (const std::string& path_arg)
{
  std::string path = file_ops::tilde_expand (path_arg);

#if defined (__WIN32__) && ! defined (__CYGWIN__)
  if (path.length () == 2 && path[1] == ':')
    path += "\\";
#endif

  return gnulib::chdir (path.c_str ());
}

#if defined (__WIN32__) && ! defined (__CYGWIN__)

pid_t
octave_popen2 (const std::string& cmd, const string_vector& args,
               bool sync_mode,
               int *fildes, std::string& msg)
{
  pid_t pid;
  PROCESS_INFORMATION pi;
  STARTUPINFO si;
  std::string command = "\"" + cmd + "\"";
  HANDLE hProcess = GetCurrentProcess ();
  HANDLE childRead, childWrite, parentRead, parentWrite;
  DWORD pipeMode;

  ZeroMemory (&pi, sizeof (pi));
  ZeroMemory (&si, sizeof (si));
  si.cb = sizeof (si);

  if (! CreatePipe (&childRead, &parentWrite, 0, 0)
      || ! DuplicateHandle (hProcess, childRead, hProcess, &childRead,
                            0, TRUE,
                            DUPLICATE_SAME_ACCESS | DUPLICATE_CLOSE_SOURCE))
    {
      msg = "popen2: pipe creation failed";
      return -1;
    }
  if (! CreatePipe (&parentRead, &childWrite, 0, 0)
      || ! DuplicateHandle (hProcess, childWrite, hProcess, &childWrite,
                            0, TRUE,
                            DUPLICATE_SAME_ACCESS | DUPLICATE_CLOSE_SOURCE))
    {
      msg = "popen2: pipe creation failed";
      return -1;
    }
  if (! sync_mode)
    {
      pipeMode = PIPE_NOWAIT;
      SetNamedPipeHandleState (parentRead, &pipeMode, 0, 0);
    }
  fildes[1] = _open_osfhandle (reinterpret_cast<intptr_t> (parentRead),
                               _O_RDONLY | _O_BINARY);
  fildes[0] = _open_osfhandle (reinterpret_cast<intptr_t> (parentWrite),
                               _O_WRONLY | _O_BINARY);
  si.dwFlags |= STARTF_USESTDHANDLES;
  si.hStdInput = childRead;
  si.hStdOutput = childWrite;

  // Ignore first arg as it is the command
  for (int k=1; k<args.length (); k++)
    command += " \"" + args[k] + "\"";
  OCTAVE_LOCAL_BUFFER (char, c_command, command.length () + 1);
  strcpy (c_command, command.c_str ());
  if (! CreateProcess (0, c_command, 0, 0, TRUE, 0, 0, 0, &si, &pi))
    {
      msg = "popen2: process creation failed";
      return -1;
    }
  pid = pi.dwProcessId;

  CloseHandle (childRead);
  CloseHandle (childWrite);
  CloseHandle (pi.hProcess);
  CloseHandle (pi.hThread);

  return pid;
}

#endif
