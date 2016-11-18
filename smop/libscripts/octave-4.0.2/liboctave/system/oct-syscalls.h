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

#if !defined (octave_oct_syscalls_h)
#define octave_oct_syscalls_h 1

#include <string>

class string_vector;

#include <sys/types.h>

struct
OCTAVE_API
octave_syscalls
{
  static int dup2 (int, int);
  static int dup2 (int, int, std::string&);

  static int execvp (const std::string&, const string_vector&);
  static int execvp (const std::string&, const string_vector&, std::string&);

  static pid_t fork (std::string&);
  static pid_t vfork (std::string&);

  static pid_t getpgrp (std::string&);

  static pid_t getpid (void);
  static pid_t getppid (void);

  static gid_t getgid (void);
  static gid_t getegid (void);

  static uid_t getuid (void);
  static uid_t geteuid (void);

  static int pipe (int *);
  static int pipe (int *, std::string&);

  static pid_t waitpid (pid_t, int *status, int);
  static pid_t waitpid (pid_t, int *status, int, std::string&);

  static int kill (pid_t, int);
  static int kill (pid_t, int, std::string&);

  static pid_t popen2 (const std::string&, const string_vector&, bool, int *);
  static pid_t popen2 (const std::string&, const string_vector&, bool, int *,
                       std::string&);
  static pid_t popen2 (const std::string&, const string_vector&, bool, int *,
                       std::string&, bool &interactive);
};

#endif

extern OCTAVE_API int octave_fcntl (int, int, long);
extern OCTAVE_API int octave_fcntl (int, int, long, std::string&);
