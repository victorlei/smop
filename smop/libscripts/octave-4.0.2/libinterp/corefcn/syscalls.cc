/*

Copyright (C) 1996-2015 John W. Eaton
Copyright (C) 2010 VZLU Prague

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

// Thomas Baier <baier@ci.tuwien.ac.at> added the original versions of
// the following functions:
//
//   mkfifo  unlink  waitpid

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cstdio>
#include <cstring>

#include <sys/types.h>
#include <unistd.h>

#include <fcntl.h>

#include "cmd-hist.h"
#include "file-ops.h"
#include "file-stat.h"
#include "oct-env.h"
#include "oct-syscalls.h"
#include "oct-uname.h"

#include "defun.h"
#include "error.h"
#include "gripes.h"
#include "lo-utils.h"
#include "oct-hist.h"
#include "oct-map.h"
#include "oct-obj.h"
#include "oct-stdstrm.h"
#include "oct-stream.h"
#include "sysdep.h"
#include "utils.h"
#include "variables.h"
#include "input.h"

static octave_scalar_map
mk_stat_map (const base_file_stat& fs)
{
  octave_scalar_map m;

  m.assign ("dev", static_cast<double> (fs.dev ()));
  m.assign ("ino", fs.ino ());
  m.assign ("mode", fs.mode ());
  m.assign ("modestr", fs.mode_as_string ());
  m.assign ("nlink", fs.nlink ());
  m.assign ("uid", fs.uid ());
  m.assign ("gid", fs.gid ());
#if defined (HAVE_STRUCT_STAT_ST_RDEV)
  m.assign ("rdev", static_cast<double> (fs.rdev ()));
#endif
  m.assign ("size", fs.size ());
  m.assign ("atime", fs.atime ());
  m.assign ("mtime", fs.mtime ());
  m.assign ("ctime", fs.ctime ());
#if defined (HAVE_STRUCT_STAT_ST_BLKSIZE)
  m.assign ("blksize", fs.blksize ());
#endif
#if defined (HAVE_STRUCT_STAT_ST_BLOCKS)
  m.assign ("blocks", fs.blocks ());
#endif

  return m;
}

static octave_value_list
mk_stat_result (const base_file_stat& fs)
{
  octave_value_list retval;

  if (fs)
    {
      retval(2) = std::string ();
      retval(1) = 0;
      retval(0) = octave_value (mk_stat_map (fs));
    }
  else
    {
      retval(2) = fs.error ();
      retval(1) = -1;
      retval(0) = Matrix ();
    }

  return retval;
}

DEFUNX ("dup2", Fdup2, args, ,
        "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {[@var{fid}, @var{msg}] =} dup2 (@var{old}, @var{new})\n\
Duplicate a file descriptor.\n\
\n\
If successful, @var{fid} is greater than zero and contains the new file ID@.\n\
Otherwise, @var{fid} is negative and @var{msg} contains a system-dependent\n\
error message.\n\
@seealso{fopen, fclose, fcntl}\n\
@end deftypefn")
{
  octave_value_list retval;

  retval(1) = std::string ();
  retval(0) = -1;

  int nargin = args.length ();

  if (nargin == 2)
    {
      octave_stream old_stream
        = octave_stream_list::lookup (args(0), "dup2");

      if (! error_state)
        {
          octave_stream new_stream
            = octave_stream_list::lookup (args(1), "dup2");

          if (! error_state)
            {
              int i_old = old_stream.file_number ();
              int i_new = new_stream.file_number ();

              if (i_old >= 0 && i_new >= 0)
                {
                  std::string msg;

                  int status = octave_syscalls::dup2 (i_old, i_new, msg);

                  retval(1) = msg;
                  retval(0) = status;
                }
            }
        }
      else
        error ("dup2: invalid stream");
    }
  else
    print_usage ();

  return retval;
}

DEFUNX ("exec", Fexec, args, ,
        "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {[@var{err}, @var{msg}] =} exec (@var{file}, @var{args})\n\
Replace current process with a new process.\n\
\n\
Calling @code{exec} without first calling @code{fork} will terminate your\n\
current Octave process and replace it with the program named by @var{file}.\n\
For example,\n\
\n\
@example\n\
exec (\"ls\" \"-l\")\n\
@end example\n\
\n\
@noindent\n\
will run @code{ls} and return you to your shell prompt.\n\
\n\
If successful, @code{exec} does not return.  If @code{exec} does return,\n\
@var{err} will be nonzero, and @var{msg} will contain a system-dependent\n\
error message.\n\
@end deftypefn")
{
  octave_value_list retval;

  retval(1) = std::string ();
  retval(0) = -1;

  int nargin = args.length ();

  if (nargin == 1 || nargin == 2)
    {
      if (args(0).is_string ())
        {
          std::string exec_file = args(0).string_value ();

          string_vector exec_args;

          if (nargin == 2)
            {
              string_vector tmp = args(1).all_strings ();

              if (! error_state)
                {
                  int len = tmp.length ();

                  exec_args.resize (len + 1);

                  exec_args[0] = exec_file;

                  for (int i = 0; i < len; i++)
                    exec_args[i+1] = tmp[i];
                }
              else
                error ("exec: all arguments must be strings");
            }
          else
            {
              exec_args.resize (1);

              exec_args[0] = exec_file;
            }

          if (! error_state)
            {
              octave_history_write_timestamp ();

              if (! command_history::ignoring_entries ())
                command_history::clean_up_and_save ();

              std::string msg;

              int status = octave_syscalls::execvp (exec_file, exec_args, msg);

              retval(1) = msg;
              retval(0) = status;
            }
        }
      else
        error ("exec: FILE must be a string");
    }
  else
    print_usage ();

  return retval;
}

DEFUNX ("popen2", Fpopen2, args, ,
        "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {[@var{in}, @var{out}, @var{pid}] =} popen2 (@var{command}, @var{args})\n\
Start a subprocess with two-way communication.\n\
\n\
The name of the process is given by @var{command}, and @var{args} is an\n\
array of strings containing options for the command.\n\
\n\
The file identifiers for the input and output streams of the subprocess are\n\
returned in @var{in} and @var{out}.  If execution of the command is\n\
successful, @var{pid} contains the process ID of the subprocess.  Otherwise,\n\
@var{pid} is @minus{}1.\n\
\n\
For example:\n\
\n\
@example\n\
[in, out, pid] = popen2 (\"sort\", \"-r\");\n\
fputs (in, \"these\\nare\\nsome\\nstrings\\n\");\n\
fclose (in);\n\
EAGAIN = errno (\"EAGAIN\");\n\
done = false;\n\
do\n\
  s = fgets (out);\n\
  if (ischar (s))\n\
    fputs (stdout, s);\n\
  elseif (errno () == EAGAIN)\n\
    sleep (0.1);\n\
    fclear (out);\n\
  else\n\
    done = true;\n\
  endif\n\
until (done)\n\
fclose (out);\n\
waitpid (pid);\n\
\n\
   @print{} these\n\
   @print{} strings\n\
   @print{} some\n\
   @print{} are\n\
@end example\n\
\n\
Note that @code{popen2}, unlike @code{popen}, will not @nospell{\"reap\"} the\n\
child process.  If you don't use @code{waitpid} to check the child's\n\
exit status, it will linger until Octave exits.\n\
@seealso{popen, waitpid}\n\
@end deftypefn")
{
  octave_value_list retval;

  retval(2) = -1;
  retval(1) = Matrix ();
  retval(0) = Matrix ();

  int nargin = args.length ();

  if (nargin >= 1 && nargin <= 3)
    {
      if (args(0).is_string ())
        {
          std::string exec_file = args(0).string_value ();

          string_vector arg_list;

          if (nargin >= 2)
            {
              string_vector tmp = args(1).all_strings ();

              if (! error_state)
                {
                  int len = tmp.length ();

                  arg_list.resize (len + 1);

                  arg_list[0] = exec_file;

                  for (int i = 0; i < len; i++)
                    arg_list[i+1] = tmp[i];
                }
              else
                error ("popen2: all arguments must be strings");
            }
          else
            {
              arg_list.resize (1);

              arg_list[0] = exec_file;
            }

          if (! error_state)
            {
              bool sync_mode = (nargin == 3 ? args(2).bool_value () : false);

              if (! error_state)
                {
                  int fildes[2];
                  std::string msg;
                  pid_t pid;

                  pid = octave_syscalls::popen2 (exec_file, arg_list, sync_mode,
                                                 fildes, msg, interactive);
                  if (pid >= 0)
                    {
                      FILE *ifile = fdopen (fildes[1], "r");
                      FILE *ofile = fdopen (fildes[0], "w");

                      std::string nm;

                      octave_stream is = octave_stdiostream::create (nm, ifile,
                                                                  std::ios::in);

                      octave_stream os = octave_stdiostream::create (nm, ofile,
                                                                 std::ios::out);

                      Cell file_ids (1, 2);

                      retval(2) = pid;
                      retval(1) = octave_stream_list::insert (is);
                      retval(0) = octave_stream_list::insert (os);
                    }
                  else
                    error (msg.c_str ());
                }
            }
          else
            error ("popen2: all arguments must be strings");
        }
      else
        error ("popen2: COMMAND argument must be a string");
    }
  else
    print_usage ();

  return retval;
}

/*

%!test  # UNIX-style test
%! if (isunix () || ismac ())
%!   [in, out, pid] = popen2 ("sort", "-r");
%!   EAGAIN = errno ("EAGAIN");
%!   fputs (in, "these\nare\nsome\nstrings\n");
%!   fclose (in);
%!   done = false;
%!   str = {};
%!   idx = 0;
%!   errs = 0;
%!   do
%!     if (ismac ())  # FIXME: Is this necessary?
%!       errno (0);
%!     endif
%!     s = fgets (out);
%!     if (ischar (s))
%!       idx++;
%!       str{idx} = s;
%!     elseif (errno () == EAGAIN)
%!       fclear (out);
%!       sleep (0.1);
%!       if (++errs == 100)
%!         done = true;
%!       endif
%!     else
%!       done = true;
%!     endif
%!   until (done)
%!   fclose (out);
%!   waitpid (pid);
%!   assert (str, {"these\n","strings\n","some\n","are\n"});
%! endif

%!test  # Windows-style test
%! if (ispc () && ! isunix ())
%!   [in, out, pid] = popen2 ('C:\Windows\system32\sort.exe', "/R");
%!   EAGAIN = errno ("EINVAL");
%!   fputs (in, "these\r\nare\r\nsome\r\nstrings\r\n");
%!   fclose (in);
%!   done = false;
%!   str = {};
%!   idx = 0;
%!   errs = 0;
%!   do
%!     errno (0);
%!     s = fgets (out);
%!     if (ischar (s))
%!       idx++;
%!       str{idx} = s;
%!     elseif (errno () == EAGAIN)
%!       fclear (out);
%!       sleep (0.1);
%!       if (++errs == 100)
%!         done = true;
%!       endif
%!     else
%!       done = true;
%!     endif
%!   until (done)
%!   fclose (out);
%!   waitpid (pid);
%!   assert (str, {"these\r\n","strings\r\n","some\r\n","are\r\n"});
%! endif

*/

DEFUNX ("fcntl", Ffcntl, args, ,
        "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {[@var{err}, @var{msg}] =} fcntl (@var{fid}, @var{request}, @var{arg})\n\
Change the properties of the open file @var{fid}.\n\
\n\
The following values may be passed as @var{request}:\n\
\n\
@vtable @code\n\
@item F_DUPFD\n\
Return a duplicate file descriptor.\n\
\n\
@item F_GETFD\n\
Return the file descriptor flags for @var{fid}.\n\
\n\
@item F_SETFD\n\
Set the file descriptor flags for @var{fid}.\n\
\n\
@item F_GETFL\n\
Return the file status flags for @var{fid}.  The following codes may be\n\
returned (some of the flags may be undefined on some systems).\n\
\n\
@vtable @code\n\
@item O_RDONLY\n\
Open for reading only.\n\
\n\
@item O_WRONLY\n\
Open for writing only.\n\
\n\
@item O_RDWR\n\
Open for reading and writing.\n\
\n\
@item O_APPEND\n\
Append on each write.\n\
\n\
@item O_CREAT\n\
Create the file if it does not exist.\n\
\n\
@item O_NONBLOCK\n\
Non-blocking mode.\n\
\n\
@item O_SYNC\n\
Wait for writes to complete.\n\
\n\
@item O_ASYNC\n\
Asynchronous I/O.\n\
@end vtable\n\
\n\
@item F_SETFL\n\
Set the file status flags for @var{fid} to the value specified by @var{arg}.\n\
 The only flags that can be changed are @w{@code{O_APPEND}} and\n\
@w{@code{O_NONBLOCK}}.\n\
@end vtable\n\
\n\
If successful, @var{err} is 0 and @var{msg} is an empty string.  Otherwise,\n\
@var{err} is nonzero and @var{msg} contains a system-dependent error\n\
message.\n\
@seealso{fopen, dup2}\n\
@end deftypefn")
{
  octave_value_list retval;

  retval(1) = std::string ();
  retval(0) = -1;

  int nargin = args.length ();

  if (nargin == 3)
    {
      octave_stream strm = octave_stream_list::lookup (args(0), "fcntl");

      if (! error_state)
        {
          int fid = strm.file_number ();

          int req = args(1).int_value (true);
          int arg = args(2).int_value (true);

          if (! error_state)
            {
              // FIXME: Need better checking here?
              if (fid < 0)
                error ("fcntl: invalid file id");
              else
                {
                  std::string msg;

                  int status = octave_fcntl (fid, req, arg, msg);

                  retval(1) = msg;
                  retval(0) = status;
                }
            }
        }
      else
        error ("fcntl: FID, REQUEST, and ARG must be integers");
    }
  else
    print_usage ();

  return retval;
}

DEFUNX ("fork", Ffork, args, ,
        "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {[@var{pid}, @var{msg}] =} fork ()\n\
Create a copy of the current process.\n\
\n\
Fork can return one of the following values:\n\
\n\
@table @asis\n\
@item > 0\n\
You are in the parent process.  The value returned from @code{fork} is the\n\
process id of the child process.  You should probably arrange to wait for\n\
any child processes to exit.\n\
\n\
@item 0\n\
You are in the child process.  You can call @code{exec} to start another\n\
process.  If that fails, you should probably call @code{exit}.\n\
\n\
@item < 0\n\
The call to @code{fork} failed for some reason.  You must take evasive\n\
action.  A system dependent error message will be waiting in @var{msg}.\n\
@end table\n\
@end deftypefn")
{
  octave_value_list retval;

  retval(1) = std::string ();
  retval(0) = -1;

  int nargin = args.length ();

  if (nargin == 0)
    {
      std::string msg;

      pid_t pid = octave_syscalls::fork (msg);

      retval(1) = msg;
      retval(0) = pid;
    }
  else
    print_usage ();

  return retval;
}

DEFUNX ("getpgrp", Fgetpgrp, args, ,
        "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {pgid =} getpgrp ()\n\
Return the process group id of the current process.\n\
@end deftypefn")
{
  octave_value_list retval;

  retval(1) = std::string ();
  retval(0) = -1;

  int nargin = args.length ();

  if (nargin == 0)
    {
      std::string msg;

      retval(1) = msg;
      retval(0) = octave_syscalls::getpgrp (msg);
    }
  else
    print_usage ();

  return retval;
}

DEFUNX ("getpid", Fgetpid, args, ,
        "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {pid =} getpid ()\n\
Return the process id of the current process.\n\
@seealso{getppid}\n\
@end deftypefn")
{
  octave_value retval = -1;

  int nargin = args.length ();

  if (nargin == 0)
    retval = octave_syscalls::getpid ();
  else
    print_usage ();

  return retval;
}

DEFUNX ("getppid", Fgetppid, args, ,
        "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {pid =} getppid ()\n\
Return the process id of the parent process.\n\
@seealso{getpid}\n\
@end deftypefn")
{
  octave_value retval = -1;

  int nargin = args.length ();

  if (nargin == 0)
    retval = octave_syscalls::getppid ();
  else
    print_usage ();

  return retval;
}

DEFUNX ("getegid", Fgetegid, args, ,
        "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {egid =} getegid ()\n\
Return the effective group id of the current process.\n\
@seealso{getgid}\n\
@end deftypefn")
{
  octave_value retval = -1;

  int nargin = args.length ();

  if (nargin == 0)
    retval = octave_syscalls::getegid ();
  else
    print_usage ();

  return retval;
}

DEFUNX ("getgid", Fgetgid, args, ,
        "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {gid =} getgid ()\n\
Return the real group id of the current process.\n\
@seealso{getegid}\n\
@end deftypefn")
{
  octave_value retval = -1;

  int nargin = args.length ();

  if (nargin == 0)
    retval = octave_syscalls::getgid ();
  else
    print_usage ();

  return retval;
}

DEFUNX ("geteuid", Fgeteuid, args, ,
        "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {euid =} geteuid ()\n\
Return the effective user id of the current process.\n\
@seealso{getuid}\n\
@end deftypefn")
{
  octave_value retval = -1;

  int nargin = args.length ();

  if (nargin == 0)
    retval = octave_syscalls::geteuid ();
  else
    print_usage ();

  return retval;
}

DEFUNX ("getuid", Fgetuid, args, ,
        "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {uid =} getuid ()\n\
Return the real user id of the current process.\n\
@seealso{geteuid}\n\
@end deftypefn")
{
  octave_value retval = -1;

  int nargin = args.length ();

  if (nargin == 0)
    retval = octave_syscalls::getuid ();
  else
    print_usage ();

  return retval;
}

DEFUNX ("kill", Fkill, args, ,
        "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {[@var{err}, @var{msg}] =} kill (@var{pid}, @var{sig})\n\
Send signal @var{sig} to process @var{pid}.\n\
\n\
If @var{pid} is positive, then signal @var{sig} is sent to @var{pid}.\n\
\n\
If @var{pid} is 0, then signal @var{sig} is sent to every process\n\
in the process group of the current process.\n\
\n\
If @var{pid} is -1, then signal @var{sig} is sent to every process\n\
except process 1.\n\
\n\
If @var{pid} is less than -1, then signal @var{sig} is sent to every\n\
process in the process group @var{-pid}.\n\
\n\
If @var{sig} is 0, then no signal is sent, but error checking is still\n\
performed.\n\
\n\
Return 0 if successful, otherwise return -1.\n\
@end deftypefn")
{
  octave_value_list retval;

  retval(1) = std::string ();
  retval(0) = -1;

  if (args.length () == 2)
    {
      pid_t pid = args(0).int_value (true);

      if (! error_state)
        {
          int sig = args(1).int_value (true);

          if (! error_state)
            {
              std::string msg;

              int status = octave_syscalls::kill (pid, sig, msg);

              retval(1) = msg;
              retval(0) = status;
            }
        }
    }
  else
    print_usage ();

  return retval;
}

DEFUNX ("lstat", Flstat, args, ,
        "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{info} =} lstat (@var{symlink})\n\
@deftypefnx {Built-in Function} {[@var{info}, @var{err}, @var{msg}] =} lstat (@var{symlink})\n\
Return a structure @var{info} containing information about the symbolic link\n\
@var{symlink}.\n\
\n\
The function outputs are described in the documentation for @code{stat}.\n\
@seealso{stat, symlink}\n\
@end deftypefn")
{
  octave_value_list retval;

  if (args.length () == 1)
    {
      std::string fname = args(0).string_value ();

      if (! error_state)
        {
          file_stat fs (fname, false);

          retval = mk_stat_result (fs);
        }
    }
  else
    print_usage ();

  return retval;
}

DEFUNX ("mkfifo", Fmkfifo, args, ,
        "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{err} =} mkfifo (@var{name}, @var{mode})\n\
@deftypefnx {Built-in Function} {[@var{err}, @var{msg}] =} mkfifo (@var{name}, @var{mode})\n\
Create a FIFO special file named @var{name} with file mode @var{mode}.\n\
\n\
@var{mode} is interpreted as a decimal number (@emph{not} octal) and is\n\
subject to umask processing.  The final calculated mode is\n\
@code{@var{mode} - @var{umask}}.\n\
\n\
If successful, @var{err} is 0 and @var{msg} is an empty string.\n\
Otherwise, @var{err} is nonzero and @var{msg} contains a system-dependent\n\
error message.\n\
@seealso{pipe, umask}\n\
@end deftypefn")
{
  octave_value_list retval;

  retval(1) = std::string ();
  retval(0) = -1;

  int nargin = args.length ();

  if (nargin == 2)
    {
      if (args(0).is_string ())
        {
          std::string name = args(0).string_value ();

          if (args(1).is_scalar_type ())
            {
              long mode = args(1).long_value ();

              if (! error_state)
                {
                  std::string msg;

                  int status = octave_mkfifo (name, mode, msg);

                  retval(0) = status;

                  if (status < 0)
                    retval(1) = msg;
                }
              else
                error ("mkfifo: invalid MODE");
            }
          else
            error ("mkfifo: MODE must be an integer");
        }
      else
        error ("mkfifo: FILE must be a string");
    }
  else
    print_usage ();

  return retval;
}

DEFUNX ("pipe", Fpipe, args, ,
        "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {[@var{read_fd}, @var{write_fd}, @var{err}, @var{msg}] =} pipe ()\n\
Create a pipe and return the reading and writing ends of the pipe into\n\
@var{read_fd} and @var{write_fd} respectively.\n\
\n\
If successful, @var{err} is 0 and @var{msg} is an empty string.\n\
Otherwise, @var{err} is nonzero and @var{msg} contains a system-dependent\n\
error message.\n\
@seealso{mkfifo}\n\
@end deftypefn")
{
  octave_value_list retval;

  retval(3) = std::string ();
  retval(2) = -1;
  retval(1) = -1;
  retval(0) = -1;

  int nargin = args.length ();

  if (nargin == 0)
    {
      int fid[2];

      std::string msg;

      int status = octave_syscalls::pipe (fid, msg);

      if (status < 0)
        retval(3) = msg;
      else
        {
          FILE *ifile = fdopen (fid[0], "r");
          FILE *ofile = fdopen (fid[1], "w");

          std::string nm;

          octave_stream is = octave_stdiostream::create (nm, ifile,
                                                         std::ios::in);

          octave_stream os = octave_stdiostream::create (nm, ofile,
                                                         std::ios::out);

          retval(2) = status;
          retval(1) = octave_stream_list::insert (os);
          retval(0) = octave_stream_list::insert (is);
        }
    }
  else
    print_usage ();

  return retval;
}

DEFUNX ("stat", Fstat, args, ,
        "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {[@var{info}, @var{err}, @var{msg}] =} stat (@var{file})\n\
@deftypefnx {Built-in Function} {[@var{info}, @var{err}, @var{msg}] =} stat (@var{fid})\n\
@deftypefnx {Built-in Function} {[@var{info}, @var{err}, @var{msg}] =} lstat (@var{file})\n\
@deftypefnx {Built-in Function} {[@var{info}, @var{err}, @var{msg}] =} lstat (@var{fid})\n\
Return a structure @var{info} containing the following information about\n\
@var{file} or file identifier @var{fid}.\n\
\n\
@table @code\n\
@item dev\n\
ID of device containing a directory entry for this file.\n\
\n\
@item ino\n\
File number of the file.\n\
\n\
@item mode\n\
File mode, as an integer.  Use the functions @w{@code{S_ISREG}},\n\
@w{@code{S_ISDIR}}, @w{@code{S_ISCHR}}, @w{@code{S_ISBLK}},\n\
@w{@code{S_ISFIFO}}, @w{@code{S_ISLNK}}, or @w{@code{S_ISSOCK}} to extract\n\
information from this value.\n\
\n\
@item modestr\n\
File mode, as a string of ten letters or dashes as would be returned by\n\
@kbd{ls -l}.\n\
\n\
@item nlink\n\
Number of links.\n\
\n\
@item uid\n\
User ID of file's owner.\n\
\n\
@item gid\n\
Group ID of file's group.\n\
\n\
@item rdev\n\
ID of device for block or character special files.\n\
\n\
@item size\n\
Size in bytes.\n\
\n\
@item atime\n\
Time of last access in the same form as time values returned from\n\
@code{time}.  @xref{Timing Utilities}.\n\
\n\
@item mtime\n\
Time of last modification in the same form as time values returned from\n\
@code{time}.  @xref{Timing Utilities}.\n\
\n\
@item ctime\n\
Time of last file status change in the same form as time values\n\
returned from @code{time}.  @xref{Timing Utilities}.\n\
\n\
@item blksize\n\
Size of blocks in the file.\n\
\n\
@item blocks\n\
Number of blocks allocated for file.\n\
@end table\n\
\n\
If the call is successful @var{err} is 0 and @var{msg} is an empty string.\n\
If the file does not exist, or some other error occurs, @var{info} is an\n\
empty matrix, @var{err} is @minus{}1, and @var{msg} contains the\n\
corresponding system error message.\n\
\n\
If @var{file} is a symbolic link, @code{stat} will return information about\n\
the actual file that is referenced by the link.  Use @code{lstat} if you\n\
want information about the symbolic link itself.\n\
\n\
For example:\n\
\n\
@example\n\
[info, err, msg] = stat (\"/vmlinuz\")\n\
  @result{} info =\n\
     @{\n\
       atime = 855399756\n\
       rdev = 0\n\
       ctime = 847219094\n\
       uid = 0\n\
       size = 389218\n\
       blksize = 4096\n\
       mtime = 847219094\n\
       gid = 6\n\
       nlink = 1\n\
       blocks = 768\n\
       mode = -rw-r--r--\n\
       modestr = -rw-r--r--\n\
       ino = 9316\n\
       dev = 2049\n\
     @}\n\
  @result{} err = 0\n\
  @result{} msg =\n\
@end example\n\
@seealso{lstat, ls, dir}\n\
@end deftypefn")
{
  octave_value_list retval;

  if (args.length () == 1)
    {
      if (args(0).is_scalar_type ())
        {
          int fid = octave_stream_list::get_file_number (args(0));

          if (! error_state)
            {
              file_fstat fs (fid);

              retval = mk_stat_result (fs);
            }
        }
      else
        {
          std::string fname = args(0).string_value ();

          if (! error_state)
            {
              file_stat fs (fname);

              retval = mk_stat_result (fs);
            }
        }
    }
  else
    print_usage ();

  return retval;
}

DEFUNX ("S_ISREG", FS_ISREG, args, ,
        "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} S_ISREG (@var{mode})\n\
Return true if @var{mode} corresponds to a regular file.\n\
\n\
The value of @var{mode} is assumed to be returned from a call to @code{stat}.\n\
@seealso{stat, lstat}\n\
@end deftypefn")
{
  octave_value retval = false;

  if (args.length () == 1)
    {
      double mode = args(0).double_value ();

      if (! error_state)
        retval = file_stat::is_reg (static_cast<mode_t> (mode));
      else
        error ("S_ISREG: invalid MODE value");
    }
  else
    print_usage ();

  return retval;
}

DEFUNX ("S_ISDIR", FS_ISDIR, args, ,
        "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} S_ISDIR (@var{mode})\n\
Return true if @var{mode} corresponds to a directory.\n\
\n\
The value of @var{mode} is assumed to be returned from a call to @code{stat}.\n\
@seealso{stat, lstat}\n\
@end deftypefn")
{
  octave_value retval = false;

  if (args.length () == 1)
    {
      double mode = args(0).double_value ();

      if (! error_state)
        retval = file_stat::is_dir (static_cast<mode_t> (mode));
      else
        error ("S_ISDIR: invalid MODE value");
    }
  else
    print_usage ();

  return retval;
}

DEFUNX ("S_ISCHR", FS_ISCHR, args, ,
        "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} S_ISCHR (@var{mode})\n\
Return true if @var{mode} corresponds to a character device.\n\
\n\
The value of @var{mode} is assumed to be returned from a call to @code{stat}.\n\
@seealso{stat, lstat}\n\
@end deftypefn")
{
  octave_value retval = false;

  if (args.length () == 1)
    {
      double mode = args(0).double_value ();

      if (! error_state)
        retval = file_stat::is_chr (static_cast<mode_t> (mode));
      else
        error ("S_ISCHR: invalid MODE value");
    }
  else
    print_usage ();

  return retval;
}

DEFUNX ("S_ISBLK", FS_ISBLK, args, ,
        "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} S_ISBLK (@var{mode})\n\
Return true if @var{mode} corresponds to a block device.\n\
\n\
The value of @var{mode} is assumed to be returned from a call to @code{stat}.\n\
@seealso{stat, lstat}\n\
@end deftypefn")
{
  octave_value retval = false;

  if (args.length () == 1)
    {
      double mode = args(0).double_value ();

      if (! error_state)
        retval = file_stat::is_blk (static_cast<mode_t> (mode));
      else
        error ("S_ISBLK: invalid MODE value");
    }
  else
    print_usage ();

  return retval;
}

DEFUNX ("S_ISFIFO", FS_ISFIFO, args, ,
        "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} S_ISFIFO (@var{mode})\n\
Return true if @var{mode} corresponds to a fifo.\n\
\n\
The value of @var{mode} is assumed to be returned from a call to @code{stat}.\n\
@seealso{stat, lstat}\n\
@end deftypefn")
{
  octave_value retval = false;

  if (args.length () == 1)
    {
      double mode = args(0).double_value ();

      if (! error_state)
        retval = file_stat::is_fifo (static_cast<mode_t> (mode));
      else
        error ("S_ISFIFO: invalid MODE value");
    }
  else
    print_usage ();

  return retval;
}

DEFUNX ("S_ISLNK", FS_ISLNK, args, ,
        "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} S_ISLNK (@var{mode})\n\
Return true if @var{mode} corresponds to a symbolic link.\n\
\n\
The value of @var{mode} is assumed to be returned from a call to @code{stat}.\n\
@seealso{stat, lstat}\n\
@end deftypefn")
{
  octave_value retval = false;

  if (args.length () == 1)
    {
      double mode = args(0).double_value ();

      if (! error_state)
        retval = file_stat::is_lnk (static_cast<mode_t> (mode));
      else
        error ("S_ISLNK: invalid MODE value");
    }
  else
    print_usage ();

  return retval;
}

DEFUNX ("S_ISSOCK", FS_ISSOCK, args, ,
        "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} S_ISSOCK (@var{mode})\n\
Return true if @var{mode} corresponds to a socket.\n\
\n\
The value of @var{mode} is assumed to be returned from a call to @code{stat}.\n\
@seealso{stat, lstat}\n\
@end deftypefn")
{
  octave_value retval = false;

  if (args.length () == 1)
    {
      double mode = args(0).double_value ();

      if (! error_state)
        retval = file_stat::is_sock (static_cast<mode_t> (mode));
      else
        error ("S_ISSOCK: invalid MODE value");
    }
  else
    print_usage ();

  return retval;
}

DEFUN (gethostname, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} gethostname ()\n\
Return the hostname of the system where Octave is running.\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 0)
    retval = octave_env::get_host_name ();
  else
    print_usage ();

  return retval;
}

DEFUN (uname, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {[@var{uts}, @var{err}, @var{msg}] =} uname ()\n\
Return system information in the structure.\n\
\n\
For example:\n\
\n\
@example\n\
@group\n\
uname ()\n\
   @result{} @{\n\
         sysname = x86_64\n\
         nodename = segfault\n\
         release = 2.6.15-1-amd64-k8-smp\n\
         version = Linux\n\
         machine = #2 SMP Thu Feb 23 04:57:49 UTC 2006\n\
      @}\n\
@end group\n\
@end example\n\
\n\
If successful, @var{err} is 0 and @var{msg} is an empty string.\n\
Otherwise, @var{err} is nonzero and @var{msg} contains a\n\
system-dependent error message.\n\
@end deftypefn")
{
  octave_value_list retval;

  if (args.length () == 0)
    {
      octave_uname sysinfo;

      octave_scalar_map m;

      m.assign ("sysname", sysinfo.sysname ());
      m.assign ("nodename", sysinfo.nodename ());
      m.assign ("release", sysinfo.release ());
      m.assign ("version", sysinfo.version ());
      m.assign ("machine", sysinfo.machine ());

      retval(2) = sysinfo.message ();
      retval(1) = sysinfo.error ();
      retval(0) = m;
    }
  else
    print_usage ();

  return retval;
}

DEFUNX ("unlink", Funlink, args, ,
        "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {[@var{err}, @var{msg}] =} unlink (@var{file})\n\
Delete the file named @var{file}.\n\
\n\
If successful, @var{err} is 0 and @var{msg} is an empty string.\n\
Otherwise, @var{err} is nonzero and @var{msg} contains a system-dependent\n\
error message.\n\
@seealso{delete, rmdir}\n\
@end deftypefn")
{
  octave_value_list retval;

  retval(1) = std::string ();
  retval(0) = -1;

  int nargin = args.length ();

  if (nargin == 1)
    {
      if (args(0).is_string ())
        {
          std::string name = args(0).string_value ();

          std::string msg;

          int status = octave_unlink (name, msg);

          retval(1) = msg;
          retval(0) = status;
        }
      else
        error ("unlink: FILE must be a string");
    }
  else
    print_usage ();

  return retval;
}

DEFUNX ("waitpid", Fwaitpid, args, ,
        "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {[@var{pid}, @var{status}, @var{msg}] =} waitpid (@var{pid}, @var{options})\n\
Wait for process @var{pid} to terminate.\n\
\n\
The @var{pid} argument can be:\n\
\n\
@table @asis\n\
@item @minus{}1\n\
Wait for any child process.\n\
\n\
@item 0\n\
Wait for any child process whose process group ID is equal to that of the\n\
Octave interpreter process.\n\
\n\
@item > 0\n\
Wait for termination of the child process with ID @var{pid}.\n\
@end table\n\
\n\
The @var{options} argument can be a bitwise OR of zero or more of the\n\
following constants:\n\
\n\
@table @code\n\
@item 0\n\
Wait until signal is received or a child process exits (this is the default\n\
if the @var{options} argument is missing).\n\
\n\
@item WNOHANG\n\
Do not hang if status is not immediately available.\n\
\n\
@item WUNTRACED\n\
Report the status of any child processes that are stopped, and whose status\n\
has not yet been reported since they stopped.\n\
\n\
@item WCONTINUE\n\
Return if a stopped child has been resumed by delivery of @code{SIGCONT}.\n\
This value may not be meaningful on all systems.\n\
@end table\n\
\n\
If the returned value of @var{pid} is greater than 0, it is the process ID\n\
of the child process that exited.  If an error occurs, @var{pid} will be\n\
less than zero and @var{msg} will contain a system-dependent error message.\n\
The value of @var{status} contains additional system-dependent information\n\
about the subprocess that exited.\n\
@seealso{WCONTINUE, WCOREDUMP, WEXITSTATUS, WIFCONTINUED, WIFSIGNALED, WIFSTOPPED, WNOHANG, WSTOPSIG, WTERMSIG, WUNTRACED}\n\
@end deftypefn")
{
  octave_value_list retval;

  retval(2) = std::string ();
  retval(1) = 0;
  retval(0) = -1;

  int nargin = args.length ();

  if (nargin == 1 || nargin == 2)
    {
      pid_t pid = args(0).int_value (true);

      if (! error_state)
        {
          int options = 0;

          if (args.length () == 2)
            options = args(1).int_value (true);

          if (! error_state)
            {
              std::string msg;

              int status = 0;

              pid_t result = octave_syscalls::waitpid (pid, &status,
                                                       options, msg);

              retval(2) = msg;
              retval(1) = status;
              retval(0) = result;
            }
          else
            error ("waitpid: OPTIONS must be an integer");
        }
      else
        error ("waitpid: PID must be an integer value");
    }
  else
    print_usage ();

  return retval;
}

DEFUNX ("WIFEXITED", FWIFEXITED, args, ,
        "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} WIFEXITED (@var{status})\n\
Given @var{status} from a call to @code{waitpid}, return\n\
true if the child terminated normally.\n\
@seealso{waitpid, WEXITSTATUS, WIFSIGNALED, WTERMSIG, WCOREDUMP, WIFSTOPPED, WSTOPSIG, WIFCONTINUED}\n\
@end deftypefn")
{
  octave_value retval = false;

  if (args.length () == 1)
    {
      int status = args(0).int_value ();

      if (! error_state)
        retval = octave_wait::ifexited (status);
      else
        error ("WIFEXITED: STATUS must be an integer");
    }

  return retval;
}

DEFUNX ("WEXITSTATUS", FWEXITSTATUS, args, ,
        "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} WEXITSTATUS (@var{status})\n\
Given @var{status} from a call to @code{waitpid}, return\n\
the exit status of the child.\n\
\n\
This function should only be employed if @code{WIFEXITED} returned true.\n\
@seealso{waitpid, WIFEXITED, WIFSIGNALED, WTERMSIG, WCOREDUMP, WIFSTOPPED, WSTOPSIG, WIFCONTINUED}\n\
@end deftypefn")
{
  octave_value retval = 0;

  if (args.length () == 1)
    {
      int status = args(0).int_value ();

      if (! error_state)
        retval = octave_wait::exitstatus (status);
      else
        error ("WEXITSTATUS: STATUS must be an integer");
    }

  return retval;
}

DEFUNX ("WIFSIGNALED", FWIFSIGNALED, args, ,
        "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} WIFSIGNALED (@var{status})\n\
Given @var{status} from a call to @code{waitpid}, return\n\
true if the child process was terminated by a signal.\n\
@seealso{waitpid, WIFEXITED, WEXITSTATUS, WTERMSIG, WCOREDUMP, WIFSTOPPED, WSTOPSIG, WIFCONTINUED}\n\
@end deftypefn")
{
  octave_value retval = false;

  if (args.length () == 1)
    {
      int status = args(0).int_value ();

      if (! error_state)
        retval = octave_wait::ifsignaled (status);
      else
        error ("WIFSIGNALED: STATUS must be an integer");
    }

  return retval;
}

DEFUNX ("WTERMSIG", FWTERMSIG, args, ,
        "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} WTERMSIG (@var{status})\n\
Given @var{status} from a call to @code{waitpid}, return\n\
the number of the signal that caused the child process to terminate.\n\
\n\
This function should only be employed if @code{WIFSIGNALED} returned true.\n\
@seealso{waitpid, WIFEXITED, WEXITSTATUS, WIFSIGNALED, WCOREDUMP, WIFSTOPPED, WSTOPSIG, WIFCONTINUED}\n\
@end deftypefn")
{
  octave_value retval = 0;

  if (args.length () == 1)
    {
      int status = args(0).int_value ();

      if (! error_state)
        retval = octave_wait::termsig (status);
      else
        error ("WTERMSIG: STATUS must be an integer");
    }

  return retval;
}

DEFUNX ("WCOREDUMP", FWCOREDUMP, args, ,
        "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} WCOREDUMP (@var{status})\n\
Given @var{status} from a call to @code{waitpid}, return\n\
true if the child produced a core dump.\n\
\n\
This function should only be employed if @code{WIFSIGNALED} returned true.\n\
The macro used to implement this function is not specified in POSIX.1-2001\n\
and is not available on some Unix implementations (e.g., AIX, SunOS).\n\
@seealso{waitpid, WIFEXITED, WEXITSTATUS, WIFSIGNALED, WTERMSIG, WIFSTOPPED, WSTOPSIG, WIFCONTINUED}\n\
@end deftypefn")
{
  octave_value retval = false;

  if (args.length () == 1)
    {
      int status = args(0).int_value ();

      if (! error_state)
        retval = octave_wait::coredump (status);
      else
        error ("WCOREDUMP: STATUS must be an integer");
    }

  return retval;
}

DEFUNX ("WIFSTOPPED", FWIFSTOPPED, args, ,
        "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} WIFSTOPPED (@var{status})\n\
Given @var{status} from a call to @code{waitpid}, return\n\
true if the child process was stopped by delivery of a signal.\n\
\n\
This is only possible if the call was done using @code{WUNTRACED} or when\n\
the child is being traced (see ptrace(2)).\n\
@seealso{waitpid, WIFEXITED, WEXITSTATUS, WIFSIGNALED, WTERMSIG, WCOREDUMP, WSTOPSIG, WIFCONTINUED}\n\
@end deftypefn")
{
  octave_value retval = false;

  if (args.length () == 1)
    {
      int status = args(0).int_value ();

      if (! error_state)
        retval = octave_wait::ifstopped (status);
      else
        error ("WIFSTOPPED: STATUS must be an integer");
    }

  return retval;
}

DEFUNX ("WSTOPSIG", FWSTOPSIG, args, ,
        "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} WSTOPSIG (@var{status})\n\
Given @var{status} from a call to @code{waitpid}, return\n\
the number of the signal which caused the child to stop.\n\
\n\
This function should only be employed if @code{WIFSTOPPED} returned true.\n\
@seealso{waitpid, WIFEXITED, WEXITSTATUS, WIFSIGNALED, WTERMSIG, WCOREDUMP, WIFSTOPPED, WIFCONTINUED}\n\
@end deftypefn")
{
  octave_value retval = 0;

  if (args.length () == 1)
    {
      int status = args(0).int_value ();

      if (! error_state)
        retval = octave_wait::stopsig (status);
      else
        error ("WSTOPSIG: STATUS must be an integer");
    }

  return retval;
}

DEFUNX ("WIFCONTINUED", FWIFCONTINUED, args, ,
        "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} WIFCONTINUED (@var{status})\n\
Given @var{status} from a call to @code{waitpid}, return\n\
true if the child process was resumed by delivery of @code{SIGCONT}.\n\
@seealso{waitpid, WIFEXITED, WEXITSTATUS, WIFSIGNALED, WTERMSIG, WCOREDUMP, WIFSTOPPED, WSTOPSIG}\n\
@end deftypefn")
{
  octave_value retval = false;

  if (args.length () == 1)
    {
      int status = args(0).int_value ();

      if (! error_state)
        retval = octave_wait::ifcontinued (status);
      else
        error ("WIFCONTINUED: STATUS must be an integer");
    }

  return retval;
}

DEFUNX ("canonicalize_file_name", Fcanonicalize_file_name, args, ,
        "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {[@var{cname}, @var{status}, @var{msg}] =} canonicalize_file_name (@var{fname})\n\
Return the canonical name of file @var{fname}.\n\
\n\
If the file does not exist the empty string (\"\") is returned.\n\
@seealso{make_absolute_filename, is_absolute_filename, is_rooted_relative_filename}\n\
@end deftypefn")
{
  octave_value_list retval;

  if (args.length () == 1)
    {
      if (args(0).is_string ())
        {
          std::string name = args(0).string_value ();
          std::string msg;

          std::string result = octave_canonicalize_file_name (name, msg);

          retval(2) = msg;
          retval(1) = msg.empty () ? 0 : -1;
          retval(0) = result;
        }
      else
        error ("canonicalize_file_name: NAME must be a string");
    }
  else
    print_usage ();

  return retval;
}

static octave_value
const_value (const octave_value_list& args, int val)
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 0)
    retval = val;
  else
    print_usage ();

  return retval;
}

#if !defined (O_NONBLOCK) && defined (O_NDELAY)
#define O_NONBLOCK O_NDELAY
#endif

DEFUNX ("F_DUPFD", FF_DUPFD, args, ,
        "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} F_DUPFD ()\n\
Return the numerical value to pass to @code{fcntl} to return\n\
a duplicate file descriptor.\n\
@seealso{fcntl, F_GETFD, F_GETFL, F_SETFD, F_SETFL}\n\
@end deftypefn")
{
#if defined (F_DUPFD)
  return const_value (args, F_DUPFD);
#else
  error ("F_DUPFD: not available on this system");
  return octave_value ();
#endif
}

DEFUNX ("F_GETFD", FF_GETFD, args, ,
        "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} F_GETFD ()\n\
Return the numerical value to pass to @code{fcntl} to return\n\
the file descriptor flags.\n\
@seealso{fcntl, F_DUPFD, F_GETFL, F_SETFD, F_SETFL}\n\
@end deftypefn")
{
#if defined (F_GETFD)
  return const_value (args, F_GETFD);
#else
  error ("F_GETFD: not available on this system");
  return octave_value ();
#endif
}

DEFUNX ("F_GETFL", FF_GETFL, args, ,
        "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} F_GETFL ()\n\
Return the numerical value to pass to @code{fcntl} to return\n\
the file status flags.\n\
@seealso{fcntl, F_DUPFD, F_GETFD, F_SETFD, F_SETFL}\n\
@end deftypefn")
{
#if defined (F_GETFL)
  return const_value (args, F_GETFL);
#else
  error ("F_GETFL: not available on this system");
  return octave_value ();
#endif
}

DEFUNX ("F_SETFD", FF_SETFD, args, ,
        "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} F_SETFD ()\n\
Return the numerical value to pass to @code{fcntl} to set the file\n\
descriptor flags.\n\
@seealso{fcntl, F_DUPFD, F_GETFD, F_GETFL, F_SETFL}\n\
@end deftypefn")
{
#if defined (F_SETFD)
  return const_value (args, F_SETFD);
#else
  error ("F_SETFD: not available on this system");
  return octave_value ();
#endif
}

DEFUNX ("F_SETFL", FF_SETFL, args, ,
        "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} F_SETFL ()\n\
Return the numerical value to pass to @code{fcntl} to set the file\n\
status flags.\n\
@seealso{fcntl, F_DUPFD, F_GETFD, F_GETFL, F_SETFD}\n\
@end deftypefn")
{
#if defined (F_SETFL)
  return const_value (args, F_SETFL);
#else
  error ("F_SETFL: not available on this system");
  return octave_value ();
#endif
}

DEFUNX ("O_APPEND", FO_APPEND, args, ,
        "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} O_APPEND ()\n\
Return the numerical value of the file status flag that may be\n\
returned by @code{fcntl} to indicate each write operation appends,\n\
or that may be passed to @code{fcntl} to set the write mode to append.\n\
@seealso{fcntl, O_ASYNC, O_CREAT, O_EXCL, O_NONBLOCK, O_RDONLY, O_RDWR, O_SYNC, O_TRUNC, O_WRONLY}\n\
@end deftypefn")
{
#if defined (O_APPEND)
  return const_value (args, O_APPEND);
#else
  error ("O_APPEND: not available on this system");
  return octave_value ();
#endif
}

DEFUNX ("O_ASYNC", FO_ASYNC, args, ,
        "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} O_ASYNC ()\n\
Return the numerical value of the file status flag that may be\n\
returned by @code{fcntl} to indicate asynchronous I/O.\n\
@seealso{fcntl, O_APPEND, O_CREAT, O_EXCL, O_NONBLOCK, O_RDONLY, O_RDWR, O_SYNC, O_TRUNC, O_WRONLY}\n\
@end deftypefn")
{
#if defined (O_ASYNC)
  return const_value (args, O_ASYNC);
#else
  error ("O_ASYNC: not available on this system");
  return octave_value ();
#endif
}

DEFUNX ("O_CREAT", FO_CREAT, args, ,
        "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} O_CREAT ()\n\
Return the numerical value of the file status flag that may be\n\
returned by @code{fcntl} to indicate that a file should be created if it\n\
does not exist.\n\
@seealso{fcntl, O_APPEND, O_ASYNC, O_EXCL, O_NONBLOCK, O_RDONLY, O_RDWR, O_SYNC, O_TRUNC, O_WRONLY}\n\
@end deftypefn")
{
#if defined (O_CREAT)
  return const_value (args, O_CREAT);
#else
  error ("O_CREAT: not available on this system");
  return octave_value ();
#endif
}

DEFUNX ("O_EXCL", FO_EXCL, args, ,
        "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} O_EXCL ()\n\
Return the numerical value of the file status flag that may be\n\
returned by @code{fcntl} to indicate that file locking is used.\n\
@seealso{fcntl, O_APPEND, O_ASYNC, O_CREAT, O_NONBLOCK, O_RDONLY, O_RDWR, O_SYNC, O_TRUNC, O_WRONLY}\n\
@end deftypefn")
{
#if defined (O_EXCL)
  return const_value (args, O_EXCL);
#else
  error ("O_EXCL: not available on this system");
  return octave_value ();
#endif
}

DEFUNX ("O_NONBLOCK", FO_NONBLOCK, args, ,
        "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} O_NONBLOCK ()\n\
Return the numerical value of the file status flag that may be\n\
returned by @code{fcntl} to indicate that non-blocking I/O is in use,\n\
or that may be passsed to @code{fcntl} to set non-blocking I/O.\n\
@seealso{fcntl, O_APPEND, O_ASYNC, O_CREAT, O_EXCL, O_RDONLY, O_RDWR, O_SYNC, O_TRUNC, O_WRONLY}\n\
@end deftypefn")
{
#if defined (O_NONBLOCK)
  return const_value (args, O_NONBLOCK);
#else
  error ("O_NONBLOCK: not available on this system");
  return octave_value ();
#endif
}

DEFUNX ("O_RDONLY", FO_RDONLY, args, ,
        "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} O_RDONLY ()\n\
Return the numerical value of the file status flag that may be\n\
returned by @code{fcntl} to indicate that a file is open for reading only.\n\
@seealso{fcntl, O_APPEND, O_ASYNC, O_CREAT, O_EXCL, O_NONBLOCK, O_RDWR, O_SYNC, O_TRUNC, O_WRONLY}\n\
@end deftypefn")
{
#if defined (O_RDONLY)
  return const_value (args, O_RDONLY);
#else
  error ("O_RDONLY: not available on this system");
  return octave_value ();
#endif
}

DEFUNX ("O_RDWR", FO_RDWR, args, ,
        "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} O_RDWR ()\n\
Return the numerical value of the file status flag that may be\n\
returned by @code{fcntl} to indicate that a file is open for both reading\n\
and writing.\n\
@seealso{fcntl, O_APPEND, O_ASYNC, O_CREAT, O_EXCL, O_NONBLOCK, O_RDONLY, O_SYNC, O_TRUNC, O_WRONLY}\n\
@end deftypefn")
{
#if defined (O_RDWR)
  return const_value (args, O_RDWR);
#else
  error ("O_RDWR: not available on this system");
  return octave_value ();
#endif
}

DEFUNX ("O_SYNC", FO_SYNC, args, ,
        "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} O_SYNC ()\n\
Return the numerical value of the file status flag that may be\n\
returned by @code{fcntl} to indicate that a file is open for synchronous I/O.\n\
@seealso{fcntl, O_APPEND, O_ASYNC, O_CREAT, O_EXCL, O_NONBLOCK, O_RDONLY, O_RDWR, O_TRUNC, O_WRONLY}\n\
@end deftypefn")
{
#if defined (O_SYNC)
  return const_value (args, O_SYNC);
#else
  error ("O_SYNC: not available on this system");
  return octave_value ();
#endif
}

DEFUNX ("O_TRUNC", FO_TRUNC, args, ,
        "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} O_TRUNC ()\n\
Return the numerical value of the file status flag that may be\n\
returned by @code{fcntl} to indicate that if file exists, it should be\n\
truncated when writing.\n\
@seealso{fcntl, O_APPEND, O_ASYNC, O_CREAT, O_EXCL, O_NONBLOCK, O_RDONLY, O_RDWR, O_SYNC, O_WRONLY}\n\
@end deftypefn")
{
#if defined (O_TRUNC)
  return const_value (args, O_TRUNC);
#else
  error ("O_TRUNC: not available on this system");
  return octave_value ();
#endif
}

DEFUNX ("O_WRONLY", FO_WRONLY, args, ,
        "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} O_WRONLY ()\n\
Return the numerical value of the file status flag that may be\n\
returned by @code{fcntl} to indicate that a file is open for writing only.\n\
@seealso{fcntl, O_APPEND, O_ASYNC, O_CREAT, O_EXCL, O_NONBLOCK, O_RDONLY, O_RDWR, O_SYNC, O_TRUNC}\n\
@end deftypefn")
{
#if defined (O_WRONLY)
  return const_value (args, O_WRONLY);
#else
  error ("O_WRONLY: not available on this system");
  return octave_value ();
#endif
}

#if !defined (WNOHANG)
#define WNOHANG 0
#endif

DEFUNX ("WNOHANG", FWNOHANG, args, ,
        "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} WNOHANG ()\n\
Return the numerical value of the option argument that may be\n\
passed to @code{waitpid} to indicate that it should return its status\n\
immediately instead of waiting for a process to exit.\n\
@seealso{waitpid, WUNTRACED, WCONTINUE}\n\
@end deftypefn")
{
  return const_value (args, WNOHANG);
}

#if !defined (WUNTRACED)
#define WUNTRACED 0
#endif

DEFUNX ("WUNTRACED", FWUNTRACED, args, ,
        "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} WUNTRACED ()\n\
Return the numerical value of the option argument that may be\n\
passed to @code{waitpid} to indicate that it should also return if the child\n\
process has stopped but is not traced via the @code{ptrace} system call\n\
@seealso{waitpid, WNOHANG, WCONTINUE}\n\
@end deftypefn")
{
  return const_value (args, WUNTRACED);
}

#if !defined (WCONTINUE)
#define WCONTINUE 0
#endif

DEFUNX ("WCONTINUE", FWCONTINUE, args, ,
        "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} WCONTINUE ()\n\
Return the numerical value of the option argument that may be\n\
passed to @code{waitpid} to indicate that it should also return if a stopped\n\
child has been resumed by delivery of a @code{SIGCONT} signal.\n\
@seealso{waitpid, WNOHANG, WUNTRACED}\n\
@end deftypefn")
{
  return const_value (args, WCONTINUE);
}

