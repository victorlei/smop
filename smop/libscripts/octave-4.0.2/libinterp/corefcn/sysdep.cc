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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cfloat>
#include <cstddef>
#include <cstdio>
#include <cstdlib>
#include <cstring>

#include <iostream>
#include <string>

#include <sys/types.h>
#include <unistd.h>

#if defined (HAVE_TERMIOS_H)
#include <termios.h>
#elif defined (HAVE_TERMIO_H)
#include <termio.h>
#elif defined (HAVE_SGTTY_H)
#include <sgtty.h>
#endif

#if defined (HAVE_CONIO_H)
#include <conio.h>
#endif

#if defined (HAVE_SYS_IOCTL_H)
#include <sys/ioctl.h>
#endif

#if defined (HAVE_FLOATINGPOINT_H)
#include <floatingpoint.h>
#endif

#if defined (HAVE_IEEEFP_H)
#include <ieeefp.h>
#endif

#if defined (HAVE_OMP_H)
#include <omp.h>
#endif

#include "cmd-edit.h"
#include "file-ops.h"
#include "lo-mappers.h"
#include "lo-math.h"
#include "mach-info.h"
#include "oct-env.h"
#include "quit.h"

#include "Cell.h"
#include "builtins.h"
#include "defun.h"
#include "display.h"
#include "error.h"
#include "input.h"
#include "oct-obj.h"
#include "ov.h"
#include "pager.h"
#include "parse.h"
#include "sighandlers.h"
#include "sysdep.h"
#include "toplev.h"
#include "utils.h"
#include "file-stat.h"

#ifndef STDIN_FILENO
#define STDIN_FILENO 1
#endif

#if defined (__386BSD__) || defined (__FreeBSD__) || defined (__NetBSD__)
static void
BSD_init (void)
{
#if defined (HAVE_FLOATINGPOINT_H)
  // Disable trapping on common exceptions.
#ifndef FP_X_DNML
#define FP_X_DNML 0
#endif
  fpsetmask (~(FP_X_OFL|FP_X_INV|FP_X_DZ|FP_X_DNML|FP_X_UFL|FP_X_IMP));
#endif
}
#endif

#if defined (__WIN32__) && ! defined (_POSIX_VERSION)

#define WIN32_LEAN_AND_MEAN
#include <tlhelp32.h>
#include <windows.h>

static void
w32_set_octave_home (void)
{
  std::string bin_dir;

  HANDLE h = CreateToolhelp32Snapshot (TH32CS_SNAPMODULE
#ifdef TH32CS_SNAPMODULE32
                                       | TH32CS_SNAPMODULE32
#endif
                                       , 0);

  if (h != INVALID_HANDLE_VALUE)
    {
      MODULEENTRY32 mod_info;

      ZeroMemory (&mod_info, sizeof (mod_info));
      mod_info.dwSize = sizeof (mod_info);

      if (Module32First (h, &mod_info))
        {
          do
            {
              std::string mod_name (mod_info.szModule);

              if (mod_name.find ("octinterp") != std::string::npos)
                {
                  bin_dir = mod_info.szExePath;
                  if (bin_dir[bin_dir.length () - 1] != '\\')
                    bin_dir.append (1, '\\');
                  break;
                }
            }
          while (Module32Next (h, &mod_info));
        }

      CloseHandle (h);
    }

  if (! bin_dir.empty ())
    {
      size_t pos = bin_dir.rfind ("\\bin\\");

      if (pos != std::string::npos)
        octave_env::putenv ("OCTAVE_HOME", bin_dir.substr (0, pos));
    }
}

void
w32_set_quiet_shutdown (void)
{
  // Let the user close the console window or shutdown without the
  // pesky dialog.
  //
  // FIXME: should this be user configurable?
  SetProcessShutdownParameters (0x280, SHUTDOWN_NORETRY);
}

void
MINGW_signal_cleanup (void)
{
  w32_set_quiet_shutdown ();
}

static void
w32_init (void)
{
  w32_set_octave_home ();

  command_editor::prefer_env_winsize (true);
}

static bool
w32_shell_execute (const std::string& file)
{
}
#endif

DEFUN (__open_with_system_app__, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Loadable Function} {} __open_with_system_app__ (@var{file})\n\
Undocumented internal function.\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 1)
    {
      std::string file = args(0).string_value ();

      if (! error_state)
        {
#if defined (__WIN32__) && ! defined (_POSIX_VERSION)
          HINSTANCE status = ShellExecute (0, 0, file.c_str (), 0, 0,
                                           SW_SHOWNORMAL);

          // ShellExecute returns a value greater than 32 if successful.
          retval = (reinterpret_cast<ptrdiff_t> (status) > 32);
#else
          octave_value_list tmp
            = Fsystem (ovl ("xdg-open " + file + " 2> /dev/null",
                            false, "async"),
                       1);

          retval = (tmp(0).double_value () == 0);
#endif
        }
      else
        error ("__open_with_system_app__: argument must be a file name");
    }
  else
    print_usage ();

  return retval;
}

#if defined (__MINGW32__)
static void
MINGW_init (void)
{
  w32_init ();
}
#endif

#if defined (_MSC_VER)
static void
MSVC_init (void)
{
  w32_init ();
}
#endif


// Return TRUE if FILE1 and FILE2 refer to the same (physical) file.

bool
same_file_internal (const std::string& file1, const std::string& file2)
{
#ifdef OCTAVE_USE_WINDOWS_API

  bool retval = false;

  const char *f1 = file1.c_str ();
  const char *f2 = file2.c_str ();

  bool f1_is_dir = GetFileAttributes (f1) & FILE_ATTRIBUTE_DIRECTORY;
  bool f2_is_dir = GetFileAttributes (f2) & FILE_ATTRIBUTE_DIRECTORY;

  // Windows native code
  // Reference: http://msdn2.microsoft.com/en-us/library/aa363788.aspx

  DWORD share = FILE_SHARE_DELETE | FILE_SHARE_READ | FILE_SHARE_WRITE;

  HANDLE hfile1
    = CreateFile (f1, 0, share, 0, OPEN_EXISTING,
                  f1_is_dir ? FILE_FLAG_BACKUP_SEMANTICS : 0, 0);

  if (hfile1 != INVALID_HANDLE_VALUE)
    {
      HANDLE hfile2
        = CreateFile (f2, 0, share, 0, OPEN_EXISTING,
                      f2_is_dir ? FILE_FLAG_BACKUP_SEMANTICS : 0, 0);

      if (hfile2 != INVALID_HANDLE_VALUE)
        {
          BY_HANDLE_FILE_INFORMATION hfi1;
          BY_HANDLE_FILE_INFORMATION hfi2;

          if (GetFileInformationByHandle (hfile1, &hfi1)
              && GetFileInformationByHandle (hfile2, &hfi2))
            {
              retval = (hfi1.dwVolumeSerialNumber == hfi2.dwVolumeSerialNumber
                        && hfi1.nFileIndexHigh == hfi2.nFileIndexHigh
                        && hfi1.nFileIndexLow == hfi2.nFileIndexLow);
            }

          CloseHandle (hfile2);
        }

      CloseHandle (hfile1);
    }

  return retval;

#else

  // POSIX Code

  file_stat fs_file1 (file1);
  file_stat fs_file2 (file2);

  return (fs_file1 && fs_file2
          && fs_file1.ino () == fs_file2.ino ()
          && fs_file1.dev () == fs_file2.dev ());

#endif
}

void
sysdep_init (void)
{
#if defined (HAVE_OPENMP)
#if defined (HAVE_OMP_GET_NUM_THREADS)
  omp_get_num_threads ();
#endif
#endif

#if defined (__386BSD__) || defined (__FreeBSD__) || defined (__NetBSD__)
  BSD_init ();
#elif defined (__MINGW32__)
  MINGW_init ();
#elif defined (_MSC_VER)
  MSVC_init ();
#endif
}

void
sysdep_cleanup (void)
{
  MINGW_SIGNAL_CLEANUP ();
}

// Set terminal in raw mode.  From less-177.
//
// Change terminal to "raw mode", or restore to "normal" mode.
// "Raw mode" means
//      1. An outstanding read will complete on receipt of a single keystroke.
//      2. Input is not echoed.
//      3. On output, \n is mapped to \r\n.
//      4. \t is NOT expanded into spaces.
//      5. Signal-causing characters such as ctrl-C (interrupt),
//         etc. are NOT disabled.
// It doesn't matter whether an input \n is mapped to \r, or vice versa.

void
raw_mode (bool on, bool wait)
{
  static bool curr_on = false;

  int tty_fd = STDIN_FILENO;
  if (! gnulib::isatty (tty_fd))
    {
      if (interactive && ! forced_interactive)
        error ("stdin is not a tty!");
      return;
    }

  if (on == curr_on)
    return;

#if defined (HAVE_TERMIOS_H)
  {
    struct termios s;
    static struct termios save_term;

    if (on)
      {
        // Get terminal modes.

        tcgetattr (tty_fd, &s);

        // Save modes and set certain variables dependent on modes.

        save_term = s;
//      ospeed = s.c_cflag & CBAUD;
//      erase_char = s.c_cc[VERASE];
//      kill_char = s.c_cc[VKILL];

        // Set the modes to the way we want them.

        s.c_lflag &= ~(ICANON|ECHO|ECHOE|ECHOK|ECHONL);
        s.c_oflag |=  (OPOST|ONLCR);
#if defined (OCRNL)
        s.c_oflag &= ~(OCRNL);
#endif
#if defined (ONOCR)
        s.c_oflag &= ~(ONOCR);
#endif
#if defined (ONLRET)
        s.c_oflag &= ~(ONLRET);
#endif
        s.c_cc[VMIN] = wait ? 1 : 0;
        s.c_cc[VTIME] = 0;
      }
    else
      {
        // Restore saved modes.

        s = save_term;
      }

    tcsetattr (tty_fd, wait ? TCSAFLUSH : TCSADRAIN, &s);
  }
#elif defined (HAVE_TERMIO_H)
  {
    struct termio s;
    static struct termio save_term;

    if (on)
      {
        // Get terminal modes.

        ioctl (tty_fd, TCGETA, &s);

        // Save modes and set certain variables dependent on modes.

        save_term = s;
//      ospeed = s.c_cflag & CBAUD;
//      erase_char = s.c_cc[VERASE];
//      kill_char = s.c_cc[VKILL];

        // Set the modes to the way we want them.

        s.c_lflag &= ~(ICANON|ECHO|ECHOE|ECHOK|ECHONL);
        s.c_oflag |=  (OPOST|ONLCR);
#if defined (OCRNL)
        s.c_oflag &= ~(OCRNL);
#endif
#if defined (ONOCR)
        s.c_oflag &= ~(ONOCR);
#endif
#if defined (ONLRET)
        s.c_oflag &= ~(ONLRET);
#endif
        s.c_cc[VMIN] = wait ? 1 : 0;
      }
    else
      {
        // Restore saved modes.

        s = save_term;
      }

    ioctl (tty_fd, TCSETAW, &s);
  }
#elif defined (HAVE_SGTTY_H)
  {
    struct sgttyb s;
    static struct sgttyb save_term;

    if (on)
      {
        // Get terminal modes.

        ioctl (tty_fd, TIOCGETP, &s);

        // Save modes and set certain variables dependent on modes.

        save_term = s;
//      ospeed = s.sg_ospeed;
//      erase_char = s.sg_erase;
//      kill_char = s.sg_kill;

        // Set the modes to the way we want them.

        s.sg_flags |= CBREAK;
        s.sg_flags &= ~(ECHO);
      }
    else
      {
        // Restore saved modes.

        s = save_term;
      }

    ioctl (tty_fd, TIOCSETN, &s);
  }
#else
  warning ("no support for raw mode console I/O on this system");

  // Make sure the current mode doesn't toggle.
  on = curr_on;
#endif

  curr_on = on;
}

FILE *
octave_popen (const char *command, const char *mode)
{
#if defined (__MINGW32__) || defined (_MSC_VER)
  if (mode && mode[0] && ! mode[1])
    {
      char tmode[3];
      tmode[0] = mode[0];
      tmode[1] = 'b';
      tmode[2] = 0;

      return _popen (command, tmode);
    }
  else
    return _popen (command, mode);
#else
  return popen (command, mode);
#endif
}

int
octave_pclose (FILE *f)
{
#if defined (__MINGW32__) || defined (_MSC_VER)
  return _pclose (f);
#else
  return pclose (f);
#endif
}

// Read one character from the terminal.

int
octave_kbhit (bool wait)
{
#if defined (HAVE__KBHIT) && defined (HAVE__GETCH)
  // This essentially means we are on a Windows system.
  int c;

  if (wait)
    c = _getch ();
  else
    c = (! _kbhit ()) ? 0 : _getch ();

#else
  raw_mode (true, wait);

  // Get current handler.
  octave_interrupt_handler saved_interrupt_handler
    = octave_ignore_interrupts ();

  // Restore it, disabling system call restarts (if possible) so the
  // read can be interrupted.

  octave_set_interrupt_handler (saved_interrupt_handler, false);

  int c = std::cin.get ();

  if (std::cin.fail () || std::cin.eof ())
    std::cin.clear ();

  // Restore it, enabling system call restarts (if possible).
  octave_set_interrupt_handler (saved_interrupt_handler, true);

  raw_mode (false, true);
#endif

  return c;
}

std::string
get_P_tmpdir (void)
{
#if defined (__WIN32__) && ! defined (_POSIX_VERSION)

  std::string retval;

#if defined (P_tmpdir)
  retval = P_tmpdir;
#endif

  // Apparently some versions of MinGW and MSVC either don't define
  // P_tmpdir, or they define it to a single backslash, neither of which
  // is particularly helpful.

  if (retval.empty () || retval == "\\")
    {
      retval = octave_env::getenv ("TEMP");

      if (retval.empty ())
        retval = octave_env::getenv ("TMP");

      if (retval.empty ())
        retval = "c:\\temp";
    }

  return retval;

#elif defined (P_tmpdir)

  return P_tmpdir;

#else

  return "/tmp";

#endif
}

DEFUN (clc, , ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} clc ()\n\
@deftypefnx {Built-in Function} {} home ()\n\
Clear the terminal screen and move the cursor to the upper left corner.\n\
@end deftypefn")
{
  bool skip_redisplay = true;

  command_editor::clear_screen (skip_redisplay);

  return octave_value_list ();
}

DEFALIAS (home, clc);

DEFUN (getenv, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} getenv (@var{var})\n\
Return the value of the environment variable @var{var}.\n\
\n\
For example,\n\
\n\
@example\n\
getenv (\"PATH\")\n\
@end example\n\
\n\
@noindent\n\
returns a string containing the value of your path.\n\
@seealso{setenv, unsetenv}\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 1)
    {
      std::string name = args(0).string_value ();

      if (! error_state)
        retval = octave_env::getenv (name);
    }
  else
    print_usage ();

  return retval;
}

/*
%!assert (ischar (getenv ("OCTAVE_HOME")))
*/

DEFUN (setenv, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} setenv (@var{var}, @var{value})\n\
@deftypefnx {Built-in Function} {} setenv (@var{var})\n\
@deftypefnx {Built-in Function} {} putenv (@dots{})\n\
Set the value of the environment variable @var{var} to @var{value}.\n\
\n\
If no @var{value} is specified then the variable will be assigned the null\n\
string.\n\
@seealso{unsetenv, getenv}\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (nargin == 2 || nargin == 1)
    {
      if (args(0).is_string ())
        {
          std::string var = args(0).string_value ();

          std::string val = (nargin == 2
                             ? args(1).string_value () : std::string ());

          if (! error_state)
            octave_env::putenv (var, val);
          else
            error ("setenv: VALUE must be a string");
        }
      else
        error ("setenv: VAR must be a string");
    }
  else
    print_usage ();

  return retval;
}

DEFALIAS (putenv, setenv);

/*
%!test
%! setenv ("dummy_variable_that_cannot_matter", "foobar");
%! assert (getenv ("dummy_variable_that_cannot_matter"), "foobar");
%! unsetenv ("dummy_variable_that_cannot_matter");
%! assert (getenv ("dummy_variable_that_cannot_matter"), "");
*/

DEFUN (unsetenv, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {@var{status} =} unsetenv (@var{var})\n\
Delete the environment variable @var{var}.\n\
\n\
Return 0 if the variable was deleted, or did not exist, and -1 if an error\n\
occurred.\n\
@seealso{setenv, getenv}\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 1)
    {
      std::string tmp = args(0).string_value ();

      if (! error_state)
        {
          int status = gnulib::unsetenv (tmp.c_str ());
          retval = status;
        }
    }
  else
    print_usage ();

  return retval;
}

/*
## Test for unsetenv is in setenv test
*/

// FIXME: perhaps kbhit should also be able to print a prompt?

DEFUN (kbhit, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} kbhit ()\n\
@deftypefnx {Built-in Function} {} kbhit (1)\n\
Read a single keystroke from the keyboard.\n\
\n\
If called with an argument, don't wait for a keypress.\n\
\n\
For example,\n\
\n\
@example\n\
x = kbhit ();\n\
@end example\n\
\n\
@noindent\n\
will set @var{x} to the next character typed at the keyboard as soon as\n\
it is typed.\n\
\n\
@example\n\
x = kbhit (1);\n\
@end example\n\
\n\
@noindent\n\
is identical to the above example, but doesn't wait for a keypress,\n\
returning the empty string if no key is available.\n\
@seealso{input, pause}\n\
@end deftypefn")
{
  octave_value retval;

  // FIXME: add timeout and default value args?

  if (interactive)
    {
      Fdrawnow ();

      int c = octave_kbhit (args.length () == 0);

      if (c == -1)
        c = 0;

      char s[2] = { static_cast<char> (c), '\0' };

      retval = s;
    }

  return retval;
}

DEFUN (pause, args, ,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {} pause ()\n\
@deftypefnx {Built-in Function} {} pause (@var{n})\n\
Suspend the execution of the program for @var{n} seconds.\n\
\n\
@var{n} is a positive real value and may be a fraction of a second.\n\
\n\
If invoked without an input arguments then the program is suspended until a\n\
character is typed.\n\
\n\
The following example prints a message and then waits 5 seconds before\n\
clearing the screen.\n\
\n\
@example\n\
@group\n\
fprintf (stderr, \"wait please...\\n\");\n\
pause (5);\n\
clc;\n\
@end group\n\
@end example\n\
@seealso{kbhit, sleep}\n\
@end deftypefn")
{
  octave_value_list retval;

  int nargin = args.length ();

  if (! (nargin == 0 || nargin == 1))
    {
      print_usage ();
      return retval;
    }

  if (nargin == 1)
    {
      double dval = args(0).double_value ();

      if (! error_state)
        {
          if (! xisnan (dval))
            {
              Fdrawnow ();

              if (xisinf (dval))
                {
                  flush_octave_stdout ();
                  octave_kbhit ();
                }
              else
                octave_sleep (dval);
            }
          else
            warning ("pause: NaN is an invalid delay");
        }
    }
  else
    {
      Fdrawnow ();
      flush_octave_stdout ();
      octave_kbhit ();
    }

  return retval;
}

/*
%!test
%! pause (1);

%!error (pause (1, 2))
*/

DEFUN (sleep, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} sleep (@var{seconds})\n\
Suspend the execution of the program for the given number of seconds.\n\
@seealso{usleep, pause}\n\
@end deftypefn")
{
  octave_value_list retval;

  if (args.length () == 1)
    {
      double dval = args(0).double_value ();

      if (! error_state)
        {
          if (xisnan (dval))
            warning ("sleep: NaN is an invalid delay");
          else
            {
              Fdrawnow ();
              octave_sleep (dval);
            }
        }
    }
  else
    print_usage ();

  return retval;
}

/*
%!test
%! sleep (1);

%!error (sleep ())
%!error (sleep (1, 2))
*/

DEFUN (usleep, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} usleep (@var{microseconds})\n\
Suspend the execution of the program for the given number of\n\
microseconds.\n\
\n\
On systems where it is not possible to sleep for periods of time less than\n\
one second, @code{usleep} will pause the execution for @code{round\n\
(@var{microseconds} / 1e6)} seconds.\n\
@seealso{sleep, pause}\n\
@end deftypefn")
{
  octave_value_list retval;

  if (args.length () == 1)
    {
      double dval = args(0).double_value ();

      if (! error_state)
        {
          if (xisnan (dval))
            warning ("usleep: NaN is an invalid delay");
          else
            {
              Fdrawnow ();

              int delay = NINT (dval);

              if (delay > 0)
                octave_usleep (delay);
            }
        }
    }
  else
    print_usage ();

  return retval;
}

/*
%!test
%! usleep (1000);

%!error (usleep ())
%!error (usleep (1, 2))
*/

// FIXME: maybe this should only return 1 if IEEE floating
// point functions really work.

DEFUN (isieee, , ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} isieee ()\n\
Return true if your computer @emph{claims} to conform to the IEEE standard\n\
for floating point calculations.\n\
\n\
No actual tests are performed.\n\
@end deftypefn")
{
  oct_mach_info::float_format flt_fmt = oct_mach_info::native_float_format ();

  return octave_value (flt_fmt == oct_mach_info::flt_fmt_ieee_little_endian
                       || flt_fmt == oct_mach_info::flt_fmt_ieee_big_endian);
}

/*
%!assert (islogical (isieee ()))
*/

DEFUN (native_float_format, , ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} native_float_format ()\n\
Return the native floating point format as a string.\n\
@end deftypefn")
{
  oct_mach_info::float_format flt_fmt = oct_mach_info::native_float_format ();

  return octave_value (oct_mach_info::float_format_as_string (flt_fmt));
}

/*
%!assert (ischar (native_float_format ()))
*/

DEFUN (tilde_expand, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} tilde_expand (@var{string})\n\
Perform tilde expansion on @var{string}.\n\
\n\
If @var{string} begins with a tilde character, (@samp{~}), all of the\n\
characters preceding the first slash (or all characters, if there is no\n\
slash) are treated as a possible user name, and the tilde and the following\n\
characters up to the slash are replaced by the home directory of the named\n\
user.  If the tilde is followed immediately by a slash, the tilde is\n\
replaced by the home directory of the user running Octave.\n\
\n\
For example:\n\
\n\
@example\n\
@group\n\
tilde_expand (\"~joeuser/bin\")\n\
     @result{} \"/home/joeuser/bin\"\n\
tilde_expand (\"~/bin\")\n\
     @result{} \"/home/jwe/bin\"\n\
@end group\n\
@end example\n\
@end deftypefn")
{
  octave_value retval;

  int nargin = args.length ();

  if (nargin == 1)
    {
      octave_value arg = args(0);

      string_vector sv = arg.all_strings ();

      if (! error_state)
        {
          sv = file_ops::tilde_expand (sv);

          if (arg.is_cellstr ())
            retval = Cell (arg.dims (), sv);
          else
            retval = sv;
        }
      else
        error ("tilde_expand: expecting argument to be char or cellstr object");
    }
  else
    print_usage ();

  return retval;
}

/*
%!test
%! home = get_home_directory ();
%! assert (tilde_expand ("~/foobar"), [home "/foobar"]);
%! assert (tilde_expand ("/foo/bar"), "/foo/bar");
%! assert (tilde_expand ("foo/bar"), "foo/bar");
*/

DEFUN (get_home_directory, , ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {@var{homedir} =} get_home_directory ()\n\
Return the current home directory.\n\
\n\
On most systems, this is equivalent to @code{getenv (\"HOME\")}.  On Windows\n\
systems, if the environment variable @env{HOME} is not set then it is\n\
equivalent to\n\
@code{fullfile (getenv (\"HOMEDRIVE\"), getenv (\"HOMEPATH\"))}\n\
@seealso{getenv}\n\
@end deftypefn")
{
  return octave_value (octave_env::get_home_directory ());
}

/*
%!test
%! if (! ispc ())
%!   assert (get_home_directory (), getenv ("HOME"));
%! endif
*/

// This function really belongs in display.cc, but including defun.h in
// that file results in conflicts with symbols from headers that are
// needed for X11 and Carbon functions.

DEFUN (have_window_system, , ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} have_window_system ()\n\
Return true if a window system is available (X11, Windows, or Apple OS X)\n\
and false otherwise.\n\
@seealso{isguirunning}\n\
@end deftypefn")
{
  return octave_value (display_info::display_available ());
}
