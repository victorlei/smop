// %NO_EDIT_WARNING%
/*

Copyright (C) 2012-2015 John W. Eaton

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

// NOTE: This program is supposed to be a small wrapper that exists
// primarily to give up the controlling TTY and then exec Octave with
// its GUI.  It may also execute Octave without the GUI or the command
// line version of Octave that is not linked with GUI libraries.  So
// that it remains small, it should NOT depend on or be linked with
// liboctave or libinterp.

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <cstdlib>
#include <cstring>

#include <algorithm>
#include <iostream>
#include <string>

#include <sys/types.h>
#include <unistd.h>

#ifndef OCTAVE_VERSION
#define OCTAVE_VERSION %OCTAVE_VERSION%
#endif

#ifndef OCTAVE_ARCHLIBDIR
#define OCTAVE_ARCHLIBDIR %OCTAVE_ARCHLIBDIR%
#endif

#ifndef OCTAVE_BINDIR
#define OCTAVE_BINDIR %OCTAVE_BINDIR%
#endif

#ifndef OCTAVE_PREFIX
#define OCTAVE_PREFIX %OCTAVE_PREFIX%
#endif

#include "display-available.h"
#include "shared-fcns.h"

#include <cstdlib>

#if (defined (HAVE_OCTAVE_GUI) \
     && ! defined (__WIN32__) || defined (__CYGWIN__))

#include <signal.h>
#include <fcntl.h>

// This is a liboctave header, but it doesn't include any other Octave
// headers or declare any functions that are defined in liboctave.
#include "syswait.h"

typedef void sig_handler (int);

// Forward signals to the GUI process.

static pid_t gui_pid = 0;

static int caught_signal = -1;

static void
gui_driver_sig_handler (int sig)
{
  if (gui_pid > 0)
    caught_signal = sig;
}

static sig_handler *
octave_set_signal_handler (int sig, sig_handler *handler)
{
  struct sigaction act, oact;

  act.sa_handler = handler;
  act.sa_flags = 0;

  gnulib::sigemptyset (&act.sa_mask);
  gnulib::sigemptyset (&oact.sa_mask);

  gnulib::sigaction (sig, &act, &oact);

  return oact.sa_handler;
}

static void
install_signal_handlers (void)
{

#ifdef SIGINT
  octave_set_signal_handler (SIGINT, gui_driver_sig_handler);
#endif

#ifdef SIGBREAK
  octave_set_signal_handler (SIGBREAK, gui_driver_sig_handler);
#endif

#ifdef SIGABRT
  octave_set_signal_handler (SIGABRT, gui_driver_sig_handler);
#endif

#ifdef SIGALRM
  octave_set_signal_handler (SIGALRM, gui_driver_sig_handler);
#endif

#ifdef SIGBUS
  octave_set_signal_handler (SIGBUS, gui_driver_sig_handler);
#endif

  // SIGCHLD
  // SIGCLD
  // SIGCONT

#ifdef SIGEMT
  octave_set_signal_handler (SIGEMT, gui_driver_sig_handler);
#endif

#ifdef SIGFPE
  octave_set_signal_handler (SIGFPE, gui_driver_sig_handler);
#endif

#ifdef SIGHUP
  octave_set_signal_handler (SIGHUP, gui_driver_sig_handler);
#endif

#ifdef SIGILL
  octave_set_signal_handler (SIGILL, gui_driver_sig_handler);
#endif

  // SIGINFO
  // SIGINT

#ifdef SIGIOT
  octave_set_signal_handler (SIGIOT, gui_driver_sig_handler);
#endif

#ifdef SIGLOST
  octave_set_signal_handler (SIGLOST, gui_driver_sig_handler);
#endif

#ifdef SIGPIPE
  octave_set_signal_handler (SIGPIPE, gui_driver_sig_handler);
#endif

#ifdef SIGPOLL
  octave_set_signal_handler (SIGPOLL, gui_driver_sig_handler);
#endif

  // SIGPROF
  // SIGPWR

#ifdef SIGQUIT
  octave_set_signal_handler (SIGQUIT, gui_driver_sig_handler);
#endif

#ifdef SIGSEGV
  octave_set_signal_handler (SIGSEGV, gui_driver_sig_handler);
#endif

  // SIGSTOP

#ifdef SIGSYS
  octave_set_signal_handler (SIGSYS, gui_driver_sig_handler);
#endif

#ifdef SIGTERM
  octave_set_signal_handler (SIGTERM, gui_driver_sig_handler);
#endif

#ifdef SIGTRAP
  octave_set_signal_handler (SIGTRAP, gui_driver_sig_handler);
#endif

  // SIGTSTP
  // SIGTTIN
  // SIGTTOU
  // SIGURG

#ifdef SIGUSR1
  octave_set_signal_handler (SIGUSR1, gui_driver_sig_handler);
#endif

#ifdef SIGUSR2
  octave_set_signal_handler (SIGUSR2, gui_driver_sig_handler);
#endif

#ifdef SIGVTALRM
  octave_set_signal_handler (SIGVTALRM, gui_driver_sig_handler);
#endif

#ifdef SIGIO
  octave_set_signal_handler (SIGIO, gui_driver_sig_handler);
#endif

  // SIGWINCH

#ifdef SIGXCPU
  octave_set_signal_handler (SIGXCPU, gui_driver_sig_handler);
#endif

#ifdef SIGXFSZ
  octave_set_signal_handler (SIGXFSZ, gui_driver_sig_handler);
#endif

}

static bool
have_controlling_terminal (void)
{
  int retval = false;

#if defined (HAVE_CTERMID)
  const char *ctty = ctermid (0);
#else
  const char *ctty = "/dev/tty";
#endif

  int fd = gnulib::open (ctty, O_RDWR, 0);

  if (fd >= 0)
    {
      gnulib::close (fd);

      retval = true;
    }

  return retval;
}

#endif

static std::string
get_octave_bindir (void)
{
  // Accept value from the environment literally, but substitute
  // OCTAVE_HOME in the configuration value OCTAVE_BINDIR in case Octave
  // has been relocated to some installation directory other than the
  // one originally configured.

  std::string obd = octave_getenv ("OCTAVE_BINDIR");

  return obd.empty () ? subst_octave_home (std::string (OCTAVE_BINDIR)) : obd;
}

static std::string
get_octave_archlibdir (void)
{
  // Accept value from the environment literally, but substitute
  // OCTAVE_HOME in the configuration value OCTAVE_ARCHLIBDIR in case
  // Octave has been relocated to some installation directory other than
  // the one originally configured.

  std::string dir = octave_getenv ("OCTAVE_ARCHLIBDIR");

  return dir.empty () ? subst_octave_home (std::string (OCTAVE_ARCHLIBDIR))
                      : dir;
}

// Adapted from libtool wrapper.
#if defined (__WIN32__) && ! defined (__CYGWIN__)

/* Prepares an argument vector before calling spawn().
   Note that spawn() does not by itself call the command interpreter
     (getenv ("COMSPEC") != NULL ? getenv ("COMSPEC") :
      ({ OSVERSIONINFO v; v.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
         GetVersionEx(&v);
         v.dwPlatformId == VER_PLATFORM_WIN32_NT;
      }) ? "cmd.exe" : "command.com").
   Instead it simply concatenates the arguments, separated by ' ', and calls
   CreateProcess().  We must quote the arguments since Win32 CreateProcess()
   interprets characters like ' ', '\t', '\\', '"' (but not '<' and '>') in a
   special way:
   - Space and tab are interpreted as delimiters. They are not treated as
     delimiters if they are surrounded by double quotes: "...".
   - Unescaped double quotes are removed from the input. Their only effect is
     that within double quotes, space and tab are treated like normal
     characters.
   - Backslashes not followed by double quotes are not special.
   - But 2*n+1 backslashes followed by a double quote become
     n backslashes followed by a double quote (n >= 0):
       \" -> "
       \\\" -> \"
       \\\\\" -> \\"
 */
#define SHELL_SPECIAL_CHARS "\"\\ \001\002\003\004\005\006\007\010\011\012\013\014\015\016\017\020\021\022\023\024\025\026\027\030\031\032\033\034\035\036\037"
#define SHELL_SPACE_CHARS " \001\002\003\004\005\006\007\010\011\012\013\014\015\016\017\020\021\022\023\024\025\026\027\030\031\032\033\034\035\036\037"
char **
prepare_spawn (char **argv)
{
  size_t argc;
  char **new_argv;
  size_t i;

  /* Count number of arguments.  */
  for (argc = 0; argv[argc] != NULL; argc++)
    ;

  /* Allocate new argument vector.  */
  new_argv = new char* [argc + 1];

  /* Put quoted arguments into the new argument vector.  */
  for (i = 0; i < argc; i++)
    {
      const char *string = argv[i];

      if (string[0] == '\0')
        new_argv[i] = strdup ("\"\"");
      else if (strpbrk (string, SHELL_SPECIAL_CHARS) != NULL)
        {
          int quote_around = (strpbrk (string, SHELL_SPACE_CHARS) != NULL);
          size_t length;
          unsigned int backslashes;
          const char *s;
          char *quoted_string;
          char *p;

          length = 0;
          backslashes = 0;
          if (quote_around)
            length++;
          for (s = string; *s != '\0'; s++)
            {
              char c = *s;
              if (c == '"')
                length += backslashes + 1;
              length++;
              if (c == '\\')
                backslashes++;
              else
                backslashes = 0;
            }
          if (quote_around)
            length += backslashes + 1;

          quoted_string = new char [length + 1];

          p = quoted_string;
          backslashes = 0;
          if (quote_around)
            *p++ = '"';
          for (s = string; *s != '\0'; s++)
            {
              char c = *s;
              if (c == '"')
                {
                  unsigned int j;
                  for (j = backslashes + 1; j > 0; j--)
                    *p++ = '\\';
                }
              *p++ = c;
              if (c == '\\')
                backslashes++;
              else
                backslashes = 0;
            }
          if (quote_around)
            {
              unsigned int j;
              for (j = backslashes; j > 0; j--)
                *p++ = '\\';
              *p++ = '"';
            }
          *p = '\0';

          new_argv[i] = quoted_string;
        }
      else
        new_argv[i] = (char *) string;
    }
  new_argv[argc] = NULL;

  return new_argv;
}

#endif // __WIN32__ && ! __CYGWIN__

static int
octave_exec (const std::string& file, char **argv)
{
#if defined (__WIN32__) && ! defined (__CYGWIN__)
  argv = prepare_spawn (argv);
  return _spawnv (_P_WAIT, file.c_str (), argv);
#else
  execv (file.c_str (), argv);

  std::cerr << "octave: failed to exec '" << file << "'" << std::endl;

  return 1;
#endif
}

static char *
strsave (const char *s)
{
  if (! s)
    return 0;

  int len = strlen (s);
  char *tmp = new char [len+1];
  tmp = strcpy (tmp, s);
  return tmp;
}

int
main (int argc, char **argv)
{
  int retval = 0;

  bool start_gui = true;
  bool gui_libs = true;

  std::string octave_bindir = get_octave_bindir ();
  std::string octave_archlibdir = get_octave_archlibdir ();
  std::string octave_cli
    = octave_bindir + dir_sep_char + "octave-cli-" OCTAVE_VERSION;
  std::string octave_gui = octave_archlibdir + dir_sep_char + "octave-gui";

#if defined (HAVE_OCTAVE_GUI)
  // The Octave version number is already embedded in the
  // octave_archlibdir directory name so we don't need to append it to
  // the octave-gui file name.

  std::string file = octave_gui;
#else
  std::string file = octave_cli;
#endif

  char **new_argv = new char * [argc + 1];

  int k = 1;

  bool warn_display = true;

  for (int i = 1; i < argc; i++)
    {
      if (! strcmp (argv[i], "--no-gui-libs"))
        {
          // Run the version of Octave that is not linked with any GUI
          // libraries.  It may not be possible to do plotting or any
          // ui* calls, but it will be a little faster to start and
          // require less memory.  Don't pass the --no-gui-libs option
          // on as that option is not recognized by Octave.

          start_gui = false;
          gui_libs = false;
          file = octave_cli;
        }
      else if (! strcmp (argv[i], "--no-gui"))
        {
          // If we see this option, then we can just exec octave; we
          // don't have to create a child process and wait for it to
          // exit.  But do exec "octave-gui", not "octave-cli", because
          // even if the --no-gui option is given, we may be asked to do
          // some plotting or ui* calls.

          start_gui = false;
          new_argv[k++] = argv[i];
        }
      else if (! strcmp (argv[i], "--silent") || ! strcmp (argv[i], "--quiet")
               || ! strcmp (argv[i], "-q"))
        {
          warn_display = false;
          new_argv[k++] = argv[i];
        }
      else
        new_argv[k++] = argv[i];
    }

  new_argv[k] = 0;

  if (gui_libs || start_gui)
    {
      int dpy_avail;

      const char *display_check_err_msg = display_available (&dpy_avail);

      if (! dpy_avail)
        {
          start_gui = false;
          gui_libs = false;

          file = octave_cli;

          if (warn_display)
            {
              if (! display_check_err_msg)
                display_check_err_msg = "graphical display unavailable";

              std::cerr << "octave: " << display_check_err_msg << std::endl;
              std::cerr << "octave: disabling GUI features" << std::endl;
            }
        }
    }

#if defined (__WIN32__) && ! defined (__CYGWIN__)
  file += ".exe";
#endif

  new_argv[0] = strsave (file.c_str ());

#if (defined (HAVE_OCTAVE_GUI) \
     && ! defined (__WIN32__) || defined (__CYGWIN__))

  if (gui_libs && start_gui && have_controlling_terminal ())
    {
      install_signal_handlers ();

      gui_pid = fork ();

      if (gui_pid < 0)
        {
          std::cerr << "octave: fork failed!" << std::endl;

          retval = 1;
        }
      else if (gui_pid == 0)
        {
          // Child.

          if (setsid () < 0)
            {
              std::cerr << "octave: error calling setsid!" << std::endl;

              retval = 1;
            }
          else
            retval = octave_exec (file, new_argv);
        }
      else
        {
          // Parent.  Forward signals to child while waiting for it to exit.

          int status;

          while (true)
            {
              WAITPID (gui_pid, &status, 0);

              if (caught_signal > 0)
                {
                  int sig = caught_signal;

                  caught_signal = -1;

                  kill (gui_pid, sig);
                }
              else if (WIFEXITED (status))
                {
                  retval = WEXITSTATUS (status);
                  break;
                }
              else if (WIFSIGNALLED (status))
                {
                  std::cerr << "octave exited with signal "
                            << WTERMSIG (status) << std::endl;
                  break;
                }
            }
        }
    }
  else
    retval = octave_exec (file, new_argv);

#else

  retval = octave_exec (file, new_argv);

#endif

  return retval;
}

/*!
@mainpage Source code documentation for GNU Octave

GNU Octave is a high-level language, primarily intended for numerical
computations.  It provides a convenient interactive command line
interface for solving linear and nonlinear problems numerically, and
for performing other numerical experiments.  It may also be used as a
batch-oriented language for data processing.

GNU Octave is free software. You may redistribute it and/or modify it
under the terms of the <a href="http://www.gnu.org/licenses/">GNU
General Public License</a> as published by the Free Software Foundation.

This is the developer documentation for Octave's own source code. It is
intended to help for hacking Octave. It may also be useful for
understanding the Octave API when writing your own .oct files.
*/
