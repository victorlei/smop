/*

Copyright (C) 2000-2015 John W. Eaton

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

#include <signal.h>

#include "siglist.h"

/* The following is all borrowed from Emacs.  */

#if ! (defined HAVE_STRSIGNAL || HAVE_DECL_SYS_SIGLIST)

static char *my_sys_siglist[NSIG];

#ifdef sys_siglist
#undef sys_siglist
#endif
#define sys_siglist my_sys_siglist

#endif

void
init_signals (void)
{
#if ! (defined HAVE_STRSIGNAL || HAVE_DECL_SYS_SIGLIST)

  static int initialized = 0;

  if (! initialized)
    {
      initialized = 1;

# ifdef SIGABRT
      sys_siglist[SIGABRT] = "Aborted";
# endif
# ifdef SIGAIO
      sys_siglist[SIGAIO] = "LAN I/O interrupt";
# endif
# ifdef SIGALRM
      sys_siglist[SIGALRM] = "Alarm clock";
# endif
# ifdef SIGBUS
      sys_siglist[SIGBUS] = "Bus error";
# endif
# ifdef SIGCLD
      sys_siglist[SIGCLD] = "Child status changed";
# endif
# ifdef SIGCHLD
      sys_siglist[SIGCHLD] = "Child status changed";
# endif
# ifdef SIGCONT
      sys_siglist[SIGCONT] = "Continued";
# endif
# ifdef SIGDANGER
      sys_siglist[SIGDANGER] = "Swap space dangerously low";
# endif
# ifdef SIGDGNOTIFY
      sys_siglist[SIGDGNOTIFY] = "Notification message in queue";
# endif
# ifdef SIGEMT
      sys_siglist[SIGEMT] = "Emulation trap";
# endif
# ifdef SIGFPE
      sys_siglist[SIGFPE] = "Arithmetic exception";
# endif
# ifdef SIGFREEZE
      sys_siglist[SIGFREEZE] = "SIGFREEZE";
# endif
# ifdef SIGGRANT
      sys_siglist[SIGGRANT] = "Monitor mode granted";
# endif
# ifdef SIGHUP
      sys_siglist[SIGHUP] = "Hangup";
# endif
# ifdef SIGILL
      sys_siglist[SIGILL] = "Illegal instruction";
# endif
# ifdef SIGINT
      sys_siglist[SIGINT] = "Interrupt";
# endif
# ifdef SIGIO
      sys_siglist[SIGIO] = "I/O possible";
# endif
# ifdef SIGIOINT
      sys_siglist[SIGIOINT] = "I/O intervention required";
# endif
# ifdef SIGIOT
      sys_siglist[SIGIOT] = "IOT trap";
# endif
# ifdef SIGKILL
      sys_siglist[SIGKILL] = "Killed";
# endif
# ifdef SIGLOST
      sys_siglist[SIGLOST] = "Resource lost";
# endif
# ifdef SIGLWP
      sys_siglist[SIGLWP] = "SIGLWP";
# endif
# ifdef SIGMSG
      sys_siglist[SIGMSG] = "Monitor mode data available";
# endif
# ifdef SIGPHONE
      sys_siglist[SIGPHONE] = "SIGPHONE";
# endif
# ifdef SIGPIPE
      sys_siglist[SIGPIPE] = "Broken pipe";
# endif
# ifdef SIGPOLL
      sys_siglist[SIGPOLL] = "Pollable event occurred";
# endif
# ifdef SIGPROF
      sys_siglist[SIGPROF] = "Profiling timer expired";
# endif
# ifdef SIGPTY
      sys_siglist[SIGPTY] = "PTY I/O interrupt";
# endif
# ifdef SIGPWR
      sys_siglist[SIGPWR] = "Power-fail restart";
# endif
# ifdef SIGQUIT
      sys_siglist[SIGQUIT] = "Quit";
# endif
# ifdef SIGRETRACT
      sys_siglist[SIGRETRACT] = "Need to relinguish monitor mode";
# endif
# ifdef SIGSAK
      sys_siglist[SIGSAK] = "Secure attention";
# endif
# ifdef SIGSEGV
      sys_siglist[SIGSEGV] = "Segmentation violation";
# endif
# ifdef SIGSOUND
      sys_siglist[SIGSOUND] = "Sound completed";
# endif
# ifdef SIGSTKFLT
      sys_siglist[SIGSTKFLT] = "Stack fault";
# endif
# ifdef SIGSTOP
      sys_siglist[SIGSTOP] = "Stopped (signal)";
# endif
# ifdef SIGSTP
      sys_siglist[SIGSTP] = "Stopped (user)";
# endif
# ifdef SIGSYS
      sys_siglist[SIGSYS] = "Bad argument to system call";
# endif
# ifdef SIGTERM
      sys_siglist[SIGTERM] = "Terminated";
# endif
# ifdef SIGTHAW
      sys_siglist[SIGTHAW] = "SIGTHAW";
# endif
# ifdef SIGTRAP
      sys_siglist[SIGTRAP] = "Trace/breakpoint trap";
# endif
# ifdef SIGTSTP
      sys_siglist[SIGTSTP] = "Stopped (user)";
# endif
# ifdef SIGTTIN
      sys_siglist[SIGTTIN] = "Stopped (tty input)";
# endif
# ifdef SIGTTOU
      sys_siglist[SIGTTOU] = "Stopped (tty output)";
# endif
# ifdef SIGUNUSED
      sys_siglist[SIGUNUSED] = "SIGUNUSED";
# endif
# ifdef SIGURG
      sys_siglist[SIGURG] = "Urgent I/O condition";
# endif
# ifdef SIGUSR1
      sys_siglist[SIGUSR1] = "User defined signal 1";
# endif
# ifdef SIGUSR2
      sys_siglist[SIGUSR2] = "User defined signal 2";
# endif
# ifdef SIGVTALRM
      sys_siglist[SIGVTALRM] = "Virtual timer expired";
# endif
# ifdef SIGWAITING
      sys_siglist[SIGWAITING] = "Process's LWPs are blocked";
# endif
# ifdef SIGWINCH
      sys_siglist[SIGWINCH] = "Window size changed";
# endif
# ifdef SIGWIND
      sys_siglist[SIGWIND] = "SIGWIND";
# endif
# ifdef SIGXCPU
      sys_siglist[SIGXCPU] = "CPU time limit exceeded";
# endif
# ifdef SIGXFSZ
      sys_siglist[SIGXFSZ] = "File size limit exceeded";
# endif
    }

#endif
}

#if ! defined (HAVE_STRSIGNAL)

char *
strsignal (int code)
{
  char *signame = "";

  if (0 <= code && code < NSIG)
    {
      /* Cast to suppress warning if the table has const char *.  */
      signame = (char *) sys_siglist[code];
    }

  return signame;
}

#endif
