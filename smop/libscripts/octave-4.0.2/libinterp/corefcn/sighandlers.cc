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

#include <cstdlib>

#include <iostream>
#include <new>

#include <sys/types.h>
#include <unistd.h>

#include "cmd-edit.h"
#include "oct-syscalls.h"
#include "quit.h"
#include "singleton-cleanup.h"

#include "debug.h"
#include "defun.h"
#include "error.h"
#include "input.h"
#include "load-save.h"
#include "oct-map.h"
#include "pager.h"
#include "pt-bp.h"
#include "pt-eval.h"
#include "sighandlers.h"
#include "sysdep.h"
#include "syswait.h"
#include "toplev.h"
#include "utils.h"
#include "variables.h"

// Nonzero means we have already printed a message for this series of
// SIGPIPES.  We assume that the writer will eventually give up.
int pipe_handler_error_count = 0;

// TRUE means we can be interrupted.
bool can_interrupt = false;

// TRUE means we should try to enter the debugger on SIGINT.
bool Vdebug_on_interrupt = false;

// Allow users to avoid writing octave-workspace for SIGHUP (sent by
// closing gnome-terminal, for example).  Note that this variable has
// no effect if Vcrash_dumps_octave_core is FALSE.
static bool Vsighup_dumps_octave_core = true;

// Similar to Vsighup_dumps_octave_core, but for SIGTERM signal.
static bool Vsigterm_dumps_octave_core = true;

// List of signals we have caught since last call to octave_signal_handler.
static bool octave_signals_caught[NSIG];

// Forward declaration.
static void user_abort (const char *sig_name, int sig_number);

#if defined (__WIN32__) && ! defined (__CYGWIN__)

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

class
w32_interrupt_manager
{
public:
  ~w32_interrupt_manager (void)
  {
    if (thread)
      CloseHandle (thread);
  }

  static bool init (void) { return instance_ok (); }

  static void octave_jump_to_enclosing_context (void)
  {
    if (instance_ok ())
      instance->do_octave_jump_to_enclosing_context ();
  }

  static void user_abort (const char *sig_name, int sig_number)
  {
    if (instance_ok ())
      instance->do_user_abort (sig_name, sig_number);
  }

  static void raise_sigint (void)
  {
    if (instance_ok ())
      instance->do_raise_sigint ();
  }

private:
  w32_interrupt_manager (void)
    : thread (0), thread_id (0)
  {
    thread_id = GetCurrentThreadId ();

    DuplicateHandle (GetCurrentProcess (), GetCurrentThread (),
                     GetCurrentProcess (), &thread, 0, FALSE,
                     DUPLICATE_SAME_ACCESS);
  }

  static void octave_jump_to_enclosing_context_sync (void)
  {
#ifdef _MSC_VER
    _fpreset ();
#endif
    ::octave_jump_to_enclosing_context ();
  }

  void do_octave_jump_to_enclosing_context (void)
  {
    bool is_interrupt_thread = (GetCurrentThreadId () == thread_id);

    if (is_interrupt_thread)
      octave_jump_to_enclosing_context_sync ();
    else
      {
        // 64-bit Windows does not appear to have threadContext.Eip.
        // Something else must be done here to allow interrupts to
        // properly work across threads.

#if ! (defined (__MINGW64__) || defined (_WIN64))

        CONTEXT threadContext;

        SuspendThread (thread);
        threadContext.ContextFlags = CONTEXT_CONTROL;
        GetThreadContext (thread, &threadContext);
        threadContext.Eip = (DWORD) octave_jump_to_enclosing_context_sync;
        SetThreadContext (thread, &threadContext);
        ResumeThread (thread);
#endif
      }
  }

  void do_user_abort (const char *sig_name, int sig_number)
  {
    bool is_interrupt_thread = (GetCurrentThreadId () == thread_id);

    if (is_interrupt_thread)
      ::user_abort (sig_name, sig_number);
    else
      {
        SuspendThread (thread);
        ::user_abort (sig_name, sig_number);
        ResumeThread (thread);
      }
  }

  void do_raise_sigint (void)
  {
    bool is_interrupt_thread = (GetCurrentThreadId () == thread_id);

    if (is_interrupt_thread)
      ::raise (SIGINT);
    else
      {
        SuspendThread (thread);
        ::raise (SIGINT);
        ResumeThread (thread);
      }
  }

  static bool instance_ok (void)
  {
    bool retval = true;

    if (! instance)
      {
        instance = new w32_interrupt_manager ();

        if (instance)
          singleton_cleanup_list::add (cleanup_instance);
      }

    if (! instance)
      {
        ::error ("unable to create w32_interrupt_manager");

        retval = false;
      }

    return retval;
  }

  static void cleanup_instance (void) { delete instance; instance = 0; }

private:
  // A handle to the thread that is running the octave interpreter.
  HANDLE thread;

  // The ID of the thread that is running the octave interpreter.
  DWORD thread_id;

  static w32_interrupt_manager* instance;
};

w32_interrupt_manager* w32_interrupt_manager::instance = 0;

void w32_raise_sigint (void)
{
  w32_interrupt_manager::raise_sigint ();
}

#endif

// Signal handler return type.
#ifndef BADSIG
#define BADSIG (void (*)(int))-1
#endif

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

#define BLOCK_SIGNAL(sig, nvar, ovar) \
  do \
    { \
      GNULIB_NAMESPACE::sigemptyset (&nvar); \
      GNULIB_NAMESPACE::sigaddset (&nvar, sig); \
      GNULIB_NAMESPACE::sigemptyset (&ovar); \
      GNULIB_NAMESPACE::sigprocmask (SIG_BLOCK, &nvar, &ovar); \
    } \
  while (0)

#if !defined (SIGCHLD) && defined (SIGCLD)
#define SIGCHLD SIGCLD
#endif

#define BLOCK_CHILD(nvar, ovar) BLOCK_SIGNAL (SIGCHLD, nvar, ovar)
#define UNBLOCK_CHILD(ovar) GNULIB_NAMESPACE::sigprocmask (SIG_SETMASK, &ovar, 0)

// Called from octave_quit () to actually do something about the signals
// we have caught.

void
octave_signal_handler (void)
{
  // The list of signals is relatively short, so we will just go
  // linearly through the list.

  for (int i = 0; i < NSIG; i++)
    {
      if (octave_signals_caught[i])
        {
          octave_signals_caught[i] = false;

          switch (i)
            {
#ifdef SIGCHLD
            case SIGCHLD:
              {
                volatile octave_interrupt_handler saved_interrupt_handler
                  = octave_ignore_interrupts ();

                sigset_t set, oset;

                BLOCK_CHILD (set, oset);

                octave_child_list::wait ();

                octave_set_interrupt_handler (saved_interrupt_handler);

                UNBLOCK_CHILD (oset);

                octave_child_list::reap ();
              }
              break;
#endif

            case SIGFPE:
              std::cerr << "warning: floating point exception" << std::endl;
              break;

#ifdef SIGPIPE
            case SIGPIPE:
              std::cerr << "warning: broken pipe" << std::endl;
              break;
#endif
            }
        }
    }
}

static void
my_friendly_exit (const char *sig_name, int sig_number,
                  bool save_vars = true)
{
  static bool been_there_done_that = false;

  if (been_there_done_that)
    {
#if defined (SIGABRT)
      octave_set_signal_handler (SIGABRT, SIG_DFL);
#endif

      std::cerr << "panic: attempted clean up apparently failed -- aborting...\n";

      MINGW_SIGNAL_CLEANUP ();

      abort ();
    }
  else
    {
      been_there_done_that = true;

      std::cerr << "panic: " << sig_name << " -- stopping myself...\n";

      if (save_vars)
        dump_octave_core ();

      if (sig_number < 0)
        {
          MINGW_SIGNAL_CLEANUP ();

          exit (1);
        }
      else
        {
          octave_set_signal_handler (sig_number, SIG_DFL);

          GNULIB_NAMESPACE::raise (sig_number);
        }
    }
}

sig_handler *
octave_set_signal_handler (int sig, sig_handler *handler,
                           bool restart_syscalls)
{
  struct sigaction act, oact;

  act.sa_handler = handler;
  act.sa_flags = 0;

#if defined (SIGALRM)
  if (sig == SIGALRM)
    {
#if defined (SA_INTERRUPT)
      act.sa_flags |= SA_INTERRUPT;
#endif
    }
#endif
#if defined (SA_RESTART)
#if defined (SIGALRM)
  else
#endif
  // FIXME: Do we also need to explicitly disable SA_RESTART?
  if (restart_syscalls)
    act.sa_flags |= SA_RESTART;
#endif

  GNULIB_NAMESPACE::sigemptyset (&act.sa_mask);
  GNULIB_NAMESPACE::sigemptyset (&oact.sa_mask);

  GNULIB_NAMESPACE::sigaction (sig, &act, &oact);

  return oact.sa_handler;
}

static void
generic_sig_handler (int sig)
{
  my_friendly_exit (strsignal (sig), sig);
}

// Handle SIGCHLD.

#ifdef SIGCHLD
static void
sigchld_handler (int /* sig */)
{
  octave_signal_caught = 1;

  octave_signals_caught[SIGCHLD] = true;
}
#endif /* defined (SIGCHLD) */

#ifdef SIGFPE
#if defined (__alpha__)
static void
sigfpe_handler (int /* sig */)
{
  if (can_interrupt && octave_interrupt_state >= 0)
    {
      octave_signal_caught = 1;

      octave_signals_caught[SIGFPE] = true;

      octave_interrupt_state++;
    }
}
#endif /* defined (__alpha__) */
#endif /* defined (SIGFPE) */

#if defined (SIGHUP) || defined (SIGTERM)
static void
sig_hup_or_term_handler (int sig)
{
  switch (sig)
    {
#if defined (SIGHUP)
    case SIGHUP:
      {
        if (Vsighup_dumps_octave_core)
          dump_octave_core ();
      }
      break;
#endif

#if defined (SIGTERM)
    case SIGTERM:
      {
        if (Vsigterm_dumps_octave_core)
          dump_octave_core ();
      }
      break;
#endif

    default:
      break;
    }

  clean_up_and_exit (0);
}
#endif

#if 0
#if defined (SIGWINCH)
static void
sigwinch_handler (int /* sig */)
{
  command_editor::resize_terminal ();
}
#endif
#endif

// Handle SIGINT by restarting the parser (see octave.cc).
//
// This also has to work for SIGBREAK (on systems that have it), so we
// use the value of sig, instead of just assuming that it is called
// for SIGINT only.

static void
user_abort (const char *sig_name, int sig_number)
{
  if (! octave_initialized)
    exit (1);

  if (can_interrupt)
    {
      if (Vdebug_on_interrupt)
        {
          if (! octave_debug_on_interrupt_state)
            {
              tree_evaluator::debug_mode = true;
              octave_debug_on_interrupt_state = true;

              return;
            }
          else
            {
              // Clear the flag and do normal interrupt stuff.

              tree_evaluator::debug_mode
                = bp_table::have_breakpoints () || Vdebugging;
              octave_debug_on_interrupt_state = false;
            }
        }

      if (octave_interrupt_immediately)
        {
          if (octave_interrupt_state == 0)
            octave_interrupt_state = 1;

#if defined (__WIN32__) && ! defined (__CYGWIN__)
          w32_interrupt_manager::octave_jump_to_enclosing_context ();
#else
          octave_jump_to_enclosing_context ();
#endif
        }
      else
        {
          // If we are already cleaning up from a previous interrupt,
          // take note of the fact that another interrupt signal has
          // arrived.

          if (octave_interrupt_state < 0)
            octave_interrupt_state = 0;

          octave_signal_caught = 1;
          octave_interrupt_state++;

          if (interactive && ! forced_interactive
              && octave_interrupt_state == 2)
            std::cerr << "Press Control-C again to abort." << std::endl;

          if (octave_interrupt_state >= 3)
            my_friendly_exit (sig_name, sig_number, true);
        }
    }

}

static void
sigint_handler (int sig)
{
#if defined (__WIN32__) && ! defined (__CYGWIN__)
  w32_interrupt_manager::user_abort (strsignal (sig), sig);
#else
  user_abort (strsignal (sig), sig);
#endif
}

#ifdef SIGPIPE
static void
sigpipe_handler (int /* sig */)
{
  octave_signal_caught = 1;

  octave_signals_caught[SIGPIPE] = true;

  // Don't loop forever on account of this.

  if (pipe_handler_error_count++ > 100 && octave_interrupt_state >= 0)
    octave_interrupt_state++;
}
#endif /* defined (SIGPIPE) */

octave_interrupt_handler
octave_catch_interrupts (void)
{
  octave_interrupt_handler retval;

#if defined (__WIN32__) && ! defined (__CYGWIN__)
  w32_interrupt_manager::init ();
#endif

#ifdef SIGINT
  retval.int_handler = octave_set_signal_handler (SIGINT, sigint_handler);
#endif

#ifdef SIGBREAK
  retval.brk_handler = octave_set_signal_handler (SIGBREAK, sigint_handler);
#endif

  return retval;
}

octave_interrupt_handler
octave_ignore_interrupts (void)
{
  octave_interrupt_handler retval;

#if defined (__WIN32__) && ! defined (__CYGWIN__)
  w32_interrupt_manager::init ();
#endif

#ifdef SIGINT
  retval.int_handler = octave_set_signal_handler (SIGINT, SIG_IGN);
#endif

#ifdef SIGBREAK
  retval.brk_handler = octave_set_signal_handler (SIGBREAK, SIG_IGN);
#endif

  return retval;
}

octave_interrupt_handler
octave_set_interrupt_handler (const volatile octave_interrupt_handler& h,
                              bool restart_syscalls)
{
  octave_interrupt_handler retval;

#if defined (__WIN32__) && ! defined (__CYGWIN__)
  w32_interrupt_manager::init ();
#endif

#ifdef SIGINT
  retval.int_handler = octave_set_signal_handler (SIGINT, h.int_handler,
                                                  restart_syscalls);
#endif

#ifdef SIGBREAK
  retval.brk_handler = octave_set_signal_handler (SIGBREAK, h.brk_handler,
                                                  restart_syscalls);
#endif

  return retval;
}

// Install all the handlers for the signals we might care about.

void
install_signal_handlers (void)
{
  for (int i = 0; i < NSIG; i++)
    octave_signals_caught[i] = false;

  octave_catch_interrupts ();

#ifdef SIGABRT
  octave_set_signal_handler (SIGABRT, generic_sig_handler);
#endif

#ifdef SIGALRM
  octave_set_signal_handler (SIGALRM, generic_sig_handler);
#endif

#ifdef SIGBUS
  octave_set_signal_handler (SIGBUS, generic_sig_handler);
#endif

#ifdef SIGCHLD
  octave_set_signal_handler (SIGCHLD, sigchld_handler);
#endif

  // SIGCLD
  // SIGCONT

#ifdef SIGEMT
  octave_set_signal_handler (SIGEMT, generic_sig_handler);
#endif

#ifdef SIGFPE
#if defined (__alpha__)
  octave_set_signal_handler (SIGFPE, sigfpe_handler);
#else
  octave_set_signal_handler (SIGFPE, generic_sig_handler);
#endif
#endif

#ifdef SIGHUP
  octave_set_signal_handler (SIGHUP, sig_hup_or_term_handler);
#endif

#ifdef SIGILL
  octave_set_signal_handler (SIGILL, generic_sig_handler);
#endif

  // SIGINFO
  // SIGINT

#ifdef SIGIOT
  octave_set_signal_handler (SIGIOT, generic_sig_handler);
#endif

#ifdef SIGLOST
  octave_set_signal_handler (SIGLOST, generic_sig_handler);
#endif

#ifdef SIGPIPE
  octave_set_signal_handler (SIGPIPE, sigpipe_handler);
#endif

#ifdef SIGPOLL
  octave_set_signal_handler (SIGPOLL, SIG_IGN);
#endif

  // SIGPROF
  // SIGPWR

#ifdef SIGQUIT
  octave_set_signal_handler (SIGQUIT, generic_sig_handler);
#endif

#ifdef SIGSEGV
  octave_set_signal_handler (SIGSEGV, generic_sig_handler);
#endif

  // SIGSTOP

#ifdef SIGSYS
  octave_set_signal_handler (SIGSYS, generic_sig_handler);
#endif

#ifdef SIGTERM
  octave_set_signal_handler (SIGTERM, sig_hup_or_term_handler);
#endif

#ifdef SIGTRAP
  octave_set_signal_handler (SIGTRAP, generic_sig_handler);
#endif

  // SIGTSTP
  // SIGTTIN
  // SIGTTOU
  // SIGURG

#ifdef SIGUSR1
  octave_set_signal_handler (SIGUSR1, generic_sig_handler);
#endif

#ifdef SIGUSR2
  octave_set_signal_handler (SIGUSR2, generic_sig_handler);
#endif

#ifdef SIGVTALRM
  octave_set_signal_handler (SIGVTALRM, generic_sig_handler);
#endif

#ifdef SIGIO
  octave_set_signal_handler (SIGIO, SIG_IGN);
#endif

#if 0
#ifdef SIGWINCH
  octave_set_signal_handler (SIGWINCH, sigwinch_handler);
#endif
#endif

#ifdef SIGXCPU
  octave_set_signal_handler (SIGXCPU, generic_sig_handler);
#endif

#ifdef SIGXFSZ
  octave_set_signal_handler (SIGXFSZ, generic_sig_handler);
#endif

}

static octave_scalar_map
make_sig_struct (void)
{
  octave_scalar_map m;

#ifdef SIGABRT
  m.assign ("ABRT", SIGABRT);
#endif

#ifdef SIGALRM
  m.assign ("ALRM", SIGALRM);
#endif

#ifdef SIGBUS
  m.assign ("BUS", SIGBUS);
#endif

#ifdef SIGCHLD
  m.assign ("CHLD", SIGCHLD);
#endif

#ifdef SIGCLD
  m.assign ("CLD", SIGCLD);
#endif

#ifdef SIGCONT
  m.assign ("CONT", SIGCONT);
#endif

#ifdef SIGEMT
  m.assign ("EMT", SIGEMT);
#endif

#ifdef SIGFPE
  m.assign ("FPE", SIGFPE);
#endif

#ifdef SIGHUP
  m.assign ("HUP", SIGHUP);
#endif

#ifdef SIGILL
  m.assign ("ILL", SIGILL);
#endif

#ifdef SIGINFO
  m.assign ("INFO", SIGINFO);
#endif

#ifdef SIGINT
  m.assign ("INT", SIGINT);
#endif

#ifdef SIGIO
  m.assign ("IO", SIGIO);
#endif

#ifdef SIGIOT
  m.assign ("IOT", SIGIOT);
#endif

#ifdef SIGKILL
  m.assign ("KILL", SIGKILL);
#endif

#ifdef SIGLOST
  m.assign ("LOST", SIGLOST);
#endif

#ifdef SIGPIPE
  m.assign ("PIPE", SIGPIPE);
#endif

#ifdef SIGPOLL
  m.assign ("POLL", SIGPOLL);
#endif

#ifdef SIGPROF
  m.assign ("PROF", SIGPROF);
#endif

#ifdef SIGPWR
  m.assign ("PWR", SIGPWR);
#endif

#ifdef SIGQUIT
  m.assign ("QUIT", SIGQUIT);
#endif

#ifdef SIGSEGV
  m.assign ("SEGV", SIGSEGV);
#endif

#ifdef SIGSTKFLT
  m.assign ("STKFLT", SIGSTKFLT);
#endif

#ifdef SIGSTOP
  m.assign ("STOP", SIGSTOP);
#endif

#ifdef SIGSYS
  m.assign ("SYS", SIGSYS);
#endif

#ifdef SIGTERM
  m.assign ("TERM", SIGTERM);
#endif

#ifdef SIGTRAP
  m.assign ("TRAP", SIGTRAP);
#endif

#ifdef SIGTSTP
  m.assign ("TSTP", SIGTSTP);
#endif

#ifdef SIGTTIN
  m.assign ("TTIN", SIGTTIN);
#endif

#ifdef SIGTTOU
  m.assign ("TTOU", SIGTTOU);
#endif

#ifdef SIGUNUSED
  m.assign ("UNUSED", SIGUNUSED);
#endif

#ifdef SIGURG
  m.assign ("URG", SIGURG);
#endif

#ifdef SIGUSR1
  m.assign ("USR1", SIGUSR1);
#endif

#ifdef SIGUSR2
  m.assign ("USR2", SIGUSR2);
#endif

#ifdef SIGVTALRM
  m.assign ("VTALRM", SIGVTALRM);
#endif

#ifdef SIGWINCH
  m.assign ("WINCH", SIGWINCH);
#endif

#ifdef SIGXCPU
  m.assign ("XCPU", SIGXCPU);
#endif

#ifdef SIGXFSZ
  m.assign ("XFSZ", SIGXFSZ);
#endif

  return m;
}

octave_child_list::octave_child_list_rep *octave_child_list::instance = 0;

bool
octave_child_list::instance_ok (void)
{
  bool retval = true;

  if (! instance)
    {
      instance = new octave_child_list_rep ();

      if (instance)
        singleton_cleanup_list::add (cleanup_instance);
    }

  if (! instance)
    {
      ::error ("unable to create child list object!");

      retval = false;
    }

  return retval;
}

void
octave_child_list::insert (pid_t pid, octave_child::child_event_handler f)
{
  if (instance_ok ())
    instance->insert (pid, f);
}

void
octave_child_list::reap (void)
{
  if (instance_ok ())
    instance->reap ();
}

bool
octave_child_list::wait (void)
{
  return (instance_ok ()) ? instance->wait () : false;
}

class pid_equal
{
public:

  pid_equal (pid_t v) : val (v) { }

  bool operator () (const octave_child& oc) const { return oc.pid == val; }

private:

  pid_t val;
};

void
octave_child_list::remove (pid_t pid)
{
  if (instance_ok ())
    instance->remove_if (pid_equal (pid));
}

#define OCL_REP octave_child_list::octave_child_list_rep

void
OCL_REP::insert (pid_t pid, octave_child::child_event_handler f)
{
  append (octave_child (pid, f));
}

void
OCL_REP::reap (void)
{
  // Mark the record for PID invalid.

  for (iterator p = begin (); p != end (); p++)
    {
      // The call to the octave_child::child_event_handler might
      // invalidate the iterator (for example, by calling
      // octave_child_list::remove), so we increment the iterator
      // here.

      octave_child& oc = *p;

      if (oc.have_status)
        {
          oc.have_status = 0;

          octave_child::child_event_handler f = oc.handler;

          if (f && f (oc.pid, oc.status))
            oc.pid = -1;
        }
    }

  remove_if (pid_equal (-1));
}

// Wait on our children and record any changes in their status.

bool
OCL_REP::wait (void)
{
  bool retval = false;

  for (iterator p = begin (); p != end (); p++)
    {
      octave_child& oc = *p;

      pid_t pid = oc.pid;

      if (pid > 0)
        {
          int status;

          if (octave_syscalls::waitpid (pid, &status, WNOHANG) > 0)
            {
              oc.have_status = 1;

              oc.status = status;

              retval = true;

              break;
            }
        }
    }

  return retval;
}

DEFUN (SIG, args, ,
       "-*- texinfo -*-\n\
@deftypefn {Built-in Function} {} SIG ()\n\
Return a structure containing Unix signal names and their defined values.\n\
@end deftypefn")
{
  octave_value retval;

  if (args.length () == 0)
    {
      static octave_scalar_map m = make_sig_struct ();

      retval = m;
    }
  else
    print_usage ();

  return retval;
}

/*
%!assert (isstruct (SIG ()))
%!assert (! isempty (SIG ()))

%!error SIG (1)
*/

DEFUN (debug_on_interrupt, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{val} =} debug_on_interrupt ()\n\
@deftypefnx {Built-in Function} {@var{old_val} =} debug_on_interrupt (@var{new_val})\n\
@deftypefnx {Built-in Function} {} debug_on_interrupt (@var{new_val}, \"local\")\n\
Query or set the internal variable that controls whether Octave will try\n\
to enter debugging mode when it receives an interrupt signal (typically\n\
generated with @kbd{C-c}).\n\
\n\
If a second interrupt signal is received before reaching the debugging mode,\n\
a normal interrupt will occur.\n\
\n\
When called from inside a function with the @qcode{\"local\"} option, the\n\
variable is changed locally for the function and any subroutines it calls.\n\
The original variable value is restored when exiting the function.\n\
@seealso{debug_on_error, debug_on_warning}\n\
@end deftypefn")
{
  return SET_INTERNAL_VARIABLE (debug_on_interrupt);
}

/*
%!test
%! orig_val = debug_on_interrupt ();
%! old_val = debug_on_interrupt (! orig_val);
%! assert (orig_val, old_val);
%! assert (debug_on_interrupt (), ! orig_val);
%! debug_on_interrupt (orig_val);
%! assert (debug_on_interrupt (), orig_val);

%!error (debug_on_interrupt (1, 2))
*/

DEFUN (sighup_dumps_octave_core, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{val} =} sighup_dumps_octave_core ()\n\
@deftypefnx {Built-in Function} {@var{old_val} =} sighup_dumps_octave_core (@var{new_val})\n\
@deftypefnx {Built-in Function} {} sighup_dumps_octave_core (@var{new_val}, \"local\")\n\
Query or set the internal variable that controls whether Octave tries\n\
to save all current variables to the file @file{octave-workspace} if it\n\
receives a hangup signal.\n\
\n\
When called from inside a function with the @qcode{\"local\"} option, the\n\
variable is changed locally for the function and any subroutines it calls.\n\
The original variable value is restored when exiting the function.\n\
@end deftypefn")
{
  return SET_INTERNAL_VARIABLE (sighup_dumps_octave_core);
}

/*
%!test
%! orig_val = sighup_dumps_octave_core ();
%! old_val = sighup_dumps_octave_core (! orig_val);
%! assert (orig_val, old_val);
%! assert (sighup_dumps_octave_core (), ! orig_val);
%! sighup_dumps_octave_core (orig_val);
%! assert (sighup_dumps_octave_core (), orig_val);

%!error (sighup_dumps_octave_core (1, 2))
*/

DEFUN (sigterm_dumps_octave_core, args, nargout,
       "-*- texinfo -*-\n\
@deftypefn  {Built-in Function} {@var{val} =} sigterm_dumps_octave_core ()\n\
@deftypefnx {Built-in Function} {@var{old_val} =} sigterm_dumps_octave_core (@var{new_val})\n\
@deftypefnx {Built-in Function} {} sigterm_dumps_octave_core (@var{new_val}, \"local\")\n\
Query or set the internal variable that controls whether Octave tries\n\
to save all current variables to the file @file{octave-workspace} if it\n\
receives a terminate signal.\n\
\n\
When called from inside a function with the @qcode{\"local\"} option, the\n\
variable is changed locally for the function and any subroutines it calls.\n\
The original variable value is restored when exiting the function.\n\
@end deftypefn")
{
  return SET_INTERNAL_VARIABLE (sigterm_dumps_octave_core);
}

/*
%!test
%! orig_val = sigterm_dumps_octave_core ();
%! old_val = sigterm_dumps_octave_core (! orig_val);
%! assert (orig_val, old_val);
%! assert (sigterm_dumps_octave_core (), ! orig_val);
%! sigterm_dumps_octave_core (orig_val);
%! assert (sigterm_dumps_octave_core (), orig_val);

%!error (sigterm_dumps_octave_core (1, 2))
*/
