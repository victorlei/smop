/*

Copyright (C) 2011-2015 Jacob Dawid

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

#include <QApplication>
#include <QTextCodec>
#include <QThread>
#include <QTranslator>

#include <iostream>

#include <unistd.h>
#include <fcntl.h>

#if defined (HAVE_SYS_IOCTL_H)
#include <sys/ioctl.h>
#endif

#include "lo-utils.h"
#include "oct-env.h"
#include "oct-syscalls.h"
#include "syswait.h"

#include "octave.h"
#include "sighandlers.h"

#include "welcome-wizard.h"
#include "resource-manager.h"
#include "shortcut-manager.h"
#include "main-window.h"
#include "octave-gui.h"
#include "thread-manager.h"

#include "builtin-defun-decls.h"
#include "__init_qt__.h"

// Allow the Octave interpreter to start as in CLI mode with a
// QApplication context so that it can use Qt for things like plotting
// and UI widgets.

class octave_cli_thread : public QThread
{
public:

  octave_cli_thread (int argc, char **argv)
    : m_argc (argc), m_argv (argv), m_result (0) { }

  int result (void) const { return m_result; }

protected:

  void run (void)
  {
    octave_thread_manager::unblock_interrupt_signal ();

    octave_initialize_interpreter (m_argc, m_argv, 0);

    m_result = octave_execute_interpreter ();

    QApplication::exit (m_result);
  }

private:

  int m_argc;
  char** m_argv;
  int m_result;
};

// Disable all Qt messages by default.

static void
message_handler (QtMsgType, const char *)
{
}

// If START_GUI is false, we still set up the QApplication so that we
// can use Qt widgets for plot windows.

int
octave_start_gui (int argc, char *argv[], bool start_gui)
{
  octave_thread_manager::block_interrupt_signal ();

  std::string show_gui_msgs = octave_env::getenv ("OCTAVE_SHOW_GUI_MESSAGES");

  // Installing our handler suppresses the messages.
  if (show_gui_msgs.empty ())
    qInstallMsgHandler (message_handler);

  install___init_qt___functions ();

  Fregister_graphics_toolkit (ovl ("qt"));

  QApplication application (argc, argv);
  QTranslator gui_tr, qt_tr, qsci_tr;

  // Set the codec for all strings (before wizard)
#if ! defined (Q_OS_WIN32)
  QTextCodec::setCodecForCStrings (QTextCodec::codecForName ("UTF-8"));
#endif

  // show wizard if this is the first run
  if (resource_manager::is_first_run () && start_gui)
    {
      // before wizard
      resource_manager::config_translators (&qt_tr, &qsci_tr, &gui_tr);
      application.installTranslator (&qt_tr);
      application.installTranslator (&gui_tr);
      application.installTranslator (&qsci_tr);

      welcome_wizard welcomeWizard;

      if (welcomeWizard.exec () == QDialog::Rejected)
        exit (1);

      resource_manager::reload_settings ();  // install settings file
    }
  else
    {
      resource_manager::reload_settings ();  // get settings file

      // after settings
      resource_manager::config_translators (&qt_tr, &qsci_tr, &gui_tr);
      application.installTranslator (&qt_tr);
      application.installTranslator (&gui_tr);
      if (start_gui)
        application.installTranslator (&qsci_tr);
    }

  if (start_gui)
    {
      // update network-settings
      resource_manager::update_network_settings ();

#if ! defined (__WIN32__) || defined (__CYGWIN__)
      // If we were started from a launcher, TERM might not be
      // defined, but we provide a terminal with xterm
      // capabilities.

      std::string term = octave_env::getenv ("TERM");

      if (term.empty ())
        octave_env::putenv ("TERM", "xterm");
#else
      std::string term = octave_env::getenv ("TERM");

      if (term.empty ())
        octave_env::putenv ("TERM", "cygwin");
#endif

      // shortcut manager
      shortcut_manager::init_data ();
    }

  // Force left-to-right alignment (see bug #46204)
  application.setLayoutDirection (Qt::LeftToRight);

  // Create and show main window.

  main_window w (0, start_gui);

  if (start_gui)
    {
      w.read_settings ();

      w.init_terminal_size ();

      // Connect signals for changes in visibility not before w
      // is shown.

      w.connect_visibility_changed ();

      w.focus_command_window ();
    }
  else
    application.setQuitOnLastWindowClosed (false);

  return application.exec ();
}
