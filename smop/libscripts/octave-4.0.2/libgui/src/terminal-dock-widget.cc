/*

Copyright (C) 2013-2015 John W. Eaton
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

#include "terminal-dock-widget.h"

terminal_dock_widget::terminal_dock_widget (QWidget *p)
  : octave_dock_widget (p), terminal (QTerminal::create (p))
{
  terminal->setObjectName ("OctaveTerminal");
  terminal->setFocusPolicy (Qt::StrongFocus);

  setObjectName ("TerminalDockWidget");
  setWindowIcon (QIcon (":/actions/icons/logo.png"));
  set_title (tr ("Command Window"));

  setWidget (terminal);
  setFocusProxy (terminal);

  connect (terminal, SIGNAL (interrupt_signal (void)),
           this, SLOT (terminal_interrupt ()));
}

bool
terminal_dock_widget::has_focus (void) const
{
  QWidget *w = widget ();

  return w->hasFocus ();
}

void
terminal_dock_widget::focus (void)
{
  octave_dock_widget::focus ();

  QWidget *w = widget ();

  w->setFocus ();
  w->activateWindow ();
  w->raise ();
}

void
terminal_dock_widget::terminal_interrupt (void)
{
  emit interrupt_signal ();
}
