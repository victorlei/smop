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

#include "documentation-dock-widget.h"

documentation_dock_widget::documentation_dock_widget (QWidget *p)
  : octave_dock_widget (p)
{
  setObjectName ("DocumentationDockWidget");
  setWindowIcon (QIcon (":/actions/icons/logo.png"));
  set_title (tr ("Documentation"));
  setStatusTip (tr ("See the documentation for help."));

  _webinfo = new webinfo (this);
  setWidget (_webinfo);

  connect (p, SIGNAL (show_doc_signal (const QString &)),
           this, SLOT (showDoc (const QString &)));
}

void
documentation_dock_widget::copyClipboard ()
{
  _webinfo->copyClipboard ();
}
void
documentation_dock_widget::pasteClipboard ()
{
  _webinfo->pasteClipboard ();
}
void
documentation_dock_widget::selectAll ()
{
  _webinfo->selectAll ();
}

void
documentation_dock_widget::showDoc (const QString &name)
{
  // show the doc pane without focus for carrying on typing in the console
  if (!isVisible ())
    setVisible (true);
  raise ();

  _webinfo->load_ref (name);

}
