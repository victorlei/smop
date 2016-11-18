/*

Copyright (C) 2011-2015 Michael Goffioul

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

#include "ToggleTool.h"

#include "ToolBarButton.cc"

namespace QtHandles
{

ToggleTool*
ToggleTool::create (const graphics_object& go)
{
  Object* parent = Object::parentObject (go);

  if (parent)
    {
      QWidget* parentWidget = parent->qWidget<QWidget> ();

      if (parentWidget)
        return new ToggleTool (go, new QAction (parentWidget));
    }

  return 0;
}

ToggleTool::ToggleTool (const graphics_object& go, QAction* action)
  : ToolBarButton<uitoggletool> (go, action)
{
  uitoggletool::properties& tp = properties<uitoggletool> ();

  action->setCheckable (true);
  action->setChecked (tp.is_state ());

  connect (action, SIGNAL (toggled (bool)),
           this, SLOT (triggered (bool)));
}

ToggleTool::~ToggleTool (void)
{
}

void
ToggleTool::update (int pId)
{
  uitoggletool::properties& tp = properties<uitoggletool> ();
  QAction* action = qWidget<QAction> ();

  switch (pId)
    {
    case uitoggletool::properties::ID_STATE:
      action->setChecked (tp.is_state ());
      break;

    default:
      ToolBarButton<uitoggletool>::update (pId);
      break;
    }
}

void
ToggleTool::triggered (bool checked)
{
  gh_manager::post_set (m_handle, "state", checked, false);
  gh_manager::post_callback (m_handle,
                             checked
                             ? "oncallback"
                             : "offcallback");
  gh_manager::post_callback (m_handle, "clickedcallback");
}

};
