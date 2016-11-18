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

#include "PushTool.h"

#include "ToolBarButton.cc"

namespace QtHandles
{

PushTool*
PushTool::create (const graphics_object& go)
{
  Object* parent = Object::parentObject (go);

  if (parent)
    {
      QWidget* parentWidget = parent->qWidget<QWidget> ();

      if (parentWidget)
        return new PushTool (go, new QAction (parentWidget));
    }

  return 0;
}

PushTool::PushTool (const graphics_object& go, QAction* action)
  : ToolBarButton<uipushtool> (go, action)
{
  connect (action, SIGNAL (triggered (bool)), this, SLOT (clicked (void)));
}

PushTool::~PushTool (void)
{
}

void
PushTool::update (int pId)
{
  switch (pId)
    {
    default:
      ToolBarButton<uipushtool>::update (pId);
      break;
    }
}

void
PushTool::clicked (void)
{
  gh_manager::post_callback (m_handle, "clickedcallback");
}

};
