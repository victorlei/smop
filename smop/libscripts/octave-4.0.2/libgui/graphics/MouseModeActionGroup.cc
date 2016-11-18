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

#include <QAction>
#include <QIcon>

#include "Figure.h"
#include "MouseModeActionGroup.h"

namespace QtHandles
{

MouseModeActionGroup::MouseModeActionGroup (QObject* xparent)
  : QObject (xparent), m_current (0)
{
  m_actions.append (new QAction (QIcon (":/images/rotate.png"),
                                 tr ("Rotate"), this));
  QAction *zoom_in = new QAction ("Z+", this);
  zoom_in->setToolTip (tr ("Zoom In"));
  m_actions.append (zoom_in);

  QAction *zoom_out = new QAction ("Z-", this);
  zoom_out->setToolTip (tr ("Zoom Out"));
  m_actions.append (zoom_out);

  m_actions.append (new QAction (QIcon (":/images/pan.png"),
                                 tr ("Pan"), this));
  m_actions.append (new QAction (QIcon::fromTheme ("insert-text"),
                                 tr ("Insert Text"), this));
  m_actions.append (new QAction (QIcon (":/images/select.png"),
                                 tr ("Select"), this));

  foreach (QAction* a, m_actions)
    {
      a->setCheckable (true);
      connect (a, SIGNAL (toggled (bool)), this, SLOT (actionToggled (bool)));
    }
}

MouseModeActionGroup::~MouseModeActionGroup (void)
{
}

void
MouseModeActionGroup::actionToggled (bool checked)
{
  if (! checked)
    {
      if (sender () == m_current)
        {
          m_current = 0;
          emit modeChanged (NoMode);
        }
    }
  else
    {
      int i = m_actions.indexOf (qobject_cast<QAction*> (sender ()));

      if (i >= 0)
        {
          m_current = m_actions[i];
          for (int j = 0; j < m_actions.size (); j++)
            {
              // SelectMode cancels all the others but the button
              // doesn't remain highlighed.

              if (j != i || i+1 == SelectMode)
                m_actions[j]->setChecked (false);
            }

          emit modeChanged (static_cast<MouseMode> (i+1));
        }
    }
}

void
MouseModeActionGroup::setMode (MouseMode mode)
{
  for (int i = 0; i < m_actions.size (); i++)
    m_actions[i]->setChecked (i+1 == mode);

  // SelectMode cancels all the others but the button doesn't remain
  // highlighed.

  if (mode == SelectMode)
    m_actions[SelectMode-1]->setChecked (false);
}

};
