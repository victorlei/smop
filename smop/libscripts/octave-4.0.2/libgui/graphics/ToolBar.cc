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
#include <QActionEvent>
#include <QApplication>
#include <QEvent>
#include <QIcon>
#include <QMainWindow>
#include <QPixmap>
#include <QTimer>
#include <QToolBar>

#include "Figure.h"
#include "ToolBar.h"
#include "QtHandlesUtils.h"

namespace QtHandles
{

static QAction*
addEmptyAction (QToolBar* bar)
{
  static QIcon _empty;

  if (_empty.isNull ())
    {
      QPixmap pix (16, 16);

      pix.fill (Qt::transparent);

      _empty = QIcon (pix);
    }

  QAction* a = bar->addAction (_empty, "Empty Toolbar");

  a->setEnabled (false);
  a->setToolTip ("");

  return a;
}

ToolBar*
ToolBar::create (const graphics_object& go)
{
  Object* parent = Object::parentObject (go);

  if (parent)
    {
      QWidget* parentWidget = parent->qWidget<QWidget> ();

      if (parentWidget)
        return new ToolBar (go, new QToolBar (parentWidget));
    }

  return 0;
}

ToolBar::ToolBar (const graphics_object& go, QToolBar* bar)
  : Object (go, bar), m_empty (0), m_figure (0)
{
  uitoolbar::properties& tp = properties<uitoolbar> ();

  bar->setFloatable (false);
  bar->setMovable (false);
  bar->setVisible (tp.is_visible ());

  m_empty = addEmptyAction (bar);

  m_figure =
    dynamic_cast<Figure*> (Object::fromQObject (bar->parentWidget ()));

  if (m_figure)
    m_figure->addCustomToolBar (bar, tp.is_visible ());

  bar->installEventFilter (this);
}

ToolBar::~ToolBar (void)
{
}

void
ToolBar::update (int pId)
{
  uitoolbar::properties& tp = properties<uitoolbar> ();
  QToolBar* bar = qWidget<QToolBar> ();

  switch (pId)
    {
    case base_properties::ID_VISIBLE:
      if (m_figure)
        m_figure->showCustomToolBar (bar, tp.is_visible ());
      break;

    default:
      Object::update (pId);
      break;
    }
}

bool
ToolBar::eventFilter (QObject* watched, QEvent* xevent)
{
  if (watched == qObject ())
    {
      switch (xevent->type ())
        {
        case QEvent::ActionAdded:
        case QEvent::ActionRemoved:
            {
              QActionEvent* ae = dynamic_cast<QActionEvent*> (xevent);
              QToolBar* bar = qWidget<QToolBar> ();

              if (ae->action () != m_empty)
                {
                  if (xevent->type () == QEvent::ActionAdded)
                    {
                      if (bar->actions ().size () == 2)
                        QTimer::singleShot (0, this, SLOT (hideEmpty (void)));
                    }
                  else
                    {
                      if (bar->actions ().size () == 1)
                        m_empty->setVisible (true);
                    }
                }
            }
          break;

        default:
          break;
        }
    }

  return false;
}

void
ToolBar::hideEmpty (void)
{
  m_empty->setVisible (false);
}

void
ToolBar::beingDeleted (void)
{
  if (m_figure)
    {
      QToolBar* bar = qWidget<QToolBar> ();

      if (bar)
        m_figure->showCustomToolBar (bar, false);
    }
}

}; // namespace QtHandles
