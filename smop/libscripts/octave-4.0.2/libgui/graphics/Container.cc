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

#include <QChildEvent>
#include <QVBoxLayout>

#include "graphics.h"

#include "Canvas.h"
#include "Container.h"
#include "Object.h"
#include "QtHandlesUtils.h"

namespace QtHandles
{

Container::Container (QWidget* xparent)
  : ContainerBase (xparent), m_canvas (0)
{
  setFocusPolicy (Qt::ClickFocus);
}

Container::~Container (void)
{
}

Canvas*
Container::canvas (const graphics_handle& gh, bool xcreate)
{
  if (! m_canvas && xcreate)
    {
      gh_manager::auto_lock lock;
      graphics_object go = gh_manager::get_object (gh);

      if (go)
        {
          graphics_object fig = go.get_ancestor ("figure");

          m_canvas = Canvas::create (fig.get("renderer").string_value (),
                                     this, gh);

          QWidget* canvasWidget = m_canvas->qWidget ();

          canvasWidget->lower ();
          canvasWidget->show ();
          canvasWidget->setGeometry (0, 0, width (), height ());
        }
    }

  return m_canvas;
}

void
Container::resizeEvent (QResizeEvent* /* event */)
{
  if (m_canvas)
    m_canvas->qWidget ()->setGeometry (0, 0, width (), height ());

  gh_manager::auto_lock lock;

  foreach (QObject* qObj, children ())
    {
      if (qObj->isWidgetType ())
        {
          Object* obj = Object::fromQObject (qObj);

          if (obj)
            {
              Matrix bb = obj->properties ().get_boundingbox (false);

              obj->qWidget<QWidget> ()
                ->setGeometry (xround (bb(0)), xround (bb(1)),
                               xround (bb(2)), xround (bb(3)));
            }
        }
    }
}

void
Container::childEvent (QChildEvent* xevent)
{
  if (xevent->child ()->isWidgetType ())
    qobject_cast<QWidget*> (xevent->child ())->setMouseTracking (hasMouseTracking ());
}

}; // namespace QtHandles
