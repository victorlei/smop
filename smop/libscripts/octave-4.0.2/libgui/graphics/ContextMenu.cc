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

#include <QMenu>

#include "Backend.h"
#include "ContextMenu.h"
#include "QtHandlesUtils.h"

namespace QtHandles
{

ContextMenu*
ContextMenu::create (const graphics_object& go)
{
  Object* xparent = Object::parentObject (go);

  if (xparent)
    {
      QWidget* w = xparent->qWidget<QWidget> ();

      return new ContextMenu (go, new QMenu (w));
    }

  return 0;
}

ContextMenu::ContextMenu (const graphics_object& go, QMenu* xmenu)
    : Object (go, xmenu)
{
  xmenu->setAutoFillBackground (true);

  connect (xmenu, SIGNAL (aboutToShow (void)), SLOT (aboutToShow (void)));
  connect (xmenu, SIGNAL (aboutToHide (void)), SLOT (aboutToHide (void)));
}

ContextMenu::~ContextMenu (void)
{
}

void
ContextMenu::update (int pId)
{
  uicontextmenu::properties& up = properties<uicontextmenu> ();
  QMenu* xmenu = qWidget<QMenu> ();

  switch (pId)
    {
    case base_properties::ID_VISIBLE:
      if (up.is_visible ())
        {
          Matrix pos = up.get_position ().matrix_value ();
          QWidget* parentW = xmenu->parentWidget ();
          QPoint pt;

          pt.rx () = xround (pos(0));
          pt.ry () = parentW->height () - xround (pos(1));
          pt = parentW->mapToGlobal (pt);

          xmenu->popup (pt);
        }
      else
        xmenu->hide ();
      break;
    default:
      Object::update (pId);
      break;
    }
}

void
ContextMenu::aboutToShow (void)
{
  gh_manager::post_callback (m_handle, "callback");
  gh_manager::post_set (m_handle, "visible", "on", false);
}

void
ContextMenu::aboutToHide (void)
{
  gh_manager::post_set (m_handle, "visible", "off", false);
}

QWidget*
ContextMenu::menu (void)
{
  return qWidget<QWidget> ();
}

void
ContextMenu::executeAt (const base_properties& props, const QPoint& pt)
{
  graphics_handle h = props.get_uicontextmenu ();

  if (h.ok ())
    {
      gh_manager::auto_lock lock;
      graphics_object go = gh_manager::get_object (h);

      if (go.valid_object ())
        {
          ContextMenu* cMenu =
            dynamic_cast<ContextMenu*> (Backend::toolkitObject (go));

          if (cMenu)
            {
              QMenu* menu = cMenu->qWidget<QMenu> ();

              if (menu)
                menu->popup (pt);
            }
        }
    }
}

}; // namespace QtHandles
