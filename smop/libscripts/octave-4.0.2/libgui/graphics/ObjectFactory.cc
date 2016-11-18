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

#include <QApplication>
#include <QThread>

#include "graphics.h"

#include "Backend.h"
#include "CheckBoxControl.h"
#include "ContextMenu.h"
#include "EditControl.h"
#include "Figure.h"
#include "ListBoxControl.h"
#include "Logger.h"
#include "Menu.h"
#include "ObjectFactory.h"
#include "ObjectProxy.h"
#include "Panel.h"
#include "PopupMenuControl.h"
#include "PushButtonControl.h"
#include "PushTool.h"
#include "RadioButtonControl.h"
#include "SliderControl.h"
#include "TextControl.h"
#include "ToggleButtonControl.h"
#include "ToggleTool.h"
#include "ToolBar.h"
#include "QtHandlesUtils.h"

namespace QtHandles
{

ObjectFactory*
ObjectFactory::instance (void)
{
  static ObjectFactory s_instance;
  static bool s_instanceCreated = false;

  if (! s_instanceCreated)
    {
      if (QThread::currentThread () != QApplication::instance ()->thread ())
        s_instance.moveToThread (QApplication::instance ()->thread ());
      s_instanceCreated = true;
    }

  return &s_instance;
}

void
ObjectFactory::createObject (double handle)
{
  gh_manager::auto_lock lock;

  graphics_object go (gh_manager::get_object (graphics_handle (handle)));

  if (go.valid_object ())
    {
      if (go.get_properties ().is_beingdeleted ())
        qWarning ("ObjectFactory::createObject: object is being deleted");
      else
        {
          ObjectProxy* proxy = Backend::toolkitObjectProxy (go);

          if (proxy)
            {
              Logger::debug ("ObjectFactory::createObject: "
                             "create %s from thread %08x",
                             go.type ().c_str (), QThread::currentThreadId ());

              Object* obj = 0;

              if (go.isa ("figure"))
                obj = Figure::create (go);
              else if (go.isa ("uicontrol"))
                {
                  uicontrol::properties& up =
                   Utils::properties<uicontrol> (go);

                  if (up.style_is ("pushbutton"))
                    obj = PushButtonControl::create (go);
                  else if (up.style_is ("edit"))
                    obj = EditControl::create (go);
                  else if (up.style_is ("checkbox"))
                    obj = CheckBoxControl::create (go);
                  else if (up.style_is ("radiobutton"))
                    obj = RadioButtonControl::create (go);
                  else if (up.style_is ("togglebutton"))
                    obj = ToggleButtonControl::create (go);
                  else if (up.style_is ("text"))
                    obj = TextControl::create (go);
                  else if (up.style_is ("popupmenu"))
                    obj = PopupMenuControl::create (go);
                  else if (up.style_is ("slider"))
                    obj = SliderControl::create (go);
                  else if (up.style_is ("listbox"))
                    obj = ListBoxControl::create (go);
                }
              else if (go.isa ("uipanel"))
                obj = Panel::create (go);
              else if (go.isa ("uimenu"))
                obj = Menu::create (go);
              else if (go.isa ("uicontextmenu"))
                obj = ContextMenu::create (go);
              else if (go.isa ("uitoolbar"))
                obj = ToolBar::create (go);
              else if (go.isa ("uipushtool"))
                obj = PushTool::create (go);
              else if (go.isa ("uitoggletool"))
                obj = ToggleTool::create (go);
              else
                qWarning ("ObjectFactory::createObject: unsupported type `%s'",
                          go.type ().c_str ());

              if (obj)
                proxy->setObject (obj);
            }
          else
            qWarning ("ObjectFactory::createObject: no proxy for handle %g",
                      handle);
        }
    }
  else
    qWarning ("ObjectFactory::createObject: invalid object for handle %g",
              handle);
}

};
