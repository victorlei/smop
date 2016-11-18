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

#include <QComboBox>

#include "Container.h"
#include "PopupMenuControl.h"
#include "QtHandlesUtils.h"

namespace QtHandles
{

PopupMenuControl*
PopupMenuControl::create (const graphics_object& go)
{
  Object* parent = Object::parentObject (go);

  if (parent)
    {
      Container* container = parent->innerContainer ();

      if (container)
        return new PopupMenuControl (go, new QComboBox (container));
    }

  return 0;
}

PopupMenuControl::PopupMenuControl (const graphics_object& go, QComboBox *box)
  : BaseControl (go, box), m_blockUpdate (false)
{
  uicontrol::properties& up = properties<uicontrol> ();

  box->addItems (Utils::fromStdString (up.get_string_string ()).split ('|'));

  update (uicontrol::properties::ID_VALUE);

  connect (box, SIGNAL (currentIndexChanged (int)),
           SLOT (currentIndexChanged (int)));
}

PopupMenuControl::~PopupMenuControl (void)
{
}

void PopupMenuControl::update (int pId)
{
  uicontrol::properties& up = properties<uicontrol> ();
  QComboBox* box = qWidget<QComboBox> ();

  switch (pId)
    {
    case uicontrol::properties::ID_STRING:
      m_blockUpdate = true;
        {
          int oldCurrent = box->currentIndex ();

          box->clear ();
          box->addItems (Utils::fromStdString
                         (up.get_string_string ()).split ('|'));
          if (box->count() > 0
              && oldCurrent >= 0
              && oldCurrent < box->count ())
            {
              box->setCurrentIndex (oldCurrent);
            }
          else
            {
              gh_manager::post_set (m_handle, "value",
                                    octave_value (box->count () > 0
                                                  ? 1.0 : 0.0),
                                    false);
            }
        }
      m_blockUpdate = false;
      break;

    case uicontrol::properties::ID_VALUE:
      m_blockUpdate = true;
        {
          Matrix value = up.get_value ().matrix_value ();

          if (value.numel () > 0)
            {
              if (value(0) !=  static_cast<int>(value(0)))
                {
                warning ("popupmenu value should be integer");
                }
              else
                {
                  int newIndex = int (value(0)) - 1;

                  if (newIndex >= 0 && newIndex < box->count ())
                    {
                      if (newIndex != box->currentIndex ())
                        box->setCurrentIndex (newIndex);
                    }
                  else
                    warning ("popupmenu value not within valid display range");
                }
            }
        }
      m_blockUpdate = false;
      break;

    default:
      BaseControl::update (pId);
      break;
    }
}

void
PopupMenuControl::currentIndexChanged (int index)
{
  if (! m_blockUpdate)
    {
      gh_manager::post_set (m_handle, "value",
                            octave_value (double (index + 1)),
                            false);
      gh_manager::post_callback (m_handle, "callback");
    }
}

}; // namespace QtHandles
