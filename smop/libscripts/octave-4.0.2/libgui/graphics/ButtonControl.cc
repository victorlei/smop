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

#include <QAbstractButton>

#include "ButtonControl.h"
#include "Container.h"
#include "QtHandlesUtils.h"

namespace QtHandles
{

ButtonControl::ButtonControl (const graphics_object& go, QAbstractButton* btn)
  : BaseControl (go, btn), m_blockCallback (false)
{
  uicontrol::properties& up = properties<uicontrol> ();

  btn->setText (Utils::fromStdString (up.get_string_string ()));
  if (btn->isCheckable () || up.style_is ("togglebutton"))
    {
      btn->setCheckable (true);

      Matrix value = up.get_value ().matrix_value ();

      if (value.numel () > 0 && value(0) == up.get_max ())
        btn->setChecked (true);
    }

  connect (btn, SIGNAL (clicked (void)), SLOT (clicked (void)));
  connect (btn, SIGNAL (toggled (bool)), SLOT (toggled (bool)));
}

ButtonControl::~ButtonControl (void)
{
}

void
ButtonControl::update (int pId)
{
  uicontrol::properties& up = properties<uicontrol> ();
  QAbstractButton* btn = qWidget<QAbstractButton> ();

  switch (pId)
    {
    case uicontrol::properties::ID_STRING:
      btn->setText (Utils::fromStdString (up.get_string_string ()));
      break;

    case uicontrol::properties::ID_VALUE:
      m_blockCallback = true;
      if (btn->isCheckable ())
        {
          Matrix value = up.get_value ().matrix_value ();

          if (value.numel () > 0)
            {
              double dValue = value(0);

              if (dValue != 0.0 && dValue != 1.0)
                warning ("button value not within valid display range");
              else if (dValue == up.get_min () && btn->isChecked ())
                btn->setChecked (false);
              else if (dValue == up.get_max () && ! btn->isChecked ())
                btn->setChecked (true);
            }
        }
      m_blockCallback = false;
      break;

    default:
      BaseControl::update (pId);
      break;
    }
}

void
ButtonControl::toggled (bool checked)
{
  QAbstractButton* btn = qWidget<QAbstractButton> ();

  if (! m_blockCallback && btn->isCheckable ())
    {
      gh_manager::auto_lock lock;

      uicontrol::properties& up = properties<uicontrol> ();

      Matrix oldValue = up.get_value ().matrix_value ();
      double newValue = (checked ? up.get_max () : up.get_min ());

      if (oldValue.numel() != 1
          || (newValue != oldValue(0)))
        gh_manager::post_set (m_handle, "value", newValue, false);
      gh_manager::post_callback (m_handle, "callback");
    }
}

void
ButtonControl::clicked (void)
{
  QAbstractButton* btn = qWidget<QAbstractButton> ();

  if (! btn->isCheckable ())
    gh_manager::post_callback (m_handle, "callback");
}

};
