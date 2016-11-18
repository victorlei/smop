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

#include <QScrollBar>

#include "Container.h"
#include "SliderControl.h"
#include "QtHandlesUtils.h"

#define RANGE_INT_MAX 1000000

namespace QtHandles
{

SliderControl*
SliderControl::create (const graphics_object& go)
{
  Object* parent = Object::parentObject (go);

  if (parent)
    {
      Container* container = parent->innerContainer ();

      if (container)
        return new SliderControl (go, new QScrollBar (container));
    }

  return 0;
}

SliderControl::SliderControl (const graphics_object& go,
                              QAbstractSlider* slider)
  : BaseControl (go, slider), m_blockUpdates (false)
{
  uicontrol::properties& up = properties<uicontrol> ();

  slider->setTracking (false);
  Matrix bb = up.get_boundingbox ();
  slider->setOrientation (bb(2) > bb(3) ? Qt::Horizontal : Qt::Vertical);
  Matrix steps = up.get_sliderstep ().matrix_value ();
  slider->setMinimum (0);
  slider->setMaximum (RANGE_INT_MAX);
  slider->setSingleStep (xround (steps(0) * RANGE_INT_MAX));
  slider->setPageStep (xround (steps(1) * RANGE_INT_MAX));
  Matrix value = up.get_value ().matrix_value ();
  if (value.numel () > 0)
    {
      double dmin = up.get_min (), dmax = up.get_max ();

      slider->setValue (xround (((value(0) - dmin) / (dmax - dmin))
                                * RANGE_INT_MAX));
    }

  connect (slider, SIGNAL (valueChanged (int)), SLOT (valueChanged (int)));
}

SliderControl::~SliderControl (void)
{
}

void
SliderControl::update (int pId)
{
  uicontrol::properties& up = properties<uicontrol> ();
  QScrollBar* slider = qWidget<QScrollBar> ();

  switch (pId)
    {
    case uicontrol::properties::ID_SLIDERSTEP:
      {
        Matrix steps = up.get_sliderstep ().matrix_value ();

        slider->setSingleStep (xround (steps(0) * RANGE_INT_MAX));
        slider->setPageStep (xround (steps(1) * RANGE_INT_MAX));
      }
      break;

    case uicontrol::properties::ID_VALUE:
      {
        Matrix value = up.get_value ().matrix_value ();
        double dmax = up.get_max (), dmin = up.get_min ();

        if (value.numel () > 0)
          {
            int ival = xround (((value(0) - dmin) / (dmax - dmin))
                               * RANGE_INT_MAX);

            m_blockUpdates = true;
            slider->setValue (ival);
            m_blockUpdates = false;
          }
      }
      break;

    default:
      BaseControl::update (pId);
      break;
    }
}

void
SliderControl::valueChanged (int ival)
{
  if (! m_blockUpdates)
    {
      gh_manager::auto_lock lock;
      graphics_object go = object ();

      if (go.valid_object ())
        {
          uicontrol::properties& up = Utils::properties<uicontrol> (go);

          Matrix value = up.get_value ().matrix_value ();
          double dmin = up.get_min (), dmax = up.get_max ();

          int ival_tmp = (value.numel () > 0 ?
                          xround (((value(0) - dmin) / (dmax - dmin))
                                  * RANGE_INT_MAX) :
                          0);

          if (ival != ival_tmp || value.numel () > 0)
            {
              double dval = dmin + (ival * (dmax - dmin) / RANGE_INT_MAX);

              gh_manager::post_set (m_handle, "value", octave_value (dval));
              gh_manager::post_callback (m_handle, "callback");
            }
        }
    }
}

}; // namespace QtHandles
