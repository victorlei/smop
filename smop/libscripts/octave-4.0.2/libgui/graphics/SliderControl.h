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

#ifndef __QtHandles_SliderControl__
#define __QtHandles_SliderControl__ 1

#include "BaseControl.h"

class QAbstractSlider;

namespace QtHandles
{

class SliderControl : public BaseControl
{
  Q_OBJECT

public:
  SliderControl (const graphics_object& go, QAbstractSlider* slider);
  ~SliderControl (void);

  static SliderControl* create (const graphics_object& go);

protected:
  void update (int pId);

private slots:
  void valueChanged (int ival);

private:
  bool m_blockUpdates;
};

}; // namespace QtHandles

#endif
