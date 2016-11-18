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

#ifndef __QtHandles_BaseControl__
#define __QtHandles_BaseControl__ 1

#include "Object.h"

class QEvent;
class QObject;

namespace QtHandles
{

class BaseControl : public Object
{
public:
  BaseControl (const graphics_object& go, QWidget* w);
  ~BaseControl (void);

  Container* innerContainer (void) { return 0; }

  bool eventFilter (QObject* watched, QEvent* e);

protected:
  void init (QWidget* w, bool callBase = false);
  void update (int pId);

private:
  bool m_normalizedFont;
  bool m_keyPressHandlerDefined;
};

}; // namespace QtHandles

#endif
