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

#include <QKeyEvent>

#include "TextEdit.h"

namespace QtHandles
{

void
TextEdit::focusOutEvent (QFocusEvent* xevent)
{
  QTextEdit::focusOutEvent (xevent);

  emit editingFinished ();
}

void
TextEdit::keyPressEvent (QKeyEvent* xevent)
{
  QTextEdit::keyPressEvent (xevent);

  if ((xevent->key () == Qt::Key_Return
       || xevent->key () == Qt::Key_Enter)
      && xevent->modifiers () == Qt::ControlModifier)
    emit returnPressed ();
}

}; // namespace QtHandles
