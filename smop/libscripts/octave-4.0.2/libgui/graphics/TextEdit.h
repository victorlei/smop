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

#ifndef __QtHandles_TextEdit__
#define __QtHandles_TextEdit__ 1

#include <QTextEdit>

namespace QtHandles
{

class TextEdit : public QTextEdit
{
  Q_OBJECT

public:
  TextEdit (QWidget* xparent) : QTextEdit(xparent) { }
  ~TextEdit (void) { }

signals:
  void editingFinished (void);
  void returnPressed (void);

protected:
  void focusOutEvent (QFocusEvent* event);
  void keyPressEvent (QKeyEvent* event);
};

}; // namespace QtHandles

#endif
