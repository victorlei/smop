/*

Copyright (C) 2011-2015 Jacob Dawid

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

#ifndef DOCUMENTATIONDOCKWIDGET_H
#define DOCUMENTATIONDOCKWIDGET_H

#include "octave-dock-widget.h"

#include "webinfo.h"

class documentation_dock_widget : public octave_dock_widget
{
  Q_OBJECT

public:

  documentation_dock_widget (QWidget *parent = 0);

protected slots:
  void copyClipboard ();
  void pasteClipboard ();
  void selectAll ();

  void showDoc (const QString & name);
private:

  webinfo *_webinfo;
};

#endif // DOCUMENTATIONDOCKWIDGET_H
