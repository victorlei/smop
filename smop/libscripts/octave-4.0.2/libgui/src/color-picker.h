//
// This class provides a simple color picker based on tQColorButton
// by Harald Jedele, 23.03.01, GPL version 2 or any later version.
//
// Copyright (C) FZI Forschungszentrum Informatik Karlsruhe
// Copyright (C) 2013-2015 Torsten
//
// This file is part of Octave.
//
// Octave is free software; you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3 of the License, or (at your
// option) any later version.
//
// Octave is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
// for more details.
//
// You should have received a copy of the GNU General Public License
// along with Octave; see the file COPYING.  If not, see
// <http://www.gnu.org/licenses/>.
//

// Author: Torsten <ttl@justmail.de>

#ifndef COLORPICKER_H
#define COLORPICKER_H

#include <QPushButton>
#include <QColorDialog>

class color_picker: public QPushButton
{
  Q_OBJECT

public:
  color_picker (QColor color = QColor (0,0,0), QWidget *parent = 0);
  QColor color () const { return _color; }

private slots:
  void select_color ();

private:
  virtual void update_button ();
  QColor _color;
};

#endif
