/*

Copyright (C) 2011, 2013 Michael Goffioul.

This file is part of QConsole.

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not,
see <http://www.gnu.org/licenses/>.

*/

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

#include "QTerminalColors.h"

//////////////////////////////////////////////////////////////////////////////

QConsoleColors::QConsoleColors (void)
    : QMap<int, QColor> ()
{
  (*this)[0]  = Qt::black;
  (*this)[1]  = Qt::darkBlue;
  (*this)[2]  = Qt::darkGreen;
  (*this)[3]  = Qt::darkCyan;
  (*this)[4]  = Qt::darkRed;
  (*this)[5]  = Qt::darkMagenta;
  (*this)[6]  = Qt::darkYellow;
  (*this)[7]  = Qt::lightGray;
  (*this)[8]  = Qt::darkGray;
  (*this)[9]  = Qt::blue;
  (*this)[10] = Qt::green;
  (*this)[11] = Qt::cyan;
  (*this)[12] = Qt::red;
  (*this)[13] = Qt::magenta;
  (*this)[14] = Qt::yellow;
  (*this)[15] = Qt::white;
}
