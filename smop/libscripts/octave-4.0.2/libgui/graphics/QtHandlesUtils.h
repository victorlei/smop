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

#ifndef __QtHandles_Utils__
#define __QtHandles_Utils__ 1

#include <QColor>
#include <QFont>
#include <QImage>
#include <QString>
#include <QStringList>

#include <string>

#include "graphics.h"

class QKeyEvent;
class QMouseEvent;

namespace QtHandles
{

namespace Utils
{
  QString fromStdString (const std::string& s);
  std::string toStdString (const QString& s);

  QStringList fromStringVector (const string_vector& v);
  string_vector toStringVector (const QStringList& l);

  Cell toCellString (const QStringList& l);

  template <class T>
  QFont computeFont (const typename T::properties& props, int height = -1);

  QColor fromRgb (const Matrix& rgb);
  Matrix toRgb (const QColor& c);

  Qt::Alignment fromHVAlign (const caseless_str& halign,
                             const caseless_str& valign);

  std::string figureSelectionType (QMouseEvent* event,
                                   bool isDoubleClick = false);

  Matrix figureCurrentPoint (const graphics_object& fig, QMouseEvent* event);
  Matrix figureCurrentPoint (const graphics_object& fig);

  template <class T>
  inline typename T::properties&
  properties (graphics_object obj)
    { return dynamic_cast<typename T::properties&> (obj.get_properties ()); }

  template <class T>
  inline typename T::properties&
  properties (const graphics_handle& h)
    { return Utils::properties<T> (gh_manager::get_object (h)); }

  QImage makeImageFromCData (const octave_value& v, int width = -1,
                             int height = -1);

  octave_scalar_map makeKeyEventStruct (QKeyEvent* event);
};

}; // namespace QtHandles

#endif
