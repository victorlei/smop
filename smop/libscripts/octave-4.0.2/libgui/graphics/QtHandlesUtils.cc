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

#include <QApplication>
#include <QKeyEvent>
#include <QMouseEvent>

#include <list>

#include "ov.h"
#include "graphics.h"

#include "Backend.h"
#include "Container.h"
#include "KeyMap.h"
#include "Object.h"
#include "QtHandlesUtils.h"

namespace QtHandles
{

namespace Utils
{

QString
fromStdString (const std::string& s)
{
  return QString::fromLocal8Bit (s.c_str ());
}

std::string
toStdString (const QString& s)
{
  return std::string (s.toLocal8Bit ().data ());
}

QStringList
fromStringVector (const string_vector& v)
{
  QStringList l;
  octave_idx_type n = v.length ();

  for (octave_idx_type i = 0; i < n; i++)
    l << fromStdString (v[i]);

  return l;
}

string_vector
toStringVector (const QStringList& l)
{
  string_vector v (l.length ());
  int i = 0;

  foreach (const QString& s, l)
    v[i++] = toStdString (s);

  return v;
}

Cell toCellString (const QStringList& l)
{
  QStringList tmp = l;

  // dont get any empty lines from end of the list
  while ((tmp.length () > 0) && (tmp.last ().length () == 0))
    {
      tmp.removeLast ();
    }
  // no strings will be a a 1x1 cell with empty string
  if (tmp.length () == 0)
    tmp += "";

  Cell v(toStringVector (tmp));
  return v;
}

template <class T>
QFont
computeFont (const typename T::properties& props, int height)
{
  QFont f (fromStdString (props.get_fontname ()));

  static std::map<std::string, QFont::Weight> weightMap;
  static std::map<std::string, QFont::Style> angleMap;
  static bool mapsInitialized = false;

  if (! mapsInitialized)
    {
      weightMap[std::string ("normal")] = QFont::Normal;
      weightMap[std::string ("light")] = QFont::Light;
      weightMap[std::string ("demi")] = QFont::DemiBold;
      weightMap[std::string ("bold")] = QFont::Normal;

      angleMap[std::string ("normal")] = QFont::StyleNormal;
      angleMap[std::string ("italic")] = QFont::StyleItalic;
      angleMap[std::string ("oblique")] = QFont::StyleOblique;

      mapsInitialized = true;
    }

  f.setPointSizeF (props.get_fontsize_points (height));
  f.setWeight (weightMap[props.get_fontweight ()]);
  f.setStyle (angleMap[props.get_fontangle ()]);

  return f;
}

template QFont computeFont<uicontrol> (const uicontrol::properties& props,
                                       int height);

template QFont computeFont<uipanel> (const uipanel::properties& props,
                                     int height);

QColor
fromRgb (const Matrix& rgb)
{
  QColor c;

  if (rgb.numel () == 3)
    c.setRgbF (rgb(0), rgb(1), rgb(2));

  return c;
}

Matrix
toRgb (const QColor& c)
{
  Matrix rgb (1, 3);
  double* rgbData = rgb.fortran_vec ();

  // qreal is a typedef for double except for ARM CPU architectures
  // where it is a typedef for float (Bug #44970).
  qreal tmp[3];
  c.getRgbF (tmp, tmp+1, tmp+2);
  rgbData[0] = tmp[0]; rgbData[1] = tmp[1]; rgbData[2] = tmp[2];

  return rgb;
}

std::string
figureSelectionType (QMouseEvent* event, bool isDoubleClick)
{
  if (isDoubleClick)
    return std::string ("open");
  else
    {
      Qt::MouseButtons buttons = event->buttons ();
      Qt::KeyboardModifiers mods = event->modifiers ();

      if (mods == Qt::NoModifier)
        {
          if (buttons == Qt::LeftButton)
            return std::string ("normal");
          else if (buttons == Qt::RightButton)
            return std::string ("alt");
#if defined (Q_WS_WIN)
          else if (buttons == (Qt::LeftButton|Qt::RightButton))
            return std::string ("extend");
#elif defined (Q_WS_X11)
          else if (buttons == Qt::MidButton)
            return std::string ("extend");
#endif
        }
      else if (buttons == Qt::LeftButton)
        {
          if (mods == Qt::ShiftModifier)
            return std::string ("extend");
          else if (mods == Qt::ControlModifier)
            return std::string ("alt");
        }
    }

  return std::string ("normal");
}

/*
   Two figureCurrentPoint() routines are required:
   1) Used for QMouseEvents where cursor position data is in callback from Qt.
   2) Used for QKeyEvents where cursor position must be determined.
*/
Matrix
figureCurrentPoint (const graphics_object& fig, QMouseEvent* event)
{
  Object* tkFig = Backend::toolkitObject (fig);

  if (tkFig)
    {
      Container* c = tkFig->innerContainer ();

      if (c)
        {
          QPoint qp = c->mapFromGlobal (event->globalPos ());

          return tkFig->properties<figure> ().map_from_boundingbox (qp.x (),
                                                                    qp.y ());
        }
    }

  return Matrix (1, 2, 0.0);
}

Matrix
figureCurrentPoint (const graphics_object& fig)
{
  Object* tkFig = Backend::toolkitObject (fig);

  if (tkFig)
    {
      Container* c = tkFig->innerContainer ();

      if (c)
        {
          // FIXME: QCursor::pos() may give inaccurate results with asynchronous
          //        window systems like X11 over ssh.
          QPoint qp = c->mapFromGlobal (QCursor::pos ());

          return tkFig->properties<figure> ().map_from_boundingbox (qp.x (),
                                                                    qp.y ());
        }
    }

  return Matrix (1, 2, 0.0);
}

Qt::Alignment
fromHVAlign (const caseless_str& halign, const caseless_str& valign)
{
  Qt::Alignment flags;

  if (halign.compare ("left"))
    flags |= Qt::AlignLeft;
  else if (halign.compare ("center"))
    flags |= Qt::AlignHCenter;
  else if (halign.compare ("right"))
    flags |= Qt::AlignRight;
  else
    flags |= Qt::AlignLeft;

  if (valign.compare ("middle"))
    flags |= Qt::AlignVCenter;
  else if (valign.compare ("top"))
    flags |= Qt::AlignTop;
  else if (valign.compare ("bottom"))
    flags |= Qt::AlignBottom;
  else
    flags |= Qt::AlignVCenter;

  return flags;
}

QImage
makeImageFromCData (const octave_value& v, int width, int height)
{
  dim_vector dv (v.dims ());

  if (dv.length () == 3 && dv(2) == 3)
    {
      int w = qMin (dv(1), static_cast<octave_idx_type> (width));
      int h = qMin (dv(0), static_cast<octave_idx_type> (height));

      int x_off = (w < width ? (width - w) / 2 : 0);
      int y_off = (h < height ? (height - h) / 2 : 0);

      QImage img (width, height, QImage::Format_ARGB32);
      img.fill (qRgba (0, 0, 0, 0));

      if (v.is_uint8_type ())
        {
          uint8NDArray d = v.uint8_array_value ();

          for (int i = 0; i < w; i++)
            for (int j = 0; j < h; j++)
              {
                int r = d(j, i, 0);
                int g = d(j, i, 1);
                int b = d(j, i, 2);
                int a = 255;

                img.setPixel (x_off + i, y_off + j, qRgba (r, g, b, a));
              }
        }
      else if (v.is_single_type ())
        {
          FloatNDArray f = v.float_array_value ();

          for (int i = 0; i < w; i++)
            for (int j = 0; j < h; j++)
              {
                float r = f(j, i, 0);
                float g = f(j, i, 1);
                float b = f(j, i, 2);
                int a = (xisnan (r) || xisnan (g) || xisnan (b) ? 0 : 255);

                img.setPixel (x_off + i, y_off + j,
                              qRgba (xround (r * 255),
                                     xround (g * 255),
                                     xround (b * 255),
                                     a));
              }
        }
      else if (v.is_real_type ())
        {
          NDArray d = v.array_value ();

          for (int i = 0; i < w; i++)
            for (int j = 0; j < h; j++)
              {
                double r = d(j, i, 0);
                double g = d(j, i, 1);
                double b = d(j, i, 2);
                int a = (xisnan (r) || xisnan (g) || xisnan (b) ? 0 : 255);

                img.setPixel (x_off + i, y_off + j,
                              qRgba (xround (r * 255),
                                     xround (g * 255),
                                     xround (b * 255),
                                     a));
              }
        }

      return img;
    }

  return QImage ();
}

octave_scalar_map
makeKeyEventStruct (QKeyEvent* event)
{
  octave_scalar_map retval;

  retval.setfield ("Key", KeyMap::qKeyToKeyString (event->key ()));
  retval.setfield ("Character", toStdString (event->text ()));

  std::list<std::string> modList;
  Qt::KeyboardModifiers mods = event->modifiers ();

  if (mods & Qt::ShiftModifier)
    modList.push_back ("shift");
  if (mods & Qt::ControlModifier)
#ifdef Q_OS_MAC
    modList.push_back ("command");
#else
    modList.push_back ("control");
#endif
  if (mods & Qt::AltModifier)
    modList.push_back ("alt");
#ifdef Q_OS_MAC
  if (mods & Qt::MetaModifier)
    modList.push_back ("control");
#endif

  retval.setfield ("Modifier", Cell (modList));

  return retval;
}

}; // namespace Utils

}; // namespace QtHandles
