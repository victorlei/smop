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

#ifndef __QtHandles_GLCanvas__
#define __QtHandles_GLCanvas__ 1

#include <QGLWidget>

#include "Canvas.h"

namespace QtHandles
{

class GLCanvas : public QGLWidget, public Canvas
{
public:
  GLCanvas (QWidget* parent, const graphics_handle& handle);
  ~GLCanvas (void);

  void draw (const graphics_handle& handle);
  void toggleAxes (const graphics_handle& handle);
  void toggleGrid (const graphics_handle& handle);
  void autoAxes (const graphics_handle& handle);
  void drawZoomBox (const QPoint& p1, const QPoint& p2);
  void resize (int /* x */, int /* y */,
               int /* width */, int /* height */) { }
  graphics_object selectFromAxes (const graphics_object& ax,
                                  const QPoint& pt);
  QWidget* qWidget (void) { return this; }

protected:
  void paintGL (void);
  void mouseDoubleClickEvent (QMouseEvent* event);
  void mouseMoveEvent (QMouseEvent* event);
  void mousePressEvent (QMouseEvent* event);
  void mouseReleaseEvent (QMouseEvent* event);
  void wheelEvent (QWheelEvent* event);
  void keyPressEvent (QKeyEvent* event);
  void keyReleaseEvent (QKeyEvent* event);
};

}; // namespace QtHandles

#endif
