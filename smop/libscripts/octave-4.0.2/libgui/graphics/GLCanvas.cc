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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "gl-render.h"
#include "graphics.h"

#include "GLCanvas.h"
#include "gl-select.h"

namespace QtHandles
{

GLCanvas::GLCanvas (QWidget* xparent, const graphics_handle& gh)
#if defined (Q_OS_WIN32)
  : QGLWidget (QGLFormat (QGL::SampleBuffers | QGL::IndirectRendering),
               xparent), Canvas (gh)
#else
  : QGLWidget (QGLFormat (QGL::SampleBuffers), xparent), Canvas (gh)
#endif
{
  setFocusPolicy (Qt::ClickFocus);
  setFocus();
}

GLCanvas::~GLCanvas (void)
{
}

void
GLCanvas::draw (const graphics_handle& gh)
{
  gh_manager::auto_lock lock;
  graphics_object go = gh_manager::get_object (gh);

  if (go)
    {
      opengl_renderer r;

      r.set_viewport (width (), height ());
      r.draw(go);
    }
}

void
GLCanvas::toggleAxes (const graphics_handle& gh)
{
  canvasToggleAxes (gh);
}

void
GLCanvas::toggleGrid (const graphics_handle& gh)
{
  canvasToggleGrid (gh);
}

void
GLCanvas::autoAxes (const graphics_handle& gh)
{
  canvasAutoAxes (gh);
}

graphics_object
GLCanvas::selectFromAxes (const graphics_object& ax, const QPoint& pt)
{
  makeCurrent ();

  if (ax)
    {
      opengl_selector s;

      s.set_viewport (width (), height ());
      return s.select (ax, pt.x (), height () - pt.y ());
    }

  return graphics_object ();
}

inline void
glDrawZoomBox (const QPoint& p1, const QPoint& p2)
{
  glVertex2d (p1.x (), p1.y ());
  glVertex2d (p2.x (), p1.y ());
  glVertex2d (p2.x (), p2.y ());
  glVertex2d (p1.x (), p2.y ());
  glVertex2d (p1.x (), p1.y ());
}

void
GLCanvas::drawZoomBox (const QPoint& p1, const QPoint& p2)
{
  glPushMatrix ();

  glMatrixMode (GL_MODELVIEW);
  glLoadIdentity ();

  glMatrixMode (GL_PROJECTION);
  glLoadIdentity ();
  glOrtho (0, width (), height (), 0, 1, -1);

  glPushAttrib (GL_DEPTH_BUFFER_BIT | GL_CURRENT_BIT);
  glDisable (GL_DEPTH_TEST);

  glBegin (GL_POLYGON);
  glColor4f (0.45, 0.62, 0.81, 0.1);
  glDrawZoomBox (p1, p2);
  glEnd ();

  glBegin (GL_LINE_STRIP);
  glLineWidth (1.5);
  glColor4f (0.45, 0.62, 0.81, 0.9);
  glDrawZoomBox (p1, p2);
  glEnd ();

  glPopAttrib ();
  glPopMatrix ();
}

void
GLCanvas::paintGL (void)
{
  canvasPaintEvent ();
}

void
GLCanvas::mouseDoubleClickEvent (QMouseEvent* xevent)
{
  canvasMouseDoubleClickEvent (xevent);
}

void
GLCanvas::mouseMoveEvent (QMouseEvent* xevent)
{
  canvasMouseMoveEvent (xevent);
}

void
GLCanvas::mousePressEvent (QMouseEvent* xevent)
{
  canvasMousePressEvent (xevent);
}

void
GLCanvas::mouseReleaseEvent (QMouseEvent* xevent)
{
  canvasMouseReleaseEvent (xevent);
}

void
GLCanvas::wheelEvent (QWheelEvent* xevent)
{
  canvasWheelEvent (xevent);
}

void
GLCanvas::keyPressEvent (QKeyEvent* xevent)
{
  if (! canvasKeyPressEvent (xevent))
    QGLWidget::keyPressEvent (xevent);
}

void
GLCanvas::keyReleaseEvent (QKeyEvent* xevent)
{
  if (! canvasKeyReleaseEvent (xevent))
    QGLWidget::keyReleaseEvent (xevent);
}

}; // namespace QtHandles
