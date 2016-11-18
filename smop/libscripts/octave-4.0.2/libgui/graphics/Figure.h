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

#ifndef __QtHandles_Figure__
#define __QtHandles_Figure__ 1

#include <QRect>

#include "GenericEventNotify.h"
#include "MenuContainer.h"
#include "Object.h"

class QMainWindow;
class QToolBar;

namespace QtHandles
{

enum MouseMode
{
  // NOTE: These values must match the order of the buttons in the
  // MouseModeActionGroup object.

  NoMode        = 0,
  RotateMode    = 1,
  ZoomInMode    = 2,
  ZoomOutMode   = 3,
  PanMode       = 4,
  TextMode      = 5,
  SelectMode    = 6
};

class Container;
class FigureWindow;
class MenuBar;
class ToolBar;

class MouseModeActionGroup;

class Figure :
  public Object,
  public MenuContainer,
  public GenericEventNotifyReceiver
{
  Q_OBJECT

  friend class ToolBar;

public:
  Figure (const graphics_object& go, FigureWindow* win);
  ~Figure (void);

  static Figure* create (const graphics_object& go);

  QString fileName (void);
  void setFileName (const QString& name);

  MouseMode mouseMode (void);

  Container* innerContainer (void);
  QWidget* menu (void);

  bool eventNotifyBefore (QObject* watched, QEvent* event);
  void eventNotifyAfter (QObject* watched, QEvent* event);

protected:
  enum UpdateBoundingBoxFlag
    {
      UpdateBoundingBoxPosition = 0x1,
      UpdateBoundingBoxSize     = 0x2,
      UpdateBoundingBoxAll      = 0x3
    };

protected:
  void redraw (void);
  void print (const QString& file_cmd, const QString& term);
  void update (int pId);
  void updateBoundingBox (bool internal = false, int flags = 0);
  void beingDeleted (void);

private:
  void createFigureToolBarAndMenuBar (void);
  void showFigureToolBar (bool visible);
  void showMenuBar (bool visible);
  void addCustomToolBar (QToolBar* bar, bool visible);
  void showCustomToolBar (QToolBar* bar, bool visible);

  void updateFigureToolBarAndMenuBar (void);

  static void updateBoundingBoxHelper (void*);

  void save_figure_callback (const std::string& file);
  void copy_figure_callback (const std::string& format);

private slots:
  void setMouseMode (MouseMode mode);
  void fileSaveFigure (bool prompt = false);
  void fileSaveFigureAs (void);
  void fileCloseFigure (void);
  void editCopy (bool choose_format = false);
  void helpAboutQtHandles (void);
  void updateMenuBar (void);
  void updateContainer (void);
  void toggleAxes (void);
  void toggleGrid (void);
  void autoAxes (void);

signals:
  void asyncUpdate (void);

private:
  Container* m_container;
  bool m_blockUpdates;
  QToolBar* m_figureToolBar;
  MenuBar* m_menuBar;
  QRect m_innerRect;
  QRect m_outerRect;
  MouseModeActionGroup* m_mouseModeGroup;
};

}; // namespace QtHandles

#endif
