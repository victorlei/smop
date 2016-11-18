/*

Copyright (C) 2012-2015 Richard Crozier
Copyright (C) 2013-2015 Torsten <ttl@justmail.de>

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
#include <QToolBar>
#include <QAction>
#include <QHBoxLayout>
#include <QLabel>
#include <QSettings>
#include <QStyle>

#include "resource-manager.h"
#include "octave-dock-widget.h"


octave_dock_widget::octave_dock_widget (QWidget *p)
  : QDockWidget (p)
{

  _parent = static_cast<QMainWindow *> (p);     // store main window
  _floating = false;
  _predecessor_widget = 0;

  connect (this, SIGNAL (visibilityChanged (bool)),
           this, SLOT (handle_visibility_changed (bool)));

  connect (p, SIGNAL (settings_changed (const QSettings*)),
           this, SLOT (handle_settings (const QSettings*)));

  connect (p, SIGNAL (active_dock_changed (octave_dock_widget*, octave_dock_widget*)),
           this, SLOT (handle_active_dock_changed (octave_dock_widget*, octave_dock_widget*)));

  QStyle *st = style ();
  _icon_size = 0.75*st->pixelMetric (QStyle::PM_SmallIconSize);

#if defined (Q_OS_WIN32)
  // windows: add an extra title bar that persists when floating

  setFeatures (QDockWidget::DockWidgetMovable); // not floatable or closeable

  // the custom (extra) title bar of the widget
  _dock_action = new QAction
                   (QIcon (":/actions/icons/widget-undock.png"), "", this);
  _dock_action-> setToolTip (tr ("Undock widget"));
  connect (_dock_action, SIGNAL (triggered (bool)),
           this, SLOT (change_floating (bool)));
  _dock_button = new QToolButton (this);
  _dock_button->setDefaultAction (_dock_action);
  _dock_button->setFocusPolicy (Qt::NoFocus);
  _dock_button->setIconSize (QSize (_icon_size,_icon_size));

  _close_action = new QAction
                   (QIcon (":/actions/icons/widget-close.png"), "", this);
  _close_action-> setToolTip (tr ("Hide widget"));
  connect (_close_action, SIGNAL (triggered (bool)),
           this, SLOT (change_visibility (bool)));
  _close_button = new QToolButton (this);
  _close_button->setDefaultAction (_close_action);
  _close_button->setFocusPolicy (Qt::NoFocus);
  _close_button->setIconSize (QSize (_icon_size,_icon_size));

  _icon_color = "";
  _title_3d = 50;

  QHBoxLayout *h_layout = new QHBoxLayout ();
  h_layout->addStretch (100);
  h_layout->addWidget (_dock_button);
  h_layout->addWidget (_close_button);
  h_layout->setSpacing (0);
  h_layout->setContentsMargins (5,2,2,2);

  _title_widget = new QWidget ();
  _title_widget->setLayout (h_layout);
  setTitleBarWidget (_title_widget);

#else

  // non windows: qt takes control of floating widgets
  setFeatures (QDockWidget::DockWidgetMovable |
               QDockWidget::DockWidgetClosable |
               QDockWidget::DockWidgetFloatable); // floatable and closeable

  connect (this, SIGNAL (topLevelChanged (bool)),
           this, SLOT (change_floating (bool)));

#endif

  // adding actions of the main window
  connect (p, SIGNAL (add_actions_signal (QList<QAction *>)),
           this, SLOT (add_actions (QList<QAction *>)));
  // copy & paste handling
  connect (p, SIGNAL (copyClipboard_signal ()),
           this, SLOT (copyClipboard ()));
  connect (p, SIGNAL (pasteClipboard_signal ()),
           this, SLOT (pasteClipboard ()));
  connect (p, SIGNAL (selectAll_signal ()),
           this, SLOT (selectAll ()));
  // undo handling
  connect (p, SIGNAL (undo_signal ()), this, SLOT (do_undo ()));

  installEventFilter (this);

  setFocusPolicy (Qt::StrongFocus);
}

octave_dock_widget::~octave_dock_widget ()
{
  // save state of this dock-widget
  QString name = objectName ();
  QSettings *settings = resource_manager::get_settings ();

  settings->beginGroup ("DockWidgets");

#if defined (Q_OS_WIN32)
  if (_floating) // widget is floating (windows), save actual floating geometry
    settings->setValue (name+"_floating_geometry", geometry ());
  else           // not floating save docked (normal) geometry
#endif
    settings->setValue (name, saveGeometry ());

  settings->setValue (name+"Visible", isVisible ()); // store visibility
  settings->setValue (name+"Floating", _floating);    // store visibility
  settings->setValue (name+"_minimized", isMinimized ()); // store minimized

  settings->endGroup ();
  settings->sync ();
}

// connect signal visibility changed to related slot (called from main-window)
void
octave_dock_widget::connect_visibility_changed (void)
{
  connect (this, SIGNAL (visibilityChanged (bool)),
           this, SLOT (handle_visibility (bool)));
  emit active_changed (isVisible ());  // emit once for init of window menu
}


// set the widget which previously had focus when tabified
void
octave_dock_widget::set_predecessor_widget (octave_dock_widget *prev_widget)
{
  _predecessor_widget = prev_widget;
}

// set the title in the dockwidgets title bar
void
octave_dock_widget::set_title (const QString& title)
{
#if defined (Q_OS_WIN32)
  QHBoxLayout* h_layout =
    static_cast<QHBoxLayout *> (titleBarWidget ()->layout ());
  QLabel *label = new QLabel (title);
  label->setStyleSheet ("background: transparent;");
  h_layout->insertWidget (0,label);
#endif
  setWindowTitle (title);
}

// set focus to previously active widget in tabbed widget stack
void
octave_dock_widget::set_focus_predecessor ()
{
  if (_predecessor_widget)    // only != 0 if widget was tabbed
    _predecessor_widget->focus ();

  _predecessor_widget = 0;
}

// make the widget floating
void
octave_dock_widget::make_window ()
{
#if defined (Q_OS_WIN32)

  // windows: the widget has to be reparented (parent = 0)

  QSettings *settings = resource_manager::get_settings ();

  // save the docking area and geometry for later redocking
  // FIXME: dockWidgetArea always returns 2
  settings->setValue ("DockWidgets/" + objectName () + "_dock_area",
                      _parent->dockWidgetArea (this));
  settings->setValue ("DockWidgets/" + objectName (), saveGeometry ());
  settings->sync ();

  // remove parent and adjust the (un)dock icon
  setParent (0, Qt::Window);
  _dock_action->setIcon (QIcon (":/actions/icons/widget-dock"+_icon_color+".png"));
  _dock_action->setToolTip (tr ("Dock widget"));

  // restore the last geometry when floating
  setGeometry (settings->value ("DockWidgets/" + objectName ()
                       + "_floating_geometry",QRect(50,100,480,480)).toRect ());

#else

  // non windows: Just set the appripriate window flag
  setWindowFlags (Qt::Window);

  QString css = styleSheet ();
  css.replace ("widget-undock","widget-dock");
  setStyleSheet (css);

#endif

  _floating = true;

  set_focus_predecessor ();  // set focus previously active widget if tabbed
}


// dock the widget
void
octave_dock_widget::make_widget (bool dock)
{
#if defined (Q_OS_WIN32)

  // windows: Since floating widget has no parent, we have to read it

  QSettings *settings = resource_manager::get_settings ();

  // save last floating geometry if widget really was floating
  if (_floating)
    settings->setValue ("DockWidgets/" + objectName () + "_floating_geometry",
                        geometry ());
  settings->sync ();

  if (dock)
    {
      // add widget to last saved docking area (dock=true is default)
      int area = settings->value ("DockWidgets/" + objectName () + "_dock_area",
                                  Qt::TopDockWidgetArea).toInt ();
      _parent->addDockWidget (static_cast<Qt::DockWidgetArea> (area), this);

      // FIXME: restoreGeometry is ignored for docked widgets
      //        and its child widget
      restoreGeometry (settings->value
             ("DockWidgets/" + objectName ()).toByteArray ());
    }
  else  // only reparent, no docking
    setParent (_parent);

  // adjust the (un)dock icon
  _dock_action->setIcon (QIcon (":/actions/icons/widget-undock"+_icon_color+".png"));
  _dock_action->setToolTip (tr ("Undock widget"));

#else

  // non windows: just say we are a docked widget again

  Q_UNUSED (dock);

  setWindowFlags (Qt::Widget);

  QString css = styleSheet ();
  css.replace ("widget-dock","widget-undock");
  setStyleSheet (css);

#endif

  _floating = false;
}

// slot for (un)dock action
void
octave_dock_widget::change_floating (bool)
{
  if (_floating)
    make_widget ();
  else
    {
      make_window ();
      focus ();
    }
}

// slot for hiding the widget
void
octave_dock_widget::change_visibility (bool)
{
  setVisible (false);
  emit active_changed (false);
}

// get focus widget
QWidget *
octave_dock_widget::focusWidget ()
{
  QWidget * w = QApplication::focusWidget ();
  if (w && w->focusProxy ()) w = w->focusProxy ();
  return w;
}

void
octave_dock_widget::set_style (bool active)
{
  QString css;
  QString css_button;
  QString dock_icon;

  QString icon_col = _icon_color;

  if (_floating)
    dock_icon = "widget-dock";
  else
    dock_icon = "widget-undock";

  if (_custom_style)
    {

      QColor bg_col, fg_col;

      if (active)
        {
          bg_col = _bg_color_active;
          fg_col = _fg_color_active;
          icon_col = _icon_color_active;
        }
      else
        {
          bg_col = _bg_color;
          fg_col = _fg_color;
          icon_col = _icon_color;
        }

      QColor bg_col_top, bg_col_bottom;
      if (_title_3d > 0)
        {
          bg_col_top = bg_col.lighter (100 + _title_3d);
          bg_col_bottom = bg_col.darker (100 + _title_3d);
        }
      else
        {
          bg_col_top = bg_col.darker (100 - _title_3d);
          bg_col_bottom = bg_col.lighter (100 - _title_3d);
        }

      QString background =
        QString ("background: qlineargradient(x1: 0, y1: 0, x2: 0, y2: 1,"
                 "            stop: 0 %1, stop: 0.60 %2, stop: 0.95 %2 stop: 1.0 %3);").
        arg (bg_col_top.name ()).
        arg (bg_col.name ()).
        arg (bg_col_bottom.name ());

#if defined (Q_OS_WIN32)
      css = background + QString (" color: %1 ;").arg (fg_col.name ());
#else
      css = QString ("QDockWidget::title { " + background +
                     "                     text-align: center left;"
                     "                     padding: 0px 0px 0px 4px;}\n"
                     "QDockWidget { color: %1 ; "
                     "  titlebar-close-icon: url(:/actions/icons/widget-close%2.png);"
                     "  titlebar-normal-icon: url(:/actions/icons/"+dock_icon+"%2); }"
                     "QDockWidget::close-button,"
                     "QDockWidget::float-button { border: 0px; icon-size: %3px; width: %3px}"
                     ).
                     arg (fg_col.name ()).arg (icon_col).arg (_icon_size);
#endif
    }
  else
    {
#if defined (Q_OS_WIN32)
      css = QString ("");
#else
      css = QString ("QDockWidget::title { text-align: center left;"
                     "                     padding: 0px 0px 0px 4px;}"
                     "QDockWidget {"
                     "  titlebar-close-icon: url(:/actions/icons/widget-close.png);"
                     "  titlebar-normal-icon: url(:/actions/icons/"+dock_icon+"); }"
                     "QDockWidget::close-button,"
                     "QDockWidget::float-button { border: 0px; icon-size: %1px; width: %1px}"
                    ).arg (_icon_size);
#endif
    }

#if defined (Q_OS_WIN32)
  _title_widget->setStyleSheet (css);
  css_button = QString ("background: transparent; border: 0px;");
  _dock_button->setStyleSheet (css_button);
  _close_button->setStyleSheet (css_button);
  _dock_action->setIcon (QIcon (":/actions/icons/" + dock_icon + icon_col +
                                ".png"));
  _close_action->setIcon (QIcon (":/actions/icons/widget-close" + icon_col +
                                 ".png"));
#else
  setStyleSheet (css);
#endif
}

void
octave_dock_widget::handle_settings (const QSettings *settings)
{
  _custom_style =
    settings->value ("DockWidgets/widget_title_custom_style",false).toBool ();

  _title_3d =
    settings->value ("DockWidgets/widget_title_3d",50).toInt ();

  QColor default_var = QColor (0,0,0);
  _fg_color = settings->value ("Dockwidgets/title_fg_color",
                               default_var).value<QColor> ();
  default_var = QColor (0,0,0);
  _fg_color_active = settings->value ("Dockwidgets/title_fg_color_active",
                                      default_var).value<QColor> ();

  default_var = QColor (255,255,255);
  _bg_color = settings->value ("Dockwidgets/title_bg_color",
                               default_var).value<QColor> ();
  default_var = QColor (192,192,192);
  _bg_color_active = settings->value ("Dockwidgets/title_bg_color_active",
                                      default_var).value<QColor> ();

  int r, g, b;
  _bg_color.getRgb (&r, &g, &b);
  if (r+g+b < 400)
    _icon_color = "-light";
  else
    _icon_color = "";

  _bg_color_active.getRgb (&r, &g, &b);
  if (r+g+b < 400)
    _icon_color_active = "-light";
  else
    _icon_color_active = "";

  notice_settings (settings);  // call individual handler

  set_style (false);
}

bool octave_dock_widget::eventFilter(QObject *obj, QEvent *e)
{
  if (e->type () == QEvent::NonClientAreaMouseButtonDblClick)
    {
      e->ignore (); // ignore double clicks into window decoration elements
      return true;
    }

  return QDockWidget::eventFilter (obj,e);
}

void
octave_dock_widget::handle_active_dock_changed (octave_dock_widget *w_old,
                                                octave_dock_widget *w_new)
{
  if (_custom_style && this == w_old)
    {
      set_style (false);
      update ();
    }

  if (_custom_style && this == w_new)
    {
      set_style (true);
      update ();
    }
}

// slot for adding actions from the main window
void
octave_dock_widget::add_actions (QList<QAction *> action_list)
{
  if (objectName () != "FileEditor")
    addActions (action_list);
}

// close event
void
octave_dock_widget::closeEvent (QCloseEvent *e)
{
  emit active_changed (false);
  set_focus_predecessor ();
  QDockWidget::closeEvent (e);
}
