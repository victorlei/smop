/*

Copyright (C) 2012-2015 Richard Crozier

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

#if !defined (octave_octave_dock_widget_h)
#define octave_octave_dock_widget_h 1

#include <QDockWidget>
#include <QSettings>
#include <QIcon>
#include <QMainWindow>
#include <QToolButton>
#include <QMouseEvent>

class octave_dock_widget : public QDockWidget
{
  Q_OBJECT

public:

  octave_dock_widget (QWidget *p = 0);
  virtual ~octave_dock_widget ();

  virtual void connect_visibility_changed (void);
  void make_window (void);
  void make_widget (bool dock=true);
  void set_title (const QString&);
  void set_predecessor_widget (octave_dock_widget *prev_widget);
signals:

  /** Custom signal that tells whether a user has clicked away
   *  that dock widget, i.e the active dock widget has
   *  changed. */
  void active_changed (bool active);

protected:

  virtual void closeEvent (QCloseEvent *e);

  QWidget * focusWidget ();

public slots:

  virtual void focus (void)
  {
    if (! isVisible ())
      setVisible (true);

    setFocus ();
    activateWindow ();
    raise ();
  }

  virtual void handle_visibility (bool visible)
  {
    if (visible && ! isFloating ())
      focus ();
  }

  virtual void notice_settings (const QSettings*)
  {
  }
  void handle_settings (const QSettings*);

  void handle_active_dock_changed (octave_dock_widget*, octave_dock_widget*);

  QMainWindow *main_win () { return _parent; }

protected slots:

  /** Slot to steer changing visibility from outside. */
  virtual void handle_visibility_changed (bool visible)
  {
    if (visible)
      emit active_changed (true);
  }
  /** slots to handle copy & paste */
  virtual void copyClipboard () {  }
  virtual void pasteClipboard () {  }
  virtual void selectAll () {  }
  /** slots to handle undo */
  virtual void do_undo () {  }

  // event filter for double clicks into the window decoration elements
  bool eventFilter(QObject *obj, QEvent *e);

  virtual void add_actions (QList<QAction *> action_list);

private slots:

  void change_floating (bool);
  void change_visibility (bool);

private:

  void set_style (bool active);
  void set_focus_predecessor ();

  QMainWindow *_parent;  // store the parent since we are reparenting to 0
  bool _floating;
  bool _custom_style;
  int _title_3d;
  int _icon_size;
  QColor _bg_color;
  QColor _bg_color_active;
  QColor _fg_color;
  QColor _fg_color_active;
  QString _icon_color;
  QString _icon_color_active;
  octave_dock_widget *_predecessor_widget;

#if defined (Q_OS_WIN32)
  QWidget *_title_widget;
  QToolButton *_dock_button;
  QToolButton *_close_button;
  QAction *_dock_action;
  QAction *_close_action;
#endif

};

#endif
