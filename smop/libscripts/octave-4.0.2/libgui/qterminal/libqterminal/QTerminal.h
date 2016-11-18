/*

Copyright (C) 2012-2015 Michael Goffioul.
Copyright (C) 2012-2015 Jacob Dawid.

This file is part of QTerminal.

This program is free software; you can redistribute it and/or modify
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

#ifndef QTERMINAL_H
#define QTERMINAL_H

#include <QSettings>
#include <QKeySequence>
#include <QWidget>
#include <QStringList>
#include <QColor>
#include <QList>
#include <QMenu>
#include <QClipboard>
#include <QApplication>
#include <QAction>

#include "resource-manager.h"

class QTerminal : public QWidget
{
  Q_OBJECT

public:

  static QTerminal *create (QWidget *xparent = 0);

  static QList<QColor> default_colors (void);

  static QStringList color_names (void);

  virtual ~QTerminal (void) { }

  virtual void setTerminalFont (const QFont& font) = 0;

  virtual void setSize (int h, int v) = 0;

  virtual void sendText (const QString& text) = 0;

  virtual QString selectedText () = 0;

  virtual void has_extra_interrupt (bool extra) = 0;

  enum CursorType
  {
    UnderlineCursor,
    BlockCursor,
    IBeamCursor
  };

  virtual void setCursorType (CursorType type, bool blinking)
  {
    // Provide empty default impl in order to avoid conflicts with the
    // win impl.

    Q_UNUSED (type);
    Q_UNUSED (blinking);
  }

  virtual void setBackgroundColor (const QColor& color) = 0;

  virtual void setForegroundColor (const QColor& color) = 0;

  virtual void setSelectionColor (const QColor& color) = 0;

  virtual void setCursorColor (bool useForegroundColor,
                               const QColor& color) = 0;

  virtual void setScrollBufferSize(int value=1000) = 0;

signals:

  void report_status_message (const QString&);

  void interrupt_signal (void);

public slots:

  virtual void copyClipboard (void) = 0;

  virtual void pasteClipboard (void) = 0;

  virtual void selectAll (void) = 0;

  virtual void handleCustomContextMenuRequested (const QPoint& at)
  {
    QClipboard * cb = QApplication::clipboard ();

    _paste_action->setEnabled (cb->text().length() > 0);
    _copy_action->setEnabled (selectedText().length() > 0);

    _contextMenu->move (mapToGlobal (at));
    _contextMenu->show ();
  }

  void notice_settings (const QSettings *settings);

  virtual void init_terminal_size (void) { }

  void terminal_interrupt (void) { emit interrupt_signal (); }

  void set_global_shortcuts (bool focus_out);

protected:

  QTerminal (QWidget *xparent = 0) : QWidget (xparent)
  {
    // context menu
    setContextMenuPolicy (Qt::CustomContextMenu);

    _contextMenu = new QMenu (this);

    _copy_action = _contextMenu->addAction (
                     resource_manager::icon ("edit-copy"),
                     tr ("Copy"), this, SLOT (copyClipboard ()));

    _paste_action = _contextMenu->addAction (
                     resource_manager::icon ("edit-paste"),
                      tr ("Paste"), this, SLOT (pasteClipboard ()));

    _contextMenu->addSeparator ();

    _selectall_action = _contextMenu->addAction (
                      tr ("Select All"), this, SLOT (selectAll ()));


    _contextMenu->addSeparator ();

    _contextMenu->addAction (tr ("Clear Window"), parent (),
                             SLOT (handle_clear_command_window_request ()));

    connect (this, SIGNAL (customContextMenuRequested (QPoint)),
             this, SLOT (handleCustomContextMenuRequested (QPoint)));

    connect (this, SIGNAL (report_status_message (const QString&)),
             xparent, SLOT (report_status_message (const QString&)));

    connect (xparent, SIGNAL (settings_changed (const QSettings *)),
             this, SLOT (notice_settings (const QSettings *)));

    connect (xparent, SIGNAL (init_terminal_size_signal ()),
             this, SLOT (init_terminal_size ()));

    connect (xparent, SIGNAL (copyClipboard_signal ()),
             this, SLOT (copyClipboard ()));

    connect (xparent, SIGNAL (pasteClipboard_signal ()),
             this, SLOT (pasteClipboard ()));

    connect (xparent, SIGNAL (selectAll_signal ()),
             this, SLOT (selectAll ()));

    // extra interrupt action
    _interrupt_action = new QAction (this);
    addAction (_interrupt_action);

    _interrupt_action->setShortcut (
            QKeySequence (Qt::ControlModifier + Qt::Key_C));

    connect (_interrupt_action, SIGNAL (triggered ()),
            this, SLOT (terminal_interrupt ()));
  }

private:

  QMenu *_contextMenu;
  QAction * _copy_action;
  QAction * _paste_action;
  QAction * _selectall_action;

  QAction *_interrupt_action;
};

#endif // QTERMINAL_H
