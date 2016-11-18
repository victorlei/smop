/*

Copyright (C) 2011-2015 Jacob Dawid

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

#ifndef FILESDOCKWIDGET_H
#define FILESDOCKWIDGET_H

#include <QListView>
#include <QDate>
#include <QObject>
#include <QWidget>
#include <QListWidget>
#include <QFileSystemModel>
#include <QToolBar>
#include <QToolButton>
#include <QVBoxLayout>
#include <QAction>
#include <QTreeView>
#include <QMouseEvent>

#include <QComboBox>
#include "octave-dock-widget.h"

/**
   @class files_dock_widget
   @brief Dock widget to display files in the current directory.
*/
class files_dock_widget : public octave_dock_widget
{
  Q_OBJECT

public:

  files_dock_widget (QWidget *parent = 0);

  ~files_dock_widget ();

public slots:

  /** Slot for handling a change in directory via double click. */
  void item_double_clicked (const QModelIndex & index);

  /** Slot for handling the up-directory button in the toolbar. */
  void change_directory_up ();

  /** Slot for handling the sync octave directory button in the toolbar. */
  void do_sync_octave_directory ();

  /** Slot for handling the sync browser directory button in the toolbar. */
  void do_sync_browser_directory ();

  /** Sets the current directory being displayed. */
  void set_current_directory (const QString& dir);

  /** Accepts user input a the line edit for the current directory. */
  void accept_directory_line_edit ();

  /** set the internal variable that holds the actual octave variable **/
  void update_octave_directory (const QString& dir);

  /** Tells the widget to react on changed settings. */
  void notice_settings (const QSettings *settings);

private slots:
  /** context menu wanted */
  void contextmenu_requested (const QPoint& pos);

  void toggle_headercontextitem_filesize ();
  void toggle_headercontextitem_filetype ();
  void toggle_headercontextitem_datemodified ();
  void toggle_headercontextitem_showhidden ();

  void headercontextmenu_requested (const QPoint& pos);

  /* context menu actions */
  void contextmenu_open (bool);
  void contextmenu_open_in_app (bool);
  void contextmenu_copy_selection (bool);
  void contextmenu_run (bool);
  void contextmenu_load (bool);
  void contextmenu_rename (bool);
  void contextmenu_delete (bool);
  void contextmenu_newfile (bool);
  void contextmenu_newdir (bool);
  void contextmenu_setcurrentdir (bool);
  void contextmenu_findfiles (bool);

  /* popdown menu options */
  void popdownmenu_newfile (bool);
  void popdownmenu_newdir (bool);
  void popdownmenu_search_dir (bool);
  void popdownmenu_findfiles (bool);
  void popdownmenu_home (bool);

  /* from octave_doc_widget */
  void copyClipboard ();
  void pasteClipboard ();
  void selectAll ();

signals:

  /** Emitted, whenever the user requested to open a file. */
  void open_file (const QString& fileName);

  /** Emitted, whenever the currently displayed directory changed. */
  void displayed_directory_changed (const QString& dir);

  /** Emitted, whenever the user requested to load a file. */
  void load_file_signal (const QString& fileName);

  /** Emitted, whenever the user requested to run a file. */
  void run_file_signal (const QFileInfo& info);

  /** Emitted, whenever wants to search for a file . */
  void find_files_signal (const QString &startdir);

private:
  void process_new_file (const QString &parent_name);
  void process_new_dir (const QString &parent_name);
  void process_set_current_dir (const QString &parent_name);
  void process_find_files (const QString &dir_name);

  /** set a new directory or open a file **/
  void display_directory (const QString& dir, bool set_octave_dir = true);

  void open_item_in_app (const QModelIndex& index);

  /** Variables for the actions **/
  QToolBar *        _navigation_tool_bar;
  QAction *         _sync_octave_directory_action;
  QAction *         _sync_browser_directory_action;

  /** The file system model. */
  QFileSystemModel *_file_system_model;

  /** The file system view. */
  QTreeView *       _file_tree_view;
  QComboBox *       _current_directory;

  /** Internal variables **/
  bool              _sync_octave_dir;    // flag if syncing with octave
  QString           _octave_dir;         // the actual octave dir

  enum { MaxMRUDirs = 10 };
};

#endif // FILESDOCKWIDGET_H
