/*

Copyright (C) 2014-2015 Torsten <ttl@justmail.de>

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

#ifndef SHORTCUT_MANAGER_H
#define SHORTCUT_MANAGER_H

#include <QWidget>
#include <QTreeWidget>
#include <QLineEdit>
#include <QKeyEvent>
#include <QLabel>
#include <QSettings>

class enter_shortcut : public QLineEdit
{
  Q_OBJECT

public:
  enter_shortcut (QWidget *p = 0);
  ~enter_shortcut ();

  virtual void  keyPressEvent (QKeyEvent *e);

public slots:
  void handle_direct_shortcut (int);

private:
  bool _direct_shortcut;

};


class shortcut_manager : public QWidget
{
  Q_OBJECT

public:
  shortcut_manager ();
  ~shortcut_manager ();

  static void init_data ()
  {
    if (instance_ok ())
      instance->do_init_data ();
  }

  static void write_shortcuts (int set, QSettings *settings, bool closing)
  {
    if (instance_ok ())
      instance->do_write_shortcuts (set, settings, closing);
  }

  static void set_shortcut (QAction *action, const QString& key)
  {
    if (instance_ok ())
      instance->do_set_shortcut (action, key);
  }

  static void fill_treewidget (QTreeWidget *tree_view)
  {
    if (instance_ok ())
      instance->do_fill_treewidget (tree_view);
  }

  static void import_export (bool import, int set)
  {
    if (instance_ok ())
      instance->do_import_export (import, set);
  }

public slots:

signals:

protected:

protected slots:

  void handle_double_clicked (QTreeWidgetItem*, int);
  void shortcut_dialog_finished (int);
  void shortcut_dialog_set_default ();

private:

  static shortcut_manager *instance;
  static void cleanup_instance (void) { delete instance; instance = 0; }

  // No copying!

  shortcut_manager (const shortcut_manager&);
  shortcut_manager& operator = (const shortcut_manager&);

  static bool instance_ok (void);

  void init (QString, QString, QKeySequence);
  void do_init_data ();
  void do_write_shortcuts (int set, QSettings *settings, bool closing);
  void do_set_shortcut (QAction *action, const QString& key);
  void do_fill_treewidget (QTreeWidget *tree_view);
  void do_import_export (bool import, int set);
  void shortcut_dialog (int);
  void import_shortcuts (int set, QSettings *settings);

  class shortcut_t
  {
  public:

    shortcut_t (void)
      : tree_item (0), description (), settings_key (),
        actual_sc (new QKeySequence[2]), default_sc (new QKeySequence[2])
    {
      actual_sc[0] = QKeySequence ();
      actual_sc[1] = QKeySequence ();

      default_sc[0] = QKeySequence ();
      default_sc[1] = QKeySequence ();
    }

    shortcut_t (const shortcut_t& x)
      : tree_item (x.tree_item), description (x.description),
        settings_key (x.settings_key),
        actual_sc (new QKeySequence[2]), default_sc (new QKeySequence[2])
    {
      actual_sc[0] = x.actual_sc[0];
      actual_sc[1] = x.actual_sc[1];

      default_sc[0] = x.default_sc[0];
      default_sc[1] = x.default_sc[1];
    }

    shortcut_t& operator = (const shortcut_t& x)
    {
      if (&x != this)
        {
          tree_item = x.tree_item;
          description = x.description;
          settings_key = x.settings_key;

          actual_sc = new QKeySequence[2];
          default_sc = new QKeySequence[2];

          actual_sc[0] = x.actual_sc[0];
          actual_sc[1] = x.actual_sc[1];

          default_sc[0] = x.default_sc[0];
          default_sc[1] = x.default_sc[1];
        }

      return *this;
    }

    ~shortcut_t (void)
    {
      delete [] actual_sc;
      delete [] default_sc;
    }

    QTreeWidgetItem *tree_item;
    QString description;
    QString settings_key;
    QKeySequence *actual_sc;
    QKeySequence *default_sc;
  };

  QList<shortcut_t> _sc;
  QHash<QString, int> _shortcut_hash;
  QHash<QString, int> _action_hash;
  QHash <QString, QTreeWidgetItem*> _level_hash;
  QHash<int, QTreeWidgetItem*> _index_item_hash;
  QHash<QTreeWidgetItem*, int> _item_index_hash;

  QDialog *_dialog;
  enter_shortcut *_edit_actual;
  QLabel *_label_default;
  int _handled_index;

  QSettings *_settings;
  int _selected_set;

};


#endif // SHORTCUT_MANAGER_H
