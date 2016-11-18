/*

Copyright (C) 2013-2015 John Donoghue

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
#if !defined (octave_find_files_dialog_h)
#define octave_find_files_dialog_h

#include <QDialog>
#include <QModelIndex>
#include <QFileInfo>

class QLineEdit;
class QPushButton;
class QTableView;
class QTimer;
class QDirIterator;
class QCheckBox;
class QStatusBar;

class find_files_dialog : public QDialog
{
  Q_OBJECT
public:
  find_files_dialog (QWidget * parent=0);
  virtual ~find_files_dialog ();

signals:
  void file_selected (const QString &fileName);
  void dir_selected (const QString &fileName);

public slots:
  void set_search_dir (const QString &dir);

private slots:
  void start_find ();
  void stop_find ();
  void browse_folders ();
  void look_for_files ();
  void item_double_clicked (const QModelIndex&);
  void handle_done (int);
private:
  bool is_match (const QFileInfo &info);
  QLineEdit * _start_dir_edit;
  QLineEdit * _file_name_edit;
  QPushButton * _stop_button;
  QPushButton * _find_button;
  QPushButton * _close_button;
  QPushButton * _browse_button;
  QTableView   * _file_list;
  QTimer      * _timer;
  QCheckBox   * _recurse_dirs_check;
  QCheckBox   * _include_dirs_check;
  QCheckBox   * _name_case_check;
  QCheckBox   * _contains_text_check;
  QCheckBox   * _content_case_check;
  QLineEdit * _contains_text_edit;
  QDirIterator * _dir_iterator;
  QStatusBar * _status_bar;
};

#endif // octave_find_files_dialog_h

