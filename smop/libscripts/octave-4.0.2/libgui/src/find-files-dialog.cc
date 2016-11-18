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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <QPushButton>
#include <QDialogButtonBox>
#include <QGridLayout>
#include <QLabel>
#include <QLineEdit>
#include <QComboBox>
#include <QCheckBox>
#include <QHeaderView>
#include <QTableView>
#include <QFileDialog>
#include <QStatusBar>
#include <QIcon>
#include <QFileInfo>
#include <QTimer>
#include <QDirIterator>
#include <QTextStream>
#include <QGroupBox>

#include "find-files-dialog.h"
#include "find-files-model.h"
#include "resource-manager.h"

find_files_dialog::find_files_dialog (QWidget * p)
  : QDialog (p)
{
  setWindowTitle (tr ("Find Files"));
  setWindowIcon (resource_manager::icon ("edit-find"));

  _dir_iterator = 0;

  _timer = new QTimer (this);
  connect (_timer, SIGNAL (timeout ()), this, SLOT (look_for_files ()));

  QSettings *settings = resource_manager::get_settings ();

  QLabel * file_name_label = new QLabel (tr ("Named:"));
  _file_name_edit = new QLineEdit;
  _file_name_edit->setToolTip (tr ("Enter the filename search expression"));

  _file_name_edit->setText (settings->value ("findfiles/file_name",
                                             "*").toString ());
  file_name_label->setBuddy (_file_name_edit);

  QLabel * start_dir_label = new QLabel (tr ("Start in:"));

  _start_dir_edit = new QLineEdit;
  _start_dir_edit->setText (settings->value ("findfiles/start_dir",
                            QDir::currentPath ()).toString ());
  _start_dir_edit->setToolTip (tr ("Enter the start directory"));
  start_dir_label->setBuddy (_start_dir_edit);

  _browse_button = new QPushButton (tr ("Browse..."));
  _browse_button->setToolTip (tr ("Browse for start directory"));
  connect (_browse_button, SIGNAL (clicked ()), this, SLOT (browse_folders ()));

  _recurse_dirs_check = new QCheckBox (tr ("Search subdirectories"));
  _recurse_dirs_check->setChecked (settings->value ("findfiles/recurse_dirs",
                                                    false).toBool ());
  _recurse_dirs_check->setToolTip (
    tr ("Search recursively through directories for matching files"));

  _include_dirs_check = new QCheckBox (tr ("Include directory names"));
  _include_dirs_check->setChecked (settings->value ("findfiles/include_dirs",
                                                    false).toBool ());
  _include_dirs_check->setToolTip (
    tr ("Include matching directories in search results"));

  _name_case_check = new QCheckBox (tr ("Name case insensitive"));
  _name_case_check->setChecked (settings->value ("findfiles/name_case",
                                                 false).toBool ());
  _name_case_check->setToolTip (tr ("Set matching name is case insensitive"));

  _contains_text_check = new QCheckBox (tr ("Contains text:"));
  _contains_text_check->setToolTip (tr ("Enter the file content search expression"));
  _contains_text_check->setChecked (settings->value ("findfiles/check_text",
                                                      false).toBool ());

  _contains_text_edit = new QLineEdit ();
  _contains_text_edit->setToolTip (tr ("Text to match"));
  _contains_text_edit->setText (settings->value ("findfiles/contains_text",
                                                 "").toString ());

  _content_case_check = new QCheckBox (tr ("Text case insensitive"));
  _content_case_check->setChecked (settings->value ("findfiles/content_case",
                                                    false).toBool ());
  _content_case_check->setToolTip (tr ("Set text content is case insensitive"));

  find_files_model * model = new find_files_model (this);

  _file_list = new QTableView;
  _file_list->setWordWrap (false);
  _file_list->setModel (model);
  _file_list->setShowGrid (false);
  _file_list->setSelectionBehavior (QAbstractItemView::SelectRows);
  _file_list->setSelectionMode (QAbstractItemView::SingleSelection);
  _file_list->setAlternatingRowColors (true);
  _file_list->setToolTip (tr ("Search results"));
  _file_list->setSortingEnabled (true);
  _file_list->horizontalHeader ()->restoreState (
    settings->value ("findfiles/column_state").toByteArray ());
  _file_list->horizontalHeader ()->setSortIndicatorShown (true);
  _file_list->horizontalHeader ()->setClickable (true);
  _file_list->horizontalHeader ()->setStretchLastSection (true);
  _file_list->sortByColumn (
                settings->value ("findfiles/sort_files_by_column",0).toInt (),
                static_cast<Qt::SortOrder>
                  (settings->value ("findfiles/sort_files_by_order",
                                    Qt::AscendingOrder).toUInt ()));

  connect (_file_list, SIGNAL (doubleClicked (const QModelIndex&)),
           this,       SLOT (item_double_clicked (const QModelIndex &)));

  _status_bar = new QStatusBar;
  _status_bar->showMessage (tr ("Idle."));

  _find_button =  new QPushButton (tr ("Find"));
  _find_button->setToolTip (tr ("Start search for matching files"));
  connect (_find_button, SIGNAL (clicked ()), this, SLOT (start_find ()));

  _stop_button =  new QPushButton (tr ("Stop"));
  _stop_button->setToolTip (tr ("Stop searching"));
  _stop_button->setEnabled (false);
  connect (_stop_button, SIGNAL (clicked ()), this, SLOT (stop_find ()));

  // layout everything
  QDialogButtonBox * button_box = new QDialogButtonBox (Qt::Vertical);
  button_box->addButton (_find_button, QDialogButtonBox::ActionRole);
  button_box->addButton (_stop_button, QDialogButtonBox::ActionRole);

  // add dialog close button
  _close_button = button_box->addButton (QDialogButtonBox::Close);
  connect (button_box,    SIGNAL (rejected ()),
           this,          SLOT (close ()));

  // name options
  QGroupBox * name_group = new QGroupBox (tr ("File name/location"));
  QGridLayout * name_layout = new QGridLayout;
  name_group->setLayout (name_layout);

  name_layout->addWidget (file_name_label,1,1, 1,1);
  name_layout->addWidget (_file_name_edit,1,2, 1,-1);

  name_layout->addWidget (start_dir_label,2,1);
  name_layout->addWidget (_start_dir_edit,2,2,1,3);
  name_layout->addWidget (_browse_button,2,5);
  name_layout->setColumnStretch (2,1);

  name_layout->addWidget (_recurse_dirs_check,3,1);
  name_layout->addWidget (_include_dirs_check,3,2);
  name_layout->addWidget (_name_case_check,3,3);

  // content options
  QGroupBox * content_group = new QGroupBox (tr ("File contents"));
  QGridLayout * content_layout = new QGridLayout;
  content_group->setLayout (content_layout);
  content_layout->addWidget (_contains_text_check,4,1);
  content_layout->addWidget (_contains_text_edit,4,2,1,3);
  content_layout->setColumnStretch (2,1);
  content_layout->addWidget (_content_case_check,5,1);

  QGridLayout *main_layout = new QGridLayout;
  main_layout->setSizeConstraint (QLayout::SetFixedSize);
  main_layout->addWidget (name_group, 0, 0);
  main_layout->addWidget (content_group, 1, 0);
  main_layout->addWidget (button_box, 0, 1,3,1);
  main_layout->addWidget (_file_list,2,0);
  main_layout->setRowStretch (2,1);
  main_layout->addWidget (_status_bar,3,0,1,-1);


  setLayout (main_layout);

  connect (this, SIGNAL (finished (int)), this, SLOT (handle_done (int)));
}

find_files_dialog::~find_files_dialog ()
{
  QSettings *settings = resource_manager::get_settings ();

  int sort_column = _file_list->horizontalHeader ()->sortIndicatorSection ();
  Qt::SortOrder sort_order
    = _file_list->horizontalHeader ()->sortIndicatorOrder ();
  settings->setValue ("findfiles/sort_files_by_column", sort_column);
  settings->setValue ("findfiles/sort_files_by_order", sort_order);
  settings->setValue ("findfiles/column_state",
                      _file_list->horizontalHeader ()->saveState ());

  settings->setValue ("findfiles/file_name", _file_name_edit->text ());

  settings->setValue ("findfiles/start_dir", _start_dir_edit->text ());

  settings->setValue ("findfiles/recurse_dirs", _recurse_dirs_check->text ());
  settings->setValue ("findfiles/include_dirs", _include_dirs_check->text ());
  settings->setValue ("findfiles/name_case", _name_case_check->text ());

  settings->setValue ("findfiles/contains_text", _contains_text_edit->text ());
  settings->setValue ("findfiles/check_text",
                      _contains_text_check->isChecked ());
  settings->setValue ("findfiles/content_case",
                      _content_case_check->isChecked ());

  settings->sync ();

  if (_dir_iterator)
    delete _dir_iterator;
}

void find_files_dialog::handle_done (int)
{
  // make sure we stopped processing
  stop_find ();
}

void find_files_dialog::set_search_dir (const QString &dir)
{
  stop_find ();
  _start_dir_edit->setText (dir);
}

void
find_files_dialog::start_find ()
{
  stop_find ();

  find_files_model *m = static_cast<find_files_model *> (_file_list->model ());
  m->clear ();

  QDirIterator::IteratorFlags flags = QDirIterator::NoIteratorFlags;
  if (_recurse_dirs_check->isChecked ())
    flags |= QDirIterator::Subdirectories;

  QDir::Filters filters = QDir::Dirs|QDir::NoDotAndDotDot|QDir::Files;
  if (!_name_case_check->isChecked ())
    filters |=  QDir::CaseSensitive;

  QStringList nameFilters;
  nameFilters.append (_file_name_edit->text ());

  if (_dir_iterator) delete _dir_iterator;

  _dir_iterator = new QDirIterator (_start_dir_edit->text (), nameFilters,
                                    filters, flags);

  // enable/disable widgets
  _find_button->setEnabled (false);
  _stop_button->setEnabled (true);
  _close_button->setEnabled (false);
  _browse_button->setEnabled (false);
  _start_dir_edit->setEnabled (false);
  _file_name_edit->setEnabled (false);
  _recurse_dirs_check->setEnabled (false);
  _include_dirs_check->setEnabled (false);
  _name_case_check->setEnabled (false);
  _contains_text_check->setEnabled (false);
  _content_case_check->setEnabled (false);
  _contains_text_edit->setEnabled (false);

  _status_bar->showMessage (tr ("Searching..."));
  _timer->start (0);
}

void
find_files_dialog::stop_find ()
{
  _timer->stop ();

  _find_button->setEnabled (true);
  _stop_button->setEnabled (false);
  _close_button->setEnabled (true);
  _browse_button->setEnabled (true);
  _start_dir_edit->setEnabled (true);
  _file_name_edit->setEnabled (true);
  _recurse_dirs_check->setEnabled (true);
  _include_dirs_check->setEnabled (true);
  _name_case_check->setEnabled (true);
  _contains_text_check->setEnabled (true);
  _content_case_check->setEnabled (true);
  _contains_text_edit->setEnabled (true);

  find_files_model *m = static_cast<find_files_model *> (_file_list->model ());
  QString res_str = QString (tr("%1 match(es)")).arg (m->rowCount ());

  _status_bar->showMessage (res_str);
}

void
find_files_dialog::browse_folders ()
{
  QString dir =
    QFileDialog::getExistingDirectory (this, tr ("Set search directory"),
                                       _start_dir_edit->text ());

  if (! dir.isEmpty ())
    {
      _start_dir_edit->setText (dir);
    }
}

void
find_files_dialog::item_double_clicked (const QModelIndex &idx)
{
  find_files_model *m = static_cast<find_files_model *> (_file_list->model ());

  QFileInfo info = m->fileInfo (idx);

  if (idx.column () == 1)
    {
      // clicked in directory part
      emit dir_selected (info.absolutePath ());
    }
  else
    {
      // clicked in filename part
      if (info.isDir ())
        emit dir_selected (info.absoluteFilePath ());
      else
        emit file_selected (info.absoluteFilePath ());
    }
}

void
find_files_dialog::look_for_files ()
{
  if (_dir_iterator && _dir_iterator->hasNext ())
    {
      QFileInfo info (_dir_iterator->next ());

      find_files_model *m
        = static_cast<find_files_model *> (_file_list->model ());

      if (is_match (info))
        m->addFile (info);
    }
  else
    {
      stop_find ();
    }
}

bool find_files_dialog::is_match (const QFileInfo &info)
{
  bool match = true;
  if (info.isDir ())
    {
      if (!_include_dirs_check->isChecked ()) match = false;
      if (_contains_text_check->isChecked ()) match = false;
    }
  else
    {
      // a file
      if (_contains_text_check->isChecked ())
        {
          match = false;

          QFile file (info.absoluteFilePath ());
          if (file.open (QIODevice::ReadOnly))
            {
              QTextStream stream (&file);

              QString line;
              QString match_str = _contains_text_edit->text ();

              Qt::CaseSensitivity cs = _content_case_check->isChecked () ?
                                       Qt::CaseInsensitive : Qt::CaseSensitive;

              do
                {
                  line = stream.readLine ();
                  match = line.contains (match_str, cs);
                }
              while (!line.isNull () && match == false);
            }

        }
    }

  return match;
}

