/*

Copyright (C) 2013-2015 John W. Eaton
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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <QInputDialog>
#include <QApplication>
#include <QClipboard>
#include <QMessageBox>
#include <QLineEdit>
#include <QHeaderView>
#include <QHBoxLayout>
#include <QVBoxLayout>
#include <QPushButton>
#include <QMenu>
#include <QLabel>
#include <QCompleter>

#include "workspace-view.h"
#include "resource-manager.h"
#include "symtab.h"

workspace_view::workspace_view (QWidget *p)
  : octave_dock_widget (p), view (new QTableView (this))
{
  setObjectName ("WorkspaceView");
  setWindowIcon (QIcon (":/actions/icons/logo.png"));
  set_title (tr ("Workspace"));
  setStatusTip (tr ("View the variables in the active workspace."));

  _filter = new QComboBox (this);
  _filter->setToolTip (tr ("Enter text to filter the workspace"));
  _filter->setEditable (true);
  _filter->setMaxCount (MaxFilterHistory);
  _filter->setInsertPolicy (QComboBox::NoInsert);
  _filter->setSizeAdjustPolicy (
    QComboBox::AdjustToMinimumContentsLengthWithIcon);
  QSizePolicy sizePol (QSizePolicy::Expanding, QSizePolicy::Preferred);
  _filter->setSizePolicy (sizePol);
  _filter->completer ()->setCaseSensitivity (Qt::CaseSensitive);

  QLabel *filter_label = new QLabel (tr ("Filter"));

  _filter_checkbox = new QCheckBox ();

  view->setWordWrap (false);
  view->setContextMenuPolicy (Qt::CustomContextMenu);
  view->setShowGrid (false);
  (view->verticalHeader) ()->hide ();
  view->setAlternatingRowColors (true);
  view_previous_row_count = 0;

  // Set an empty widget, so we can assign a layout to it.
  setWidget (new QWidget (this));

  // Create a new layout and add widgets to it.
  QVBoxLayout *vbox_layout = new QVBoxLayout ();
  QHBoxLayout *hbox_layout = new QHBoxLayout ();
  hbox_layout->addWidget (filter_label);
  hbox_layout->addWidget (_filter_checkbox);
  hbox_layout->addWidget (_filter);
  vbox_layout->addLayout (hbox_layout);
  vbox_layout->addWidget (view);
  vbox_layout->setMargin (2);

  // Set the empty widget to have our layout.
  widget ()->setLayout (vbox_layout);

  // Initialize collapse/expand state of the workspace subcategories.

  QSettings *settings = resource_manager::get_settings ();

  //enable sorting (setting column and order after model was set)
  view->setSortingEnabled (true);
  // Initialize column order and width of the workspace
  view->horizontalHeader ()->restoreState (
    settings->value ("workspaceview/column_state").toByteArray ());
  // Set header properties for sorting
  view->horizontalHeader ()->setClickable (true);
  view->horizontalHeader ()->setMovable (true);
  view->horizontalHeader ()->setSortIndicator (
    settings->value ("workspaceview/sort_by_column",0).toInt (),
    static_cast<Qt::SortOrder>
    (settings->value ("workspaceview/sort_order", Qt::AscendingOrder).toUInt ())
  );
  view->horizontalHeader ()->setSortIndicatorShown (true);

  // Init state of the filter
  _filter->addItems (settings->value ("workspaceview/mru_list").toStringList ());

  bool filter_state =
    settings->value ("workspaceview/filter_active", false).toBool ();
  _filter_checkbox->setChecked (filter_state);
  filter_activate (filter_state);

  // Connect signals and slots.

  connect (_filter, SIGNAL (editTextChanged (const QString&)),
           this, SLOT (filter_update (const QString&)));
  connect (_filter_checkbox, SIGNAL (toggled (bool)),
           this, SLOT (filter_activate (bool)));
  connect (_filter->lineEdit (), SIGNAL (editingFinished ()),
           this, SLOT (update_filter_history ()));

  connect (view, SIGNAL (customContextMenuRequested (const QPoint&)),
           this, SLOT (contextmenu_requested (const QPoint&)));

  connect (this, SIGNAL (command_requested (const QString&)),
           p, SLOT (execute_command_in_terminal (const QString&)));

}

workspace_view::~workspace_view (void)
{
  QSettings *settings = resource_manager::get_settings ();

  settings->setValue ("workspaceview/column_state",
                      view->horizontalHeader ()->saveState ());

  int sort_column = view->horizontalHeader ()->sortIndicatorSection ();
  Qt::SortOrder sort_order = view->horizontalHeader ()->sortIndicatorOrder ();
  settings->setValue ("workspaceview/sort_by_column", sort_column);
  settings->setValue ("workspaceview/sort_order", sort_order);

  settings->setValue ("workspaceview/filter_active",
                      _filter_checkbox->isChecked ());

  QStringList mru;
  for (int i = 0; i < _filter->count (); i++)
    mru.append (_filter->itemText (i));
  settings->setValue ("workspaceview/mru_list", mru);

  settings->sync ();
}

void workspace_view::setModel (workspace_model *model)
{
  _filter_model.setSourceModel (model);
  _filter_model.setFilterKeyColumn(0);

  view->setModel (&_filter_model);

  // set the sorting after a model was set, it would be ignored otherwise
  QSettings *settings = resource_manager::get_settings ();
  view->sortByColumn (
    settings->value ("workspaceview/sort_by_column",0).toInt (),
    static_cast<Qt::SortOrder>
    (settings->value ("workspaceview/sort_order", Qt::AscendingOrder).toUInt ())
  );

  _model = model;
}

void
workspace_view::closeEvent (QCloseEvent *e)
{
  emit active_changed (false);
  QDockWidget::closeEvent (e);
}

void
workspace_view::filter_update (const QString& expression)
{
  _filter_model.setFilterWildcard (expression);
  handle_model_changed ();
}

void
workspace_view::filter_activate (bool state)
{
  _filter->setEnabled (state);
  _filter_model.setDynamicSortFilter (state);

  if (state)
    filter_update (_filter->currentText ());
  else
    filter_update (QString ());
}

void
workspace_view::update_filter_history ()
{
  QString text = _filter->currentText ();   // get current text
  int index = _filter->findText (text);     // and its actual index

  if (index > -1)
    _filter->removeItem (index);    // remove if already existing

  _filter->insertItem (0, text);    // (re)insert at beginning
  _filter->setCurrentIndex (0);
}

QString
workspace_view::get_var_name (QModelIndex index)
{
  index = index.sibling (index.row (), 0);
  QAbstractItemModel *m = view->model ();
  QMap<int, QVariant> item_data = m->itemData (index);

  return item_data[0].toString ();
}

void
workspace_view::contextmenu_requested (const QPoint& qpos)
{
  QMenu menu (this);

  QModelIndex index = view->indexAt (qpos);

  // if it isnt Local, Glocal etc, allow the ctx menu
  if (index.isValid () && index.column () == 0)
    {
      QString var_name = get_var_name (index);

      menu.addAction (tr ("Copy name"), this,
                      SLOT (handle_contextmenu_copy ()));

      menu.addAction (tr ("Copy value"), this,
                      SLOT (handle_contextmenu_copy_value ()));

      QAction *rename = menu.addAction (tr ("Rename"), this,
                                        SLOT (handle_contextmenu_rename ()));

      QAbstractItemModel *m = view->model ();
      const workspace_model *wm = static_cast<const workspace_model *> (m);

      if (! wm->is_top_level ())
        {
          rename->setDisabled (true);
          rename->setToolTip (tr ("Only top-level symbols may be renamed"));
        }

      menu.addSeparator ();

      menu.addAction ("disp (" + var_name + ")", this,
                      SLOT (handle_contextmenu_disp ()));

      menu.addAction ("plot (" + var_name + ")", this,
                      SLOT (handle_contextmenu_plot ()));

      menu.addAction ("stem (" + var_name + ")", this,
                      SLOT (handle_contextmenu_stem ()));

      menu.exec (view->mapToGlobal (qpos));
    }
}

void
workspace_view::handle_contextmenu_copy (void)
{
  QModelIndex index = view->currentIndex ();

  if (index.isValid ())
    {
      QString var_name = get_var_name (index);

      QClipboard *clipboard = QApplication::clipboard ();

      clipboard->setText (var_name);
    }
}

void
workspace_view::handle_contextmenu_copy_value (void)
{
  QModelIndex index = view->currentIndex ();

  if (index.isValid ())
    {
      QString var_name = get_var_name (index);

      octave_value val = symbol_table::varval (var_name.toStdString ());
      std::ostringstream buf;
      val.print_raw (buf, true);

      QClipboard *clipboard = QApplication::clipboard ();
      clipboard->setText (QString::fromStdString (buf.str ()));
    }
}

void
workspace_view::handle_contextmenu_rename (void)
{
  QModelIndex index = view->currentIndex ();

  if (index.isValid ())
    {
      QString var_name = get_var_name (index);

      QInputDialog* inputDialog = new QInputDialog ();

      inputDialog->setOptions (QInputDialog::NoButtons);

      bool ok = false;

      QString new_name
        =  inputDialog->getText (0, "Rename Variable", "New name:",
                                 QLineEdit::Normal, var_name, &ok);

      if (ok && ! new_name.isEmpty ())
        {
          QAbstractItemModel *m = view->model ();
          m->setData (index, new_name, Qt::EditRole);
        }
    }
}

void
workspace_view::handle_contextmenu_disp (void)
{
  relay_contextmenu_command ("disp");
}

void
workspace_view::handle_contextmenu_plot (void)
{
  relay_contextmenu_command ("figure (); plot");
}

void
workspace_view::handle_contextmenu_stem (void)
{
  relay_contextmenu_command ("figure (); stem");
}

void
workspace_view::relay_contextmenu_command (const QString& cmdname)
{
  QModelIndex index = view->currentIndex ();

  if (index.isValid ())
    {
      QString var_name = get_var_name (index);

      emit command_requested (cmdname + " (" + var_name + ");");
    }
}

void
workspace_view::handle_model_changed (void)
{
//  view->resizeRowsToContents ();
  // Just modify those rows that have been added rather than go through
  // the whole list.  For-loop test will handle when number of rows reduced.
  QFontMetrics fm = view->fontMetrics ();
  int row_height =  fm.height ();
  int new_row_count = _filter_model.rowCount ();
  for (int i = view_previous_row_count; i < new_row_count; i++)
    view->setRowHeight (i, row_height);
  view_previous_row_count = new_row_count;
}

void
workspace_view::notice_settings (const QSettings *settings)
{
  _model->notice_settings (settings); // update colors of model first

  QString tool_tip;

  if (!settings->value ("workspaceview/hide_tool_tips",false).toBool ())
    {
      tool_tip  = QString (tr ("View the variables in the active workspace.<br>"));
      tool_tip += QString (tr ("Colors for variable attributes:"));
      for (int i = 0; i < resource_manager::storage_class_chars ().length (); i++)
        {
          tool_tip +=
            QString ("<div style=\"background-color:%1;color:#000000\">%2</div>")
            .arg (_model->storage_class_color (i).name ())
            .arg (resource_manager::storage_class_names ().at (i));
        }
    }

  setToolTip (tool_tip);

}

void
workspace_view::copyClipboard ()
{
  if (view->hasFocus ())
    handle_contextmenu_copy ();
}

void
workspace_view::selectAll ()
{
  if (view->hasFocus ())
    view->selectAll ();
}

