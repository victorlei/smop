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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <QApplication>
#include <QClipboard>
#include <QVBoxLayout>
#include <QMenu>
#include <QScrollBar>
#include <QDesktopWidget>
#include <QCompleter>
#include <QLabel>

#include "error.h"
#include "resource-manager.h"

#include "cmd-hist.h"

#include "history-dock-widget.h"

history_dock_widget::history_dock_widget (QWidget *p)
  : octave_dock_widget (p)
{
  setObjectName ("HistoryDockWidget");
  setStatusTip (tr ("Browse and search the command history."));

  connect (this, SIGNAL (command_create_script (const QString&)),
           p, SLOT (new_file (const QString&)));

  connect (this, SIGNAL (information (const QString&)),
           p, SLOT (report_status_message (const QString&)));

  connect (this, SIGNAL (command_double_clicked (const QString&)),
           p, SLOT (execute_command_in_terminal (const QString&)));

  construct ();
}

void
history_dock_widget::construct ()
{
  _history_model = new QStringListModel ();
  _sort_filter_proxy_model.setSourceModel (_history_model);
  _history_list_view = new QListView (this);
  _history_list_view->setModel (&_sort_filter_proxy_model);
  _history_list_view->setAlternatingRowColors (true);
  _history_list_view->setEditTriggers (QAbstractItemView::NoEditTriggers);
  _history_list_view->setStatusTip (
    tr ("Double-click a command to transfer it to the terminal."));
  _history_list_view->setSelectionMode (QAbstractItemView::ExtendedSelection);
  _history_list_view->setContextMenuPolicy (Qt::CustomContextMenu);
  connect (_history_list_view,
           SIGNAL (customContextMenuRequested (const QPoint &)), this,
           SLOT (ctxMenu (const QPoint &)));

  _filter = new QComboBox (this);
  _filter->setToolTip (tr ("Enter text to filter the command history"));
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

  setWindowIcon (QIcon (":/actions/icons/logo.png"));
  set_title (tr ("Command History"));
  setWidget (new QWidget ());

  QVBoxLayout *vbox_layout = new QVBoxLayout ();
  QHBoxLayout *hbox_layout = new QHBoxLayout ();
  hbox_layout->addWidget (filter_label);
  hbox_layout->addWidget (_filter_checkbox);
  hbox_layout->addWidget (_filter);
  vbox_layout->addLayout (hbox_layout);
  vbox_layout->addWidget (_history_list_view);
  vbox_layout->setMargin (2);

  widget ()->setLayout (vbox_layout);

  setFocusProxy (_filter->lineEdit ());

  // Init state of the filter
  QSettings *settings = resource_manager::get_settings ();
  _filter->addItems (settings->value ("history_dock_widget/mru_list").toStringList ());

  bool filter_state =
            settings->value ("history_dock_widget/filter_active", false).toBool ();
  _filter_checkbox->setChecked (filter_state);
  filter_activate (filter_state);

  // Connect signals and slots
  connect (_filter, SIGNAL (editTextChanged (const QString&)),
           &_sort_filter_proxy_model, SLOT (setFilterWildcard (const QString&)));
  connect (_filter_checkbox, SIGNAL (toggled (bool)),
           this, SLOT (filter_activate (bool)));
  connect (_filter->lineEdit (), SIGNAL (editingFinished ()),
           this, SLOT (update_filter_history ()));

  connect (_history_list_view, SIGNAL (doubleClicked (QModelIndex)),
           this, SLOT (handle_double_click (QModelIndex)));

  // shrink max. displayed entry size to desktop width
  QSize screen = QDesktopWidget ().screenGeometry ().size ();
  int w = screen.width ();
  QFontMetrics fm = _history_list_view->fontMetrics ();
  int h = fm.height ();
  _history_list_view->setGridSize (QSize (w,h));
  _history_list_view->setTextElideMode (Qt::ElideRight);
}

history_dock_widget::~history_dock_widget ()
{
  QSettings *settings = resource_manager::get_settings ();

  settings->setValue ("history_dock_widget/filter_active",
                      _filter_checkbox->isChecked ());

  QStringList mru;
  for (int i = 0; i < _filter->count (); i++)
    mru.append (_filter->itemText (i));
  settings->setValue ("history_dock_widget/mru_list", mru);

  settings->sync ();
}

void
history_dock_widget::filter_activate (bool state)
{
  _filter->setEnabled (state);
  _sort_filter_proxy_model.setDynamicSortFilter (state);

  if (state)
    _sort_filter_proxy_model.setFilterWildcard (_filter->currentText ());
  else
    _sort_filter_proxy_model.setFilterWildcard (QString ());
}

void
history_dock_widget::update_filter_history ()
{
  QString text = _filter->currentText ();   // get current text
  int index = _filter->findText (text);     // and its actual index

  if (index > -1)
    _filter->removeItem (index);    // remove if already existing

  _filter->insertItem (0, text);    // (re)insert at beginning
  _filter->setCurrentIndex (0);
}

void history_dock_widget::ctxMenu (const QPoint &xpos)
{
  QMenu menu (this);

  QModelIndex index = _history_list_view->indexAt (xpos);

  if (index.isValid () && index.column () == 0)
    {
      menu.addAction (resource_manager::icon ("edit-copy"),
                  tr ("Copy"), this, SLOT (handle_contextmenu_copy (bool)));
      menu.addAction (tr ("Evaluate"), this,
                  SLOT (handle_contextmenu_evaluate (bool)));
      menu.addAction (resource_manager::icon ("document-new"),
                  tr ("Create script"), this,
                  SLOT (handle_contextmenu_create_script (bool)));
      menu.exec (_history_list_view->mapToGlobal (xpos));
    }
}

void history_dock_widget::handle_contextmenu_copy (bool)
{
  QString text;
  QItemSelectionModel *selectionModel = _history_list_view->selectionModel ();
  QModelIndexList rows = selectionModel->selectedRows ();
  QModelIndexList::iterator it;
  bool prev_valid_row = false;
  for (it = rows.begin (); it != rows.end (); it++)
    {
      if ((*it).isValid ())
        {
          if (prev_valid_row)
            text += "\n";
          text += (*it).data ().toString ();
          prev_valid_row = true;
        }
    }
  QApplication::clipboard ()->setText (text);
}

void history_dock_widget::handle_contextmenu_evaluate (bool)
{
  QItemSelectionModel *selectionModel = _history_list_view->selectionModel ();
  QModelIndexList rows = selectionModel->selectedRows ();
  QModelIndexList::iterator it;
  for (it = rows.begin () ; it != rows.end (); it++)
    {
      if ((*it).isValid ())
        emit command_double_clicked ((*it).data ().toString ());
    }
}

void
history_dock_widget::handle_contextmenu_create_script (bool)
{
  QString text;
  QItemSelectionModel *selectionModel = _history_list_view->selectionModel ();
  QModelIndexList rows = selectionModel->selectedRows ();

  bool prev_valid_row = false;
  for (QModelIndexList::iterator it = rows.begin (); it != rows.end (); it++)
    {
      if ((*it).isValid ())
        {
          if (prev_valid_row)
            text += "\n";
          text += (*it).data ().toString ();
          prev_valid_row = true;
        }
    }

  if (text.length () > 0)
    emit command_create_script (text);
}


void
history_dock_widget::handle_double_click (QModelIndex modelIndex)
{
  emit command_double_clicked (modelIndex.data ().toString ());
}

void
history_dock_widget::set_history (const QStringList& hist)
{
  _history_model->setStringList (hist);
  _history_list_view->scrollToBottom ();
}

void
history_dock_widget::append_history (const QString& hist_entry)
{
  QStringList lst = _history_model->stringList ();
  lst.append (hist_entry);

  QScrollBar *scroll_bar = _history_list_view->verticalScrollBar ();

  bool at_bottom = scroll_bar->maximum () - scroll_bar->value () < 1;

  _history_model->setStringList (lst);

  // Scroll if slider position at bottom.
  if (at_bottom)
    _history_list_view->scrollToBottom ();
}

void
history_dock_widget::clear_history (void)
{
  _history_model->setStringList (QStringList ());
}

void
history_dock_widget::copyClipboard ()
{
  if (_history_list_view->hasFocus ())
    handle_contextmenu_copy (true);
  if (_filter->lineEdit ()->hasFocus ()
      && _filter->lineEdit ()->hasSelectedText ())
    {
      QClipboard *clipboard = QApplication::clipboard ();
      clipboard->setText ( _filter->lineEdit ()->selectedText ());
    }
}

void
history_dock_widget::pasteClipboard ()
{
  if (_filter->lineEdit ()->hasFocus ())
    {
      QClipboard *clipboard = QApplication::clipboard ();
      QString str =  clipboard->text ();
      if (str.length () > 0)
        _filter->lineEdit ()->insert (str);
    }
}

void
history_dock_widget::selectAll ()
{
  if (_filter->lineEdit ()->hasFocus ())
    {
      _filter->lineEdit ()->selectAll ();
    }
  if (_history_list_view->hasFocus ())
    {
      _history_list_view->selectAll ();
    }
}

void history_dock_widget::handle_visibility (bool visible)
{
  octave_dock_widget::handle_visibility (visible);

  if (visible)
    {
      int filter_state = _filter_checkbox->isChecked ();
      filter_activate (filter_state);
    }
}