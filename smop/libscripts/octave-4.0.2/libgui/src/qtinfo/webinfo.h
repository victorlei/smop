/*

Copyright (C) 2009 P. L. Lucas
Copyright (C) 2012-2015 Jacob Dawid

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

// Author: P. L. Lucas
// Author: 2012 Jacob Dawid <jacob.dawid@cybercatalyst.com>

#include <QTextBrowser>
#include "parser.h"
#include <QStackedWidget>
#include <QTabBar>
#include <QPushButton>
#include <QLineEdit>
#include <QCheckBox>
#include <QToolButton>

class webinfo : public QWidget
{
  Q_OBJECT
public:
  webinfo (QWidget *parent = 0);
  bool set_info_path (const QString& info_path);
  void load_node (const QString& node_name);

  void load_ref (const QString &ref_name);

public slots:
  void link_clicked (const QUrl& link);
  void current_tab_changed (int index);
  void close_tab (int index);
  void search ();
  void zoom_in ();
  void zoom_out ();

  void copyClipboard ();
  void pasteClipboard ();
  void selectAll ();

private:
  QTextBrowser        *_text_browser;
  QTabBar             *_tab_bar;
  QStackedWidget      *_stacked_widget;
  QLineEdit           *_search_line_edit;
  QCheckBox           *_search_check_box;
  QToolButton         *_zoom_in_button;
  QToolButton         *_zoom_out_button;

  parser              _parser;
  QFont               _font_web;

  QTextBrowser *addNewTab (const QString& name);
};
