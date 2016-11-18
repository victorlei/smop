/****************************************************************************

Find dialog derived from an example from Qt Toolkit (license below (**))

Copyright (C) 2012-2015 Torsten <ttl@justmail.de>
Copyright (C) 2009 Nokia Corporation and/or its subsidiary(-ies).
 All rights reserved.
 Contact: Nokia Corporation (qt-info@nokia.com)

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

** This file is part of the examples of the Qt Toolkit.
**
** $QT_BEGIN_LICENSE:LGPL$
** Commercial Usage
** Licensees holding valid Qt Commercial licenses may use this file in
** accordance with the Qt Commercial License Agreement provided with the
** Software or, alternatively, in accordance with the terms contained in
** a written agreement between you and Nokia.
**
** GNU Lesser General Public License Usage
** Alternatively, this file may be used under the terms of the GNU Lesser
** General Public License version 2.1 as published by the Free Software
** Foundation and appearing in the file LICENSE.LGPL included in the
** packaging of this file.  Please review the following information to
** ensure the GNU Lesser General Public License version 2.1 requirements
** will be met: http://www.gnu.org/licenses/old-licenses/lgpl-2.1.html.
**
** In addition, as a special exception, Nokia gives you certain additional
** rights.  These rights are described in the Nokia Qt LGPL Exception
** version 1.1, included in the file LGPL_EXCEPTION.txt in this package.
**
** GNU General Public License Usage
** Alternatively, this file may be used under the terms of the GNU
** General Public License version 3.0 as published by the Free Software
** Foundation and appearing in the file LICENSE.GPL included in the
** packaging of this file.  Please review the following information to
** ensure the GNU General Public License version 3.0 requirements will be
** met: http://www.gnu.org/copyleft/gpl.html.
**
** If you have questions regarding the use of this file, please contact
** Nokia at qt-info@nokia.com.
** $QT_END_LICENSE$
**
****************************************************************************/

#ifndef FIND_DIALOG_H
#define FIND_DIALOG_H

#include <QDialog>
#include <Qsci/qsciscintilla.h>

class QCheckBox;
class QDialogButtonBox;
class QGroupBox;
class QLabel;
class QLineEdit;
class QPushButton;

class find_dialog : public QDialog
{
  Q_OBJECT
public:
  find_dialog (QsciScintilla* edit_area, QWidget *parent = 0);
  void init_search_text ();

private slots:
#ifdef HAVE_QSCI_FINDSELECTION
  void handle_sel_search_changed (int);
  void handle_selection_changed (bool has_selected);
#endif
  void handle_backward_search_changed (int);
  void handle_search_text_changed (QString new_search_text);

  void find (bool forward = true);
  void find_next ();
  void find_prev ();
  void replace ();
  void replace_all ();

private:

  void no_matches_message ();
  void do_replace ();

  QLabel            *_search_label;
  QLineEdit         *_search_line_edit;
  QLabel            *_replace_label;
  QLineEdit         *_replace_line_edit;
  QCheckBox         *_case_check_box;
  QCheckBox         *_from_start_check_box;
  QCheckBox         *_wrap_check_box;
  QCheckBox         *_whole_words_check_box;
  QCheckBox         *_regex_check_box;
  QCheckBox         *_search_selection_check_box;
  QCheckBox         *_backward_check_box;
  QDialogButtonBox  *_button_box;
  QPushButton       *_find_next_button;
  QPushButton       *_find_prev_button;
  QPushButton       *_replace_button;
  QPushButton       *_replace_all_button;
  QPushButton       *_more_button;
  QWidget           *_extension;
  QsciScintilla     *_edit_area;
  bool               _find_result_available;
  int                _rep_all;
  bool               _rep_active;
};

#endif // FIND_DIALOG_H

