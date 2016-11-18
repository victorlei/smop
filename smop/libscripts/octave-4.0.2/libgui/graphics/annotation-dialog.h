/*

Copyright (C) 2015 John Donoghue

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

#ifndef ANNOTATIONDIALOG_H
#define ANNOTATIONDIALOG_H

#include <QDialog>
#include <QLineEdit>
#include <QAbstractButton>

#include "oct-obj.h"

namespace Ui
{
  class annotation_dialog;
}

class annotation_dialog : public QDialog
{
  Q_OBJECT
public:
  explicit annotation_dialog (QWidget * parent, const octave_value_list &pr);
  ~annotation_dialog ();

  octave_value_list get_properties() const;

private slots:
  // slots for dialog's buttons
  void button_clicked (QAbstractButton *button);
  void edit_string_changed (const QString &str);
  void prompt_for_color ();

private:
  void init();

  void get_gui_props ();
  void set_gui_props ();

  Ui::annotation_dialog * ui;
  octave_value_list props;
};

#endif // ANNOTATIONDIALOG_H
