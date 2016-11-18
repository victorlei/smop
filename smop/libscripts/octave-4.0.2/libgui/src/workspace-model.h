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

#if !defined (octave_workspace_model_h)
#define octave_workspace_model_h 1

#include <QAbstractTableModel>
#include <QVector>
#include <QSemaphore>
#include <QStringList>
#include <QChar>
#include <QList>
#include <QColor>
#include <QSettings>

// Defined for purposes of sending QList<int> as part of signal.
typedef QList<int> QIntList;

class workspace_model
  : public QAbstractTableModel
{
  Q_OBJECT

public:

  workspace_model (QObject *parent = 0);

  ~workspace_model (void) { }

  static QList<QColor> storage_class_default_colors (void);

  static QStringList storage_class_names (void);

  QVariant data (const QModelIndex& index, int role) const;

  bool setData (const QModelIndex& index, const QVariant& value,
                int role = Qt::EditRole);

  Qt::ItemFlags flags (const QModelIndex& index) const;

  QVariant headerData (int section, Qt::Orientation orientation,
                       int role = Qt::DisplayRole) const;

  int rowCount (const QModelIndex& parent = QModelIndex ()) const;

  int columnCount (const QModelIndex& parent = QModelIndex ()) const;

  bool is_top_level (void) const { return _top_level; }

  QColor storage_class_color (int s_class)
  { return _storage_class_colors.at (s_class); }

public slots:

  void set_workspace (bool top_level,
                      bool debug,
                      const QString& scopes,
                      const QStringList& symbols,
                      const QStringList& class_names,
                      const QStringList& dimensions,
                      const QStringList& values,
                      const QIntList& complex_flags);

  void clear_workspace (void);

  void notice_settings (const QSettings *);

signals:

  void model_changed (void);

  void rename_variable (const QString& old_name, const QString& new_name);

private:

  void clear_data (void);
  void update_table (void);

  bool _top_level;
  QString _scopes;
  QStringList _symbols;
  QStringList _class_names;
  QStringList _dimensions;
  QStringList _values;
  QIntList _complex_flags;

  QStringList _columnNames;

  QList<QColor>  _storage_class_colors;

};

#endif
